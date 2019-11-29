module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet
  , queryOpFromPlan
  , ReusableQueryPlan
  , GeneratedSqlMap
  , PreparedSql(..)
  ) where

import qualified Data.Aeson                             as J
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Data.IntMap                            as IntMap
import qualified Data.TByteString                       as TBS
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Control.Lens
import           Control.Lens.TH                        (makeLenses)
import           Data.Has

import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import qualified Hasura.GraphQL.Validate.Field          as V
import qualified Hasura.SQL.DML                         as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.DML.Select                  (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

type PlanVariables = Map.HashMap G.Variable Int

-- | The value is (Q.PrepArg, PGScalarValue) because we want to log the human-readable value of the
-- prepared argument and not the binary encoding in PG format
type PrepArgMap = IntMap.IntMap (Q.PrepArg, PGScalarValue)

data PGPlan
  = PGPlan
  { _ppQuery            :: !Q.Query
  , _ppVariables        :: !PlanVariables
  , _ppPrepared         :: !PrepArgMap
  , _ppSessionVariables :: !(Set.HashSet SessVar)
  }

instance J.ToJSON PGPlan where
  toJSON (PGPlan q vars prepared sessionVariables) =
    J.object [ "query"             J..= Q.getQueryText q
             , "variables"         J..= vars
             , "prepared"          J..= fmap show prepared
             , "session_variables" J..= sessionVariables
             ]

data RootFieldPlan
  = RFPRaw !B.ByteString
  | RFPPostgres !PGPlan

fldPlanFromJ :: (J.ToJSON a) => a -> RootFieldPlan
fldPlanFromJ = RFPRaw . LBS.toStrict . J.encode

instance J.ToJSON RootFieldPlan where
  toJSON = \case
    RFPRaw encJson     -> J.toJSON $ TBS.fromBS encJson
    RFPPostgres pgPlan -> J.toJSON pgPlan

type FieldPlans = [(G.Alias, RootFieldPlan)]

data ReusableQueryPlan
  = ReusableQueryPlan
  { _rqpVariableTypes :: !ReusableVariableTypes
  , _rqpFldPlans      :: !FieldPlans
  }

instance J.ToJSON ReusableQueryPlan where
  toJSON (ReusableQueryPlan varTypes fldPlans) =
    J.object [ "variables"       J..= varTypes
             , "field_plans"     J..= fldPlans
             ]

withUserVars
  :: (MonadError QErr m)
  => UserVars -> Set.HashSet SessVar -> [(Q.PrepArg, PGScalarValue)]
  -> m [(Q.PrepArg, PGScalarValue)]
withUserVars usrVars requiredSessionVariables list = do
  let usrVarsAsPgScalar = PGValJSON $ Q.JSON $ J.toJSON usrVars
      prepArg = Q.toPrepVal (Q.AltJ usrVars)

  case getMissingSessionVariables requiredSessionVariables usrVars of
    Nothing -> pure $ (prepArg, usrVarsAsPgScalar):list
    Just l  -> throw500 $ mkMissingSessionVariablesMessage l

data PlanningSt
  = PlanningSt
  { _psArgNumber        :: !Int
  , _psVariables        :: !PlanVariables
  , _psPrepped          :: !PrepArgMap
  , _psSessionVariables :: !(Set.HashSet SessVar)
  }
$(makeLenses ''PlanningSt)

initPlanningSt :: PlanningSt
initPlanningSt =
  PlanningSt 2 mempty mempty mempty

getVarArgNum :: (MonadState PlanningSt m) => G.Variable -> m Int
getVarArgNum var = do
  planningState <- get
  case planningState ^.psVariables.at var of
    Just argNum -> pure argNum
    Nothing     -> do
      let newVariableNumber = planningState ^. psArgNumber
      put $ planningState
          -- set the variable to the current arg number
          & psVariables.at var ?~ newVariableNumber
          -- and then bump it
          & over psArgNumber (+1)
      pure newVariableNumber

addPrepArg
  :: (MonadState PlanningSt m)
  => Int -> (Q.PrepArg, PGScalarValue) -> m ()
addPrepArg argNum arg =
  psPrepped.at argNum .= Just arg

-- | In queries, the argument '$1' has the current session
currentSession :: S.SQLExp
currentSession = S.SEPrep 1

getSessionVariableValue
  :: (MonadState PlanningSt m)
  => SessVar -> m S.SQLExp
getSessionVariableValue sessionVariable = do
  -- record the use of the session variable
  psSessionVariables %= Set.insert (T.toLower sessionVariable)
  return $ S.SEOpApp (S.SQLOp "->>")
    [currentSession, S.SELit $ T.toLower sessionVariable]

getNextArgNum :: (MonadState PlanningSt m) => m Int
getNextArgNum = do
  newArgumentNumber <- use psArgNumber
  psArgNumber %= (+1)
  pure newArgumentNumber

prepareWithPlan :: (MonadState PlanningSt m) => UnresolvedVal -> m S.SQLExp
prepareWithPlan = \case
  R.UVPG annPGVal -> do
    let AnnPGVal varM _ colVal = annPGVal
    argNum <- case varM of
      Just var -> getVarArgNum var
      Nothing  -> getNextArgNum
    addPrepArg argNum (toBinaryValue colVal, pstValue colVal)
    return $ toPrepParam argNum (pstType colVal)

  R.UVSessVar ty sessVar -> do
    sessVarVal <- getSessionVariableValue sessVar
    return $ flip S.SETyAnn (S.mkTypeAnn ty) $ case ty of
      PGTypeScalar colTy -> withConstructorFn colTy sessVarVal
      PGTypeArray _      -> sessVarVal

  R.UVSQL sqlExp -> pure sqlExp
  R.UVSession    -> pure currentSession

queryRootName :: Text
queryRootName = "query_root"

convertQuerySelSet
  :: ( MonadError QErr m
     , MonadReader r m
     , Has TypeMap r
     , Has QueryCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has UserInfo r
     )
  => QueryReusability
  -> V.SelSet
  -> m (LazyRespTx, Maybe ReusableQueryPlan, GeneratedSqlMap)
convertQuerySelSet initialReusability fields = do
  usrVars <- asks (userVars . getter)
  (fldPlans, finalReusability) <- runReusabilityTWith initialReusability $
    forM (toList fields) $ \fld -> do
      fldPlan <- case V._fName fld of
        "__type"     -> fldPlanFromJ <$> R.typeR fld
        "__schema"   -> fldPlanFromJ <$> R.schemaR fld
        "__typename" -> pure $ fldPlanFromJ queryRootName
        _            -> do
          unresolvedAst <- R.queryFldToPGAST fld
          (q, PlanningSt _ vars prepped sessionVariables) <-
            flip runStateT initPlanningSt $
            R.traverseQueryRootFldAST prepareWithPlan unresolvedAst
          let fieldPlan = PGPlan (R.toPGQuery q) vars prepped sessionVariables
          pure $ RFPPostgres fieldPlan
      pure (V._fAlias fld, fldPlan)
  let varTypes = finalReusability ^? _Reusable
      reusablePlan = ReusableQueryPlan <$> varTypes <*> pure fldPlans
  (tx, sql) <- mkCurPlanTx usrVars fldPlans
  pure (tx, reusablePlan, sql)

data PreparedSql
  = PreparedSql
  { _psQuery    :: !Q.Query
  , _psPrepArgs :: ![(Q.PrepArg, PGScalarValue)]
    -- ^ The value is (Q.PrepArg, PGScalarValue) because we want to log the human-readable value of the
    -- prepared argument (PGScalarValue) and not the binary encoding in PG format (Q.PrepArg)
  }

-- | Required to log in `query-log`
instance J.ToJSON PreparedSql where
  toJSON (PreparedSql q prepArgs) =
    J.object [ "query" J..= Q.getQueryText q
             , "prepared_arguments" J..= map (txtEncodedPGVal . snd) prepArgs
             ]

-- | Intermediate reperesentation of a computed SQL statement and prepared
-- arguments, or a raw bytestring (mostly, for introspection responses)
-- From this intermediate representation, a `LazyTx` can be generated, or the
-- SQL can be logged etc.
data ResolvedQuery
  = RRRaw !B.ByteString
  | RRSql !PreparedSql

-- | The computed SQL with alias which can be logged. Nothing here represents no
-- SQL for cases like introspection responses. Tuple of alias to a (maybe)
-- prepared statement
type GeneratedSqlMap = [(G.Alias, Maybe PreparedSql)]

withPlan
  :: (MonadError QErr m)
  => UserVars -> PGPlan -> ReusableVariableValues -> m PreparedSql
withPlan usrVars (PGPlan q reqVars prepMap sessionVariables) annVars = do
  prepMap' <- foldM getVar prepMap (Map.toList reqVars)
  args <- withUserVars usrVars sessionVariables $ IntMap.elems prepMap'
  return $ PreparedSql q args
  where
    getVar accum (var, prepNo) = do
      let varName = G.unName $ G.unVariable var
      colVal <- onNothing (Map.lookup var annVars) $
        throw500 $ "missing variable in annVars : " <> varName
      let prepVal = (toBinaryValue colVal, pstValue colVal)
      return $ IntMap.insert prepNo prepVal accum

-- turn the current plan into a transaction
mkCurPlanTx
  :: (MonadError QErr m)
  => UserVars
  -> FieldPlans
  -> m (LazyRespTx, GeneratedSqlMap)
mkCurPlanTx usrVars fldPlans = do
  -- generate the SQL and prepared vars or the bytestring
  resolved <- forM fldPlans $ \(alias, fldPlan) -> do
    fldResp <- case fldPlan of
      RFPRaw resp                      -> return $ RRRaw resp
      RFPPostgres (PGPlan q _ prepMap sessionVariables) -> do
        args <- withUserVars usrVars sessionVariables $ IntMap.elems prepMap
        return $ RRSql $ PreparedSql q args
    return (alias, fldResp)

  return (mkLazyRespTx resolved, mkGeneratedSqlMap resolved)

-- use the existing plan and new variables to create a pg query
queryOpFromPlan
  :: (MonadError QErr m)
  => UserVars
  -> Maybe GH.VariableValues
  -> ReusableQueryPlan
  -> m (LazyRespTx, GeneratedSqlMap)
queryOpFromPlan usrVars varValsM (ReusableQueryPlan varTypes fldPlans) = do
  validatedVars <- GV.validateVariablesForReuse varTypes varValsM
  -- generate the SQL and prepared vars or the bytestring
  resolved <- forM fldPlans $ \(alias, fldPlan) ->
    (alias,) <$> case fldPlan of
      RFPRaw resp        -> return $ RRRaw resp
      RFPPostgres pgPlan -> RRSql <$> withPlan usrVars pgPlan validatedVars

  return (mkLazyRespTx resolved, mkGeneratedSqlMap resolved)

mkLazyRespTx :: [(G.Alias, ResolvedQuery)] -> LazyRespTx
mkLazyRespTx resolved =
  fmap encJFromAssocList $ forM resolved $ \(alias, node) -> do
    resp <- case node of
      RRRaw bs                   -> return $ encJFromBS bs
      RRSql (PreparedSql q args) -> liftTx $ asSingleRowJsonResp q (map fst args)
    return (G.unName $ G.unAlias alias, resp)

mkGeneratedSqlMap :: [(G.Alias, ResolvedQuery)] -> GeneratedSqlMap
mkGeneratedSqlMap resolved =
  flip map resolved $ \(alias, node) ->
    let res = case node of
                RRRaw _  -> Nothing
                RRSql ps -> Just ps
    in (alias, res)
