module Hasura.GraphQL.Resolve
  ( mutFldToTx
  , queryFldToPGAST
  , traverseQueryRootFldAST
  , UnresolvedVal(..)

  , AnnPGVal(..)
  , txtConverter

  , QueryRootFldAST(..)
  , QueryRootFldUnresolved
  , QueryRootFldResolved
  , toPGQuery

  , RIntro.schemaR
  , RIntro.typeR
  ) where

import           Data.Has

import qualified Data.HashMap.Strict               as Map
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G
import qualified Network.HTTP.Client               as HTTP

import           Hasura.GraphQL.Resolve.Context
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Resolve.Action     as RA
import qualified Hasura.GraphQL.Resolve.Insert     as RI
import qualified Hasura.GraphQL.Resolve.Introspect as RIntro
import qualified Hasura.GraphQL.Resolve.Mutation   as RM
import qualified Hasura.GraphQL.Resolve.Select     as RS
import qualified Hasura.GraphQL.Validate           as V
import qualified Hasura.RQL.DML.Select             as DS
import qualified Hasura.SQL.DML                    as S

data QueryRootFldAST v
  = QRFPk !(DS.AnnSimpleSelG v)
  | QRFSimple !(DS.AnnSimpleSelG v)
  | QRFAgg !(DS.AnnAggSelG v)
  | QRFFnSimple !(DS.AnnSimpleSelG v)
  | QRFFnAgg !(DS.AnnAggSelG v)
  | QRFActionSelect !(RA.ActionSelect v)
  deriving (Show, Eq)

type QueryRootFldUnresolved = QueryRootFldAST UnresolvedVal
type QueryRootFldResolved = QueryRootFldAST S.SQLExp

traverseQueryRootFldAST
  :: (Applicative f)
  => (a -> f b)
  -> QueryRootFldAST a
  -> f (QueryRootFldAST b)
traverseQueryRootFldAST f = \case
  QRFPk s           -> QRFPk <$> DS.traverseAnnSimpleSel f s
  QRFSimple s       -> QRFSimple <$> DS.traverseAnnSimpleSel f s
  QRFAgg s          -> QRFAgg <$> DS.traverseAnnAggSel f s
  QRFFnSimple s     -> QRFFnSimple <$> DS.traverseAnnSimpleSel f s
  QRFFnAgg s        -> QRFFnAgg <$> DS.traverseAnnAggSel f s
  QRFActionSelect s -> QRFActionSelect <$> RA.traverseActionSelect f s

toPGQuery :: QueryRootFldResolved -> Q.Query
toPGQuery = \case
  QRFPk s           -> DS.selectQuerySQL True s
  QRFSimple s       -> DS.selectQuerySQL False s
  QRFAgg s          -> DS.selectAggQuerySQL s
  QRFFnSimple s     -> DS.selectQuerySQL False s
  QRFFnAgg s        -> DS.selectAggQuerySQL s
  QRFActionSelect s -> RA.actionSelectToSql s

validateHdrs
  :: (Foldable t, QErrM m) => UserInfo -> t Text -> m ()
validateHdrs userInfo hdrs = do
  let receivedVars = userVars userInfo
  forM_ hdrs $ \hdr ->
    unless (isJust $ getVarVal hdr receivedVars) $
    throw400 NotFound $ hdr <<> " header is expected but not found"

queryFldToPGAST
  :: ( MonadResolve m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, Has UserInfo r
     , Has QueryCtxMap r
     )
  => V.Field
  -> m QueryRootFldUnresolved
queryFldToPGAST fld = do
  opCtx <- getOpCtx $ V._fName fld
  userInfo <- asks getter
  case opCtx of
    QCSelect ctx -> do
      validateHdrs userInfo (_socHeaders ctx)
      QRFSimple <$> RS.convertSelect ctx fld
    QCSelectPkey ctx -> do
      validateHdrs userInfo (_spocHeaders ctx)
      QRFPk <$> RS.convertSelectByPKey ctx fld
    QCSelectAgg ctx -> do
      validateHdrs userInfo (_socHeaders ctx)
      QRFAgg <$> RS.convertAggSelect ctx fld
    QCFuncQuery ctx -> do
      validateHdrs userInfo (_fqocHeaders ctx)
      QRFFnSimple <$> RS.convertFuncQuerySimple ctx fld
    QCFuncAggQuery ctx -> do
      validateHdrs userInfo (_fqocHeaders ctx)
      QRFFnAgg <$> RS.convertFuncQueryAgg ctx fld
    QCActionFetch ctx ->
      QRFActionSelect <$> RA.resolveActionSelect ctx fld

mutFldToTx
  :: ( MonadResolve m
     , MonadReader r m
     , Has UserInfo r
     , Has MutationCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has InsCtxMap r
     , Has HTTP.Manager r
     , MonadIO m
     )
  => V.Field
  -> m RespTx
mutFldToTx fld = do
  userInfo <- asks getter
  opCtx <- getOpCtx $ V._fName fld
  case opCtx of
    MCInsert ctx -> do
      let roleName = userRole userInfo
      validateHdrs userInfo (_iocHeaders ctx)
      RI.convertInsert roleName (_iocTable ctx) fld
    MCUpdate ctx -> do
      validateHdrs userInfo (_uocHeaders ctx)
      RM.convertUpdate ctx fld
    MCDelete ctx -> do
      validateHdrs userInfo (_docHeaders ctx)
      RM.convertDelete ctx fld
    MCAction ctx ->
      RA.resolveActionInsert fld ctx (userVars userInfo)

getOpCtx
  :: ( MonadResolve m
     , MonadReader r m
     , Has (OpCtxMap a) r
     )
  => G.Name -> m a
getOpCtx f = do
  opCtxMap <- asks getter
  onNothing (Map.lookup f opCtxMap) $ throw500 $
    "lookup failed: opctx: " <> showName f
