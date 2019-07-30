module Hasura.RQL.DDL.Action
  ( CreateAction
  , validateCreateAction
  , buildActionInfo
  , runCreateAction

  , DropAction
  , runDropAction

  , fetchActions

  , CreateActionPermission
  , runCreateActionPermission

  , DropActionPermission
  , runDropActionPermission
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Permission.Internal (valueParser)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.Aeson                         as J
import qualified Data.Aeson.Casing                  as J
import qualified Data.Aeson.TH                      as J
import qualified Data.HashMap.Strict                as Map
import qualified Database.PG.Query                  as Q

import           Language.Haskell.TH.Syntax         (Lift)
-- data RetryConf
--   = RetryConf
--   { _rcNumRetries  :: !Word64
--   , _rcIntervalSec :: !Word64
--   , _rcTimeoutSec  :: !(Maybe Word64)
--   } deriving (Show, Eq, Lift)

-- data WebhookConf
--   = WebhookConf
--   { _wcUrl     :: !Text
--   , _wcTimeout :: !Word64
--   , _wcRetry   :: !RetryConf
--   } deriving (Show, Eq)

type InputWebhook = Text
type ActionDefinitionInput = ActionDefinition InputWebhook

data CreateAction
  = CreateAction
  { _caName       :: !ActionName
  , _caDefinition :: !ActionDefinitionInput
  , _caComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''CreateAction)

getActionInfo
  :: (QErrM m, CacheRM m)
  => ActionName -> m ActionInfo
getActionInfo actionName = do
  actionMap <- scActions <$> askSchemaCache
  case Map.lookup actionName actionMap of
    Just actionInfo -> return actionInfo
    Nothing         ->
      throw400 NotExists $
      "action with name " <> actionName <<> " does not exist"

runCreateAction
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     )
  => CreateAction -> m EncJSON
runCreateAction q@(CreateAction actionName actionDefinition comment) = do
  validateCreateAction q
  actionInfo <- buildActionInfo q
  addActionToCache actionInfo
  persistCreateAction
  return successMsg
  where
    persistCreateAction :: (MonadTx m) => m ()
    persistCreateAction = do
      liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
        INSERT into hdb_catalog.hdb_action
          (name, definition, comment)
          VALUES ($1, $2, $3)
      |] (actionName, Q.AltJ actionDefinition, comment) True

validateCreateAction
  :: (QErrM m, UserInfoM m, CacheRM m)
  => CreateAction -> m ()
validateCreateAction q = do
  adminOnly
  actionMap <- scActions <$> askSchemaCache
  onJust (Map.lookup actionName actionMap) $
    const $ throw400 AlreadyExists $
    "action with name " <> actionName <<> " already exists"
  where
    actionName  = _caName q

buildActionInfo
  :: (QErrM m)
  => CreateAction -> m ActionInfo
buildActionInfo q = do
  return $ ActionInfo actionName actionDefinition Map.empty
  where
    CreateAction actionName actionDefinition _ = q


data DropAction
  = DropAction
  { _daName      :: !ActionName
  , _daClearData :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''DropAction)

runDropAction
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => DropAction -> m EncJSON
runDropAction (DropAction actionName clearDataM)= do
  adminOnly
  void $ getActionInfo actionName
  delActionFromCache actionName
  liftTx $ do
    deleteActionFromCatalog
    when clearData clearActionData
  return successMsg
  where
    -- When clearData is not present we assume that
    -- the data needs to be retained
    clearData = fromMaybe False clearDataM

    deleteActionFromCatalog :: Q.TxE QErr ()
    deleteActionFromCatalog =
      Q.unitQE defaultTxErrorHandler [Q.sql|
          DELETE FROM hdb_catalog.hdb_action
            WHERE name = $1
          |] (Identity actionName) True

    clearActionData :: Q.TxE QErr ()
    clearActionData =
      Q.unitQE defaultTxErrorHandler [Q.sql|
          DELETE FROM hdb_catalog.hdb_action_log
            WHERE action_name = $1
          |] (Identity actionName) True

fetchActions :: Q.TxE QErr [CreateAction]
fetchActions =
  map fromRow <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT action_name, action_defn, comment
       FROM hdb_catalog.hdb_action
     |] () True
  where
    fromRow (actionName, Q.AltJ definition, comment) =
      CreateAction actionName definition comment

data ActionPermissionInsert
  = ActionPermissionInsert
  { _apiSet :: !(Map.HashMap PGCol J.Value)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionInsert)

newtype ActionMetadataField
  = ActionMetadataField { unActionMetadataField :: Text }
  deriving (Show, Eq, J.FromJSON, J.ToJSON)

data IfExists
  = IfExistsReplace
  | IfExistsError
  | IfExistsIgnore
  deriving (Show, Eq)

data ActionPermissionSelect
  = ActionPermissionSelect
  { _apsOutputFields :: ![PGCol]
  -- , _apsMetadataFields :: ![ActionMetadataField]
  , _apsFilter       :: !BoolExp
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionSelect)

data ActionPermissionDefinition
  = ActionPermissionDefinition
  { _apdInsert :: !ActionPermissionInsert
  , _apdSelect :: !ActionPermissionSelect
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionDefinition)

data CreateActionPermission
  = CreateActionPermission
  { _capAction     :: !ActionName
  , _capRole       :: !RoleName
  , _capDefinition :: !ActionPermissionDefinition
  , _capComment    :: !(Maybe Text)
  -- , _capIfExists   :: !(Maybe IfExists)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''CreateActionPermission)

runCreateActionPermission
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     )
  => CreateActionPermission -> m EncJSON
runCreateActionPermission createActionPermission = do
  adminOnly
  actionInfo <- getActionInfo actionName
  onJust (Map.lookup role $ _aiPermissions actionInfo) $ \_ ->
    throw400 AlreadyExists $
    "permission for role: " <> role <<> " is already defined on " <>> actionName

  -- process the field presets
  let inputFields = _adInputFields $ _aiDefintion actionInfo
  insertPermissionInfo <-
    fmap ActionPermissionInsertInfo $ processFieldPresets inputFields $
    _apiSet insertPermissionDefinition

  let outputFieldSet = Map.keysSet $ _adOutputFields $ _aiDefintion actionInfo

  let actionPermissionInfo =
        ActionPermissionInfo role insertPermissionInfo undefined

  -- TODO
  addActionPermissionToCache actionName actionPermissionInfo

  persistCreateActionPermission
  return successMsg
  where
    actionName = _capAction createActionPermission
    role = _capRole createActionPermission
    permissionDefinition = _capDefinition createActionPermission
    selectPermissionDefinition = _apdSelect permissionDefinition
    insertPermissionDefinition = _apdInsert permissionDefinition
    comment = _capComment createActionPermission

    persistCreateActionPermission :: (MonadTx m) => m ()
    persistCreateActionPermission = do
      liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
        INSERT into hdb_catalog.hdb_action_permission
          (action_name, role_name, definition, comment)
          VALUES ($1, $2, $3)
      |] (actionName, role, Q.AltJ permissionDefinition, comment) True

    processFieldPresets
      :: (QErrM m)
      => Map.HashMap PGCol PGColType
      -> Map.HashMap PGCol J.Value
      -> m PreSetColsPartial
    processFieldPresets inputFields fieldPresets = do
      flip Map.traverseWithKey fieldPresets $ \fieldName fieldValue -> do
        fieldType <- onNothing (Map.lookup fieldName inputFields) $
                     throw400 NotExists $ "no such field: " <> fieldName <<>
                     " exists in the input fields of action: " <>> actionName
        valueParser (PgTypeSimple fieldType) fieldValue

data DropActionPermission
  = DropActionPermission
  { _dapAction :: !ActionName
  , _dapRole   :: !RoleName
  -- , _capIfExists   :: !(Maybe IfExists)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''DropActionPermission)

runDropActionPermission
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     )
  => DropActionPermission -> m EncJSON
runDropActionPermission dropActionPermission = do
  adminOnly
  actionInfo <- getActionInfo actionName
  void $ onNothing (Map.lookup role $ _aiPermissions actionInfo) $
    throw400 NotExists $
    "permission for role: " <> role <<> " is not defined on " <>> actionName
  delActionPermissionFromCache actionName role
  liftTx deleteActionPermissionFromCatalog
  return successMsg
  where
    actionName = _dapAction dropActionPermission
    role = _dapRole dropActionPermission

    deleteActionPermissionFromCatalog :: Q.TxE QErr ()
    deleteActionPermissionFromCatalog =
      Q.unitQE defaultTxErrorHandler [Q.sql|
          DELETE FROM hdb_catalog.hdb_action_permission
            WHERE action_name = $1
              AND role_name = $2
          |] (actionName, role) True
