module Hasura.RQL.Types.Action
  ( ActionInfo(..)
  , ActionName(..)
  , ActionDefinition(..)

  , ActionPermissionSelectInfo(..)
  , ActionPermissionInsertInfo(..)
  , ActionPermissionInfo(..)

  , ActionPermissionMap
  ) where


import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Common     (NonEmptyText (..))
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax  (Lift)

import qualified Data.Aeson                  as J
import qualified Data.Aeson.Casing           as J
import qualified Data.Aeson.TH               as J
import qualified Data.HashMap.Strict         as Map
import qualified Data.HashSet                as Set
import qualified Database.PG.Query           as Q

newtype ActionName
  = ActionName { unActionName :: Text }
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey
           , Q.FromCol, Q.ToPrepArg, Hashable, DQuote, Lift)

type ResolvedWebhook = Text

data ActionDefinition a
  = ActionDefinition
  -- The table where this information is stored.
  { _adInputFields  :: !(Map.HashMap PGCol PGColType)
  , _adOutputFields :: !(Map.HashMap PGCol PGColType)
  , _adWebhook      :: !a
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''ActionDefinition)

type ResolvedActionDefinition = ActionDefinition ResolvedWebhook

data ActionPermissionSelectInfo
  = ActionPermissionSelectInfo
  { _apsiOutputFields :: !(Set.HashSet PGCol)
  , _apsiFilter       :: !AnnBoolExpPartialSQL
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 5 J.snakeCase) ''ActionPermissionSelectInfo)

data ActionPermissionInsertInfo
  = ActionPermissionInsertInfo
  { _apiiSet :: !PreSetColsPartial
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 5 J.snakeCase) ''ActionPermissionInsertInfo)

data ActionPermissionInfo
  = ActionPermissionInfo
  { _apiRole   :: !RoleName
  , _apiInsert :: !ActionPermissionInsertInfo
  , _apiSelect :: !ActionPermissionSelectInfo
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionInfo)

type ActionPermissionMap
  = Map.HashMap RoleName ActionPermissionInfo

newtype ActionMetadataField
  = ActionMetadataField { unActionMetadataField :: Text }
  deriving (Show, Eq, J.FromJSON, J.ToJSON)

data ActionInfo
  = ActionInfo
  { _aiName        :: !ActionName
  , _aiDefintion   :: !ResolvedActionDefinition
  , _aiPermissions :: !ActionPermissionMap
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''ActionInfo)
