{-# LANGUAGE TypeApplications #-}
module Hasura.Server.Utils where

import           Hasura.Prelude

import           Hasura.RQL.Instances       ()

userRoleHeader :: IsString a => a
userRoleHeader = "x-hasura-role"

deprecatedAccessKeyHeader :: IsString a => a
deprecatedAccessKeyHeader = "x-hasura-access-key"

adminSecretHeader :: IsString a => a
adminSecretHeader = "x-hasura-admin-secret"

userIdHeader :: IsString a => a
userIdHeader = "x-hasura-user-id"

