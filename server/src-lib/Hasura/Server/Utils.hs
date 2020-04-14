{-# LANGUAGE TypeApplications #-}
module Hasura.Server.Utils where

import           Hasura.Prelude

import           Language.Haskell.TH.Syntax (Q, TExp)

import qualified Text.Regex.TDFA            as TDFA
import qualified Text.Regex.TDFA.ReadRegex  as TDFA
import qualified Text.Regex.TDFA.TDFA       as TDFA

import           Hasura.RQL.Instances       ()

userRoleHeader :: IsString a => a
userRoleHeader = "x-hasura-role"

deprecatedAccessKeyHeader :: IsString a => a
deprecatedAccessKeyHeader = "x-hasura-access-key"

adminSecretHeader :: IsString a => a
adminSecretHeader = "x-hasura-admin-secret"

userIdHeader :: IsString a => a
userIdHeader = "x-hasura-user-id"

-- | Quotes a regex using Template Haskell so syntax errors can be reported at
-- compile-time.
quoteRegex :: TDFA.CompOption -> TDFA.ExecOption -> String -> Q (TExp TDFA.Regex)
quoteRegex compOption execOption regexText = do
  regex <- TDFA.parseRegex regexText `onLeft` (fail . show)
  [|| TDFA.patternToRegex regex compOption execOption ||]

fmapL :: (a -> a') -> Either a b -> Either a' b
fmapL fn (Left e) = Left (fn e)
fmapL _ (Right x) = pure x

