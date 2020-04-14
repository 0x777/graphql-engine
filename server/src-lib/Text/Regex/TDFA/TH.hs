{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Regex.TDFA.TH
  ( quoteRegex
  )where

import           Hasura.Prelude

import           Instances.TH.Lift             ()

import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Regex.TDFA            as TDFA
import qualified Text.Regex.TDFA.ReadRegex  as TDFA
import qualified Text.Regex.TDFA.TDFA       as TDFA
import qualified Text.Regex.TDFA.Pattern       as TDFA

-- | Quotes a regex using Template Haskell so syntax errors can be reported at
-- compile-time.
quoteRegex :: TDFA.CompOption -> TDFA.ExecOption -> String -> TH.Q (TH.TExp TDFA.Regex)
quoteRegex compOption execOption regexText = do
  regex <- TDFA.parseRegex regexText `onLeft` (fail . show)
  [|| TDFA.patternToRegex regex compOption execOption ||]

deriving instance TH.Lift TDFA.CompOption
deriving instance TH.Lift TDFA.DoPa
deriving instance TH.Lift TDFA.ExecOption
deriving instance TH.Lift TDFA.Pattern
deriving instance TH.Lift TDFA.PatternSet
deriving instance TH.Lift TDFA.PatternSetCharacterClass
deriving instance TH.Lift TDFA.PatternSetCollatingElement
deriving instance TH.Lift TDFA.PatternSetEquivalenceClass


