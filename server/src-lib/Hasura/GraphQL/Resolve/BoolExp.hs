{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.GraphQL.Resolve.BoolExp
  ( parseBoolExp
  , pgColValToBoolExpG
  , pgColValToBoolExp
  -- , convertBoolExpG
  , convertBoolExp
  , prepare
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Language.GraphQL.Draft.Syntax     as G

-- import qualified Hasura.RQL.GBoolExp               as RA
-- import qualified Hasura.RQL.GBoolExp               as RG
import qualified Hasura.SQL.DML                    as S

import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types

import           Hasura.SQL.Types
import           Hasura.SQL.Value

type OpExp = OpExpG (PGColType, PGColValue)

parseOpExps
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => AnnGValue -> m [OpExp]
parseOpExps annVal = do
  opExpsM <- flip withObjectM annVal $ \nt objM -> forM objM $ \obj ->
    forM (OMap.toList obj) $ \(k, v) -> case k of
      "_eq"           -> fmap AEQ <$> asPGColValM v
      "_ne"           -> fmap ANE <$> asPGColValM v
      "_neq"          -> fmap ANE <$> asPGColValM v
      "_is_null"      -> resolveIsNull v

      "_in"           -> fmap (AIN . catMaybes) <$> parseMany asPGColValM v
      "_nin"          -> fmap (ANIN . catMaybes) <$> parseMany asPGColValM v

      "_gt"           -> fmap AGT <$> asPGColValM v
      "_lt"           -> fmap ALT <$> asPGColValM v
      "_gte"          -> fmap AGTE <$> asPGColValM v
      "_lte"          -> fmap ALTE <$> asPGColValM v

      "_like"         -> fmap ALIKE <$> asPGColValM v
      "_nlike"        -> fmap ANLIKE <$> asPGColValM v

      "_ilike"        -> fmap AILIKE <$> asPGColValM v
      "_nilike"       -> fmap ANILIKE <$> asPGColValM v

      "_similar"      -> fmap ASIMILAR <$> asPGColValM v
      "_nsimilar"     -> fmap ANSIMILAR <$> asPGColValM v

      -- jsonb related operators
      "_contains"     -> fmap AContains <$> asPGColValM v
      "_contained_in" -> fmap AContainedIn <$> asPGColValM v
      "_has_key"      -> fmap AHasKey <$> asPGColValM v
      "_has_keys_any" -> fmap AHasKeysAny <$> parseMany asPGColText v
      "_has_keys_all" -> fmap AHasKeysAll <$> parseMany asPGColText v

      _ ->
        throw500
          $  "unexpected operator found in opexp of "
          <> showNamedTy nt
          <> ": "
          <> showName k
  return $ catMaybes $ fromMaybe [] opExpsM
  where
    resolveIsNull v = case v of
      AGScalar _ Nothing -> return Nothing
      AGScalar _ (Just (PGValBoolean b)) ->
        return $ Just $ bool ANISNOTNULL ANISNULL b
      AGScalar _ _ -> throw500 "boolean value is expected"
      _ -> tyMismatch "pgvalue" v

parseAsEqOp
  :: (MonadError QErr m)
  => AnnGValue -> m [OpExp]
parseAsEqOp annVal = do
  annValOpExp <- AEQ <$> asPGColVal annVal
  return [annValOpExp]

parseColExp
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => ((PGColType, PGColValue) -> m S.SQLExp)
  -> G.NamedType
  -> G.Name
  -> AnnGValue
  -> (AnnGValue -> m [OpExp])
  -> m AnnBoolExpFldSQL
parseColExp f nt n val expParser = do
  fldInfo <- getFldInfo nt n
  case fldInfo of
    Left  pgColInfo -> do
      opExps <- expParser val
      AVCol pgColInfo <$> traverse (traverse f) opExps
    Right (relInfo, _, permExp, _) -> do
      relBoolExp <- parseBoolExp f val
      return $ AVRel relInfo $ andAnnBoolExps relBoolExp permExp

parseBoolExp
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => ((PGColType, PGColValue) -> m S.SQLExp)
  -> AnnGValue
  -> m AnnBoolExpSQL
parseBoolExp f annGVal = do
  boolExpsM <-
    flip withObjectM annGVal
      $ \nt objM -> forM objM $ \obj -> forM (OMap.toList obj) $ \(k, v) -> if
          | k == "_or"  -> BoolOr . fromMaybe []
                           <$> parseMany (parseBoolExp f) v
          | k == "_and" -> BoolAnd . fromMaybe []
                           <$> parseMany (parseBoolExp f) v
          | k == "_not" -> BoolNot <$> parseBoolExp f v
          | otherwise   -> BoolFld <$> parseColExp f nt k v parseOpExps
  return $ BoolAnd $ fromMaybe [] boolExpsM

-- convertBoolExpG
--   :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
--   => ((PGColType, PGColValue) -> m S.SQLExp)
--   -> QualifiedTable
--   -> AnnGValue
--   -> m AnnBoolExpSQL
-- convertBoolExpG f tn whereArg = do
--   whereExp <- parseBoolExp f whereArg
--   traverse f whereExp

convertBoolExp
  :: AnnGValue
  -> Convert AnnBoolExpSQL
convertBoolExp =
  parseBoolExp prepare

type PGColValMap = Map.HashMap G.Name AnnGValue

pgColValToBoolExpG
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => ((PGColType, PGColValue) -> m S.SQLExp)
  -> PGColValMap
  -> m AnnBoolExpSQL
pgColValToBoolExpG f colValMap = do
  colExps <- forM colVals $ \(name, valR) -> do
    (ty, val) <- asPGColVal valR
    let namedTy = mkScalarTy ty
    BoolFld <$> parseColExp f namedTy name valR parseAsEqOp
  return $ BoolAnd colExps
  where
    colVals = Map.toList colValMap

pgColValToBoolExp
  :: PGColValMap
  -> Convert AnnBoolExpSQL
pgColValToBoolExp =
  pgColValToBoolExpG prepare
