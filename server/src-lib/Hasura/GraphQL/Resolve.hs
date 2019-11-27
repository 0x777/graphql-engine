module Hasura.GraphQL.Resolve
  ( mutFldToTx
  , queryFldToPGAST
  , RS.traverseQueryRootFldAST
  , RS.toPGQuery
  , UnresolvedVal(..)

  , AnnPGVal(..)
  , txtConverter

  , RS.QueryRootFldUnresolved
  , RIntro.schemaR
  , RIntro.typeR
  ) where

import           Data.Has

import qualified Data.HashMap.Strict               as Map
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.GraphQL.Resolve.Context
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Resolve.Insert     as RI
import qualified Hasura.GraphQL.Resolve.Introspect as RIntro
import qualified Hasura.GraphQL.Resolve.Mutation   as RM
import qualified Hasura.GraphQL.Resolve.Select     as RS
import qualified Hasura.GraphQL.Validate           as V

queryFldToPGAST
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     , Has QueryCtxMap r
     )
  => V.Field
  -> m RS.QueryRootFldUnresolved
queryFldToPGAST fld = do
  opCtx <- getOpCtx $ V._fName fld
  case opCtx of
    QCSelect ctx       -> RS.convertSelect ctx fld
    QCSelectPkey ctx   -> RS.convertSelectByPKey ctx fld
    QCSelectAgg ctx    -> RS.convertAggSelect ctx fld
    QCFuncQuery ctx    -> RS.convertFuncQuerySimple ctx fld
    QCFuncAggQuery ctx -> RS.convertFuncQueryAgg ctx fld

mutFldToTx
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , Has MutationCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has InsCtxMap r
     )
  => V.Field
  -> m RespTx
mutFldToTx fld = do
  opCtx <- getOpCtx $ V._fName fld
  case opCtx of
    MCInsert ctx -> RI.convertInsert (_iocTable ctx) fld
    MCUpdate ctx -> RM.convertUpdate ctx fld
    MCDelete ctx -> RM.convertDelete ctx fld

getOpCtx
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has (OpCtxMap a) r
     )
  => G.Name -> m a
getOpCtx f = do
  opCtxMap <- asks getter
  onNothing (Map.lookup f opCtxMap) $ throw500 $
    "lookup failed: opctx: " <> showName f
