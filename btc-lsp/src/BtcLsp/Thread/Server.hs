{-# LANGUAGE TypeApplications #-}

module BtcLsp.Thread.Server
  ( apply,
  )
where

import BtcLsp.Import hiding (Sig (..))
import BtcLsp.ProtoLensGrpc.Data
import qualified BtcLsp.Storage.Model.User as User
import Data.ProtoLens.Field
import Data.ProtoLens.Message
--
-- TODO : add to global imports?
--
import Data.Type.Equality (type (==))
import Lens.Micro
import Network.GRPC.HTTP2.ProtoLens (RPC (..))
import Network.GRPC.Server
import qualified Network.Wai.Internal as Wai
import Proto.BtcLsp (Service)
import qualified Proto.BtcLsp.Data.HighLevel as Proto
import qualified Proto.BtcLsp.Data.HighLevel_Fields as Proto
import qualified Proto.BtcLsp.Method.GetCfg as GetCfg
import qualified Proto.BtcLsp.Method.SwapFromLn as SwapFromLn
import qualified Proto.BtcLsp.Method.SwapIntoLn as SwapIntoLn

apply :: (Env m) => m ()
apply = do
  env <- getGsEnv
  withUnliftIO $ \run ->
    runServer env $ handlers run

handlers ::
  ( Env m
  ) =>
  UnliftIO m ->
  GSEnv ->
  MVar (Sig 'Server) ->
  [ServiceHandler]
handlers run _ _ =
  [ unary (RPC :: RPC Service "getCfg") $
      withMiddleware run getCfg,
    unary (RPC :: RPC Service "swapIntoLn") $
      withMiddleware run swapIntoLn,
    unary (RPC :: RPC Service "swapFromLn") $
      withMiddleware run swapFromLn
  ]

withMiddleware ::
  ( HasField req "maybe'ctx" (Maybe Proto.Ctx),
    Message req,
    Env m
  ) =>
  UnliftIO m ->
  (req -> m res) ->
  Wai.Request ->
  req ->
  IO res
withMiddleware (UnliftIO run) handler waiReq protoReq =
  --
  -- TODO : !!!
  --
  run $ do
    res <- runExceptT $ do
      nonce <-
        fromReqT $
          protoReq
            ^? field @"maybe'ctx"
              . _Just
              . Proto.maybe'nonce
              . _Just
      pub <-
        fromReqT $
          protoReq
            ^? field @"maybe'ctx"
              . _Just
              . Proto.maybe'lnPubKey
              . _Just
      lift $
        User.createVerify pub nonce
    case res of
      Left {} -> undefined
      Right {} -> handler protoReq

fromReqT ::
  forall a b m.
  ( Monad m,
    From a b,
    'False ~ (a == b)
  ) =>
  Maybe a ->
  ExceptT Failure m b
fromReqT =
  except
    . fromReqE

fromReqE ::
  forall a b.
  ( From a b,
    'False ~ (a == b)
  ) =>
  Maybe a ->
  Either Failure b
fromReqE =
  (from <$>)
    . maybeToRight
      --
      -- TODO : replace with real error
      --
      (FailureInput defMessage)

getCfg ::
  ( Monad m
  ) =>
  GetCfg.Request ->
  m GetCfg.Response
getCfg _ =
  pure defMessage

swapIntoLn ::
  ( Monad m
  ) =>
  SwapIntoLn.Request ->
  m SwapIntoLn.Response
swapIntoLn _ =
  pure defMessage

swapFromLn ::
  ( Monad m
  ) =>
  SwapFromLn.Request ->
  m SwapFromLn.Response
swapFromLn _ =
  pure defMessage
