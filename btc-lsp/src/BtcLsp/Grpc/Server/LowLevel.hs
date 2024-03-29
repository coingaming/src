--
-- NOTE : requestBody is deprecated
-- but we need it.
--
{-# OPTIONS_GHC -Wno-deprecations #-}

module BtcLsp.Grpc.Server.LowLevel
  ( GSEnv (..),
    runServer,
    serverApp,
  )
where

import BtcLsp.Grpc.Data
import qualified BtcLsp.Grpc.Sig as Sig
import BtcLsp.Import.External
import Control.Concurrent (modifyMVar)
import Data.Aeson (withObject, (.:), (.:?))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Network.GRPC.HTTP2.Encoding (gzip)
import Network.GRPC.Server
import Network.HTTP2.Server hiding (Request)
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsMemory)
import Text.PrettyPrint.GenericPretty.Import (inspect)

data GSEnv = GSEnv
  { gsEnvPort :: Int,
    gsEnvSigVerify :: Bool,
    gsEnvSigHeaderName :: SigHeaderName,
    gsEnvEncryption :: Encryption,
    gsEnvTls :: Maybe (TlsData 'Server),
    gsEnvLogger :: Text -> IO (),
    gsEnvSigner :: Sig.MsgToSign -> IO (Maybe Sig.LndSig)
  }
  deriving stock (Generic)

instance FromJSON GSEnv where
  parseJSON =
    withObject
      "GSEnv"
      ( \x ->
          GSEnv
            <$> x .: "port"
            <*> x .: "sig_verify"
            <*> x .: "sig_header_name"
            <*> x .: "encryption"
            <*> x .:? "tls"
            <*> pure (const $ pure ())
            <*> pure (const $ pure Nothing)
      )

runServer ::
  GSEnv ->
  (GSEnv -> RawRequestBytes -> [ServiceHandler]) ->
  IO ()
runServer env handlers =
  case (gsEnvEncryption env, gsEnvTls env) of
    (Encrypted, Just tls) ->
      runTLS
        ( tlsSettingsMemory
            (TE.encodeUtf8 . unTlsCert $ tlsCert tls)
            (TE.encodeUtf8 . unTlsKey $ tlsKey tls)
        )
        (setPort port defaultSettings)
    (Encrypted, Nothing) ->
      error $
        "Fatal error - can not run LSP gRPC endpoint"
          <> " over TLS unless TlsData is provided!"
    (UnEncrypted, _) ->
      Warp.run port
    $ if gsEnvSigVerify env
      then extractBodyBytesMiddleware env $ serverApp handlers
      else serverApp handlers env (RawRequestBytes mempty)
  where
    port = gsEnvPort env

serverApp ::
  (GSEnv -> RawRequestBytes -> [ServiceHandler]) ->
  GSEnv ->
  RawRequestBytes ->
  Application
serverApp handlers env body req rep = do
  let app = grpcApp [gzip] $ handlers env body
  app req middleware
  where
    sigHeaderName =
      from $ gsEnvSigHeaderName env
    middleware res = do
      modifyHTTP2Data req $ \http2data0 ->
        let http2data = fromMaybe defaultHTTP2Data http2data0
         in Just $
              http2data
                { http2dataTrailers =
                    trailersMaker
                      mempty
                      (http2dataTrailers http2data)
                }
      rep $
        mapResponseHeaders
          (\hs -> ("trailer", sigHeaderName) : hs)
          res
    trailersMaker acc oldMaker Nothing = do
      ts <- oldMaker Nothing
      case ts of
        Trailers ss -> do
          mSig <- gsEnvSigner env $ Sig.MsgToSign acc
          pure $ case mSig of
            Nothing ->
              ts
            Just sig ->
              Trailers $
                ( CI.mk sigHeaderName,
                  B64.encode $ Sig.unLndSig sig
                ) :
                ss
        NextTrailersMaker {} ->
          throwIO $
            GRPCStatus
              INTERNAL
              "UNEXPECTED_NEW_TRAILERS_MAKER"
    trailersMaker acc oldMaker (Just bs) = do
      pure
        . NextTrailersMaker
        $ trailersMaker (acc <> bs) oldMaker

extractBodyBytesMiddleware ::
  GSEnv ->
  (GSEnv -> RawRequestBytes -> Application) ->
  Application
extractBodyBytesMiddleware env app req resp = do
  body <- BSL.toStrict <$> strictRequestBody req
  gsEnvLogger env $
    "Server ==> extracted raw request body"
      <> inspect body
  body' <- newMVar body
  app env (RawRequestBytes body) (req' body') resp
  where
    requestBody' mvar =
      modifyMVar
        mvar
        ( \b ->
            pure $
              if b == mempty
                then (mempty, mempty)
                else (mempty, b)
        )
    req' b =
      req
        { requestBody = requestBody' b
        }
