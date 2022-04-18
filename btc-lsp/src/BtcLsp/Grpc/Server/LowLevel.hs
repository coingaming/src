{-# OPTIONS_GHC -Wno-deprecations #-}

module BtcLsp.Grpc.Server.LowLevel
  ( GSEnv (..),
    runServer,
    serverApp,
  )
where

import BtcLsp.Grpc.Data
import BtcLsp.Grpc.Sig
import BtcLsp.Import.Witch
import Control.Concurrent (modifyMVar)
import Data.Aeson (FromJSON (..), withObject, (.:))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Coerce (coerce)
import qualified Data.Text.Encoding as TE
import Network.GRPC.HTTP2.Encoding (gzip)
import Network.GRPC.Server
import Network.GRPC.Server.Wai (grpcApp)
import Network.HTTP2.Server hiding (Request)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsMemory)
import Network.Wai.Internal (Request (..))
import Text.PrettyPrint.GenericPretty.Import (inspect)
import Universum

data GSEnv = GSEnv
  { gsEnvPort :: GSPort,
    gsEnvSigVerify :: SigVerify,
    gsEnvSigHeaderName :: SigHeaderName,
    gsEnvTlsCert :: TlsCert 'Server,
    gsEnvTlsKey :: TlsKey 'Server,
    gsEnvLogger :: Text -> IO (),
    gsEnvSigner :: SigMsg -> IO (Maybe SigBytes)
  }
  deriving (Generic)

instance FromJSON GSEnv where
  parseJSON =
    withObject
      "GSEnv"
      ( \x ->
          GSEnv
            <$> x .: "port"
            <*> x .: "sig_verify"
            <*> x .: "sig_header_name"
            <*> x .: "tls_cert"
            <*> x .: "tls_key"
            <*> pure (const $ pure ())
            <*> pure (const $ pure Nothing)
      )

newtype GSPort
  = GSPort PortNumber
  deriving
    ( Enum,
      Eq,
      Integral,
      Num,
      Ord,
      Read,
      Real,
      Show
    )

runServer ::
  GSEnv ->
  (GSEnv -> RawRequestBytes -> [ServiceHandler]) ->
  IO ()
runServer env handlers =
  runTLS
    ( tlsSettingsMemory
        (TE.encodeUtf8 . coerce $ gsEnvTlsCert env)
        (TE.encodeUtf8 . coerce $ gsEnvTlsKey env)
    )
    (setPort (gsEnvPort env) defaultSettings)
    $ case gsEnvSigVerify env of
        SigVerify.Enabled -> extractBodyBytesMiddleware env $ serverApp handlers
        SigVerify.Disabled -> serverApp handlers env (RawRequestBytes mempty)

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
          mSig <- gsEnvSigner env acc
          pure $ case mSig of
            Nothing ->
              ts
            Just sig ->
              Trailers $
                ( CI.mk sigHeaderName,
                  B64.encode sig
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
