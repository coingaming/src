module BtcLsp.Grpc.Client.LowLevel
  ( runUnary,
    GCEnv (..),
    GCPort (..),
  )
where

import BtcLsp.Grpc.Data
import BtcLsp.Grpc.Sig
import BtcLsp.Grpc.Orphan ()
import BtcLsp.Import.Witch
import Data.Aeson
  ( FromJSON (..),
    withObject,
    withScientific,
    (.:),
  )
import qualified Data.Binary.Builder as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Coerce (coerce)
import Data.ProtoLens (Message)
import Data.ProtoLens.Encoding (encodeMessage)
import Data.ProtoLens.Service.Types (HasMethod, HasMethodImpl (..))
import Data.Scientific (floatingOrInteger)
import GHC.TypeLits (Symbol)
import Network.GRPC.Client
import Network.GRPC.Client.Helpers
import Network.GRPC.HTTP2.Encoding (gzip)
import qualified Network.GRPC.HTTP2.Encoding as G
import qualified Network.GRPC.HTTP2.ProtoLens as ProtoLens
import Network.HTTP2.Client2
import Text.PrettyPrint.GenericPretty
  ( Out,
  )
import Text.PrettyPrint.GenericPretty.Import
  ( inspectPlain,
  )
import Universum

data GCEnv = GCEnv
  { gcEnvHost :: String,
    gcEnvPort :: GCPort,
    gcEnvSigHeaderName :: SigHeaderName,
    gcEnvCompressMode :: CompressMode,
    gcEnvSigner :: SigMsg -> IO (Maybe SigBytes)
  }
  deriving stock
    ( Generic
    )

instance FromJSON GCEnv where
  parseJSON =
    withObject
      "GCEnv"
      ( \x ->
          GCEnv
            <$> x .: "host"
            <*> x .: "port"
            <*> x .: "sig_header_name"
            <*> x .: "compress_mode"
            <*> pure (const $ pure Nothing)
      )

newtype GCPort
  = GCPort PortNumber
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

instance FromJSON GCPort where
  parseJSON =
    withScientific "GCPort" $ \x0 ->
      case floatingOrInteger x0 of
        Left (_ :: Double) -> fail "Non-integer"
        Right x -> pure x

runUnary ::
  ( Out res,
    Show res,
    HasMethod s m,
    req ~ MethodInput s m,
    res ~ MethodOutput s m
  ) =>
  ProtoLens.RPC s (m :: Symbol) ->
  GCEnv ->
  (res -> ByteString -> CompressMode -> IO Bool) ->
  req ->
  IO (Either Text res)
runUnary rpc env verifySig req = do
  res <-
    runClientIO $
      bracket
        (makeClient env req True)
        close
        (\grpc -> rawUnary rpc grpc req)
  --
  -- TODO : better composition with ExceptT
  --
  case res of
    Right (Right (Right (h, mh, Right x))) ->
      case find (\header -> fst header == sigHeaderName) $ h <> fromMaybe mempty mh of
        Nothing ->
          pure . Left $
            "Client ==> missing server header "
              <> inspectPlain sigHeaderName
        Just (_, b64sig) -> do
          let sigDer = B64.decodeLenient b64sig
          isVerified <-
            verifySig x sigDer $
              gcEnvCompressMode env
          pure $
            if isVerified
              then Right x
              else
                Left $
                  "Client ==> server signature verification failed for raw bytes "
                    <> " from decoded payload "
                    <> inspectPlain x
                    <> " with signature "
                    <> inspectPlain sigDer
    x ->
      --
      -- TODO : replace show with inspectPlain
      -- need additional instances for this.
      --
      pure . Left $
        "Client ==> server grpc failure "
          <> show x
  where
    sigHeaderName = CI.mk . from $ gcEnvSigHeaderName env

msgToSignBytes ::
  ( Message msg
  ) =>
  CompressMode ->
  msg ->
  ByteString
msgToSignBytes compressMode msg = header <> body
  where
    rawBody = encodeMessage msg
    body =
      case compressMode of
        Compressed -> G._compressionFunction G.gzip rawBody
        Uncompressed -> rawBody
    header =
      BS.pack
        [ case compressMode of
            Compressed -> 1
            Uncompressed -> 0
        ]
        <> ( BL.toStrict
               . BS.toLazyByteString
               . BS.putWord32be
               . fromIntegral
               $ BS.length body
           )

makeClient ::
  Message req =>
  GCEnv ->
  req ->
  UseTlsOrNot ->
  ClientIO GrpcClient
makeClient env req tlsEnabled = do
  mSignature <- liftIO doSignature
  case mSignature of
    Just signature ->
      setupGrpcClient $
        (grpcClientConfigSimple (gcEnvHost env) (coerce $ gcEnvPort env) tlsEnabled)
          { _grpcClientConfigCompression = compression,
            _grpcClientConfigHeaders =
              [ ( sigHeaderName,
                  B64.encode signature
                )
              ]
          }
    Nothing -> throwError EarlyEndOfStream
  where
    signer = gcEnvSigner env
    sigHeaderName = from $ gcEnvSigHeaderName env
    compressMode = gcEnvCompressMode env
    doSignature = signer $ msgToSignBytes compressMode req
    compression =
      case compressMode of
        Compressed -> gzip
        Uncompressed -> uncompressed
