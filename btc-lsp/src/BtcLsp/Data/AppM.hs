{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module BtcLsp.Data.AppM
  ( runApp,
    AppM (..),
  )
where

import BtcLsp.Data.Env as Env (Env (..))
import BtcLsp.Import as I
import qualified BtcLsp.Import.Psql as Psql
import qualified LndClient.Data.WalletBalance as Lnd
import qualified Text.Pretty.Simple as PrettySimple

newtype AppM m a = AppM
  { unAppM :: ReaderT Env.Env m a
  }
  deriving stock (Functor)
  deriving newtype
    ( Applicative,
      Monad,
      MonadIO,
      MonadReader Env.Env,
      MonadUnliftIO
    )

runApp :: Env.Env -> AppM m a -> m a
runApp env app = runReaderT (unAppM app) env

instance (MonadIO m) => Katip (AppM m) where
  getLogEnv = asks envKatipLE
  localLogEnv f (AppM m) =
    AppM (local (\s -> s {envKatipLE = f (envKatipLE s)}) m)

instance (MonadIO m) => KatipContext (AppM m) where
  getKatipContext = asks envKatipCTX
  localKatipContext f (AppM m) =
    AppM (local (\s -> s {envKatipCTX = f (envKatipCTX s)}) m)
  getKatipNamespace = asks envKatipNS
  localKatipNamespace f (AppM m) =
    AppM (local (\s -> s {envKatipNS = f (envKatipNS s)}) m)

instance (MonadUnliftIO m) => BtcEnv (AppM m) Failure where
  getBtcCfg = asks Env.envBtcCfg
  getBtcClient = asks Env.envBtc
  handleBtcFailure = handleBtcFailureGen

instance (MonadUnliftIO m) => I.Env (AppM m) where
  getGsEnv =
    asks Env.envGrpcServer
  getSwapIntoLnMinAmt =
    asks Env.envSwapIntoLnMinAmt
  getMsatPerByte =
    asks Env.envMsatPerByte
  getLspPubKeyVar =
    asks Env.envLndPubKey
  getLspLndEnv =
    asks Env.envLnd
  getYesodLog =
    asks Env.envYesodLog
  getLogStyle =
    asks Env.envLogStyle
  getLndP2PSocketAddress = do
    host <- asks Env.envLndP2PHost
    port <- asks Env.envLndP2PPort
    pure
      SocketAddress
        { socketAddressHost = host,
          socketAddressPort = port
        }
  withLnd method args = do
    lnd <- asks Env.envLnd
    first (const $ FailureInt FailureRedacted) <$> args (method lnd)
  monitorTotalExtOutgoingLiquidity amt = do
    lim <- asks Env.envMinTotalExtOutgoingLiquidity
    Inspect inspect <- getInspect
    when (amt < lim) $
      $(logTM) CriticalS . logStr $
        "Not enough outgoing liquidity to the external "
          <> "lightning network, got "
          <> inspect @Text amt
          <> " but minimum is "
          <> inspect lim
          <> "."
  monitorTotalExtIncomingLiquidity amt = do
    lim <- asks Env.envMinTotalExtIncomingLiquidity
    Inspect inspect <- getInspect
    when (amt < lim) $
      $(logTM) CriticalS . logStr $
        "Not enough incoming liquidity from the external "
          <> "lightning network, got "
          <> inspect @Text amt
          <> " but minimum is "
          <> inspect lim
          <> "."
  monitorTotalOnChainLiquidity wal = do
    lim <- asks Env.envMinTotalOnChainLiquidity
    Inspect inspect <- getInspect
    when (Lnd.totalBalance wal < lim) $
      $(logTM) CriticalS . logStr $
        "Not enough onchain liquidity, got "
          <> inspect @Text wal
          <> " but minimum is "
          <> inspect lim
          <> "."

instance (MonadUnliftIO m) => Storage (AppM m) where
  getSqlPool = asks envSQLPool
  runSql query = do
    pool <- asks envSQLPool
    Psql.runSqlPool query pool

instance (MonadIO m, MonadUnliftIO m) => GenericPrettyEnv (AppM m) where
  getStyle = do
    logStyle <- getLogStyle
    case logStyle of
      DarkBg -> pure PrettySimple.defaultOutputOptionsDarkBg
      LightBg -> pure PrettySimple.defaultOutputOptionsLightBg
      NoColor -> pure PrettySimple.defaultOutputOptionsNoColor
