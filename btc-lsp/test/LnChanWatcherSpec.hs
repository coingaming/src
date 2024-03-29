module LnChanWatcherSpec
  ( spec,
  )
where

import BtcLsp.Import hiding (newEmptyMVar, putMVar, takeMVar)
import qualified BtcLsp.Storage.Model.LnChan as LnChan
import LndClient
import qualified LndClient.Data.ChannelPoint as Lnd
import qualified LndClient.Data.CloseChannel as Lnd
import LndClient.Data.GetInfo
import qualified LndClient.Data.OpenChannel as OpenChannel
import LndClient.LndTest
import qualified LndClient.RPC.Silent as Lnd
import Test.Hspec
import TestAppM
import UnliftIO.Concurrent (killThread, threadDelay)

openChannelRequest ::
  NodePubKey ->
  OpenChannel.OpenChannelRequest
openChannelRequest nodePubkey =
  OpenChannel.OpenChannelRequest
    { OpenChannel.nodePubkey = nodePubkey,
      OpenChannel.localFundingAmount = Msat 200000000,
      OpenChannel.pushMSat = Just $ Msat 10000000,
      OpenChannel.targetConf = Nothing,
      OpenChannel.mSatPerByte = Nothing,
      OpenChannel.private = Nothing,
      OpenChannel.minHtlcMsat = Nothing,
      OpenChannel.remoteCsvDelay = Nothing,
      OpenChannel.minConfs = Nothing,
      OpenChannel.spendUnconfirmed = Nothing,
      OpenChannel.closeAddress = Nothing,
      OpenChannel.fundingShim = Nothing
    }

closeChannelRequest ::
  Lnd.ChannelPoint ->
  Lnd.CloseChannelRequest
closeChannelRequest cp =
  Lnd.CloseChannelRequest cp False Nothing Nothing Nothing

getNodePubKey :: MonadUnliftIO m => LndEnv -> m NodePubKey
getNodePubKey lndEnv = do
  GetInfoResponse merchantPubKey _ _ <-
    liftLndResult =<< Lnd.getInfo lndEnv
  pure merchantPubKey

queryChannel ::
  ( Storage m
  ) =>
  Lnd.ChannelPoint ->
  m (Maybe (Entity LnChan))
queryChannel (Lnd.ChannelPoint txid vout) =
  runSql $
    LnChan.getByChannelPointSql txid vout

tryTimes ::
  MonadUnliftIO m =>
  Int ->
  Int ->
  m (Maybe a) ->
  m (Maybe a)
tryTimes times delaySec tryFn = go times
  where
    go 0 = pure Nothing
    go n = do
      res <- tryFn
      case res of
        Just r -> pure $ Just r
        Nothing -> threadDelay (delaySec * 1000000) >> go (n - 1)

justTrue :: Maybe Bool -> Maybe Bool
justTrue (Just True) = Just True
justTrue _ = Nothing

testFun ::
  ( LndTest m TestOwner,
    Storage m
  ) =>
  m [Maybe Bool]
testFun = do
  lndFrom <- getLndEnv LndLsp
  lndTo <- getLndEnv LndAlice
  toPubKey <- getNodePubKey lndTo
  cp <-
    liftLndResult
      =<< Lnd.openChannelSync
        lndFrom
        (openChannelRequest toPubKey)
  isPendingOpenOk <-
    tryTimes 5 1 $
      justTrue
        . fmap
          ( (== LnChanStatusPendingOpen)
              . lnChanStatus
              . entityVal
          )
        <$> queryChannel cp
  mine 10 LndLsp
  isOpenedOk <- tryTimes 5 1 $ do
    ch <- fmap entityVal <$> queryChannel cp
    let r =
          and
            <$> sequence
              [(== LnChanStatusActive) . lnChanStatus <$> ch]
    pure $ justTrue r
  isBackedUp <- tryTimes 5 1 $ do
    ch <- fmap entityVal <$> queryChannel cp
    let r =
          and
            <$> sequence
              [isJust . lnChanBak <$> ch]
    pure $ justTrue r
  (ctid, _) <- forkThread $ do
    void $
      liftLndResult
        =<< Lnd.closeChannel
          (const $ pure ())
          lndFrom
          (closeChannelRequest cp)
  isInactivedOk <- tryTimes 5 1 $ do
    ch <- fmap entityVal <$> queryChannel cp
    let r =
          and
            <$> sequence
              [(== LnChanStatusInactive) . lnChanStatus <$> ch]
    pure $ justTrue r
  mine 10 LndLsp
  isClosedOk <- tryTimes 5 1 $ do
    ch <- fmap entityVal <$> queryChannel cp
    let r =
          and
            <$> sequence
              [ ( `elem`
                    [ LnChanStatusFullyResolved,
                      LnChanStatusClosed
                    ]
                )
                  . lnChanStatus
                  <$> ch
              ]
    pure $ justTrue r
  void $ killThread ctid
  pure
    [ isPendingOpenOk,
      isOpenedOk,
      isBackedUp,
      isInactivedOk,
      isClosedOk
    ]

spec :: Spec
spec =
  itMain @'LndLsp "Watch channel" $ do
    r <- testFun
    liftIO $ r `shouldSatisfy` all (== Just True)
