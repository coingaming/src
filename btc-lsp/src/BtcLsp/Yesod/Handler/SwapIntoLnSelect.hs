{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module BtcLsp.Yesod.Handler.SwapIntoLnSelect
  ( getSwapIntoLnSelectR,
  )
where

import BtcLsp.Data.Smart
import BtcLsp.Data.Type
import qualified BtcLsp.Math.Swap as Math
import BtcLsp.Storage.Model
import qualified BtcLsp.Storage.Model.SwapIntoLn as SwapIntoLn
import BtcLsp.Yesod.Data.Widget
import qualified BtcLsp.Yesod.Handler.SwapUpdates as SU
import BtcLsp.Yesod.Import
import qualified Data.UUID as UUID
import qualified LndClient as Lnd

getSwapIntoLnSelectR :: Uuid 'SwapIntoLnTable -> Handler Html
getSwapIntoLnSelectR uuid = do
  app@App {appMRunner = UnliftIO run} <- getYesod
  nodeUri <- liftIO $ run getLndNodeUri
  nodeUriHex <-
    eitherM
      (const badMethod)
      (pure . unNodeUriHex)
      . pure
      $ tryFrom nodeUri
  nodeUriQr <-
    maybeM badMethod pure
      . pure
      $ toQr nodeUriHex
  swapHash <- maybeM notFound pure (SU.getSwapUpdate app uuid)
  maybeM
    notFound
    ( \swapInfo@SwapIntoLn.SwapInfo {..} -> do
        minAmt <- liftIO $ run getSwapIntoLnMinAmt
        let SwapIntoLn {..} = entityVal swapInfoSwap
        let (msgShort, msgLong, color) =
              case swapIntoLnStatus of
                SwapWaitingFundChain ->
                  ( MsgSwapIntoLnWaitingFundChainShort,
                    MsgSwapIntoLnWaitingFundChainLong
                      minAmt
                      Math.swapLnMaxAmt,
                    Info
                  )
                SwapWaitingPeer ->
                  ( MsgSwapIntoLnFundedShort,
                    MsgSwapIntoLnFundedLong,
                    Info
                  )
                SwapInPsbtThread ->
                  ( MsgSwapIntoLnWaitingChanShort,
                    MsgSwapIntoLnWaitingChanLong,
                    Info
                  )
                SwapWaitingChan ->
                  ( MsgSwapIntoLnWaitingChanShort,
                    MsgSwapIntoLnWaitingChanLong,
                    Info
                  )
                SwapSucceeded ->
                  ( MsgSwapIntoLnSucceededShort,
                    MsgSwapIntoLnSucceededLong,
                    Success
                  )
                SwapExpired ->
                  ( MsgSwapIntoLnExpiredShort,
                    MsgSwapIntoLnExpiredLong,
                    Danger
                  )
        fundAddrQr <-
          maybeM badMethod pure
            . pure
            . toQr
            $ unOnChainAddress swapIntoLnFundAddress
        let mSwapWidget = newSwapWidget swapInfo
        let mUtxoWidget = newUtxoWidget swapInfoUtxo
        let mChanWidget = newChanWidget swapInfoChan
        panelLayout color msgShort msgLong $ do
          setTitleI $ MsgSwapIntoLnSelectRTitle swapIntoLnUuid
          $(widgetFile "swap_updates")
          $(widgetFile "swap_into_ln_select")
    )
    . liftIO
    . run
    . runSql
    $ SwapIntoLn.getByUuidSql uuid
  where
    htmlUuid = $(mkHtmlUuid)

newSwapWidget ::
  SwapIntoLn.SwapInfo ->
  Maybe Widget
newSwapWidget swapInfo =
  newNamedListWidget MsgSwapIntoLnHeaderInfo
    . singleton
    $ [ ( MsgSwapIntoLnTotalOnChainReceived,
          Just
            . MsgSatoshi
            $ totalOnChainAmt (/= SwapUtxoOrphan) swapInfo
        ),
        ( MsgSwapIntoLnTotalOnChainReserved,
          Just
            . MsgSatoshi
            $ totalOnChainAmt
              ( `elem`
                  [ SwapUtxoUnspentChanReserve
                  ]
              )
              swapInfo
        ),
        ( MsgSwapIntoLnTotalOnChainSwapped,
          Just
            . MsgSatoshi
            $ totalOnChainAmt
              (== SwapUtxoSpentChanSwapped)
              swapInfo
        ),
        ( MsgSwapIntoLnTotalOnChainRefunded,
          Just
            . MsgSatoshi
            $ totalOnChainAmt (== SwapUtxoSpentRefund) swapInfo
        ),
        ( MsgSwapIntoLnTotalOnChainDust,
          Just
            . MsgSatoshi
            $ totalOnChainAmt (== SwapUtxoUnspentDust) swapInfo
        ),
        ( MsgSwapIntoLnFeeLsp,
          Just
            . MsgSatoshi
            $ unMoney swapIntoLnFeeLsp
        ),
        ( MsgSwapIntoLnChanCapUser,
          Just
            . MsgSatoshi
            $ unMoney swapIntoLnChanCapUser
        ),
        ( MsgSwapIntoLnChanCapLsp,
          Just
            . MsgSatoshi
            $ unMoney swapIntoLnChanCapLsp
        ),
        ( MsgSwapIntoLnChanCapTotal,
          Just
            . MsgSatoshi
            $ unMoney swapIntoLnChanCapUser
              + unMoney swapIntoLnChanCapLsp
        ),
        ( MsgChannelPrivacy,
          Just $
            chanPrivacyMsg swapIntoLnPrivacy
        ),
        ( MsgStatus,
          Just $
            swapStatusMsg swapIntoLnStatus
        ),
        ( MsgExpiresAt,
          Just $
            MsgUtcTime swapIntoLnExpiresAt
        ),
        ( MsgSwapIntoLnUuid,
          Just
            . MsgProxy
            . UUID.toText
            . unUuid
            $ swapIntoLnUuid
        ),
        ( MsgSwapIntoLnUserId,
          Just $
            MsgProxy userPub
        ),
        ( MsgSwapIntoLnFundAddress,
          Just
            . MsgProxy
            $ unOnChainAddress swapIntoLnFundAddress
        ),
        ( MsgSwapIntoLnRefundAddress,
          Just
            . MsgProxy
            $ unOnChainAddress swapIntoLnRefundAddress
        ),
        ( MsgInsertedAt,
          Just $
            MsgUtcTime swapIntoLnInsertedAt
        ),
        ( MsgUpdatedAt,
          Just $
            MsgUtcTime swapIntoLnUpdatedAt
        )
      ]
      >>= \case
        (msg, Just txt) -> [(msg, txt)]
        (_, Nothing) -> []
  where
    SwapIntoLn {..} =
      entityVal $
        SwapIntoLn.swapInfoSwap swapInfo
    userPub =
      toHex
        . unNodePubKey
        . userNodePubKey
        . entityVal
        $ SwapIntoLn.swapInfoUser swapInfo

totalOnChainAmt ::
  (SwapUtxoStatus -> Bool) ->
  SwapIntoLn.SwapInfo ->
  Msat
totalOnChainAmt only =
  unMoney
    . sum
    . fmap swapUtxoAmount
    . filter (only . swapUtxoStatus)
    . fmap (entityVal . SwapIntoLn.utxoInfoUtxo)
    . SwapIntoLn.swapInfoUtxo

newUtxoWidget :: [SwapIntoLn.UtxoInfo] -> Maybe Widget
newUtxoWidget utxos =
  newNamedListWidget MsgSwapIntoLnHeaderUtxos $
    ( \row ->
        let SwapUtxo {..} =
              entityVal $ SwapIntoLn.utxoInfoUtxo row
            Block {..} =
              entityVal $ SwapIntoLn.utxoInfoBlock row
         in [ ( MsgBlock,
                MsgProxy
                  . inspectPlain @Text @Word64
                  $ unBlkHeight blockHeight
              ),
              ( MsgAmount,
                MsgSatoshi $
                  unMoney swapUtxoAmount
              ),
              ( MsgStatus,
                swapUtxoStatusMsg swapUtxoStatus
              ),
              ( MsgTxId,
                MsgProxy
                  . txIdHex
                  $ Lnd.unTxId swapUtxoTxid
              ),
              ( MsgVout,
                MsgProxy
                  . inspectPlain
                  $ unVout swapUtxoVout
              ),
              ( MsgInsertedAt,
                MsgUtcTime swapUtxoInsertedAt
              ),
              ( MsgUpdatedAt,
                MsgUtcTime swapUtxoUpdatedAt
              )
            ]
    )
      <$> utxos

newChanWidget :: [Entity LnChan] -> Maybe Widget
newChanWidget chans =
  newNamedListWidget MsgSwapIntoLnHeaderChans $
    ( \row ->
        let LnChan {..} = entityVal row
         in [ ( MsgStatus,
                lnChanStatusMsg lnChanStatus
              ),
              ( MsgTxId,
                MsgProxy
                  . txIdHex
                  $ Lnd.unTxId lnChanFundingTxId
              ),
              ( MsgVout,
                MsgProxy
                  . inspectPlain
                  $ unVout lnChanFundingVout
              ),
              ( MsgInsertedAt,
                MsgUtcTime lnChanInsertedAt
              ),
              ( MsgUpdatedAt,
                MsgUtcTime lnChanUpdatedAt
              )
            ]
    )
      <$> chans

swapStatusMsg :: SwapStatus -> AppMessage
swapStatusMsg = \case
  SwapWaitingFundChain -> MsgSwapWaitingFundChain
  SwapWaitingPeer -> MsgSwapWaitingPeer
  SwapInPsbtThread -> MsgSwapWaitingChan
  SwapWaitingChan -> MsgSwapWaitingChan
  SwapSucceeded -> MsgSwapSucceeded
  SwapExpired -> MsgSwapExpired

chanPrivacyMsg :: Privacy -> AppMessage
chanPrivacyMsg = \case
  Private -> MsgChanPrivate
  Public -> MsgChanPublic

swapUtxoStatusMsg :: SwapUtxoStatus -> AppMessage
swapUtxoStatusMsg = \case
  SwapUtxoUnspent -> MsgSwapUtxoUnspent
  SwapUtxoUnspentDust -> MsgSwapUtxoUnspentDust
  SwapUtxoUnspentChanReserve -> MsgSwapUtxoUnspentChanReserve
  SwapUtxoSpentChanSwapped -> MsgSwapUtxoSpentChanSwapped
  SwapUtxoSpentRefund -> MsgSwapUtxoSpentRefund
  SwapUtxoOrphan -> MsgSwapUtxoOrphan

lnChanStatusMsg :: LnChanStatus -> AppMessage
lnChanStatusMsg = \case
  LnChanStatusPendingOpen -> MsgLnChanStatusPendingOpen
  LnChanStatusOpened -> MsgLnChanStatusOpened
  LnChanStatusActive -> MsgLnChanStatusActive
  LnChanStatusFullyResolved -> MsgLnChanStatusFullyResolved
  LnChanStatusInactive -> MsgLnChanStatusInactive
  LnChanStatusPendingClose -> MsgLnChanStatusPendingClose
  LnChanStatusClosed -> MsgLnChanStatusClosed
