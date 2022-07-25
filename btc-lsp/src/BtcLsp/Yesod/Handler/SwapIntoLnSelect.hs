{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module BtcLsp.Yesod.Handler.SwapIntoLnSelect
  ( getSwapIntoLnSelectR,
  )
where

import BtcLsp.Data.Type
import qualified BtcLsp.Math.Swap as Math
import BtcLsp.Storage.Model
import qualified BtcLsp.Storage.Model.SwapIntoLn as SwapIntoLn
import BtcLsp.Yesod.Data.Widget
import qualified BtcLsp.Yesod.Handler.SwapUpdates as SU
import BtcLsp.Yesod.Import
import qualified Data.UUID as UUID

getSwapIntoLnSelectR :: Uuid 'SwapIntoLnTable -> Handler Html
getSwapIntoLnSelectR uuid = do
  app@App {appMRunner = UnliftIO run} <- getYesod
  nodeUri <- liftIO $ run getLndNodeUri
  nodeUriHex <-
    eitherM
      (const badMethod)
      (pure . from @NodeUriHex @Text)
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
            $ from swapIntoLnFundAddress
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
            $ from swapIntoLnFeeLsp
        ),
        ( MsgSwapIntoLnChanCapUser,
          Just
            . MsgSatoshi
            $ from swapIntoLnChanCapUser
        ),
        ( MsgSwapIntoLnChanCapLsp,
          Just
            . MsgSatoshi
            $ from swapIntoLnChanCapLsp
        ),
        ( MsgSwapIntoLnChanCapTotal,
          Just
            . MsgSatoshi
            $ from swapIntoLnChanCapUser
              + from swapIntoLnChanCapLsp
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
            $ toText swapIntoLnFundAddress
        ),
        ( MsgSwapIntoLnRefundAddress,
          Just
            . MsgProxy
            $ toText swapIntoLnRefundAddress
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
        . coerce
        . userNodePubKey
        . entityVal
        $ SwapIntoLn.swapInfoUser swapInfo

totalOnChainAmt ::
  (SwapUtxoStatus -> Bool) ->
  SwapIntoLn.SwapInfo ->
  MSat
totalOnChainAmt only =
  from
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
                  . inspectPlain @Word64
                  $ from blockHeight
              ),
              ( MsgAmount,
                MsgSatoshi $
                  from swapUtxoAmount
              ),
              ( MsgStatus,
                swapUtxoStatusMsg swapUtxoStatus
              ),
              ( MsgTxId,
                MsgProxy
                  . txIdHex
                  $ coerce swapUtxoTxid
              ),
              ( MsgVout,
                MsgProxy
                  . inspectPlain @Word32
                  $ coerce swapUtxoVout
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
                  $ coerce lnChanFundingTxId
              ),
              ( MsgVout,
                MsgProxy
                  . inspectPlain @Word32
                  $ coerce lnChanFundingVout
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
