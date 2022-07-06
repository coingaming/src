{-# LANGUAGE TemplateHaskell #-}

module BtcLsp.Psbt.PsbtOpener (openChannelPsbt) where

import BtcLsp.Import
import qualified BtcLsp.Math.OnChain as Math
import BtcLsp.Psbt.Utils
  ( finalizePsbt,
    fundPsbtReq,
    lockUtxos,
    openChannelReq,
    psbtFinalizeReq,
    psbtVerifyReq,
    releaseUtxosLocks,
    releaseUtxosPsbtLocks,
    shimCancelReq,
    unspendUtxoLookup,
  )
import qualified Data.Map as M
import qualified LndClient as Lnd
import qualified LndClient.Data.ChannelPoint as Lnd
import qualified LndClient.Data.FinalizePsbt as FNP
import qualified LndClient.Data.FundPsbt as FP
import qualified LndClient.Data.ListUnspent as LU
import qualified LndClient.Data.OpenChannel as Lnd
import qualified LndClient.Data.OutPoint as OP
import qualified LndClient.RPC.Katip as Lnd
import qualified UnliftIO.Exception as UE
import qualified UnliftIO.STM as T

sumAmt :: [PsbtUtxo] -> MSat
sumAmt utxos = sum $ getAmt <$> utxos

autoSelectUtxos :: Env m => OnChainAddress 'Fund -> MSat -> ExceptT Failure m FP.FundPsbtResponse
autoSelectUtxos addr amt = withLndT Lnd.fundPsbt ($ req)
  where
    req = fundPsbtReq $ FP.TxTemplate [] (M.fromList [(unOnChainAddress addr, amt)])

utxoLeaseToPsbtUtxo :: Map OP.OutPoint LU.Utxo -> FP.UtxoLease -> Maybe PsbtUtxo
utxoLeaseToPsbtUtxo l ul = psbtUtxo . LU.amountSat <$> M.lookup op l
  where
    op = FP.outpoint ul
    psbtUtxo amt =
      PsbtUtxo
        { getAmt = amt,
          getLockId = Just . UtxoLockId $ FP.id ul,
          getOutPoint = op
        }

mapLeaseUtxosToPsbtUtxo :: Env m => [FP.UtxoLease] -> ExceptT Failure m [PsbtUtxo]
mapLeaseUtxosToPsbtUtxo lockedUtxos = do
  releaseUtxosLocks lockedUtxos
  l <- unspendUtxoLookup
  newLockedUtxos <- lockUtxos (FP.outpoint <$> lockedUtxos)
  case sequence $ utxoLeaseToPsbtUtxo l <$> newLockedUtxos of
    Just us -> pure us
    Nothing -> do
      $(logTM) DebugS $
        logStr $
          "Cannot find utxo in utxos:" <> inspect lockedUtxos <> " lookupMap: " <> inspect l
      throwE $ FailureInternal "Cannot find utxo in unspent list"

fundChanPsbt ::
  (Env m) =>
  [PsbtUtxo] ->
  OnChainAddress 'Fund ->
  OnChainAddress 'Gain ->
  Money 'Lsp 'OnChain 'Gain ->
  ExceptT Failure m Lnd.Psbt
fundChanPsbt userUtxos chanFundAddr changeAddr lspFee = do
  let userFundingAmt = sumAmt userUtxos - coerce lspFee

  $(logTM) DebugS $
    logStr $
      "UserAmt:"
        <> inspect (sumAmt userUtxos)
        <> " LspFee:"
        <> inspect lspFee

  lspFunded <- autoSelectUtxos (coerce chanFundAddr) userFundingAmt
  lspUtxos <- mapLeaseUtxosToPsbtUtxo $ FP.lockedUtxos lspFunded
  let selectedInputsAmt = sumAmt lspUtxos
  $(logTM) DebugS $ logStr $ "Coins sum by lsp" <> inspect selectedInputsAmt
  let allInputs = getOutPoint <$> (userUtxos <> lspUtxos)
  numInps <- tryFromT (length allInputs)
  estFee <- tryFailureT $ Math.trxEstFee (Math.InQty numInps) (Math.OutQty 2) Math.minFeeRate
  -- TODO: find exact additional cost of open trx
  let fee = estFee + MSat 50000
  $(logTM) DebugS $ logStr $ "Est fee:" <> inspect fee
  let changeAmt = selectedInputsAmt - userFundingAmt + coerce lspFee - fee
  let outputs =
        if changeAmt > Math.trxDustLimit
          then
            [ (unOnChainAddress chanFundAddr, userFundingAmt * 2),
              (unOnChainAddress changeAddr, changeAmt)
            ]
          else
            [ (unOnChainAddress chanFundAddr, userFundingAmt * 2 + changeAmt)
            ]
  let req = fundPsbtReq $ FP.TxTemplate allInputs (M.fromList outputs)
  releaseUtxosPsbtLocks (userUtxos <> lspUtxos)
  psbt <- withLndT Lnd.fundPsbt ($ req)
  pure $ Lnd.Psbt $ FP.fundedPsbt psbt

data OpenUpdateEvt = LndUpdate Lnd.OpenStatusUpdate | LndSubFail deriving stock (Generic)

instance Out OpenUpdateEvt

openChannelPsbt ::
  Env m =>
  [PsbtUtxo] ->
  NodePubKey ->
  OnChainAddress 'Gain ->
  Money 'Lsp 'OnChain 'Gain ->
  ExceptT Failure m Lnd.ChannelPoint
openChannelPsbt utxos toPubKey changeAddress lspFee = do
  chan <- lift T.newTChanIO
  pcid <- Lnd.newPendingChanId
  let openChannelRequest = openChannelReq pcid toPubKey (coerce (2 * amt)) (coerce amt)
  let subUpdates u = void . T.atomically . T.writeTChan chan $ LndUpdate u
  res <- lift . UE.tryAny . spawnLink $ do
    r <- withLnd (Lnd.openChannel subUpdates) ($ openChannelRequest)
    whenLeft r $ \e -> do
      $(logTM) ErrorS $ logStr $ "Open channel failed" <> inspect e
      void . T.atomically . T.writeTChan chan $ LndSubFail
  case res of
    Left e -> throwE $ FailureInternal $ inspect e
    Right _ -> fundStep pcid chan
  where
    amt = sumAmt utxos - coerce lspFee
    fundStep pcid chan = do
      upd <- T.atomically $ T.readTChan chan
      $(logTM) DebugS $ logStr $ "Got chan status update" <> inspect upd
      case upd of
        LndUpdate (Lnd.OpenStatusUpdate _ (Just (Lnd.OpenStatusUpdatePsbtFund (Lnd.ReadyForPsbtFunding faddr famt _)))) -> do
          $(logTM) DebugS $ logStr $ "Chan ready for funding at addr:" <> inspect faddr <> " with amt:" <> inspect famt
          psbt' <- fundChanPsbt utxos (unsafeNewOnChainAddress faddr) (coerce changeAddress) lspFee
          void $ withLndT Lnd.fundingStateStep ($ psbtVerifyReq pcid psbt')
          sPsbtResp <- finalizePsbt psbt'
          $(logTM) DebugS $ logStr $ "Used psbt for funding:" <> inspect sPsbtResp
          void $ withLndT Lnd.fundingStateStep ($ psbtFinalizeReq pcid (Lnd.Psbt $ FNP.signedPsbt sPsbtResp))
          fundStep pcid chan
        LndUpdate (Lnd.OpenStatusUpdate _ (Just (Lnd.OpenStatusUpdateChanPending p))) -> do
          $(logTM) DebugS $ logStr $ "Chan is pending... mining..." <> inspect p
          fundStep pcid chan
        LndUpdate (Lnd.OpenStatusUpdate _ (Just (Lnd.OpenStatusUpdateChanOpen (Lnd.ChannelOpenUpdate cp)))) -> do
          $(logTM) DebugS $ logStr $ "Chan is open" <> inspect cp
          pure cp
        LndSubFail -> do
          void $ withLndT Lnd.fundingStateStep ($ shimCancelReq pcid)
          throwE (FailureInternal "Lnd subscription failed trying to cancel psbt flow")
        _ -> throwE (FailureInternal "Unexpected update")