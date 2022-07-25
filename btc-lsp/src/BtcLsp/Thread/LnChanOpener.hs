{-# LANGUAGE TemplateHaskell #-}

module BtcLsp.Thread.LnChanOpener
  ( apply,
  )
where

import BtcLsp.Import
import qualified BtcLsp.Import.Psql as Psql
import qualified BtcLsp.Psbt.PsbtOpener as PO
import BtcLsp.Psbt.Utils (swapUtxoToPsbtUtxo)
import qualified BtcLsp.Storage.Model.LnChan as LnChan
import qualified BtcLsp.Storage.Model.SwapIntoLn as SwapIntoLn
import qualified BtcLsp.Storage.Model.SwapUtxo as SwapUtxo
import Control.Concurrent.Extra
import qualified Data.Set as Set
import qualified LndClient.Data.ChannelPoint as ChannelPoint
import qualified LndClient.Data.Peer as Peer
import qualified LndClient.RPC.Silent as LndSilent

apply :: (Env m) => m ()
apply = do
  lock <- liftIO newLock
  forever $ do
    ePeerList <-
      withLnd LndSilent.listPeers id
    whenLeft ePeerList $
      $(logTM) ErrorS
        . logStr
        . ("ListPeers procedure failed: " <>)
        . inspect
    let peerSet =
          Set.fromList $
            Peer.pubKey <$> fromRight [] ePeerList
    swaps <- runSql $ filter
          ( \x ->
              Set.member
                (userNodePubKey . entityVal $ snd x)
                peerSet
          )
          <$> SwapIntoLn.getSwapsWaitingPeerSql
    mapM_ (\(swp, usr) -> spawnLink $ do
      void $ runSql $ SwapIntoLn.updateInPsbtThreadSql $ entityKey swp
      r <- runSql $ openChanSql lock swp usr
      whenLeft r $ pure $ runSql $ SwapIntoLn.updateRevertInPsbtThreadSql $ entityKey swp)
      swaps
    sleep300ms

--
-- TODO : Do not open channel in case where
-- there not is enough liquidity to perform swap.
-- Maybe also put some limits into amount of
-- opening chans per user.
--
openChanSql ::
  ( Env m
  ) =>
  Lock ->
  Entity SwapIntoLn ->
  Entity User ->
  ReaderT Psql.SqlBackend m (Either (Entity SwapIntoLn) ())
openChanSql lock (Entity swapKey _) userEnt = do
  res <-
    SwapIntoLn.withLockedRowSql swapKey (== SwapInPsbtThread) $
      \swapVal -> do
        utxos <- SwapUtxo.getSpendableUtxosBySwapIdSql swapKey
        cpEither <- lift . runExceptT $ do
          r <-
            PO.openChannelPsbt
              lock
              (swapUtxoToPsbtUtxo . entityVal <$> utxos)
              (userNodePubKey $ entityVal userEnt)
              (coerce $ swapIntoLnLspFeeAndChangeAddress swapVal)
              (coerce swapIntoLnFeeLsp swapVal)
              (swapIntoLnPrivacy swapVal)
          liftIO (wait $ PO.fundAsync r) >>= except
        either
          ( $(logTM) ErrorS . logStr
              . ("OpenChan procedure failed: " <>)
              . inspect
          )
          ( \cp ->
              LnChan.createUpdateSql
                swapKey
                (ChannelPoint.fundingTxId cp)
                (ChannelPoint.outputIndex cp)
                >> SwapIntoLn.updateWaitingChanSql swapKey
                >> SwapUtxo.updateSpentChanSwappedSql swapKey
          )
          cpEither
  whenLeft res $
    $(logTM) ErrorS
      . logStr
      . ("Channel opening failed due to wrong status " <>)
      . inspect
  pure res
