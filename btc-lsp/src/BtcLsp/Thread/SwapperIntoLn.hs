{-# LANGUAGE TemplateHaskell #-}

module BtcLsp.Thread.SwapperIntoLn
  ( apply,
  )
where

import BtcLsp.Import
import qualified BtcLsp.Storage.Model.SwapIntoLn as SwapIntoLn
import qualified Data.Set as Set
import qualified LndClient.Data.Peer as Peer
import qualified LndClient.Data.SendPayment as Lnd
import qualified LndClient.RPC.Katip as LndKatip
import qualified LndClient.RPC.Silent as LndSilent

apply :: (Env m) => m ()
apply = do
  ePeerList <- withLnd LndSilent.listPeers id
  whenLeft ePeerList $
    $(logTM) ErrorS
      . logStr
      . ("ListPeers procedure failed: " <>)
      . inspect
  let peerSet =
        Set.fromList $
          Peer.pubKey <$> fromRight [] ePeerList
  swaps <- SwapIntoLn.getSwapsToSettle
  tasks <-
    mapM
      ( spawnLink
          . settleSwap
      )
      $ filter
        ( \x ->
            Set.member
              (userNodePubKey . entityVal $ snd x)
              peerSet
        )
        swaps
  mapM_ (liftIO . wait) tasks
  sleep $ MicroSecondsDelay 500000
  apply

settleSwap :: (Env m) => (Entity SwapIntoLn, Entity User) -> m ()
settleSwap (swapEnt, _) = do
  let swap = entityVal swapEnt
  res <- runExceptT $ do
    --
    -- TODO : use preimage as a proof??
    --
    void $
      withLndT
        LndKatip.sendPayment
        ( $
            Lnd.SendPaymentRequest
              { Lnd.paymentRequest =
                  from $ swapIntoLnFundInvoice swap,
                Lnd.amt =
                  from $ swapIntoLnChanCapUser swap
              }
        )
    lift
      . SwapIntoLn.updateSettled
      $ entityKey swapEnt
  whenLeft res $
    $(logTM) ErrorS . logStr
      . ("SettleSwap procedure failed: " <>)
      . inspect