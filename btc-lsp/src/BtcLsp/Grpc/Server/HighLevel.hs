{-# LANGUAGE TypeApplications #-}

module BtcLsp.Grpc.Server.HighLevel
  ( swapIntoLn,
    swapIntoLnT,
    getCfg,
  )
where

import BtcLsp.Import
import qualified BtcLsp.Math as Math
import qualified BtcLsp.Storage.Model.SwapIntoLn as SwapIntoLn
import qualified LndClient.Data.NewAddress as Lnd
import qualified LndClient.Data.PayReq as Lnd
import qualified LndClient.RPC.Katip as Lnd
import qualified Proto.BtcLsp.Data.HighLevel_Fields as Grpc
import qualified Proto.BtcLsp.Method.GetCfg as GetCfg
import qualified Proto.BtcLsp.Method.GetCfg_Fields as GetCfg
import qualified Proto.BtcLsp.Method.SwapIntoLn as SwapIntoLn
import qualified Proto.BtcLsp.Method.SwapIntoLn_Fields as SwapIntoLn

swapIntoLn ::
  ( Env m
  ) =>
  Entity User ->
  SwapIntoLn.Request ->
  m SwapIntoLn.Response
swapIntoLn userEnt req = do
  res <- runExceptT $ do
    fundInv <-
      fromReqT $
        req ^. SwapIntoLn.maybe'fundLnInvoice
    refundAddr <-
      fromReqT $
        req ^. SwapIntoLn.maybe'refundOnChainAddress
    fundInvLnd <-
      withLndT
        Lnd.decodePayReq
        ($ from fundInv)
    swapIntoLnT
      userEnt
      fundInv
      fundInvLnd
      refundAddr
  pure $ case res of
    Left e ->
      failResE e
    Right (Entity _ swap) ->
      defMessage
        & SwapIntoLn.success
          .~ ( defMessage
                 & SwapIntoLn.fundOnChainAddress
                   .~ from (swapIntoLnFundAddress swap)
                 & SwapIntoLn.minFundMoney
                   .~ from @MSat
                     ( from (swapIntoLnChanCapUser swap)
                         + from (swapIntoLnFeeLsp swap)
                     )
             )

swapIntoLnT ::
  ( Env m
  ) =>
  Entity User ->
  LnInvoice 'Fund ->
  Lnd.PayReq ->
  OnChainAddress 'Refund ->
  ExceptT Failure m (Entity SwapIntoLn)
swapIntoLnT userEnt fundInv fundInvLnd refundAddr = do
  --
  -- TODO : proper input failure
  --
  when
    ( Lnd.numMsat fundInvLnd /= MSat 0
        || Lnd.destination fundInvLnd
          /= userNodePubKey (entityVal userEnt)
    )
    . throwE
    $ FailureInput [defMessage]
  fundAddr <-
    from
      <$> withLndT
        Lnd.newAddress
        ( $
            Lnd.NewAddressRequest
              { Lnd.addrType = Lnd.WITNESS_PUBKEY_HASH,
                Lnd.account = Nothing
              }
        )
  expAt <-
    lift
      . getFutureTime
      $ Lnd.expiry fundInvLnd
  lift $
    SwapIntoLn.createIgnore
      userEnt
      fundInv
      (Lnd.paymentHash fundInvLnd)
      fundAddr
      refundAddr
      expAt

getCfg ::
  ( Env m
  ) =>
  Entity User ->
  GetCfg.Request ->
  m GetCfg.Response
getCfg _ _ = do
  pub <- getLspPubKey
  sa <- getLndP2PSocketAddress
  swapMinAmt <- getSwapIntoLnMinAmt
  pure $
    defMessage
      & GetCfg.success
        .~ ( defMessage
               & GetCfg.lspLnNodes
                 .~ [ defMessage
                        & Grpc.pubKey
                          .~ from pub
                        & Grpc.host
                          .~ from (socketAddressHost sa)
                        & Grpc.port
                          .~ from (socketAddressPort sa)
                    ]
               & GetCfg.swapIntoLnMinAmt
                 .~ from swapMinAmt
               & GetCfg.swapIntoLnMaxAmt
                 .~ from Math.swapLnMaxAmt
               & GetCfg.swapFromLnMinAmt
                 .~ from swapMinAmt
               & GetCfg.swapFromLnMaxAmt
                 .~ from Math.swapLnMaxAmt
               & GetCfg.swapLnFeeRate
                 .~ from Math.swapLnFeeRate
               & GetCfg.swapLnMinFee
                 .~ from Math.swapLnMinFee
           )
