{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module BtcLsp.Grpc.Server.HighLevel
  ( swapIntoLn,
    swapIntoLnT,
    getCfg,
  )
where

import qualified BtcLsp.Data.Smart as Smart
import BtcLsp.Import
import qualified BtcLsp.Math.Swap as Math
import qualified BtcLsp.Storage.Model.SwapIntoLn as SwapIntoLn
import qualified LndClient as Lnd
import qualified LndClient.Data.NewAddress as Lnd
import qualified LndClient.RPC.Katip as Lnd
import qualified Proto.BtcLsp.Data.HighLevel as Grpc
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
    privacy <-
      fromReqT
        $( mkFieldLocation
             @SwapIntoLn.Request
             [ "privacy"
             ]
         )
        $ req ^? SwapIntoLn.privacy
    unsafeRefundAddr <-
      fromReqT
        $( mkFieldLocation
             @SwapIntoLn.Request
             [ "refund_on_chain_address"
             ]
         )
        $ req ^. SwapIntoLn.maybe'refundOnChainAddress
    swapIntoLnT
      userEnt
      unsafeRefundAddr
      privacy
  pure $ case res of
    Left e -> e
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
  UnsafeOnChainAddress 'Refund ->
  Privacy ->
  ExceptT SwapIntoLn.Response m (Entity SwapIntoLn)
swapIntoLnT userEnt unsafeRefundAddr chanPrivacy = do
  --
  -- TODO : Do not fail immediately, but collect
  -- all the input failures.
  --
  refundAddr <-
    withExceptT
      ( \case
          FailureInp FailureNonce ->
            newGenFailure
              Grpc.VERIFICATION_FAILED
              $( mkFieldLocation
                   @SwapIntoLn.Request
                   [ "ctx",
                     "nonce"
                   ]
               )
          FailureInp FailureNonSegwitAddr ->
            newSpecFailure SwapIntoLn.Response'Failure'REFUND_ON_CHAIN_ADDRESS_IS_NOT_SEGWIT
          FailureInp FailureNonValidAddr ->
            newSpecFailure SwapIntoLn.Response'Failure'REFUND_ON_CHAIN_ADDRESS_IS_NOT_VALID
          FailureInt e ->
            newInternalFailure e
      )
      $ Smart.newOnChainAddressT unsafeRefundAddr
  fundAddr <-
    from
      <$> withLndServerT
        Lnd.newAddress
        ( $
            Lnd.NewAddressRequest
              { Lnd.addrType = Lnd.WITNESS_PUBKEY_HASH,
                Lnd.account = Nothing
              }
        )
  feeAndChangeAddr <-
    withLndServerT
      Lnd.newAddress
      ( $
          Lnd.NewAddressRequest
            { Lnd.addrType = Lnd.WITNESS_PUBKEY_HASH,
              Lnd.account = Nothing
            }
      )
  expAt <-
    getFutureTime
      . Lnd.Seconds
      $ 7 * 24 * 60 * 60
  lift
    . runSql
    . SwapIntoLn.createIgnoreSql
      userEnt
      fundAddr
      (from feeAndChangeAddr)
      refundAddr
      expAt
    $ chanPrivacy

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
