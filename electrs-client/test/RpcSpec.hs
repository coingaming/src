{-# LANGUAGE TemplateHaskell #-}

module RpcSpec
  ( spec,
  )
where

import ElectrsClient.Import.External
import ElectrsClient.Rpc as Rpc
--import ElectrsClient.Helper
import ElectrsClient.Data.Env
--import ElectrsClient.Type
--import Network.Bitcoin.Mining (generateToAddress)
--import Network.Bitcoin
import Test.Hspec

spec :: Spec
spec = do
--  it "getMsatPerByte" $ do
--    x <- getMsatPerByte
--    liftIO $ x `shouldBe` Just 1000
  it "Version" $ do
    rc <- liftIO readRawConfig
    let env = rawConfigElectrsEnv rc
    ver <- Rpc.version env ()
    ver `shouldSatisfy` isRight
--  it "Get Balance" $ do
--    elecBal <- runExceptT $ do
--      rc <- liftIO readRawConfig
--      let env = rawConfigElectrsEnv rc
--      let btcdEnv = rawConfigBtcEnv rc
--      btcClient <-
--        liftIO $
--          getClient
--            (unpack . bitcoindEnvHost $ btcdEnv)
--            (encodeUtf8 . bitcoindEnvUsername $ btcdEnv)
--            (encodeUtf8 . bitcoindEnvPassword $ btcdEnv)
--      addr <- liftIO $ getNewAddress btcClient Nothing
--      _ <- liftIO $ generateToAddress btcClient 3 addr Nothing
--      --_ <- waitTillLastBlockProcessedT 100
--      Rpc.getBalance env btcdEnv (Left $ OnChainAddress addr)
--    elecBal `shouldSatisfy` isRight
--    case elecBal of
--      Right (Right bal) -> confirmed bal `shouldSatisfy` (> 0)
--      Left _ -> error "Error getting balance"
