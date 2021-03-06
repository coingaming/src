{-# LANGUAGE TypeApplications #-}

module ReorgSpec
  ( spec,
  )
where

import BtcLsp.Grpc.Orphan ()
import BtcLsp.Import hiding (setGrpcCtx, setGrpcCtxT)
import qualified BtcLsp.Storage.Model.Block as Block
import qualified BtcLsp.Thread.BlockScanner as BlockScanner
import qualified Network.Bitcoin as Btc
import Test.Hspec
import TestAppM
import TestOrphan ()

spec :: Spec
spec = do
  itEnvT @'LndLsp "Reorg test" $ do
    void $ withBtcT Btc.setNetworkActive ($ False)
    _ <- BlockScanner.scan

    void $ withBtcT Btc.generate (\h -> h 5 Nothing)
    void $ withBtc2T Btc.generate (\h -> h 10 Nothing)
    blockCount <- withBtcT Btc.getBlockCount id
    _ <- BlockScanner.scan
    blkList1 <-
      lift . runSql
        . Block.getBlockByHeightSql
        . BlkHeight
        $ fromIntegral blockCount
    let blk1 = unsafeHead $ entityVal <$> blkList1
    blockHash1 <- withBtcT Btc.getBlockHash ($ blockCount)
    liftIO $ from (blockHash blk1) `shouldBe` blockHash1

    void $ withBtcT Btc.setNetworkActive ($ True)
    _ <- lift waitTillNodesSynchronized
    _ <- BlockScanner.scan
    blkList2 <-
      lift . runSql
        . Block.getBlockByHeightSql
        . BlkHeight
        $ fromIntegral blockCount
    let blk2 = unsafeHead $ entityVal <$> blkList2
    blockHash2 <- withBtcT Btc.getBlockHash ($ blockCount)
    liftIO $ from (blockHash blk2) `shouldBe` blockHash2

unsafeHead :: [Block] -> Block
unsafeHead blkList = do
  case blkList of
    [blk] -> blk
    _ -> error "There is no blocks in a list"

waitTillNodesSynchronized ::
  ( MonadReader (TestEnv o) m,
    Env m
  ) =>
  m (Either Failure ())
waitTillNodesSynchronized = runExceptT $ do
  sleep1s
  blockCount1 <- withBtcT Btc.getBlockCount id
  blockCount2 <- withBtc2T Btc.getBlockCount id
  if blockCount1 == blockCount2
    then do
      blockHash1 <- withBtcT Btc.getBlockHash ($ blockCount1)
      blockHash2 <- withBtc2T Btc.getBlockHash ($ blockCount2)
      if blockHash1 == blockHash2
        then return ()
        else ExceptT waitTillNodesSynchronized
    else ExceptT waitTillNodesSynchronized
