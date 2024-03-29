{-# LANGUAGE TemplateHaskell #-}

module BtcLsp.Thread.BlockScanner
  ( apply,
    scan,
    Utxo (..),
    extractRelatedUtxoFromBlock,
    mapVout,
  )
where

import BtcLsp.Data.Orphan ()
import qualified BtcLsp.Data.Smart as Smart
import BtcLsp.Import
import qualified BtcLsp.Import.Psql as Psql
import qualified BtcLsp.Math.OnChain as Math
import BtcLsp.Psbt.Utils (lockUtxo)
import qualified BtcLsp.Storage.Model.Block as Block
import qualified BtcLsp.Storage.Model.SwapIntoLn as SwapIntoLn
import qualified BtcLsp.Storage.Model.SwapUtxo as SwapUtxo
import qualified Data.Vector as V
import LndClient (txIdParser)
import qualified LndClient.Data.FundPsbt as FP
import qualified LndClient.Data.OutPoint as OP
import qualified LndClient.RPC.Katip as Lnd
import qualified Network.Bitcoin as Btc
import qualified Network.Bitcoin.BlockChain as Btc
import qualified Network.Bitcoin.Types as Btc
import qualified Universum

apply :: (Env m, GenericPrettyEnv m) => m ()
apply =
  forever $ do
    Inspect inspect <- getInspect
    eitherM
      ( $logTM ErrorS
          . logStr
          . inspect @Text
      )
      maybeFunded
      . runExceptT
      $ monitorOnChainLiquidity >> scan
    sleep300ms

maybeFunded :: (Env m, GenericPrettyEnv m) => [Utxo] -> m ()
maybeFunded [] =
  pure ()
maybeFunded utxos =
  mapM_ maybeFundSwap
    . nubOrd
    $ utxoSwapId <$> utxos

maybeFundSwap :: (Env m, GenericPrettyEnv m) => SwapIntoLnId -> m ()
maybeFundSwap swapId = do
  Inspect inspect <- getInspect
  res <- runSql
    . SwapIntoLn.withLockedRowSql
      swapId
      (`elem` swapStatusChain)
    $ \swapVal -> do
      us <- SwapUtxo.getSpendableUtxosBySwapIdSql swapId
      let amt = sum $ swapUtxoAmount . entityVal <$> us
      mCap <- lift $ newSwapCapM amt
      $logTM DebugS . logStr $
        maybe
          ( "Not enough funds for "
              <> inspect @Text swapId
              <> " with amt = "
              <> inspect amt
          )
          ( \cap ->
              "Marking funded "
                <> inspect @Text swapId
                <> " with amt = "
                <> inspect amt
                <> " and cap = "
                <> inspect cap
          )
          mCap
      whenJust mCap $ \swapCap -> do
        qty <-
          SwapUtxo.updateUnspentChanReserveSql $
            entityKey <$> us
        if qty /= (RowQty . from @Int @Int64 . length $ us)
          then do
            Psql.transactionUndo
            $logTM ErrorS . logStr $
              "Funding update "
                <> inspect @Text swapVal
                <> " failed for UTXOs "
                <> inspect us
          else
            SwapIntoLn.updateWaitingPeerSql
              swapId
              swapCap
  whenLeft res $
    $logTM WarningS
      . logStr
      . ("Funding failed due to wrong status " <>)
      . inspect @Text

data Utxo = Utxo
  { utxoAmt :: Msat,
    utxoVout :: Vout 'Funding,
    utxoTxId :: TxId 'Funding,
    utxoSwapId :: SwapIntoLnId,
    utxoLockId :: Maybe UtxoLockId
  }
  deriving stock (Show)

mapVout ::
  ( Env m,
    GenericPrettyEnv m
  ) =>
  Btc.TransactionID ->
  Btc.TxOut ->
  m (Maybe Utxo)
mapVout txid (Btc.TxOut amt vout (Btc.StandardScriptPubKeyV22 _ _ _ addr)) =
  handleAddr addr amt vout txid
mapVout txid txout@(Btc.TxOut amt vout (Btc.StandardScriptPubKey _ _ _ _ addrsV)) =
  case V.toList addrsV of
    [addr] -> handleAddr addr amt vout txid
    _ -> do
      Inspect inspect <- getInspect
      $logTM ErrorS . logStr $
        "Unsupported address vector in txid = "
          <> inspect @Text txid
          <> " and txout = "
          <> Universum.show txout
      pure Nothing
mapVout _ (Btc.TxOut _ _ Btc.NonStandardScriptPubKey {}) =
  pure Nothing

handleAddr ::
  ( Env m,
    GenericPrettyEnv m
  ) =>
  Btc.Address ->
  Btc.BTC ->
  Integer ->
  Btc.TransactionID ->
  m (Maybe Utxo)
handleAddr addr amt vout txid = do
  mswp <- maybeSwap addr
  case mswp of
    Just swp ->
      newUtxo
        (Math.trySatToMsat amt)
        (tryFrom vout)
        (txIdParser $ Btc.unTransactionID txid)
        swp
    Nothing ->
      pure Nothing

newUtxo ::
  ( Env m,
    GenericPrettyEnv m
  ) =>
  Either Failure Msat ->
  Either (TryFromException Integer (Vout 'Funding)) (Vout 'Funding) ->
  Either LndError ByteString ->
  Entity SwapIntoLn ->
  m (Maybe Utxo)
newUtxo (Right amt) (Right n) (Right txid) swp =
  pure . Just $
    Utxo amt n (from txid) (entityKey swp) Nothing
newUtxo amt vout txid swp = do
  Inspect inspect <- getInspect
  $logTM ErrorS . logStr $
    "TryFrom overflow error amt = "
      <> Universum.show amt
      <> " vout = "
      <> Universum.show vout
      <> " txid = "
      <> inspect @Text txid
      <> " and swap = "
      <> inspect swp
  pure Nothing

extractRelatedUtxoFromBlock ::
  ( Env m,
    GenericPrettyEnv m
  ) =>
  Btc.BlockVerbose ->
  m [Utxo]
extractRelatedUtxoFromBlock blk =
  foldrM foldTrx [] $
    Btc.vSubTransactions blk
  where
    foldTrx trx acc = do
      utxos <-
        mapM (mapVout $ Btc.decTxId trx)
          . V.toList
          $ Btc.decVout trx
      pure $
        catMaybes utxos <> acc

persistBlockT ::
  ( Storage m,
    Env m,
    GenericPrettyEnv m
  ) =>
  Btc.BlockVerbose ->
  [Utxo] ->
  ExceptT Failure m ()
persistBlockT blk utxos = do
  Inspect inspect <- lift getInspect
  height <-
    tryFromT "persistBlockT block height" $
      Btc.vBlkHeight blk
  lift . runSql $ do
    blockId <-
      entityKey
        <$> Block.createUpdateConfirmedSql
          height
          (BlkHash $ Btc.vBlockHash blk)
    ct <-
      getCurrentTime
    res <-
      Block.withLockedRowSql blockId (== BlkConfirmed)
        . const
        $ do
          SwapUtxo.createIgnoreManySql $
            newSwapUtxo ct blockId <$> utxos
          SwapUtxo.updateRefundBlockIdSql blockId
    whenLeft res $
      $logTM ErrorS
        . logStr
        . ("UTXOs are not persisted for the block " <>)
        . inspect @Text

newSwapUtxo :: UTCTime -> BlockId -> Utxo -> SwapUtxo
newSwapUtxo ct blkId utxo = do
  SwapUtxo
    { swapUtxoSwapIntoLnId = utxoSwapId utxo,
      swapUtxoBlockId = blkId,
      swapUtxoTxid = utxoTxId utxo,
      swapUtxoVout = utxoVout utxo,
      swapUtxoAmount = Money amt,
      swapUtxoStatus =
        if amt >= Math.trxDustLimit
          then SwapUtxoUnspent
          else SwapUtxoUnspentDust,
      swapUtxoRefundTxId = Nothing,
      swapUtxoRefundBlockId = Nothing,
      swapUtxoLockId = utxoLockId utxo,
      swapUtxoInsertedAt = ct,
      swapUtxoUpdatedAt = ct
    }
  where
    amt = utxoAmt utxo

scan ::
  ( Env m,
    GenericPrettyEnv m
  ) =>
  ExceptT Failure m [Utxo]
scan = do
  Inspect inspect <- lift getInspect
  mBlk <- lift $ runSql Block.getLatestSql
  cHeight <-
    tryFromT "BlockScanner block count"
      =<< withBtcT Btc.getBlockCount id
  case mBlk of
    Nothing -> do
      $logTM InfoS . logStr $
        "Found no blocks, scanning height = "
          <> inspect @Text cHeight
      scanOneBlock cHeight
    Just lBlk -> do
      reorgDetected <- detectReorg lBlk
      case reorgDetected of
        Nothing -> do
          let known = unBlkHeight . blockHeight $ entityVal lBlk
          scannerStep [] (1 + known) $ unBlkHeight cHeight
        Just height -> do
          $logTM WarningS . logStr $
            "Reorg detected from height = "
              <> inspect @Text height
          bHeight <-
            tryFromT "BlockScanner reorg block height" height
          withExceptT
            ( FailureInt
                . FailurePrivate
                . ("Block scanner failed due to bad status " <>)
                . inspectPlain
            )
            . ExceptT
            . runSql
            . Block.withLockedRowSql
              (entityKey lBlk)
              (== BlkConfirmed)
            . const
            $ do
              blks <- Block.getBlocksHigherSql bHeight
              Block.updateOrphanHigherSql bHeight
              SwapUtxo.revertRefundedSql (entityKey <$> blks)
              SwapUtxo.updateOrphanSql (entityKey <$> blks)
          scannerStep [] (1 + unBlkHeight bHeight) $ unBlkHeight cHeight

scannerStep ::
  ( Env m,
    GenericPrettyEnv m
  ) =>
  [Utxo] ->
  Word64 ->
  Word64 ->
  ExceptT Failure m [Utxo]
scannerStep acc cur end =
  if cur > end
    then pure acc
    else do
      Inspect inspect <- lift getInspect
      $logTM InfoS . logStr $
        "Scanner step cur = "
          <> inspect @Text cur
          <> " end = "
          <> inspect end
          <> " got utxos qty = "
          <> inspect (length acc)
      utxos <- scanOneBlock (BlkHeight cur)
      scannerStep (acc <> utxos) (cur + 1) end

detectReorg ::
  ( Env m
  ) =>
  Entity Block ->
  ExceptT Failure m (Maybe Btc.BlockHeight)
detectReorg blk = do
  cReorgHeight <- checkReorgHeight cHeight
  pure $
    if cReorgHeight == cHeight
      then Nothing
      else Just cReorgHeight
  where
    cHeight = toInteger . unBlkHeight . blockHeight $ entityVal blk

checkReorgHeight ::
  ( Env m
  ) =>
  Btc.BlockHeight ->
  ExceptT Failure m Btc.BlockHeight
checkReorgHeight bHeight = do
  res <- compareHash bHeight
  case res of
    Just True -> pure bHeight
    Just False -> checkReorgHeight (bHeight - 1)
    Nothing -> pure bHeight

compareHash ::
  ( Env m
  ) =>
  Btc.BlockHeight ->
  ExceptT Failure m (Maybe Bool)
compareHash height = do
  w64h <- tryFromT "BlockScanner compareHash" height
  cHash <- withBtcT Btc.getBlockHash ($ height)
  lift
    . ( (== cHash)
          . unBlkHash
          . blockHash
          . entityVal
          <<$>>
      )
    . (listToMaybe <$>)
    . runSql
    . Block.getBlockByHeightSql
    $ BlkHeight w64h

scanOneBlock ::
  ( Env m,
    GenericPrettyEnv m
  ) =>
  BlkHeight ->
  ExceptT Failure m [Utxo]
scanOneBlock height = do
  Inspect inspect <- lift getInspect
  hash <- withBtcT Btc.getBlockHash ($ toInteger . unBlkHeight $ height)
  blk <- withBtcT Btc.getBlockVerbose ($ hash)
  $logTM InfoS . logStr $
    "Got new block with height = "
      <> inspect @Text height
      <> " and hash = "
      <> inspect hash
  utxos <- lift $ extractRelatedUtxoFromBlock blk
  lockedUtxos <- mapM lockUtxo' utxos
  persistBlockT blk lockedUtxos
  pure utxos

--
-- TODO : Verify that it's possible to lock already locked UTXO.
-- It's corner case where UTXO has been locked but storage
-- procedure later failed.
--

utxoToOutPoint :: Utxo -> OP.OutPoint
utxoToOutPoint u =
  OP.OutPoint (unTxId $ utxoTxId u) (unsafeFrom . unVout $ utxoVout u)

lockUtxo' :: Env m => Utxo -> ExceptT Failure m Utxo
lockUtxo' u = do
  l <- lockUtxo (utxoToOutPoint u)
  pure $ u {utxoLockId = Just $ UtxoLockId $ FP.id l}

maybeSwap ::
  ( Env m
  ) =>
  Btc.Address ->
  m (Maybe (Entity SwapIntoLn))
maybeSwap =
  runSql
    . SwapIntoLn.getByFundAddressSql
    . Smart.unsafeNewOnChainAddress

monitorOnChainLiquidity ::
  ( Env m
  ) =>
  ExceptT Failure m ()
monitorOnChainLiquidity =
  withLndT Lnd.walletBalance id
    >>= lift . monitorTotalOnChainLiquidity
