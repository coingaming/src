{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BtcLsp.Data.Orphan () where

import BtcLsp.Import.External
import qualified BtcLsp.Import.Psql as Psql
import qualified LndClient as Lnd
import qualified Network.Bitcoin.BlockChain as Btc
import qualified Network.Bitcoin.RawTransaction as Btc
import qualified Text.PrettyPrint as PP
import qualified Universum
import qualified Witch

deriving stock instance Generic Btc.TxnOutputType

deriving stock instance Generic Btc.ScriptSig

deriving stock instance Generic Btc.ScriptPubKey

deriving stock instance Generic Btc.TxIn

deriving stock instance Generic Btc.TxOut

deriving stock instance Generic Btc.BlockVerbose

deriving stock instance Generic Btc.DecodedRawTransaction

deriving stock instance Generic Btc.BlockChainInfo

instance Out Btc.TxnOutputType

instance Out Btc.ScriptSig

instance Out Btc.ScriptPubKey

instance Out Btc.TxIn

instance Out Btc.TxOut

instance Out Btc.BlockVerbose

instance Out Btc.DecodedRawTransaction

instance Out Btc.BlockChainInfo

instance From Text Lnd.PaymentRequest

instance From Lnd.PaymentRequest Text

instance From Word64 MSat

instance From MSat Word64

instance From Word64 Lnd.Seconds

instance From Lnd.Seconds Word64

deriving stock instance Generic Btc.Block

instance Out Btc.Block

instance Out Btc.TransactionID

instance Out Natural where
  docPrec x =
    docPrec x . into @Integer
  doc =
    docPrec 0

instance
  (Psql.ToBackendKey Psql.SqlBackend a) =>
  TryFrom (Psql.Key a) Natural
  where
  tryFrom =
    tryFrom `composeTryLhs` Psql.fromSqlKey

instance
  (Psql.ToBackendKey Psql.SqlBackend a) =>
  TryFrom Natural (Psql.Key a)
  where
  tryFrom =
    Psql.toSqlKey `composeTryRhs` tryFrom

instance Out SomeException where
  docPrec _ =
    PP.text . Universum.show
  doc =
    docPrec 0

instance From Word32 (Vout 'Funding)

instance From ByteString (TxId 'Funding)

instance TryFrom Integer (Vout 'Funding) where
  tryFrom =
    from @Word32
      `composeTryRhs` tryFrom
