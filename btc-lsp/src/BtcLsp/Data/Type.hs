{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module BtcLsp.Data.Type
  ( Nonce,
    newNonce,
    LnInvoice (..),
    LnInvoiceStatus (..),
    LnChanStatus (..),
    Liquidity (..),
    Money (..),
    FeeRate (..),
    UnsafeOnChainAddress (..),
    Seconds (..),
    LogFormat (..),
    YesodLog (..),
    MicroSeconds (..),
    SwapStatus (..),
    swapStatusChain,
    swapStatusLn,
    swapStatusFinal,
    Failure (..),
    FailureInternal (..),
    FailureInput (..),
    tryFailureE,
    tryFailureT,
    tryFromE,
    tryFromT,
    SocketAddress (..),
    BlkHash (..),
    BlkHeight (..),
    BlkStatus (..),
    SwapUtxoStatus (..),
    Privacy (..),
    NodePubKeyHex (..),
    NodeUri (..),
    NodeUriHex (..),
    UtxoLockId (..),
    RHashHex (..),
    Uuid,
    unUuid,
    newUuid,
    Vbyte (..),
    RowQty (..),
    PsbtUtxo (..),
    SwapHash (..),
  )
where

import BtcLsp.Data.Kind
import BtcLsp.Data.Orphan ()
import BtcLsp.Import.External
import qualified BtcLsp.Import.Psql as Psql
import qualified BtcLsp.Text as T
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock as Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified LndClient as Lnd
import qualified LndClient.Data.OutPoint as OP
import qualified Network.Bitcoin.BlockChain as Btc
import Text.Julius (ToJavascript)
import qualified Universum
import qualified Witch
import Yesod.Core

newtype Nonce
  = Nonce Word64
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      Psql.PersistField,
      Psql.PersistFieldSql
    )
  deriving stock
    ( Generic
    )

instance Out Nonce

instance From Nonce Word64

instance From Word64 Nonce

newNonce :: (MonadIO m) => m Nonce
newNonce =
  liftIO $
    Nonce
      . utcTimeToMicros
      <$> Clock.getCurrentTime

utcTimeToMicros :: UTCTime -> Word64
utcTimeToMicros x =
  fromInteger $
    diffTimeToPicoseconds
      ( fromRational
          . toRational
          $ diffUTCTime x epoch
      )
      `div` 1000000

epoch :: UTCTime
epoch =
  posixSecondsToUTCTime 0

data LogFormat
  = Bracket
  | JSON
  deriving stock
    ( Read
    )

data YesodLog
  = YesodLogAll
  | YesodLogNoMain
  | YesodLogNothing
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Generic
    )

instance FromJSON YesodLog

newtype Seconds
  = Seconds Word64
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num
    )
  deriving stock
    ( Generic
    )

instance Out Seconds

newtype MicroSeconds
  = MicroSeconds Integer
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num
    )
  deriving stock
    ( Generic
    )

instance Out MicroSeconds

newtype LnInvoice (mrel :: MoneyRelation)
  = LnInvoice Lnd.PaymentRequest
  deriving newtype
    ( Eq,
      Show,
      Psql.PersistField,
      Psql.PersistFieldSql,
      PathPiece
    )
  deriving stock
    ( Generic
    )

instance Out (LnInvoice mrel)

instance From Lnd.PaymentRequest (LnInvoice mrel)

instance From (LnInvoice mrel) Lnd.PaymentRequest

instance From Text (LnInvoice mrel) where
  from =
    via @Lnd.PaymentRequest

instance From (LnInvoice mrel) Text where
  from =
    via @Lnd.PaymentRequest

newtype SwapHash = SwapHash Text
  deriving newtype
    ( Eq,
      Show,
      Read,
      PathPiece,
      ToJavascript,
      ToJSON
    )
  deriving stock
    ( Generic
    )

instance Out SwapHash

instance ToTypedContent (Maybe SwapHash) where
  toTypedContent = toTypedContent . toJSON

instance ToContent (Maybe SwapHash) where
  toContent = toContent . toJSON

data LnInvoiceStatus
  = LnInvoiceStatusNew
  | LnInvoiceStatusLocked
  | LnInvoiceStatusSettled
  | LnInvoiceStatusCancelled
  | LnInvoiceStatusExpired
  deriving stock
    ( Generic,
      Show,
      Read,
      Eq
    )

instance Out LnInvoiceStatus

data LnChanStatus
  = LnChanStatusPendingOpen
  | LnChanStatusOpened
  | LnChanStatusActive
  | LnChanStatusFullyResolved
  | LnChanStatusInactive
  | LnChanStatusPendingClose
  | LnChanStatusClosed
  deriving stock
    ( Generic,
      Show,
      Read,
      Eq,
      Ord
    )

instance Out LnChanStatus

newtype Liquidity (dir :: Direction) = Liquidity
  { unLiquidity :: MSat
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      Num
    )
  deriving stock
    ( Generic
    )

instance Out (Liquidity dir)

newtype
  Money
    (owner :: Owner)
    (btcl :: BitcoinLayer)
    (mrel :: MoneyRelation) = Money
  { unMoney :: MSat
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      Num,
      Psql.PersistField,
      Psql.PersistFieldSql
    )
  deriving stock
    ( Generic
    )

instance Out (Money owner btcl mrel)

instance From MSat (Money owner btcl mrel)

instance From (Money owner btcl mrel) MSat

instance From Word64 (Money owner btcl mrel) where
  from =
    via @MSat

instance From (Money owner btcl mrel) Word64 where
  from =
    via @MSat

instance TryFrom Natural (Money owner btcl mrel) where
  tryFrom =
    from @Word64 `composeTryRhs` tryFrom

instance From (Money owner btcl mrel) Natural where
  from =
    via @Word64

instance TryFrom (Ratio Natural) (Money owner btcl mrel) where
  tryFrom =
    tryFrom @Natural
      `composeTry` tryFrom

instance From (Money owner btcl mrel) (Ratio Natural) where
  from =
    via @Natural

instance TryFrom Rational (Money owner btcl mrel) where
  tryFrom =
    tryFrom @(Ratio Natural)
      `composeTry` tryFrom

instance From (Money owner btcl mrel) Rational where
  from =
    via @(Ratio Natural)

instance ToMessage (Money owner btcl mrel) where
  toMessage =
    T.displayRational 1
      . (/ 1000)
      . from

newtype FeeRate
  = FeeRate (Ratio Word64)
  deriving newtype
    ( Eq,
      Ord,
      Show
    )
  deriving stock
    ( Generic
    )

instance From (Ratio Word64) FeeRate

instance From FeeRate (Ratio Word64)

instance From FeeRate (Ratio Natural) where
  from =
    via @(Ratio Word64)

instance TryFrom Rational FeeRate where
  tryFrom =
    from @(Ratio Word64)
      `composeTryRhs` tryFrom

instance From FeeRate Rational where
  from =
    via @(Ratio Word64)

instance ToMessage FeeRate where
  toMessage =
    (<> "%")
      . T.displayRational 1
      . (* 100)
      . from

newtype UnsafeOnChainAddress (mrel :: MoneyRelation)
  = UnsafeOnChainAddress Text
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      PathPiece,
      Psql.PersistField,
      Psql.PersistFieldSql
    )
  deriving stock
    ( Generic
    )

instance Out (UnsafeOnChainAddress mrel)

instance From Text (UnsafeOnChainAddress mrel)

instance From (UnsafeOnChainAddress mrel) Text

data SwapStatus
  = -- | Waiting on-chain funding trx with
    -- given amt from user with
    -- some confirmations.
    SwapWaitingFundChain
  | -- | Swap has been funded on-chain,
    -- need to open LN channel now.
    SwapWaitingPeer
  | -- | Channel opener thread is in progress
    SwapInPsbtThread
  | -- | Waiting channel opening trx
    -- to be mined with some confirmations.
    SwapWaitingChan
  | -- | Final statuses
    SwapSucceeded
  | SwapExpired
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Generic,
      Enum,
      Bounded
    )

instance Out SwapStatus

swapStatusChain :: [SwapStatus]
swapStatusChain =
  [ SwapWaitingFundChain,
    SwapWaitingPeer
  ]

swapStatusLn :: [SwapStatus]
swapStatusLn =
  [ SwapInPsbtThread,
    SwapWaitingChan
  ]

swapStatusFinal :: [SwapStatus]
swapStatusFinal =
  [ SwapSucceeded,
    SwapExpired
  ]

instance PathPiece SwapStatus where
  fromPathPiece :: Text -> Maybe SwapStatus
  fromPathPiece =
    readMaybe
      . unpack
      . T.toTitle
  toPathPiece :: SwapStatus -> Text
  toPathPiece =
    T.toLower
      . Universum.show

data Failure
  = FailureInp FailureInput
  | FailureInt FailureInternal
  deriving stock
    ( Eq,
      Show,
      Generic
    )

instance Out Failure

data FailureInput
  = FailureNonce
  | FailureNonSegwitAddr
  | FailureNonValidAddr
  deriving stock
    ( Eq,
      Show,
      Generic
    )

instance Out FailureInput

data FailureInternal
  = FailureGrpcServer Text
  | FailureGrpcClient Text
  | FailureMath Text
  | FailurePrivate Text
  | FailureRedacted
  deriving stock
    ( Eq,
      Show,
      Generic
    )

instance Out FailureInternal

tryFailureE ::
  forall source target.
  ( Show source,
    Typeable source,
    Typeable target
  ) =>
  Text ->
  Either (TryFromException source target) target ->
  Either Failure target
tryFailureE label =
  first $
    FailureInt
      . FailureMath
      . (label <>)
      . (" " <>)
      . Universum.show

tryFailureT ::
  forall source target m.
  ( Show source,
    Typeable source,
    Typeable target,
    Monad m
  ) =>
  Text ->
  Either (TryFromException source target) target ->
  ExceptT Failure m target
tryFailureT label =
  except . tryFailureE label

tryFromE ::
  forall source target.
  ( Show source,
    Typeable source,
    Typeable target,
    TryFrom source target,
    'False ~ (source == target)
  ) =>
  Text ->
  source ->
  Either Failure target
tryFromE label =
  tryFailureE label . tryFrom

tryFromT ::
  forall source target m.
  ( Show source,
    Typeable source,
    Typeable target,
    TryFrom source target,
    Monad m,
    'False ~ (source == target)
  ) =>
  Text ->
  source ->
  ExceptT Failure m target
tryFromT label =
  except . tryFromE label

data SocketAddress = SocketAddress
  { socketAddressHost :: HostName,
    socketAddressPort :: PortNumber
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

instance Out SocketAddress

newtype BlkHash
  = BlkHash Btc.BlockHash
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Psql.PersistField, Psql.PersistFieldSql)

instance Out BlkHash

instance From Btc.BlockHash BlkHash

instance From BlkHash Btc.BlockHash

newtype BlkHeight
  = BlkHeight Word64
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
  deriving newtype
    ( Num,
      Psql.PersistField,
      Psql.PersistFieldSql
    )

instance Out BlkHeight

instance ToJSON BlkHeight

instance From Word64 BlkHeight

instance From BlkHeight Word64

instance From BlkHeight Natural where
  from =
    via @Word64

instance TryFrom Btc.BlockHeight BlkHeight where
  tryFrom =
    from @Word64
      `composeTryRhs` tryFrom

instance From BlkHeight Btc.BlockHeight where
  from =
    via @Word64

data BlkStatus
  = BlkConfirmed
  | BlkOrphan
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Generic
    )

instance Out BlkStatus

data SwapUtxoStatus
  = SwapUtxoUnspent
  | SwapUtxoUnspentDust
  | SwapUtxoUnspentChanReserve
  | SwapUtxoSpentChanSwapped
  | SwapUtxoSpentRefund
  | SwapUtxoOrphan
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Generic
    )

instance Out SwapUtxoStatus

data Privacy
  = Public
  | Private
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Enum,
      Bounded,
      Generic
    )

instance Out Privacy

newtype NodePubKeyHex
  = NodePubKeyHex Text
  deriving newtype (Eq, Ord, Show, Read, IsString)
  deriving stock (Generic)

instance Out NodePubKeyHex

instance From NodePubKeyHex Text

instance From Text NodePubKeyHex

instance TryFrom NodePubKey NodePubKeyHex where
  tryFrom src =
    from
      `composeTryRhs` ( first
                          ( TryFromException src
                              . Just
                              . toException
                          )
                          . TE.decodeUtf8'
                          . B16.encode
                          . coerce
                      )
      $ src

newtype UtxoLockId = UtxoLockId ByteString
  deriving newtype (Eq, Ord, Show, Read)
  deriving stock (Generic)

instance Out UtxoLockId

data NodeUri = NodeUri
  { nodeUriPubKey :: NodePubKey,
    nodeUriSocketAddress :: SocketAddress
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

instance Out NodeUri

newtype NodeUriHex
  = NodeUriHex Text
  deriving newtype (Eq, Ord, Show, Read, IsString)
  deriving stock (Generic)

instance Out NodeUriHex

instance From NodeUriHex Text

instance From Text NodeUriHex

instance TryFrom NodeUri NodeUriHex where
  tryFrom src =
    bimap
      (withTarget @NodeUriHex . withSource src)
      ( \pubHex ->
          from @Text $
            from pubHex
              <> "@"
              <> from host
              <> ":"
              <> from (showIntegral port)
      )
      $ tryFrom @NodePubKey @NodePubKeyHex $
        nodeUriPubKey src
    where
      sock = nodeUriSocketAddress src
      host = socketAddressHost sock
      port = socketAddressPort sock

newtype RHashHex = RHashHex
  { unRHashHex :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      PathPiece
    )
  deriving stock
    ( Generic
    )

instance Out RHashHex

instance From RHashHex Text

instance From Text RHashHex

instance From RHash RHashHex where
  from =
    --
    -- NOTE : decodeUtf8 in general is unsafe
    -- but here we know that it will not fail
    -- because of B16
    --
    RHashHex
      . decodeUtf8
      . B16.encode
      . coerce

instance From RHashHex RHash where
  from =
    --
    -- NOTE : this is not RFC 4648-compliant,
    -- using only for the practical purposes
    --
    RHash
      . B16.decodeLenient
      . encodeUtf8
      . unRHashHex

newtype Uuid (tab :: Table) = Uuid
  { unUuid' :: UUID
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read
    )
  deriving stock (Generic)

unUuid :: Uuid tab -> UUID
unUuid =
  unUuid'

instance Out (Uuid tab) where
  docPrec x =
    docPrec x
      . UUID.toText
      . unUuid
  doc =
    docPrec 0

newUuid :: (MonadIO m) => m (Uuid tab)
newUuid =
  liftIO $
    Uuid <$> UUID.nextRandom

newtype Vbyte = Vbyte
  { unVbyte :: Ratio Natural
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num
    )
  deriving stock
    ( Generic
    )

instance Out Vbyte

instance From Vbyte (Ratio Natural)

instance From (Ratio Natural) Vbyte

newtype RowQty = RowQty
  { unRowQty :: Int64
  }
  deriving newtype
    ( Eq,
      Ord,
      Show
    )
  deriving stock
    ( Generic
    )

instance Out RowQty

data PsbtUtxo = PsbtUtxo
  { getOutPoint :: OP.OutPoint,
    getAmt :: MSat,
    getLockId :: Maybe UtxoLockId
  }
  deriving stock (Show, Generic)

instance Out PsbtUtxo

instance From RowQty Int64

instance From Int64 RowQty

instance From Int RowQty where
  from =
    via @Int64

--
-- NOTE :  we're taking advantage of
-- PostgreSQL understanding UUID values
--
instance Psql.PersistField (Uuid tab) where
  toPersistValue =
    Psql.PersistLiteral_ Psql.Escaped
      . UUID.toASCIIBytes
      . unUuid
  fromPersistValue = \case
    Psql.PersistLiteral_ Psql.Escaped x ->
      maybe
        ( Left $
            "Failed to deserialize a UUID, got literal: "
              <> inspectPlain x
        )
        ( Right
            . Uuid
        )
        $ UUID.fromASCIIBytes x
    failure ->
      Left $
        "Failed to deserialize a UUID, got: "
          <> inspectPlain failure

instance Psql.PersistFieldSql (Uuid tab) where
  sqlType =
    const $
      Psql.SqlOther "uuid"

instance ToMessage (Uuid tab) where
  toMessage =
    (<> "...")
      . T.take 5
      . UUID.toText
      . unUuid

instance PathPiece (Uuid tab) where
  fromPathPiece =
    (Uuid <$>)
      . UUID.fromText
  toPathPiece =
    UUID.toText
      . unUuid

Psql.derivePersistField "LnInvoiceStatus"
Psql.derivePersistField "LnChanStatus"
Psql.derivePersistField "SwapStatus"
Psql.derivePersistField "BlkStatus"
Psql.derivePersistField "SwapUtxoStatus"
Psql.derivePersistField "Privacy"
Psql.derivePersistField "UtxoLockId"
