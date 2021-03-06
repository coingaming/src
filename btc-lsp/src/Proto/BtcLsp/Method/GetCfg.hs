{- This file was auto-generated from btc_lsp/method/get_cfg.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies, DeriveGeneric#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.BtcLsp.Method.GetCfg (
        Request(), Response(), Response'Either(..), _Response'Success',
        _Response'Failure', Response'Failure(),
        Response'Failure'InputFailure(..), Response'Failure'InputFailure(),
        Response'Failure'InputFailure'UnrecognizedValue, Response'Success()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
import qualified Text.PrettyPrint.GenericPretty.Instance
import qualified GHC.Generics
import qualified Text.PrettyPrint.GenericPretty
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
import qualified Proto.BtcLsp.Data.HighLevel
{- | Fields :
     
         * 'Proto.BtcLsp.Method.GetCfg_Fields.ctx' @:: Lens' Request Proto.BtcLsp.Data.HighLevel.Ctx@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'ctx' @:: Lens' Request (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.Ctx)@ -}
data Request
  = Request'_constructor {_Request'ctx :: !(Prelude.Maybe Proto.BtcLsp.Data.HighLevel.Ctx),
                          _Request'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord, GHC.Generics.Generic)
instance Prelude.Show Request where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Text.PrettyPrint.GenericPretty.Out Request
instance Data.ProtoLens.Field.HasField Request "ctx" Proto.BtcLsp.Data.HighLevel.Ctx where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Request'ctx (\ x__ y__ -> x__ {_Request'ctx = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Request "maybe'ctx" (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.Ctx) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Request'ctx (\ x__ y__ -> x__ {_Request'ctx = y__}))
        Prelude.id
instance Data.ProtoLens.Message Request where
  messageName _ = Data.Text.pack "BtcLsp.Method.GetCfg.Request"
  packedMessageDescriptor _
    = "\n\
      \\aRequest\DC2,\n\
      \\ETXctx\CAN\SOH \SOH(\v2\SUB.BtcLsp.Data.HighLevel.CtxR\ETXctx"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        ctx__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ctx"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.Ctx)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'ctx")) ::
              Data.ProtoLens.FieldDescriptor Request
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, ctx__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Request'_unknownFields
        (\ x__ y__ -> x__ {_Request'_unknownFields = y__})
  defMessage
    = Request'_constructor
        {_Request'ctx = Prelude.Nothing, _Request'_unknownFields = []}
  parseMessage
    = let
        loop :: Request -> Data.ProtoLens.Encoding.Bytes.Parser Request
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "ctx"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"ctx") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Request"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'ctx") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Request where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Request'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Request'ctx x__) ())
{- | Fields :
     
         * 'Proto.BtcLsp.Method.GetCfg_Fields.ctx' @:: Lens' Response Proto.BtcLsp.Data.HighLevel.Ctx@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'ctx' @:: Lens' Response (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.Ctx)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'either' @:: Lens' Response (Prelude.Maybe Response'Either)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'success' @:: Lens' Response (Prelude.Maybe Response'Success)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.success' @:: Lens' Response Response'Success@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'failure' @:: Lens' Response (Prelude.Maybe Response'Failure)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.failure' @:: Lens' Response Response'Failure@ -}
data Response
  = Response'_constructor {_Response'ctx :: !(Prelude.Maybe Proto.BtcLsp.Data.HighLevel.Ctx),
                           _Response'either :: !(Prelude.Maybe Response'Either),
                           _Response'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord, GHC.Generics.Generic)
instance Prelude.Show Response where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Text.PrettyPrint.GenericPretty.Out Response
data Response'Either
  = Response'Success' !Response'Success |
    Response'Failure' !Response'Failure
  deriving stock (Prelude.Show,
                  Prelude.Eq,
                  Prelude.Ord,
                  GHC.Generics.Generic)
instance Text.PrettyPrint.GenericPretty.Out Response'Either
instance Data.ProtoLens.Field.HasField Response "ctx" Proto.BtcLsp.Data.HighLevel.Ctx where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'ctx (\ x__ y__ -> x__ {_Response'ctx = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Response "maybe'ctx" (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.Ctx) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'ctx (\ x__ y__ -> x__ {_Response'ctx = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Response "maybe'either" (Prelude.Maybe Response'Either) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'either (\ x__ y__ -> x__ {_Response'either = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Response "maybe'success" (Prelude.Maybe Response'Success) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'either (\ x__ y__ -> x__ {_Response'either = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Response'Success' x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Response'Success' y__))
instance Data.ProtoLens.Field.HasField Response "success" Response'Success where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'either (\ x__ y__ -> x__ {_Response'either = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Response'Success' x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Response'Success' y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField Response "maybe'failure" (Prelude.Maybe Response'Failure) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'either (\ x__ y__ -> x__ {_Response'either = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (Response'Failure' x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap Response'Failure' y__))
instance Data.ProtoLens.Field.HasField Response "failure" Response'Failure where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'either (\ x__ y__ -> x__ {_Response'either = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (Response'Failure' x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap Response'Failure' y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message Response where
  messageName _ = Data.Text.pack "BtcLsp.Method.GetCfg.Response"
  packedMessageDescriptor _
    = "\n\
      \\bResponse\DC2,\n\
      \\ETXctx\CAN\SOH \SOH(\v2\SUB.BtcLsp.Data.HighLevel.CtxR\ETXctx\DC2B\n\
      \\asuccess\CAN\STX \SOH(\v2&.BtcLsp.Method.GetCfg.Response.SuccessH\NULR\asuccess\DC2B\n\
      \\afailure\CAN\ETX \SOH(\v2&.BtcLsp.Method.GetCfg.Response.FailureH\NULR\afailure\SUB\175\EOT\n\
      \\aSuccess\DC2?\n\
      \\flsp_ln_nodes\CAN\SOH \ETX(\v2\GS.BtcLsp.Data.HighLevel.LnPeerR\n\
      \lspLnNodes\DC2S\n\
      \\DC4swap_into_ln_min_amt\CAN\STX \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapIntoLnMinAmt\DC2S\n\
      \\DC4swap_into_ln_max_amt\CAN\ETX \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapIntoLnMaxAmt\DC2S\n\
      \\DC4swap_from_ln_min_amt\CAN\EOT \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapFromLnMinAmt\DC2S\n\
      \\DC4swap_from_ln_max_amt\CAN\ENQ \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapFromLnMaxAmt\DC2G\n\
      \\DLEswap_ln_fee_rate\CAN\ACK \SOH(\v2\RS.BtcLsp.Data.HighLevel.FeeRateR\rswapLnFeeRate\DC2F\n\
      \\SIswap_ln_min_fee\CAN\a \SOH(\v2\US.BtcLsp.Data.HighLevel.FeeMoneyR\fswapLnMinFee\SUB\250\SOH\n\
      \\aFailure\DC2=\n\
      \\ageneric\CAN\SOH \ETX(\v2#.BtcLsp.Data.HighLevel.InputFailureR\ageneric\DC2O\n\
      \\bspecific\CAN\STX \ETX(\SO23.BtcLsp.Method.GetCfg.Response.Failure.InputFailureR\bspecific\DC2B\n\
      \\binternal\CAN\ETX \ETX(\v2&.BtcLsp.Data.HighLevel.InternalFailureR\binternal\"\ESC\n\
      \\fInputFailure\DC2\v\n\
      \\aDEFAULT\DLE\NULB\b\n\
      \\ACKeither"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        ctx__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ctx"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.Ctx)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'ctx")) ::
              Data.ProtoLens.FieldDescriptor Response
        success__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "success"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Response'Success)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'success")) ::
              Data.ProtoLens.FieldDescriptor Response
        failure__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "failure"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Response'Failure)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'failure")) ::
              Data.ProtoLens.FieldDescriptor Response
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, ctx__field_descriptor),
           (Data.ProtoLens.Tag 2, success__field_descriptor),
           (Data.ProtoLens.Tag 3, failure__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Response'_unknownFields
        (\ x__ y__ -> x__ {_Response'_unknownFields = y__})
  defMessage
    = Response'_constructor
        {_Response'ctx = Prelude.Nothing,
         _Response'either = Prelude.Nothing, _Response'_unknownFields = []}
  parseMessage
    = let
        loop :: Response -> Data.ProtoLens.Encoding.Bytes.Parser Response
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "ctx"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"ctx") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "success"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"success") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "failure"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"failure") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Response"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'ctx") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'either") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (Response'Success' v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v)
                   (Prelude.Just (Response'Failure' v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Response where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Response'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Response'ctx x__)
                (Control.DeepSeq.deepseq (_Response'either x__) ()))
instance Control.DeepSeq.NFData Response'Either where
  rnf (Response'Success' x__) = Control.DeepSeq.rnf x__
  rnf (Response'Failure' x__) = Control.DeepSeq.rnf x__
_Response'Success' ::
  Data.ProtoLens.Prism.Prism' Response'Either Response'Success
_Response'Success'
  = Data.ProtoLens.Prism.prism'
      Response'Success'
      (\ p__
         -> case p__ of
              (Response'Success' p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_Response'Failure' ::
  Data.ProtoLens.Prism.Prism' Response'Either Response'Failure
_Response'Failure'
  = Data.ProtoLens.Prism.prism'
      Response'Failure'
      (\ p__
         -> case p__ of
              (Response'Failure' p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.BtcLsp.Method.GetCfg_Fields.generic' @:: Lens' Response'Failure [Proto.BtcLsp.Data.HighLevel.InputFailure]@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.vec'generic' @:: Lens' Response'Failure (Data.Vector.Vector Proto.BtcLsp.Data.HighLevel.InputFailure)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.specific' @:: Lens' Response'Failure [Response'Failure'InputFailure]@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.vec'specific' @:: Lens' Response'Failure (Data.Vector.Vector Response'Failure'InputFailure)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.internal' @:: Lens' Response'Failure [Proto.BtcLsp.Data.HighLevel.InternalFailure]@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.vec'internal' @:: Lens' Response'Failure (Data.Vector.Vector Proto.BtcLsp.Data.HighLevel.InternalFailure)@ -}
data Response'Failure
  = Response'Failure'_constructor {_Response'Failure'generic :: !(Data.Vector.Vector Proto.BtcLsp.Data.HighLevel.InputFailure),
                                   _Response'Failure'specific :: !(Data.Vector.Vector Response'Failure'InputFailure),
                                   _Response'Failure'internal :: !(Data.Vector.Vector Proto.BtcLsp.Data.HighLevel.InternalFailure),
                                   _Response'Failure'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord, GHC.Generics.Generic)
instance Prelude.Show Response'Failure where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Text.PrettyPrint.GenericPretty.Out Response'Failure
instance Data.ProtoLens.Field.HasField Response'Failure "generic" [Proto.BtcLsp.Data.HighLevel.InputFailure] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Failure'generic
           (\ x__ y__ -> x__ {_Response'Failure'generic = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Response'Failure "vec'generic" (Data.Vector.Vector Proto.BtcLsp.Data.HighLevel.InputFailure) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Failure'generic
           (\ x__ y__ -> x__ {_Response'Failure'generic = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Response'Failure "specific" [Response'Failure'InputFailure] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Failure'specific
           (\ x__ y__ -> x__ {_Response'Failure'specific = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Response'Failure "vec'specific" (Data.Vector.Vector Response'Failure'InputFailure) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Failure'specific
           (\ x__ y__ -> x__ {_Response'Failure'specific = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Response'Failure "internal" [Proto.BtcLsp.Data.HighLevel.InternalFailure] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Failure'internal
           (\ x__ y__ -> x__ {_Response'Failure'internal = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Response'Failure "vec'internal" (Data.Vector.Vector Proto.BtcLsp.Data.HighLevel.InternalFailure) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Failure'internal
           (\ x__ y__ -> x__ {_Response'Failure'internal = y__}))
        Prelude.id
instance Data.ProtoLens.Message Response'Failure where
  messageName _
    = Data.Text.pack "BtcLsp.Method.GetCfg.Response.Failure"
  packedMessageDescriptor _
    = "\n\
      \\aFailure\DC2=\n\
      \\ageneric\CAN\SOH \ETX(\v2#.BtcLsp.Data.HighLevel.InputFailureR\ageneric\DC2O\n\
      \\bspecific\CAN\STX \ETX(\SO23.BtcLsp.Method.GetCfg.Response.Failure.InputFailureR\bspecific\DC2B\n\
      \\binternal\CAN\ETX \ETX(\v2&.BtcLsp.Data.HighLevel.InternalFailureR\binternal\"\ESC\n\
      \\fInputFailure\DC2\v\n\
      \\aDEFAULT\DLE\NUL"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        generic__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "generic"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.InputFailure)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"generic")) ::
              Data.ProtoLens.FieldDescriptor Response'Failure
        specific__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "specific"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor Response'Failure'InputFailure)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed (Data.ProtoLens.Field.field @"specific")) ::
              Data.ProtoLens.FieldDescriptor Response'Failure
        internal__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "internal"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.InternalFailure)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"internal")) ::
              Data.ProtoLens.FieldDescriptor Response'Failure
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, generic__field_descriptor),
           (Data.ProtoLens.Tag 2, specific__field_descriptor),
           (Data.ProtoLens.Tag 3, internal__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Response'Failure'_unknownFields
        (\ x__ y__ -> x__ {_Response'Failure'_unknownFields = y__})
  defMessage
    = Response'Failure'_constructor
        {_Response'Failure'generic = Data.Vector.Generic.empty,
         _Response'Failure'specific = Data.Vector.Generic.empty,
         _Response'Failure'internal = Data.Vector.Generic.empty,
         _Response'Failure'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Response'Failure
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.BtcLsp.Data.HighLevel.InputFailure
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.BtcLsp.Data.HighLevel.InternalFailure
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Response'Failure'InputFailure
                   -> Data.ProtoLens.Encoding.Bytes.Parser Response'Failure
        loop x mutable'generic mutable'internal mutable'specific
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'generic <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'generic)
                      frozen'internal <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'internal)
                      frozen'specific <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'specific)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'generic") frozen'generic
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'internal") frozen'internal
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'specific") frozen'specific
                                    x))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "generic"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'generic y)
                                loop x v mutable'internal mutable'specific
                        16
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.toEnum
                                           (Prelude.fmap
                                              Prelude.fromIntegral
                                              Data.ProtoLens.Encoding.Bytes.getVarInt))
                                        "specific"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'specific y)
                                loop x mutable'generic mutable'internal v
                        18
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.toEnum
                                                                       (Prelude.fmap
                                                                          Prelude.fromIntegral
                                                                          Data.ProtoLens.Encoding.Bytes.getVarInt))
                                                                    "specific"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'specific)
                                loop x mutable'generic mutable'internal y
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "internal"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'internal y)
                                loop x mutable'generic v mutable'specific
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'generic mutable'internal mutable'specific
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'generic <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              mutable'internal <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              mutable'specific <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage mutable'generic mutable'internal
                mutable'specific)
          "Failure"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'generic") _x))
             ((Data.Monoid.<>)
                (let
                   p = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'specific") _x
                 in
                   if Data.Vector.Generic.null p then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            (Data.ProtoLens.Encoding.Bytes.runBuilder
                               (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                  ((Prelude..)
                                     ((Prelude..)
                                        Data.ProtoLens.Encoding.Bytes.putVarInt
                                        Prelude.fromIntegral)
                                     Prelude.fromEnum)
                                  p))))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'internal") _x))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Response'Failure where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Response'Failure'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Response'Failure'generic x__)
                (Control.DeepSeq.deepseq
                   (_Response'Failure'specific x__)
                   (Control.DeepSeq.deepseq (_Response'Failure'internal x__) ())))
newtype Response'Failure'InputFailure'UnrecognizedValue
  = Response'Failure'InputFailure'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq,
                  Prelude.Ord,
                  Prelude.Show,
                  GHC.Generics.Generic)
instance Text.PrettyPrint.GenericPretty.Out Response'Failure'InputFailure'UnrecognizedValue
data Response'Failure'InputFailure
  = Response'Failure'DEFAULT |
    Response'Failure'InputFailure'Unrecognized !Response'Failure'InputFailure'UnrecognizedValue
  deriving stock (Prelude.Show,
                  Prelude.Eq,
                  Prelude.Ord,
                  GHC.Generics.Generic)
instance Data.ProtoLens.MessageEnum Response'Failure'InputFailure where
  maybeToEnum 0 = Prelude.Just Response'Failure'DEFAULT
  maybeToEnum k
    = Prelude.Just
        (Response'Failure'InputFailure'Unrecognized
           (Response'Failure'InputFailure'UnrecognizedValue
              (Prelude.fromIntegral k)))
  showEnum Response'Failure'DEFAULT = "DEFAULT"
  showEnum
    (Response'Failure'InputFailure'Unrecognized (Response'Failure'InputFailure'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "DEFAULT" = Prelude.Just Response'Failure'DEFAULT
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded Response'Failure'InputFailure where
  minBound = Response'Failure'DEFAULT
  maxBound = Response'Failure'DEFAULT
instance Prelude.Enum Response'Failure'InputFailure where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum InputFailure: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum Response'Failure'DEFAULT = 0
  fromEnum
    (Response'Failure'InputFailure'Unrecognized (Response'Failure'InputFailure'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ Response'Failure'DEFAULT
    = Prelude.error
        "Response'Failure'InputFailure.succ: bad argument Response'Failure'DEFAULT. This value would be out of bounds."
  succ (Response'Failure'InputFailure'Unrecognized _)
    = Prelude.error
        "Response'Failure'InputFailure.succ: bad argument: unrecognized value"
  pred Response'Failure'DEFAULT
    = Prelude.error
        "Response'Failure'InputFailure.pred: bad argument Response'Failure'DEFAULT. This value would be out of bounds."
  pred (Response'Failure'InputFailure'Unrecognized _)
    = Prelude.error
        "Response'Failure'InputFailure.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault Response'Failure'InputFailure where
  fieldDefault = Response'Failure'DEFAULT
instance Control.DeepSeq.NFData Response'Failure'InputFailure where
  rnf x__ = Prelude.seq x__ ()
instance Text.PrettyPrint.GenericPretty.Out Response'Failure'InputFailure
{- | Fields :
     
         * 'Proto.BtcLsp.Method.GetCfg_Fields.lspLnNodes' @:: Lens' Response'Success [Proto.BtcLsp.Data.HighLevel.LnPeer]@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.vec'lspLnNodes' @:: Lens' Response'Success (Data.Vector.Vector Proto.BtcLsp.Data.HighLevel.LnPeer)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.swapIntoLnMinAmt' @:: Lens' Response'Success Proto.BtcLsp.Data.HighLevel.LocalBalance@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'swapIntoLnMinAmt' @:: Lens' Response'Success (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.swapIntoLnMaxAmt' @:: Lens' Response'Success Proto.BtcLsp.Data.HighLevel.LocalBalance@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'swapIntoLnMaxAmt' @:: Lens' Response'Success (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.swapFromLnMinAmt' @:: Lens' Response'Success Proto.BtcLsp.Data.HighLevel.LocalBalance@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'swapFromLnMinAmt' @:: Lens' Response'Success (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.swapFromLnMaxAmt' @:: Lens' Response'Success Proto.BtcLsp.Data.HighLevel.LocalBalance@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'swapFromLnMaxAmt' @:: Lens' Response'Success (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.swapLnFeeRate' @:: Lens' Response'Success Proto.BtcLsp.Data.HighLevel.FeeRate@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'swapLnFeeRate' @:: Lens' Response'Success (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.FeeRate)@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.swapLnMinFee' @:: Lens' Response'Success Proto.BtcLsp.Data.HighLevel.FeeMoney@
         * 'Proto.BtcLsp.Method.GetCfg_Fields.maybe'swapLnMinFee' @:: Lens' Response'Success (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.FeeMoney)@ -}
data Response'Success
  = Response'Success'_constructor {_Response'Success'lspLnNodes :: !(Data.Vector.Vector Proto.BtcLsp.Data.HighLevel.LnPeer),
                                   _Response'Success'swapIntoLnMinAmt :: !(Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance),
                                   _Response'Success'swapIntoLnMaxAmt :: !(Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance),
                                   _Response'Success'swapFromLnMinAmt :: !(Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance),
                                   _Response'Success'swapFromLnMaxAmt :: !(Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance),
                                   _Response'Success'swapLnFeeRate :: !(Prelude.Maybe Proto.BtcLsp.Data.HighLevel.FeeRate),
                                   _Response'Success'swapLnMinFee :: !(Prelude.Maybe Proto.BtcLsp.Data.HighLevel.FeeMoney),
                                   _Response'Success'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord, GHC.Generics.Generic)
instance Prelude.Show Response'Success where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Text.PrettyPrint.GenericPretty.Out Response'Success
instance Data.ProtoLens.Field.HasField Response'Success "lspLnNodes" [Proto.BtcLsp.Data.HighLevel.LnPeer] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'lspLnNodes
           (\ x__ y__ -> x__ {_Response'Success'lspLnNodes = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Response'Success "vec'lspLnNodes" (Data.Vector.Vector Proto.BtcLsp.Data.HighLevel.LnPeer) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'lspLnNodes
           (\ x__ y__ -> x__ {_Response'Success'lspLnNodes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Response'Success "swapIntoLnMinAmt" Proto.BtcLsp.Data.HighLevel.LocalBalance where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapIntoLnMinAmt
           (\ x__ y__ -> x__ {_Response'Success'swapIntoLnMinAmt = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Response'Success "maybe'swapIntoLnMinAmt" (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapIntoLnMinAmt
           (\ x__ y__ -> x__ {_Response'Success'swapIntoLnMinAmt = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Response'Success "swapIntoLnMaxAmt" Proto.BtcLsp.Data.HighLevel.LocalBalance where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapIntoLnMaxAmt
           (\ x__ y__ -> x__ {_Response'Success'swapIntoLnMaxAmt = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Response'Success "maybe'swapIntoLnMaxAmt" (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapIntoLnMaxAmt
           (\ x__ y__ -> x__ {_Response'Success'swapIntoLnMaxAmt = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Response'Success "swapFromLnMinAmt" Proto.BtcLsp.Data.HighLevel.LocalBalance where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapFromLnMinAmt
           (\ x__ y__ -> x__ {_Response'Success'swapFromLnMinAmt = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Response'Success "maybe'swapFromLnMinAmt" (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapFromLnMinAmt
           (\ x__ y__ -> x__ {_Response'Success'swapFromLnMinAmt = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Response'Success "swapFromLnMaxAmt" Proto.BtcLsp.Data.HighLevel.LocalBalance where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapFromLnMaxAmt
           (\ x__ y__ -> x__ {_Response'Success'swapFromLnMaxAmt = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Response'Success "maybe'swapFromLnMaxAmt" (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.LocalBalance) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapFromLnMaxAmt
           (\ x__ y__ -> x__ {_Response'Success'swapFromLnMaxAmt = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Response'Success "swapLnFeeRate" Proto.BtcLsp.Data.HighLevel.FeeRate where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapLnFeeRate
           (\ x__ y__ -> x__ {_Response'Success'swapLnFeeRate = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Response'Success "maybe'swapLnFeeRate" (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.FeeRate) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapLnFeeRate
           (\ x__ y__ -> x__ {_Response'Success'swapLnFeeRate = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Response'Success "swapLnMinFee" Proto.BtcLsp.Data.HighLevel.FeeMoney where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapLnMinFee
           (\ x__ y__ -> x__ {_Response'Success'swapLnMinFee = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Response'Success "maybe'swapLnMinFee" (Prelude.Maybe Proto.BtcLsp.Data.HighLevel.FeeMoney) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Response'Success'swapLnMinFee
           (\ x__ y__ -> x__ {_Response'Success'swapLnMinFee = y__}))
        Prelude.id
instance Data.ProtoLens.Message Response'Success where
  messageName _
    = Data.Text.pack "BtcLsp.Method.GetCfg.Response.Success"
  packedMessageDescriptor _
    = "\n\
      \\aSuccess\DC2?\n\
      \\flsp_ln_nodes\CAN\SOH \ETX(\v2\GS.BtcLsp.Data.HighLevel.LnPeerR\n\
      \lspLnNodes\DC2S\n\
      \\DC4swap_into_ln_min_amt\CAN\STX \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapIntoLnMinAmt\DC2S\n\
      \\DC4swap_into_ln_max_amt\CAN\ETX \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapIntoLnMaxAmt\DC2S\n\
      \\DC4swap_from_ln_min_amt\CAN\EOT \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapFromLnMinAmt\DC2S\n\
      \\DC4swap_from_ln_max_amt\CAN\ENQ \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapFromLnMaxAmt\DC2G\n\
      \\DLEswap_ln_fee_rate\CAN\ACK \SOH(\v2\RS.BtcLsp.Data.HighLevel.FeeRateR\rswapLnFeeRate\DC2F\n\
      \\SIswap_ln_min_fee\CAN\a \SOH(\v2\US.BtcLsp.Data.HighLevel.FeeMoneyR\fswapLnMinFee"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        lspLnNodes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "lsp_ln_nodes"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.LnPeer)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"lspLnNodes")) ::
              Data.ProtoLens.FieldDescriptor Response'Success
        swapIntoLnMinAmt__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "swap_into_ln_min_amt"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.LocalBalance)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'swapIntoLnMinAmt")) ::
              Data.ProtoLens.FieldDescriptor Response'Success
        swapIntoLnMaxAmt__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "swap_into_ln_max_amt"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.LocalBalance)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'swapIntoLnMaxAmt")) ::
              Data.ProtoLens.FieldDescriptor Response'Success
        swapFromLnMinAmt__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "swap_from_ln_min_amt"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.LocalBalance)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'swapFromLnMinAmt")) ::
              Data.ProtoLens.FieldDescriptor Response'Success
        swapFromLnMaxAmt__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "swap_from_ln_max_amt"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.LocalBalance)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'swapFromLnMaxAmt")) ::
              Data.ProtoLens.FieldDescriptor Response'Success
        swapLnFeeRate__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "swap_ln_fee_rate"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.FeeRate)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'swapLnFeeRate")) ::
              Data.ProtoLens.FieldDescriptor Response'Success
        swapLnMinFee__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "swap_ln_min_fee"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.BtcLsp.Data.HighLevel.FeeMoney)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'swapLnMinFee")) ::
              Data.ProtoLens.FieldDescriptor Response'Success
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, lspLnNodes__field_descriptor),
           (Data.ProtoLens.Tag 2, swapIntoLnMinAmt__field_descriptor),
           (Data.ProtoLens.Tag 3, swapIntoLnMaxAmt__field_descriptor),
           (Data.ProtoLens.Tag 4, swapFromLnMinAmt__field_descriptor),
           (Data.ProtoLens.Tag 5, swapFromLnMaxAmt__field_descriptor),
           (Data.ProtoLens.Tag 6, swapLnFeeRate__field_descriptor),
           (Data.ProtoLens.Tag 7, swapLnMinFee__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Response'Success'_unknownFields
        (\ x__ y__ -> x__ {_Response'Success'_unknownFields = y__})
  defMessage
    = Response'Success'_constructor
        {_Response'Success'lspLnNodes = Data.Vector.Generic.empty,
         _Response'Success'swapIntoLnMinAmt = Prelude.Nothing,
         _Response'Success'swapIntoLnMaxAmt = Prelude.Nothing,
         _Response'Success'swapFromLnMinAmt = Prelude.Nothing,
         _Response'Success'swapFromLnMaxAmt = Prelude.Nothing,
         _Response'Success'swapLnFeeRate = Prelude.Nothing,
         _Response'Success'swapLnMinFee = Prelude.Nothing,
         _Response'Success'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Response'Success
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.BtcLsp.Data.HighLevel.LnPeer
             -> Data.ProtoLens.Encoding.Bytes.Parser Response'Success
        loop x mutable'lspLnNodes
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'lspLnNodes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'lspLnNodes)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'lspLnNodes") frozen'lspLnNodes
                              x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "lsp_ln_nodes"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'lspLnNodes y)
                                loop x v
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "swap_into_ln_min_amt"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"swapIntoLnMinAmt") y x)
                                  mutable'lspLnNodes
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "swap_into_ln_max_amt"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"swapIntoLnMaxAmt") y x)
                                  mutable'lspLnNodes
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "swap_from_ln_min_amt"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"swapFromLnMinAmt") y x)
                                  mutable'lspLnNodes
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "swap_from_ln_max_amt"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"swapFromLnMaxAmt") y x)
                                  mutable'lspLnNodes
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "swap_ln_fee_rate"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"swapLnFeeRate") y x)
                                  mutable'lspLnNodes
                        58
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "swap_ln_min_fee"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"swapLnMinFee") y x)
                                  mutable'lspLnNodes
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'lspLnNodes
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'lspLnNodes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'lspLnNodes)
          "Success"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view
                   (Data.ProtoLens.Field.field @"vec'lspLnNodes") _x))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'swapIntoLnMinAmt") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'swapIntoLnMaxAmt") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'swapFromLnMinAmt") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage _v))
                      ((Data.Monoid.<>)
                         (case
                              Lens.Family2.view
                                (Data.ProtoLens.Field.field @"maybe'swapFromLnMaxAmt") _x
                          of
                            Prelude.Nothing -> Data.Monoid.mempty
                            (Prelude.Just _v)
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                   ((Prelude..)
                                      (\ bs
                                         -> (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                              (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Data.ProtoLens.encodeMessage _v))
                         ((Data.Monoid.<>)
                            (case
                                 Lens.Family2.view
                                   (Data.ProtoLens.Field.field @"maybe'swapLnFeeRate") _x
                             of
                               Prelude.Nothing -> Data.Monoid.mempty
                               (Prelude.Just _v)
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                      ((Prelude..)
                                         (\ bs
                                            -> (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    (Prelude.fromIntegral
                                                       (Data.ByteString.length bs)))
                                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                         Data.ProtoLens.encodeMessage _v))
                            ((Data.Monoid.<>)
                               (case
                                    Lens.Family2.view
                                      (Data.ProtoLens.Field.field @"maybe'swapLnMinFee") _x
                                of
                                  Prelude.Nothing -> Data.Monoid.mempty
                                  (Prelude.Just _v)
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                                         ((Prelude..)
                                            (\ bs
                                               -> (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       (Prelude.fromIntegral
                                                          (Data.ByteString.length bs)))
                                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                            Data.ProtoLens.encodeMessage _v))
                               (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                  (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))))
instance Control.DeepSeq.NFData Response'Success where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Response'Success'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Response'Success'lspLnNodes x__)
                (Control.DeepSeq.deepseq
                   (_Response'Success'swapIntoLnMinAmt x__)
                   (Control.DeepSeq.deepseq
                      (_Response'Success'swapIntoLnMaxAmt x__)
                      (Control.DeepSeq.deepseq
                         (_Response'Success'swapFromLnMinAmt x__)
                         (Control.DeepSeq.deepseq
                            (_Response'Success'swapFromLnMaxAmt x__)
                            (Control.DeepSeq.deepseq
                               (_Response'Success'swapLnFeeRate x__)
                               (Control.DeepSeq.deepseq
                                  (_Response'Success'swapLnMinFee x__) ())))))))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\FSbtc_lsp/method/get_cfg.proto\DC2\DC4BtcLsp.Method.GetCfg\SUB\GSbtc_lsp/data/high_level.proto\"7\n\
    \\aRequest\DC2,\n\
    \\ETXctx\CAN\SOH \SOH(\v2\SUB.BtcLsp.Data.HighLevel.CtxR\ETXctx\"\249\a\n\
    \\bResponse\DC2,\n\
    \\ETXctx\CAN\SOH \SOH(\v2\SUB.BtcLsp.Data.HighLevel.CtxR\ETXctx\DC2B\n\
    \\asuccess\CAN\STX \SOH(\v2&.BtcLsp.Method.GetCfg.Response.SuccessH\NULR\asuccess\DC2B\n\
    \\afailure\CAN\ETX \SOH(\v2&.BtcLsp.Method.GetCfg.Response.FailureH\NULR\afailure\SUB\175\EOT\n\
    \\aSuccess\DC2?\n\
    \\flsp_ln_nodes\CAN\SOH \ETX(\v2\GS.BtcLsp.Data.HighLevel.LnPeerR\n\
    \lspLnNodes\DC2S\n\
    \\DC4swap_into_ln_min_amt\CAN\STX \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapIntoLnMinAmt\DC2S\n\
    \\DC4swap_into_ln_max_amt\CAN\ETX \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapIntoLnMaxAmt\DC2S\n\
    \\DC4swap_from_ln_min_amt\CAN\EOT \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapFromLnMinAmt\DC2S\n\
    \\DC4swap_from_ln_max_amt\CAN\ENQ \SOH(\v2#.BtcLsp.Data.HighLevel.LocalBalanceR\DLEswapFromLnMaxAmt\DC2G\n\
    \\DLEswap_ln_fee_rate\CAN\ACK \SOH(\v2\RS.BtcLsp.Data.HighLevel.FeeRateR\rswapLnFeeRate\DC2F\n\
    \\SIswap_ln_min_fee\CAN\a \SOH(\v2\US.BtcLsp.Data.HighLevel.FeeMoneyR\fswapLnMinFee\SUB\250\SOH\n\
    \\aFailure\DC2=\n\
    \\ageneric\CAN\SOH \ETX(\v2#.BtcLsp.Data.HighLevel.InputFailureR\ageneric\DC2O\n\
    \\bspecific\CAN\STX \ETX(\SO23.BtcLsp.Method.GetCfg.Response.Failure.InputFailureR\bspecific\DC2B\n\
    \\binternal\CAN\ETX \ETX(\v2&.BtcLsp.Data.HighLevel.InternalFailureR\binternal\"\ESC\n\
    \\fInputFailure\DC2\v\n\
    \\aDEFAULT\DLE\NULB\b\n\
    \\ACKeitherJ\146\t\n\
    \\ACK\DC2\EOT\NUL\NUL%\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DLE\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\GS\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\EOT\NUL'\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\ACK\NUL\b\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\ACK\b\SI\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\a\STX%\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\a\STX\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\a\GS \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\a#$\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\n\
    \\NUL%\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\n\
    \\b\DLE\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\v\STX%\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX\v\STX\FS\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\v\GS \n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\v#$\n\
    \\f\n\
    \\EOT\EOT\SOH\b\NUL\DC2\EOT\r\STX\DLE\ETX\n\
    \\f\n\
    \\ENQ\EOT\SOH\b\NUL\SOH\DC2\ETX\r\b\SO\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\SO\EOT\CAN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETX\SO\EOT\v\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\SO\f\DC3\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\SO\SYN\ETB\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\STX\DC2\ETX\SI\EOT\CAN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ACK\DC2\ETX\SI\EOT\v\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\SOH\DC2\ETX\SI\f\DC3\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ETX\DC2\ETX\SI\SYN\ETB\n\
    \\f\n\
    \\EOT\EOT\SOH\ETX\NUL\DC2\EOT\DC2\STX\SUB\ETX\n\
    \\f\n\
    \\ENQ\EOT\SOH\ETX\NUL\SOH\DC2\ETX\DC2\n\
    \\DC1\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\NUL\STX\NUL\DC2\ETX\DC3\EOT<\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\NUL\EOT\DC2\ETX\DC3\EOT\f\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\NUL\ACK\DC2\ETX\DC3\r*\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\NUL\SOH\DC2\ETX\DC3+7\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\NUL\ETX\DC2\ETX\DC3:;\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\NUL\STX\SOH\DC2\ETX\DC4\EOTA\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\SOH\ACK\DC2\ETX\DC4\EOT'\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\SOH\SOH\DC2\ETX\DC4(<\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\SOH\ETX\DC2\ETX\DC4?@\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\NUL\STX\STX\DC2\ETX\NAK\EOTA\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\STX\ACK\DC2\ETX\NAK\EOT'\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\STX\SOH\DC2\ETX\NAK(<\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\STX\ETX\DC2\ETX\NAK?@\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\NUL\STX\ETX\DC2\ETX\SYN\EOTA\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\ETX\ACK\DC2\ETX\SYN\EOT'\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\ETX\SOH\DC2\ETX\SYN(<\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\ETX\ETX\DC2\ETX\SYN?@\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\NUL\STX\EOT\DC2\ETX\ETB\EOTA\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\EOT\ACK\DC2\ETX\ETB\EOT'\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\EOT\SOH\DC2\ETX\ETB(<\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\EOT\ETX\DC2\ETX\ETB?@\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\NUL\STX\ENQ\DC2\ETX\CAN\EOT8\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\ENQ\ACK\DC2\ETX\CAN\EOT\"\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\ENQ\SOH\DC2\ETX\CAN#3\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\ENQ\ETX\DC2\ETX\CAN67\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\NUL\STX\ACK\DC2\ETX\EM\EOT8\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\ACK\ACK\DC2\ETX\EM\EOT#\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\ACK\SOH\DC2\ETX\EM$3\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\NUL\STX\ACK\ETX\DC2\ETX\EM67\n\
    \\f\n\
    \\EOT\EOT\SOH\ETX\SOH\DC2\EOT\FS\STX$\ETX\n\
    \\f\n\
    \\ENQ\EOT\SOH\ETX\SOH\SOH\DC2\ETX\FS\n\
    \\DC1\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\SOH\STX\NUL\DC2\ETX\GS\EOT=\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\NUL\EOT\DC2\ETX\GS\EOT\f\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\NUL\ACK\DC2\ETX\GS\r0\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\NUL\SOH\DC2\ETX\GS18\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\NUL\ETX\DC2\ETX\GS;<\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\SOH\STX\SOH\DC2\ETX\RS\EOT'\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\SOH\EOT\DC2\ETX\RS\EOT\f\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\SOH\ACK\DC2\ETX\RS\r\EM\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\SOH\SOH\DC2\ETX\RS\SUB\"\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\SOH\ETX\DC2\ETX\RS%&\n\
    \\r\n\
    \\ACK\EOT\SOH\ETX\SOH\STX\STX\DC2\ETX\US\EOTA\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\STX\EOT\DC2\ETX\US\EOT\f\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\STX\ACK\DC2\ETX\US\r3\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\STX\SOH\DC2\ETX\US4<\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\STX\STX\ETX\DC2\ETX\US?@\n\
    \\SO\n\
    \\ACK\EOT\SOH\ETX\SOH\EOT\NUL\DC2\EOT!\EOT#\ENQ\n\
    \\SO\n\
    \\a\EOT\SOH\ETX\SOH\EOT\NUL\SOH\DC2\ETX!\t\NAK\n\
    \\SI\n\
    \\b\EOT\SOH\ETX\SOH\EOT\NUL\STX\NUL\DC2\ETX\"\ACK\DC2\n\
    \\DLE\n\
    \\t\EOT\SOH\ETX\SOH\EOT\NUL\STX\NUL\SOH\DC2\ETX\"\ACK\r\n\
    \\DLE\n\
    \\t\EOT\SOH\ETX\SOH\EOT\NUL\STX\NUL\STX\DC2\ETX\"\DLE\DC1b\ACKproto3"