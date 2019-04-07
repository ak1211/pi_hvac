{-
 PiHVAC <https://github.com/ak1211/pi_hvac>
 Copyright 2019 Akihiro Yamamoto

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-}

module InfraredCode
  ( Bit(..)
  , Count(..)
  , InfraredBasebandSignals(..)
  , InfraredHexString
  , InfraredLeader(..)
  , LsbFirst(..)
  , ProcessError
  , Pulse
  , deserialize
  , fromMilliseconds
  , infraredBasebandPhase1
  , infraredBasebandPhase2
  , infraredBasebandPhase3
  , infraredBasebandSignals
  , infraredHexStringParser
  , showBit
  , showLsbFirst
  , toMilliseconds
  ) where

import Prelude

import Control.MonadZero (guard)
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Ring (genericSub)
import Data.Generic.Rep.Semiring (genericAdd, genericMul, genericOne, genericZero)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Unfoldable1 (unfoldr1)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Token (hexDigit)
import Utils (toArray2D)

-- | input infrared hexadecimal code
type InfraredHexString = String

-- |
type ProcessError = String

-- |
type Pulse = {on :: Count, off :: Count}

-- | count is based on 38khz carrier
newtype Count = Count Int
derive instance genericCount  :: Generic Count _
derive instance newtypeCount  :: Newtype Count _
derive instance eqCount       :: Eq Count
derive instance ordCount      :: Ord Count
instance showCount            :: Show Count where
  show = genericShow
instance semiringCount        :: Semiring Count where
  add = genericAdd
  zero = genericZero
  mul = genericMul
  one = genericOne
instance ringCount            :: Ring Count where
  sub = genericSub
instance commutativeRingCount :: CommutativeRing Count
instance eucideanRingCount    :: EuclideanRing Count where
  degree (Count c) = degree c
  div (Count c) (Count d) = Count (div c d)
  mod (Count c) (Count d) = Count (mod c d)

-- |
withTolerance
  :: {upper :: Milliseconds, lower :: Milliseconds}
  -> Milliseconds
  -> Array Count
withTolerance tolerance typical =
  let (Count up) = fromMilliseconds tolerance.upper
      (Count lo) = fromMilliseconds tolerance.lower
      (Count typ) = fromMilliseconds typical
  in
  Count <$> (typ - lo) .. (typ + up)

-- |
fromMilliseconds :: Milliseconds -> Count
fromMilliseconds (Milliseconds msec)=
  let freq    = 38    -- 38.0 kHz
      usec    = 1000 / freq
  in
  Count $ Int.floor (1000.0 * msec) / usec

-- |
toMilliseconds :: Count -> Milliseconds
toMilliseconds (Count counts) =
  let freq    = 38    -- 38.0 kHz
      msec10x = 10 * counts / freq
  in
  Milliseconds (Int.toNumber msec10x / 10.0)

-- |
data Bit
  = Negate
  | Assert
derive instance eqBit :: Eq Bit
instance showBit'     :: Show Bit where
  show = showBit

--|
showBit :: Bit -> String
showBit Negate = "0"
showBit Assert = "1"

-- |
data InfraredLeader
  = ProtoAeha Pulse 
  | ProtoNec Pulse 
  | ProtoSirc Pulse 
  | ProtoUnknown Pulse 
derive instance genericInfraredLeader :: Generic InfraredLeader _
derive instance eqInfraredLeader      :: Eq InfraredLeader
instance showInfraredLeader           :: Show InfraredLeader where
  show = genericShow

-- | InfraredLeader data constructor
makeInfraredLeader :: Pulse -> InfraredLeader 
makeInfraredLeader = case _ of
  p | aeha p    -> ProtoAeha p
    | nec p     -> ProtoNec p
    | sirc p    -> ProtoSirc p
    | otherwise -> ProtoUnknown p
  where

  -- | upper lower tolerance 0.2ms
  typical = withTolerance {upper: Milliseconds 0.2, lower: Milliseconds 0.2}

  -- | H-level width, typical 3.4ms
  -- | L-level width, typical 1.7ms
  aeha :: Pulse -> Boolean
  aeha pulse =
    let on_   = typical (Milliseconds 3.4)
        off_  = typical (Milliseconds 1.7)
    in
    (Array.any (_ == pulse.on) on_) && (Array.any (_ == pulse.off) off_)

  -- | H-level width, typical 9.0ms
  -- | L-level width, typical 4.5ms
  nec :: Pulse -> Boolean
  nec pulse =
    let on_   = typical (Milliseconds 9.0)
        off_  = typical (Milliseconds 4.5)
    in
    (Array.any (_ == pulse.on) on_) && (Array.any (_ == pulse.off) off_)

  -- | H-level width, typical 2.4ms
  -- | L-level width, typical 0.6ms
  sirc :: Pulse -> Boolean
  sirc pulse =
    let on_   = typical (Milliseconds 2.4)
        off_  = typical (Milliseconds 0.6)
    in
    (Array.any (_ == pulse.on) on_) && (Array.any (_ == pulse.off) off_)

-- |
newtype LsbFirst = LsbFirst (NonEmptyArray Bit)
derive instance eqLsbFirst      :: Eq LsbFirst
instance showLsbFirst'          :: Show LsbFirst where
  show = showLsbFirst

-- |
showLsbFirst  :: LsbFirst -> String
showLsbFirst (LsbFirst xs) =
  "(LsbFirst " <> (String.joinWith "" $ NEA.toArray $ map showBit xs) <> ")"

-- |
data InfraredBasebandSignals
  = Unknown (Array Bit)
  | AEHA  {customer0 :: LsbFirst, customer1 :: LsbFirst, parity :: LsbFirst, data0 :: LsbFirst, data :: Array LsbFirst}
  | NEC   {customer0 :: LsbFirst, customer1 :: LsbFirst, data :: LsbFirst, invData :: LsbFirst}
  | SIRC {command :: LsbFirst, address :: LsbFirst}
derive instance genericInfraredBasebandSignals  :: Generic InfraredBasebandSignals _
derive instance eqInfraredBasebandSignals       :: Eq InfraredBasebandSignals
instance showInfraredBasebandSignals            :: Show InfraredBasebandSignals where
  show = genericShow

-- |
infraredHexStringParser:: Parser InfraredHexString (Array Pulse)
infraredHexStringParser =
  Array.many pulse
  where

  pulse = do
    -- 入力値はon -> offの順番
    ton <- valueOf32Bit <?> "on-counts"
    toff <- valueOf32Bit <?> "off-counts"
    pure {on: Count ton, off: Count toff}

  valueOf32Bit = do
    -- 入力値はLower -> Higherの順番
    lower <- hexd16bit <?> "Lower-pair hex digit"
    higher<- hexd16bit <?> "Higher-pair hex digit"
    -- ここは普通の数字の書き方(位取り記数法: 高位が前, 下位が後)
    let str = higher <> lower
        maybeNum = Int.fromStringAs Int.hexadecimal str
    -- 入力値は検査済みなのでfromJustでよい
    pure (unsafePartial $ fromJust maybeNum)

  hexd16bit = do
    a <- hexDigit
    b <- hexDigit
    pure $ fromCharArray [ a, b ]

-- |
infraredBasebandSignals :: Array Pulse -> Either ProcessError (Array InfraredBasebandSignals)
infraredBasebandSignals =
  traverse (infraredBasebandPhase3 <=< infraredBasebandPhase2) <<< infraredBasebandPhase1 

-- | 入力を各フレームに分ける
infraredBasebandPhase1 :: Array Pulse -> Array (Array Pulse)
infraredBasebandPhase1 =
  unfoldr1 chop
  where

  chop :: Array Pulse -> Tuple (Array Pulse) (Maybe (Array Pulse))
  chop xs = 
    let x = Array.span (\c -> c.off < threshold) xs
    in
    case Array.drop 1 x.rest of
      [] -> Tuple (x.init <> Array.take 1 x.rest) Nothing
      a -> Tuple (x.init <> Array.take 1 x.rest) (Just a)

  threshold = fromMilliseconds (Milliseconds 8.0)

-- | 入力フレームをリーダ部とビット配列にする
infraredBasebandPhase2 :: Array Pulse -> Either ProcessError (Tuple InfraredLeader (Array Bit))
infraredBasebandPhase2 tokens =
  case Array.uncons tokens of
    Just {head: x, tail: xs} ->
      let leader = makeInfraredLeader x
      in
      Right $ Tuple leader (demodulate leader xs)

    Nothing ->
      Left "Unexpected end of input"
  where

  demodulate :: InfraredLeader -> Array Pulse -> Array Bit
  demodulate leader ps =
    case leader of
      ProtoSirc _ ->
        map sircModulation ps
      _ ->
        map pulseDistanceModulation ps
    where

    -- | pulse distance modulation is NEC, AEHA
    pulseDistanceModulation :: Pulse -> Bit
    pulseDistanceModulation = case _ of
      x | (Count 2 * x.on) <= x.off -> Assert
        | otherwise                 -> Negate

    sircModulation :: Pulse -> Bit
    sircModulation p =
      let threshold = withTolerance
                        {upper: Milliseconds 0.1, lower: Milliseconds 0.1}
                        (Milliseconds 1.2)
      in
      case Array.any (_ == p.on) threshold of
        true -> Assert
        false -> Negate

-- | 入力リーダ部とビット配列から赤外線信号にする
infraredBasebandPhase3 :: Tuple InfraredLeader (Array Bit) -> Either ProcessError InfraredBasebandSignals
infraredBasebandPhase3 input =
  case Tuple.fst input of
    ProtoAeha _     -> aeha
    ProtoNec _      -> nec 
    ProtoSirc _     -> sirc
    ProtoUnknown _  -> other
  where

  take :: Int -> Int -> ProcessError -> Either ProcessError (NonEmptyArray Bit)
  take begin length errmsg =
    let sigs = Array.slice begin (begin + length) (Tuple.snd input)
    in
    maybe (Left errmsg) Right do
      guard (Array.length sigs == length)
      NEA.fromArray sigs

  takeEnd :: Int -> ProcessError -> Either ProcessError (NonEmptyArray Bit)
  takeEnd begin errmsg =
    let xs = Array.drop (begin - 1) (Tuple.snd input)
    in
    maybe (Left errmsg) Right $ NEA.fromArray xs

  aeha :: Either ProcessError InfraredBasebandSignals
  aeha = do
    sCustomer0  <- take 0 8 "fail to read: customer0 code (AEHA)"
    sCustomer1  <- take 8 8 "fail to read: customer1 code (AEHA)"
    sParity     <- take 16 4 "fail to read: parity (AEHA)"
    sData0      <- take 20 4 "fail to read: data zero (AEHA)"
    sData       <- takeEnd 24 "fail to read: data (AEHA)"
    let octet = toArray2D 8 $ NEA.init sData
    { customer0: LsbFirst sCustomer0
    , customer1: LsbFirst sCustomer1
    , parity: LsbFirst sParity
    , data0: LsbFirst sData0
    , data: map LsbFirst $ Array.mapMaybe NEA.fromArray $ octet
    } # (Right <<< AEHA)

  nec :: Either ProcessError InfraredBasebandSignals
  nec = do
    sCustomer0 <- take 0 8 "fail to read: customer0 code (NEC)"
    sCustomer1 <- take 8 8 "fail to read: customer1 code (NEC)"
    sData       <- take 16 8 "fail to read: data code (NEC)"
    sInvData    <- take 24 8 "fail to read: inv-data code (NEC)"
    stopBit     <- take 32 1 "fail to read: stop bit (NEC)"
    { customer0: LsbFirst sCustomer0
    , customer1: LsbFirst sCustomer1
    , data: LsbFirst sData
    , invData: LsbFirst sInvData
    } # (Right <<< NEC)

  sirc :: Either ProcessError InfraredBasebandSignals
  sirc = do
    sCommand <- take 0 7 "fail to read: command code (SIRC)"
    sAddress <- takeEnd 8 "fail to read: address (SIRC)"
    { command: LsbFirst sCommand
    , address: LsbFirst sAddress
    } # (Right <<< SIRC)

  other :: Either ProcessError InfraredBasebandSignals
  other =
    Right $ Unknown (Tuple.snd input)

-- |
deserialize :: LsbFirst -> Int
deserialize (LsbFirst bits) =
  Array.foldl f 0 $ NEA.reverse bits
  where
  f :: Int -> Bit -> Int
  f acc Assert = acc * 2 + 1
  f acc Negate = acc * 2 + 0
