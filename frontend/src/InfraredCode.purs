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
  ( Baseband(..)
  , Bit(..)
  , Count(..)
  , InfraredCodes(..)
  , InfraredHexString
  , InfraredLeader(..)
  , LsbFirst(..)
  , ProcessError
  , Pulse
  , decodeBaseband
  , decodePhase1
  , decodePhase2
  , decodePhase3
  , deserialize
  , fromMilliseconds
  , infraredHexStringParser
  , showBit
  , showLsbFirst
  , toMilliseconds
  , toStringLsbFirst
  , toStringLsbFirstWithHex
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState)
import Control.Monad.State as State
import Control.MonadZero (guard)
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Ring (genericSub)
import Data.Generic.Rep.Semigroup (genericAppend)
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

-- |
newtype Baseband = Baseband (Array Pulse)
derive instance genericBaseband :: Generic Baseband _
derive instance newtypeBaseband :: Newtype Baseband _
derive instance eqBaseband      :: Eq Baseband
instance showBaseband           :: Show Baseband where
  show = genericShow

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
  let period  = 0.026   -- 1 / 38kHz
  in
  Count $ Int.floor (msec / period)

-- |
toMilliseconds :: Count -> Milliseconds
toMilliseconds (Count counts) =
  let period  = 0.026   -- 1 / 38kHz
  in
  Milliseconds (Int.toNumber counts * period)

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
derive instance genericLsbFirst :: Generic LsbFirst _
derive instance eqLsbFirst      :: Eq LsbFirst
instance semigroupLsbFirst      :: Semigroup LsbFirst where
  append = genericAppend
instance showLsbFirst'          :: Show LsbFirst where
  show = showLsbFirst

-- |
showLsbFirst  :: LsbFirst -> String
showLsbFirst (LsbFirst xs) =
  "(LsbFirst " <> (String.joinWith "" $ NEA.toArray $ map showBit xs) <> ")"

-- |
toStringLsbFirst :: LsbFirst -> String
toStringLsbFirst =
  Int.toStringAs Int.decimal <<< deserialize

-- |
toStringLsbFirstWithHex :: LsbFirst -> String
toStringLsbFirstWithHex =
  Int.toStringAs Int.hexadecimal <<< deserialize

-- |
deserialize :: LsbFirst -> Int
deserialize (LsbFirst bits) =
  Array.foldl f 0 $ NEA.reverse bits
  where
  f :: Int -> Bit -> Int
  f acc Assert = acc * 2 + 1
  f acc Negate = acc * 2 + 0

-- |
data InfraredCodes
  = Unknown (Array Bit)
  | AEHA {customLo :: LsbFirst, customHi :: LsbFirst, parity :: LsbFirst, data0 :: LsbFirst, data :: Array LsbFirst, stop :: Bit}
  | NEC  {customLo :: LsbFirst, customHi :: LsbFirst, data :: LsbFirst, invData :: LsbFirst, stop :: Bit}
  | SIRC {command :: LsbFirst, address :: LsbFirst}
derive instance genericInfraredCodes  :: Generic InfraredCodes _
derive instance eqInfraredCodes       :: Eq InfraredCodes
instance showInfraredCodes            :: Show InfraredCodes where
  show = genericShow

-- |
infraredHexStringParser:: Parser InfraredHexString Baseband
infraredHexStringParser =
  Baseband <$> Array.many pulse
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
decodeBaseband :: Baseband -> Either ProcessError (Array InfraredCodes)
decodeBaseband =
  traverse (decodePhase3 <=< decodePhase2) <<< decodePhase1 

-- | 入力を各フレームに分ける
decodePhase1 :: Baseband -> Array (Array Pulse)
decodePhase1 (Baseband bb) =
  unfoldr1 chop bb
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
decodePhase2 :: Array Pulse -> Either ProcessError (Tuple InfraredLeader (Array Bit))
decodePhase2 tokens =
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
decodePhase3 :: Tuple InfraredLeader (Array Bit) -> Either ProcessError InfraredCodes
decodePhase3 (Tuple leader bitarray) =
  evalState (runExceptT protocol) bitarray
  where
  protocol = case leader of
    ProtoAeha _     -> aehaProtocol
    ProtoNec _      -> necProtocol
    ProtoSirc _     -> sircProtocol
    ProtoUnknown _  -> unknownProtocol

-- |
type ProtocolDecoder a = ExceptT ProcessError (State (Array Bit)) a

-- |
takeBit :: ProcessError -> ProtocolDecoder Bit
takeBit errmsg = do
  state <- State.get
  case Array.uncons state of
    Nothing ->
      throwError errmsg
    Just x -> do
      State.put x.tail
      pure x.head

-- |
takeBits :: Int -> ProcessError -> ProtocolDecoder (NonEmptyArray Bit)
takeBits n errmsg = do
  state <- State.get
  let bitarray  = Array.take n state
      nextState = Array.drop n state
  State.put nextState
  maybe (throwError errmsg) pure do
    guard (Array.length bitarray == n)
    NEA.fromArray bitarray

-- |
takeEnd :: ProcessError -> ProtocolDecoder (NonEmptyArray Bit)
takeEnd errmsg = do
  array <- State.get
  State.put []
  maybe (throwError errmsg) pure $ NEA.fromArray array

-- |
toNonEmptyArray2D :: forall a. Int -> Array a -> Array (NonEmptyArray a)
toNonEmptyArray2D n =
  Array.mapMaybe NEA.fromArray <<< toArray2D n

-- |
aehaProtocol :: ProtocolDecoder InfraredCodes
aehaProtocol = do
  lo <- takeBits 8 "fail to read: custom code lower (AEHA)"
  hi <- takeBits 8 "fail to read: custom code higher (AEHA)"
  p_ <- takeBits 4 "fail to read: parity (AEHA)"
  d0 <- takeBits 4 "fail to read: data0 (AEHA)"
  d_ <- takeEnd "fail to read: data (AEHA)"
  let init = NEA.init d_
      last = NEA.last d_
      octets = toNonEmptyArray2D 8 init
  pure $ AEHA { customLo: LsbFirst lo
              , customHi: LsbFirst hi
              , parity: LsbFirst p_
              , data0: LsbFirst d0
              , data: map LsbFirst octets
              , stop: last
              }


-- |
necProtocol :: ProtocolDecoder InfraredCodes
necProtocol = do
  lo <- takeBits 8 "fail to read: custom code lower (NEC)"
  hi <- takeBits 8 "fail to read: custom code higher (NEC)"
  d0 <- takeBits 8 "fail to read: data (NEC)"
  d1 <- takeBits 8 "fail to read: inv-data (NEC)"
  sb <- takeBit "fail to read: stop bit (NEC)"
  pure $ NEC  { customLo: LsbFirst lo
              , customHi: LsbFirst hi
              , data: LsbFirst d0
              , invData: LsbFirst d1
              , stop: sb
              }

-- |
sircProtocol :: ProtocolDecoder InfraredCodes
sircProtocol = do
  com <- takeBits 7 "fail to read: command code (SIRC)"
  add <- takeEnd "fail to read: address (SIRC)"
  pure $ SIRC { command: LsbFirst com
              , address: LsbFirst add
              }

-- |
unknownProtocol :: ProtocolDecoder InfraredCodes
unknownProtocol = do
  array <- State.get
  State.put []
  pure $ Unknown array

