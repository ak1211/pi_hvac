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
  , BitStream
  , Count(..)
  , InfraredCodeFormat(..)
  , InfraredHexString
  , InfraredLeader(..)
  , IrRemoteControlCode(..)
  , LsbFirst(..)
  , MsbFirst(..)
  , ProcessError
  , Pulse
  , decodePhase1
  , decodePhase2
  , decodePhase3
  , fromBoolean
  , fromMilliseconds
  , infraredHexStringParser
  , showBit
  , toBoolean 
  , toMilliseconds
  , toLsbFirst
  , toMsbFirst
  , toStringLsbFirst
  , toStringLsbFirstWithHex
  , toStringMsbFirst
  , toStringMsbFirstWithHex
  , toIrCodeFormats
  , toIrRemoteControlCode
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState)
import Control.Monad.State as State
import Control.MonadZero (guard)
import Data.Newtype (class Newtype, unwrap)
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr1)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.String (eof, skipSpaces)
import Text.Parsing.Parser.Token (hexDigit)
import Utils (toArrayNonEmptyArray)

-- | input infrared hexadecimal code
type InfraredHexString = String

-- |
type ProcessError = String

-- |
type Pulse = {on :: Count, off :: Count}

-- |
newtype Baseband = Baseband (Array Pulse)
derive newtype instance eqBaseband    :: Eq Baseband
derive newtype instance showBaseband  :: Show Baseband

-- |
infraredHexStringParser:: Parser InfraredHexString Baseband
infraredHexStringParser = do
    arr <- Array.some (pulse <* skipSpaces)
    eof
    pure (Baseband arr)
  where

  pulse = do
    -- 入力値はon -> offの順番
    ton <- valueOf32Bit <?> "on-counts"
    toff <- valueOf32Bit <?> "off-counts"
    pure {on: Count ton, off: Count toff}

  valueOf32Bit = do
    -- 入力値はLower -> Higherの順番
    lower <- hexd16bit <?> "lower-pair hex digit"
    higher<- hexd16bit <?> "higher-pair hex digit"
    -- ここは普通の数字の書き方(位取り記数法: 高位が前, 下位が後)
    let str = higher <> lower
        maybeNum = Int.fromStringAs Int.hexadecimal str
    -- 入力値は検査済みなのでfromJustでよい
    pure (unsafePartial $ fromJust maybeNum)

  hexd16bit = do
    a <- hexDigit
    b <- hexDigit
    pure $ fromCharArray [ a, b ]

-- | count is based on 38khz carrier
newtype Count = Count Int
derive newtype instance eqCount               :: Eq Count
derive newtype instance ordCount              :: Ord Count
derive newtype instance showCount             :: Show Count
derive newtype instance semiringCount         :: Semiring Count
derive newtype instance ringCount             :: Ring Count
derive newtype instance commutativeRingCount  :: CommutativeRing Count
derive newtype instance eucideanRingCount     :: EuclideanRing Count

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

--|
fromBoolean :: Boolean -> Bit
fromBoolean false = Negate
fromBoolean true = Assert

--|
toBoolean :: Bit -> Boolean
toBoolean Negate = false
toBoolean Assert = true

-- |
type BitStream = NonEmptyArray Bit

-- |
data InfraredLeader
  = LeaderAeha Pulse 
  | LeaderNec Pulse 
  | LeaderSirc Pulse 
  | LeaderUnknown Pulse 
derive instance genericInfraredLeader :: Generic InfraredLeader _
derive instance eqInfraredLeader      :: Eq InfraredLeader
instance showInfraredLeader           :: Show InfraredLeader where
  show = genericShow

-- | InfraredLeader data constructor
makeInfraredLeader :: Pulse -> InfraredLeader 
makeInfraredLeader = case _ of
  p | aeha p    -> LeaderAeha p
    | nec p     -> LeaderNec p
    | sirc p    -> LeaderSirc p
    | otherwise -> LeaderUnknown p
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
demodulate :: InfraredLeader -> Array Pulse -> Array Bit
demodulate leader ps =
  case leader of
    LeaderSirc _ ->
      map sircModulation ps
    _ ->
      map pulseDistanceModulation ps
  where

  -- | pulse distance modulation is NEC, AEHA
  pulseDistanceModulation :: Pulse -> Bit
  pulseDistanceModulation = case _ of
    x | (Count 2 * x.on) <= x.off -> Assert
      | otherwise                 -> Negate

  -- | pulse width modulation is SIRC
  sircModulation :: Pulse -> Bit
  sircModulation p =
    let threshold = withTolerance
                      {upper: Milliseconds 0.1, lower: Milliseconds 0.1}
                      (Milliseconds 1.2)
    in
    fromBoolean $ Array.any (_ == p.on) threshold

-- |
newtype LsbFirst = LsbFirst Int
derive instance newtypeLsbFirst         :: Newtype LsbFirst _
derive newtype instance eqLsbFirst      :: Eq LsbFirst
derive newtype instance ordLsbFirst     :: Ord LsbFirst
instance showLsbFirst :: Show LsbFirst where
  show (LsbFirst x) =
    "0x" <> Int.toStringAs Int.hexadecimal x <> "(LSBFirst)"

-- |
toStringLsbFirst :: LsbFirst -> String
toStringLsbFirst = Int.toStringAs Int.decimal <<< unwrap

-- |
toStringLsbFirstWithHex :: LsbFirst -> String
toStringLsbFirstWithHex = Int.toStringAs Int.hexadecimal <<< unwrap

-- |
toLsbFirst :: BitStream -> LsbFirst
toLsbFirst bits =
  LsbFirst (Array.foldl f 0 $ NEA.reverse bits)
  where

  f :: Int -> Bit -> Int
  f acc Assert = acc * 2 + 1
  f acc Negate = acc * 2 + 0

-- |
newtype MsbFirst = MsbFirst Int
derive instance newtypeMsbFirst     :: Newtype MsbFirst _
derive newtype instance eqMsbFirst  :: Eq MsbFirst
derive newtype instance ordMsbFirst :: Ord MsbFirst
instance showMsbFirst :: Show MsbFirst where
  show (MsbFirst v) =
    "0x" <> Int.toStringAs Int.hexadecimal v <> "(MSBFirst)"

-- |
toStringMsbFirst :: MsbFirst -> String
toStringMsbFirst = Int.toStringAs Int.decimal <<< unwrap

-- |
toStringMsbFirstWithHex :: MsbFirst -> String
toStringMsbFirstWithHex = Int.toStringAs Int.hexadecimal <<< unwrap

-- |
toMsbFirst :: BitStream -> MsbFirst
toMsbFirst bits =
  MsbFirst (Array.foldl f 0 bits)
  where

  f :: Int -> Bit -> Int
  f acc Assert = acc * 2 + 1
  f acc Negate = acc * 2 + 0

-- |
data InfraredCodeFormat
  = FormatUnknown (Array Bit)
  | FormatAEHA {custom :: BitStream, octets :: Array BitStream, stop :: Bit}
  | FormatNEC  {custom :: BitStream, data :: BitStream, invData :: BitStream, stop :: Bit}
  | FormatSIRC {command :: BitStream, address :: BitStream}
derive instance genericInfraredCodeFormat :: Generic InfraredCodeFormat _
derive instance eqInfraredCodeFormat      :: Eq InfraredCodeFormat
instance showInfraredCodeFormat           :: Show InfraredCodeFormat where
  show = genericShow

-- |
data IrRemoteControlCode
  = UnknownIrRemote       (Array InfraredCodeFormat)
  | IrRemotePanasonicHvac (Array InfraredCodeFormat)

-- |
toIrCodeFormats :: Baseband -> Either ProcessError (Array InfraredCodeFormat)
toIrCodeFormats =
    traverse (decodePhase3 <=< decodePhase2) <<< decodePhase1 

-- |
toIrRemoteControlCode :: Array InfraredCodeFormat -> IrRemoteControlCode
toIrRemoteControlCode formats =
  UnknownIrRemote formats

-- | 入力を各フレームに分ける
decodePhase1 :: Baseband -> Array (Array Pulse)
decodePhase1 (Baseband bb) =
  unfoldr1 chop bb
  where

  chop :: Array Pulse -> Tuple (Array Pulse) (Maybe (Array Pulse))
  chop xs = 
    case frames xs of
      { init: a, rest: [] } -> Tuple a Nothing
      { init: a, rest: b_ } -> Tuple a (Just b_)

  frames :: Array Pulse -> {init :: Array Pulse, rest :: Array Pulse}
  frames pulses = 
    let sep = Array.span (\count -> count.off < threshold) pulses
    in
    { init: sep.init <> Array.take 1 sep.rest
    , rest: Array.drop 1 sep.rest
    }

  threshold :: Count
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

-- | 入力リーダ部とビット配列から赤外線信号にする
decodePhase3 :: Tuple InfraredLeader (Array Bit) -> Either ProcessError InfraredCodeFormat
decodePhase3 (Tuple leader bitarray) =
  evalState (runExceptT decoder) bitarray
  where

  decoder :: DecodeMonad ProcessError InfraredCodeFormat
  decoder = case leader of
    LeaderAeha _    -> decodeAeha
    LeaderNec _     -> decodeNec
    LeaderSirc _    -> decodeSirc
    LeaderUnknown _ -> decodeUnknown

-- | 各機種の赤外線信号にする
decodePhase4 :: Array InfraredCodeFormat -> IrRemoteControlCode
decodePhase4 irFrames =
  UnknownIrRemote irFrames
--  | IrRemotePanasonicHvac (Array InfraredCodeFormat)

-- |
type DecodeMonad e a = ExceptT e (State (Array Bit)) a

-- |
takeBit :: ProcessError -> DecodeMonad ProcessError Bit
takeBit errmsg = do
  state <- State.get
  case Array.uncons state of
    Nothing ->
      throwError errmsg

    Just x -> do
      State.put x.tail
      pure x.head

-- |
takeBits :: Int -> ProcessError -> DecodeMonad ProcessError (NonEmptyArray Bit)
takeBits n errmsg = do
  state <- State.get
  let bitarray  = Array.take n state
      nextState = Array.drop n state
  State.put nextState
  maybe (throwError errmsg) pure do
    guard (Array.length bitarray == n)
    NEA.fromArray bitarray

-- |
takeEnd :: ProcessError -> DecodeMonad ProcessError (NonEmptyArray Bit)
takeEnd errmsg = do
  array <- State.get
  State.put []
  maybe (throwError errmsg) pure $ NEA.fromArray array

-- |
decodeAeha :: DecodeMonad ProcessError InfraredCodeFormat
decodeAeha = do
  custom <- takeBits 16 "fail to read: custom code (AEHA)"
  data_N <- takeEnd "fail to read: data (AEHA)"
  let init = NEA.init data_N
      last = NEA.last data_N
      octets = toArrayNonEmptyArray 8 init
  pure $ FormatAEHA { custom: custom
                    , octets: octets
                    , stop: last
                    }

-- |
decodeNec :: DecodeMonad ProcessError InfraredCodeFormat
decodeNec = do
  custom <- takeBits 16 "fail to read: custom code (NEC)"
  data__ <- takeBits 8 "fail to read: data (NEC)"
  i_data <- takeBits 8 "fail to read: inv-data (NEC)"
  stopbt <- takeBit "fail to read: stop bit (NEC)"
  pure $ FormatNEC  { custom: custom
                    , data: data__
                    , invData: i_data
                    , stop: stopbt
                    }

-- |
decodeSirc :: DecodeMonad ProcessError InfraredCodeFormat
decodeSirc = do
  comm <- takeBits 7 "fail to read: command code (SIRC)"
  addr <- takeEnd "fail to read: address (SIRC)"
  pure $ FormatSIRC { command: comm
                    , address: addr
                    }

-- |
decodeUnknown :: DecodeMonad ProcessError InfraredCodeFormat
decodeUnknown = do
  array <- State.get
  State.put []
  pure $ FormatUnknown array
