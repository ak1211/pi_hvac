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
  , Celsius(..)
  , Swing(..)
  , Switch(..)
  , Mode(..)
  , Fan(..)
  , Crc(..)
  , Profile(..)
  , InfraredCodeFrame(..)
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
  , decodePhase4
  , fromBoolean
  , fromMilliseconds
  , infraredHexStringParser
  , validCrc
  , showBit
  , toBoolean 
  , toMilliseconds
  , toLsbFirst
  , toMsbFirst
  , toStringLsbFirst
  , toStringLsbFirstWithHex
  , toStringMsbFirst
  , toStringMsbFirstWithHex
  , toIrCodeFrames
  , toIrRemoteControlCode
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
import Data.Bifunctor as Bifunctor
import Data.Either (Either(..))
import Data.Enum (class Enum, class BoundedEnum, toEnum)
import Data.Foldable (all, sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Int.Bits ((.&.), shr)
import Data.Maybe (Maybe(..), isJust, fromJust, fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (fromCharArray, toCharArray)
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

---|
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
fromBinaryString :: String -> Maybe BitStream
fromBinaryString input =
  let xs = map f $ toCharArray input
  in do
    guard $ all isJust xs
    NEA.fromArray $ Array.catMaybes xs
  where
  f :: Char -> Maybe Bit
  f '0' = Just Negate
  f '1' = Just Assert
  f _ = Nothing

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
data InfraredCodeFrame
  = FormatUnknown (Array Bit)
  | FormatAEHA {custom :: NonEmptyArray BitStream, octets :: Array BitStream, stop :: Bit}
  | FormatNEC  {custom0 :: BitStream, custom1 :: BitStream, data0 :: BitStream, data1 :: BitStream, stop :: Bit}
  | FormatSIRC {command :: BitStream, address :: BitStream}
derive instance genericInfraredCodeFrame  :: Generic InfraredCodeFrame _
derive instance eqInfraredCodeFrame       :: Eq InfraredCodeFrame
instance showInfraredCodeFrame            :: Show InfraredCodeFrame where
  show = genericShow

-- |
newtype Celsius = Celsius Int
derive instance newtypeCelsius      :: Newtype Celsius _
derive newtype instance eqCelsius   :: Eq Celsius
derive newtype instance ordCelsius  :: Ord Celsius
derive newtype instance showCelsius :: Show Celsius

-- |
data Mode = MAuto | MFan | MDry | MCool | MHeat
derive instance genericMode :: Generic Mode _
derive instance eqMode      :: Eq Mode
instance showMode           :: Show Mode where
  show = genericShow

-- |
newtype Switch = Switch Boolean
derive instance newtypeSwitch             :: Newtype Switch _
derive newtype instance eqSwitch          :: Eq Switch
derive newtype instance ordSwitch         :: Ord Switch
derive newtype instance enumSwitch        :: Enum Switch
derive newtype instance boundedSwitch     :: Bounded Switch
derive newtype instance boundedEnumSwitch :: BoundedEnum Switch
instance showSwitch :: Show Switch where
  show (Switch true) = "ON"
  show (Switch false) = "OFF"

-- |
data Swing = SAuto | SHorizontal | SNotch2 | SNotch3 | SNotch4 | SNotch5
derive instance genericSwing  :: Generic Swing _
derive instance eqSwing       :: Eq Swing
instance showSwing            :: Show Swing where
  show = genericShow

-- |
data Fan = FAuto | FSlowest | FNotch2 | FNotch3 | FNotch4 | FNotch5
derive instance genericFan  :: Generic Fan _
derive instance eqFan       :: Eq Fan
instance showFan            :: Show Fan where
  show = genericShow

-- |
data Profile = PNormal | PBoost | PQuiet | POther Int
derive instance genericProfile  :: Generic Profile _
derive instance eqProfile       :: Eq Profile
instance showProfile            :: Show Profile where
  show = genericShow

-- |
newtype Crc = Crc Int
derive instance genericCrc      :: Generic Crc _
derive instance newtypeCrc      :: Newtype Crc _
derive newtype instance eqCrc   :: Eq Crc
instance showCrc                :: Show Crc where
  show = genericShow

-- |
validCrc :: Crc -> Array BitStream -> Boolean
validCrc crc body18bytes =
  let
    x = unwrap crc
    y = (sum values) .&. 0xff
  in
  x == y
  where
  values :: Array Int
  values = map (unwrap <<< toLsbFirst) body18bytes

-- |
data IrRemoteControlCode
  = UnknownIrRemote       (Array InfraredCodeFrame)
  | IrRemotePanasonicHvac
    { body18bytes :: Array BitStream
    , temperature :: Celsius
    , mode :: Mode
    , switch :: Switch
    , swing :: Swing
    , fan :: Fan
    , profile :: Profile
    , crc :: Crc
    }

derive instance genericIrRemoteControlCode  :: Generic IrRemoteControlCode _
derive instance eqIrRemoteControlCode       :: Eq IrRemoteControlCode
instance showIrRemoteControlCode            :: Show IrRemoteControlCode where
  show = genericShow

-- |
toIrCodeFrames :: Baseband -> Either ProcessError (Array InfraredCodeFrame)
toIrCodeFrames =
  traverse (decodePhase3 <=< decodePhase2) <<< decodePhase1 

-- |
toIrRemoteControlCode :: Baseband -> Either ProcessError IrRemoteControlCode
toIrRemoteControlCode =
  Bifunctor.rmap decodePhase4 <<< toIrCodeFrames

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
decodePhase3 :: Tuple InfraredLeader (Array Bit) -> Either ProcessError InfraredCodeFrame
decodePhase3 (Tuple leader bitarray) =
  evalState (runExceptT decoder) bitarray
  where

  decoder :: DecodeMonad ProcessError InfraredCodeFrame
  decoder = case leader of
    LeaderAeha _    -> decodeAeha
    LeaderNec _     -> decodeNec
    LeaderSirc _    -> decodeSirc
    LeaderUnknown _ -> decodeUnknown

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
decodeAeha :: DecodeMonad ProcessError InfraredCodeFrame
decodeAeha = do
  custom0 <- takeBits 8 "fail to read: custom code (AEHA)"
  custom1 <- takeBits 8 "fail to read: custom code (AEHA)"
  data_N <- takeEnd "fail to read: data (AEHA)"
  let custom = NEA.singleton custom0 <> NEA.singleton custom1
      init = NEA.init data_N
      last = NEA.last data_N
      octets = toArrayNonEmptyArray 8 init
  pure $ FormatAEHA { custom: custom
                    , octets: octets
                    , stop: last
                    }

-- |
decodeNec :: DecodeMonad ProcessError InfraredCodeFrame
decodeNec = do
  custom0 <- takeBits 8 "fail to read: custom code0 (NEC)"
  custom1 <- takeBits 8 "fail to read: custom code1 (NEC)"
  data0   <- takeBits 8 "fail to read: data0 (NEC)"
  data1   <- takeBits 8 "fail to read: data1 (NEC)"
  stopbt <- takeBit "fail to read: stop bit (NEC)"
  pure $ FormatNEC  { custom0: custom0
                    , custom1: custom1
                    , data0: data0
                    , data1: data1
                    , stop: stopbt
                    }

-- |
decodeSirc :: DecodeMonad ProcessError InfraredCodeFrame
decodeSirc = do
  comm <- takeBits 7 "fail to read: command code (SIRC)"
  addr <- takeEnd "fail to read: address (SIRC)"
  pure $ FormatSIRC { command: comm
                    , address: addr
                    }

-- |
decodeUnknown :: DecodeMonad ProcessError InfraredCodeFrame
decodeUnknown = do
  array <- State.get
  State.put []
  pure $ FormatUnknown array

-- | 各機種の赤外線信号にする
decodePhase4 :: Array InfraredCodeFrame -> IrRemoteControlCode
decodePhase4 irFrames =
  fromMaybe (UnknownIrRemote irFrames) $ do
    ff <- decodePanasonicHVAC irFrames
    pure ff

-- |
-- Panasonic HVAC remote control
--
decodePanasonicHVAC :: Array InfraredCodeFrame -> Maybe IrRemoteControlCode
decodePanasonicHVAC = case _ of
  [ a, b ] | a == firstFrame -> decode b
  _ -> Nothing
  where

  decode :: InfraredCodeFrame -> Maybe IrRemoteControlCode
  decode = case _ of
    FormatAEHA 
      { custom: b1b2
      , octets: [ b_3, b_4, b_5, b_6
                , b_7, b_8, b_9, b10
                , b11, b12, b13, b14
                , b15, b16, b17, b18
                , b19
                ]
      , stop: _
      } ->
        let
          body = [ NEA.toArray b1b2
                  , [ b_3, b_4, b_5, b_6
                    , b_7, b_8, b_9, b10
                    , b11, b12, b13, b14
                    , b15, b16, b17, b18
                    ]
                  ]
          temp  = (unwrap (toLsbFirst b_7) `shr` 1) .&. 0xf
          mode  = (unwrap (toLsbFirst b_6) `shr` 4) .&. 0xf
          switch = unwrap (toLsbFirst b_6) .&. 0x1
          fan   = (unwrap (toLsbFirst b_9) `shr` 4) .&. 0xf
          swing = unwrap (toLsbFirst b_9) .&. 0xf
          prof  = unwrap (toLsbFirst b14)
          crc   = unwrap (toLsbFirst b19)
        in do
        mode_ <- toMode mode
        switch_ <- toEnum switch
        swing_ <- toSwingNotch swing
        fan_ <- toFanNotch fan
        pure $ IrRemotePanasonicHvac
          { body18bytes: Array.concat body
          , temperature: Celsius (16 + temp)
          , mode: mode_
          , switch: switch_
          , swing: swing_
          , fan: fan_
          , profile: toProfile prof
          , crc: Crc crc
          }
    _ ->  Nothing
  
  toMode = case _ of
    0x0 -> Just MAuto
    0x2 -> Just MDry
    0x3 -> Just MCool
    0x4 -> Just MHeat
    0x6 -> Just MFan
    _ -> Nothing

  toSwingNotch = case _ of
    0xf -> Just SAuto
    0x1 -> Just SHorizontal
    0x2 -> Just SNotch2
    0x3 -> Just SNotch3
    0x4 -> Just SNotch4
    0x5 -> Just SNotch5
    _ -> Nothing

  toFanNotch = case _ of
    0xa -> Just FAuto
    0x3 -> Just FSlowest
    0x4 -> Just FNotch2
    0x5 -> Just FNotch3
    0x6 -> Just FNotch4
    0x7 -> Just FNotch5
    _ -> Nothing

  toProfile = case _ of
    0x10 -> PNormal
    0x11 -> PBoost
    0x30 -> PQuiet
    value -> POther value

  -- |
  firstFrame :: InfraredCodeFrame
  firstFrame =
    --
    -- Panasonic HVAC first frame value is
    -- LSB first                                    -- MSB first
    -- 0x02 20 e0 04 00 00 00 06                    -- 0x40 04 07 20 00 00 00 60
    --
    -- first byte "01000000"
    -- LSB first                                    -- MSB first
    -- 1   2   4   8  16  32  64 128                -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   |                --   |   |   |   |   |   |   |   |
    -- 0   1   0   0   0   0   0   0 == 02h         --   0   1   0   0   0   0   0   0 == 40h
    --
    -- second byte "00000100"
    -- LSB first                                    -- MSB first
    -- 1   2   4   8  16  32  64 128                -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   |                --   |   |   |   |   |   |   |   |
    -- 0   0   0   0   0   1   0   0 == 20h         --   0   0   0   0   0   1   0   0 == 04h
    --
    -- 3rd byte "00000111"
    -- LSB first                                    -- MSB first
    -- 1   2   4   8  16  32  64 128                -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   | 32+64+128=224  --   |   |   |   |   |   |   |   | 1+2+4=7
    -- 0   0   0   0   0   1   1   1 == e0h         --   0   0   0   0   0   1   1   1 == 07h
    --
    -- 4th byte "00100000"
    -- LSB first                                    -- MSB first
    -- 1   2   4   8  16  32  64 128                -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   |                --   |   |   |   |   |   |   |   |
    -- 0   0   1   0   0   0   0   0 == 04h         --   0   0   1   0   0   0   0   0 == 20h
    --
    -- 5th byte "00000000"
    -- 6th byte "00000000"
    -- 7th byte "00000000"
    --
    -- 8th byte "01100000"
    -- LSB first                                    -- MSB first
    -- 1   2   4   8  16  32  64 128                -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   | 2+4=6          --   |   |   |   |   |   |   |   | 32+64=96
    -- 0   1   1   0   0   0   0   0 == 06h         --   0   1   1   0   0   0   0   0 == 60h
    --
    FormatAEHA  { custom: unsafePartial $ fromJust $ NEA.fromArray custom
                , octets: octets
                , stop: Assert
                }
    where
    custom :: Array BitStream
    custom = Array.mapMaybe fromBinaryString
              [ "01000000"  -- 02 (LSB first) | 40 (MSB first)
              , "00000100"  -- 20 (LSB first) | 04 (MSB first)
              ]
    octets :: Array BitStream
    octets = Array.mapMaybe fromBinaryString
              [ "00000111"  -- e0 (LSB first) | 07 (MSB first)
              , "00100000"  -- 04 (LSB first) | 20 (MSB first)
              , "00000000"  -- 00 (LSB first) | 00 (MSB first)
              , "00000000"  -- 00 (LSB first) | 00 (MSB first)
              , "00000000"  -- 00 (LSB first) | 00 (MSB first)
              , "01100000"  -- 06 (LSB first) | 60 (MSB first)
              ]
