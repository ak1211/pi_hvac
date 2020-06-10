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
module InfraredRemoteCode.Devices.DaikinHvac
  ( Checksum(..)
  , DaikinHvac(..)
  , Fan(..)
  , Mode(..)
  , Swing(..)
  , Switch(..)
  , TimerDulationHour(..)
  , decodeDaikinHvac
  , validChecksum
  ) where

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Enum (class Enum, class BoundedEnum, toEnum)
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericPred, genericSucc)
import Data.Generic.Rep.Show (genericShow)
import Data.Int.Bits ((.&.), (.|.), shr, shl)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import InfraredRemoteCode.Internal (Bit(..), BitStream, BitOrder(..), Celsius(..), InfraredCodeFrame(..), unBitOrder, fromBinaryString, toLsbFirst)
import Prelude

-- |
newtype ComfortMode
  = ComfortMode Boolean

derive instance newtypeComfortMode :: Newtype ComfortMode _

derive newtype instance eqComfortMode :: Eq ComfortMode

derive newtype instance ordComfortMode :: Ord ComfortMode

derive newtype instance enumComfortMode :: Enum ComfortMode

derive newtype instance boundedComfortMode :: Bounded ComfortMode

derive newtype instance boundedEnumComfortMode :: BoundedEnum ComfortMode

instance showComfortMode :: Show ComfortMode where
  show (ComfortMode true) = "ON"
  show (ComfortMode false) = "OFF"

-- |
data Mode
  = MAuto
  | MDry
  | MCool
  | MHeat
  | MFan

derive instance genericMode :: Generic Mode _

derive instance eqMode :: Eq Mode

derive instance ordMode :: Ord Mode

instance enumMode :: Enum Mode where
  succ = genericSucc
  pred = genericPred

instance boundedMode :: Bounded Mode where
  top = genericTop
  bottom = genericBottom

instance boundedEnumMode :: BoundedEnum Mode where
  cardinality = genericCardinality
  toEnum 0 = Just MAuto
  toEnum 2 = Just MDry
  toEnum 3 = Just MCool
  toEnum 4 = Just MHeat
  toEnum 6 = Just MFan
  toEnum _ = Nothing
  --
  fromEnum MAuto = 0
  fromEnum MDry = 2
  fromEnum MCool = 3
  fromEnum MHeat = 4
  fromEnum MFan = 6

instance showMode :: Show Mode where
  show = genericShow

-- |
data Switch = SwitchON | SwitchOFF

derive instance genericSwitch :: Generic Switch _

derive instance eqSwitch :: Eq Switch

derive instance ordSwitch :: Ord Switch

instance enumSwitch :: Enum Switch where
  succ = genericSucc
  pred = genericPred

instance boundedSwitch :: Bounded Switch where
  top = genericTop
  bottom = genericBottom

instance boundedEnumSwitch :: BoundedEnum Switch where
  cardinality = genericCardinality
  toEnum 1 = Just SwitchON
  toEnum 0 = Just SwitchOFF
  toEnum _ = Nothing
  --
  fromEnum SwitchON = 1
  fromEnum SwitchOFF = 0


instance showSwitch :: Show Switch where
  show = genericShow

-- |
data Swing = SwingEnabled | SwingDisabled

derive instance genericSwing :: Generic Swing _

derive instance eqSwing :: Eq Swing

derive instance ordSwing :: Ord Swing

instance enumSwing :: Enum Swing where
  succ = genericSucc
  pred = genericPred

instance boundedSwing :: Bounded Swing where
  top = genericTop
  bottom = genericBottom

instance boundedEnumSwing :: BoundedEnum Swing where
  cardinality = genericCardinality
  toEnum 0xf = Just SwingEnabled
  toEnum 0x0 = Just SwingDisabled
  toEnum _ = Nothing
  --
  fromEnum SwingEnabled = 0xf
  fromEnum SwingDisabled = 0x0

instance showSwing :: Show Swing where
  show = genericShow

-- |
data Fan
  = FAuto
  | FSilent
  | FNotch1
  | FNotch2
  | FNotch3
  | FNotch4
  | FNotch5

derive instance genericFan :: Generic Fan _

derive instance eqFan :: Eq Fan

derive instance ordFan :: Ord Fan

instance enumFan :: Enum Fan where
  succ = genericSucc
  pred = genericPred

instance boundedFan :: Bounded Fan where
  top = genericTop
  bottom = genericBottom

instance boundedEnumFan :: BoundedEnum Fan where
  cardinality = genericCardinality
  toEnum 0xa = Just FAuto
  toEnum 0xb = Just FSilent
  toEnum 0x3 = Just FNotch1
  toEnum 0x4 = Just FNotch2
  toEnum 0x5 = Just FNotch3
  toEnum 0x6 = Just FNotch4
  toEnum 0x7 = Just FNotch5
  toEnum _ = Nothing
  --
  fromEnum FAuto = 0xa
  fromEnum FSilent = 0xb
  fromEnum FNotch1 = 0x3
  fromEnum FNotch2 = 0x4
  fromEnum FNotch3 = 0x5
  fromEnum FNotch4 = 0x6
  fromEnum FNotch5 = 0x7

instance showFan :: Show Fan where
  show = genericShow

-- |
newtype TimerDulationHour
  = TimerDulationHour Int

derive instance genericTimerDulationHour :: Generic TimerDulationHour _

derive instance newtypeTimerDulationHour :: Newtype TimerDulationHour _

derive newtype instance eqTimerDulationHour :: Eq TimerDulationHour

derive newtype instance ordTimerDulationHour :: Ord TimerDulationHour

instance showTimerDulationHour :: Show TimerDulationHour where
  show = genericShow

-- |
newtype Checksum
  = Checksum Int

derive instance genericChecksum :: Generic Checksum _

derive instance newtypeChecksum :: Newtype Checksum _

derive newtype instance eqChecksum :: Eq Checksum

instance showChecksum :: Show Checksum where
  show = genericShow

-- |
validChecksum :: Checksum -> Array BitStream -> Boolean
validChecksum checksum original =
  let
    x = unwrap checksum

    y = (sum values) .&. 0xff
  in
    x == y
  where
  values :: Array Int
  values = map (unBitOrder <<< toLsbFirst) $ Array.take 17 original

-- |
newtype DaikinHvac
  = DaikinHvac
  { original :: Array BitStream
  , temperature :: Celsius
  , mode :: Mode
  , onTimer :: Boolean
  , onTimerDulationHour :: TimerDulationHour
  , offTimer :: Boolean
  , offTimerDulationHour :: TimerDulationHour
  , switch :: Switch
  , fan :: Fan
  , swing :: Swing
  , checksum :: Checksum
  }

derive instance newtypeDaikinHvac :: Newtype DaikinHvac _

derive instance genericDaikinHvac :: Generic DaikinHvac _

derive newtype instance eqDaikinHvac :: Eq DaikinHvac

instance showDaikinHvac :: Show DaikinHvac where
  show = genericShow

-- |
-- Daikin HVAC remote control
--
decodeDaikinHvac :: Array InfraredCodeFrame -> Maybe (NonEmptyArray DaikinHvac)
decodeDaikinHvac frames =
  -- 先頭に意味のないフレームがついている場合があるので,
  -- デコード失敗後に先頭フレームを取り除いて再度試行する
  go frames <|> go (Array.drop 1 frames)
  where
  go :: Array InfraredCodeFrame -> Maybe (NonEmptyArray DaikinHvac)
  go xs = do
    first <- xs !! 0
    second <- xs !! 1
    third <- xs !! 2
    comfortMode <- takeComfortMode first
    guard $ second == secondFrame
    case third of
      FormatAEHA { octets: bitstreams, stop: _ } -> NEA.singleton <$> decode bitstreams
      _ -> Nothing

  decode :: Array BitStream -> Maybe DaikinHvac
  decode bitstreams = do
    b_0 <- toLsbFirst <$> bitstreams !! 0
    b_1 <- toLsbFirst <$> bitstreams !! 1
    b_2 <- toLsbFirst <$> bitstreams !! 2
    b_3 <- toLsbFirst <$> bitstreams !! 3
    b_4 <- toLsbFirst <$> bitstreams !! 4
    b_5 <- (unBitOrder <<< toLsbFirst) <$> bitstreams !! 5
    b_6 <- (unBitOrder <<< toLsbFirst) <$> bitstreams !! 6
    b_8 <- (unBitOrder <<< toLsbFirst) <$> bitstreams !! 8
    b10 <- (unBitOrder <<< toLsbFirst) <$> bitstreams !! 10
    b11 <- (unBitOrder <<< toLsbFirst) <$> bitstreams !! 11
    b12 <- (unBitOrder <<< toLsbFirst) <$> bitstreams !! 12
    b18 <- (unBitOrder <<< toLsbFirst) <$> bitstreams !! 18
    guard $ b_0 == LsbFirst 0x11
    guard $ b_1 == LsbFirst 0xda
    guard $ b_2 == LsbFirst 0x27
    guard $ b_3 == LsbFirst 0x00
    guard $ b_4 == LsbFirst 0x00
    mode_ <- toEnum $ (b_5 `shr` 4) .&. 0xf
    offTimer <- toEnum $ (b_5 `shr` 2) .&. 0x1
    onTimer <- toEnum $ (b_5 `shr` 1) .&. 0x1
    power <- toEnum $ (b_5 `shr` 0) .&. 0x1
    fan <- toEnum $ (b_8 `shr` 4) .&. 0xf
    swing <- toEnum $ b_8 .&. 0xf
    let onDulation = TimerDulationHour $ ((b11 .&. 0xf) `shl` 8 .|. b10) / 60
        offDulation = TimerDulationHour $ ((b12 `shl` 4) .|. (b11 `shr` 4)) / 60
    pure
      $ DaikinHvac
          { original: bitstreams
          , mode: mode_
          , temperature: Celsius (b_6 / 2)
          , onTimer: onTimer
          , onTimerDulationHour: onDulation
          , offTimer: offTimer
          , offTimerDulationHour: offDulation
          , switch: power
          , fan: fan
          , swing: swing
          , checksum: Checksum b18
          }

  toMode1 = case _ of
    0x4 -> Just MAuto
    0x2 -> Just MDry
    0x3 -> Just MCool
    0x1 -> Just MHeat
    _ -> Nothing

  -- |
  takeComfortMode :: InfraredCodeFrame -> Maybe ComfortMode
  takeComfortMode = case _ of
    FormatAEHA { octets: xs, stop: _ }
      | xs == comfortModeEnabled -> Just $ ComfortMode true
      | xs == comfortModeDisabled -> Just $ ComfortMode false
    _ -> Nothing
    where
    comfortModeEnabled =
      Array.mapMaybe fromBinaryString
        [ "10001000" -- 11 (LSB first)
        , "01011011" -- da (LSB first)
        , "11100100" -- 27 (LSB first)
        , "00000000" -- 00 (LSB first)
        , "10100011" -- c5 (LSB first)
        , "00000000" -- 00 (LSB first)
        , "10000000" -- 10 (LSB first)
        , "11100111" -- e7 (LSB first)
        ]

    comfortModeDisabled =
      Array.mapMaybe fromBinaryString
        [ "10001000" -- 11 (LSB first)
        , "01011011" -- da (LSB first)
        , "11100100" -- 27 (LSB first)
        , "00000000" -- 00 (LSB first)
        , "10100011" -- c5 (LSB first)
        , "00000000" -- 00 (LSB first)
        , "00000000" -- 00 (LSB first)
        , "11101011" -- d7 (LSB first)
        ]

  -- |
  secondFrame :: InfraredCodeFrame
  secondFrame = FormatAEHA { octets: octets, stop: Assert }
    where
    octets :: Array BitStream
    octets =
      Array.mapMaybe fromBinaryString
        [ "10001000" -- 11 (LSB first) | 88 (MSB first)
        , "01011011" -- da (LSB first) | 5b (MSB first)
        , "11100100" -- 27 (LSB first) | e4 (MSB first)
        , "00000000" -- 00 (LSB first) | 00 (MSB first)
        , "01000010" -- 42 (LSB first) | 42 (MSB first)
        , "00000000" -- 00 (LSB first) | 00 (MSB first)
        , "00000000" -- 00 (LSB first) | 00 (MSB first)
        , "00101010" -- 54 (LSB first) | 2a (MSB first)
        ]
