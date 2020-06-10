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
module InfraredRemoteCode.Devices.PanasonicHvac
  ( Crc(..)
  , Fan(..)
  , Mode(..)
  , PanasonicHvac(..)
  , Profile(..)
  , Swing(..)
  , Switch(..)
  , decodePanasonicHvac
  , validCrc
  ) where

import Control.MonadZero (guard)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Enum (class Enum, class BoundedEnum, toEnum)
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int.Bits ((.&.), shr)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import InfraredRemoteCode.Internal (Bit(..), BitStream, Celsius(..), InfraredCodeFrame(..), unBitOrder, fromBinaryString, toLsbFirst)
import Prelude

-- |
data Mode
  = MAuto
  | MFan
  | MDry
  | MCool
  | MHeat

derive instance genericMode :: Generic Mode _

derive instance eqMode :: Eq Mode

instance showMode :: Show Mode where
  show = genericShow

-- |
newtype Switch
  = Switch Boolean

derive instance newtypeSwitch :: Newtype Switch _

derive newtype instance eqSwitch :: Eq Switch

derive newtype instance ordSwitch :: Ord Switch

derive newtype instance enumSwitch :: Enum Switch

derive newtype instance boundedSwitch :: Bounded Switch

derive newtype instance boundedEnumSwitch :: BoundedEnum Switch

instance showSwitch :: Show Switch where
  show (Switch true) = "ON"
  show (Switch false) = "OFF"

-- |
data Swing
  = SAuto
  | SHorizontal
  | SNotch2
  | SNotch3
  | SNotch4
  | SNotch5

derive instance genericSwing :: Generic Swing _

derive instance eqSwing :: Eq Swing

instance showSwing :: Show Swing where
  show = genericShow

-- |
data Fan
  = FAuto
  | FSlowest
  | FNotch2
  | FNotch3
  | FNotch4
  | FNotch5

derive instance genericFan :: Generic Fan _

derive instance eqFan :: Eq Fan

instance showFan :: Show Fan where
  show = genericShow

-- |
data Profile
  = PNormal
  | PBoost
  | PQuiet
  | POther Int

derive instance genericProfile :: Generic Profile _

derive instance eqProfile :: Eq Profile

instance showProfile :: Show Profile where
  show = genericShow

-- |
newtype Crc
  = Crc Int

derive instance genericCrc :: Generic Crc _

derive instance newtypeCrc :: Newtype Crc _

derive newtype instance eqCrc :: Eq Crc

instance showCrc :: Show Crc where
  show = genericShow

-- |
validCrc :: Crc -> Array BitStream -> Boolean
validCrc crc original =
  let
    x = unwrap crc

    y = (sum values) .&. 0xff
  in
    x == y
  where
  values :: Array Int
  values = map (unBitOrder <<< toLsbFirst) $ Array.take 18 original

-- |
newtype PanasonicHvac
  = PanasonicHvac
  { original :: Array BitStream
  , temperature :: Celsius
  , mode :: Mode
  , switch :: Switch
  , swing :: Swing
  , fan :: Fan
  , profile :: Profile
  , crc :: Crc
  }

derive instance newtypePanasonicHvac :: Newtype PanasonicHvac _

derive instance genericPanasonicHvac :: Generic PanasonicHvac _

derive newtype instance eqPanasonicHvac :: Eq PanasonicHvac

instance showPanasonicHvac :: Show PanasonicHvac where
  show = genericShow

-- |
-- Panasonic HVAC remote control
--
decodePanasonicHvac :: Array InfraredCodeFrame -> Maybe (NonEmptyArray PanasonicHvac)
decodePanasonicHvac frames = do
  a <- frames !! 0
  b <- frames !! 1
  guard $ a == firstFrame
  result <- decode b
  pure $ NEA.singleton result
  where
  decode :: InfraredCodeFrame -> Maybe PanasonicHvac
  decode = case _ of
    FormatAEHA
      { octets:
        original@[ b_1
        , b_2
        , b_3
        , b_4
        , b_5
        , b_6
        , b_7
        , b_8
        , b_9
        , b10
        , b11
        , b12
        , b13
        , b14
        , b15
        , b16
        , b17
        , b18
        , b19
        ]
    , stop: _
    } ->
      let
        temp = (unBitOrder (toLsbFirst b_7) `shr` 1) .&. 0xf

        mode = (unBitOrder (toLsbFirst b_6) `shr` 4) .&. 0xf

        switch = unBitOrder (toLsbFirst b_6) .&. 0x1

        fan = (unBitOrder (toLsbFirst b_9) `shr` 4) .&. 0xf

        swing = unBitOrder (toLsbFirst b_9) .&. 0xf

        prof = unBitOrder (toLsbFirst b14)

        crc = unBitOrder (toLsbFirst b19)
      in
        do
          mode_ <- toMode mode
          switch_ <- toEnum switch
          swing_ <- toSwingNotch swing
          fan_ <- toFanNotch fan
          pure
            $ PanasonicHvac
                { original: original
                , temperature: Celsius (16 + temp)
                , mode: mode_
                , switch: switch_
                , swing: swing_
                , fan: fan_
                , profile: toProfile prof
                , crc: Crc crc
                }
    _ -> Nothing

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
  firstFrame = FormatAEHA { octets: octets, stop: Assert }
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
    where
    octets :: Array BitStream
    octets =
      Array.mapMaybe fromBinaryString
        [ "01000000" -- 02 (LSB first) | 40 (MSB first)
        , "00000100" -- 20 (LSB first) | 04 (MSB first)
        , "00000111" -- e0 (LSB first) | 07 (MSB first)
        , "00100000" -- 04 (LSB first) | 20 (MSB first)
        , "00000000" -- 00 (LSB first) | 00 (MSB first)
        , "00000000" -- 00 (LSB first) | 00 (MSB first)
        , "00000000" -- 00 (LSB first) | 00 (MSB first)
        , "01100000" -- 06 (LSB first) | 60 (MSB first)
        ]
