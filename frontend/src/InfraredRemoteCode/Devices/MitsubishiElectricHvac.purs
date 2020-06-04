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
module InfraredRemoteCode.Devices.MitsubishiElectricHvac
  ( Crc(..)
  , Fan(..)
  , MitsubishiElectricHvac(..)
  , Mode(..)
  , Profile(..)
  , Swing(..)
  , Switch(..)
  , decodeMitsubishiElectricHvac
  , validCrc
  ) where

import Control.MonadZero (guard)
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
import InfraredRemoteCode.Internal (BitStream, Celsius(..), InfraredCodeFrame(..), unBitOrder, fromBinaryString, toLsbFirst)
import Prelude

-- |
data Mode
  = MAuto
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
  values = map (unBitOrder <<< toLsbFirst) $ Array.take 17 original

-- |
newtype MitsubishiElectricHvac
  = MitsubishiElectricHvac
  { original :: Array BitStream
  , temperature :: Celsius
  , mode1 :: Mode
  , switch :: Switch
  , crc :: Crc
  }

derive instance newtypeMitsubishiElectricHvac :: Newtype MitsubishiElectricHvac _

derive instance genericMitsubishiElectricHvac :: Generic MitsubishiElectricHvac _

derive newtype instance eqMitsubishiElectricHvac :: Eq MitsubishiElectricHvac

instance showMitsubishiElectricHvac :: Show MitsubishiElectricHvac where
  show = genericShow

-- |
-- Mitsubishi Electric HVAC remote control
--
decodeMitsubishiElectricHvac :: Array InfraredCodeFrame -> Maybe (NonEmptyArray MitsubishiElectricHvac)
decodeMitsubishiElectricHvac = case _ of
  [] -> Nothing
  xs -> NEA.fromArray $ Array.mapMaybe decode xs
  where
  decode :: InfraredCodeFrame -> Maybe MitsubishiElectricHvac
  decode = case _ of
    FormatAEHA
      { octets:
        original@[ b_0
        , b_1
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
        ]
    , stop: _
    } ->
      let
        temp = unBitOrder (toLsbFirst b_7) .&. 0xf

        mode1 = (unBitOrder (toLsbFirst b_6) `shr` 3) .&. 0x7

        switch = (unBitOrder (toLsbFirst b_5) `shr` 5) .&. 0x1

        crc = unBitOrder (toLsbFirst b17)
      in
        do
          guard $ isValidIdentifier original
          mode1_ <- toMode1 mode1
          switch_ <- toEnum switch
          pure
            $ MitsubishiElectricHvac
                { original: original
                , temperature: Celsius (16 + temp)
                , mode1: mode1_
                , switch: switch_
                , crc: Crc crc
                }
    _ -> Nothing

  toMode1 = case _ of
    0x4 -> Just MAuto
    0x2 -> Just MDry
    0x3 -> Just MCool
    0x1 -> Just MHeat
    _ -> Nothing

  -- |
  isValidIdentifier :: Array BitStream -> Boolean
  isValidIdentifier input = (Array.take 5 input) == first5bytes

  -- |
  first5bytes :: Array BitStream
  first5bytes =
    --
    -- Mitsubishi Electric HVAC first 5bytes value is
    -- LSB first                                        -- MSB first
    -- 0x23 cb 26 01 00                                 -- 0xc4 d3 64 80 00
    --
    -- first byte "11000100"
    -- LSB first                                        -- MSB first
    -- 1   2   4   8  16  32  64 128                    -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   | 1+2+32=35          --   |   |   |   |   |   |   |   | 4+64+128=196
    -- 1   1   0   0   0   1   0   0 == 23h             --   1   1   0   0   0   1   0   0 == c4h
    --
    -- second byte "11010011"
    -- LSB first                                        -- MSB first
    -- 1   2   4   8  16  32  64 128                    -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   | 1+2+8+64+128=203   --   |   |   |   |   |   |   |   | 1+2+16+64+128=211
    -- 1   1   0   1   0   0   1   1 == cbh             --   1   1   0   1   0   0   1   1 == d3h
    --
    -- 3rd byte "01100100"
    -- LSB first                                        -- MSB first
    -- 1   2   4   8  16  32  64 128                    -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   | 2+4+32=38          --   |   |   |   |   |   |   |   | 4+32+64=100
    -- 0   1   1   0   0   1   0   0 == 26h             --   0   1   1   0   0   1   0   0 == 64h
    --
    -- 4th byte "10000000"
    -- LSB first                                        -- MSB first
    -- 1   2   4   8  16  32  64 128                    -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   |                    --   |   |   |   |   |   |   |   |
    -- 1   0   0   0   0   0   0   0 == 01h             --   1   0   0   0   0   0   0   0 == 80h
    --
    -- 5th byte "00000000"
    --
    Array.mapMaybe fromBinaryString
      [ "11000100" -- 23 (LSB first) | c4 (MSB first)
      , "11010011" -- cb (LSB first) | d3 (MSB first)
      , "01100100" -- 26 (LSB first) | 64 (MSB first)
      , "10000000" -- 01 (LSB first) | 80 (MSB first)
      , "00000000" -- 00 (LSB first) | 00 (MSB first)
      ]
