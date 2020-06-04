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
module InfraredRemoteCode.Devices.HitachiHvac
  ( Fan(..)
  , HitachiHvac(..)
  , Mode(..)
  , Switch(..)
  , decodeHitachiHvac
  ) where

import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Enum (class BoundedEnum, class Enum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericPred, genericSucc)
import Data.Generic.Rep.Show (genericShow)
import Data.Int.Bits ((.&.), shr)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import InfraredRemoteCode.Internal (BitStream, Celsius(..), InfraredCodeFrame(..), unBitOrder, fromBinaryString, toLsbFirst)
import Prelude

-- |
data Mode
  = MCool
  | MDryCool
  | MDehumidify
  | MHeat
  | MAutomatic
  | MAutoDehumidifying
  | MQuickLaundry
  | MCondensationControl

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
  toEnum 0x3 {- 0011 -} = Just MCool
  toEnum 0x4 {- 0100 -} = Just MDryCool
  toEnum 0x5 {- 0101 -} = Just MDehumidify
  toEnum 0x6 {- 0110 -} = Just MHeat
  toEnum 0x7 {- 0111 -} = Just MAutomatic
  toEnum 0x9 {- 1001 -} = Just MAutoDehumidifying
  toEnum 0xa {- 1010 -} = Just MQuickLaundry
  toEnum 0xc {- 1100 -} = Just MCondensationControl
  toEnum _ = Nothing
  fromEnum MCool = 0x3
  fromEnum MDryCool = 0x4
  fromEnum MDehumidify = 0x5
  fromEnum MHeat = 0x6
  fromEnum MAutomatic = 0x7
  fromEnum MAutoDehumidifying = 0x9
  fromEnum MQuickLaundry = 0xa
  fromEnum MCondensationControl = 0xc

instance showMode :: Show Mode where
  show = genericShow

-- |
data Fan
  = FSilent
  | FLow
  | FMed
  | FHigh
  | FAuto

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
  toEnum 0x1 {- 001 -} = Just FSilent
  toEnum 0x2 {- 010 -} = Just FLow
  toEnum 0x3 {- 011 -} = Just FMed
  toEnum 0x4 {- 100 -} = Just FHigh
  toEnum 0x5 {- 101 -} = Just FAuto
  toEnum _ = Nothing
  fromEnum FSilent = 0x1
  fromEnum FLow = 0x2
  fromEnum FMed = 0x3
  fromEnum FHigh = 0x4
  fromEnum FAuto = 0x5

instance showFan :: Show Fan where
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
newtype HitachiHvac
  = HitachiHvac
  { original :: Array BitStream
  , mode :: Mode
  , temperature :: Celsius
  , fan :: Fan
  , switch :: Switch
  }

derive instance newtypeHitachiHvac :: Newtype HitachiHvac _

derive instance genericHitachiHvac :: Generic HitachiHvac _

derive newtype instance eqHitachiHvac :: Eq HitachiHvac

instance showHitachiHvac :: Show HitachiHvac where
  show = genericShow

-- |
-- Hitachi HVAC remote control
--
decodeHitachiHvac :: Array InfraredCodeFrame -> Maybe (NonEmptyArray HitachiHvac)
decodeHitachiHvac frames = NEA.fromArray $ Array.mapMaybe go frames
  where
  go :: InfraredCodeFrame -> Maybe HitachiHvac
  go = case _ of
    FormatAEHA { octets: a, stop: _ } -> decode a
    _ -> Nothing

  decode :: Array BitStream -> Maybe HitachiHvac
  decode bitstreams
    | isValidIdentifier bitstreams = do
      b13 <- (unBitOrder <<< toLsbFirst) <$> bitstreams !! 13
      b25 <- (unBitOrder <<< toLsbFirst) <$> bitstreams !! 25
      b27 <- (unBitOrder <<< toLsbFirst) <$> bitstreams !! 27
      mode :: Mode <- toEnum (b25 .&. 0xf)
      fan :: Fan <- toEnum ((b25 `shr` 4) .&. 0xf)
      sw :: Switch <- toEnum ((b27 `shr` 4) .&. 0x1)
      pure
        $ HitachiHvac
            { original: bitstreams
            , mode: mode
            , temperature: Celsius ((b13 `shr` 2) .&. 0x1f)
            , fan: fan
            , switch: sw
            }
    | otherwise = Nothing

  -- |
  isValidIdentifier :: Array BitStream -> Boolean
  isValidIdentifier input = (Array.take 9 input) == first9bytes

  -- |
  first9bytes :: Array BitStream
  first9bytes =
    --
    -- Hitachi HVAC first 9bytes value is
    -- LSB first                                        -- MSB first
    -- 0x01 10 00 40 bf ff 00 cc 33                     -- 0x80 08 00 02 fd ff 00 33 cc
    --
    -- first byte "10000000"
    -- LSB first                                        -- MSB first
    -- 1   2   4   8  16  32  64 128                    -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   |                    --   |   |   |   |   |   |   |   |
    -- 1   0   0   0   0   0   0   0 == 01h             --   1   0   0   0   0   0   0   0 == 80h
    --
    -- second byte "00001000"
    -- LSB first                                        -- MSB first
    -- 1   2   4   8  16  32  64 128                    -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   |                    --   |   |   |   |   |   |   |   |
    -- 0   0   0   0   1   0   0   0 == 10h             --   0   0   0   0   1   0   0   0 == 08h
    --
    -- 3rd byte "00000000"
    --
    -- 4th byte "00000010"
    -- LSB first                                        -- MSB first
    -- 1   2   4   8  16  32  64 128                    -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   |                    --   |   |   |   |   |   |   |   |
    -- 0   0   0   0   0   0   1   0 == 40h             --   0   0   0   0   0   0   1   0 == 02h
    --
    -- 5th byte "11111101"
    -- LSB first                                        -- MSB first
    -- 1   2   4   8  16  32  64 128  1+2+4+8+16+       -- 128  64  32  16   8   4   2   1  1+4+8+16+
    -- |   |   |   |   |   |   |   |  32+128=191        --   |   |   |   |   |   |   |   |  32+64+128=253
    -- 1   1   1   1   1   1   0   1 == bfh             --   1   1   1   1   1   1   0   1 == fdh
    --
    -- 6th byte "11111111"
    --
    -- 7th byte "00000000"
    --
    -- 8th byte "00110011"
    -- LSB first                                        -- MSB first
    -- 1   2   4   8  16  32  64 128                    -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   |  4+8+64+128=204    --   |   |   |   |   |   |   |   |  1+2+16+32=51
    -- 0   0   1   1   0   0   1   1 == cch             --   0   0   1   1   0   0   1   1 == 33h
    --
    -- 9th byte "11001100"
    -- LSB first                                        -- MSB first
    -- 1   2   4   8  16  32  64 128                    -- 128  64  32  16   8   4   2   1
    -- |   |   |   |   |   |   |   |  1+2+16+32=51      --   |   |   |   |   |   |   |   |  4+8+64+128=204
    -- 1   1   0   0   1   1   0   0 == 33h             --   1   1   0   0   1   1   0   0 == cch
    --
    Array.mapMaybe fromBinaryString
      [ "10000000" -- 01 (LSB first) | 80 (MSB first)
      , "00001000" -- 10 (LSB first) | 08 (MSB first)
      , "00000000" -- 00 (LSB first) | 00 (MSB first)
      , "00000010" -- 40 (LSB first) | 02 (MSB first)
      , "11111101" -- bf (LSB first) | fd (MSB first)
      , "11111111" -- ff (LSB first) | ff (MSB first)
      , "00000000" -- 00 (LSB first) | 00 (MSB first)
      , "00110011" -- cc (LSB first) | 33 (MSB first)
      , "11001100" -- 33 (LSB first) | cc (MSB first)
      ]
