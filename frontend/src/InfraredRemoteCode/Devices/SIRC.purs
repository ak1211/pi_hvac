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
module InfraredRemoteCode.Devices.SIRC
  ( Device(..)
  , Command(..)
  , SIRC(..)
  , decodeSirc
  ) where

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Enum (class BoundedEnum, class Enum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericPred, genericSucc)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import InfraredRemoteCode.Internal (BitStream, InfraredCodeFrame(..), toLsbFirst, unBitOrder)
import Prelude

-- |
data Device
  = DTV
  | DVCR1
  | DVCR2
  | DLaserDisk
  | DSurroundSound
  | DCassetteDeckTuner
  | DCDPlayer
  | DEqualizer

derive instance genericDevice :: Generic Device _

derive instance eqDevice :: Eq Device

derive instance ordDevice :: Ord Device

instance enumDevice :: Enum Device where
  succ = genericSucc
  pred = genericPred

instance boundedDevice :: Bounded Device where
  top = genericTop
  bottom = genericBottom

instance boundedEnumDevice :: BoundedEnum Device where
  cardinality = genericCardinality
  toEnum 1 = Just DTV
  toEnum 2 = Just DVCR1
  toEnum 3 = Just DVCR2
  toEnum 6 = Just DLaserDisk
  toEnum 12 = Just DSurroundSound
  toEnum 16 = Just DCassetteDeckTuner
  toEnum 17 = Just DCDPlayer
  toEnum 18 = Just DEqualizer
  toEnum _ = Nothing
  --
  fromEnum DTV = 1
  fromEnum DVCR1 = 2
  fromEnum DVCR2 = 3
  fromEnum DLaserDisk = 6
  fromEnum DSurroundSound = 12
  fromEnum DCassetteDeckTuner = 16
  fromEnum DCDPlayer = 17
  fromEnum DEqualizer = 18

instance showDevice :: Show Device where
  show = genericShow

-- |
data Command
  = CDigitKey1
  | CDigitKey2
  | CDigitKey3
  | CDigitKey4
  | CDigitKey5
  | CDigitKey6
  | CDigitKey7
  | CDigitKey8
  | CDigitKey9
  | CDigitKey0
  | CChannelPlus
  | CChannelMinus
  | CVolumePlus
  | CVolumeMinus
  | CMute
  | CPower
  | CReset
  | CAudioMode
  | CContrastPlus
  | CContrastMinus
  | CColourPlus
  | CColourMinus
  | CBrightnessPlus
  | CBrightnessMinus
  | CAUXInputSelect
  | CBalanceLeft
  | CBalanceRight
  | CStandby

derive instance genericCommand :: Generic Command _

derive instance eqCommand :: Eq Command

derive instance ordCommand :: Ord Command

instance enumCommand :: Enum Command where
  succ = genericSucc
  pred = genericPred

instance boundedCommand :: Bounded Command where
  top = genericTop
  bottom = genericBottom

instance boundedEnumCommand :: BoundedEnum Command where
  cardinality = genericCardinality
  toEnum 0 = Just CDigitKey1
  toEnum 1 = Just CDigitKey2
  toEnum 2 = Just CDigitKey3
  toEnum 3 = Just CDigitKey4
  toEnum 4 = Just CDigitKey5
  toEnum 5 = Just CDigitKey6
  toEnum 6 = Just CDigitKey7
  toEnum 7 = Just CDigitKey8
  toEnum 8 = Just CDigitKey9
  toEnum 9 = Just CDigitKey0
  toEnum 16 = Just CChannelPlus
  toEnum 17 = Just CChannelMinus
  toEnum 18 = Just CVolumePlus
  toEnum 19 = Just CVolumeMinus
  toEnum 20 = Just CMute
  toEnum 21 = Just CPower
  toEnum 22 = Just CReset
  toEnum 23 = Just CAudioMode
  toEnum 24 = Just CContrastPlus
  toEnum 25 = Just CContrastMinus
  toEnum 26 = Just CColourPlus
  toEnum 27 = Just CColourMinus
  toEnum 30 = Just CBrightnessPlus
  toEnum 31 = Just CBrightnessMinus
  toEnum 37 = Just CAUXInputSelect
  toEnum 38 = Just CBalanceLeft
  toEnum 39 = Just CBalanceRight
  toEnum 47 = Just CStandby
  toEnum _ = Nothing
  --
  fromEnum CDigitKey1 = 0
  fromEnum CDigitKey2 = 1
  fromEnum CDigitKey3 = 2
  fromEnum CDigitKey4 = 3
  fromEnum CDigitKey5 = 4
  fromEnum CDigitKey6 = 5
  fromEnum CDigitKey7 = 6
  fromEnum CDigitKey8 = 7
  fromEnum CDigitKey9 = 8
  fromEnum CDigitKey0 = 9
  fromEnum CChannelPlus = 16
  fromEnum CChannelMinus = 17
  fromEnum CVolumePlus = 18
  fromEnum CVolumeMinus = 19
  fromEnum CMute = 20
  fromEnum CPower = 21
  fromEnum CReset = 22
  fromEnum CAudioMode = 23
  fromEnum CContrastPlus = 24
  fromEnum CContrastMinus = 25
  fromEnum CColourPlus = 26
  fromEnum CColourMinus = 27
  fromEnum CBrightnessPlus = 30
  fromEnum CBrightnessMinus = 31
  fromEnum CAUXInputSelect = 37
  fromEnum CBalanceLeft = 38
  fromEnum CBalanceRight = 39
  fromEnum CStandby = 47

instance showCommand :: Show Command where
  show = genericShow

-- |
newtype SIRC
  = SIRC { command :: Command, device :: Device }

derive instance newtypeSIRC :: Newtype SIRC _

derive instance genericSIRC :: Generic SIRC _

derive newtype instance eqSIRC :: Eq SIRC

instance showSIRC :: Show SIRC where
  show = genericShow

-- |
type DataSets
  = { command :: BitStream, address :: BitStream, extended :: Maybe BitStream }

-- |
-- SIRC protocol remote control
--
decodeSirc :: Array InfraredCodeFrame -> Maybe (NonEmptyArray SIRC)
decodeSirc = case _ of
  [] -> Nothing
  xs -> NEA.fromArray $ Array.mapMaybe go xs
  where
  go :: InfraredCodeFrame -> Maybe SIRC
  go = case _ of
    FormatSIRC12 v -> decode { command: v.command, address: v.address, extended: Nothing }
    FormatSIRC15 v -> decode { command: v.command, address: v.address, extended: Nothing }
    FormatSIRC20 v -> decode { command: v.command, address: v.address, extended: Just v.extended }
    _ -> Nothing

  decode :: DataSets -> Maybe SIRC
  decode v = do
    com <- toEnum $ unBitOrder $ toLsbFirst v.command
    dev <- toEnum $ unBitOrder $ toLsbFirst v.address
    pure $ SIRC { command: com, device: dev }
