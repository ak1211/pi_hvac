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
module InfraredRemoteCode
  ( IrRemoteControlCode(..)
  , decodePhase4
  , module InfraredRemoteCode.Internal
  , toIrRemoteControlCode
  ) where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor as Bifunctor
import Data.Either (Either)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import InfraredRemoteCode.Devices.DaikinHvac (DaikinHvac, decodeDaikinHvac)
import InfraredRemoteCode.Devices.HitachiHvac (HitachiHvac, decodeHitachiHvac)
import InfraredRemoteCode.Devices.MitsubishiElectricHvac (MitsubishiElectricHvac, decodeMitsubishiElectricHvac)
import InfraredRemoteCode.Devices.PanasonicHvac (PanasonicHvac, decodePanasonicHvac)
import InfraredRemoteCode.Devices.SIRC (SIRC, decodeSirc)
import InfraredRemoteCode.Internal (Baseband(..), Bit(..), BitOrder(..), BitStream, Celsius(..), Count(..), InfraredCodeFrame(..), InfraredHexString, InfraredLeader(..), ProcessError, Pulse, decodePhase1, decodePhase2, decodePhase3, fromBinaryString, fromBoolean, fromMilliseconds, infraredCodeTextParser, showBit, toBoolean, toInfraredHexString, toIrCodeFrames, toLsbFirst, toMilliseconds, toMsbFirst, unBitOrder)
import Prelude (class Eq, class Show, map, ($), (<$>), (<<<))

-- |
data IrRemoteControlCode
  = IrRemoteDaikinHvac DaikinHvac
  | IrRemoteHitachiHvac HitachiHvac
  | IrRemoteMitsubishiElectricHvac MitsubishiElectricHvac
  | IrRemotePanasonicHvac PanasonicHvac
  | IrRemoteSIRC SIRC
  | IrRemoteUnknown (Array InfraredCodeFrame)

derive instance genericIrRemoteControlCode :: Generic IrRemoteControlCode _

derive instance eqIrRemoteControlCode :: Eq IrRemoteControlCode

instance showIrRemoteControlCode :: Show IrRemoteControlCode where
  show = genericShow

-- |
toIrRemoteControlCode :: Baseband -> Either ProcessError (NonEmptyArray IrRemoteControlCode)
toIrRemoteControlCode = Bifunctor.rmap decodePhase4 <<< toIrCodeFrames

-- | 各機種の赤外線信号にする
decodePhase4 :: Array InfraredCodeFrame -> NonEmptyArray IrRemoteControlCode
decodePhase4 frames =
  fromMaybe (unknown frames)
    $ oneOf
        [ sirc frames
        , daikin frames
        , hitachi frames
        , melco frames
        , pana frames
        ]
  where
  daikin x = map IrRemoteDaikinHvac <$> decodeDaikinHvac x

  hitachi x = map IrRemoteHitachiHvac <$> decodeHitachiHvac x

  melco x = map IrRemoteMitsubishiElectricHvac <$> decodeMitsubishiElectricHvac x

  pana x = map IrRemotePanasonicHvac <$> decodePanasonicHvac x

  sirc x = map IrRemoteSIRC <$> decodeSirc x

  unknown x = NEA.singleton $ IrRemoteUnknown x
