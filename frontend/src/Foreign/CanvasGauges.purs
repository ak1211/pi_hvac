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
module Foreign.CanvasGauges
  ( RadialGaugeOptions
  , GaugeJsInstance
  , destroyGauge
  , drawRadialGauge
  , redrawRadialGauge
  ) where

import Prelude
import Effect (Effect)
import Web.HTML (HTMLElement)

-- | radial gauge option
type RadialGaugeOptions
  = { width :: Int
    , height :: Int
    , units :: String
    , title :: String
    , value :: Number
    , minValue :: Number
    , maxValue :: Number
    , majorTicks :: Array Int
    , minorTicks :: Int
    , animationRule :: String
    , animationDuration :: Int
    }

-- |
foreign import data GaugeJsInstance :: Type

foreign import destroyGaugeInstance :: GaugeJsInstance -> Effect Unit

foreign import createRadialGauge :: HTMLElement -> RadialGaugeOptions -> Effect GaugeJsInstance

foreign import updateRadialGauge :: GaugeJsInstance -> RadialGaugeOptions -> Effect Unit

--|
destroyGauge :: GaugeJsInstance -> Effect Unit
destroyGauge = destroyGaugeInstance

--|
drawRadialGauge :: HTMLElement -> RadialGaugeOptions -> Effect GaugeJsInstance
drawRadialGauge = createRadialGauge

--|
redrawRadialGauge :: GaugeJsInstance -> RadialGaugeOptions -> Effect Unit
redrawRadialGauge = updateRadialGauge
