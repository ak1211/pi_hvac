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

module Component.RadialGauge
  ( Query(..)
  , Input
  , component
  ) where

import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Foreign.CanvasGauges (GaugeJsInstance, RadialGaugeOptions, destroyGauge, drawRadialGauge) 
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

type State =
  { refLabel :: H.RefLabel
  , options :: RadialGaugeOptions
  , gaugeInstance :: Maybe GaugeJsInstance
  }

data Query a = HandleInput Input a

-- |
type Input =
  { refLabel :: H.RefLabel
  , options :: RadialGaugeOptions
  }

-- | canvas gauges component
component :: forall m. MonadAff m => H.Component HH.HTML Query Input Void m
component =
  H.lifecycleComponent
    { initialState: initialState
    , render
    , eval
    , initializer: Nothing
    , finalizer: Nothing
    , receiver: HE.input HandleInput
    }
  where

  initialState input =
    { refLabel: input.refLabel
    , gaugeInstance: Nothing
    , options: input.options
    }

  render state =
    HH.canvas [ HP.ref state.refLabel ]
 
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    HandleInput input next -> do
      state <- H.get
      maybeElem <- H.getHTMLElementRef input.refLabel
      newInstance <- H.liftEffect $ case maybeElem of
        Nothing ->
          pure Nothing
        Just elem -> do
          maybe (pure unit) destroyGauge state.gaugeInstance
          Just <$> drawRadialGauge elem input.options
      H.put { refLabel: input.refLabel
            , options: input.options
            , gaugeInstance: newInstance
            }
      pure next
