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

module Component.LineChart
  ( Query
  , Input
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Foreign.ChartJs (LineChartInstance, ChartDatasets, LineChartOptions, drawLineChart, destroyLineChart)
import Graphics.Canvas (Context2D)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Page.Commons as Commons

type State =
  { canvasId :: String
  , maybeLineChart :: Maybe LineChartInstance
  , datasets :: ChartDatasets
  , options :: LineChartOptions
  }

data Query a

data Action
  = Initialize
  | HandleInput Input

-- |
type Input =
  { canvasId :: String
  , datasets :: ChartDatasets
  , options :: LineChartOptions
  }

-- | chartjs component
component :: forall m. MonadAff m => H.Component HH.HTML Query Input Void m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< HandleInput
      }
    }

-- |
initialState :: Input -> State
initialState input =
  { canvasId: input.canvasId
  , datasets: input.datasets
  , options: input.options
  , maybeLineChart: Nothing
  }

-- |
render :: forall i m. State -> H.ComponentHTML Action i m
render state =
  HH.canvas [ HP.id_ state.canvasId ]

-- |
handleAction
  :: forall i o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action i o m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    maybeChart <- H.liftEffect $ newChart state.canvasId state.datasets state.options
    H.put $ state { maybeLineChart = maybeChart }

  HandleInput input -> do
    state <- H.get
    H.liftEffect $ maybe (pure unit) destroyLineChart state.maybeLineChart
    maybeChart <- H.liftEffect $ newChart input.canvasId input.datasets input.options
    let newState = state  { canvasId = input.canvasId
                          , datasets = input.datasets
                          , options = input.options
                          , maybeLineChart = maybeChart
                          } 
    H.put newState

-- |
newChart :: String -> ChartDatasets -> LineChartOptions -> Effect (Maybe LineChartInstance)
newChart canvasId datasets options =
  f =<< Commons.getContext2dById canvasId
  where
  f :: Maybe Context2D -> Effect (Maybe LineChartInstance)
  f Nothing = pure Nothing
  f (Just x)= Just <$> drawLineChart x datasets options
