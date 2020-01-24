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

module Foreign.ChartJs
  ( ChartData
  , ChartDatasets
  , LineChartInstance
  , LineChartOptionAxis
  , LineChartOptionScales
  , LineChartOptions
  , defLineChartOptionAxes
  , defLineChartOptions
  , destroyLineChart
  , drawLineChart
  ) where

import Prelude

import Effect (Effect)
import Graphics.Canvas (Context2D)

-- | chart data set
type ChartDatasets =
  { labels :: Array String
  , datasets :: Array ChartData
  }

-- | chart data
type ChartData =
  { label :: String
  , data :: Array Number
  , backgroundColor :: Array String
  , borderColor :: Array String
  , borderWidth :: Int
  }

-- | line chart options
type LineChartOptions =
  { xAxisID :: String
  , yAxisID :: String
  , responsive :: Boolean
  , maintainAspectRatio :: Boolean
  , scales :: LineChartOptionScales
  }

type LineChartOptionScales =
  { xAxes :: Array LineChartOptionAxis
  , yAxes :: Array LineChartOptionAxis
  }

type LineChartOptionAxis =
  { display :: Boolean
  , autoSkip :: Boolean
  , beginAtZero :: Boolean
  , maxTicksLimit :: Int
  }

-- |
foreign import data LineChartInstance :: Type

foreign import destroyLineChartJs :: LineChartInstance -> Effect Unit
foreign import lineChartJs :: Context2D -> ChartDatasets -> LineChartOptions -> Effect LineChartInstance

--|
destroyLineChart :: LineChartInstance -> Effect Unit
destroyLineChart = destroyLineChartJs

--|
drawLineChart :: Context2D -> ChartDatasets -> LineChartOptions -> Effect LineChartInstance
drawLineChart = lineChartJs

--|
defLineChartOptions :: LineChartOptions
defLineChartOptions =
  { xAxisID: "x"
  , yAxisID: "y"
  , responsive: true
  , maintainAspectRatio: true
  , scales: { xAxes: [ defAxes ] , yAxes: [ defAxes ] }
  }
  where
  defAxes = defLineChartOptionAxes

-- |
defLineChartOptionAxes :: LineChartOptionAxis
defLineChartOptionAxes =
  { display: true 
  , autoSkip: false
  , beginAtZero: false
  , maxTicksLimit: 1
  }
