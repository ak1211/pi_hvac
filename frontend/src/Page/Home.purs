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

module Page.Home
  ( Query(..)
  , component
  ) where

import Prelude hiding (top)

import Api as Api
import AppM (class HasApiAccessible, class Navigate, getApiBaseURL, getApiTimeout, navigate)
import CSS (marginTop, marginBottom, px)
import Component.RadialGauge as RadialGauge
import Control.Alt ((<|>))
import Data.Array.NonEmpty as NEA
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (Either(..))
import Data.Either.Nested (Either3)
import Data.Foldable (intercalate)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay, sequential, parallel)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (now)
import Halogen as H
import Halogen.Component.ChildPath as ChildPath
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB
import Page.Commons as Commons
import Route (Route)
import Route as Route
import Utils as Utils

type NonEmptyValues = NEA.NonEmptyArray Api.EnvMeasValue

type State =
  { measValues :: Either String NonEmptyValues
  , nowTime :: Milliseconds
  }

data Query a
  = NavigateTo Route a
  | Update a

type ChildQuery = Coproduct3 RadialGauge.Query RadialGauge.Query RadialGauge.Query
type ChildSlot = Either3 Unit Unit Unit

-- | component
component
  :: forall m
   . MonadAff m
  => Navigate m
  => HasApiAccessible m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Update)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialMeasValues = Left "測定データがありません"

  initialState =
    { measValues: initialMeasValues
    , nowTime: Milliseconds 0.0
    }

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    NavigateTo route next -> do
      navigate route
      pure next

    Update next -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      val <- H.liftAff $
              let request = Api.getApiV1Measurements url Nothing
                  timeout = delay millisec $> Left "サーバーからの応答がありませんでした"
                  nonEmptyVal = maybe initialMeasValues Right <<< NEA.fromArray
                  response r = nonEmptyVal =<< r.body
              in
              sequential $ parallel (response <$> request) <|> parallel timeout
      time <- unInstant <$> H.liftEffect now
      H.modify_ \st -> st { measValues = val, nowTime = time }
      H.liftEffect $ Commons.showToast
      pure next

  --| render
  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
    let v = latestValue state
    in
    HH.div_
      [ Commons.navbar NavigateTo Route.Home
      , HH.div [ HP.class_ HB.container ]
        [ HH.div
          [ HP.class_ HB.row
          , style do
            marginTop (px 30.0)
            marginBottom (px 30.0)
          ]
          [ gauge $ HH.slot' ChildPath.cp1 unit RadialGauge.component v.t absurd
          , gauge $ HH.slot' ChildPath.cp2 unit RadialGauge.component v.p absurd
          , gauge $ HH.slot' ChildPath.cp3 unit RadialGauge.component v.h absurd
          ]
        , Commons.toast [ v.msg ]
        ]
      ]
    where

    gauge x =
      HH.div [ HP.class_ HB.colSm ] [ x ]

-- |
latestValue
  :: forall p i
  . State
  -> {t :: RadialGauge.Input, p :: RadialGauge.Input, h :: RadialGauge.Input, msg :: H.HTML p i}
latestValue state = case state.measValues of
  Left err ->
    { t: temperature 0.0
    , p: pressure 0.0
    , h: hygro 0.0
    , msg: msgFailToAccess err
    }

  Right measValues ->
    let v = unwrap $ NEA.last measValues
    in
    { t: temperature v.degc
    , p: pressure v.hpa
    , h: hygro v.rh
    , msg: msgLastUpdatedAt state v.measured_at
    }

-- |
msgFailToAccess :: forall p i. String -> H.HTML p i
msgFailToAccess reason =
  Commons.toastItem "Error" "" reason

-- |
msgLastUpdatedAt :: forall p i. State -> Api.MeasDateTime -> H.HTML p i
msgLastUpdatedAt state (Api.MeasDateTime utc) =
  maybe invalidType ok $ Utils.asiaTokyoDateTime utc
  where

  invalidType = Commons.toastItem "Error" "" "有効な日付ではありませんでした" 

  ok v = Commons.snackbarItem $ message v.time
  
  at :: Milliseconds
  at = unInstant $ fromDateTime utc

  duration :: Milliseconds
  duration = wrap (unwrap state.nowTime - unwrap at)

  minute :: Int
  minute = Int.floor (unwrap duration / (60.0 * 1000.0))

  message :: String -> String
  message time =
    intercalate " "
      [ "Measured at"
      , time <> ","
      , Int.toStringAs Int.decimal minute
      , "mins ago."
      ]

-- |
temperature :: Number ->  RadialGauge.Input
temperature val =
  { refLabel: H.RefLabel "temp"
  , options:  { width: 300
              , height: 300
              , units: "°C"
              , title: "Temprature"
              , value: val
              , minValue: -30.0
              , maxValue: 60.0
              , majorTicks: [-30, -20, -10, 0, 10, 20, 30, 40, 50, 60]
              , minorTicks: 10
              }
  }

-- |
pressure :: Number ->  RadialGauge.Input
pressure val =
  { refLabel: H.RefLabel "pres"
  , options:  { width: 300
              , height: 300
              , units: "hPa"
              , title: "Pressure"
              , value: val
              , minValue: 950.0
              , maxValue: 1050.0
              , majorTicks: [950, 960, 970, 980, 990, 1000, 1010, 1020, 1030, 1040, 1050]
              , minorTicks: 10
              }
  }

-- |
hygro :: Number ->  RadialGauge.Input
hygro val =
  { refLabel: H.RefLabel "hygro"
  , options:  { width: 300
              , height: 300
              , units: "%"
              , title: "Hygro"
              , value: val
              , minValue: 0.0
              , maxValue: 100.0
              , majorTicks: [0, 20, 40, 60, 80, 100]
              , minorTicks: 10
              }
  }
