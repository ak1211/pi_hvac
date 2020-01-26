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

module Page.Plotdata
  ( Query
  , component
  ) where

import Prelude

import Api as Api
import AppM (class HasApiAccessible, class Navigate, getApiBaseURL, navigate)
import CSS (marginLeft, height, rem, vh)
import Component.LineChart as LineChart
import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Formatter.Number as FN
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Foreign.ChartJs (ChartDatasets, defLineChartOptions)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB
import Page.Commons as Commons
import Route (Route)
import Route as Route
import Utils as Utils

type State =
  { queryParams :: Route.PlotdataQueryParams
  , measValues  :: Api.RespGetMeasurements
  }

data Query a

data Action
  = NavigateTo Route
  | Initialize
  | ChangedRoute Route
  | OnInputRecordsLimit String

type ChildSlots =
  ( cTemperature  :: H.Slot LineChart.Query Void Unit
  , cPressure     :: H.Slot LineChart.Query Void Unit
  , cHum          :: H.Slot LineChart.Query Void Unit
  )
 
_cTemperature = SProxy :: SProxy "cTemperature"
_cPressure    = SProxy :: SProxy "cPressure"
_cHum         = SProxy :: SProxy "cHum"

--| canvas element id
idChartArea :: { temp :: String, press :: String, hum :: String }
idChartArea =
  { temp: "temp-chart-area"
  , press: "press-chart-area"
  , hum: "hum-chart-area"
  }

-- | component
component
  :: forall m
   . MonadAff m
  => Navigate m
  => HasApiAccessible m
  => H.Component HH.HTML Query Route Void m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< ChangedRoute
      }
    }

-- |
initialQueryParams :: Route.PlotdataQueryParams
initialQueryParams =
  { limits: Just 30
  }

-- |
initialState :: Route -> State
initialState route =
  let qry = case route of
              Route.Plotdata Nothing ->
                initialQueryParams

              Route.Plotdata (Just qryparams) ->
                { limits: qryparams.limits <|> initialQueryParams.limits
                }

              _ -> initialQueryParams
  in
  { queryParams: qry
  , measValues: []
  }

--| render
render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  let options = defLineChartOptions
      chartdatasets = chartDatasets state.measValues
      tempChartData = {canvasId: idChartArea.temp, datasets: chartdatasets.degc, options: options}
      pressChartData = {canvasId: idChartArea.press, datasets: chartdatasets.hpa, options: options}
      humChartData = {canvasId: idChartArea.hum, datasets: chartdatasets.rh, options: options}
  in
  HH.div
    [ HP.id_ "wrapper"
    ]
    [ Commons.navbar NavigateTo (Route.Plotdata Nothing)
    , HH.main
      [ HP.class_ HB.container
      ]
      [ HH.div
        [ HP.classes [ HB.alert, HB.alertInfo ]
        ] 
        [ HH.div
          [ HP.class_ HB.formGroup
          ]
          [ HH.label
            [ HP.class_ HB.alertHeading
            ]
            [ HH.text "表示数"
            , HH.span [ style $ marginLeft $ rem 1.0 ] []
            , case state.queryParams.limits of
                Nothing -> HH.text "--"
                Just x -> HH.text $ Int.toStringAs Int.decimal x
            ]
          , HH.input
            [ HP.class_ HB.customRange
            , HP.type_ HP.InputRange
            , HP.attr (HC.AttrName "min") "10"
            , HP.attr (HC.AttrName "max") "1000"
            , HP.attr (HC.AttrName "step") "10"
            , HE.onValueInput (\x -> Just $ OnInputRecordsLimit x)
            ]
          ]
        ]
      , card "temp" [{-HB.show-}] "気温" [ HH.slot _cTemperature unit LineChart.component tempChartData absurd ]
      , card "pres" [{-HB.show-}] "気圧" [ HH.slot _cPressure unit LineChart.component pressChartData absurd ]
      , card "humi" [{-HB.show-}] "相対湿度" [ HH.slot _cHum unit LineChart.component humChartData absurd ]
      , card "meas" [] "測定値" [measurementalTable state.measValues]
      ]
    , Commons.footer
    ]

-- |
handleAction
  :: forall i o m
   . MonadAff m
  => Navigate m
  => HasApiAccessible m
  => Action
  -> H.HalogenM State Action i o m Unit
handleAction = case _ of
  NavigateTo route ->
    navigate route

  Initialize -> do
    {queryParams} <- H.get
    let newRoute = Route.Plotdata (Just queryParams)
    handleAction (ChangedRoute newRoute)

  ChangedRoute route -> do
    curState <- H.get
    --
    let state = case route of
                  Route.Plotdata Nothing ->
                    curState {queryParams = initialQueryParams}

                  Route.Plotdata (Just qry) ->
                    curState {queryParams = qry}

                  _ -> curState
    --
    url <- getApiBaseURL
    let param = {baseurl: url, limits: state.queryParams.limits}
    res <- H.liftAff $ Api.getApiV1Measurements param
    case res of
      Left a -> do
        H.put state
        H.liftEffect $ logShow a

      Right result ->
        H.put state { measValues = result.body }

  OnInputRecordsLimit strLimits -> do
    let newLimits = Int.fromString strLimits
    { queryParams } <- H.get
    let newQueryParams = queryParams { limits = newLimits }
    H.modify_ _ { queryParams = newQueryParams }
    navigate $ Route.Plotdata (Just newQueryParams)

-- |
card :: forall p i. String -> Array HC.ClassName -> String -> Array (HH.HTML p i) -> HH.HTML p i
card id_ class_ header body =
  HH.article
    [ HP.class_ HB.card
    ]
    [ HH.div
      [ HP.class_ HB.cardHeader
      , HP.attr (HC.AttrName "data-toggle") "collapse"
      , HP.attr (HC.AttrName "data-target") $ "#" <> id_
      , HP.attr (HC.AttrName "aria-expanded") "true"
      , HP.attr (HC.AttrName "aria-controls") id_
      ]
      [ HH.span [ HP.class_ HB.ml0 ] [ HH.text header ]
      , HH.span [ style $ marginLeft $ rem 1.0 ] []
      , HH.span [ HP.class_ HB.textRight ] [ Commons.icon "fas fa-chevron-down" ]
      ]
    , HH.div
      [ HP.classes ([ HB.cardBody, HB.collapse ] <> class_)
      , style do
          height $ vh 90.0
      , HP.id_ id_
      , HP.attr (HC.AttrName "aria-labelledby") id_
      , HP.attr (HC.AttrName "data-parent") $ "#" <> id_
      ]
      body
    ]

-- |
chartLabels :: Api.RespGetMeasurements -> Array String
chartLabels values =
  map (maybe "N/A" showDateTime <<< localDateTime <<< measDateTime) values
  where
  showDateTime x  = x.date <> " " <> x.time
  localDateTime   = Utils.asiaTokyoDateTime <<< unwrap
  measDateTime    = _.measured_at <<< unwrap

-- |
chartDatasets :: Api.RespGetMeasurements -> {degc :: ChartDatasets, hpa :: ChartDatasets, rh :: ChartDatasets}
chartDatasets values =
  let
    labels = chartLabels values
    dDegc = { label: "気温[℃]"
            , data: map (_.degc <<< unwrap) values
            , backgroundColor: [ "rgba(255, 99, 132, 0.2)" ]
            , borderColor: [ "rgba(255, 99, 132, 1)" ]
            , borderWidth: 1
            }
    dHpa =  { label: "気圧[hpa]"
            , data: map (_.hpa <<< unwrap) values
            , backgroundColor: [ "rgba(99, 200, 0, 0.2)" ]
            , borderColor: [ "rgba(99, 200, 0, 1)" ]
            , borderWidth: 1
            }
    dRh =   { label: "相対湿度[％]"
            , data: map (_.rh <<< unwrap) values
            , backgroundColor: [ "rgba(0, 99, 200, 0.2)" ]
            , borderColor: [ "rgba(0, 99, 200, 1)" ]
            , borderWidth: 1
            }
  in
  { degc: { labels: labels, datasets: [ dDegc ] }
  , hpa: { labels: labels, datasets: [ dHpa ] }
  , rh: { labels: labels, datasets: [ dRh ] }
  }

--| temp / press / hum table
measurementalTable :: forall p i. Api.RespGetMeasurements -> HH.HTML p i
measurementalTable measValues =
  HH.table [ HP.class_ HB.table ]
    [ HH.thead_ heading
    , HH.tbody_ $ map tableRow measValues
    ]
  where

  heading =
    [ HH.tr_
      [ HH.td_ [ HH.text "日付" ]
      , HH.td_ [ HH.text "時刻" ]
      , HH.td_ [ HH.text "温度" ]
      , HH.td_ [ HH.text "気圧" ]
      , HH.td_ [ HH.text "湿度" ]
      ]
    ]
  
  tableRow (Api.MeasEnvironment v) =
    let datetime = Utils.asiaTokyoDateTime $ unwrap v.measured_at
    in
    HH.tr_ 
      [ HH.td_ [ HH.text $ fromMaybe "N/A" $ _.date <$> datetime ]
      , HH.td_ [ HH.text $ fromMaybe "N/A" $ _.time <$> datetime ]
      , HH.td_ [ HH.text $ fromNumber v.degc ]
      , HH.td_ [ HH.text $ fromNumber v.hpa ]
      , HH.td_ [ HH.text $ fromNumber v.rh ]
      ]

--|
fromNumber :: Number -> String
fromNumber =
  either (const "N/A") identity <<< FN.formatNumber "0.00"
