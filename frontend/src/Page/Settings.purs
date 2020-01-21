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

module Page.Settings
  ( Query
  , component
  ) where

import Prelude

import Api as Api
import AppM (class HasApiAccessible, class Navigate, getApiBaseURL, getApiTimeout, navigate)
import CSS (marginLeft, px)
import Control.Alt ((<|>))
import Data.Array ((..), (:))
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (delay, parallel, sequential)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (FormMethod(..), InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Halogen.Themes.Bootstrap4 as HB
import Page.Commons as Commons
import Route (Route)
import Route as Route
import Utils (toArrayArray)

type DetectedAddresses = Either String (Array Int)

type State =
  { urlApiV1IRCSV     :: String
  , detectedAddresses :: Maybe DetectedAddresses
  }

data Query a

data Action
  = NavigateTo Route
  | Initialize
  | OnClickI2CDetect

-- | child component
component
  :: forall m
   . MonadAff m
  => Navigate m
  => HasApiAccessible m
  => H.Component HH.HTML Query Unit Void m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }

-- |
initialState :: forall i. i -> State
initialState _ =
  { urlApiV1IRCSV: ""
  , detectedAddresses: Nothing
  }

-- |
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
      [ HP.id_ "wrapper"
      ]
    [ Commons.navbar NavigateTo Route.Settings
    , HH.main
      [ HP.classes [ HB.mbAuto, HB.container ]
      ]
      [ HH.h2 [ HP.class_ HB.h2 ] [ i2c, HH.text " devices", i2cDetectButton OnClickI2CDetect ]
      , i2cDevices state.detectedAddresses
      , HH.h2 [ HP.class_ HB.h2 ] [ HH.text "Upload to Infrared Code Database" ]
      , uploadCsvFile state
      ]
    , Commons.footer
    ]

-- |
handleAction
  :: forall output m
   . MonadAff m
  => Navigate m
  => HasApiAccessible m
  => Action
  -> H.HalogenM State Action () output m Unit
handleAction = case _ of
    NavigateTo route -> do
      navigate route
      pure mempty

    Initialize -> do
      url <- getApiBaseURL
      H.modify_ _{ urlApiV1IRCSV = Api.urlApiV1Ircsv url }
      pure mempty

    OnClickI2CDetect -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      val <- H.liftAff $
              let param = {baseurl: url, busnumber: Just 1}
                  request = Api.getApiV1I2cDevices param
                  timeout = delay millisec $> Left "サーバーからの応答がありませんでした"
                  response = Bifunctor.rmap (\x -> i2cdevices x.body)
                  i2cdevices (Api.RespGetI2cDevices x) = x.data
              in
              sequential $ parallel (response <$> request) <|> parallel timeout
      H.modify_ \st -> st { detectedAddresses = Just val }
      pure mempty

-- |
i2cDetectButton :: forall p f. f -> HH.HTML p f
i2cDetectButton action =
  HH.button
    [ HP.classes
      [ HB.btn
      , HB.btnOutlineSuccess
      , HB.justifyContentCenter
      ]
    , HE.onClick (\_ -> Just action)
    , style do
      marginLeft (px 31.0)
    ]
    [ Commons.icon "fas fa-search" ]

-- |
i2c :: forall p i. HH.HTML p i
i2c =
  HH.span_ [ HH.text "I", HH.sup_ [ HH.text "2"], HH.text "C" ]

-- |
i2cDevices :: forall p i. Maybe DetectedAddresses -> HH.HTML p i
i2cDevices = case _ of
  Nothing ->
    HH.p
      [ HP.classes [ HB.alert, HB.alertInfo ] ]
      [ HH.text "Click Search button to detect devices." ]

  Just (Left reason) ->
    HH.p
      [ HP.classes [ HB.alert, HB.alertDanger, HB.textCenter ] ]
      [ HH.text reason ]

  Just (Right addresses) ->
    HH.p
      []
      [ HH.table
        [ HP.classes [ HB.table, HB.tableHover ]
        ]
        [ HH.caption_ [ HH.text "Hexadecimal addresses" ]
        , tableHeading addressTableRangeHeading
        , tableBody addressTableRangeRowHeading $ toArrayArray 16 $ detectedDevAddresses addresses
        ]
      ]
  where

  addressTableRange :: Array Int
  addressTableRange = 0x00..0x7f

  addressTableRangeHeading :: Array Int
  addressTableRangeHeading =  0x0..0xf

  addressTableRangeRowHeading :: Array Int
  addressTableRangeRowHeading = (_ * 0x10) <$> 0x0..0xf

  detectedDevAddresses :: Array Int -> Array String
  detectedDevAddresses addresses =
    map f addressTableRange
    where
    f x =
      fromMaybe (cellFiller x) $ Map.lookup x (devices addresses)

  cellFiller :: Int -> String
  cellFiller addr | Array.any (_ == addr) $ 0x03..0x77 = "•"
                  | otherwise = " "

  devices :: Array Int -> Map Int String
  devices xs =
    Map.fromFoldable $ map f xs
    where
    f :: Int -> Tuple Int String
    f n = Tuple n (toHexS n)

-- |
uploadCsvFile :: forall p i. State -> HH.HTML p i
uploadCsvFile state =
  HH.form
    [ HP.method POST
    , HP.action $ state.urlApiV1IRCSV
    , HP.enctype $ MediaType "multipart/form-data"
    ]
    {-}
    [ HH.div
      [ HP.classes [ HB.formGroup ] ]
      [ HH.label_
        [ HH.text "Please select an infrared code CSV file." ]
      , HH.input
        [ HP.class_ $ HB.formControlFile
        , HP.type_ InputFile
        , HP.accept $ MediaType ".csv"
        , HP.name "ircsv"
        ]
      ]
      -}
    [ HH.div
      [ HP.classes [ HB.formGroup ] ]
      [ HH.input
        [ HP.classes [ HB.btn, HB.btnPrimary ]
        , HP.type_ InputSubmit
        , HP.value "Submit"
        ]
      ]
    ]

-- |
tableHeading :: forall p i. Array Int -> HH.HTML p i
tableHeading range =
  HH.thead_ [ HH.tr_ $ hd : tl ]
  where
  hd = HH.th_ [ HH.text "#" ]
  tl =
    map f range
    where
    f = HH.th_ <<< Array.singleton <<< HH.text <<< toHexS

-- |
tableBody :: forall p i. Array Int -> Array (Array String) -> HH.HTML p i
tableBody range array2D =
  HH.tbody_ $ Array.zipWith tableRow range array2D

-- |
tableRow :: forall p i. Int -> Array String -> HH.HTML p i
tableRow index columns =
  HH.tr_  $ hd : tl
  where
  hd = HH.th_ [ HH.text $ toHexS index ]
  tl =
    map f columns
    where
    f = HH.td_ <<< Array.singleton <<< HH.text

-- |
toHexS :: Int -> String
toHexS = Int.toStringAs Int.hexadecimal

-- |
fromHexS :: String -> Maybe Int
fromHexS = Int.fromStringAs Int.hexadecimal
