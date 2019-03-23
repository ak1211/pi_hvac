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
  ( Query(..)
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
import Data.Tuple (Tuple(..))
import Effect.Aff (delay, parallel, sequential)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB
import Page.Commons as Commons
import Route (Route)
import Route as Route
import Utils (toArray2D)

type DetectedAddresses = Either String (Array Int)

type State =
  { detectedAddresses :: DetectedAddresses
  }

data Query a
  = NavigateTo Route a
  | Initialize a
  | OnClickI2CDetect a

-- | child component
component
  :: forall m
   . MonadAff m
  => Navigate m
  => HasApiAccessible m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState =
    { detectedAddresses: Left "Click Search button to detect devices."
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    NavigateTo route next -> do
      navigate route
      pure next

    Initialize next -> do
      pure next

    OnClickI2CDetect next -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      val <- H.liftAff $
              let request = Api.getApiV1I2cDevices url (Just 1)
                  timeout = delay millisec $> Left "サーバーからの応答がありませんでした"
                  response r = Bifunctor.rmap (\(Api.I2cDevices ds) -> ds.data) r.body
              in
              sequential $ parallel (response <$> request) <|> parallel timeout
      H.modify_ \st -> st { detectedAddresses = val }
      pure next


render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ Commons.navbar NavigateTo Route.Settings
    , HH.div
      [ HP.class_ HB.container ]
      [ HH.h2 [ HP.class_ HB.h2 ] [ i2c, HH.text " devices", i2cDetectButton ]
      , i2cDevices state.detectedAddresses
      ]
    ]
  where
  i2cDetectButton =
    HH.button
      [ HP.classes
        [ HB.btn
        , HB.btnOutlineSuccess
        , HB.justifyContentCenter
        ]
      , HE.onClick $ HE.input_ OnClickI2CDetect
      , style do
        marginLeft (px 31.0)
      ]
      [ Commons.icon "fas fa-search" ]


-- |
i2c :: forall p i. H.HTML p i
i2c =
  HH.span_ [ HH.text "I", HH.sup_ [ HH.text "2"], HH.text "C" ]

-- |
i2cDevices :: forall p i. DetectedAddresses -> H.HTML p i
i2cDevices (Left reason) =
  HH.p
    [ HP.classes [ HB.alert, HB.alertDanger ] ]
    [ HH.text reason ]

i2cDevices (Right detectedDeviceAddresses) =
  HH.p
    []
    [ HH.table
      [ HP.classes [ HB.table, HB.tableHover ]
      ]
      [ HH.caption_ [ HH.text "Hexadecimal addresses" ]
      , tableHeading addressTableRangeHeading
      , tableBody addressTableRangeRowHeading $ toArray2D 16 detectedDevAddresses
      ]
    ]
  where

  addressTableRange :: Array Int
  addressTableRange = 0x00..0x7f

  addressTableRangeHeading :: Array Int
  addressTableRangeHeading =  0x0..0xf

  addressTableRangeRowHeading :: Array Int
  addressTableRangeRowHeading = (_ * 0x10) <$> 0x0..0xf

  detectedDevAddresses :: Array String
  detectedDevAddresses =
    map f addressTableRange
    where
    f x =
      fromMaybe (cellFiller x) $ Map.lookup x devices

  cellFiller :: Int -> String
  cellFiller addr | Array.any (_ == addr) $ 0x03..0x77 = "•"
                  | otherwise = " "

  devices :: Map Int String
  devices =
    Map.fromFoldable $ map f detectedDeviceAddresses
    where
    f :: Int -> Tuple Int String
    f n = Tuple n (toHexS n)

-- |
tableHeading :: forall p i. Array Int -> H.HTML p i
tableHeading range =
  HH.thead_ [ HH.tr_ $ hd : tl ]
  where
  hd = HH.th_ [ HH.text "#" ]
  tl =
    map f range
    where
    f = HH.th_ <<< Array.singleton <<< HH.text <<< toHexS

-- |
tableBody :: forall p i. Array Int -> Array (Array String) -> H.HTML p i
tableBody range array2D =
  HH.tbody_ $ Array.zipWith tableRow range array2D

-- |
tableRow :: forall p i. Int -> Array String -> H.HTML p i
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
