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

module Page.InfraRed
  ( Query(..)
  , component
  ) where

import Prelude

import Api (InfraRedValue(..))
import Api as Api
import AppM (class HasApiAccessible, class Navigate, getApiBaseURL, getApiTimeout, navigate)
import CSS (em, margin, marginLeft, minHeight, padding, px, rem, width)
import Control.Alt ((<|>))
import Data.Array ((..), (:))
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Either (Either(..), either, isRight)
import Data.Foldable (intercalate)
import Data.Formatter.Number as FN
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr1)
import Effect.Aff (delay, parallel, sequential)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB
import InfraRedCode (IRCodeEnvelope(..), IRCodeToken(..))
import InfraRedCode as IRC
import Page.Utils as PU
import Route (Route)
import Route as Route
import Text.Parsing.Parser (parseErrorMessage, runParser)

type DetectedAddresses = Either String (Array Int)

type State =
  { detectedAddresses :: DetectedAddresses
  , infraredValue :: Either String Api.InfraRedValue
  , buttonNumber :: Int
  }

data Query a
  = NavigateTo Route a
  | Initialize a
  | Detect a
  | InfraredCodeDownload a
  | InfraredCodeUpload a
  | InfraredCodeTransmit a
  | OnValueChangeButtonNumber String a

-- | component
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
    , infraredValue: Left "Click Download button to show."
    , buttonNumber: 1
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    NavigateTo route next -> do
      navigate route
      pure next

    Initialize next -> do
      pure next

    Detect next -> do
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

    InfraredCodeDownload next -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      { buttonNumber } <- H.get
      val <- H.liftAff $
            let request = Api.getApiV1InfraRed url buttonNumber
                timeout = delay millisec $> Left "サーバーからの応答がありませんでした"
                response r = r.body
            in
            sequential $ parallel (response <$> request) <|> parallel timeout
      H.modify_ \st -> st { infraredValue = val }
      pure next

    InfraredCodeUpload next -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      state <- H.get
      case state.infraredValue of
        Left _ ->
          pure unit

        Right ircode -> do
          res <- H.liftAff $
              let request = Api.postApiV1InfraRed url ircode
                  timeout = delay millisec $> Left "サーバーからの応答がありませんでした"
                  response r = r.body
              in
              sequential $ parallel (response <$> request) <|> parallel timeout
          H.liftEffect $ logShow res
      pure next

    InfraredCodeTransmit next -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      state <- H.get
      case state.infraredValue of
        Left _ ->
          pure unit

        Right ircode -> do
          res <- H.liftAff $
                let request = Api.postApiV1TransIR url ircode
                    timeout = delay millisec $> Left "サーバーからの応答がありませんでした"
                    response r = r.body
                in
                sequential $ parallel (response <$> request) <|> parallel timeout
          H.liftEffect $ logShow res
      pure next

    OnValueChangeButtonNumber val next -> do
      case Int.fromString val of
        Nothing -> pure unit
        Just n -> H.modify_ \st -> st { buttonNumber = n }
      pure next

  --| render
  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ PU.navbar NavigateTo Route.Infrared
      , HH.div
        [ HP.class_ HB.container ]
        [ HH.h2 [ HP.class_ HB.h2 ] [ i2c, HH.text " devices", i2cDetectButton ]
        , i2cDevices state.detectedAddresses
        , HH.h2 [ HP.class_ HB.h2 ] [ HH.text "Infra-red remote control code" ]
        , HH.div_
          [ HH.div
            [ HP.class_ HB.formGroup ]
            [ HH.div
              [ HP.class_ HB.row ]
              [ HH.div
                [ HP.classes [ HB.col, HB.colLg2 ] ] 
                [ HH.label_ [ HH.text "Button Number" ] ]
              , HH.hr_
              , HH.div
                [ HP.classes [ HB.col, HB.colLg2 ] ] 
                [ HH.select
                  [ HP.class_ HB.formControl
                  , HE.onValueChange $ HE.input OnValueChangeButtonNumber
                  ]
                  $ map (HH.option_ <<< Array.singleton <<< HH.text <<< Int.toStringAs Int.decimal)
                  $ 1..10
                ]
              , HH.div
                [ HP.class_ HB.col ]
                [ irDownloadButton
                , irUploadButton $ isRight state.infraredValue 
                , irTransmitButton $ isRight state.infraredValue
                ]
              ]
            ]
          , downloadInfraredRemoconCode state
          ]
        , HH.h2 [ HP.class_ HB.h2 ] [ HH.text "Pulse milliseconds" ]
        , HH.p_ $ infraredPulse state.infraredValue
        , HH.h2 [ HP.class_ HB.h2 ] [ HH.text "Decoded infrared signal" ]
        , HH.p_ $ infraredSignal state.infraredValue
        ]
      ]

  infraredPulse (Left _) = [ HH.text " " ]
  infraredPulse (Right (InfraRedValue ir)) =
    case runParser ir.code IRC.irCodeParser of
      Left err ->
        [ HH.text $ parseErrorMessage err ]
      Right tokens ->
        intercalate [HH.text ", "] $ map toText tokens
    where

    toText (Pulse p) =
      [ HH.span [HP.class_ HB.textPrimary] [HH.text $ strMillisec p.on <> "on"]
      , HH.text ", "
      , HH.span [HP.class_ HB.textSuccess] [HH.text $ strMillisec p.off <> "off"]
      ]
    toText (Leftover n) =
      [ HH.span [HP.class_ HB.textDanger] [HH.text $ strMillisec n <> "leftover"] ]

    strMillisec :: Int -> String
    strMillisec n =
      either (const "N/A") identity
      $ FN.formatNumber "0.0"
      $ unwrap
      $ IRC.toMilliseconds n

  infraredSignal (Left _) = [ HH.text " " ]
  infraredSignal (Right (InfraRedValue ir)) =
    Bifunctor.lmap parseErrorMessage (runParser ir.code IRC.irCodeParser)
    >>= IRC.irCodeSemanticAnalysis
    # case _ of
      Left msg ->
        [ HH.text msg ]

      Right (NEC irValue) ->
        [ HH.dl_
          [ HH.dt_ [ HH.text "format" ]
          , HH.dd_ [ HH.text "NEC" ]
          , HH.dt_ [ HH.text "customer" ]
          , HH.dd_ [ HH.text $ showHex irValue.customer ]
          , HH.dt_ [ HH.text "data" ]
          , HH.dd_ [ HH.text $ showHex irValue.data ]
          , HH.dt_ [ HH.text "invarted-data" ]
          , HH.dd_ [ HH.text $ showHex irValue.invData ]
          ]
        ]

      Right (AEHA irValue) ->
        [ HH.dl_
          [ HH.dt_ [ HH.text "format" ]
          , HH.dd_ [ HH.text "AEHA" ]
          , HH.dt_ [ HH.text "customer" ]
          , HH.dd_ [ HH.text $ showHex irValue.customer ]
          , HH.dt_ [ HH.text "parity" ]
          , HH.dd_ [ HH.text $ showHex irValue.parity ]
          , HH.dt_ [ HH.text "data0" ]
          , HH.dd_ [ HH.text $ showHex irValue.data0 ]
          , HH.dt_ [ HH.text "data" ]
          , HH.dd_
            $ intercalate
                [ HH.text ", " ]
                $ map (Array.singleton <<< HH.text <<< showHex) irValue.data
          ]
        ]

      Right (IRCodeEnvelope irValue) ->
        [ HH.text "unknown format"
        , HH.p_ [ HH.text $ show irValue ]
        ]
      where
      showHex x | x < 16 = "0x0" <> hexs x
                | otherwise = "0x" <> hexs x
      hexs = String.toUpper <<< Int.toStringAs Int.hexadecimal

  i2cDetectButton =
    HH.button
      [ HP.classes
        [ HB.btn
        , HB.btnOutlineSuccess
        , HB.justifyContentCenter
        ]
      , HE.onClick $ HE.input_ Detect
      , style do
        marginLeft (px 31.0)
      ]
      [ PU.icon "fas fa-search" ]

  irDownloadButton =
    HH.button
      [ HP.classes
        [ HB.btn
        , HB.btnOutlineSuccess
        , HB.justifyContentCenter
        ]
      , HE.onClick $ HE.input_ InfraredCodeDownload
      , style do
        margin (px 2.0) (px 2.0) (px 2.0) (px 2.0)
        width (rem 8.0)
      ]
      [ HH.text "Download" ]

  irUploadButton isActive =
    HH.button
      [ HP.classes
        [ HB.btn
        , HB.btnOutlineDanger
        , HB.justifyContentCenter
        ]
      , HE.onClick $ HE.input_ InfraredCodeUpload
      , style do
        margin (px 2.0) (px 2.0) (px 2.0) (px 2.0)
        width (rem 8.0)
      , appendix isActive
      ]
      [ HH.text "Upload" ]
    where
    appendix true = 
      HP.attr (HC.AttrName "active") "active"
    appendix false = 
      HP.attr (HC.AttrName "disabled") "disabled"

  irTransmitButton isActive =
    HH.button
      [ HP.classes
        [ HB.btn
        , HB.btnOutlinePrimary
        , HB.justifyContentCenter
        ]
      , HE.onClick $ HE.input_ InfraredCodeTransmit
      , style do
        margin (px 2.0) (px 2.0) (px 2.0) (px 2.0)
        width (rem 8.0)
      , appendix isActive
      ]
      [ HH.text "Transmit" ]
    where
    appendix true = 
      HP.attr (HC.AttrName "active") "active"
    appendix false = 
      HP.attr (HC.AttrName "disabled") "disabled"

  downloadInfraredRemoconCode state =
    HH.div
      [ HP.class_ HB.formGroup ]
      [ HH.p [ HP.class_ HB.h4 ] [ HH.text "Show IR code" ]
      , HH.p
        [ HP.classes [ HB.formControl, HC.ClassName "overflow-auto" ]
        , style do
          padding (px 10.0) (px 10.0) (px 10.0) (px 10.0)
          minHeight (em 5.0)
        ]
        [ HH.text $ either identity (_.code <<< unwrap) $ state.infraredValue ]
      ]

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
toArray2D :: forall a. Int -> Array a -> Array (Array a)
toArray2D width =
  unfoldr1 (chop width)
  where

  chop :: Int -> Array a -> Tuple (Array a) (Maybe (Array a))
  chop n xs = 
    case splitAt n xs of
      Tuple a [] -> Tuple a Nothing
      Tuple a b -> Tuple a (Just b)

-- |
splitAt :: forall a. Int -> Array a -> Tuple (Array a) (Array a)
splitAt n xs = 
  Tuple (Array.take n xs) (Array.drop n xs)

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
