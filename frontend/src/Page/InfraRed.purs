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
  , SelectedTab(..)
  , component
  ) where

import Prelude

import Api as Api
import AppM (class HasApiAccessible, class Navigate, getApiBaseURL, getApiTimeout, navigate)
import CSS (em, margin, marginBottom, marginLeft, marginTop, minHeight, padding, px, rem, width)
import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Either (Either(..), either, isRight)
import Data.Foldable (intercalate)
import Data.Formatter.Number as FN
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Effect (Effect)
import Effect.Aff (delay, parallel, sequential)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Foreign (unsafeFromForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Halogen.Themes.Bootstrap4 as HB
import InfraRedCode (IRCodeEnvelope(..), IRCodeToken(..))
import InfraRedCode as InfraRedCode
import Page.Commons as Commons
import Route (Route)
import Route as Route
import Text.Parsing.Parser (parseErrorMessage, runParser)
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.File as File
import Web.File.FileList as FileList
import Web.File.FileReader as FileReader
import Web.HTML.HTMLInputElement as InputElement

type DetectedAddresses = Either String (Array Int)

data SelectedTab = TabControlPanel | TabIrdbTable

type State =
  { infraredValue :: Either String Api.InfraRedValue
  , irdbValues    :: Either String Api.IRDBValues
  , buttonNumber  :: Int
  , selectedTab   :: SelectedTab
  }

data Query a
  = NavigateTo Route a
  | Initialize a
  | OnClickTab SelectedTab a
  | OnClickIRCodeDownload a
  | OnClickIRCodeUpload a
  | OnClickIRCodeTransmit a
  | OnClickIrdbFetch a
  | OnClickIrdbTable String a
  | OnValueChangeButtonNumber String a
  | OnChangeCSVFileSelect Event a

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
    { infraredValue: Left "Click Download button to show."
    , irdbValues: Left "Click Download button to show."
    , buttonNumber: 1
    , selectedTab: TabControlPanel
    }

  csvFileInputLabel = H.RefLabel "CSVFileInput"

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    NavigateTo route next -> do
      navigate route
      pure next

    Initialize next -> do
      pure next

    OnClickTab newTab next -> do
      H.modify_ _ { selectedTab = newTab }
      pure next

    OnClickIRCodeDownload next -> do
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

    OnClickIRCodeUpload next -> do
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

    OnClickIRCodeTransmit next -> do
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

    OnClickIrdbFetch next -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      val <- H.liftAff $
            let request = Api.getApiV1IRDB url
                timeout = delay millisec $> Left "サーバーからの応答がありませんでした"
                response r = r.body
            in
            sequential $ parallel (response <$> request) <|> parallel timeout
      H.modify_ \st -> st { irdbValues = val }
      pure next

    OnClickIrdbTable code next -> do
      { buttonNumber } <- H.get
      let irval = Api.InfraRedValue {button_number: buttonNumber, code: code}
      H.modify_ _ { infraredValue = Right irval, selectedTab = TabControlPanel }
      pure next

    OnValueChangeButtonNumber val next -> do
      case Int.fromString val of
        Nothing -> pure unit
        Just n -> H.modify_ \st -> st { buttonNumber = n }
      pure next

    OnChangeCSVFileSelect evt next -> do
      element <- H.getHTMLElementRef csvFileInputLabel
      H.liftEffect do
        let maybeInput = InputElement.fromHTMLElement =<< element
        maybeFiles <- maybe (pure Nothing) InputElement.files maybeInput
        case maybeFiles of
          Nothing ->
            pure unit
          
          Just filelist -> do
            let file = FileList.item 0 filelist
            maybe (pure unit) readCSVFile file
      pure next

  -- |
--  readCSVFile :: File -> Effect Unit
  readCSVFile file = do
    reader <- FileReader.fileReader
    FileReader.readAsText (File.toBlob file) reader
    el <- eventListener listener
    addEventListener
      (EventType "load")
      el
      false
      (FileReader.toEventTarget reader)
    logShow $ File.type_ file
--    csv <- Co.runProcess $ Co.loop (wait reader)
--    log csv
    where
      listener :: Event -> Effect (Maybe String)
      listener evt = do
        let maybeTarget = FileReader.fromEventTarget =<< Event.target evt
        case maybeTarget of
          Nothing ->
            pure Nothing

          Just fr -> do
            v <- FileReader.result fr
            log $ unsafeFromForeign v
            pure $ Just $ unsafeFromForeign v
--            H.subscribe $ HES.eventSource'

  --| render
  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ Commons.navbar NavigateTo Route.Infrared
      , HH.div
        [ HP.class_ HB.container ]
        [ renderTab state
        , case state.selectedTab of
            TabControlPanel ->
              renderControlPanel state

            TabIrdbTable ->
              renderIrdbTable state
        ]
      ]

  renderTab state =
    HH.ul
      [ HP.classes
        [ HB.nav
        , HB.navTabs
        , HB.navPills
        , HB.navJustified
        , HB.justifyContentCenter
        ]
      , style do
        marginTop (px 12.0)
        marginBottom (px 36.0)
      ]
      case state.selectedTab of
        TabControlPanel ->
          [ tabControlPanel [HB.active], tabIrdbTable [] ]

        TabIrdbTable ->
          [ tabControlPanel [], tabIrdbTable [HB.active] ]
    where
    tabControlPanel =
      item TabControlPanel "Control panel"

    tabIrdbTable =
      item TabIrdbTable "Infrared database"

    item newTab caption appendix =
      HH.li
        [ HP.class_ HB.navItem
        ]
        [ HH.a
          [ HP.classes $ [HB.navLink] <> appendix
          , HE.onClick $ HE.input_ (OnClickTab newTab)
          ]
          [ HH.text caption
          ]
        ]

  renderControlPanel state =
    HH.div_
      [ HH.div
        [ HP.class_ HB.row ]
        [ HH.div
          [ HP.classes [ HB.col, HB.colLg2 ] ] 
          [ HH.label_ [ HH.text "Button Number" ] ]
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
          [ irDownloadButton OnClickIRCodeDownload
          , irUploadButton OnClickIRCodeUpload $ isRight state.infraredValue 
          , irTransmitButton OnClickIRCodeTransmit $ isRight state.infraredValue
          ]
        ]
      , renderInfraredRemoconCode state
      ]

  renderIrdbTable state =
    HH.div_
      [ HH.h2_ [ HH.text "Infrared code database", irdbFetchButton OnClickIrdbFetch ]
      , HH.div_
        [ HH.div
          [ HP.class_ HB.formGroup ]
--            [ HH.label_ [ HH.text "IR-code file input" ]
--            , HH.input
--              [ HP.ref csvFileInputLabel
--              , HP.class_ HB.formControlFile
--              , HP.type_ InputFile
--              , HE.onChange $ HE.input OnChangeCSVFileSelect
--              ]
          [ irdbTable OnClickIrdbTable state.irdbValues
          ]
        ]
    ]

-- |
infraredPulse :: forall p i. Api.InfraRedValue -> Array (H.HTML p i)
infraredPulse (Api.InfraRedValue ir) =
  case runParser ir.code InfraRedCode.irCodeParser of
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
    $ InfraRedCode.toMilliseconds n

-- |
infraredSignal :: forall p i. Api.InfraRedValue -> Array (H.HTML p i)
infraredSignal (Api.InfraRedValue ir) =
  Bifunctor.lmap parseErrorMessage (runParser ir.code InfraRedCode.irCodeParser)
  >>= InfraRedCode.irCodeSemanticAnalysis
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

-- |
irDownloadButton :: forall p f. HQ.Action f -> H.HTML p f
irDownloadButton action =
  HH.button
    [ HP.classes
      [ HB.btn
      , HB.btnOutlineSuccess
      , HB.justifyContentCenter
      ]
    , HE.onClick $ HE.input_ action
    , style do
      margin (px 2.0) (px 2.0) (px 2.0) (px 2.0)
      width (rem 8.0)
    ]
    [ HH.text "Download" ]

-- |
irUploadButton :: forall p f. HQ.Action f -> Boolean -> H.HTML p f
irUploadButton action isActive =
  HH.button
    [ HP.classes
      [ HB.btn
      , HB.btnOutlineDanger
      , HB.justifyContentCenter
      ]
    , HE.onClick $ HE.input_ action
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

-- |
irTransmitButton :: forall p f. HQ.Action f -> Boolean -> H.HTML p f
irTransmitButton action isActive =
  HH.button
    [ HP.classes
      [ HB.btn
      , HB.btnOutlinePrimary
      , HB.justifyContentCenter
      ]
    , HE.onClick $ HE.input_ action
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

-- |
renderInfraredRemoconCode :: forall p i. State -> H.HTML p i
renderInfraredRemoconCode state =
  HH.div
    [ HP.class_ HB.formGroup ]
    [ HH.h5_ [ HH.text "Infrared remote control code" ]
    , HH.p
      [ HP.classes [ HB.formControl, HC.ClassName "overflow-auto" ]
      , style do
        padding (px 10.0) (px 10.0) (px 10.0) (px 10.0)
        minHeight (em 5.0)
      ]
      [ HH.text $ either identity (_.code <<< unwrap) $ state.infraredValue
      ]
    , HH.p_
      $ case state.infraredValue of
        Left _ ->
          []

        Right val -> 
          [ HH.h5_ [ HH.text "Pulse milliseconds" ]
          , HH.p_ $ infraredPulse val
          , HH.h5_ [ HH.text "Decoded infrared signal" ]
          , HH.p_ $ infraredSignal val
          ]
    ]

-- |
irdbFetchButton :: forall p f. HQ.Action f -> H.HTML p f
irdbFetchButton action =
  HH.button
    [ HP.classes
      [ HB.btn
      , HB.btnOutlinePrimary
      , HB.justifyContentCenter
      ]
    , HE.onClick $ HE.input_ action
    , style do
      marginLeft (px 31.0)
    ]
    [ Commons.icon "fas fa-download" ]

irdbTable :: forall p f. (String -> HQ.Action f) -> Either String Api.IRDBValues -> H.HTML p f
irdbTable _ (Left reason) =
  HH.p
    [ HP.classes [ HB.alert, HB.alertDanger ] ]
    [ HH.text reason ]

irdbTable action (Right values) =
  HH.p_
    [ HH.table
      [ HP.classes [ HB.table, HB.tableHover ]
      ]
      [ tableHeading
      , tableBody action values
      ]
    ]

-- |
tableHeading :: forall p i. H.HTML p i
tableHeading =
  HH.thead_ [ HH.tr_ items ]
  where
  items =
    map
      (HH.th_ <<< Array.singleton <<< HH.text)
      [ "id", "manufacturer", "product", "key", "code" ]

-- |
tableBody :: forall p f. (String -> HQ.Action f) -> Api.IRDBValues -> H.HTML p f
tableBody action values =
  HH.tbody_ $ map (tableRow action) values

-- |
tableRow :: forall p f. (String -> HQ.Action f) -> Api.IRDBValue -> H.HTML p f
tableRow action (Api.IRDBValue val) =
  HH.tr
    [ HE.onClick $ HE.input_ (action val.code)
    ]
    [ HH.th_ [ HH.text $ Int.toStringAs Int.decimal val.id ]
    , HH.td_ [ HH.text val.manuf ]
    , HH.td_ [ HH.text val.prod ]
    , HH.td_ [ HH.text val.key ]
    , HH.td_ [ HH.text $ String.take 8 val.code ]
    ]
