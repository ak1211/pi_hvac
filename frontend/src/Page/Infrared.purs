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

module Page.Infrared
  ( Query
  , SelectedTab
  , component
  ) where

import Prelude

import Api as Api
import AppM (class HasApiAccessible, class Navigate, getApiBaseURL, getApiTimeout, navigate)
import CSS (em, margin, marginBottom, marginTop, minHeight, overline, padding, px, rem, textDecoration, width)
import Components.InfraredCodeEditor as Editor
import Control.Alt ((<|>))
import Data.Array ((:), (..))
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Char (fromCharCode)
import Data.Either (Either(..), either, isRight)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Foldable (intercalate)
import Data.Formatter.Number as FN
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
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
import InfraredCode (Baseband(..), Bit, Count, InfraredCodes(..), InfraredHexString, InfraredLeader(..), LsbFirst, decodeBaseband, decodePhase1, decodePhase2, decodePhase3, infraredHexStringParser, toMilliseconds, toStringLsbFirst, toStringLsbFirstWithHex)
import Page.Commons as Commons
import Route (Route)
import Route as Route
import Text.Parsing.Parser (parseErrorMessage, runParser)
import Utils (toArrayArray)
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.File as File
import Web.File.FileList as FileList
import Web.File.FileReader as FileReader
import Web.HTML.HTMLInputElement as InputElement

-- |
data SelectedTab = TabControlPanel | TabIrdbTable
derive instance genericSelectedTab :: Generic SelectedTab _
derive instance eqSelectedTab :: Eq SelectedTab
derive instance ordSelectedTab :: Ord SelectedTab
instance enumSelectedTab :: Enum SelectedTab where
  succ = genericSucc
  pred = genericPred
instance boundedSelectedTab :: Bounded SelectedTab where
  top = genericTop
  bottom = genericBottom
instance boundedEnumSelectedTab :: BoundedEnum SelectedTab where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

-- |
type State =
  { queryParams       :: Route.InfraredQueryParams
  , infraredValue     :: Either String Api.DatumInfraRed
  , irdb              :: Either String Api.RespGetIrdb
  , irdbManufacturers :: Either String Api.RespGetIrdbManufacturers
  , buttonNumber      :: Int
  }

-- |
data Query a
  = NavigateTo Route a
  | Initialize a
  | ChangedRoute Route a
  | HandleEditorUpdate Editor.Output a
  | OnClickIRCodeDownload a
  | OnClickIRCodeUpload a
  | OnClickIRCodeTransmit a
  | OnClickIrdbPagination Int a
  | OnClickIrdbTable String a
  | OnClickIrdbTableCode String a
  | OnValueChangeButtonNumber String a
  | OnValueChangeManufacturer String a
  | OnValueChangeLimits String a
  | OnChangeCSVFileSelect Event a

type ChildQuery = Editor.Query
type ChildSlot = Unit

-- | component
component
  :: forall m
   . MonadAff m
  => Navigate m
  => HasApiAccessible m
  => H.Component HH.HTML Query Route Void m
component =
  H.lifecycleParentComponent
    { initialState: initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: HE.input ChangedRoute
    }
  where

  initialQueryParams :: Route.InfraredQueryParams
  initialQueryParams =
    { tab: Just $ fromEnum TabControlPanel
    , manuf: Just 0
    , limits: Just 10
    , page: Just 1
    }

  initialState route =
    let qry = case route of
                Route.Infrared Nothing ->
                  initialQueryParams

                Route.Infrared (Just a) ->
                  { tab: a.tab <|> initialQueryParams.tab
                  , manuf: a.manuf <|> initialQueryParams.manuf
                  , limits: a.limits <|> initialQueryParams.limits
                  , page: a.page <|> initialQueryParams.page
                  }

                _ -> initialQueryParams
    in
    { queryParams: qry
    , infraredValue: Left "Click Download button to show."
    , irdb: Left "データがありません"
    , irdbManufacturers: Left "データがありません"
    , buttonNumber: 1
    }

  csvFileInputLabel = H.RefLabel "CSVFileInput"

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    NavigateTo route next -> do
      H.liftEffect Commons.disposePopover
      navigate route
      pure next

    Initialize next -> do
      {queryParams} <- H.get
      let newRoute = Route.Infrared (Just queryParams)
      void $ eval (ChangedRoute newRoute next)
      pure next

    ChangedRoute route next -> do
      H.liftEffect Commons.disposePopover
      curState <- H.get
      --
      let state = case route of
                    Route.Infrared Nothing ->
                      curState { queryParams = initialQueryParams}

                    Route.Infrared (Just qry) ->
                      curState { queryParams = qry }

                    _ -> curState
      --
      case toEnum =<< state.queryParams.tab of
        Just TabIrdbTable -> do
          url <- getApiBaseURL
          millisec <- getApiTimeout
          val <- H.liftAff $
                let param = {baseurl: url}
                    request = Api.getApiV1IrdbManufacturers param
                    timeout = delay millisec $> Left "サーバーからの応答がありませんでした"
                    response r = r.body
                in
                sequential $ parallel (response <$> request) <|> parallel timeout
          H.put state { irdbManufacturers = val
                      }
          --
          fetchIrdb

        _ ->
          H.put state
      H.liftEffect Commons.enablePopover
      pure next

    HandleEditorUpdate (Editor.TextChanged hexstr) next -> do
      { buttonNumber } <- H.get
      let code = hexstr
          val = Api.DatumInfraRed {button_number: buttonNumber, code: code}
      H.modify_ \st -> st { infraredValue = Right val }
      pure next

    HandleEditorUpdate Editor.Reset next -> do
      H.modify_ \st -> st { infraredValue = Left "reset" }
      pure next

    OnClickIRCodeDownload next -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      { buttonNumber } <- H.get
      val <- H.liftAff $
            let param = {baseurl: url, buttonNumber: buttonNumber}
                request = Api.getApiV1InfraRed param
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
              let param = {baseurl: url, datum: ircode}
                  request = Api.postApiV1InfraRed param
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
                let param = {baseurl: url, datum: ircode}
                    request = Api.postApiV1TransIR param
                    timeout = delay millisec $> Left "サーバーからの応答がありませんでした"
                    response r = r.body
                in
                sequential $ parallel (response <$> request) <|> parallel timeout
          H.liftEffect $ logShow res
      pure next

    OnClickIrdbPagination page next -> do
      H.liftEffect Commons.disposePopover
      state <- H.get
      let qp = state.queryParams {page = Just page}
      H.modify_ _ {queryParams = qp}
      fetchIrdb
      navigate $ Route.Infrared (Just qp)
      H.liftEffect Commons.enablePopover
      pure next

    OnClickIrdbTable code next -> do
      H.liftEffect Commons.disposePopover
      state <- H.get
      let irval = Api.DatumInfraRed {button_number: state.buttonNumber, code: code}
          newTab = Just $ fromEnum TabControlPanel
          qp = state.queryParams {tab = newTab}
      H.modify_ _ {infraredValue = Right irval, queryParams = qp}
      navigate $ Route.Infrared (Just qp)
      pure next

    OnClickIrdbTableCode code next -> do
      case runParser code infraredHexStringParser of
        Left err ->
          H.liftEffect $ logShow $ parseErrorMessage err
        Right tokens ->
          H.liftEffect $ logShow tokens
      pure next

    OnValueChangeButtonNumber text next -> do
      case Int.fromString text of
        Nothing -> pure unit
        Just n -> H.modify_ \st -> st { buttonNumber = n }
      pure next

    OnValueChangeManufacturer text next -> do
      H.liftEffect Commons.disposePopover
      state <- H.get
      let maybeManuf = either
                        (const Nothing)
                        (\(Api.RespGetIrdbManufacturers x) -> Just x.manufacturers)
                        state.irdbManufacturers
          maybeIndex = Array.elemIndex text =<< maybeManuf
          newQry = state.queryParams
                    { manuf = maybeIndex <|> initialQueryParams.manuf
                    , page = Just 1
                    }
      H.modify_ _ {queryParams = newQry}
      fetchIrdb
      navigate $ Route.Infrared (Just newQry)
      H.liftEffect Commons.enablePopover
      pure next

    OnValueChangeLimits text next -> do
      H.liftEffect Commons.disposePopover
      state <- H.get
      let maybeLimits = Int.fromString text
          newQry = state.queryParams
                    { limits = maybeLimits <|> initialQueryParams.limits
                    , page = Just 1
                    }
      H.modify_ _ {queryParams = newQry}
      fetchIrdb
      navigate $ Route.Infrared (Just newQry)
      H.liftEffect Commons.enablePopover
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
    where

      fetchIrdb = do
        url <- getApiBaseURL
        millisec <- getApiTimeout
        state <- H.get
        case state.irdbManufacturers of
          Left _ ->
            pure unit

          Right (Api.RespGetIrdbManufacturers xs) -> do
            val <- H.liftAff $
                  let param = { baseurl: url
                              , manufacturer: Array.index xs.manufacturers =<< state.queryParams.manuf
                              , product: Nothing
                              , limits: state.queryParams.limits
                              , page: state.queryParams.page
                              }
                      request = Api.getApiV1Irdb param
                      timeout = delay millisec $> Left "サーバーからの応答がありませんでした"
                      response r = r.body
                  in
                  sequential $ parallel (response <$> request) <|> parallel timeout
            H.modify_ \st -> st { irdb = val }

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
  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
    HH.div
      [ HP.id_ "wrapper"
      ]
      [ Commons.navbar NavigateTo (Route.Infrared Nothing)
      , HH.main
        [ HP.class_ HB.container
        ]
        [ renderTab state
        , case toEnum =<< state.queryParams.tab of
            Nothing               -> renderControlPanel state
            Just TabControlPanel  -> renderControlPanel state
            Just TabIrdbTable     -> renderIrdbTable state
        ]
      , Commons.footer
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
      case toEnum =<< state.queryParams.tab of
        Nothing ->
          [ tabControlPanel [HB.active] , tabIrdbTable [] ]

        Just TabControlPanel ->
          [ tabControlPanel [HB.active] , tabIrdbTable [] ]

        Just TabIrdbTable ->
          [ tabControlPanel [], tabIrdbTable [HB.active] ]
    where

    tabControlPanel =
      item TabControlPanel "Control panel"

    tabIrdbTable =
      item TabIrdbTable "Infrared database"

    item newTab caption appendix =
      let qp = state.queryParams { tab = Just $ fromEnum newTab }
      in
      HH.li
        [ HP.class_ HB.navItem
        ]
        [ HH.a
          [ HP.classes $ [HB.navLink] <> appendix
          , HE.onClick $ HE.input_ (NavigateTo $ Route.Infrared $ Just qp)
          ]
          [ HH.text caption
          ]
        ]

  renderControlPanel state =
    HH.div_
      [ HH.div
        [ HP.class_ HB.formInline ]
        [ HH.div
          [ HP.classes [ HB.formGroup ]
          ]
          [ HH.label_ [ HH.text "Button Number" ]
          , HH.select
            [ HP.classes [ HB.m3, HB.formControl ]
            , HE.onValueChange $ HE.input OnValueChangeButtonNumber
            ]
            $ map (HH.option_ <<< Array.singleton <<< HH.text <<< Int.toStringAs Int.decimal)
            $ 1..10
          ]
        , HH.div
          [ HP.classes [ HB.m3, HB.formGroup ] ]
          [ irDownloadButton OnClickIRCodeDownload
          , irUploadButton OnClickIRCodeUpload $ isRight state.infraredValue 
          , irTransmitButton OnClickIRCodeTransmit $ isRight state.infraredValue
          ]
        ]
      , renderInfraredRemoconCode state
      ]

  renderIrdbTable state =
    HH.div_
      [ HH.div_
        [ case state.irdbManufacturers of
            Left reason ->
              HH.p
                [ HP.classes [ HB.alert, HB.alertDanger ] ]
                [ HH.text reason ]

            Right manuf ->
              dropdownManuf manuf
        , dropdownLimits
        ]
      , HH.h2_ [ HH.text "Infrared code database" ]
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
          [ irdbPagination OnClickIrdbPagination state
          , irdbTable OnClickIrdbTable OnClickIrdbTableCode state.irdb
          ]
        ]
      ]
    where

    dropdownLimits =
      HH.div
        [ HP.classes [ HB.formGroup, HB.row ]
        ]
        [ HH.label
          [ HP.class_ HB.colSm2 ]
          [ HH.text "limits" ]
        , HH.div
          [ HP.class_ HB.colSm10
          ]
          [ HH.select
            [ HP.classes [ HB.formControl ]
            , HE.onValueChange $ HE.input OnValueChangeLimits
            ]
            [ item 10
            , item 25
            , item 50
            , item 100
            ]
          ]
        ]
        where

        item number =
          let str = Int.toStringAs Int.decimal number 
          in
          case state.queryParams.limits of
            Just x | x == number  -> HH.option [HP.selected true] [HH.text str]
            _                     -> HH.option [] [HH.text str]

    dropdownManuf (Api.RespGetIrdbManufacturers x) =
      HH.div
        [ HP.classes [ HB.formGroup, HB.row ]
        ]
        [ HH.label
          [ HP.class_ HB.colSm2 ]
          [ HH.text "manufacturer" ]
        , HH.div
          [ HP.class_ HB.colSm10
          ]
          [ HH.select
            [ HP.classes [ HB.formControl ]
            , HE.onValueChange $ HE.input OnValueChangeManufacturer
            ]
            $ Array.zipWith item (0 .. Array.length x.manufacturers) x.manufacturers
          ]
        ]
        where

        item number name =
          case state.queryParams.manuf of
            Just manuf | manuf == number  -> HH.option [HP.selected true] [HH.text name]
            _                             -> HH.option [] [HH.text name]

-- |
infraredBaseband :: forall p i. Baseband -> H.HTML p i
infraredBaseband (Baseband pulses) =
  HH.div [HP.class_ HB.row] $ map col pulses
  where

  toText p =
    [ HH.span [HP.class_ HB.textPrimary] [HH.text $ strMillisec p.on <> "on"]
    , HH.text ", "
    , HH.span [HP.class_ HB.textSuccess] [HH.text $ strMillisec p.off <> "off"]
    ]

  strMillisec :: Count -> String
  strMillisec n =
    either (const "N/A") identity
    $ FN.formatNumber "0.00"
    $ unwrap
    $ toMilliseconds n

  col p =
    HH.div
      [ HP.classes [HB.col6, HB.colMd2]
      ]
      [ HH.div_
        [ HH.span
          [ HP.class_ HB.textPrimary ]
          [ HH.text $ strMillisec p.on <> "on" ]
        , HH.text ("," <> String.singleton nbsp)
        , HH.span
          [ HP.class_ HB.textSecondary ]
          [ HH.text $ strMillisec p.off <> "off" ]
        ]
      ]

  -- | nbsp - non-break spase
  nbsp =
    String.codePointFromChar $ fromMaybe '?' $ fromCharCode 0x00a0

-- |
infraredBitpatterns :: forall p i. Tuple InfraredLeader (Array Bit) -> Array (H.HTML p i)
infraredBitpatterns (Tuple leader vs) =
  case leader of
    ProtoAeha _     ->
      [ HH.text "AEHA"
      , HH.br_
      , row $ toArrayArray 8 vs
      ]
    ProtoNec _      ->
      [ HH.text "NEC"
      , HH.br_
      , row $ toArrayArray 8 vs
      ]
    ProtoSirc _  ->
      let bit7 = Array.take 7 vs
          left = Array.drop 7 vs
      in
      [ HH.text "SIRC"
      , HH.br_
      , row (bit7 : toArrayArray 8 left)
      ]
    ProtoUnknown _     ->
      [ HH.text "Unknown"
      , HH.br_
      , row $ toArrayArray 8 vs
      ]
  where

  row xxs =
    HH.div [HP.class_ HB.row] $ map col xxs

  col xs =
    HH.div
      [ HP.classes [HB.col6, HB.colMd2]
      ]
      $ map (HH.text <<< show) xs

-- |
infraredSignal :: forall p i. InfraredCodes -> Array (H.HTML p i)
infraredSignal =
  case _ of
    NEC irValue ->
      [ HH.dl_
        [ dt [ HH.text "protocol" ]
        , dd [ HH.text "NEC" ]
        , dt [ HH.text "custom code" ]
        , dd [ HH.text $ showHex 4 (irValue.customHi <> irValue.customLo) ]
        , dt [ HH.text "data" ]
        , dd [ HH.text $ showHexAndDec 2 irValue.data ]
        , dt [ HH.span [ style do textDecoration overline ] [ HH.text "data" ] ]
        , dd [ HH.text $ showHexAndDec 2 irValue.invData ]
        , dt [ HH.text "stop" ]
        , dd [ HH.text $ show irValue.stop ]
        ]
      ]

    AEHA irValue ->
      [ HH.dl_
        [ dt [ HH.text "protocol" ]
        , dd [ HH.text "AEHA" ]
        , dt [ HH.text "custom code" ]
        , dd [ HH.text $ showHex 4 (irValue.customHi <> irValue.customLo) ]
        , dt [ HH.text "parity" ]
        , dd [ HH.text $ showHexAndDec 1 irValue.parity ]
        , dt [ HH.text "data0" ]
        , dd [ HH.text $ showHexAndDec 1 irValue.data0 ]
        , dt [ HH.text "data" ]
        , dd $ map showData irValue.data
        , dt [ HH.text "stop" ]
        , dd [ HH.text $ show irValue.stop ]
        ]
      ]

    SIRC irValue ->
      [ HH.dl_
        [ dt [ HH.text "protocol" ]
        , dd [ HH.text "SIRC" ]
        , dt [ HH.text "command" ]
        , dd [ HH.text $ showHexAndDec 1 irValue.command ]
        , dt [ HH.text "address" ]
        , dd [ HH.text $ showHexAndDec 2 irValue.address ]
        ]
      ]

    Unknown irValue ->
      [ HH.dl_
        [ dt [ HH.text "unknown protocol" ]
        , dd [ HH.text $ show irValue ]
        ]
      ]
  where

  dt = HH.dt_

  dd = HH.dd [ HP.classes [HB.pl4, HB.row] ]

  showData x =
    HH.span
      [ HP.classes [HB.col6, HB.colMd2]
      ]
      [ HH.text $ showHexAndDec 2 x, HH.text " " ]

-- |
showHex :: Int -> LsbFirst -> String
showHex width bits =
  case width - strLen of
    x | x < 0     -> "0x" <> strNum
      | otherwise -> "0x" <> fill x <> strNum
  where
  strNum = toStringLsbFirstWithHex bits
  strLen = String.length strNum
  fill n = String.joinWith "" $ Array.replicate n "0"
 
-- |
showHexAndDec :: Int -> LsbFirst -> String
showHexAndDec width bits =
  showHex width bits <> "(" <> toStringLsbFirst bits <> ")"

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

  appendix = case _ of
    true -> HP.attr (HC.AttrName "active") "active"
    false -> HP.attr (HC.AttrName "disabled") "disabled"

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
renderInfraredRemoconCode
  :: forall m
   . MonadAff m
  => State
  -> H.ParentHTML Query ChildQuery ChildSlot m
renderInfraredRemoconCode state =
  HH.div
    [ HP.class_ HB.formGroup ]
    $ case state.infraredValue of
      Left _ ->
        [ HH.h3_ [ HH.text "Edit codes" ]
        , HH.slot unit Editor.component "" (HE.input HandleEditorUpdate)
        ]

      Right (Api.DatumInfraRed ir) ->
        [ HH.h3_ [ HH.text "Edit codes" ]
        , HH.slot unit Editor.component ir.code (HE.input HandleEditorUpdate)
        , display ir
        ]
    where

    display ir =
      let baseband    = Bifunctor.lmap parseErrorMessage (runParser ir.code infraredHexStringParser)
          bitPatterns = (traverse decodePhase2 <<< decodePhase1) =<< baseband
          signal      = traverse decodePhase3 =<< bitPatterns
      in
      HH.p_
        [ HH.h3_ [ HH.text "Binaries" ]
        , HH.p
          [ HP.classes [ HB.p3, HC.ClassName "overflow-auto" ]
          , style do
            padding (px 10.0) (px 10.0) (px 10.0) (px 10.0)
            minHeight (em 5.0)
          ]
          [ HH.text $ either identity (_.code <<< unwrap) $ state.infraredValue
          ]
        , HH.h3_ [ HH.text "Baseband in milliseconds" ]
        , HH.p
          [ HP.class_ HB.p3 ]
          [ either HH.text infraredBaseband baseband ]
        , HH.h3_ [ HH.text "Bit patterns" ]
        , HH.p
          [ HP.class_ HB.p3 ]
          $ either (Array.singleton <<< HH.text) (intercalate [HH.hr_] <<< map infraredBitpatterns) bitPatterns
        , HH.h3_ [ HH.text "Infrared remote control code" ]
        , HH.p
          [ HP.class_ HB.p3 ]
          $ either (Array.singleton <<< HH.text) (intercalate [HH.hr_] <<< map infraredSignal) signal
        ]
 
-- |
irdbPagination
  :: forall p f
   . (Int -> HQ.Action f)
  -> State
  -> H.HTML p f
irdbPagination click state = case state.irdb of
  Left _ ->
    HH.div_ []

  Right (Api.RespGetIrdb irdb) -> do
    HH.nav
      [ HP.attr (HC.AttrName "area-label") "Pagination"
      ]
      [ HH.ul
        [ HP.classes [HB.pagination, HB.row, HB.noGutters]
        ]
        $ map (item irdb) (1 .. irdb.pages)
      ]
  where

  item irdb number =
    HH.li
    [ HP.classes $ classes irdb number ]
    [ HH.a
      [ HP.class_ HB.pageLink
      , HE.onClick $ HE.input_ (click number)
      ]
      $ text irdb number
    ]

  classes irdb n =
    if n == irdb.page then
      [ HB.pageItem, HB.colAuto, HB.active ]
    else
      [ HB.pageItem, HB.colAuto ]

  text irdb n =
    if n == irdb.page then
      [ HH.text $ Int.toStringAs Int.decimal n
      , HH.span [HP.class_ HB.srOnly] [HH.text "(current)"]
      ]
    else
      [ HH.text $ Int.toStringAs Int.decimal n ]

-- |
irdbTable
  :: forall p f
   . (String -> HQ.Action f)
  -> (String -> HQ.Action f)
  -> Either String Api.RespGetIrdb
  -> H.HTML p f
irdbTable rowClick codeClick = case _ of
  Left reason ->
    HH.p
      [ HP.classes [ HB.alert, HB.alertDanger ] ]
      [ HH.text reason ]

  Right (Api.RespGetIrdb irdb) ->
    HH.p_
      [ HH.table
        [ HP.classes [ HB.table, HB.tableHover ]
        ]
        [ tableHeading
        , tableBody irdb.data
        ]
      ]
  where

  tableHeading =
    HH.thead_ [ HH.tr_ items ]
    where
    items =
      map
        (HH.th_ <<< Array.singleton <<< HH.text)
        [ "id", "manufacturer", "product", "key", "code" ]

  tableBody values =
    HH.tbody_ $ map tableRow values

  tableRow (Api.DatumIrdb val) =
    let clk = HE.onClick $ HE.input_ (rowClick val.code)
    in
    HH.tr_
      [ HH.th [HE.onClick $ HE.input_ (rowClick val.code)] [HH.text $ Int.toStringAs Int.decimal val.id]
      , HH.td [clk] [HH.text val.manuf]
      , HH.td [clk] [HH.text val.prod]
      , HH.td [clk] [HH.text val.key]
      , HH.td_
        [ HH.button
          [ HP.classes [ HB.btn, HB.btnSecondary, HB.justifyContentCenter ]
          , HP.attr (HC.AttrName "data-container") "body"
          , HP.attr (HC.AttrName "data-toggle") "popover"
          , HP.attr (HC.AttrName "data-placement") "left"
          , HP.attr (HC.AttrName "data-content") $ popoverContents val.code
          ]
          [ HH.text $ String.take 8 val.code, HH.text "..."
          ]
        ]
      ]

-- |
popoverContents :: InfraredHexString -> String
popoverContents x =
  Bifunctor.lmap parseErrorMessage (runParser x infraredHexStringParser)
  >>= decodeBaseband
  # case _ of
    Left msg -> msg
    Right xs -> String.joinWith ", " $ map display xs
  where

  display = case _ of
    NEC irValue ->
      String.joinWith " "
        [ "NEC"
        , showHex 4 (irValue.customHi <> irValue.customLo)
        , showHex 2 irValue.data
        , showHex 2 irValue.invData
        ]

    AEHA irValue ->
      String.joinWith " " $ Array.concat
        [ [ "AEHA"
          , showHex 4 (irValue.customHi <> irValue.customLo)
          , showHex 1 irValue.parity
          , showHex 1 irValue.data0
          ]
        , map (showHex 2) irValue.data
        ]

    SIRC irValue ->
      String.joinWith " "
        [ "SIRC"
        , showHex 1 irValue.command
        , showHex 2 irValue.address
        ]

    Unknown irValue ->
      String.joinWith " " $ Array.concat
        [ [ "Unkown"
          , show irValue
          ]
        ]
