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
import CSS (em, margin, marginBottom, marginTop, minHeight, padding, px, rem, width)
import Control.Alt ((<|>))
import Data.Array ((:), (..))
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Either (Either(..), either, isRight)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Foldable (intercalate)
import Data.Formatter.Number as FN
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
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
import Halogen.Themes.Bootstrap4 as BS
import Halogen.Themes.Bootstrap4 as HB
import InfraRedCode (IRCodeEnvelope(..), IRCodeToken(..), IRLeader(..), InfraredHexString)
import InfraRedCode as InfraRedCode
import Page.Commons as Commons
import Route (Route)
import Route as Route
import Text.Parsing.Parser (parseErrorMessage, runParser)
import Utils (toArray2D)
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
  | OnClickIRCodeDownload a
  | OnClickIRCodeUpload a
  | OnClickIRCodeTransmit a
  | OnClickIrdbPagination Int a
  | OnClickIrdbTable String a
  | OnClickIrdbTableCode String a
  | OnValueChangeButtonNumber String a
  | OnValueChangeManufacturer String a
  | OnChangeCSVFileSelect Event a

-- | component
component
  :: forall m
   . MonadAff m
  => Navigate m
  => HasApiAccessible m
  => H.Component HH.HTML Query Route Void m
component =
  H.lifecycleComponent
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
    , page: Just 1
    }

  initialState route =
    let qry = case route of
                Route.Infrared Nothing ->
                  initialQueryParams

                Route.Infrared (Just a) ->
                  { tab: a.tab <|> initialQueryParams.tab
                  , manuf: a.manuf <|> initialQueryParams.manuf
                  , page: a.page <|> initialQueryParams.page
                  }

                _ -> initialQueryParams
    in
    { queryParams: qry
    , infraredValue: Left "Click Download button to show."
    , irdb: Left "Click Download button to show."
    , irdbManufacturers: Left "empty"
    , buttonNumber: 1
    }

  csvFileInputLabel = H.RefLabel "CSVFileInput"

  eval :: Query ~> H.ComponentDSL State Query Void m
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
      case runParser code InfraRedCode.irCodeParser of
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
      let newQry = state.queryParams {manuf = maybeIndex, page = Just 1}
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
                              , limits: Just 10
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
  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ Commons.navbar NavigateTo (Route.Infrared Nothing)
      , HH.div
        [ HP.class_ HB.container ]
        [ renderTab state
        , case toEnum =<< state.queryParams.tab of
            Nothing               -> renderControlPanel state
            Just TabControlPanel  -> renderControlPanel state
            Just TabIrdbTable     -> renderIrdbTable state
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
      [ case state.irdbManufacturers of
          Left reason ->
            HH.p
              [ HP.classes [ HB.alert, HB.alertDanger ] ]
              [ HH.text reason ]

          Right manuf ->
            dropdown manuf
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
    dropdown (Api.RespGetIrdbManufacturers x) =
      HH.div
        [ HP.class_ HB.row ]
        [ HH.div
          [ HP.classes [ HB.col, HB.colLg2 ] ] 
          [ HH.label_ [ HH.text "manufacturer" ] ]
        , HH.div
          [ HP.classes [ HB.col, HB.colLg2 ] ] 
          [ HH.select
            [ HP.class_ HB.formControl
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
infraredPulse :: forall p i. InfraredHexString -> Array (H.HTML p i)
infraredPulse code =
  case runParser code InfraRedCode.irCodeParser of
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
infraredBinary :: forall p i. InfraredHexString -> Array (H.HTML p i)
infraredBinary code =
  Bifunctor.lmap parseErrorMessage (runParser code InfraRedCode.irCodeParser)
  >>= InfraRedCode.semanticAnalysisPhase1
  >>= traverse InfraRedCode.semanticAnalysisPhase2
  # case _ of
    Left msg ->
      [ HH.text msg ]
    Right xs ->
      intercalate [HH.hr_] $ map display xs
  where

  display (Tuple leader vs) = case leader of
    ProtoAeha _     ->
      [ HH.text "AEHA"
      , HH.br_
      , row $ toArray2D 8 vs
      ]
    ProtoNec _      ->
      [ HH.text "NEC"
      , HH.br_
      , row $ toArray2D 8 vs
      ]
    ProtoSony _  ->
      let bit7 = Array.take 7 vs
          left = Array.drop 7 vs
      in
      [ HH.text "SONY"
      , HH.br_
      , row (bit7 : toArray2D 8 left)
      ]
    ProtoUnknown _     ->
      [ HH.text "Unknown"
      , HH.br_
      , row $ toArray2D 8 vs
      ]

  row xxs =
    HH.div [HP.class_ BS.row] $ map col xxs

  col xs =
    HH.div [HP.class_ BS.col1] $ map (HH.text <<< show) xs

-- |
infraredSignal :: forall p i. InfraredHexString -> Array (H.HTML p i)
infraredSignal code =
  Bifunctor.lmap parseErrorMessage (runParser code InfraRedCode.irCodeParser)
  >>= InfraRedCode.irCodeSemanticAnalysis
  # case _ of
    Left msg ->
      [ HH.text msg ]
    Right xs ->
      intercalate [HH.hr_] $ map display xs
  where

  display = case _ of
    NEC irValue ->
      [ HH.dl_
        [ HH.dt_ [ HH.text "protocol" ]
        , HH.dd_ [ HH.text "NEC" ]
        , HH.dt_ [ HH.text "customer" ]
        , HH.dd_ [ HH.text $ showHexAndDec irValue.customer ]
        , HH.dt_ [ HH.text "data" ]
        , HH.dd_ [ HH.text $ showHexAndDec irValue.data ]
        , HH.dt_ [ HH.text "invarted-data" ]
        , HH.dd_ [ HH.text $ showHexAndDec irValue.invData ]
        ]
      ]

    AEHA irValue ->
      [ HH.dl_
        [ HH.dt_ [ HH.text "protocol" ]
        , HH.dd_ [ HH.text "AEHA" ]
        , HH.dt_ [ HH.text "customer" ]
        , HH.dd_ [ HH.text $ showHexAndDec irValue.customer ]
        , HH.dt_ [ HH.text "parity" ]
        , HH.dd_ [ HH.text $ showHexAndDec irValue.parity ]
        , HH.dt_ [ HH.text "data0" ]
        , HH.dd_ [ HH.text $ showHexAndDec irValue.data0 ]
        , HH.dt_ [ HH.text "data" ]
        , HH.dd_ [ row irValue.data ]
        ]
      ]

    SONY irValue ->
      [ HH.dl_
        [ HH.dt_ [ HH.text "protocol" ]
        , HH.dd_ [ HH.text "SONY" ]
        , HH.dt_ [ HH.text "command" ]
        , HH.dd_ [ HH.text $ showHexAndDec irValue.command ]
        , HH.dt_ [ HH.text "address" ]
        , HH.dd_ [ HH.text $ showHexAndDec irValue.address ]
        ]
      ]

    IRCodeEnvelope irValue ->
      [ HH.text "unknown protocol"
      , HH.p_ [ HH.text $ show irValue ]
      ]

  row xs =
    HH.div [HP.class_ BS.row] $ map col xs

  col x =
    HH.div [HP.class_ BS.col1] [HH.text $ showHexAndDec x]

-- |
showHex :: Int -> String
showHex = case _ of
  x | x < 16    -> "0x0" <> hexs x
    | otherwise -> "0x" <> hexs x
  where
  hexs = String.toUpper <<< Int.toStringAs Int.hexadecimal

-- |
showHexAndDec :: Int -> String
showHexAndDec n =
  showHex n <> "(" <> Int.toStringAs Int.decimal n <> ")"

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

        Right (Api.DatumInfraRed ir) -> 
          [ HH.h5_ [ HH.text "Pulse milliseconds" ]
          , HH.p_ $ infraredPulse ir.code
          , HH.h5_ [ HH.text "Decoded binaries" ]
          , HH.p_ $ infraredBinary ir.code
          , HH.h5_ [ HH.text "Decoded infrared signal" ]
          , HH.p_ $ infraredSignal ir.code
          ]
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
        [ HP.classes [BS.pagination, BS.row, BS.noGutters]
        ]
        $ map (item irdb) (1 .. irdb.pages)
      ]
  where

  item irdb number =
    HH.li
    [ HP.classes $ classes irdb number ]
    [ HH.a
      [ HP.class_ BS.pageLink
      , HE.onClick $ HE.input_ (click number)
      ]
      $ text irdb number
    ]

  classes irdb n =
    if n == irdb.page then
      [ BS.pageItem, BS.colAuto, BS.active ]
    else
      [ BS.pageItem, BS.colAuto ]

  text irdb n =
    if n == irdb.page then
      [ HH.text $ Int.toStringAs Int.decimal n
      , HH.span [HP.class_ BS.srOnly] [HH.text "(current)"]
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
          , HP.attr (HC.AttrName "data-content") $ infraredSemantics val.code
          ]
          [ HH.text $ String.take 8 val.code, HH.text "..."
          ]
        ]
      ]

-- |
infraredSemantics :: String -> String
infraredSemantics x =
  Bifunctor.lmap parseErrorMessage (runParser x InfraRedCode.irCodeParser)
  >>= InfraRedCode.irCodeSemanticAnalysis
  # case _ of
    Left msg -> msg
    Right xs -> String.joinWith ", " $ map display xs
  where

  display = case _ of
    NEC irValue ->
      String.joinWith " "
        [ "NEC"
        , showHex irValue.customer
        , showHex irValue.data
        , showHex irValue.invData
        ]

    AEHA irValue ->
      String.joinWith " " $ Array.concat
        [ [ "AEHA"
          , showHex irValue.customer
          , showHex irValue.parity
          , showHex irValue.data0
          ]
        , map showHex irValue.data
        ]

    SONY irValue ->
      String.joinWith " "
        [ "SONY"
        , showHex irValue.command
        , showHex irValue.address
        ]

    IRCodeEnvelope irValue ->
      String.joinWith " " $ Array.concat
        [ [ "Unkown"
          , show irValue
          ]
        ]
