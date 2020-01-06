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

import Affjax as AX
import Api as Api
import AppM (class HasApiAccessible, class Navigate, getApiBaseURL, getApiTimeout, navigate)
import CSS (em, margin, marginBottom, marginTop, minHeight, padding, px, rem, width)
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
import Effect.Aff (Aff, Milliseconds, delay, parallel, sequential)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB
import InfraredRemote.Code (Baseband(..), Bit, Count, InfraredCodeFrame(..), InfraredHexString, InfraredLeader(..), IrRemoteControlCode(..), decodePhase1, decodePhase2, decodePhase3, decodePhase4, infraredHexStringParser, toInfraredHexString, toIrRemoteControlCode, toLsbFirst, toMilliseconds)
import InfraredRemote.MitsubishiElectricHvac (MitsubishiElectricHvac(..))
import InfraredRemote.MitsubishiElectricHvac as M
import InfraredRemote.PanasonicHvac (PanasonicHvac(..))
import InfraredRemote.PanasonicHvac as P
import Page.Commons as Commons
import Route (Route)
import Route as Route
import Text.Parsing.Parser (parseErrorMessage, runParser)
import Utils (toArrayArray)

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
  , infraredValue     :: Maybe (Either String Api.DatumInfraRed)
  , irdb              :: Maybe (Either String Api.RespGetIrdb)
  , irdbManufacturers :: Maybe (Either String Api.RespGetIrdbManufacturers)
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
  | OnValueChangeButtonNumber String a
  | OnValueChangeManufacturer String a
  | OnValueChangeLimits String a

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

  initialState :: Route -> State
  initialState route =
    let st =  { queryParams: _
              , infraredValue: Nothing
              , irdb: Nothing
              , irdbManufacturers: Nothing
              , buttonNumber: 1
              }
    in
    case route of
      Route.Infrared (Just a) ->
        st  { tab: a.tab <|> initialQueryParams.tab
            , manuf: a.manuf <|> initialQueryParams.manuf
            , limits: a.limits <|> initialQueryParams.limits
            , page: a.page <|> initialQueryParams.page
            }

      _ ->
        st initialQueryParams

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
      --
      state <- case route of
                Route.Infrared Nothing ->
                  H.modify _{queryParams = initialQueryParams}

                Route.Infrared (Just qry) ->
                  H.modify _{queryParams = qry}

                _ ->
                  H.get
      --
      case toEnum =<< state.queryParams.tab of
        Just TabIrdbTable -> do
          url <- getApiBaseURL
          millisec <- getApiTimeout
          let accessor = Api.getApiV1IrdbManufacturers {baseurl: url}
          response <- accessToBackend millisec accessor
          H.put state {irdbManufacturers = Just response}
          case response of
            Left _ ->
              pure unit

            Right x -> do
              irdb <- getIrdb state.queryParams x
              H.modify_ \st -> st {irdb = Just irdb}

        _ ->
          pure unit
      --
      H.liftEffect Commons.enablePopover
      pure next

    HandleEditorUpdate (Editor.TextChanged hexstr) next -> do
      { buttonNumber } <- H.get
      let code = hexstr
          val = Api.DatumInfraRed {button_number: buttonNumber, code: code}
      H.modify_ \st -> st { infraredValue = Just (Right val) }
      pure next

    HandleEditorUpdate Editor.Reset next -> do
      H.modify_ \st -> st { infraredValue = Nothing }
      pure next

    OnClickIRCodeDownload next -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      { buttonNumber } <- H.get
      let accessor = Api.getApiV1InfraRed {baseurl: url, buttonNumber: buttonNumber}
      response <- accessToBackend millisec accessor
      H.modify_ \st -> st {infraredValue = Just response}
      pure next

    OnClickIRCodeUpload next -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      state <- H.get
      case state.infraredValue of
        Just (Right (Api.DatumInfraRed d)) -> do
          let newD = Api.DatumInfraRed {button_number: state.buttonNumber, code: d.code}
              accessor = Api.postApiV1InfraRed {baseurl: url, datum: newD}
          response <- accessToBackend millisec accessor
          H.liftEffect $ logShow response
          pure next

        _ ->
          pure next

    OnClickIRCodeTransmit next -> do
      url <- getApiBaseURL
      millisec <- getApiTimeout
      state <- H.get
      case state.infraredValue of
        Just (Right ircode) -> do
          let accessor = Api.postApiV1TransIR {baseurl: url, datum: ircode}
          response <- accessToBackend millisec accessor
          H.liftEffect $ logShow response
          pure next

        _ ->
          pure next

    OnClickIrdbPagination page next -> do
      H.liftEffect Commons.disposePopover
      state <- H.get
      let qp = state.queryParams {page = Just page}
      newState <- H.modify _ {queryParams = qp}
      case newState.irdbManufacturers of
        Just (Right x) -> do
          irdb <- getIrdb newState.queryParams x
          H.modify_ \st -> st {irdb = Just irdb}

        _ ->
          pure unit
      navigate $ Route.Infrared (Just qp)
      H.liftEffect Commons.enablePopover
      pure next

    OnClickIrdbTable code next -> do
      H.liftEffect Commons.disposePopover
      state <- H.get
      let irval = Api.DatumInfraRed {button_number: state.buttonNumber, code: code}
          newTab = Just $ fromEnum TabControlPanel
          qp = state.queryParams {tab = newTab}
      H.modify_ _ {infraredValue = Just (Right irval), queryParams = qp}
      navigate $ Route.Infrared (Just qp)
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
                        (\x -> Just (unwrap x).manufacturers)
                        =<< state.irdbManufacturers
          maybeIndex = Array.elemIndex text =<< maybeManuf
          newQry = state.queryParams
                    { manuf = maybeIndex <|> initialQueryParams.manuf
                    , page = Just 1
                    }
      newState <- H.modify _{queryParams = newQry}
      case newState.irdbManufacturers of
        Just (Right x) -> do
          irdb <- getIrdb newState.queryParams x
          H.modify_ \st -> st {irdb = Just irdb}

        _ ->
          pure unit
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
      newState <- H.modify _{queryParams = newQry}
      case newState.irdbManufacturers of
        Just (Right x) -> do
          irdb <- getIrdb newState.queryParams x
          H.modify_ \st -> st {irdb = Just irdb}

        _ ->
          pure unit
      navigate $ Route.Infrared (Just newQry)
      H.liftEffect Commons.enablePopover
      pure next

-- |
getIrdb
  :: forall m
   . MonadAff m
  => HasApiAccessible m
  => Route.InfraredQueryParams
  -> Api.RespGetIrdbManufacturers
  -> m (Either String Api.RespGetIrdb)
getIrdb queryParams (Api.RespGetIrdbManufacturers xs) = do
  url <- getApiBaseURL
  millisec <- getApiTimeout
  let param = { baseurl: url
              , manufacturer: Array.index xs.manufacturers =<< queryParams.manuf
              , product: Nothing
              , limits: queryParams.limits
              , page: queryParams.page
              }
  accessToBackend millisec (Api.getApiV1Irdb param)

--|
accessToBackend
  :: forall a m
   . MonadAff m
  => Milliseconds
  -> Aff (AX.Response (Either String a))
  -> m (Either String a)
accessToBackend millisec accessor =
  H.liftAff $ sequential
    $ parallel (response accessor)
  <|> parallel timeout
  where

  response f = _.body <$> f
  timeout = delay millisec $> Left "サーバーからの応答がありませんでした"

--| render
render
  :: forall m
   . MonadAff m
  => State
  -> H.ParentHTML Query ChildQuery ChildSlot m
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

-- |
renderTab :: forall g p m. MonadAff m => State -> H.ParentHTML Query g p m
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
        [ tabControlPanel [HB.active], tabIrdbTable [] ]

      Just TabControlPanel ->
        [ tabControlPanel [HB.active], tabIrdbTable [] ]

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

-- |
renderControlPanel :: forall m. MonadAff m => State -> H.ParentHTML Query ChildQuery ChildSlot m
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
        [ irDownloadButton
        , irUploadButton $ maybe false isRight $ state.infraredValue 
        , irTransmitButton $ maybe false isRight $ state.infraredValue
        ]
      ]
    , renderInfraredRemoconCode state
    ]

-- |
renderIrdbTable :: forall g p m. MonadAff m => State -> H.ParentHTML Query g p m
renderIrdbTable state =
  case state.irdbManufacturers of
    Nothing ->
      nowreading

    Just (Left reason) ->
      error reason

    Just (Right manuf) ->
      HH.div_ [dropdownManuf manuf, dropdownLimits, table state.irdb]
  where

  table = case _ of
    Nothing ->
      nowreading

    Just (Left reason) ->
      error reason

    Just (Right irdb) ->
      HH.div_
        [ HH.h2_ [ HH.text "Infrared code database" ]
        , HH.div_
          [ HH.div
            [ HP.class_ HB.formGroup ]
            [ irdbPagination irdb, irdbTable irdb ]
          ]
        ]

  nowreading =
    HH.p
      [ HP.classes [ HB.alert, HB.alertInfo ] ]
      [ HH.text "Now on reading..." ]

  error reason =
    HH.p
      [ HP.classes [ HB.alert, HB.alertDanger, HB.textCenter ] ]
      [ HH.text reason ]

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
infraredTimingTable :: forall p i. Baseband -> H.HTML p i
infraredTimingTable (Baseband pulses) =
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
    LeaderAeha _     ->
      [ HH.text "AEHA"
      , HH.br_
      , row $ toArrayArray 8 vs
      ]
    LeaderNec _      ->
      [ HH.text "NEC"
      , HH.br_
      , row $ toArrayArray 8 vs
      ]
    LeaderSirc _  ->
      let bit7 = Array.take 7 vs
          left = Array.drop 7 vs
      in
      [ HH.text "SIRC"
      , HH.br_
      , row (bit7 : toArrayArray 8 left)
      ]
    LeaderUnknown _     ->
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
infraredCodeFrame :: forall p i. InfraredCodeFrame -> Array (H.HTML p i)
infraredCodeFrame =
  case _ of
    FormatNEC irValue ->
      [ HH.dl_
        [ dt [ HH.text "format" ]
        , dd [ HH.text "NEC" ]
        , dt [ HH.text "custom code (LSBit first)" ]
        , dd $ map (showOctet <<< unwrap <<< toLsbFirst) [irValue.custom0, irValue.custom1]
        , dt [ HH.text "octets (LSBit first)" ]
        , dd $ map (showOctet <<< unwrap <<< toLsbFirst) [irValue.data0, irValue.data1]
        , dt [ HH.text "stop" ]
        , dd [ HH.text $ show irValue.stop ]
        ]
      ]

    FormatAEHA irValue ->
      [ HH.dl_
        [ dt [ HH.text "format" ]
        , dd [ HH.text "AEHA" ]
        , dt [ HH.text "octets (LSBit first)" ]
        , dd $ map (showOctet <<< unwrap <<< toLsbFirst) irValue.octets
        , dt [ HH.text "stop" ]
        , dd [ HH.text $ show irValue.stop ]
        ]
      ]

    FormatSIRC irValue ->
      [ HH.dl_
        [ dt [ HH.text "format" ]
        , dd [ HH.text "SIRC" ]
        , dt [ HH.text "command (LSBit first)" ]
        , dd [ HH.text $ showHex <<< unwrap $ toLsbFirst irValue.command ]
        , dt [ HH.text "address (LSBit first)" ]
        , dd [ HH.text $ showHex <<< unwrap $ toLsbFirst irValue.address ]
        ]
      ]

    FormatUnknown irValue ->
      [ HH.dl_
        [ dt [ HH.text "unknown format" ]
        , dd [ HH.text $ show irValue ]
        ]
      ]
  where

  dt = HH.dt_

  dd = HH.dd [ HP.classes [HB.pl4, HB.row] ]

  showOctet x =
    HH.span
      [ HP.classes [HB.col6, HB.colMd2]
      ]
      [ HH.text $ showHex x, HH.text " " ]

-- |
infraredRemoteControlCode :: forall p i. IrRemoteControlCode -> Array (H.HTML p i)
infraredRemoteControlCode = case _ of
  IrRemoteUnknown formats ->
    [ HH.text "Unknown IR remote Code"
    ]

  IrRemotePanasonicHvac (PanasonicHvac v) ->
    [ HH.text "Panasonic HVAC"
    , HH.dl_
      [ dt [ HH.text "Temperature" ]
      , dd [ HH.text (show v.temperature) ]
      , dt [ HH.text "Mode" ]
      , dd [ HH.text (show v.mode) ]
      , dt [ HH.text "Switch" ]
      , dd [ HH.text (show v.switch) ]
      , dt [ HH.text "Fan" ]
      , dd [ HH.text (show v.fan) ]
      , dt [ HH.text "Swing" ]
      , dd [ HH.text (show v.swing) ]
      , dt [ HH.text "Profile" ]
      , dd [ HH.text (show v.profile) ]
      , dt [ HH.text "CRC" ]
      , dd [ HH.text (if P.validCrc v.crc v.original then "Checksum is valid." else "Checksum is NOT valid.")
           , HH.text $ " " <> (show v.crc)
           ]
      ]
    ]

  IrRemoteMitsubishiElectricHvac (MitsubishiElectricHvac v) ->
    [ HH.text "MitsubishiElectric HVAC"
    , HH.dl_
      [ dt [ HH.text "Temperature" ]
      , dd [ HH.text (show v.temperature) ]
      , dt [ HH.text "Mode1" ]
      , dd [ HH.text (show v.mode1) ]
      , dt [ HH.text "Switch" ]
      , dd [ HH.text (show v.switch) ]
      , dt [ HH.text "CRC" ]
      , dd [ HH.text (if M.validCrc v.crc v.original then "Checksum is valid." else "Checksum is NOT valid.")
           , HH.text $ " " <> (show v.crc)
           ]
      ]
    ]
  where

  dt = HH.dt_

  dd = HH.dd [ HP.classes [HB.pl4, HB.row] ]


-- |
showHex :: Int -> String
showHex v =
  let str = Int.toStringAs Int.hexadecimal v
  in
  case String.length str of
    x | x < 2     -> "0" <> str
      | otherwise -> str
 
-- |
irDownloadButton :: forall g p m. H.ParentHTML Query g p m
irDownloadButton =
  HH.button
    [ HP.classes
      [ HB.btn
      , HB.btnOutlineSuccess
      , HB.justifyContentCenter
      ]
    , HE.onClick $ HE.input_ OnClickIRCodeDownload
    , style do
      margin (px 2.0) (px 2.0) (px 2.0) (px 2.0)
      width (rem 8.0)
    ]
    [ HH.text "Download" ]

-- |
irUploadButton :: forall g p m. Boolean -> H.ParentHTML Query g p m
irUploadButton isActive =
  HH.button
    [ HP.classes
      [ HB.btn
      , HB.btnOutlineDanger
      , HB.justifyContentCenter
      ]
    , HE.onClick $ HE.input_ OnClickIRCodeUpload
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
irTransmitButton :: forall g p m. Boolean -> H.ParentHTML Query g p m
irTransmitButton isActive =
  HH.button
    [ HP.classes
      [ HB.btn
      , HB.btnOutlinePrimary
      , HB.justifyContentCenter
      ]
    , HE.onClick $ HE.input_ OnClickIRCodeTransmit
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
renderInfraredRemoconCode :: forall m. MonadAff m => State -> H.ParentHTML Query ChildQuery ChildSlot m
renderInfraredRemoconCode state =
  HH.div
    [ HP.class_ HB.formGroup ]
    $ case state.infraredValue of
      Nothing ->
        [ HH.h3_ [ HH.text "Edit codes" ]
        , HH.slot unit Editor.component "" (HE.input HandleEditorUpdate)
        ]

      Just (Left _) ->
        [ HH.h3_ [ HH.text "Edit codes" ]
        , HH.slot unit Editor.component "" (HE.input HandleEditorUpdate)
        ]

      Just (Right (Api.DatumInfraRed ir)) ->
        [ HH.h3_ [ HH.text "Edit codes" ]
        , HH.slot unit Editor.component ir.code (HE.input HandleEditorUpdate)
        , display ir
        ]
    where

    display ir =
      let baseband      = Bifunctor.lmap parseErrorMessage (runParser ir.code infraredHexStringParser)
          bitPatterns   = (traverse decodePhase2 <<< decodePhase1) =<< baseband
          irframes      = traverse decodePhase3 =<< bitPatterns
          irRemoteCode  = Bifunctor.rmap decodePhase4 irframes
      in
      HH.p_
        [ HH.h3_ [ HH.text "Binaries" ]
        , HH.p
          [ HP.classes [ HB.p3, HC.ClassName "overflow-auto" ]
          , style do
            padding (px 10.0) (px 10.0) (px 10.0) (px 10.0)
            minHeight (em 5.0)
          ]
          [ case state.infraredValue of
              Nothing -> HH.text ""
              Just (Left x) -> HH.text x
              Just (Right x) -> 
                let
                  input = (unwrap x).code
                  bb = toBaseband input
                in
                either HH.text (HH.text <<< toInfraredHexString) bb
          ]
        , HH.h3_ [ HH.text "Timing table in milliseconds" ]
        , HH.p
          [ HP.class_ HB.p3 ]
          [ either HH.text infraredTimingTable baseband ]
        , HH.h3_ [ HH.text "Bit patterns" ]
        , HH.p
          [ HP.class_ HB.p3 ]
          $ either (Array.singleton <<< HH.text) (intercalate [HH.hr_] <<< map infraredBitpatterns) bitPatterns
        , HH.h3_ [ HH.text "Infrared remote control frames" ]
        , HH.p
          [ HP.class_ HB.p3 ]
          $ either (Array.singleton <<< HH.text) (intercalate [HH.hr_] <<< map infraredCodeFrame) irframes
        , HH.h3_ [ HH.text "Infrared remote control code" ]
        , HH.p
          [ HP.class_ HB.p3 ]
          $ either (Array.singleton <<< HH.text) infraredRemoteControlCode irRemoteCode
        ]
 
-- |
irdbPagination :: forall g p m. Api.RespGetIrdb -> H.ParentHTML Query g p m
irdbPagination (Api.RespGetIrdb irdb) =
  HH.nav
    [ HP.attr (HC.AttrName "area-label") "Pagination"
    ]
    [ HH.ul
      [ HP.classes [HB.pagination, HB.row, HB.noGutters]
      ]
      $ map item (1 .. irdb.pages)
    ]
  where

  item number =
    HH.li
    [ HP.classes $ classes number ]
    [ HH.a
      [ HP.class_ HB.pageLink
      , HE.onClick $ HE.input_ (OnClickIrdbPagination number)
      ]
      $ text number
    ]

  classes n =
    if n == irdb.page then
      [ HB.pageItem, HB.colAuto, HB.active ]
    else
      [ HB.pageItem, HB.colAuto ]

  text n =
    if n == irdb.page then
      [ HH.text $ Int.toStringAs Int.decimal n
      , HH.span [HP.class_ HB.srOnly] [HH.text "(current)"]
      ]
    else
      [ HH.text $ Int.toStringAs Int.decimal n ]

-- |
irdbTable :: forall g p m. Api.RespGetIrdb -> H.ParentHTML Query g p m
irdbTable (Api.RespGetIrdb irdb) =
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
    let clk = HE.onClick $ HE.input_ (OnClickIrdbTable val.code)
    in
    HH.tr_
      [ HH.th [HE.onClick $ HE.input_ (OnClickIrdbTable val.code)] [HH.text $ Int.toStringAs Int.decimal val.id]
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

toBaseband :: InfraredHexString -> Either String Baseband
toBaseband inp =
  Bifunctor.lmap parseErrorMessage (runParser inp infraredHexStringParser)

-- |
popoverContents :: InfraredHexString -> String
popoverContents input =
  either identity display $ toIrCode input
  where
  
  toIrCode :: InfraredHexString -> Either String IrRemoteControlCode
  toIrCode =
    toIrRemoteControlCode <=< toBaseband
 
  display :: IrRemoteControlCode -> String
  display = case _ of
    IrRemoteUnknown formats ->
      String.joinWith ", " $ map showFormat formats

    IrRemotePanasonicHvac _ ->
      "Panasonic HVAC"

    IrRemoteMitsubishiElectricHvac _ ->
      "Mitsubishi Electric HVAC"

  showFormat :: InfraredCodeFrame -> String
  showFormat = case _ of
    FormatNEC irValue ->
      String.joinWith " "
        [ "NEC"
        , showHex <<< unwrap $ toLsbFirst irValue.custom0
        , showHex <<< unwrap $ toLsbFirst irValue.custom1
        , showHex <<< unwrap $ toLsbFirst irValue.data0
        , showHex <<< unwrap $ toLsbFirst irValue.data1
        ]

    FormatAEHA irValue ->
      String.joinWith " " $ Array.concat
        [ Array.singleton "AEHA"
        , map (showHex <<< unwrap <<< toLsbFirst) irValue.octets
        ]

    FormatSIRC irValue ->
      String.joinWith " "
        [ "SIRC"
        , showHex <<< unwrap $ toLsbFirst irValue.command
        , showHex <<< unwrap $ toLsbFirst irValue.address
        ]

    FormatUnknown irValue ->
      String.joinWith " " $ Array.concat
        [ [ "Unkown"
          , show irValue
          ]
        ]
