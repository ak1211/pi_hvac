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

module Api 
  ( BaseURL(..)
  , MeasDateTime(..)
  , EnvMeasValue(..)
  , EnvMeasValues
  , I2cDevices(..)
  , InfraRedValue(..)
  , IRDBValue(..)
  , IRDBValues
  , Manufacturers(..)
  , getApiV1Measurements
  , getApiV1I2cDevices
  , getApiV1InfraRed
  , postApiV1InfraRed
  , postApiV1TransIR
  , urlApiV1Ircsv
  , getApiV1Irdb
  , getApiV1IrdbManufacturers
  ) where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept, mapExcept)
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.Parser.Interval (parseDateTime)
import Data.Formatter.Parser.Utils as Parser
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (class Newtype)
import Data.String as String
import Effect.Aff (Aff)
import Foreign as Foreign
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode, genericDecodeJSON, genericEncodeJSON)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)

-- | JSONデコーダー
decoder :: forall a rep. Generic a rep => GenericDecode rep => String -> Either String a
decoder =
  Bifunctor.lmap renderError <<< runExcept <<< decodeJSON
  where

  renderError :: Foreign.MultipleErrors -> String
  renderError = String.joinWith "" <<< Array.fromFoldable <<< map Foreign.renderForeignError

  decodeJSON :: String -> Foreign.F a
  decodeJSON = genericDecodeJSON defaultOptions {unwrapSingleConstructors = true}

-- | JSONエンコーダー
encoder :: forall a rep. Generic a rep => GenericEncode rep => a -> String
encoder =
  genericEncodeJSON defaultOptions {unwrapSingleConstructors = true}

-- | API endpoint
newtype BaseURL = BaseURL String

--
-- /api/v1/measurements
--

-- | 測定時間型
newtype MeasDateTime = MeasDateTime DateTime

-- | 測定時間型インスタンス
derive instance genericMeasDateTime :: Generic MeasDateTime _
derive instance newtypeMeasDateTime :: Newtype MeasDateTime _
derive instance eqMeasDateTime :: Eq MeasDateTime
derive instance ordMeasDateTime :: Ord MeasDateTime
instance showMeasDateValue :: Show MeasDateTime where
  show = genericShow
instance decodeMeasDateTime :: Decode MeasDateTime where
  decode :: Foreign.Foreign -> Foreign.F MeasDateTime
  decode value = 
    mapExcept (fromString =<< _) $ Foreign.readString value
    where

    fromString :: String -> Either Foreign.MultipleErrors MeasDateTime
    fromString original =
      Bifunctor.bimap (const error) MeasDateTime $ Parser.runP parseDateTime original

    error :: Foreign.MultipleErrors
    error =
      NEL.singleton $ Foreign.TypeMismatch "DateTime" (Foreign.tagOf value)

-- | 測定値型
newtype EnvMeasValue = EnvMeasValue
  { id :: Int
  , degc :: Number
  , hpa :: Number
  , measured_at :: MeasDateTime
  , rh :: Number
  , sensor_id :: String
  }

-- | 測定値型インスタンス
derive instance genericEnvMeasValue :: Generic EnvMeasValue _
derive instance newtypeEnvMeasValue :: Newtype EnvMeasValue _
derive instance eqEnvMeasValue :: Eq EnvMeasValue
instance showEnvMeasValue :: Show EnvMeasValue where
  show = genericShow
instance decodeEnvMeasValue :: Decode EnvMeasValue where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-- | 測定値配列の別名
type EnvMeasValues = Array EnvMeasValue

-- | Phoenix Frameworkサーバーが送ってくるJSONは
-- | JSONキー"data"に内容物が入っている
newtype EnvMeasurements = EnvMeasurements
  { data :: EnvMeasValues
  }

derive instance genericEnvMeasurements :: Generic EnvMeasurements _
instance decodeEnvMeasurements :: Decode EnvMeasurements where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-- | HTTPメソッドgetをエンドポイント(/api/v1/measurements)に送った結果を得る
getApiV1Measurements :: BaseURL -> Maybe Int -> Aff (AX.Response (Either String EnvMeasValues))
getApiV1Measurements (BaseURL baseURL) maybeLimits =
  AX.get ResponseFormat.string url
  <#> \res -> case res.body of
        Left err -> res {body = Left $ AX.printResponseFormatError err}
        Right ok -> res {body = Bifunctor.rmap pickupData $ decoder ok}
  where

  url =
    baseURL <> "/api/v1/measurements" <> case maybeLimits of
      Just n -> "?limits=" <> Int.toStringAs Int.decimal n
      Nothing -> ""

  pickupData (EnvMeasurements x) = x.data

--
-- /api/v1/i2c-devices
--

-- | Phoenix Frameworkサーバーが送ってくるJSONは
-- | JSONキー"data"に内容物が入っている
newtype I2cDevices = I2cDevices
  { data :: Array Int
  }

derive instance genericI2cDevices :: Generic I2cDevices _
instance decodeI2cDevices :: Decode I2cDevices where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance showI2cDevices :: Show I2cDevices where
  show = genericShow

-- | HTTPメソッドgetをエンドポイント(/api/v1/i2c-devices)に送った結果を得る
getApiV1I2cDevices :: BaseURL -> Maybe Int -> Aff (AX.Response (Either String I2cDevices))
getApiV1I2cDevices (BaseURL baseURL) maybeBusNum =
  AX.get ResponseFormat.string url
  <#> \res -> case res.body of
        Left err -> res {body = Left $ AX.printResponseFormatError err}
        Right ok -> res {body = decoder ok}
  where

  url =
    baseURL <> "/api/v1/i2c-devices" <> case maybeBusNum of
      Just n -> "?busnum=" <> Int.toStringAs Int.decimal n
      Nothing -> ""

--
-- /api/v1/infra-red
--

-- | 赤外線コードAPI型
newtype InfraRedValue = InfraRedValue
  { button_number :: Int
  , code :: String
  }

-- | 赤外線コードAPI型インスタンス
derive instance genericInfraRedValue :: Generic InfraRedValue _
derive instance newtypeInfraRedValue :: Newtype InfraRedValue _
derive instance eqInfraRedValue :: Eq InfraRedValue
instance showInfraRedValue :: Show InfraRedValue where
  show = genericShow
instance decodeInfraRedValue :: Decode InfraRedValue where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeInfraRedValue :: Encode InfraRedValue where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

-- | Phoenix Frameworkサーバーが送ってくるJSONは
-- | JSONキー"data"に内容物が入っている
newtype InfraRed = InfraRed
  { data :: InfraRedValue
  }

derive instance genericInfraRed :: Generic InfraRed _
instance decodeInfraRed :: Decode InfraRed where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeInfraRed :: Encode InfraRed where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance showInfraRed :: Show InfraRed where
  show = genericShow

-- | HTTPメソッドgetをエンドポイント(/api/v1/infra-red)に送った結果を得る
getApiV1InfraRed :: BaseURL -> Int -> Aff (AX.Response (Either String InfraRedValue))
getApiV1InfraRed (BaseURL baseURL) buttonNumb =
  AX.get ResponseFormat.string url
  <#> \res -> case res.body of
        Left err -> res {body = Left $ AX.printResponseFormatError err}
        Right ok -> res {body = Bifunctor.rmap pickupData $ decoder ok}
  where

  url =
    baseURL <> "/api/v1/infra-red/" <> Int.toStringAs Int.decimal buttonNumb

  pickupData (InfraRed x) = x.data

-- | HTTPメソッドpostをエンドポイント(/api/v1/infra-red)に送った結果を得る
postApiV1InfraRed :: BaseURL -> InfraRedValue -> Aff (AX.Response (Either String String))
postApiV1InfraRed (BaseURL baseURL) ircode =
  AX.request request
  <#> \res -> case res.body of
        Left err -> res {body = Left $ AX.printResponseFormatError err}
        Right ok -> res {body = Right ok}
  where

  request = AX.defaultRequest
    { url = baseURL <> "/api/v1/infra-red/"
    , method = Left POST
    , headers = [ ContentType applicationJSON ]
    , content = Just (RequestBody.string $ encoder $ packData ircode)
    , responseFormat = ResponseFormat.string
    }

  packData :: InfraRedValue -> InfraRed
  packData x = InfraRed {data: x}

-- | HTTPメソッドpostをエンドポイント(/api/v1/trans-ir)に送った結果を得る
postApiV1TransIR :: BaseURL -> InfraRedValue -> Aff (AX.Response (Either String String))
postApiV1TransIR (BaseURL baseURL) ircode =
  AX.request request
  <#> \res -> case res.body of
        Left err -> res {body = Left $ AX.printResponseFormatError err}
        Right ok -> res {body = Right ok}
  where

  request = AX.defaultRequest
    { url = baseURL <> "/api/v1/trans-ir/"
    , method = Left POST
    , headers = [ ContentType applicationJSON ]
    , content = Just (RequestBody.string $ encoder $ packData ircode)
    , responseFormat = ResponseFormat.string
    }

  packData :: InfraRedValue -> InfraRed
  packData x = InfraRed {data: x}

--
-- /api/v1/ir-csv
--

-- | エンドポイント(/api/v1/ir-csv)
urlApiV1Ircsv :: BaseURL -> String
urlApiV1Ircsv (BaseURL baseURL) = baseURL <> "/api/v1/ir-csv"

--
-- /api/v1/irdb
--

-- | 赤外線データーベース型
newtype IRDBValue = IRDBValue
  { id :: Int
  , manuf :: String
  , prod :: String
  , key :: String
  , code :: String
  }

-- | 赤外線データーベースAPI型インスタンス
derive instance genericIRDBValue :: Generic IRDBValue _
derive instance newtypeIRDBValue :: Newtype IRDBValue _
derive instance eqIRDBValue :: Eq IRDBValue
instance showIRDBValue :: Show IRDBValue where
  show = genericShow
instance decodeIRDBValue :: Decode IRDBValue where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeIRDBValue :: Encode IRDBValue where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

-- | 赤外線データーベース型配列の別名
type IRDBValues = Array IRDBValue

-- | Phoenix Frameworkサーバーが送ってくるJSONは
-- | JSONキー"data"に内容物が入っている
newtype IRDB = IRDB
  { data :: IRDBValues
  }

derive instance genericIRDB :: Generic IRDB _
instance decodeIRDB :: Decode IRDB where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeIRDB :: Encode IRDB where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance showIRDB :: Show IRDB where
  show = genericShow

-- | HTTPメソッドgetをエンドポイント(/api/v1/irdb)に送った結果を得る
getApiV1Irdb :: BaseURL -> Maybe String -> Aff (AX.Response (Either String IRDBValues))
getApiV1Irdb (BaseURL baseURL) maybeManufacture =
  AX.get ResponseFormat.string url
  <#> \res -> case res.body of
        Left err -> res {body = Left $ AX.printResponseFormatError err}
        Right ok -> res {body = Bifunctor.rmap pickupData $ decoder ok}
  where

  url =
    baseURL <> "/api/v1/irdb" <> case maybeManufacture of
      Just v -> "?manufacturer=" <> v
      Nothing -> ""


  pickupData (IRDB x) = x.data

--
-- /api/v1/irdb/manufacturer
--

-- |
newtype Manufacturers = Manufacturers
  { manufacturers :: Array String
  }

-- |
derive instance genericManufacturers :: Generic Manufacturers _
derive instance newtypeManufacturers :: Newtype Manufacturers _
derive instance eqManufacturers :: Eq Manufacturers
instance showManufacturers :: Show Manufacturers where
  show = genericShow
instance decodeManufacturers :: Decode Manufacturers where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-- | HTTPメソッドgetをエンドポイント(/api/v1/irdb/manufacturers)に送った結果を得る
getApiV1IrdbManufacturers :: BaseURL -> Aff (AX.Response (Either String Manufacturers))
getApiV1IrdbManufacturers (BaseURL baseURL) =
  AX.get ResponseFormat.string url
  <#> \res -> case res.body of
        Left err -> res {body = Left $ AX.printResponseFormatError err}
        Right ok -> res {body = decoder ok}
  where

  url =
    baseURL <> "/api/v1/irdb/manufacturers"
