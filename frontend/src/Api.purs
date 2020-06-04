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
  , DatumInfraRed(..)
  , DatumIrdb(..)
  , MeasDateTime(..)
  , MeasEnvironment(..)
  , ParamGetI2cDevices(..)
  , ParamGetIrdb
  , ParamGetIrdbManufacturers
  , RespGetI2cDevices(..)
  , RespGetIrdb(..)
  , RespGetIrdbManufacturers(..)
  , RespGetMeasurements
  , getApiV1I2cDevices
  , getApiV1InfraRed
  , getApiV1Irdb
  , getApiV1IrdbManufacturers
  , getApiV1Measurements
  , postApiV1InfraRed
  , postApiV1TransIR
  , urlApiV1Ircsv
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
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Effect.Aff (Aff)
import Foreign as Foreign
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode, genericDecodeJSON, genericEncodeJSON)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import InfraredRemoteCode (InfraredHexString)

-- | JSONデコーダー
decoder :: forall a rep. Generic a rep => GenericDecode rep => String -> Either String a
decoder = Bifunctor.lmap renderError <<< runExcept <<< decodeJSON
  where
  renderError :: Foreign.MultipleErrors -> String
  renderError = String.joinWith "" <<< Array.fromFoldable <<< map Foreign.renderForeignError

  decodeJSON :: String -> Foreign.F a
  decodeJSON = genericDecodeJSON defaultOptions { unwrapSingleConstructors = true }

-- | JSONエンコーダー
encoder :: forall a rep. Generic a rep => GenericEncode rep => a -> String
encoder = genericEncodeJSON defaultOptions { unwrapSingleConstructors = true }

-- | API endpoint
newtype BaseURL
  = BaseURL String

derive instance genericBaseURL :: Generic BaseURL _

derive instance newtypeBaseURL :: Newtype BaseURL _

instance showBaseURL :: Show BaseURL where
  show = genericShow

-- | 測定時間型
newtype MeasDateTime
  = MeasDateTime DateTime

-- | 測定時間型インスタンス
derive instance genericMeasDateTime :: Generic MeasDateTime _

derive instance newtypeMeasDateTime :: Newtype MeasDateTime _

derive instance eqMeasDateTime :: Eq MeasDateTime

derive instance ordMeasDateTime :: Ord MeasDateTime

instance showMeasDateValue :: Show MeasDateTime where
  show = genericShow

instance decodeMeasDateTime :: Decode MeasDateTime where
  decode :: Foreign.Foreign -> Foreign.F MeasDateTime
  decode value = mapExcept (fromString =<< _) $ Foreign.readString value
    where
    fromString :: String -> Either Foreign.MultipleErrors MeasDateTime
    fromString original = Bifunctor.bimap (const error) MeasDateTime $ Parser.runP parseDateTime original

    error :: Foreign.MultipleErrors
    error = NEL.singleton $ Foreign.TypeMismatch "DateTime" (Foreign.tagOf value)

--
-- /api/v1/measurements
--
-- | 測定値型
newtype MeasEnvironment
  = MeasEnvironment
  { id :: Int
  , degc :: Number
  , hpa :: Number
  , measured_at :: MeasDateTime
  , rh :: Number
  , sensor_id :: String
  }

-- | 測定値型インスタンス
derive instance genericMeasEnvironment :: Generic MeasEnvironment _

derive instance newtypeMeasEnvironment :: Newtype MeasEnvironment _

derive instance eqMeasEnvironment :: Eq MeasEnvironment

instance showMeasEnvironment :: Show MeasEnvironment where
  show = genericShow

instance decodeMeasEnvironment :: Decode MeasEnvironment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

-- | 受け取ったJSONのキー"env"に内容物が入っている
newtype EnvMeasurements
  = EnvMeasurements
  { env :: Array MeasEnvironment
  }

derive instance genericEnvMeasurements :: Generic EnvMeasurements _

instance decodeEnvMeasurements :: Decode EnvMeasurements where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

-- |
type ParamGetMeasurements
  = { baseurl :: BaseURL
    , limits :: Maybe Int
    }

-- |
type RespGetMeasurements
  = Array MeasEnvironment

-- | HTTPメソッドgetをエンドポイント(/api/v1/measurements)に送った結果を得る
getApiV1Measurements :: ParamGetMeasurements -> Aff (Either String (AX.Response RespGetMeasurements))
getApiV1Measurements param = resp <$> AX.get ResponseFormat.string url
  where
  resp = case _ of
    Right ok -> do
      newBody <- decoder ok.body
      Right $ ok { body = pickupData newBody }
    Left err -> Left $ AX.printError err

  url = (unwrap param.baseurl) <> "/api/v1/measurements" <> opts

  opts :: String
  opts =
    [ ("limits=" <> _) <<< Int.toStringAs Int.decimal <$> param.limits
    ]
      # Array.catMaybes
      # Array.intercalate "&"
      # case _ of
          "" -> ""
          str -> "?" <> str

  pickupData (EnvMeasurements x) = x.env

--
-- /api/v1/i2c-devices
--
-- |
type ParamGetI2cDevices
  = { baseurl :: BaseURL
    , busnumber :: Maybe Int
    }

-- |
newtype RespGetI2cDevices
  = RespGetI2cDevices
  { data :: Array Int
  }

derive instance genericRespGetI2cDevices :: Generic RespGetI2cDevices _

instance decodeRespGetI2cDevices :: Decode RespGetI2cDevices where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance showRespGetI2cDevices :: Show RespGetI2cDevices where
  show = genericShow

-- | HTTPメソッドgetをエンドポイント(/api/v1/i2c-devices)に送った結果を得る
getApiV1I2cDevices :: ParamGetI2cDevices -> Aff (Either String (AX.Response RespGetI2cDevices))
getApiV1I2cDevices param = resp <$> AX.get ResponseFormat.string url
  where
  resp = case _ of
    Right ok -> do
      newBody <- decoder ok.body
      Right $ ok { body = newBody }
    Left err -> Left $ AX.printError err

  url = (unwrap param.baseurl) <> "/api/v1/i2c-devices" <> opts

  opts :: String
  opts =
    [ ("busnum=" <> _) <<< Int.toStringAs Int.decimal <$> param.busnumber
    ]
      # Array.catMaybes
      # Array.intercalate "&"
      # case _ of
          "" -> ""
          str -> "?" <> str

--
-- /api/v1/infra-red
--
-- | 赤外線コードAPI型
newtype DatumInfraRed
  = DatumInfraRed
  { button_number :: Int
  , code :: InfraredHexString
  }

-- | 赤外線コードAPI型インスタンス
derive instance genericDatumInfraRed :: Generic DatumInfraRed _

derive instance newtypeDatumInfraRed :: Newtype DatumInfraRed _

derive instance eqDatumInfraRed :: Eq DatumInfraRed

instance showDatumfraRed :: Show DatumInfraRed where
  show = genericShow

instance decodeDatumInfraRed :: Decode DatumInfraRed where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeDatumInfraRed :: Encode DatumInfraRed where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

-- |
type ParamGetInfraRed
  = { baseurl :: BaseURL
    , buttonNumber :: Int
    }

-- |
type RespGetInfraRed
  = DatumInfraRed

-- | HTTPメソッドgetをエンドポイント(/api/v1/infra-red)に送った結果を得る
getApiV1InfraRed :: ParamGetInfraRed -> Aff (Either String (AX.Response RespGetInfraRed))
getApiV1InfraRed param = resp <$> AX.get ResponseFormat.string url
  where
  resp = case _ of
    Right ok -> do
      newBody <- decoder ok.body
      Right $ ok { body = newBody }
    Left err -> Left $ AX.printError err

  url =
    String.joinWith ""
      [ unwrap param.baseurl
      , "/api/v1/infra-red/"
      , Int.toStringAs Int.decimal param.buttonNumber
      ]

-- |
type ParamPostInfraRed
  = { baseurl :: BaseURL
    , datum :: DatumInfraRed
    }

-- |
type RespPostInfraRed
  = String

-- | HTTPメソッドpostをエンドポイント(/api/v1/infra-red)に送った結果を得る
postApiV1InfraRed :: ParamPostInfraRed -> Aff (Either String (AX.Response RespPostInfraRed))
postApiV1InfraRed param = Bifunctor.lmap AX.printError <$> AX.request request
  where
  request =
    AX.defaultRequest
      { url = (unwrap param.baseurl) <> "/api/v1/infra-red/"
      , method = Left POST
      , headers = [ ContentType applicationJSON ]
      , content = Just (RequestBody.string $ encoder param.datum)
      , responseFormat = ResponseFormat.string
      }

--
-- /api/v1/trans-ir
--
-- |
type ParamPostTransIR
  = { baseurl :: BaseURL
    , datum :: DatumInfraRed
    }

-- |
type RespPostTransIR
  = String

-- | HTTPメソッドpostをエンドポイント(/api/v1/trans-ir)に送った結果を得る
postApiV1TransIR :: ParamPostTransIR -> Aff (Either String (AX.Response RespPostTransIR))
postApiV1TransIR param = Bifunctor.lmap AX.printError <$> AX.request request
  where
  request =
    AX.defaultRequest
      { url = (unwrap param.baseurl) <> "/api/v1/trans-ir/"
      , method = Left POST
      , headers = [ ContentType applicationJSON ]
      , content = Just (RequestBody.string $ encoder param.datum)
      , responseFormat = ResponseFormat.string
      }

--
-- /api/v1/ir-csv
--
-- | エンドポイント(/api/v1/ir-csv)
urlApiV1Ircsv :: BaseURL -> String
urlApiV1Ircsv (BaseURL baseURL) = baseURL <> "/api/v1/ir-csv"

--
-- /api/v1/irdb
--
-- | 赤外線データ型
newtype DatumIrdb
  = DatumIrdb
  { id :: Int
  , manuf :: String
  , prod :: String
  , key :: String
  , code :: String
  }

derive instance genericDatumIrdb :: Generic DatumIrdb _

derive instance newtypeDatumIrdb :: Newtype DatumIrdb _

derive instance eqDatumIrdb :: Eq DatumIrdb

instance showDatumIrdb :: Show DatumIrdb where
  show = genericShow

instance decodeDatumIrdb :: Decode DatumIrdb where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeDatumIrdb :: Encode DatumIrdb where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

-- |
type ParamGetIrdb
  = { baseurl :: BaseURL
    , manufacturer :: Maybe String
    , product :: Maybe String
    , limits :: Maybe Int
    , page :: Maybe Int
    }

-- |
data RespGetIrdb
  = RespGetIrdb
    { counts :: Int
    , limits :: Int
    , page :: Int
    , pages :: Int
    , data :: Array DatumIrdb
    }

derive instance genericRespGetIrdb :: Generic RespGetIrdb _

instance decodeRespGetIrdb :: Decode RespGetIrdb where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance showRespGetIrdb :: Show RespGetIrdb where
  show = genericShow

-- | HTTPメソッドgetをエンドポイント(/api/v1/irdb)に送った結果を得る
getApiV1Irdb :: ParamGetIrdb -> Aff (Either String (AX.Response RespGetIrdb))
getApiV1Irdb param = resp <$> AX.get ResponseFormat.string url
  where
  resp = case _ of
    Right ok -> do
      newBody <- decoder ok.body
      Right $ ok { body = newBody }
    Left err -> Left $ AX.printError err

  url = (unwrap param.baseurl) <> "/api/v1/irdb" <> opts

  opts :: String
  opts =
    [ ("manufacturer=" <> _) <$> param.manufacturer
    , ("product=" <> _) <$> param.product
    , ("limits=" <> _) <<< Int.toStringAs Int.decimal <$> param.limits
    , ("page=" <> _) <<< Int.toStringAs Int.decimal <$> param.page
    ]
      # Array.catMaybes
      # Array.intercalate "&"
      # case _ of
          "" -> ""
          str -> "?" <> str

--
-- /api/v1/irdb-manufacturer
--
-- |
type ParamGetIrdbManufacturers
  = { baseurl :: BaseURL
    }

-- |
newtype RespGetIrdbManufacturers
  = RespGetIrdbManufacturers
  { manufacturers :: Array String
  }

derive instance genericRespGetIrdbManufacturers :: Generic RespGetIrdbManufacturers _

derive instance newtypeRespGetIrdbManufacturers :: Newtype RespGetIrdbManufacturers _

derive instance eqRespGetIrdbManufacturers :: Eq RespGetIrdbManufacturers

instance showRespGetIrdbManufacturers :: Show RespGetIrdbManufacturers where
  show = genericShow

instance decodeRespGetIrdbManufacturers :: Decode RespGetIrdbManufacturers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

-- | HTTPメソッドgetをエンドポイント(/api/v1/irdb/manufacturers)に送った結果を得る
getApiV1IrdbManufacturers ::
  ParamGetIrdbManufacturers ->
  Aff (Either String (AX.Response RespGetIrdbManufacturers))
getApiV1IrdbManufacturers param = resp <$> AX.get ResponseFormat.string url
  where
  resp = case _ of
    Right ok -> do
      newBody <- decoder ok.body
      Right $ ok { body = newBody }
    Left err -> Left $ AX.printError err

  url = (unwrap param.baseurl) <> "/api/v1/irdb-manufacturers"
