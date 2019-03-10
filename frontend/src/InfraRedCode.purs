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

module InfraRedCode
  ( toMilliseconds
  , irCodeParser
  , IRCodeToken(..)
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (fromJust)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.Token (hexDigit)

-- | count is based on 38khz carrier
data IRCodeToken = Ton Int | Toff Int | Leftover Int

derive instance genericIRCodeToken :: Generic IRCodeToken _
derive instance eqIRCodeToken :: Eq IRCodeToken
instance showIRCodeToken :: Show IRCodeToken where
  show = genericShow

-- |
toMilliseconds :: Int -> Milliseconds
toMilliseconds counts =
  let freq = 38   -- 38.0 kHz
      msec10x = 10 * counts / freq
  in
  Milliseconds (Int.toNumber msec10x / 10.0)

-- |
irCodeParser :: Parser String (Array IRCodeToken)
irCodeParser =
  Array.concat <$> Array.many (try pair <|> leftover)
  where

  pair = do
    -- 入力値はon -> offの順番
    ton <- valueOf32Bit <?> "ton-counts"
    toff <- valueOf32Bit <?> "toff-counts"
    pure [ Ton ton, Toff toff ]

  leftover = do
    t <- valueOf32Bit <?> "leftover-counts"
    pure [ Leftover t ]

  valueOf32Bit = do
    -- 入力値はLower -> Higherの順番
    lower <- hexd16bit <?> "Lower-pair hex digit"
    higher<- hexd16bit <?> "Higher-pair hex digit"
    -- ここは普通の数字の書き方(位取り記数法: 高位が前, 下位が後)
    let str = higher <> lower
        maybeNum = Int.fromStringAs Int.hexadecimal str
    -- 入力値は検査済みなのでfromJustでよい
    pure (unsafePartial $ fromJust maybeNum)

  hexd16bit = do
    a <- hexDigit
    b <- hexDigit
    pure $ fromCharArray [ a, b ]
