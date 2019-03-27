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

module Utils
  ( asiaTokyoDateTime
  , toArray2D
  ) where

import Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Hours(..), fromDuration)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (unfoldr1)

--|
asiaTokyoDateTime :: DateTime -> Maybe {date :: String, time :: String}
asiaTokyoDateTime utc = do
  let offset = fromDuration (Hours 9.0)
  local <- DateTime.adjust offset utc
  dt <- either (const Nothing) Just $ formatDateTime "YYYY-MM-DD HH:mm:ss" local
  chopDateTime dt

-- |
chopDateTime :: String -> Maybe {date :: String, time :: String}
chopDateTime original =
  case String.split (String.Pattern " ") original of
    [a, b] ->
      Just {date: a, time: b}
    
    _ ->
      Nothing

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
