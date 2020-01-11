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

module InfraredRemote.Types
  ( Bit(..)
  , BitStream
  , Celsius(..)
  , InfraredCodeFrame(..)
  , LsbFirst(..)
  , MsbFirst(..)
  , fromBoolean
  , showBit
  , toBoolean 
  , toLsbFirst
  , toMsbFirst
  , toStringLsbFirst
  , toStringLsbFirstWithHex
  , toStringMsbFirst
  , toStringMsbFirstWithHex
  , fromBinaryString
  ) where

import Prelude

import Control.MonadZero (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (toCharArray)

-- |
data Bit
  = Negate
  | Assert
derive instance eqBit :: Eq Bit
instance showBit'     :: Show Bit where
  show = showBit

--|
showBit :: Bit -> String
showBit Negate = "0"
showBit Assert = "1"

---|
fromBoolean :: Boolean -> Bit
fromBoolean false = Negate
fromBoolean true = Assert

--|
toBoolean :: Bit -> Boolean
toBoolean Negate = false
toBoolean Assert = true

-- |
type BitStream = NonEmptyArray Bit

-- |
fromBinaryString :: String -> Maybe BitStream
fromBinaryString input =
  let xs = map f $ toCharArray input
  in do
    guard $ all isJust xs
    NEA.fromArray $ Array.catMaybes xs
  where
  f :: Char -> Maybe Bit
  f '0' = Just Negate
  f '1' = Just Assert
  f _ = Nothing

-- |
newtype LsbFirst = LsbFirst Int
derive instance newtypeLsbFirst         :: Newtype LsbFirst _
derive newtype instance eqLsbFirst      :: Eq LsbFirst
derive newtype instance ordLsbFirst     :: Ord LsbFirst
instance showLsbFirst :: Show LsbFirst where
  show (LsbFirst x) =
    "0x" <> Int.toStringAs Int.hexadecimal x <> "(LSBFirst)"

-- |
toStringLsbFirst :: LsbFirst -> String
toStringLsbFirst = Int.toStringAs Int.decimal <<< unwrap

-- |
toStringLsbFirstWithHex :: LsbFirst -> String
toStringLsbFirstWithHex = Int.toStringAs Int.hexadecimal <<< unwrap

-- |
toLsbFirst :: BitStream -> LsbFirst
toLsbFirst bits =
  LsbFirst (Array.foldl f 0 $ NEA.reverse bits)
  where

  f :: Int -> Bit -> Int
  f acc Assert = acc * 2 + 1
  f acc Negate = acc * 2 + 0

-- |
newtype MsbFirst = MsbFirst Int
derive instance newtypeMsbFirst     :: Newtype MsbFirst _
derive newtype instance eqMsbFirst  :: Eq MsbFirst
derive newtype instance ordMsbFirst :: Ord MsbFirst
instance showMsbFirst :: Show MsbFirst where
  show (MsbFirst v) =
    "0x" <> Int.toStringAs Int.hexadecimal v <> "(MSBFirst)"

-- |
toStringMsbFirst :: MsbFirst -> String
toStringMsbFirst = Int.toStringAs Int.decimal <<< unwrap

-- |
toStringMsbFirstWithHex :: MsbFirst -> String
toStringMsbFirstWithHex = Int.toStringAs Int.hexadecimal <<< unwrap

-- |
toMsbFirst :: BitStream -> MsbFirst
toMsbFirst bits =
  MsbFirst (Array.foldl f 0 bits)
  where

  f :: Int -> Bit -> Int
  f acc Assert = acc * 2 + 1
  f acc Negate = acc * 2 + 0

-- |
data InfraredCodeFrame
  = FormatUnknown (Array Bit)
  | FormatAEHA {octets :: Array BitStream, stop :: Bit}
  | FormatNEC  {custom0 :: BitStream, custom1 :: BitStream, data0 :: BitStream, data1 :: BitStream, stop :: Bit}
  | FormatSIRC {command :: BitStream, address :: BitStream}
derive instance genericInfraredCodeFrame  :: Generic InfraredCodeFrame _
derive instance eqInfraredCodeFrame       :: Eq InfraredCodeFrame
instance showInfraredCodeFrame            :: Show InfraredCodeFrame where
  show = genericShow

-- |
newtype Celsius = Celsius Int
derive instance newtypeCelsius      :: Newtype Celsius _
derive newtype instance eqCelsius   :: Eq Celsius
derive newtype instance ordCelsius  :: Ord Celsius
derive newtype instance showCelsius :: Show Celsius
