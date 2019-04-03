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

module InfraredCode
  ( Bit(..)
  , InfraredBasebandSignals(..)
  , InfraredHexString
  , InfraredLeader(..)
  , LsbFirst(..)
  , ProcessError
  , Pulse
  , deserialize
  , fromMilliseconds
  , infraredHexStringParser
  , infraredBasebandSignals
  , infraredBasebandPhase1
  , infraredBasebandPhase2
  , infraredBasebandPhase3
  , toMilliseconds
  ) where

import Prelude

import Control.MonadZero (guard)
import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (wrap)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Unfoldable1 (unfoldr1)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Token (hexDigit)
import Utils (toArray2D)

-- | input infrared hexadecimal code
type InfraredHexString = String

-- |
type ProcessError = String

-- | count is based on 38khz carrier
type Pulse = {on :: Int, off :: Int}

-- |
data Bit
  = Negate
  | Assert
derive instance eqBit :: Eq Bit
instance showBit      :: Show Bit where
  show Negate = "0"
  show Assert = "1"

-- |
data InfraredLeader
  = ProtoAeha Pulse 
  | ProtoNec Pulse 
  | ProtoSony Pulse 
  | ProtoUnknown Pulse 
derive instance genericInfraredLeader :: Generic InfraredLeader _
derive instance eqInfraredLeader      :: Eq InfraredLeader
instance showInfraredLeader           :: Show InfraredLeader where
  show = genericShow

-- | InfraredLeader data constructor
makeInfraredLeader :: Pulse -> InfraredLeader 
makeInfraredLeader = case _ of
  p | aeha p    -> ProtoAeha p
    | nec p     -> ProtoNec p
    | sony p    -> ProtoSony p
    | otherwise -> ProtoUnknown p
  where
  -- | H-level width, typical 3.4ms
  -- | L-level width, typical 1.7ms
  aeha :: Pulse -> Boolean
  aeha pulse =
    let on_   = fromMilliseconds (wrap 3.2) .. fromMilliseconds (wrap 3.6)
        off_  = fromMilliseconds (wrap 1.5) .. fromMilliseconds (wrap 1.9)
    in
    (Array.any (_ == pulse.on) on_) && (Array.any (_ == pulse.off) off_)

  -- | H-level width, typical 9.0ms
  -- | L-level width, typical 4.5ms
  nec :: Pulse -> Boolean
  nec pulse =
    let on_   = fromMilliseconds (wrap 8.8) .. fromMilliseconds (wrap 9.2)
        off_  = fromMilliseconds (wrap 4.3) .. fromMilliseconds (wrap 4.7)
    in
    (Array.any (_ == pulse.on) on_) && (Array.any (_ == pulse.off) off_)

  -- | H-level width, typical 2.4ms
  sony :: Pulse -> Boolean
  sony pulse =
    let on_ = fromMilliseconds (wrap 2.2) .. fromMilliseconds (wrap 2.6)
    in
    Array.any (_ == pulse.on) on_

-- |
newtype LsbFirst = LsbFirst (Array Bit)
derive instance genericLsbFirst :: Generic LsbFirst _
derive instance eqLsbFirst      :: Eq LsbFirst
instance showLsbFirst           :: Show LsbFirst where
  show = genericShow

-- |
data InfraredBasebandSignals
  = Unknown LsbFirst
  | AEHA  {customer :: LsbFirst, parity :: LsbFirst, data0 :: LsbFirst, data :: Array LsbFirst}
  | NEC   {customer :: LsbFirst, data :: LsbFirst, invData :: LsbFirst}
  | SONY  {command:: LsbFirst, address :: LsbFirst}
derive instance genericInfraredBasebandSignals  :: Generic InfraredBasebandSignals _
derive instance eqInfraredBasebandSignals       :: Eq InfraredBasebandSignals
instance showInfraredBasebandSignals            :: Show InfraredBasebandSignals where
  show = genericShow

-- |
fromMilliseconds :: Milliseconds -> Int
fromMilliseconds (Milliseconds msec)=
  let freq    = 38    -- 38.0 kHz
      usec    = 1000 / freq
  in
  Int.floor (1000.0 * msec) / usec

-- |
toMilliseconds :: Int -> Milliseconds
toMilliseconds counts =
  let freq    = 38    -- 38.0 kHz
      msec10x = 10 * counts / freq
  in
  Milliseconds (Int.toNumber msec10x / 10.0)

-- |
infraredHexStringParser:: Parser InfraredHexString (Array Pulse)
infraredHexStringParser =
  Array.many pulse
  where

  pulse = do
    -- 入力値はon -> offの順番
    ton <- valueOf32Bit <?> "on-counts"
    toff <- valueOf32Bit <?> "off-counts"
    pure {on: ton, off: toff}

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

-- |
infraredBasebandSignals :: Array Pulse -> Either ProcessError (Array InfraredBasebandSignals)
infraredBasebandSignals =
  traverse (infraredBasebandPhase3 <=< infraredBasebandPhase2) <<< infraredBasebandPhase1 

-- | 入力を各フレームに分ける
infraredBasebandPhase1 :: Array Pulse -> Array (Array Pulse)
infraredBasebandPhase1 =
  unfoldr1 chop
  where

  chop :: Array Pulse -> Tuple (Array Pulse) (Maybe (Array Pulse))
  chop xs = 
    let x = Array.span (\p -> p.off < threshold) xs
    in
    case Array.drop 1 x.rest of
      [] -> Tuple (x.init <> Array.take 1 x.rest) Nothing
      a -> Tuple (x.init <> Array.take 1 x.rest) (Just a)

  threshold = fromMilliseconds (wrap 8.0)

-- | 入力フレームをリーダ部とビット配列にする
infraredBasebandPhase2 :: Array Pulse -> Either ProcessError (Tuple InfraredLeader (Array Bit))
infraredBasebandPhase2 tokens =
  case Array.uncons tokens of
    Just {head: x, tail: xs} ->
      Right $ Tuple (makeInfraredLeader x) (map pulsePositionModulation xs)

    Nothing ->
      Left "Unexpected end of input"
  where

  equal' :: Pulse -> Boolean
  equal' x
    | x.on == x.off = true
    | x.on > x.off = (x.on - x.off) < (x.off / 2)
    | x.off > x.on = (x.off - x.on) < (x.on / 2)
    | otherwise = false

  pulsePositionModulation :: Pulse -> Bit
  pulsePositionModulation p | equal' p  = Negate
                            | otherwise = Assert

-- | 入力リーダ部とビット配列から赤外線信号にする
infraredBasebandPhase3 :: Tuple InfraredLeader (Array Bit) -> Either ProcessError InfraredBasebandSignals
infraredBasebandPhase3 input =
  case Tuple.fst input of
    ProtoAeha _     -> aeha
    ProtoNec _      -> nec 
    ProtoSony _     -> sony
    ProtoUnknown _  -> other
  where

  take :: Int -> Int -> ProcessError -> Either ProcessError (Array Bit)
  take begin length errmsg =
    let sigs = Array.slice begin (begin + length) (Tuple.snd input)
    in
    maybe (Left errmsg) Right do
      guard (Array.length sigs == length)
      pure sigs

  takeEnd :: Int -> Array Bit
  takeEnd begin =
    Array.drop (begin - 1) (Tuple.snd input)

  aeha :: Either ProcessError InfraredBasebandSignals
  aeha = do
    sCustomer <- take 0 16 "fail to read: customer code (AEHA)"
    sParity   <- take 16 4 "fail to read: parity (AEHA)"
    sData0    <- take 20 4 "fail to read: data zero (AEHA)"
    let arr   = takeEnd 24
    sData     <- maybe (Left "fail to read: data (AEHA)") Right
                  $ Array.unsnoc arr
    stopBit   <- maybe (Left "fail to read: stop bit (AEHA)") Right
                  $ guard (sData.last == Assert)
    let octet = toArray2D 8 $ sData.init
    { customer: LsbFirst sCustomer
    , parity: LsbFirst sParity
    , data0: LsbFirst sData0
    , data: map LsbFirst octet
    } # (Right <<< AEHA)

  nec :: Either ProcessError InfraredBasebandSignals
  nec = do
    sCustomer <- take 0 16 "fail to read: customer code (NEC)"
    sData     <- take 16 8 "fail to read: data code (NEC)"
    sInvData  <- take 24 8 "fail to read: inv-data code (NEC)"
    stopBit   <- take 32 1 "fail to read: stop bit (NEC)"
    { customer: LsbFirst sCustomer
    , data: LsbFirst sData
    , invData: LsbFirst sInvData
    } # (Right <<< NEC)

  sony :: Either ProcessError InfraredBasebandSignals
  sony = do
    sCommand <- take 0 6 "fail to read: command code (SONY)"
    let sAddress = takeEnd 7
    { command: LsbFirst sCommand
    , address: LsbFirst sAddress
    } # (Right <<< SONY)

  other :: Either ProcessError InfraredBasebandSignals
  other =
    Right $ Unknown $ LsbFirst (Tuple.snd input)

-- |
deserialize :: LsbFirst -> Int
deserialize (LsbFirst bits) =
  Array.foldl f 0 $ Array.reverse bits
  where
  f :: Int -> Bit -> Int
  f acc Assert = acc * 2 + 1
  f acc Negate = acc * 2 + 0
