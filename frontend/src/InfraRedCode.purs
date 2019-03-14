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
  ( fromMilliseconds
  , toMilliseconds
  , irCodeParser
  , irCodeSemanticAnalysis
  , analysisPhase1
  , analysisPhase2
  , OnOffCount
  , IRCodeToken(..)
  , CodeBodyAEHA
  , CodeBodyNEC
  , IRCodeEnvelope(..)
  , IRLeader(..)
  , IRSignal(..)
  , IRSignals
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Unfoldable1 (unfoldr1)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.Token (hexDigit)

-- | count is based on 38khz carrier
type OnOffCount = {on :: Int, off :: Int}

-- |
data IRCodeToken = Pulse OnOffCount | Leftover Int

derive instance genericIRCodeToken :: Generic IRCodeToken _
derive instance eqIRCodeToken :: Eq IRCodeToken
instance showIRCodeToken :: Show IRCodeToken where
  show = genericShow

-- |
newtype IRLeader = IRLeader OnOffCount

derive instance newtypeIRLeader :: Newtype IRLeader _
derive instance genericIRLeader :: Generic IRLeader _
derive instance eqIRLeader :: Eq IRLeader
instance showIRLeader :: Show IRLeader where
  show = genericShow

-- |
data IRSignal = Negate | Assert

derive instance eqIRSignal :: Eq IRSignal
instance showIRSignal :: Show IRSignal where
  show Negate = "0"
  show Assert = "1"

-- |
type IRSignals = Array IRSignal

-- |
type CodeBodyAEHA = {customer :: Int, parity :: Int, data0 :: Int, data :: Array Int}

-- |
type CodeBodyNEC = {customer :: Int, data :: Int, invData :: Int}

-- |
data IRCodeEnvelope = AEHA CodeBodyAEHA | NEC CodeBodyNEC | IRCodeEnvelope IRSignals

derive instance genericIRCodeEnvelope :: Generic IRCodeEnvelope _
derive instance eqIRCodeEnvelope :: Eq IRCodeEnvelope
instance showIRCodeEnvelope :: Show IRCodeEnvelope where
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
irCodeParser :: Parser String (Array IRCodeToken)
irCodeParser =
  Array.many (try pair <|> leftover)
  where

  pair = do
    -- 入力値はon -> offの順番
    ton <- valueOf32Bit <?> "on-counts"
    toff <- valueOf32Bit <?> "off-counts"
    pure $ Pulse {on: ton, off: toff}

  leftover = do
    t <- valueOf32Bit <?> "leftover-counts"
    pure $ Leftover t

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
irCodeSemanticAnalysis :: Array IRCodeToken -> Either String IRCodeEnvelope
irCodeSemanticAnalysis =
  analysisPhase2 <=< analysisPhase1

-- |
analysisPhase1 :: Array IRCodeToken -> Either String (Tuple IRLeader IRSignals)
analysisPhase1 tokens =
  case Array.uncons tokens of
    Just {head: x, tail: xs} ->
      maybe (Left "broken code") Right $ go x xs
    Nothing ->
      Left "Unexpected end of input"
  where

  go :: IRCodeToken -> Array IRCodeToken -> Maybe (Tuple IRLeader IRSignals)
  go (Leftover _) _ = Nothing
  go (Pulse p) ps = do
    let sigs = map decodePPM ps
    guard $ Array.all Maybe.isJust sigs
    pure $ Tuple (wrap p) (Array.catMaybes sigs)

  decodePPM :: IRCodeToken -> Maybe IRSignal
  decodePPM = case _ of
    (Pulse p) | 2*p.on <= p.off -> Just Assert
              | otherwise       -> Just Negate
    (Leftover _) -> Nothing

-- |
analysisPhase2 :: Tuple IRLeader IRSignals -> Either String IRCodeEnvelope
analysisPhase2 input =
  case unwrap $ Tuple.fst input of
    p | whenAEHA p  -> aeha
      | whenNEC p   -> nec
      | otherwise   -> other
  where

  -- | typical 3.4ms
  whenAEHA pulse =
    Array.any (_ == pulse.on)
    $ fromMilliseconds (wrap 2.8) .. fromMilliseconds (wrap 4.0)

  -- | typical 9.0ms
  whenNEC pulse =
    Array.any (_ == pulse.on)
    $ fromMilliseconds (wrap 8.5) .. fromMilliseconds (wrap 9.5)

  take :: Int -> Int -> String -> Either String IRSignals
  take begin length errmsg =
    let sigs = Array.slice begin (begin + length) (Tuple.snd input)
    in
    maybe (Left errmsg) Right do
      guard (Array.length sigs == length)
      pure sigs

  takeEnd :: Int -> IRSignals
  takeEnd begin =
    Array.drop (begin - 1) (Tuple.snd input)

  aeha :: Either String IRCodeEnvelope
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
    { customer: deserialize sCustomer
    , parity: deserialize sParity
    , data0: deserialize sData0
    , data: map deserialize octet
    } # (Right <<< AEHA)

  nec :: Either String IRCodeEnvelope
  nec = do
    sCustomer <- take 0 16 "fail to read: customer code (NEC)"
    sData     <- take 16 8 "fail to read: data code (NEC)"
    sInvData  <- take 24 8 "fail to read: inv-data code (NEC)"
    stopBit   <- take 32 1 "fail to read: stop bit (NEC)"
    { customer: deserialize sCustomer
    , data: deserialize sData
    , invData: deserialize sInvData
    } # (Right <<< NEC)

  other :: Either String IRCodeEnvelope
  other =
    Right $ IRCodeEnvelope (Tuple.snd input)

-- |
deserialize :: IRSignals -> Int
deserialize =
  Array.foldl f 0 <<< Array.reverse
  where
  f :: Int -> IRSignal -> Int
  f acc Assert = acc * 2 + 1
  f acc Negate = acc * 2 + 0

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
