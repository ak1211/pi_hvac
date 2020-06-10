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
module InfraredRemoteCode.Internal
  ( Baseband(..)
  , Bit(..)
  , BitOrder(..)
  , BitStream
  , Celsius(..)
  , Count(..)
  , InfraredCodeFrame(..)
  , InfraredHexString
  , InfraredLeader(..)
  , ProcessError
  , Pulse
  , decodePhase1
  , decodePhase2
  , decodePhase3
  , fromBinaryString
  , fromBoolean
  , fromMilliseconds
  , infraredCodeTextParser
  , showBit
  , toBoolean
  , toInfraredHexString
  , toIrCodeFrames
  , toLsbFirst
  , toMilliseconds
  , toMsbFirst
  , unBitOrder
  ) where

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState)
import Control.Monad.State as State
import Control.MonadZero (guard)
import Data.Array (catMaybes, (..))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Int.Bits ((.&.), shr)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr1)
import Partial.Unsafe (unsafePartial)
import Prelude hiding (between)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, try, (<?>))
import Text.Parsing.Parser.String (char, eof, noneOf, skipSpaces, string)
import Text.Parsing.Parser.Token (digit, hexDigit)
import Utils (toArrayArray, toArrayNonEmptyArray)

-- |
data Bit
  = Negate
  | Assert

derive instance eqBit :: Eq Bit

instance showBit' :: Show Bit where
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
type BitStream
  = NonEmptyArray Bit

-- |
fromBinaryString :: String -> Maybe BitStream
fromBinaryString input =
  let
    xs = map f $ toCharArray input
  in
    do
      guard $ all isJust xs
      NEA.fromArray $ Array.catMaybes xs
  where
  f :: Char -> Maybe Bit
  f '0' = Just Negate

  f '1' = Just Assert

  f _ = Nothing

-- |
data BitOrder
  = LsbFirst Int
  | MsbFirst Int

derive instance eqBitOrder :: Eq BitOrder

-- |
unBitOrder :: BitOrder -> Int
unBitOrder = case _ of
  (LsbFirst x) -> x
  (MsbFirst x) -> x

-- |
shiftRegister :: Int -> Bit -> Int
shiftRegister register input = case input of
  Assert -> register * 2 + 1
  Negate -> register * 2 + 0

-- |
toLsbFirst :: BitStream -> BitOrder
toLsbFirst bits = LsbFirst (Array.foldl shiftRegister 0 $ NEA.reverse bits)

-- |
toMsbFirst :: BitStream -> BitOrder
toMsbFirst bits = MsbFirst (Array.foldl shiftRegister 0 bits)

-- |
data InfraredCodeFrame
  = FormatUnknown (Array Bit)
  | FormatAEHA { octets :: Array BitStream, stop :: Bit }
  | FormatNEC { custom0 :: BitStream, custom1 :: BitStream, data0 :: BitStream, data1 :: BitStream, stop :: Bit }
  | FormatSIRC12 { command :: BitStream, address :: BitStream }
  | FormatSIRC15 { command :: BitStream, address :: BitStream }
  | FormatSIRC20 { command :: BitStream, address :: BitStream, extended :: BitStream }

derive instance genericInfraredCodeFrame :: Generic InfraredCodeFrame _

derive instance eqInfraredCodeFrame :: Eq InfraredCodeFrame

instance showInfraredCodeFrame :: Show InfraredCodeFrame where
  show = genericShow

-- |
newtype Celsius
  = Celsius Int

derive instance newtypeCelsius :: Newtype Celsius _

derive newtype instance eqCelsius :: Eq Celsius

derive newtype instance ordCelsius :: Ord Celsius

derive newtype instance showCelsius :: Show Celsius

-- | input infrared hexadecimal code
type InfraredHexString
  = String

-- |
type ProcessError
  = String

-- |
type Pulse
  = { on :: Count, off :: Count }

-- |
newtype Baseband
  = Baseband (Array Pulse)

derive newtype instance eqBaseband :: Eq Baseband

derive newtype instance showBaseband :: Show Baseband

-- |
toInfraredHexString :: Baseband -> InfraredHexString
toInfraredHexString (Baseband pulses) = String.joinWith "" $ map f pulses
  where
  f :: Pulse -> String
  f x = countToHex x.on <> countToHex x.off

  countToHex :: Count -> String
  countToHex (Count val) =
    let
      hi = (val `shr` 8) .&. 0xff

      lo = val .&. 0xff
    in
      toHex lo <> toHex hi

  toHex :: Int -> String
  toHex x
    | x < 16 = "0" <> Int.toStringAs Int.hexadecimal x
    | otherwise = Int.toStringAs Int.hexadecimal x

-- |
infraredCodeTextParser :: Parser String Baseband
infraredCodeTextParser = formatOnOffPair <|> formatPigpioIrrp

--| 
formatPigpioIrrp :: Parser String Baseband
formatPigpioIrrp = do
  Tuple _ times <- (skipSpaces *> jsonObject)
  eof
  case toPulses times of
    Nothing -> fail "must be on-off pair."
    Just xs -> pure $ Baseband xs
  where
  toPulses :: (Array Int) -> Maybe (Array Pulse)
  toPulses input =
    --
    -- 最後のoffは適当な値を入れる
    --
    let
      xs = map pulse $ toArrayArray 2 $ Array.snoc input (35 * 1000)
    in
      pure $ catMaybes xs
    where
    pulse :: (Array Int) -> Maybe Pulse
    pulse [ on_microsec, off_microsec ] =
      let
        a = Milliseconds (Int.toNumber on_microsec / 1000.0)

        b = Milliseconds (Int.toNumber off_microsec / 1000.0)
      in
        Just { on: fromMilliseconds a, off: fromMilliseconds b }

    pulse _ = Nothing

  jsonObject :: Parser String (Tuple String (Array Int))
  jsonObject = do
    void $ char '{'
    vs <- Array.many (skipSpaces *> hashmap <* sepalator)
    void $ char '}'
    -- todo
    -- 現在の版では要素がいくつあろうとも先頭だけしか取り出さない
    --
    maybe (fail "Empty content.") pure $ Array.head vs

  hashmap :: Parser String (Tuple String (Array Int))
  hashmap = do
    k <- key
    skipSpaces
    void $ char ':'
    skipSpaces
    values <- jsonArray
    pure $ Tuple (fromCharArray k) values
    where
    key :: Parser String (Array Char)
    key = between (string "\"") (string "\"") (Array.some $ noneOf [ '\"' ])

  jsonArray :: Parser String (Array Int)
  jsonArray = do
    void $ char '['
    vs <- Array.some (skipSpaces *> number <* sepalator)
    void $ char ']'
    pure vs
    where
    number = do
      v <- fromCharArray <$> Array.some digit
      case Int.fromStringAs Int.decimal v of
        Nothing -> fail "value is must be numeric."
        Just x -> pure x

  sepalator :: Parser String Unit
  sepalator = do
    skipSpaces
    try (void $ char ',') <|> pure unit

--| 
formatOnOffPair :: Parser String Baseband
formatOnOffPair = do
  xs <- Array.some (pulse <* skipSpaces)
  eof
  pure (Baseband xs)
  where
  pulse :: Parser String Pulse
  pulse = do
    -- 入力値はon -> offの順番
    ton <- valueOf32Bit <?> "on-counts"
    toff <- valueOf32Bit <?> "off-counts"
    pure { on: Count ton, off: Count toff }

  valueOf32Bit :: Parser String Int
  valueOf32Bit = do
    -- 入力値はLower -> Higherの順番
    lower <- hexd16bit <?> "lower-pair hex digit"
    higher <- hexd16bit <?> "higher-pair hex digit"
    -- ここは普通の数字の書き方(位取り記数法: 高位が前, 下位が後)
    let
      str = higher <> lower

      maybeNum = Int.fromStringAs Int.hexadecimal str
    -- 入力値は検査済みなのでfromJustでよい
    pure (unsafePartial $ fromJust maybeNum)

  hexd16bit = do
    a <- hexDigit
    b <- hexDigit
    pure $ fromCharArray [ a, b ]

-- | count is based on 38khz carrier
newtype Count
  = Count Int

derive newtype instance eqCount :: Eq Count

derive newtype instance ordCount :: Ord Count

derive newtype instance showCount :: Show Count

derive newtype instance semiringCount :: Semiring Count

derive newtype instance ringCount :: Ring Count

derive newtype instance commutativeRingCount :: CommutativeRing Count

derive newtype instance eucideanRingCount :: EuclideanRing Count

-- |
withTolerance ::
  { upper :: Milliseconds, lower :: Milliseconds } ->
  Milliseconds ->
  Array Count
withTolerance tolerance typical =
  let
    (Count up) = fromMilliseconds tolerance.upper

    (Count lo) = fromMilliseconds tolerance.lower

    (Count typ) = fromMilliseconds typical
  in
    Count <$> (typ - lo) .. (typ + up)

-- |
fromMilliseconds :: Milliseconds -> Count
fromMilliseconds (Milliseconds msec) =
  let
    period = 0.026 -- 1 / 38kHz
  in
    Count $ Int.floor (msec / period)

-- |
toMilliseconds :: Count -> Milliseconds
toMilliseconds (Count counts) =
  let
    period = 0.026 -- 1 / 38kHz
  in
    Milliseconds (Int.toNumber counts * period)

-- |
data InfraredLeader
  = LeaderAeha Pulse
  | LeaderNec Pulse
  | LeaderSirc Pulse
  | LeaderUnknown Pulse

derive instance genericInfraredLeader :: Generic InfraredLeader _

derive instance eqInfraredLeader :: Eq InfraredLeader

instance showInfraredLeader :: Show InfraredLeader where
  show = genericShow

-- | InfraredLeader data constructor
makeInfraredLeader :: Pulse -> InfraredLeader
makeInfraredLeader = case _ of
  p
    | aeha p -> LeaderAeha p
    | nec p -> LeaderNec p
    | sirc p -> LeaderSirc p
    | otherwise -> LeaderUnknown p
  where
  -- | upper lower tolerance 0.2ms
  typical = withTolerance { upper: Milliseconds 0.2, lower: Milliseconds 0.2 }

  -- | H-level width, typical 3.4ms
  -- | L-level width, typical 1.7ms
  aeha :: Pulse -> Boolean
  aeha pulse =
    let
      on_ = typical (Milliseconds 3.4)

      off_ = typical (Milliseconds 1.7)
    in
      (Array.any (_ == pulse.on) on_) && (Array.any (_ == pulse.off) off_)

  -- | H-level width, typical 9.0ms
  -- | L-level width, typical 4.5ms
  nec :: Pulse -> Boolean
  nec pulse =
    let
      on_ = typical (Milliseconds 9.0)

      off_ = typical (Milliseconds 4.5)
    in
      (Array.any (_ == pulse.on) on_) && (Array.any (_ == pulse.off) off_)

  -- | H-level width, typical 2.4ms
  -- | L-level width, typical 0.6ms
  sirc :: Pulse -> Boolean
  sirc pulse =
    let
      on_ = typical (Milliseconds 2.4)

      off_ = typical (Milliseconds 0.6)
    in
      (Array.any (_ == pulse.on) on_) && (Array.any (_ == pulse.off) off_)

-- |
demodulate :: InfraredLeader -> Array Pulse -> Array Bit
demodulate leader ps = case leader of
  LeaderSirc _ -> map sircModulation ps
  _ -> map pulseDistanceModulation ps
  where
  -- | pulse distance modulation is NEC, AEHA
  pulseDistanceModulation :: Pulse -> Bit
  pulseDistanceModulation = case _ of
    x
      | (Count 2 * x.on) <= x.off -> Assert
      | otherwise -> Negate

  -- | pulse width modulation is SIRC
  sircModulation :: Pulse -> Bit
  sircModulation p =
    let
      threshold =
        withTolerance
          { upper: Milliseconds 0.1, lower: Milliseconds 0.1 }
          (Milliseconds 1.2)
    in
      fromBoolean $ Array.any (_ == p.on) threshold

-- |
toIrCodeFrames :: Baseband -> Either ProcessError (Array InfraredCodeFrame)
toIrCodeFrames = traverse (decodePhase3 <=< decodePhase2) <<< decodePhase1

-- | 入力を各フレームに分ける
decodePhase1 :: Baseband -> Array (Array Pulse)
decodePhase1 (Baseband bb) = unfoldr1 chop bb
  where
  chop :: Array Pulse -> Tuple (Array Pulse) (Maybe (Array Pulse))
  chop xs = case frames xs of
    { init: a, rest: [] } -> Tuple a Nothing
    { init: a, rest: b_ } -> Tuple a (Just b_)

  frames :: Array Pulse -> { init :: Array Pulse, rest :: Array Pulse }
  frames pulses =
    let
      sep = Array.span (\count -> count.off < threshold) pulses
    in
      { init: sep.init <> Array.take 1 sep.rest
      , rest: Array.drop 1 sep.rest
      }

  threshold :: Count
  threshold = fromMilliseconds (Milliseconds 8.0)

-- | 入力フレームをリーダ部とビット配列にする
decodePhase2 :: Array Pulse -> Either ProcessError (Tuple InfraredLeader (Array Bit))
decodePhase2 tokens = case Array.uncons tokens of
  Just { head: x, tail: xs } ->
    let
      leader = makeInfraredLeader x
    in
      Right $ Tuple leader (demodulate leader xs)
  Nothing -> Left "Unexpected end of input"

-- | 入力リーダ部とビット配列から赤外線信号にする
decodePhase3 :: Tuple InfraredLeader (Array Bit) -> Either ProcessError InfraredCodeFrame
decodePhase3 (Tuple leader bitarray) = evalState (runExceptT decoder) bitarray
  where
  decoder :: DecodeMonad ProcessError InfraredCodeFrame
  decoder = case leader of
    LeaderAeha _ -> decodeAehaFormat
    LeaderNec _ -> decodeNecFormat
    LeaderSirc _ -> decodeSircFormat
    LeaderUnknown _ -> decodeUnknownFormat

-- |
type DecodeMonad e a
  = ExceptT e (State (Array Bit)) a

-- |
takeBit :: ProcessError -> DecodeMonad ProcessError Bit
takeBit errmsg = do
  state <- State.get
  case Array.uncons state of
    Nothing -> throwError errmsg
    Just x -> do
      State.put x.tail
      pure x.head

-- |
takeBits :: Int -> ProcessError -> DecodeMonad ProcessError (NonEmptyArray Bit)
takeBits n errmsg = do
  state <- State.get
  let
    bitarray = Array.take n state

    nextState = Array.drop n state
  State.put nextState
  maybe (throwError errmsg) pure do
    guard (Array.length bitarray == n)
    NEA.fromArray bitarray

-- |
takeEnd :: ProcessError -> DecodeMonad ProcessError (NonEmptyArray Bit)
takeEnd errmsg = do
  array <- State.get
  State.put []
  maybe (throwError errmsg) pure $ NEA.fromArray array

-- |
decodeAehaFormat :: DecodeMonad ProcessError InfraredCodeFrame
decodeAehaFormat = do
  data_N <- takeEnd "fail to read: data (AEHA)"
  let
    init = NEA.init data_N

    last = NEA.last data_N

    octets = toArrayNonEmptyArray 8 init
  pure $ FormatAEHA { octets: octets, stop: last }

-- |
decodeNecFormat :: DecodeMonad ProcessError InfraredCodeFrame
decodeNecFormat = do
  custom0 <- takeBits 8 "fail to read: custom code0 (NEC)"
  custom1 <- takeBits 8 "fail to read: custom code1 (NEC)"
  data0 <- takeBits 8 "fail to read: data0 (NEC)"
  data1 <- takeBits 8 "fail to read: data1 (NEC)"
  stopbt <- takeBit "fail to read: stop bit (NEC)"
  pure
    $ FormatNEC
        { custom0: custom0
        , custom1: custom1
        , data0: data0
        , data1: data1
        , stop: stopbt
        }

-- |
decodeSircFormat :: DecodeMonad ProcessError InfraredCodeFrame
decodeSircFormat = do
  comm <- takeBits 7 "fail to read: command code (SIRC)"
  addr <- takeEnd "fail to read: address (SIRC)"
  let
    width = 7 + NEA.length addr
  case width of
    12 -> pure $ FormatSIRC12 { command: comm, address: addr }
    15 -> pure $ FormatSIRC15 { command: comm, address: addr }
    20 ->
      let
        -- 20bits - 7bits = 13 bits あるのでここでは空配列にならないのでfromJustでよい
        first5bits = unsafePartial $ fromJust $ NEA.fromArray $ NEA.take 5 addr

        after5bits = unsafePartial $ fromJust $ NEA.fromArray $ NEA.drop 5 addr
      in
        pure $ FormatSIRC20 { command: comm, address: first5bits, extended: after5bits }
    _ -> throwError ("fail to read: Width " <> show width <> " is not allowed (SIRC)")

-- |
decodeUnknownFormat :: DecodeMonad ProcessError InfraredCodeFrame
decodeUnknownFormat = do
  array <- State.get
  State.put []
  pure $ FormatUnknown array
