module Test.Main where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import InfraredCode (Bit(..), Count(..), InfraredBasebandSignals(..), InfraredHexString, LsbFirst(..), Pulse, fromMilliseconds, infraredBasebandPhase1, infraredHexStringParser, toMilliseconds)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert')
import Text.Parsing.Parser (Parser, parseErrorPosition, runParser)
import Text.Parsing.Parser.Pos (Position(..))

inputIRCode :: InfraredHexString
inputIRCode = String.joinWith ""
  [ "7B00", "3D00" 
  , "0F00", "0F00"
  , "0F00", "2E00"
  , "0F00", "0F00"
  , "0F00", "0F00"
  , "0F00", "2E00"
  , "0F00", "2E00"
  , "0F00", "0F00"
  ]

expectIRCode :: Array Pulse 
expectIRCode =
  [ {on: Count 0x7B, off: Count 0x3D}   -- on 0x007B, off 0x003D
  , {on: Count 0x0F, off: Count 0x0F}
  , {on: Count 0x0F, off: Count 0x2E}
  , {on: Count 0x0F, off: Count 0x0F}
  , {on: Count 0x0F, off: Count 0x0F}
  , {on: Count 0x0F, off: Count 0x2E}
  , {on: Count 0x0F, off: Count 0x2E}
  , {on: Count 0x0F, off: Count 0x0F}
  ]

inputIRCode2 :: InfraredHexString
inputIRCode2 = String.joinWith ""
  [ "5801", "AA00"
  , "1900", "1400"
  , "1900", "1400"
  , "1A00", "1300"
  , "1A00", "1400"
  , "1900", "1400"
  , "1900", "1400"
  , "1900", "3E00"
  , "1A00", "1300"
  , "1A00", "3E00"
  , "1A00", "3D00"
  , "1A00", "3E00"
  , "1900", "3E00"
  , "1900", "3E00"
  , "1A00", "3E00"
  , "1900", "1400"
  , "1900", "3E00"
  , "1A00", "1300"
  , "1A00", "3E00"
  , "1900", "1400"
  , "1900", "1400"
  , "1900", "3F00"
  , "1900", "1400"
  , "1900", "1400"
  , "1900", "1400"
  , "1900", "3E00"
  , "1A00", "1400"
  , "1900", "3E00"
  , "1900", "3E00"
  , "1A00", "1400"
  , "1900", "3E00"
  , "1900", "3E00"
  , "1A00", "3E00"
  , "1900", "4205"
  ]

expectIRCode2 :: Array Pulse
expectIRCode2 =
  [ {on: Count 0x158, off: Count 0xAA}
  , {on: Count 0x19, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x14}
  , {on: Count 0x1A, off: Count 0x13}
  , {on: Count 0x1A, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x3E}
  , {on: Count 0x1A, off: Count 0x13}
  , {on: Count 0x1A, off: Count 0x3E}
  , {on: Count 0x1A, off: Count 0x3D}
  , {on: Count 0x1A, off: Count 0x3E}
  , {on: Count 0x19, off: Count 0x3E}
  , {on: Count 0x19, off: Count 0x3E}
  , {on: Count 0x1A, off: Count 0x3E}
  , {on: Count 0x19, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x3E}
  , {on: Count 0x1A, off: Count 0x13}
  , {on: Count 0x1A, off: Count 0x3E}
  , {on: Count 0x19, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x3F}
  , {on: Count 0x19, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x3E}
  , {on: Count 0x1A, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x3E}
  , {on: Count 0x19, off: Count 0x3E}
  , {on: Count 0x1A, off: Count 0x14}
  , {on: Count 0x19, off: Count 0x3E}
  , {on: Count 0x19, off: Count 0x3E}
  , {on: Count 0x1A, off: Count 0x3E}
  , {on: Count 0x19, off: Count 0x542}
  ]

expectIRCodeFormat2 :: InfraredBasebandSignals
expectIRCodeFormat2 =
  NEC { customer0: LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                    [ Assert,Negate,Assert,Assert
                    , Assert,Assert,Negate,Negate
                    ]
      , customer1: LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                    [ Negate,Assert,Negate,Negate
                    , Negate,Assert,Negate,Assert
                    ]                             -- binary digit 1011 1100 0100 0101 : TOSHIBA
      , data: LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                    [ Negate,Negate,Negate,Assert
                    , Negate,Negate,Assert,Negate
                    ]                             -- binary digit 0001 0010 : TV POWER
      , invData: LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                    [ Assert,Assert,Assert,Negate
                    , Assert,Assert,Negate,Assert
                    ]                             -- binary digit 1110 1101
      }

inputIRCode3 :: InfraredHexString
inputIRCode3 = String.joinWith ""
  [ "5801", "AA00"
  , "1900", "3E00"
  , "1A00", "1300"
  , "1A00", "3E00"
  , "1900", "1400"
  , "1900", "1400"
  , "1900", "1400"
  , "1A00", "3E00"
  , "1900", "1400"
  , "1A00", "1300"
  , "1900", "1400"
  , "1900", "3F00"
  , "1900", "3E00"
  , "1900", "3E00"
  , "1A00", "3E00"
  , "1900", "1400"
  , "1900", "3E00"
  , "1A00", "1400"
  , "1900", "3E00"
  , "1900", "1400"
  , "1900", "1400"
  , "1A00", "3E00"
  , "1900", "1400"
  , "1900", "1400"
  , "1900", "1400"
  , "1900", "3F00"
  , "1A00", "1300"
  , "1900", "3E00"
  , "1A00", "3D00"
  , "1A00", "1400"
  , "1900", "3E00"
  , "1900", "3E00"
  , "1A00", "3E00"
  , "1900", "4205"
  ]

expectIRCode3 :: Array Pulse
expectIRCode3 =
  [ {on: Count 344, off: Count 170}
  , {on: Count 25, off: Count 62}
  , {on: Count 26, off: Count 19}
  , {on: Count 26, off: Count 62}
  , {on: Count 25, off: Count 20}
  , {on: Count 25, off: Count 20}
  , {on: Count 25, off: Count 20}
  , {on: Count 26, off: Count 62}
  , {on: Count 25, off: Count 20}
  , {on: Count 26, off: Count 19}
  , {on: Count 25, off: Count 20}
  , {on: Count 25, off: Count 63}
  , {on: Count 25, off: Count 62}
  , {on: Count 25, off: Count 62}
  , {on: Count 26, off: Count 62}
  , {on: Count 25, off: Count 20}
  , {on: Count 25, off: Count 62}
  , {on: Count 26, off: Count 20}
  , {on: Count 25, off: Count 62}
  , {on: Count 25, off: Count 20}
  , {on: Count 25, off: Count 20}
  , {on: Count 26, off: Count 62}
  , {on: Count 25, off: Count 20}
  , {on: Count 25, off: Count 20}
  , {on: Count 25, off: Count 20}
  , {on: Count 25, off: Count 63}
  , {on: Count 26, off: Count 19}
  , {on: Count 25, off: Count 62}
  , {on: Count 26, off: Count 61}
  , {on: Count 26, off: Count 20}
  , {on: Count 25, off: Count 62}
  , {on: Count 25, off: Count 62}
  , {on: Count 26, off: Count 62}
  , {on: Count 25, off: Count 1346}
  ]

expectIRCodeFormat3 :: InfraredBasebandSignals
expectIRCodeFormat3 =
  NEC { customer0: LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                    [ Assert,Negate,Assert,Assert
                    , Assert,Assert,Negate,Negate
                    ]
      , customer1: LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                    [ Negate,Assert,Negate,Negate
                    , Negate,Assert,Negate,Assert
                    ]                             -- binary digit 1011 1100 0100 0101 : TOSHIBA
      , data:     LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                    [ Negate,Negate,Negate,Assert
                    , Negate,Negate,Assert,Negate
                    ]                             -- binary digit 0001 0010 : TV POWER
      , invData:  LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                    [ Assert,Assert,Assert,Negate
                    , Assert,Assert,Negate,Assert
                    ]                             -- binary digit 1110 1101
      }

parseTest :: forall s a. Show a => Eq a => s -> a -> Parser s a -> Effect Unit
parseTest input expected p = case runParser input p of
  Right actual -> do
    assert' ("expected: " <> show expected <> ", actual: " <> show actual) (expected == actual)
    logShow actual
  Left err -> assert' ("error: " <> show err) false

parseErrorTestPosition :: forall s a. Show a => Parser s a -> s -> Position -> Effect Unit
parseErrorTestPosition p input expected = case runParser input p of
  Right _ -> assert' "error: ParseError expected!" false
  Left err -> do
    let pos = parseErrorPosition err
    assert' ("expected: " <> show expected <> ", pos: " <> show pos) (expected == pos)
    logShow expected

mkPos :: Int -> Position
mkPos n = mkPos' n 1

mkPos' :: Int -> Int -> Position
mkPos' column line = Position { column: column, line: line }

main :: Effect Unit
main = do
  log "from, toMilliseconds test"
  --
  let msec = toMilliseconds (Count 0x7B)
  assert' ("expected: 0x7B") (Count 0x7B == fromMilliseconds (Milliseconds 3.2))
  assert' ("expected: 3.2 msec ") (msec == Milliseconds 3.2)
  log $ "count 0x7B is " <> show (unwrap msec) <> " ms"
  --
  log ""
  log "parser test"
  parseErrorTestPosition infraredHexStringParser "1x" $ mkPos 2
  parseErrorTestPosition infraredHexStringParser "1+1" $ mkPos 2
  --
  parseErrorTestPosition infraredHexStringParser "1" $ mkPos 2
  parseErrorTestPosition infraredHexStringParser "10" $ mkPos 3
  parseErrorTestPosition infraredHexStringParser "100" $ mkPos 4
  parseErrorTestPosition infraredHexStringParser "1000" $ mkPos 5
  parseErrorTestPosition infraredHexStringParser "10002" $ mkPos 6
  parseErrorTestPosition infraredHexStringParser "100020" $ mkPos 7
  parseErrorTestPosition infraredHexStringParser "1000200" $ mkPos 8
  --
  parseTest "10002000" [{on: Count 0x10, off: Count 0x20}] infraredHexStringParser
  parseTest "1000200030004000" [{on: Count 0x10, off: Count 0x20}, {on: Count 0x30, off: Count 0x40}] infraredHexStringParser
  --
  log ""
  log "analysis test"
  --
  let tokens =  [ {on: Count 30, off: Count 60}
                , {on: Count 30, off: Count 30}
                , {on: Count 30, off: fromMilliseconds (wrap 10.0)}
                , {on: Count 30, off: Count 30}
                , {on: Count 30, off: Count 60}
                ]
  let value = infraredBasebandPhase1 tokens
  let expected =  [ [{ off: Count 60, on: Count 30 }, { off: Count 30, on: Count 30 }, { off: Count 384, on: Count 30 }]
                  , [{ off: Count 30, on: Count 30 }, { off: Count 60, on: Count 30 }]
                  ]
  assert' ("expected: " <> show expected) (expected == value)
  --
  parseTest inputIRCode expectIRCode infraredHexStringParser
  --
  parseTest inputIRCode2 expectIRCode2 infraredHexStringParser
