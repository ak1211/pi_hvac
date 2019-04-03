module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import InfraredCode (Bit(..), InfraredBasebandSignals(..), InfraredHexString, LsbFirst(..), Pulse, fromMilliseconds, infraredBasebandPhase1, infraredHexStringParser, toMilliseconds)
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
  [ {on: 0x7B, off: 0x3D}   -- on 0x007B, off 0x003D
  , {on: 0x0F, off: 0x0F}
  , {on: 0x0F, off: 0x2E}
  , {on: 0x0F, off: 0x0F}
  , {on: 0x0F, off: 0x0F}
  , {on: 0x0F, off: 0x2E}
  , {on: 0x0F, off: 0x2E}
  , {on: 0x0F, off: 0x0F}
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
  [ {on: 0x158, off: 0xAA}
  , {on: 0x19, off: 0x14}
  , {on: 0x19, off: 0x14}
  , {on: 0x1A, off: 0x13}
  , {on: 0x1A, off: 0x14}
  , {on: 0x19, off: 0x14}
  , {on: 0x19, off: 0x14}
  , {on: 0x19, off: 0x3E}
  , {on: 0x1A, off: 0x13}
  , {on: 0x1A, off: 0x3E}
  , {on: 0x1A, off: 0x3D}
  , {on: 0x1A, off: 0x3E}
  , {on: 0x19, off: 0x3E}
  , {on: 0x19, off: 0x3E}
  , {on: 0x1A, off: 0x3E}
  , {on: 0x19, off: 0x14}
  , {on: 0x19, off: 0x3E}
  , {on: 0x1A, off: 0x13}
  , {on: 0x1A, off: 0x3E}
  , {on: 0x19, off: 0x14}
  , {on: 0x19, off: 0x14}
  , {on: 0x19, off: 0x3F}
  , {on: 0x19, off: 0x14}
  , {on: 0x19, off: 0x14}
  , {on: 0x19, off: 0x14}
  , {on: 0x19, off: 0x3E}
  , {on: 0x1A, off: 0x14}
  , {on: 0x19, off: 0x3E}
  , {on: 0x19, off: 0x3E}
  , {on: 0x1A, off: 0x14}
  , {on: 0x19, off: 0x3E}
  , {on: 0x19, off: 0x3E}
  , {on: 0x1A, off: 0x3E}
  , {on: 0x19, off: 0x542}
  ]

expectIRCodeFormat2 :: InfraredBasebandSignals
expectIRCodeFormat2 =
  NEC { customer: LsbFirst  [ Assert,Negate,Assert,Assert
                            , Assert,Assert,Negate,Negate
                            , Negate,Assert,Negate,Negate
                            , Negate,Assert,Negate,Assert
                            ]                             -- binary digit 1011 1100 0100 0101 : TOSHIBA
      , data:     LsbFirst  [ Negate,Negate,Negate,Assert
                            , Negate,Negate,Assert,Negate
                            ]                             -- binary digit 0001 0010 : TV POWER
      , invData:  LsbFirst  [ Assert,Assert,Assert,Negate
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
  [ {on: 344, off: 170}
  , {on: 25, off: 62}
  , {on: 26, off: 19}
  , {on: 26, off: 62}
  , {on: 25, off: 20}
  , {on: 25, off: 20}
  , {on: 25, off: 20}
  , {on: 26, off: 62}
  , {on: 25, off: 20}
  , {on: 26, off: 19}
  , {on: 25, off: 20}
  , {on: 25, off: 63}
  , {on: 25, off: 62}
  , {on: 25, off: 62}
  , {on: 26, off: 62}
  , {on: 25, off: 20}
  , {on: 25, off: 62}
  , {on: 26, off: 20}
  , {on: 25, off: 62}
  , {on: 25, off: 20}
  , {on: 25, off: 20}
  , {on: 26, off: 62}
  , {on: 25, off: 20}
  , {on: 25, off: 20}
  , {on: 25, off: 20}
  , {on: 25, off: 63}
  , {on: 26, off: 19}
  , {on: 25, off: 62}
  , {on: 26, off: 61}
  , {on: 26, off: 20}
  , {on: 25, off: 62}
  , {on: 25, off: 62}
  , {on: 26, off: 62}
  , {on: 25, off: 1346}
  ]

expectIRCodeFormat3 :: InfraredBasebandSignals
expectIRCodeFormat3 =
  NEC { customer: LsbFirst  [ Assert,Negate,Assert,Assert
                            , Assert,Assert,Negate,Negate
                            , Negate,Assert,Negate,Negate
                            , Negate,Assert,Negate,Assert
                            ]                             -- binary digit 1011 1100 0100 0101 : TOSHIBA
      , data:     LsbFirst  [ Negate,Negate,Negate,Assert
                            , Negate,Negate,Assert,Negate
                            ]                             -- binary digit 0001 0010 : TV POWER
      , invData:  LsbFirst  [ Assert,Assert,Assert,Negate
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
  let msec = toMilliseconds 0x7B
  assert' ("expected: 0x7B") (0x7B == fromMilliseconds (Milliseconds 3.2))
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
  parseTest "10002000" [{on: 0x10, off: 0x20}] infraredHexStringParser
  parseTest "1000200030004000" [{on: 0x10, off: 0x20}, {on: 0x30, off: 0x40}] infraredHexStringParser
  --
  log ""
  log "analysis test"
  --
  let tokens =  [ {on: 30, off: 60}
                , {on: 30, off: 30}
                , {on: 30, off: fromMilliseconds (wrap 10.0)}
                , {on: 30, off: 30}
                , {on: 30, off: 60}
                ]
  let value = infraredBasebandPhase1 tokens
  let expected =  [ [{ off: 60, on: 30 }, { off: 30, on: 30 }, { off: 384, on: 30 }]
                  , [{ off: 30, on: 30 }, { off: 60, on: 30 }]
                  ]
  assert' ("expected: " <> show expected) (expected == value)
  --
  parseTest inputIRCode expectIRCode infraredHexStringParser
  --
  parseTest inputIRCode2 expectIRCode2 infraredHexStringParser
