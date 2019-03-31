module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import InfraRedCode (IRCodeEnvelope(..), IRCodeToken(..), fromMilliseconds, irCodeParser, irCodeSemanticAnalysis, semanticAnalysisPhase1, semanticAnalysisPhase2, toMilliseconds)
import Test.Assert (assert')
import Text.Parsing.Parser (Parser, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser.Pos (Position(..))

inputIRCode :: String
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

expectIRCode :: Array IRCodeToken
expectIRCode =
  [ Pulse {on: 0x7B, off: 0x3D}   -- on 0x007B, off 0x003D
  , Pulse {on: 0x0F, off: 0x0F}
  , Pulse {on: 0x0F, off: 0x2E}
  , Pulse {on: 0x0F, off: 0x0F}
  , Pulse {on: 0x0F, off: 0x0F}
  , Pulse {on: 0x0F, off: 0x2E}
  , Pulse {on: 0x0F, off: 0x2E}
  , Pulse {on: 0x0F, off: 0x0F}
  ]

inputIRCode2 :: String
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

expectIRCode2 :: Array IRCodeToken
expectIRCode2 =
  [ Pulse {on: 0x158, off: 0xAA}
  , Pulse {on: 0x19, off: 0x14}
  , Pulse {on: 0x19, off: 0x14}
  , Pulse {on: 0x1A, off: 0x13}
  , Pulse {on: 0x1A, off: 0x14}
  , Pulse {on: 0x19, off: 0x14}
  , Pulse {on: 0x19, off: 0x14}
  , Pulse {on: 0x19, off: 0x3E}
  , Pulse {on: 0x1A, off: 0x13}
  , Pulse {on: 0x1A, off: 0x3E}
  , Pulse {on: 0x1A, off: 0x3D}
  , Pulse {on: 0x1A, off: 0x3E}
  , Pulse {on: 0x19, off: 0x3E}
  , Pulse {on: 0x19, off: 0x3E}
  , Pulse {on: 0x1A, off: 0x3E}
  , Pulse {on: 0x19, off: 0x14}
  , Pulse {on: 0x19, off: 0x3E}
  , Pulse {on: 0x1A, off: 0x13}
  , Pulse {on: 0x1A, off: 0x3E}
  , Pulse {on: 0x19, off: 0x14}
  , Pulse {on: 0x19, off: 0x14}
  , Pulse {on: 0x19, off: 0x3F}
  , Pulse {on: 0x19, off: 0x14}
  , Pulse {on: 0x19, off: 0x14}
  , Pulse {on: 0x19, off: 0x14}
  , Pulse {on: 0x19, off: 0x3E}
  , Pulse {on: 0x1A, off: 0x14}
  , Pulse {on: 0x19, off: 0x3E}
  , Pulse {on: 0x19, off: 0x3E}
  , Pulse {on: 0x1A, off: 0x14}
  , Pulse {on: 0x19, off: 0x3E}
  , Pulse {on: 0x19, off: 0x3E}
  , Pulse {on: 0x1A, off: 0x3E}
  , Pulse {on: 0x19, off: 0x542}
  ]

expectIRCodeFormat2 :: IRCodeEnvelope
expectIRCodeFormat2 =
  NEC { customer: 0xBF40  -- binary digit 1011 1111 0100 0000 : TOSHIBA
      , data:     0x12    -- binary digit 0001 0010 : TV POWER
      , invData:  0xED    -- binary digit 1110 1101
      }

inputIRCode3 :: String
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

expectIRCode3 :: Array IRCodeToken
expectIRCode3 =
  [ Pulse {on: 344, off: 170}
  , Pulse {on: 25, off: 62}
  , Pulse {on: 26, off: 19}
  , Pulse {on: 26, off: 62}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 26, off: 62}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 26, off: 19}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 25, off: 63}
  , Pulse {on: 25, off: 62}
  , Pulse {on: 25, off: 62}
  , Pulse {on: 26, off: 62}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 25, off: 62}
  , Pulse {on: 26, off: 20}
  , Pulse {on: 25, off: 62}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 26, off: 62}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 25, off: 20}
  , Pulse {on: 25, off: 63}
  , Pulse {on: 26, off: 19}
  , Pulse {on: 25, off: 62}
  , Pulse {on: 26, off: 61}
  , Pulse {on: 26, off: 20}
  , Pulse {on: 25, off: 62}
  , Pulse {on: 25, off: 62}
  , Pulse {on: 26, off: 62}
  , Pulse {on: 25, off: 1346}
  ]

expectIRCodeFormat3 :: IRCodeEnvelope
expectIRCodeFormat3 =
  NEC { customer: 0xBC45  -- binary digit 1011 1100 0100 0101 : TOSHIBA
      , data:     0x12    -- binary digit 0001 0010 : TV POWER
      , invData:  0xED    -- binary digit 1110 1101
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
  parseErrorTestPosition irCodeParser "1x" $ mkPos 2
  parseErrorTestPosition irCodeParser "1+1" $ mkPos 2
  --
  parseErrorTestPosition irCodeParser "1" $ mkPos 2
  parseErrorTestPosition irCodeParser "10" $ mkPos 3
  parseErrorTestPosition irCodeParser "100" $ mkPos 4
  parseTest "1000" [Leftover 0x10] irCodeParser
  --
  parseTest "10002000" [Pulse {on: 0x10, off: 0x20}] irCodeParser
  parseTest "100020003000" [Pulse {on: 0x10, off: 0x20}, Leftover 0x30] irCodeParser
  parseTest "1000200030004000" [Pulse {on: 0x10, off: 0x20}, Pulse {on: 0x30, off: 0x40}] irCodeParser
  --
  parseTest inputIRCode expectIRCode irCodeParser
  --
  parseTest inputIRCode2 expectIRCode2 irCodeParser
  case runParser inputIRCode2 irCodeParser of
    Left a -> log $ parseErrorMessage a
    Right a -> 
      let expected = expectIRCodeFormat2 
          value = irCodeSemanticAnalysis a
      in
      assert' ("expected: " <> show expected) (Right expected == value)
  --
  parseTest inputIRCode3 expectIRCode3 irCodeParser
  case runParser inputIRCode3 irCodeParser of
    Left a -> log $ parseErrorMessage a
    Right a -> 
      let expected = expectIRCodeFormat3 
          value = semanticAnalysisPhase2 =<< semanticAnalysisPhase1 a
      in
      assert' ("expected: " <> show expected) (Right expected == value)
