module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import InfraRedCode (IRCodeToken(..), irCodeParser, toMilliseconds)
import Test.Assert (assert')
import Text.Parsing.Parser (Parser, parseErrorPosition, runParser)
import Text.Parsing.Parser.Pos (Position(..))

inputIRCode :: String
inputIRCode = String.joinWith ""
  [ "7B00" 
  , "3D00" 
  , "0F00"
  , "0F00"
  , "0F00"
  , "2E00"
  , "0F00"
  , "0F00"
  , "0F00"
  , "0F00"
  , "0F00"
  , "2E00"
  , "0F00"
  , "2E00"
  , "0F00"
  , "0F00"
  ]

expectIRCode :: Array IRCodeToken
expectIRCode =
  [ Ton 0x7B    -- on 0x007B
  , Toff 0x3D   -- off 0x003D
  , Ton 0x0F
  , Toff 0x0F
  , Ton 0x0F
  , Toff 0x2E
  , Ton 0x0F
  , Toff 0x0F
  , Ton 0x0F
  , Toff 0x0F
  , Ton 0x0F
  , Toff 0x2E
  , Ton 0x0F
  , Toff 0x2E
  , Ton 0x0F
  , Toff 0x0F
  ]

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
  log "toMilliseconds test"
  --
  let msec = toMilliseconds 0x7B
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
  parseTest "10002000" [Ton 0x10, Toff 0x20] irCodeParser
  parseTest "100020003000" [Ton 0x10, Toff 0x20, Leftover 0x30] irCodeParser
  parseTest "1000200030004000" [Ton 0x10, Toff 0x20, Ton 0x30, Toff 0x40] irCodeParser
  parseTest inputIRCode expectIRCode irCodeParser
