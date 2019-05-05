module Test.Main where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Newtype (wrap)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import InfraredCode (Baseband(..), Bit(..), Count(..), InfraredCode(..), InfraredHexString, LsbFirst(..), decodeBaseband, decodePhase1, fromMilliseconds, infraredHexStringParser, toMilliseconds)
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

expectIRCode :: Baseband
expectIRCode =
  Baseband
  [ {on: Count 0x7B, off: Count 0x3D}   -- on 0x007B, off 0x003D
  , {on: Count 0x0F, off: Count 0x0F}
  , {on: Count 0x0F, off: Count 0x2E}
  , {on: Count 0x0F, off: Count 0x0F}
  , {on: Count 0x0F, off: Count 0x0F}
  , {on: Count 0x0F, off: Count 0x2E}
  , {on: Count 0x0F, off: Count 0x2E}
  , {on: Count 0x0F, off: Count 0x0F}
  ]

inputIRCode3 :: InfraredHexString
inputIRCode3 =
  "5901A9001A003D00190014001B003D0019001400190014001A00130019003F0019001400190014001A00130019003E001B003D0019003E001A003E00190014001A003D00190014001A003E00190014001900140019003E001B0013001A0013001A0013001A003D001A0013001B003D0019003E001A0013001A003E001A003D0019003E001B004205"

expectIRCodeFormat3 :: InfraredCode
expectIRCodeFormat3 = NEC
  { custom: LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                [ Assert,Negate,Assert,Negate, Negate,Negate,Assert,Negate
                , Negate,Negate,Assert,Assert, Assert,Assert,Negate,Assert
                ]                             -- binary digit 1010 0010 0011 1101 : TOSHIBA
  , data:     LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                [ Negate,Assert,Negate,Negate, Assert,Negate,Negate,Negate
                ]                             -- binary digit 0100 1000 : TV POWER
  , invData:  LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                [ Assert,Negate,Assert,Assert, Negate,Assert,Assert,Assert
                ]                             -- binary digit 1011 0111
  , stop: Assert
  }

inputIRCode4 :: InfraredHexString
inputIRCode4 =
  "8500430013001100120032001100110012001000120012001200110012001000120011001300100012001200110010001300110012001100120032001100110011001100120012001200110012001000130011001200100012001100120010001300320012001000130011001200100012001000130010001200120012001000130010001300310012001100120032001200320012003100120033001200110012001000130031001200100013003200110033001200310011003300120010001300320012004F03"

expectIRCodeFormat4 :: InfraredCode
expectIRCodeFormat4 = AEHA
  { custom: LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                [ Negate,Assert,Negate,Negate, Negate,Negate,Negate,Negate
                , Negate,Negate,Negate,Negate, Negate,Assert,Negate,Negate ]
                -- binary digit 0100 0000 0000 0100 : PANASONIC
  , parity: LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                [ Negate,Negate,Negate,Negate ]
                -- binary digit 0000
  , data0: LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                [ Negate,Negate,Negate,Assert ]
                -- binary digit 0001
  , data: [ LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                [ Negate,Negate,Negate,Negate, Negate,Negate,Negate,Negate ]
          , LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                [ Assert,Negate,Assert,Assert, Assert,Assert,Negate,Negate ]
          , LsbFirst $ unsafePartial $ fromJust $ NEA.fromArray
                [ Assert,Negate,Assert,Assert, Assert,Assert,Negate,Assert ]
          ]
          -- binary digit 0000 0000 / 1011 1100 / 1011 1101: TV POWER
  , stop: Assert
  }

parseTest :: forall s a. Show a => Eq a => s -> a -> Parser s a -> Effect Unit
parseTest input expected p = case runParser input p of
  Right actual -> do
    assert' ("expected: " <> show expected <> ", actual: " <> show actual) (expected == actual)
    logShow actual
  Left err -> assert' ("error: " <> show err) false

bbsignalsTest :: InfraredHexString -> Array InfraredCode -> Effect Unit
bbsignalsTest input expected =
  lmap show (runParser input infraredHexStringParser)
  >>= decodeBaseband
  # case _ of
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
  let count = fromMilliseconds (Milliseconds 3.2)
  log $ show count
  assert' ("expected: 123") (Count 123 == count)
  let msec = toMilliseconds count
  log $ show msec
  let count' = fromMilliseconds msec
  log $ show count'
  assert' ("expected: 123") (Count 123 == count')

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
  parseTest "10002000" (Baseband [{on: Count 0x10, off: Count 0x20}]) infraredHexStringParser
  parseTest "1000200030004000" (Baseband [{on: Count 0x10, off: Count 0x20}, {on: Count 0x30, off: Count 0x40}]) infraredHexStringParser
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
  let value = decodePhase1 (Baseband tokens)
  let expected =  [ [{ off: Count 60, on: Count 30 }, { off: Count 30, on: Count 30 }, { off: Count 384, on: Count 30 }]
                  , [{ off: Count 30, on: Count 30 }, { off: Count 60, on: Count 30 }]
                  ]
  assert' ("expected: " <> show expected) (expected == value)
  --
  parseTest inputIRCode expectIRCode infraredHexStringParser
  --
  bbsignalsTest inputIRCode3 [expectIRCodeFormat3]
  bbsignalsTest inputIRCode4 [expectIRCodeFormat4]
