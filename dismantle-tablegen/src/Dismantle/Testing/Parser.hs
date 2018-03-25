{-# LANGUAGE TupleSections #-}
module Dismantle.Testing.Parser (
  objdumpParser,
  Disassembly(..),
  Section(..),
  Instruction(..),
  InstructionLayout(..),
  Parser
  ) where

import Control.Applicative
import Control.Monad ( replicateM, replicateM_, void )
import qualified Data.ByteString.Lazy as LBS
import           Data.Char ( isHexDigit )
import Data.Monoid ((<>))
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word ( Word8, Word64 )
import qualified Data.Attoparsec.Text.Lazy as P
import Text.Read ( readMaybe )
import Prelude

type Parser = P.Parser

data Disassembly = Disassembly { sections :: [Section] }
  deriving (Show)

data Section = Section { sectionName :: TL.Text
                       , instructions :: [Instruction]
                       }
             deriving (Show)

data Instruction =
    Instruction { insnAddress :: Word64
                , insnBytes :: LBS.ByteString
                , insnText :: TL.Text
                , insnLayout :: InstructionLayout
                }
                deriving (Show)

data InstructionLayout =
    FullWord
    -- ^ The instruction bytes are to be treated as a contiguous
    -- four-byte word.
    | HalfWord
    -- ^ The instruction bytes are to be treated as a single two-byte
    -- half word.
    | HalfWordPair
    -- ^ The instruction bytes are to be treated as a pair of two-byte
    -- half words.
    | SingleByte
    -- ^ Used only by .byte entries.
    deriving (Eq, Read, Show)

objdumpParser :: Parser Disassembly
objdumpParser =
  consumeHeader *> (Disassembly <$> P.many1 parseSection) <* P.endOfInput

consumeLine :: Parser ()
consumeLine = void (P.manyTill P.anyChar P.endOfLine)

consumeHeader :: Parser ()
consumeHeader = replicateM_ 2 consumeLine >> P.skipSpace

parseSectionName :: Parser TL.Text
parseSectionName = TL.pack <$> P.many1 sectionNameChar

tryOne :: [Parser a] -> Parser a
tryOne = P.choice . map P.try

alphaNumChar :: Parser Char
alphaNumChar = tryOne [ P.digit, P.letter ]

oneOf :: [Char] -> P.Parser Char
oneOf cs = P.satisfy (`elem` cs)

sectionNameChar :: Parser Char
sectionNameChar = tryOne [ alphaNumChar
                         , oneOf ['.', '_']
                         ]

-- | Characters that can appear in symbol names, including symbol
-- names that are an offset from another symbol.
symbolNameChar :: Parser Char
symbolNameChar = tryOne [ alphaNumChar
                        , oneOf ['-', '@', '_', '.']
                        ]

parseSection :: Parser Section
parseSection = do
  _ <- P.string (T.pack "Disassembly of section ") P.<?> "Disassembly of section"
  sn <- parseSectionName
  _ <- P.char ':'
  _ <- P.endOfLine
  consumeLine
  insns <- catMaybes <$> P.many1 tryParseInstruction
  P.skipSpace
  return Section { sectionName = sn
                 , instructions = insns
                 }

tryParseInstruction :: Parser (Maybe Instruction)
tryParseInstruction =
  tryOne [ parseAddressOutOfBounds
         , parseEllipses
         , parseFunctionHeading
         , parseJunkData
         , parseInstruction
         ]

parseJunkData :: Parser (Maybe Instruction)
parseJunkData = do
  P.skipMany (P.char ' ')
  void parseAddress
  void $ P.char ':'
  P.skipSpace

  -- We have seen character data lines like these and want to ignore
  -- them:
  --
  -- 80800000:       b8 00 00 ea 14 f0 9f e5 14 f0 9f e5 14 f0 9f e5     <char data>
  -- 8084f118:       20494249 54535953 00020005 00000018     <char data>
  let parseWord = void parseHalfWord >> void parseHalfWord
  tryOne [ replicateM_ 15 (parseByte >> void (P.char ' ')) >> void parseByte
         , replicateM_ 3 (parseWord >> void (P.char ' ')) >> void parseWord
         , parseWord >> void (P.char ' ') >> void parseWord
         ]

  void $ P.manyTill P.anyChar P.endOfLine
  void $ optional P.endOfLine

  return Nothing

ellipses :: Parser T.Text
ellipses = P.string (T.pack "...") P.<?> "ellipses"

parseEllipses :: Parser (Maybe a)
parseEllipses = tryOne [ P.skipSpace >> ellipses >> P.endOfLine >> P.endOfLine >> return Nothing
                       , P.skipSpace >> ellipses >> P.endOfLine >> return Nothing
                       ]

parseInstructionBytes :: Parser ([Word8], InstructionLayout)
parseInstructionBytes =
  tryOne [ parseThumbFull
         , parsePowerPCFull
         , parseARMFull
         , parseThumbHalf
         , (, SingleByte) <$> pure <$> parseByte
         ]

parseHalfWord :: Parser [Word8]
parseHalfWord = replicateM 2 parseByte

parseThumbFull :: Parser ([Word8], InstructionLayout)
parseThumbFull = do
    w1 <- parseHalfWord
    void $ P.char ' '
    w2 <- parseHalfWord
    return (w1 <> w2, HalfWordPair)

parsePowerPCFull :: Parser ([Word8], InstructionLayout)
parsePowerPCFull = do
    b1 <- parseByte
    void $ P.char ' '
    b2 <- parseByte
    void $ P.char ' '
    b3 <- parseByte
    void $ P.char ' '
    b4 <- parseByte
    return ([b1, b2, b3, b4], FullWord)

parseARMFull :: Parser ([Word8], InstructionLayout)
parseARMFull =
    (, FullWord) <$> replicateM 4 parseByte

parseThumbHalf :: Parser ([Word8], InstructionLayout)
parseThumbHalf =
    (, HalfWord) <$> parseHalfWord

-- Objdump sometimes emits these lines for thumb disassemblies.
parseAddressOutOfBounds :: Parser (Maybe Instruction)
parseAddressOutOfBounds = do
  P.skipSpace
  void parseAddress
  void $ P.char ':'
  P.skipSpace
  void (P.string (T.pack "Address 0x") P.<?> "Address 0x")
  void parseAddress
  void (P.string (T.pack " is out of bounds.") P.<?> "is out of bounds")
  void $ replicateM 3 P.endOfLine
  return Nothing

parseInstruction :: Parser (Maybe Instruction)
parseInstruction = do
  P.skipMany (P.char ' ')
  addr <- parseAddress
  _ <- P.char ':'
  P.skipSpace
  (bytes, layout) <- parseInstructionBytes
  P.skipSpace
  txt <- TL.pack <$> P.manyTill P.anyChar P.endOfLine
  _ <- optional P.endOfLine
  case isDataDirective txt || isUndefinedInstruction txt of
    True -> return Nothing
    False ->
      return $ Just Instruction { insnAddress = addr
                                , insnBytes = LBS.pack bytes
                                , insnText = txt
                                , insnLayout = layout
                                }

isDataDirective :: TL.Text -> Bool
isDataDirective t =  or [ TL.pack ".long" `TL.isInfixOf` t
                        , TL.pack ".word" `TL.isInfixOf` t
                        , TL.pack ".short" `TL.isInfixOf` t
                        , TL.pack ".byte" `TL.isInfixOf` t
                        , TL.pack ".inst" `TL.isInfixOf` t
                        ]

isUndefinedInstruction :: TL.Text -> Bool
isUndefinedInstruction t =
    TL.pack "UNDEFINED" `TL.isInfixOf` t ||
    TL.pack "undefined" `TL.isInfixOf` t

-- | These are the markers for where symbols point in the decoded
-- stream.  Even stripped binaries tend to have at least a few (at the
-- start of each section).
parseFunctionHeading :: Parser (Maybe a)
parseFunctionHeading = do
  _ <- parseAddress
  _ <- P.string (T.pack " <") P.<?> "parseFunctionHeading <start>"
  _ <- P.many1 symbolNameChar
  _ <- P.string (T.pack ">:") P.<?> "parseFunctionHeading <end>"
  _ <- P.endOfLine
  return Nothing

parseAddress :: Parser Word64
parseAddress = P.hexadecimal

hexDigitChar :: Parser Char
hexDigitChar = P.satisfy isHexDigit

parseByte :: Parser Word8
parseByte = do
  d1 <- hexDigitChar
  d2 <- hexDigitChar
  case readMaybe ('0' : 'x' : d1 : d2 : []) of
    Just b -> return b
    Nothing -> fail "Invalid byte"
