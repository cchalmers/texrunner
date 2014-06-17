{-# LANGUAGE OverloadedStrings #-}
module System.TeXRunner.Parse
  ( someError
  , badBox
  , parseBox
  , parseUnit
  , parseLog
  , Box (..)
  , TeXLog (..)
  ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8            (ByteString)
import Data.Maybe
import Data.Monoid
import Debug.Trace

data TeXLog = TeXLog
  { thisis    :: Maybe ByteString
  , numPages  :: Maybe Int
  , texErrors :: [TeXError]
  } deriving Show

instance Monoid TeXLog where
  mempty = TeXLog Nothing Nothing []
  (TeXLog prog pages1 errors1) `mappend` (TeXLog _ pages2 errors2) =
    case (pages1,pages2) of
      (Just a,_) -> TeXLog prog (Just a) (errors1 ++ errors2)
      (_,b)      -> TeXLog prog b (errors1 ++ errors2)

logFile :: Parser TeXLog
logFile = mconcat <$> many logLine
  where
    logLine = do
      prog   <- optional $ "This is " *> restOfLine
      pages  <- optional nPages
      errors <- maybeToList <$> optional someError
      _      <- restOfLine
      return $ TeXLog prog pages errors

parseLog :: ByteString -> Either String TeXLog
parseLog = parseOnly logFile


-- * Boxes

-- | Data type for holding dimensions of a hbox.
data Box = Box
  { boxHeight :: Double
  , boxDepth  :: Double
  , boxWidth  :: Double
  } deriving Show

int :: Parser Int
int = decimal

parseBox :: Parser Box
parseBox = do
  A.skipWhile (/='\\') <* char '\\'
  parseSingle <|> parseBox
  where
    parseSingle = do
      _ <- "box" *> int *> "=\n\\hbox("
      h <- double <* char '+'
      d <- double <* ")x"
      w <- double
      --
      return $ Box (pt2bp h) (pt2bp d) (pt2bp w)

parseUnit :: Parser Double
parseUnit = do
  A.skipWhile (/='>') <* char '>'
  skipSpace
  fmap pt2bp double <|> parseUnit

pt2bp :: Double -> Double
pt2bp = (/1.00374)

-- * Errors

data TeXError
  = UndefinedControlSequence ByteString
  | MissingNumber
  | Missing Char
  | IllegalUnit (Maybe Char) (Maybe Char)
  | PackageError String String
  | LaTeXError ByteString
  | BadBox ByteString
  | EmergencyStop
  | ParagraphEnded
  | TooManyErrors
  | NumberTooBig
  | ExtraBrace
  | FatalError ByteString
  | UnknownError ByteString
  deriving (Show)

someError :: Parser TeXError
someError =  "! " *> errors
  where
    errors =  undefinedControlSequence
          <|> illegalUnit
          <|> missingNumber
          <|> missing
          <|> latexError
          <|> emergencyStop
          <|> extraBrace
          <|> paragraphEnded
          <|> numberTooBig
          <|> tooManyErrors
          <|> fatalError
          <|> UnknownError <$> restOfLine

noteStar :: Parser ()
noteStar = skipSpace *> "<*>" *> skipSpace

toBeReadAgain :: Parser Char
toBeReadAgain = do
  skipSpace
  _ <- "<to be read again>"
  skipSpace
  anyChar

-- insertedText :: Parser ByteString
-- insertedText = do
--   skipSpace
--   _ <- "<inserted text>"
--   skipSpace
--   restOfLine

-- General errors

undefinedControlSequence :: Parser TeXError
undefinedControlSequence = do
  _ <- "Undefined control sequence."
  _ <- optional noteStar
  skipSpace
  l <- optional line
  skipSpace
  UndefinedControlSequence <$> traceShow l takeTill isSpace

illegalUnit :: Parser TeXError
illegalUnit = do
  _ <- "Illegal unit of measure (pt inserted)."
  a <- optional toBeReadAgain
  b <- optional toBeReadAgain
  noteStar
  return $ IllegalUnit a b

missingNumber :: Parser TeXError
missingNumber = do
  _ <- "Missing number, treated as zero."
  _ <- toBeReadAgain
  noteStar
  return MissingNumber

badBox :: Parser TeXError
badBox = do
  s <- choice ["Underfull", "Overfull", "Tight", "Loose"]
  _ <- " \\hbox " *> char '(' *> takeTill (==')') <* char ')'
  _ <- optional line
  return $ BadBox s

missing :: Parser TeXError
missing = do
  c <- "Missing " *> anyChar <* " inserted."
  _ <- optional line
  return $ Missing c

line :: Parser Int
line =  " detected at line " *> decimal
    <|> "l."                 *> decimal

emergencyStop :: Parser TeXError
emergencyStop = "Emergency stop."
             *> return EmergencyStop

fatalError :: Parser TeXError
fatalError = FatalError <$> (" ==> Fatal error occurred, " *> restOfLine)

-- line 8058 tex.web
extraBrace :: Parser TeXError
extraBrace = "Argument of" *> return ExtraBrace

tooManyErrors :: Parser TeXError
tooManyErrors = "That makes 100 errors; please try again."
             *> return TooManyErrors

-- line 8075 tex.web
paragraphEnded :: Parser TeXError
paragraphEnded = do
  _ <- "Paragraph ended before "
  _ <- takeTill isSpace
  _ <- toBeReadAgain
  _ <- line
  return ParagraphEnded

numberTooBig :: Parser TeXError
numberTooBig = "Number too big" *> return NumberTooBig

-- LaTeX errors

latexError :: Parser TeXError
latexError = do
  _ <- "LaTeX Error: "
  LaTeXError <$> takeTill isSpace

-- Pages

nPages :: Parser Int
nPages = "Output written on "
      *> skipWhile (/= '(') *> char '('
      *> decimal

-- Utilities

restOfLine :: Parser ByteString
restOfLine = takeTill (=='\n') <* char '\n'

