{-# LANGUAGE OverloadedStrings #-}
module System.TeXRunner.Parse
  ( someError 
  , badBox
  , parseBox
  , parseLog
  , Box (..)
  , TeXLog (..)
  ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as B
import Data.Maybe
import           Debug.Trace
import           Data.Monoid

data TeXLog = TeXLog
  { thisis    :: Maybe ByteString
  , numPages  :: Maybe Int
  , texErrors :: [TeXError]
  } deriving Show

instance Monoid TeXLog where
  mempty = TeXLog Nothing Nothing []
  (TeXLog thisis pages1 errors1)
    `mappend`
    (TeXLog _ pages2 errors2) = case (pages1,pages2) of
                                   (Just a,_) -> TeXLog thisis (Just a) (errors1 ++ errors2)
                                   (_,b)      -> TeXLog thisis b (errors1 ++ errors2)

logFile = mconcat <$> many logLine

logLine = do
  thisis <- optional $ "This is " *> restOfLine
  pages  <- optional nPages
  errors <- maybeToList <$> optional someError
  _      <- restOfLine
  return $ TeXLog thisis pages errors

parseLog = parseOnly logFile


p = parseOnly someError

-- * Boxes


-- \setbox0=\hbox{<<string>>}
-- \showbox0
-- >   *\showbox0
-- >   > \box0=
------------ height  depth    width
-- >   \hbox(6.83331+2.15277)x18.6108
-- >   .\tenrm T
-- >   .\kern -1.66702
-- >   .\hbox(6.83331+0.0)x6.80557, shifted 2.15277
-- >   ..\tenrm E
-- >   .\kern -1.25
-- >   .\tenrm X
-- >
-- >   ! OK.
-- >   <*> \showbox0
-- >
-- >   ?
--

-- | Data type for holding dimensions of a hbox.
data Box = Box
  { boxHeight :: Double
  , boxDepth  :: Double
  , boxWidth  :: Double
  } deriving Show

parseBox :: Parser Box
parseBox = do
  A.skipWhile (/='\\') <* char '\\'
  parseSingle <|> parseBox
  where
    parseSingle = do
      _ <- "hbox("
      h <- double <* char '+'
      d <- double <* ")x"
      w <- double
      --
      return $ Box (h/8) (d/8) (w/8)

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

insertedText :: Parser ByteString
insertedText = do
  skipSpace
  _ <- "<inserted text>"
  skipSpace
  restOfLine

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

