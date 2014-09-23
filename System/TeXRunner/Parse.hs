{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

----------------------------------------------------------------------------
-- |
-- Module      :  System.TeXRunner
-- Copyright   :  (c) 2014 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- Functions for parsing TeX output and logs.
--
-----------------------------------------------------------------------------

module System.TeXRunner.Parse
  ( -- * Box
    Box (..)
  , parseBox
    -- * Errors
  , TeXLog (..)
  , TeXInfo (..)
  , TeXError (..)
  , TeXError' (..)
  , someError
  , badBox
  , parseUnit
  , parseLog
  , prettyPrintLog
  ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString.Char8            (ByteString, cons, pack)
import qualified Data.ByteString.Char8            as B
import           Data.Maybe
import           Data.Monoid

-- Everything's done using ByteString because io-streams' attoparsec module 
-- only has a ByteString function. It's very likely this will all change to 
-- Text soon.

data TeXLog = TeXLog
  { texInfo   :: TeXInfo
  , numPages  :: Maybe Int
  , texErrors :: [TeXError]
  } deriving Show

data TeXInfo = TeXInfo
  { texCommand      :: Maybe ByteString
  , texVersion      :: Maybe ByteString
  , texDistribution :: Maybe ByteString
  -- , texDate    :: Maybe Date
  }
  deriving Show

instance Monoid TeXLog where
  mempty = TeXLog (TeXInfo Nothing Nothing Nothing) Nothing []
  (TeXLog prog pages1 errors1) `mappend` (TeXLog _ pages2 errors2) =
    case (pages1,pages2) of
      (Just a,_) -> TeXLog prog (Just a) (errors1 ++ errors2)
      (_,b)      -> TeXLog prog b (errors1 ++ errors2)

infoParser :: Parser TeXInfo
infoParser
  = TeXInfo
  <$> optional ("This is"   *> takeTill (== ',') <* anyChar)
  <*> optional (" Version " *> takeTill (== ' ') <* anyChar)
  <*> optional (char '('    *> takeTill (== ')') <* anyChar)
  -- <*> Nothing

logFile :: Parser TeXLog
logFile = mconcat <$> many logLine
  where
    logLine = do
      info   <- infoParser
      pages  <- optional nPages
      errors <- maybeToList <$> optional someError
      _      <- restOfLine
      return $ TeXLog info pages errors

-- thisIs :: Parser TeXVersion

parseLog :: ByteString -> TeXLog
parseLog = (\(Right a) -> a) . parseOnly logFile
-- the parse should never fail (I think)


prettyPrintLog :: TeXLog -> ByteString
prettyPrintLog (TeXLog {..}) =
  fromMaybe "unknown program" (texCommand texInfo)
  <> maybe "" (" version " <>) (texVersion texInfo)
  <> maybe "" (" " <>) (texDistribution texInfo)
  <> "\n"
  <> maybe "" ((<> "pages\n") . pack . show) numPages
  <> B.unlines (map (pack . show) texErrors)

-- * Boxes

-- | Data type for holding dimensions of a hbox.
data Box n = Box
  { boxHeight :: n
  , boxDepth  :: n
  , boxWidth  :: n
  } deriving Show

int :: Parser Int
int = decimal

parseBox :: Fractional n => Parser (Box n)
parseBox = do
  A.skipWhile (/='\\') <* char '\\'
  parseSingle <|> parseBox
  where
    parseSingle = do
      _ <- "box" *> int <* "=\n\\hbox("
      h <- rational <* char '+'
      d <- rational <* ")x"
      w <- rational
      --
      return $ Box (pt2bp h) (pt2bp d) (pt2bp w)

parseUnit :: Fractional n => Parser n
parseUnit = do
  A.skipWhile (/='>') <* char '>'
  skipSpace
  fmap pt2bp rational <|> parseUnit

pt2bp :: Fractional n => n -> n
pt2bp = (/1.00374)

-- * Errors

-- | An error from tex with possible line number.
data TeXError = TeXError
  { errorLine :: Maybe Int
  , error'    :: TeXError'
  }
  deriving Show

instance Eq TeXError where
  TeXError _ a == TeXError _ b = a == b

-- | A subset of possible error TeX can throw.
data TeXError'
  = UndefinedControlSequence ByteString
  | MissingNumber
  | Missing Char
  | IllegalUnit -- (Maybe Char) (Maybe Char)
  | PackageError String String
  | LaTeXError ByteString
  | BadBox ByteString
  | EmergencyStop
  | ParagraphEnded
  | TooMany ByteString
  | DimensionTooLarge
  | TooManyErrors
  | NumberTooBig
  | ExtraBrace
  | FatalError ByteString
  | UnknownError ByteString
  deriving (Show, Read, Eq)

-- Parse any line begining with "! ". Any unknown errors are returned as 'UnknowError'.
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
          <|> tooMany
          <|> dimentionTooLarge
          <|> tooManyErrors
          <|> fatalError
          <|> TeXError Nothing <$> UnknownError <$> restOfLine

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

  _ <- optional $ do -- for context log
    skipSpace
    _ <- "system"
    let skipLines = line <|> restOfLine *> skipLines
    skipLines

  _ <- optional noteStar
  skipSpace
  l <- optional line
  skipSpace
  cs <- finalControlSequence
  return $ TeXError l (UndefinedControlSequence cs)

finalControlSequence :: Parser ByteString
finalControlSequence = last <$> many1 controlSequence
  where
    controlSequence = cons '\\' <$>
      (char '\\' *> takeTill (\x -> isSpace x || x=='\\'))

illegalUnit :: Parser TeXError
illegalUnit = do
  _ <- "Illegal unit of measure (pt inserted)."
  _ <- optional toBeReadAgain
  _ <- optional toBeReadAgain

  return $ TeXError Nothing IllegalUnit

missingNumber :: Parser TeXError
missingNumber = do
  _ <- "Missing number, treated as zero."
  _ <- optional toBeReadAgain
  _ <- optional noteStar
  return $ TeXError Nothing MissingNumber

badBox :: Parser TeXError
badBox = do
  s <- choice ["Underfull", "Overfull", "Tight", "Loose"]
  _ <- " \\hbox " *> char '(' *> takeTill (==')') <* char ')'
  l <- optional line
  return $ TeXError l (BadBox s)

missing :: Parser TeXError
missing = do
  c <- "Missing " *> anyChar <* " inserted."
  l <- optional line
  return $ TeXError l (Missing c)

line :: Parser Int
line =  " detected at line " *> decimal
    <|> "l."                 *> decimal

emergencyStop :: Parser TeXError
emergencyStop = "Emergency stop."
             *> return (TeXError Nothing EmergencyStop)

fatalError :: Parser TeXError
fatalError = TeXError Nothing <$> FatalError <$> (" ==> Fatal error occurred, " *> restOfLine)

-- line 8058 tex.web
extraBrace :: Parser TeXError
extraBrace = "Argument of" *> return (TeXError Nothing ExtraBrace)

tooMany :: Parser TeXError
tooMany = TeXError Nothing <$> TooMany <$> ("Too Many " *> takeTill (=='\''))

tooManyErrors :: Parser TeXError
tooManyErrors = "That makes 100 errors; please try again."
             *> return (TeXError Nothing TooManyErrors)

dimentionTooLarge :: Parser TeXError
dimentionTooLarge = "Dimension too large."
                 *> return (TeXError Nothing DimensionTooLarge)

-- line 8075 tex.web
paragraphEnded :: Parser TeXError
paragraphEnded = do
  _ <- "Paragraph ended before "
  _ <- takeTill isSpace
  _ <- toBeReadAgain
  l <- optional line
  return $ TeXError l ParagraphEnded

numberTooBig :: Parser TeXError
numberTooBig = "Number too big"
            *> return (TeXError Nothing NumberTooBig)

-- LaTeX errors

latexError :: Parser TeXError
latexError = TeXError Nothing <$> LaTeXError <$> ("LaTeX Error: " *> restOfLine)

-- Pages

nPages :: Parser Int
nPages = "Output written on "
      *> skipWhile (/= '(') *> char '('
      *> decimal

-- Utilities

restOfLine :: Parser ByteString
restOfLine = takeTill (=='\n') <* char '\n'

