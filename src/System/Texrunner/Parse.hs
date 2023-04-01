{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

----------------------------------------------------------------------------
-- |
-- Module      :  System.Texrunner.Parse
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- Functions for parsing Tex output and logs. This log is parser is
-- experimental and largely untested. Please make an issue for any logs
-- that aren't parsed properly.
--
-----------------------------------------------------------------------------

module System.Texrunner.Parse
  ( -- * Box
    Box (..)
  , parseBox
    -- * Errors
  , TexLog (..)
  , TexInfo (..)
  , TexError (..)
  , TexError' (..)
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
import           Data.Semigroup

------------------------------------------------------------------------
-- Boxes
------------------------------------------------------------------------

-- | Data type for holding dimensions of a hbox. It is likely the
--   internal representation will change to allow nested boxes in the
--   future.
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

------------------------------------------------------------------------
-- Logs
------------------------------------------------------------------------

-- Everything's done using ByteString because io-streams' attoparsec module
-- only has a ByteString function. It's very likely this will all change to
-- Text in the future.

data TexLog = TexLog
  { texInfo   :: TexInfo
  , numPages  :: Maybe Int
  , texErrors :: [TexError]
  -- , rawLog    :: ByteString
  } deriving Show

data TexInfo = TexInfo
  { texCommand      :: Maybe ByteString
  , texVersion      :: Maybe ByteString
  , texDistribution :: Maybe ByteString
  -- , texDate    :: Maybe Date
  }
  deriving Show

-- Make shift way to parse a log by combining it in this way.
instance Semigroup TexLog where
  TexLog prog pages1 errors1 <> TexLog _ pages2 errors2 =
    case (pages1,pages2) of
      (Just a,_) -> TexLog prog (Just a) (errors1 ++ errors2)
      (_,b)      -> TexLog prog b (errors1 ++ errors2)

instance Monoid TexLog where
  mempty  = TexLog (TexInfo Nothing Nothing Nothing) Nothing []
  mappend = (<>)

infoParser :: Parser TexInfo
infoParser
  = TexInfo
  <$> optional ("This is"   *> takeTill (== ',') <* anyChar)
  <*> optional (" Version " *> takeTill (== ' ') <* anyChar)
  <*> optional (char '('    *> takeTill (== ')') <* anyChar)
  -- <*> Nothing

logFile :: Parser TexLog
logFile = mconcat <$> many logLine
  where
    logLine = do
      info   <- infoParser
      pages  <- optional nPages
      errors <- maybeToList <$> optional someError
      _      <- restOfLine
      return $ TexLog info pages errors

-- thisIs :: Parser TexVersion

parseLog :: ByteString -> TexLog
parseLog = (\(Right a) -> a) . parseOnly logFile
-- the parse should never fail (I think)

prettyPrintLog :: TexLog -> ByteString
prettyPrintLog TexLog {..} =
  fromMaybe "unknown program" (texCommand texInfo)
  <> maybe "" (" version " <>) (texVersion texInfo)
  <> maybe "" (" " <>) (texDistribution texInfo)
  <> "\n"
  <> maybe "" ((<> "pages\n") . pack . show) numPages
  <> B.unlines (map (pack . show) texErrors)

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | An error from tex with possible line number.
data TexError = TexError
  { errorLine :: Maybe Int
  , error'    :: TexError'
  }
  deriving Show

instance Eq TexError where
  TexError _ a == TexError _ b = a == b

-- | A subset of possible error Tex can throw.
data TexError'
  = UndefinedControlSequence ByteString
  | MissingNumber
  | Missing Char
  | IllegalUnit -- (Maybe Char) (Maybe Char)
  | PackageError String String
  | LatexError ByteString
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

-- | Parse any line beginning with "! ". Any unknown errors are returned as 'UnknownError'.
someError :: Parser TexError
someError =  mark *> errors
  where
    -- in context exclamation mark isn't always at the beginning
    mark = "! " <|> (notChar '\n' *> mark)
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
          <|> dimensionTooLarge
          <|> tooManyErrors
          <|> fatalError
          <|> TexError Nothing <$> UnknownError <$> restOfLine

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

------------------------------------------------------------------------
-- Error parsers
------------------------------------------------------------------------

undefinedControlSequence :: Parser TexError
undefinedControlSequence = do
  _ <- "Undefined control sequence"
  _ <- optional "."

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
  return $ TexError l (UndefinedControlSequence cs)

finalControlSequence :: Parser ByteString
finalControlSequence = last <$> many1 controlSequence
  where
    controlSequence = cons '\\' <$>
      (char '\\' *> takeTill (\x -> isSpace x || x=='\\'))

illegalUnit :: Parser TexError
illegalUnit = do
  _ <- "Illegal unit of measure (pt inserted)"
  _ <- optional toBeReadAgain
  _ <- optional toBeReadAgain

  return $ TexError Nothing IllegalUnit

missingNumber :: Parser TexError
missingNumber = do
  _ <- "Missing number, treated as zero"
  _ <- optional toBeReadAgain
  _ <- optional noteStar
  return $ TexError Nothing MissingNumber

badBox :: Parser TexError
badBox = do
  s <- choice ["Underfull", "Overfull", "Tight", "Loose"]
  _ <- " \\hbox " *> char '(' *> takeTill (==')') <* char ')'
  l <- optional line
  return $ TexError l (BadBox s)

missing :: Parser TexError
missing = do
  c <- "Missing " *> anyChar <* " inserted"
  l <- optional line
  return $ TexError l (Missing c)

line :: Parser Int
line =  " detected at line " *> decimal
    <|> "l."                 *> decimal

emergencyStop :: Parser TexError
emergencyStop = "Emergency stop"
             *> return (TexError Nothing EmergencyStop)

fatalError :: Parser TexError
fatalError = TexError Nothing <$> FatalError <$> (" ==> Fatal error occurred, " *> restOfLine)

-- line 8058 tex.web
extraBrace :: Parser TexError
extraBrace = "Argument of" *> return (TexError Nothing ExtraBrace)

tooMany :: Parser TexError
tooMany = TexError Nothing <$> TooMany <$> ("Too Many " *> takeTill (=='\''))

tooManyErrors :: Parser TexError
tooManyErrors = "That makes 100 errors; please try again"
             *> return (TexError Nothing TooManyErrors)

dimensionTooLarge :: Parser TexError
dimensionTooLarge = "Dimension too large"
                 *> return (TexError Nothing DimensionTooLarge)

-- line 8075 tex.web
paragraphEnded :: Parser TexError
paragraphEnded = do
  _ <- "Paragraph ended before "
  _ <- takeTill isSpace
  _ <- toBeReadAgain
  l <- optional line
  return $ TexError l ParagraphEnded

numberTooBig :: Parser TexError
numberTooBig = "Number too big"
            *> return (TexError Nothing NumberTooBig)

-- Latex errors

latexError :: Parser TexError
latexError = TexError Nothing <$> LatexError <$> ("Latex Error: " *> restOfLine)

-- Pages

nPages :: Parser Int
nPages = "Output written on "
      *> skipWhile (/= '(') *> char '('
      *> decimal

-- Utilities

restOfLine :: Parser ByteString
restOfLine = takeTill (=='\n') <* char '\n'

