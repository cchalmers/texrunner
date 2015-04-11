{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module TeX.LogParse where

import Data.ByteString.Lazy.Char8 as B (unlines, ByteString)
import Data.Monoid

import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework as F

import System.TeXRunner
import System.TeXRunner.Parse

tests = texTests ++ latexTests ++ contextTests

texTests = [checkErrors "tex error parse" tex]
latexTests = [checkErrors "latex error parse" latex]
contextTests = [checkErrors "context error parse" context]

withHead :: Monad m => [a] -> (a -> m ()) -> m ()
withHead (a:_) f = f a
withHead _     _ = return ()

tex e code = testCase ("tex" ++ show e) $ do
  (exitCode, texLog, mPDF) <- runTex "pdftex" [] [] code
  map error' (texErrors texLog) @?= [e]

latexHeader :: ByteString
latexHeader = B.unlines
  [ "\\documentclass{article}"
  , "\\begin{document}"
  ]

latex e code = testCase ("latex" ++ show e) $ do
  (exitCode, texLog, mPDF) <- runTex "pdflatex" [] [] (latexHeader <> code)
  head (map error' $ texErrors texLog) @?= e

contextHeader :: ByteString
contextHeader = "\\starttext"

context e code = testCase ("context" ++ show e) $ do
  (exitCode, texLog, mPDF) <- runTex "context" [] [] (contextHeader <> code)
  map error' (texErrors texLog) @?= [e]
  -- head (map error' $ texErrors texLog) @?= e
  -- assertBool ("context" ++ show e) $ texLog `containsError` e

containsError :: TeXLog -> TeXError -> Bool
containsError log (TeXError _ err) =  err `elem` map error' (texErrors log)

checkError :: (TeXError' -> ByteString -> F.Test) -> (TeXError', [ByteString]) -> F.Test
checkError f (e, codes) = testGroup (show e) $ map (f e) codes

checkErrors :: TestName -> (TeXError' -> ByteString -> F.Test) ->  F.Test
checkErrors name f = testGroup name $ map (checkError f) texErrs

texErrs =
  [ missingDollar
  , dimensionTooLarge
  , illegalUnit
  , missingNumber
  , undefinedControlSequence
  ]

missingDollar = (,) (Missing '$')
  [ "$x+1=2\n\n"
  -- , "x_1"
  , "$$x+1=2\n\n"
  ]

dimensionTooLarge = (,) DimensionTooLarge
  [ "\\hskip100000em"
  ]

illegalUnit = (,) IllegalUnit
  [ "\\hskip1cn"
  ]

missingNumber = (,) MissingNumber
  [ "\\hskip hi"
  ]

undefinedControlSequence = (,) (UndefinedControlSequence "\\hobx")
  [ "\\hobx"
  ]



--
-- missingDollarExample2= "x_1"
--
-- missingDollarExample3= "
--
-- numberTooBig = "10000000000"
--
-- overfull = "\\hbox to 1em{overfill box}"
--
-- underfill = "\\hbox to 20em{underfill box}"
--
-- illegalUnit = "\\hskip{1cn}"
--
-- undefinedControlSequence = "\\hobx"
--
-- missingNumber = "\\hskip"
--
--
-- missingDollarTest = (texPutStrLn missingDollarExample, MissingDollar)
--
