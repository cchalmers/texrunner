{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tex.LogParse where

import Data.ByteString.Lazy.Char8 as B (unlines, ByteString, writeFile)
import Data.Monoid

import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework as F

import System.Texrunner
import System.Texrunner.Parse
import Control.Lens
import Data.Foldable

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

latexHeader, latexBye :: ByteString
latexHeader = B.unlines
  [ "\\documentclass{article}"
  , "\\begin{document}"
  ]
latexBye = "\\end{document}"

latex e code = testCase ("latex" ++ show e) $ do
  (exitCode, texLog, mPDF) <- runTex "pdflatex" [] [] (latexHeader <> code)
  head (map error' $ texErrors texLog) @?= e

contextHeader, contextBye :: ByteString
contextHeader = "\\starttext"
contextBye = "\\stoptext"

context e code = testCase ("context" ++ show e) $ do
  (exitCode, texLog, mPDF) <- runTex "context" [] [] (contextHeader <> code)
  take 1 (map error' (texErrors texLog)) @?= [e]
  -- head (map error' $ texErrors texLog) @?= e
  -- assertBool ("context" ++ show e) $ texLog `containsError` e

-- Generating tex sample tex files -------------------------------------

-- plain tex

genTexFiles :: IO ()
genTexFiles = for_ labeledErrors mkFile
  where
    mkFile (nm, (_err, xs)) = ifor_ xs $ \i x -> do
      let doc = latexHeader <> x <> latexBye
          name | length xs == 1 = nm
               | otherwise      = nm <> "-" <> show (i+1)
      B.writeFile ("tests/samples/tex/" <> name <> ".tex") doc

-- latex

-- pdflatex -draftmode --interaction=nonstopmode $i

genLatexFiles :: IO ()
genLatexFiles = for_ labeledErrors mkFile
  where
    mkFile (nm, (_err, xs)) = ifor_ xs $ \i x -> do
      let doc = latexHeader <> x <> latexBye
          name | length xs == 1 = nm
               | otherwise      = nm <> "-" <> show (i+1)
      B.writeFile ("tests/samples/latex/" <> name <> ".tex") doc

-- context tex

genContextFiles :: IO ()
genContextFiles = for_ labeledErrors mkFile
  where
    mkFile (nm, (_err, xs)) = ifor_ xs $ \i x -> do
      let doc = contextHeader <> x <> contextBye
          name | length xs == 1 = nm
               | otherwise      = nm <> "-" <> show (i+1)
      B.writeFile ("tests/samples/context/" <> name <> ".tex") doc

labeledErrors =
  [ ("missing-dollar", missingDollar)
  , ("dimention-too-large", dimensionTooLarge)
  , ("illegal-unit", illegalUnit)
  , ("missing-number", missingNumber)
  , ("undefined-control-sequence", undefinedControlSequence)
  ]

-- Checking error parsing ----------------------------------------------

containsError :: TexLog -> TexError -> Bool
containsError log (TexError _ err) = err `elem` map error' (texErrors log)

checkError :: (TexError' -> ByteString -> F.Test) -> (TexError', [ByteString]) -> F.Test
checkError f (e, codes) = testGroup (show e) $ map (f e) codes

checkErrors :: TestName -> (TexError' -> ByteString -> F.Test) ->  F.Test
checkErrors name f = testGroup name $ map (checkError f) texErrs

-- Sample errors -------------------------------------------------------

texErrs =
  [ missingDollar
  , dimensionTooLarge
  , illegalUnit
  , missingNumber
  , undefinedControlSequence
  ]

missingDollar = (,) (Missing '$')
  [ "$x+1=2\n\n"
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
