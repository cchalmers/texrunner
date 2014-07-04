{-# LANGUAGE OverloadedStrings #-}
module TeX.PDF where

import Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import System.Exit

import Test.HUnit
import Test.Framework.Providers.HUnit

import System.TeXRunner

tests = [tex, latex, context]

texHeader :: ByteString
texHeader = "hi\\bye"

tex = testCase "pdftex" $ do
  (exitCode, _, mPDF) <- runTex "pdftex" [] [] "hi\\bye"
  assertEqual "pdftex exit success" ExitSuccess exitCode
  assertBool "pdftex pdf" $ isJust mPDF

latexDocument :: ByteString
latexDocument = B.unlines
  [ "\\documentclass{article}"
  , "\\begin{document}"
  , "hi"
  , "\\end{document}"
  ]

latex = testCase "pdflatex" $ do
  (exitCode, _, mPDF) <- runTex "pdflatex" [] [] latexDocument
  assertEqual "pdflatex exit success" ExitSuccess exitCode
  assertBool "pdflatex pdf" $ isJust mPDF

contextDocument :: ByteString
contextDocument = B.unlines
  [ "\\starttext"
  , "hi"
  , "\\stoptext"
  ]

context = testCase "context" $ do
  (exitCode, _, mPDF) <- runTex "context" [] [] contextDocument
  assertEqual "context exit success" ExitSuccess exitCode
  assertBool "context pdf" $ isJust mPDF

-- tests to make:
-- * texinputs for files in cwd

-- missingDollarExample1= "$x+1=2\n\n"
-- 
-- missingDollarExample2= "x_1"
-- 
-- missingDollarExample3= "$$x+1=2\n\n"
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
