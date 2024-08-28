{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tex.PDF where

import Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import System.Exit

import Test.HUnit
import Test.Framework.Providers.HUnit

import System.Texrunner
import System.Texrunner.Online

tests = texTests ++ latexTests -- ++ contextTests
texTests = [tex, texOnline]
latexTests = [latex, latexOnline]
contextTests = [] -- [context, contextOnline] https://github.com/cchalmers/texrunner/pull/12

texDocument :: ByteString
texDocument = "hi\\bye"

latexDocument :: ByteString
latexDocument = B.unlines
  [ "\\documentclass{article}"
  , "\\begin{document}"
  , "hi"
  , "\\end{document}"
  ]

contextDocument :: ByteString
contextDocument = B.unlines
  [ "\\starttext"
  , "hi"
  , "\\stoptext"
  ]

tex     = testRunTeX "pdftex" [] texDocument
latex   = testRunTeX "pdflatex" [] latexDocument
context = testRunTeX "context" ["--once"] contextDocument

testRunTeX command args document = testCase command $ do
  (exitCode, _, mPDF) <- runTex command args [] document
  exitCode @?= ExitSuccess
  assertBool "pdf found" $ isJust mPDF


-- online

testOnlineTeX command args document = testCase (command ++ "Online") $ do
  ((), _, mPDF) <- runOnlineTex' command args "" (texPutStrLn $ toStrict document)
  assertBool "pdf found" $ isJust mPDF

texOnline     = testOnlineTeX "pdftex" [] texDocument
latexOnline   = testOnlineTeX "pdflatex" [] latexDocument
contextOnline = testOnlineTeX "context" ["--luatex", "--pipe"] contextDocument




-- tests to make:
-- * texinputs for files in cwd
-- * pdf made online

