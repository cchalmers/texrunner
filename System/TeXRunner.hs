----------------------------------------------------------------------------
-- |
-- Module      :  System.TeXRunner
-- Copyright   :  (c) 2014 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- Functions for running TeX.
--
-----------------------------------------------------------------------------

module System.TeXRunner
  ( runTex
  , runTex'
  ) where

import Control.Applicative
import Data.ByteString.Char8 as C8
import Data.Maybe
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

import System.TeXRunner.Parse

-- | Run TeX program in a temporary system directory.
runTex :: String     -- ^ TeX command
       -> [String]   -- ^ Additional arguments
       -> ByteString -- ^ Source TeX file
       -> IO (ExitCode,
              Either String TeXLog,
              Maybe ByteString)

runTex command args source =
  withSystemTempDirectory "texrunner." $ \path ->
    runTex' path command args source

runTex' :: FilePath   -- ^ Directory to run TeX in
        -> String     -- ^ TeX command
        -> [String]   -- ^ Additional arguments
        -> ByteString -- ^ Source TeX file
        -> IO (ExitCode,
               Either String TeXLog,
               Maybe ByteString)

runTex' path command args source = do

  C8.writeFile (path </> "texrunner.tex") source

  let p = (proc command ("texrunner.tex" : args))
            { cwd     = Just path
            , std_in  = CreatePipe
            , std_out = CreatePipe
            , env     = Nothing -- Add dirs to "TEXINPUTS"
            }

  (Just inH, Just outH, _, pHandle) <- createProcess p

  hClose inH
  a <- C8.hGetContents outH

  hClose outH
  exitC <- waitForProcess pHandle

  pdfFile <- optional $ C8.readFile (path </> "texrunner.pdf")
  logFile <- optional $ C8.readFile (path </> "texrunner.log")

  return (exitC, parseLog $ fromMaybe a logFile, pdfFile)

