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
  ( runTeX
  , runTeX'
  ) where

import Data.ByteString.Char8 as C8
import Data.Functor
import Data.Maybe
import Control.Applicative
import Control.Monad
import System.Directory
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
    runTeX' path command args source

runTeX' :: FilePath   -- ^ Directory to run TeX in
        -> String     -- ^ TeX command
        -> [String]   -- ^ Additional arguments
        -> ByteString -- ^ Source TeX file
        -> IO (ExitCode,
              Either String TeXLog,
              Maybe ByteString)

runTeX' path command args source = do

  C8.writeFile (path </> "texrunner.tex") source

  let p = (proc command ("texrunner.tex" : args))
            { cwd     = Just path
            , std_in  = CreatePipe
            , std_out = CreatePipe
            , env     = Nothing
            }

  -- (Just inH, Nothing, _, pHandle) <- createProcess p
  (Just inH, Just outH, _, pHandle) <- createProcess p

  -- this is important, TeX doesn't work unless you gobble it's output

  hClose inH
  a <- C8.hGetContents outH

  hClose outH
  exitC <- waitForProcess pHandle

  pdfFile <- optional $ C8.readFile (path </> "texrunner.pdf")
  logFile <- optional $ C8.readFile (path </> "texrunner.log")

  return (exitC, parseLog $ fromMaybe a logFile, pdfFile)

-- instance MonadPlus IO where
--     mzero       = ioError (userError "mzero")
--     m `mplus` n = m `catchIOError` \ _ -> n

instance Alternative IO where
    empty = mzero
    (<|>) = mplus

