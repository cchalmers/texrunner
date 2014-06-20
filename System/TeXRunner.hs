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
import Data.ByteString.Char8 as C8 hiding (intercalate, concatMap)
-- import Data.List             (intercalate)
import Data.Maybe
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

import System.TeXRunner.Parse

-- | Run TeX program in a temporary system directory.
runTex :: String     -- ^ TeX command
       -> [String]   -- ^ Additional arguments
       -> [FilePath] -- ^ Additional TeX inputs
       -> ByteString -- ^ Source TeX file
       -> IO (ExitCode,
              Either String TeXLog,
              Maybe ByteString)

runTex command args extras source =
  withSystemTempDirectory "texrunner." $ \path ->
    runTex' path command args extras source

runTex' :: FilePath   -- ^ Directory to run TeX in
        -> String     -- ^ TeX command
        -> [String]   -- ^ Additional arguments
        -> [FilePath] -- ^ Additional TeX inputs
        -> ByteString -- ^ Source TeX file
        -> IO (ExitCode,
               Either String TeXLog,
               Maybe ByteString)

runTex' path command args extras source = do

  C8.writeFile (path </> "texrunner.tex") source

  environment <- extraTeXInputs (path:extras) <$> getEnvironment

  let p = (proc command ("texrunner.tex" : args))
            { cwd     = Just path
            , std_in  = CreatePipe
            , std_out = CreatePipe
            , env     = Just environment
            }

  (Just inH, Just outH, _, pHandle) <- createProcess p

  hClose inH
  a <- C8.hGetContents outH

  hClose outH
  exitC <- waitForProcess pHandle

  pdfFile <- optional $ C8.readFile (path </> "texrunner.pdf")
  logFile <- optional $ C8.readFile (path </> "texrunner.log")

  return (exitC, parseLog $ fromMaybe a logFile, pdfFile)

-- | Add a list of paths to the tex 
extraTeXInputs :: [FilePath] -> [(String,String)] -> [(String,String)]
extraTeXInputs []      = id
extraTeXInputs inputss = alter f "TEXINPUTS"
  where
    f Nothing  = Just inputs
    f (Just x) = Just (inputs ++ [searchPathSeparator] ++ x)
    --
    inputs = concatMap (++ [searchPathSeparator]) inputss
    -- inputs = intercalate [searchPathSeparator] inputss

-- Alter can be used to insert, delete or update an element. Similar to alter
-- in Data.Map.
alter :: Eq k => (Maybe a -> Maybe a) -> k -> [(k,a)] -> [(k,a)]
alter f k = go
  where
    -- go :: [(k,a)] -> [(k,a)]
    go [] = maybeToList ((,) k <$> f Nothing)
    go ((k',x):xs) | k' == k   = case f (Just x) of
                                   Just x' -> (k',x') : xs
                                   Nothing -> xs
                   | otherwise = (k',x) : go xs

