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
  , prettyPrintLog
  ) where

import           Control.Applicative
import qualified Data.ByteString.Char8      as C8 hiding (concatMap)
import           Data.ByteString.Lazy.Char8 as LC8 hiding (concatMap)
import           Data.Maybe

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.Process

import           System.TeXRunner.Parse

-- | Same as 'runTex'' but runs TeX in a temporary system directory.
runTex :: String     -- ^ TeX command
       -> [String]   -- ^ Additional arguments
       -> [FilePath] -- ^ Additional TeX input paths
       -> ByteString -- ^ Source TeX file
       -> IO (ExitCode, TeXLog, Maybe ByteString)
runTex command args extras source =
  withSystemTempDirectory "texrunner." $ \path ->
    runTex' path command args extras source

-- | Run TeX program in the given directory. Additional TeX inputs are
--   for filepaths to things like images that TeX can refer to.
runTex' :: FilePath   -- ^ Directory to run TeX in
        -> String     -- ^ TeX command
        -> [String]   -- ^ Additional arguments
        -> [FilePath] -- ^ Additional TeX inputs
        -> ByteString -- ^ Source TeX file
        -> IO (ExitCode, TeXLog, Maybe ByteString)
runTex' path command args extras source = do

  LC8.writeFile (path </> "texrunner.tex") source

  environment <- extraTeXInputs (path:extras) <$> getEnvironment

  let p = (proc command ("texrunner.tex" : args))
            { cwd     = Just path
            , std_in  = CreatePipe
            , std_out = CreatePipe
            , env     = Just environment
            }

  (Just inH, Just outH, _, pHandle) <- createProcess p

  hClose inH
  a <- C8.hGetContents outH -- backup log

  hClose outH
  exitC <- waitForProcess pHandle

  pdfExists <- doesFileExist (path </> "texrunner.pdf")
  pdfFile   <- if pdfExists
                  then Just <$> LC8.readFile (path </> "texrunner.pdf")
                  else return Nothing

  logExists <- doesFileExist (path </> "texrunner.log")
  logFile   <- if logExists
                  then Just <$> C8.readFile (path </> "texrunner.log")
                  else return Nothing

  -- pdfFile <- optional $ LC8.readFile (path </> "texrunner.pdf")
  -- logFile <- optional $ C8.readFile (path </> "texrunner.log")

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
    go []         = maybeToList ((,) k <$> f Nothing)
    go ((k',x):xs)
      | k' == k   = case f (Just x) of
                      Just x' -> (k',x') : xs
                      Nothing -> xs
      | otherwise = (k',x) : go xs

