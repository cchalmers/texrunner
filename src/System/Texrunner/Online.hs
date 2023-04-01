{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

----------------------------------------------------------------------------
-- |
-- Module      :  System.Texrunner.Online
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- Functions for running and parsing using Tex's online interface. This is
-- mostly used for getting measurements like hbox dimensions and textwidth.
--
-- Tex's online interface is basically running the command line. You can
-- see it by running @pdflatex@ without any arguments. The contents can
-- be written line by and tex can give feedback though stdout, which gets
-- parsed in by this module. This is the only way I know to get info
-- like hbox sizes. Please let me know if you know a better way.
--
-----------------------------------------------------------------------------

module System.Texrunner.Online
  ( OnlineTex
  -- * Running Tex online
  , runOnlineTex

  , runOnlineTex'
  -- * Interaction
  , hbox
  , hsize
  , showthe
  , onlineTexParser
  , texPutStrLn

  -- * Low level
  -- | These functions allow give you direct access to the iostreams
  --   with tex. The implementation is likely to change in the future
  --   and using them directly is not recommended.
  , TexStreams
  , getInStream
  , getOutStream
  , clearUnblocking
  ) where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.Attoparsec.ByteString   as A
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as C8
import qualified Data.ByteString.Lazy.Char8   as LC8
import           Data.List                    (find)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Traversable             as T

import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Streams            as Streams
import           System.IO.Streams.Attoparsec
import           System.IO.Temp
import           System.Process               as P (runInteractiveProcess)

import           System.Texrunner.Parse

-- | Type for dealing with Tex's piping interface; the current streams
--   are available though the 'MonadReader' instance.
newtype OnlineTex a = OnlineTex {runOnlineTexT :: ReaderT TexStreams IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TexStreams)

-- | Run a tex process, discarding the resulting PDF.
runOnlineTex :: String      -- ^ tex command
             -> [String]    -- ^ tex command arguments
             -> ByteString  -- ^ preamble
             -> OnlineTex a -- ^ Online Tex to be Run
             -> IO a
runOnlineTex command args preamble process =
  (\(a,_,_) -> a) <$> runOnlineTex' command args preamble process

-- | Run a tex process, keeping the resulting PDF. The OnlineTex must receive
--   the terminating control sequence (\\bye, \\end{document}, \\stoptext).
runOnlineTex' :: String
              -> [String]
              -> ByteString
              -> OnlineTex a
              -> IO (a, TexLog, Maybe LC8.ByteString)
runOnlineTex' command args preamble process =
  withSystemTempDirectory "onlinetex." $ \path -> do
    (outS, inS, h) <- mkTexHandles path Nothing command args preamble
    a              <- flip runReaderT (outS, inS) . runOnlineTexT $ process

    write Nothing outS
    _ <- waitForProcess h

    -- it's normally texput.pdf but some (Context) choose random names
    pdfPath  <- find ((==".pdf") . takeExtension) <$> getDirectoryContents path
    pdfFile  <- T.mapM (LC8.readFile . (path </>)) pdfPath

    logPath  <- find ((==".log") . takeExtension) <$> getDirectoryContents path
    logFile  <- T.mapM (C8.readFile . (path </>)) logPath

    return (a, parseLog $ fromMaybe "" logFile, pdfFile)

-- | Get the dimensions of a hbox.
hbox :: Fractional n => ByteString -> OnlineTex (Box n)
hbox str = do
  clearUnblocking
  texPutStrLn $ "\\setbox0=\\hbox{" <> str <> "}\n\\showbox0\n"
  onlineTexParser parseBox

-- | Parse result from @\showthe@.
showthe :: Fractional n => ByteString -> OnlineTex n
showthe str = do
  clearUnblocking
  texPutStrLn $ "\\showthe" <> str
  onlineTexParser parseUnit

-- | Dimensions from filling the current line.
hsize :: Fractional n => OnlineTex n
hsize = boxWidth <$> hbox "\\line{\\hfill}"

-- | Run an Attoparsec parser on Tex's output.
onlineTexParser :: A.Parser a -> OnlineTex a
onlineTexParser p = getInStream >>= liftIO . parseFromStream p
  -- TODO: have a timeout

texPutStrLn :: ByteString -> OnlineTex ()
texPutStrLn a = getOutStream >>= liftIO . write (Just $ C8.append a "\n")

-- * Internal
-- These functions should be used with caution.

type TexStreams = (OutputStream ByteString, InputStream ByteString)

-- | Get the output stream to read tex's output.
getOutStream :: OnlineTex (OutputStream ByteString)
getOutStream = reader fst

-- | Get the input stream to give text to tex.
getInStream :: OnlineTex (InputStream ByteString)
getInStream = reader snd

-- | Clear any output tex has already given.
clearUnblocking :: OnlineTex ()
clearUnblocking = getInStream >>= void . liftIO . Streams.read

-- | Uses a surface to open an interface with Tex.
mkTexHandles :: FilePath
             -> Maybe [(String, String)]
             -> String
             -> [String]
             -> ByteString
             -> IO (OutputStream ByteString,
                    InputStream ByteString,
                    ProcessHandle)
mkTexHandles dir env command args preamble = do

  -- Tex doesn't send anything to stderr
  (outStream, inStream, _, h) <- runInteractiveProcess'
                                   command
                                   args
                                   (Just dir)
                                   env

  -- inStream <- debugStream inStream'

  -- commands to get Tex to play nice
  write (Just $ "\\tracingonline=1"  -- \showbox is echoed to stdout
             <> "\\showboxdepth=1"   -- show boxes one deep
             <> "\\showboxbreadth=1"
             <> "\\scrollmode\n"     -- don't pause after showing something
        ) outStream
  write (Just preamble) outStream

  return (outStream, inStream, h)

-- Adapted from io-streams. Sets input handle to line buffering.
runInteractiveProcess'
    :: FilePath                 -- ^ Filename of the executable (see 'proc' for details)
    -> [String]                 -- ^ Arguments to pass to the executable
    -> Maybe FilePath           -- ^ Optional path to the working directory
    -> Maybe [(String,String)]  -- ^ Optional environment (otherwise inherit)
    -> IO (OutputStream ByteString,
           InputStream ByteString,
           InputStream ByteString,
           ProcessHandle)
runInteractiveProcess' cmd args wd env = do
    (hin, hout, herr, ph) <- P.runInteractiveProcess cmd args wd env

    -- it is possible to flush using write (Just "") but this seems nicer
    -- is there a better way?
    hSetBuffering hin LineBuffering

    sIn  <- Streams.handleToOutputStream hin >>=
            Streams.atEndOfOutput (hClose hin) >>=
            Streams.lockingOutputStream
    sOut <- Streams.handleToInputStream hout >>=
            Streams.atEndOfInput (hClose hout) >>=
            Streams.lockingInputStream
    sErr <- Streams.handleToInputStream herr >>=
            Streams.atEndOfInput (hClose herr) >>=
            Streams.lockingInputStream

    return (sIn, sOut, sErr, ph)

-- debugStream :: InputStream ByteString -> IO (InputStream ByteString)
-- debugStream = debugInput id "tex" Streams.stdout
