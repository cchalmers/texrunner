{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module System.TeXRunner.Online
  ( OnlineTeX
  -- * Running TeX online
  , runOnlineTex
  , runOnlineTex'
  -- * Interaction
  , hbox
  , hsize
  , showthe
  , onlineTeXParser
  , texPutStrLn
  ) where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.Attoparsec.ByteString   as A
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as B
import           Data.Maybe
import           Data.Monoid
import           System.FilePath
import           System.IO
import           System.IO.Streams            as Streams
import           System.IO.Streams.Attoparsec
import           System.IO.Temp
import qualified System.Process               as P

import System.TeXRunner.Parse
-- import System.TeXRunner

-- import System.IO.Streams.Debug

-- | New type for dealing with TeX's pipeing interface.
newtype OnlineTeX a = OnlineTeX {runOnlineTeX :: ReaderT TeXStreams IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TeXStreams)

-- Run a tex process, disguarding the resulting PDF.
runOnlineTex :: String
             -> [String]
             -> ByteString
             -> OnlineTeX a
             -> IO a
runOnlineTex command args preamble process =
  (\(a,_,_) -> a) <$> runOnlineTex' command args preamble process

-- Run a tex process, keeping the resulting PDF. The OnlineTeX must receive
-- the terminating control sequence (\bye, \end{document}, \stoptext).
runOnlineTex' :: String
              -> [String]
              -> ByteString
              -> OnlineTeX a
              -> IO (a,
                     TeXLog,
                     Maybe ByteString)
runOnlineTex' command args preamble process =
  withSystemTempDirectory "onlinetex." $ \path -> do
    (outS, inS, h) <- mkTeXHandles path Nothing command args preamble
    a              <- flip runReaderT (outS, inS) . runOnlineTeX $ process

    write Nothing outS
    _ <- waitForProcess h

    mPDF <- optional $ B.readFile (path </> "texput.pdf")
    mLog <- optional $ B.readFile (path </> "texput.log")

    return (a, parseLog $ fromMaybe "" mLog, mPDF)

-- | Get the dimensions of a hbox.
hbox :: ByteString -> OnlineTeX Box
hbox str = do
  clearUnblocking
  texPutStrLn $ "\\setbox0=\\hbox{" <> str <> "}\n\\showbox0\n"
  onlineTeXParser parseBox

-- | Parse result from @\showthe@.
showthe :: ByteString -> OnlineTeX Double
showthe str = do
  clearUnblocking
  texPutStrLn $ "\\showthe" <> str
  onlineTeXParser parseUnit

-- | Dimensions from filling the current line.
hsize :: OnlineTeX Double
hsize = boxWidth <$> hbox "\\line{\\hfill}"

-- | Run an Attoparsec parser on TeX's output.
onlineTeXParser :: A.Parser a -> OnlineTeX a
onlineTeXParser p = getInStream >>= liftIO . parseFromStream p
  -- TODO: have a timeout

texPutStrLn :: ByteString -> OnlineTeX ()
texPutStrLn a = getOutStream >>= liftIO . write (Just $ B.append a "\n")

-- * Internal
-- These funcions should be used with caution.

type TeXStreams = (OutputStream ByteString, InputStream ByteString)

getOutStream :: OnlineTeX (OutputStream ByteString)
getOutStream = reader fst

getInStream :: OnlineTeX (InputStream ByteString)
getInStream = reader snd

clearUnblocking :: OnlineTeX ()
clearUnblocking = getInStream >>= void . liftIO . Streams.read

-- | Uses a surface to open an interface with TeX,
mkTeXHandles :: FilePath
             -> Maybe [(String, String)]
             -> String
             -> [String]
             -> ByteString
             -> IO (OutputStream ByteString,
                    InputStream ByteString,
                    ProcessHandle)
mkTeXHandles dir env command args preamble = do

  -- TeX doesn't send anything to stderr
  (outStream, inStream, _, h) <- runInteractiveProcess'
                                   command
                                   args
                                   (Just dir)
                                   env

  -- inStream <- debugStream inStream'

  -- commands to get TeX to play nice
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
