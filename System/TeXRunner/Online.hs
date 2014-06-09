{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module System.TeXRunner.Online where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.Attoparsec.ByteString   as A
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as B
import           Data.Monoid
import           System.IO
import           System.IO.Streams            as Streams
import           System.IO.Streams.Attoparsec
import           System.IO.Temp
import qualified System.Process               as P

import System.TeXRunner.Parse

-- | New type for helping
newtype TeXProcess a = TeXProcess {runTeXProcess :: ReaderT TeXStreams IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TeXStreams)

-- Run a tex process, disguarding the resulting PDF.
runTexProcess :: FilePath
              -> Maybe [(String, String)]
              -> String
              -> [String]
              -> ByteString
              -> TeXProcess a
              -> IO a
runTexProcess dir env command args preamble process = do
  (outS, inS, h) <- mkTeXHandles dir env command args preamble
  a              <- flip runReaderT (outS, inS) . runTeXProcess $ process

  write Nothing outS
  _ <- waitForProcess h

  return a

-- -- Run a tex process, keeping the resulting PDF.
-- runTexOnline :: FilePath
--              -> Maybe [(String, String)]
--              -> String
--              -> [String]
--              -> ByteString
--              -> TeXProcess a
--              -> IO (a, TeXResult)
-- runTexOnline dir env command args preamble process = do
--   streams <- mkTeXHandles command args preamble
--   a       <- flip runReaderT streams . runTeXProcess $ process
--
--   getOutSteam >>= write Nothing
--   waitForProcess pHandle
--
--   return a

-- | Get the dimensions of a hbox.
hbox :: ByteString -> TeXProcess Box
hbox str = do
  clearUnblocking
  texPutStrLn $ "\\setbox0=\\hbox{" <> str <> "}\n\\showbox0\n"
  --
  texProcessParser parseBox

-- | Dimensions from filling the current line.
hfill :: TeXProcess Double
hfill = boxWidth <$> hbox "\\line{\\hfill}"

-- | Run an Attoparsec parser on TeX's output.
texProcessParser :: A.Parser a -> TeXProcess a
texProcessParser p = getInStream >>= liftIO . parseFromStream p
  -- TODO: have a timeout

texPutStrLn :: ByteString -> TeXProcess ()
texPutStrLn a = getOutStream >>= liftIO . write (Just $ B.append a "\n")

-- * Internal
-- These funcions should be used with caution.

type TeXStreams = (OutputStream ByteString, InputStream ByteString)

getOutStream :: TeXProcess (OutputStream ByteString)
getOutStream = reader fst

getInStream :: TeXProcess (InputStream ByteString)
getInStream = reader snd


clearUnblocking :: TeXProcess ()
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

  -- inStream  <- debugInputBS "in" Streams.stdout inStream'
  -- outStream <- debugOutputBS "out" Streams.stdout outStream'

  write (Just "\\tracingonline=1\\showboxdepth=1\\showboxbreadth=1\\scrollmode\n")
        outStream
  write (Just preamble) outStream

  -- flush
  -- write (Just "\n") outStream
  -- write (Just "") outStream

  return (outStream, inStream, h)

------------------------------------------------------------------------
-- debug

-- debugBox :: ByteString -> TeXProcess ()
-- debugBox str = do
--   clearUnblocking
--   texPutStrLnD $ "\\setbox0\\hbox{" <> str <> "}\n\\showbox0\n"
--
--   outH <- getOutStream
--   runEffect $ P.fromHandle outH >-> P.takeWhile (/=33) >-> P.stdout
--
-- texPutStrLn :: ByteString -> TeXProcess ()
-- texPutStrLn str = getInStream >>= liftIO . flip B.hPutStrLn str
--
-- texPutStrLnD :: ByteString -> TeXProcess ()
-- texPutStrLnD str = getInStream >>= liftIO . flip B.hPutStrLn str
--                 >> liftIO (B.putStrLn $ "!!! " <>  str)

plain :: TeXProcess a -> IO a
plain a = withSystemTempDirectory "texonline." $ \path ->
            runTexProcess path Nothing "pdftex" [] "" a

plainHandles :: IO (OutputStream ByteString,
                    InputStream ByteString,
                    ProcessHandle)
plainHandles = mkTeXHandles "./" Nothing "pdftex" [] ""
-- plainHandles = withSystemTempDirectory "texonline." $ \path ->
--                  mkTeXHandles path Nothing "pdftex" [] ""

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


------------------------------------------------------------------------



-- combineBoxString :: Box -> String -> Diagram PGF R2
-- combineBoxString (Box h d w) str = withEnvelope rec $ baselineText str
--   where
--     rec = translateY (-d) . alignBL $ rect w (h+d) :: Diagram PGF R2

-- diagramText :: Surface -> String -> IO (Diagram PGF R2)
-- diagramText surf str = do
--   p <- mkTeXHandles surf
--   threadDelay 1000000
--   box <- strToBox p str
--   closeTeXHandles p
--   return $ combineBoxString box str


-- \setbox0=\hbox{<<string>>}
-- \showbox0
-- >   *\showbox0
-- >   > \box0=
------------ height  depth    width
-- >   \hbox(6.83331+2.15277)x18.6108
-- >   .\tenrm T
-- >   .\kern -1.66702
-- >   .\hbox(6.83331+0.0)x6.80557, shifted 2.15277
-- >   ..\tenrm E
-- >   .\kern -1.25
-- >   .\tenrm X
-- >
-- >   ! OK.
-- >   <*> \showbox0
-- >
-- >   ?
--



-- pstrToBox :: TeXHandles -> ByteString -> IO (Maybe (Either AP.ParsingError Box))
-- pstrToBox (TeXHandles _ inH outH) str = do
--   _ <- B.hGetNonBlocking outH 10000
--   B.hPutStrLn inH $ "\\setbox0=\\hbox{" <> str <> "}"
--   hPutStrLn inH "\\showbox0\n"
--   (result, unused) <- runStateT myParser (P.fromHandle outH)
--   -- _ <- B.hGetNonBlocking outH 10000
--   return result
  -- runEffect $ P.hGet 10 outH >-> P.dropWhile (/=42) >-> P.takeWhile (/=33) >-> P.stdout

-- pipeGetBox :: Handle -> IO ByteString
-- pipeGetBox h = do
--   let prod = P.fromHandle h
