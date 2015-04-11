module Main (main) where

import Test.Framework (defaultMain)
import qualified Tex.PDF
import qualified Tex.LogParse

main :: IO ()
main = defaultMain (Tex.PDF.tests ++ Tex.LogParse.tests)

