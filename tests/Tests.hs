module Main (main) where

import Test.Framework (defaultMain)
import qualified TeX.PDF
import qualified TeX.LogParse

main :: IO ()
main = defaultMain (TeX.PDF.tests ++ TeX.LogParse.tests)

