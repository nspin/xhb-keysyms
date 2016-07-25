module Test where

import Parse

import Prelude hiding (readFile)
import Data.Text.IO (readFile)

test fname = do
    file <- readFile fname
    case parseDefs file of
        Left err -> print err
        Right defs -> print $ toPairs defs
