{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( KeySymDef(..)
    , toPairs
    , parseDefs
    ) where

import Prelude hiding (getContents, takeWhile)
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Either
import Data.List
import Data.Text (Text, unpack)
import Data.Text.IO (getContents)
import Data.Attoparsec.Text
import Data.Maybe
import System.Exit
import System.Environment
import Language.Haskell.Exts


data Case = Capital | Small
    deriving Show


data KeySymDef = KeySymDef String
                           Integer
                           (Maybe (Char, Maybe (Case, (String, [String])))) -- (Small, ("LATIN", ["A",  "WITH", "SQUIGGLE"]))
    deriving Show


toPairs :: [KeySymDef] -> [(Integer, Integer)]
toPairs defs = rights . map (crunch . map snd) . groupBy (\a b -> fst a == fst b) $ sortBy (\a b -> fst a `compare` fst b) alphs
  where
    crunch [(Small, i), (Capital, j)] = Right (i, j)
    crunch [(Capital, i), (Small, j)] = Right (j, i)
    crunch x = Left x
    alphs :: [((String, [String]), (Case, Integer))]
    alphs = let f (KeySymDef name val spec) = do
                    (_, spc) <- spec
                    (isBig, tag) <- spc
                    return (tag, (isBig, val))
            in catMaybes $ map f defs


parseDefs :: Text -> Either String [KeySymDef]
parseDefs = parseOnly keySymDefs


keySymDefs :: Parser [KeySymDef]
keySymDefs = catMaybes <$> many (Just <$> keySymDef <|> Nothing <$ restOfLine)


keySymDef :: Parser KeySymDef
keySymDef = do
    string "#define XK_"
    name <- unpack <$> takeTill (== ' ')
    skipSpace
    string "0x"
    code <- hexadecimal :: Parser Integer
    skipWhile (== ' ')
    spec <- Just <$> comment <|> return Nothing
    restOfLine
    return $ KeySymDef name code spec
  where
    comment = do
        string "/* "
        n <- string "U+" *> hexadecimal
        char ' '
        -- not "*/" because of XK_Sinh_{oo,au}2
        cap <- Just <$> (capSpec <* (skipSpace >> string "*/")) <|> Nothing <$ manyTill anyChar (string "*/")
        return (chr n, cap)
    capSpec = do
        category <- upperWord
        char ' '
        isBig <- Capital <$ string "CAPITAL" <|> Small <$ string "SMALL"
        char ' '
        spec <- upperWord `sepBy1` char ' '
        return (isBig, (category, spec))


upperWord :: Parser String
upperWord = many1 $ satisfy isUpper

restOfLine :: Parser Text
restOfLine = takeTill isEndOfLine <* endOfLine
