module Graphics.XHB.KeySym.Names
    ( keySymToString
    , keySymToChar
    , stringToKeySym
    , charToKeySym
    ) where


import Graphics.XHB
import Graphics.XHB.KeySym.Names.Internal
import Data.Maybe
import Data.List


getAssoc :: KEYSYM -> Maybe (String, Maybe Char)
getAssoc sym = lookup sym $ map (\(a, b, c) -> (a, (b, c))) keySymNames

keySymToString :: KEYSYM -> Maybe String
keySymToString sym = fst <$> getAssoc sym

keySymToChar :: KEYSYM -> Maybe Char
keySymToChar sym = getAssoc sym >>= snd

stringToKeySym :: String -> Maybe KEYSYM
stringToKeySym name = lookup name $ map (\(a, b, c) -> (b, a)) keySymNames

charToKeySym :: Char -> Maybe KEYSYM
charToKeySym char = lookup char . catMaybes $ map f keySymNames
  where
    f (_, _, Nothing) = Nothing
    f (sym, _, Just c) = Just (c, sym)
