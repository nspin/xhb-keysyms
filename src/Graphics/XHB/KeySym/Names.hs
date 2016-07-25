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
import qualified Data.Map as M


getAssoc :: KEYSYM -> Maybe (String, Maybe Char)
getAssoc sym = M.lookup sym keySymNames

keySymToString :: KEYSYM -> Maybe String
keySymToString sym = fst <$> getAssoc sym

keySymToChar :: KEYSYM -> Maybe Char
keySymToChar sym = snd =<< getAssoc sym

stringToKeySym :: String -> Maybe KEYSYM
stringToKeySym name = lookup name . map f $ M.toList keySymNames
  where
    f (sym, (name, _)) = (name, sym)

charToKeySym :: Char -> Maybe KEYSYM
charToKeySym char = lookup char . catMaybes . map f $ M.toList keySymNames
  where
    f (_, (_, Nothing)) = Nothing
    f (sym, (_, Just c)) = Just (c, sym)
