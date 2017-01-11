module Graphics.XHB.KeySym.Alph
    ( lowerCaseOf
    , isUpperCase
    , upperCaseOf
    , isLowerCase
    ) where


import Graphics.XHB
import Graphics.XHB.KeySym.Alph.Internal

import Data.Map as M
import Data.Maybe


isLowerCase :: KEYSYM -> Bool
isLowerCase = isJust . upperCaseOf

lowerCaseOf :: KEYSYM -> Maybe KEYSYM
lowerCaseOf = flip M.lookup upperToLower

isUpperCase :: KEYSYM -> Bool
isUpperCase = isJust . lowerCaseOf

upperCaseOf :: KEYSYM -> Maybe KEYSYM
upperCaseOf = flip M.lookup lowerToUpper
