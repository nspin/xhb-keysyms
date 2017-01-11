module Graphics.XHB.KeySym.Alph
    ( lowerCaseOf
    , upperCaseOf
    , isLowerCase
    , isUpperCase
    ) where


import Graphics.XHB
import Graphics.XHB.KeySym.Alph.Internal

import Data.Map as M
import Data.Maybe


lowerCaseOf :: KEYSYM -> Maybe KEYSYM
lowerCaseOf = flip M.lookup upperToLower

upperCaseOf :: KEYSYM -> Maybe KEYSYM
upperCaseOf = flip M.lookup lowerToUpper

isLowerCase :: KEYSYM -> Bool
isLowerCase = isJust . upperCaseOf

isUpperCase :: KEYSYM -> Bool
isUpperCase = isJust . lowerCaseOf
