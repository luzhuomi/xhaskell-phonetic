-- | define sound pattern construction of english terms
module Text.Phonetic.PDeriv.Consonant where

import qualified Data.ByteString.Char8 as S
import Data.Maybe (isJust)
import Data.List (inits)
import Data.Char (toLower)

import qualified Text.Phonetic.PDeriv.Dictionary as D


consonantsList = map S.pack [ "b", "p", "f", "ph", "v", "w", "wh", "c", "ch", "g", "gh", "j", "k", "q" ,"s", "ps", "sh", "x", "z", "dg", "d","t","th","l","n","ng","kn","m","y","r","rh","h" ]
consonantsDict = D.fromList $ zip consonantsList (repeat ())

mConsonantsList = map S.pack [ "ng", "ps" ]    -- minor
wConsonantsList = map S.pack [ "r", "l", "n" ] -- weak
eConsonantsList = map S.pack [ "n", "r", "y" ] -- ending



