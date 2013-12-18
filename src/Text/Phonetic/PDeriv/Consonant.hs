-- | define sound pattern construction of english terms
module Text.Phonetic.PDeriv.Consonant where

import qualified Data.ByteString.Char8 as S
import Data.Maybe (isJust)
import Data.List (inits)
import Data.Char (toLower)

import qualified Text.Phonetic.PDeriv.Dictionary as D

import Text.Phonetic.PDeriv.SoundMap

type ConsonantTable = SoundMap

mConsonantsList = map S.pack [ "ng", "ps" ]    -- minor
wConsonantsList = map S.pack [ "r", "l", "n" ] -- weak
eConsonantsList = map S.pack [ "n", "r", "y" ] -- ending



