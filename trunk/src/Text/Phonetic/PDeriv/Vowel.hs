-- | define sound pattern construction of english terms
module Text.Phonetic.PDeriv.Vowel where

import qualified Data.ByteString.Char8 as S
import Data.Maybe (isJust)
import Data.List (inits)
import Data.Char (toLower)
import qualified Text.Phonetic.PDeriv.Dictionary as D


vowelsList = map S.pack [ "a", "e", "i", "o", "u", "y", "ah", "ar", "al", "all", "aw", "er" , "el" , "ell", "ew", "il", "ill", "ir", "or", "ol", "oll", "ow", "ur", "ul", "ull", "ough", "igh" ] 
vowelsDict = D.fromList $ zip vowelsList (repeat ())
eVowelsList = map S.pack [ "e" ]


