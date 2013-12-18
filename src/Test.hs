module Test where

import qualified Data.ByteString.Char8 as S

import Text.Phonetic.PDeriv.Match
import Text.Phonetic.PDeriv.Extract
import Text.Phonetic.PDeriv.ConsonantTable
import Text.Phonetic.PDeriv.VowelTable

phonetic_distance :: ConsonantTable -> VowelTable -> S.ByteString -> S.ByteString -> Float
phonetic_distance ctab vtab en sg = 
  let 
      sConsonants = toConsonants sg ctab
      sVowels     = toVowels sg vtab
      eConPattern = mkConsonantsPattern en ctab
      eVowPattern = mkVowelsPattern en vtab
  in ((match eConPattern sConsonants) + (match eVowPattern sVowels)) / 2
     
     
singlish_consonant_table :: IO ConsonantTable
singlish_consonant_table = parseConsonantTable "../resources/consonants_consonants_snglish.dct"


singlish_vowel_table :: IO ConsonantTable
singlish_vowel_table = parseVowelTable "../resources/consonants_vowels_snglish.dct"


