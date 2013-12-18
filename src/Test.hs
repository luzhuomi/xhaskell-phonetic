module Test where

import qualified Data.ByteString.Char8 as S

import Text.Phonetic.PDeriv.Match
import Text.Phonetic.PDeriv.Extract
import Text.Phonetic.PDeriv.SoundMap
import Text.Phonetic.PDeriv.Vowel
import Text.Phonetic.PDeriv.Consonant
import qualified Text.Phonetic.PDeriv.Dictionary as D

phonetic_distance :: ConsonantTable -> VowelTable -> S.ByteString -> S.ByteString -> Float
phonetic_distance ctab vtab en sg = 
  let 
      consonant_dict = D.fromList (map (\x -> (x,x)) (rowIDs ctab) ) -- assuming it is the rowIds are the english consonants
      row_dict       = D.fromList (map (\x -> (x,x)) (rowIDs vtab) )
      
      sConsonants = getConsonantsFromSentence sg consonant_dict row_dict
      sVowels     = getVowelsFromSentence sg consonant_dict row_dict
      
      eConsonants = getConsonantsFromSentence en consonant_dict row_dict
      eVowels     = getVowelsFromSentence en consonant_dict row_dict      
      
      eConPattern = mkConsonantPattern eConsonants ctab
      eVowPattern = mkVowelPattern eVowels vtab
  in ((match eConPattern sConsonants) + (match eVowPattern sVowels)) / (fromIntegral ((length sConsonants) + (length sVowels)))
     


phonetic_distance2 :: ConsonantTable -> VowelTable -> S.ByteString -> S.ByteString -> Float
phonetic_distance2 ctab vtab en sg = 
  let 
      consonant_dict = D.fromList (map (\x -> (x,x)) (rowIDs ctab) ) -- assuming it is the rowIds are the english consonants
      row_dict       = D.fromList (map (\x -> (x,x)) (rowIDs vtab) )
      
      sSounds = getSoundsFromSentence sg consonant_dict row_dict
      eSounds = getSoundsFromSentence en consonant_dict row_dict      
      
      eSoundPattern = mkSoundPattern eSounds ctab vtab
  in (match eSoundPattern sSounds) / (fromIntegral (length sSounds))
     
singlish_consonant_table :: IO ConsonantTable
singlish_consonant_table = parseSoundMap "../resources/consonants_consonants_snglish.dct"


singlish_vowel_table :: IO VowelTable
singlish_vowel_table = parseSoundMap "../resources/consonants_vowels_snglish.dct"

