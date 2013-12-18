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
      vowel_dict       = D.fromList (map (\x -> (x,x)) (rowIDs vtab) )
      
      sConsonants = getConsonantsFromSentence sg consonant_dict vowel_dict
      sVowels     = getVowelsFromSentence sg consonant_dict vowel_dict
      
      eConsonants = getConsonantsFromSentence en consonant_dict vowel_dict
      eVowels     = getVowelsFromSentence en consonant_dict vowel_dict      
      
      eConPattern = mkConsonantPattern eConsonants ctab
      eVowPattern = mkVowelPattern eVowels vtab
  in ((match eConPattern sConsonants) + (match eVowPattern sVowels)) / (fromIntegral ((length sConsonants) + (length sVowels)))
     


phonetic_distance2 :: ConsonantTable -> VowelTable -> S.ByteString -> S.ByteString -> Float
phonetic_distance2 ctab vtab en sg = 
  let 
      consonant_dict = D.fromList (map (\x -> (x,x)) (rowIDs ctab) ) -- assuming it is the rowIds are the english consonants
      vowel_dict       = D.fromList (map (\x -> (x,x)) (rowIDs vtab) )
      
      sSounds = getSoundsFromSentence sg consonant_dict vowel_dict
      eSounds = getSoundsFromSentence en consonant_dict vowel_dict      
      
      eSoundPattern = mkSoundPattern eSounds ctab vtab
  in (match eSoundPattern sSounds) / (fromIntegral (length sSounds))
     
     
phonetic_en_cn_consonant_only :: ConsonantTable -> S.ByteString -> S.ByteString -> Float     
phonetic_en_cn_consonant_only ctab en cn = 
  let consonant_dict = D.fromList (map (\x -> (x,x)) (rowIDs ctab) ) -- assuming it is the rowIds are the english consonants
      tctab          = transpose ctab
      initial_dict = D.fromList (map (\x -> (x,x)) (rowIDs tctab )) -- assuming it is the colIds are the chinese initials
      vowel_dict     = D.empty
      cInitials = getConsonantsFromSentence cn initial_dict vowel_dict
      eConsonants = getConsonantsFromSentence en consonant_dict vowel_dict      
      eConPattern = mkConsonantPattern eConsonants ctab
      cIniPattern = mkConsonantPattern cInitials tctab      
  in ((match eConPattern cInitials) + (match cIniPattern eConsonants)) / (fromIntegral ((length eConsonants) + (length cInitials)))
      
      
     
singlish_consonant_table :: IO ConsonantTable
singlish_consonant_table = parseSoundMap "../resources/consonants_consonants_snglish.dct"


singlish_vowel_table :: IO VowelTable
singlish_vowel_table = parseSoundMap "../resources/consonants_vowels_snglish.dct"


