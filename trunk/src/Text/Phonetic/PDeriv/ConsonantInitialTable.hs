-- The ConsonantsIntialis table records the similarity between the consonants (found in english) and
-- the initials (found in Chinese)
-- The mkConsonantsInitialsPattern function construct a pattern which acc
module Text.Phonetic.PDeriv.ConsonantInitialTable where

import qualified Data.ByteString.Char8 as S
import Data.Maybe

import Text.Phonetic.PDeriv.Common
import qualified Text.Phonetic.PDeriv.Dictionary as D
import Text.Phonetic.PDeriv.Pattern 
import Text.Phonetic.PDeriv.Consonant

-- an : Asian language
data ConsonantInitialTable = ConsonantInitialTable 
    { anToEn :: D.Dictionary [(S.ByteString, Float)]
    , enToAn :: D.Dictionary [(S.ByteString, Float)]
    } 

emptyTable = ConsonantInitialTable D.empty D.empty 

-- | parse a tsv file into a CI table.
-- | the file is in the following in format
-- |   b   p    m   f ...
-- | b 1.0 0.5
-- | p 

parseConsonantInitialTable :: FilePath -> IO ConsonantInitialTable 
parseConsonantInitialTable fp = do 
  { c <- S.readFile fp
  ; let lns = S.lines c
  ; case lns of 
     { [] -> error ("The file " ++ fp ++ "is empty.")
     ; (header:lns') -> 
         let (_:initials) = parseHeader header -- the header contains a list of pinyin initials
         in return $ foldl (forEachRow initials) emptyTable lns'
     }
  }                       
    where forEachRow :: [S.ByteString] -> ConsonantInitialTable -> S.ByteString -> ConsonantInitialTable
          forEachRow initials tab ln = 
              let (consonant:scores) = bs_splitBy' isTab ln
                  initials_scores = map (\(x, Just y) -> (x,y)) (filter (\p -> isJust (snd p)) (zip initials (map parseScore scores)))
                  anDict = anToEn tab
                  enDict = enToAn tab
                  enDict' = D.insert consonant initials_scores enDict
                  anDict' = foldl (\d (initial,score) -> 
                                       case D.lookup initial d of
                                         Nothing  -> D.insert initial [(consonant,score)] d
                                         Just kvs -> D.update initial (kvs ++ [(consonant,score)]) d
                                  ) anDict initials_scores
              in tab{anToEn = anDict', enToAn = enDict' }
          parseHeader = bs_splitBy' isTab
          parseScore :: S.ByteString -> Maybe Float
          parseScore s | S.length s > 0 = Just (read (S.unpack s))
                       | otherwise      = Nothing 

initialToConsonants :: S.ByteString -> ConsonantInitialTable -> [(S.ByteString, Float)]
initialToConsonants initial tab = 
    case D.lookup initial (anToEn tab) of
      Nothing -> []
      Just kvs -> kvs

consonantToInitials :: S.ByteString -> ConsonantInitialTable -> [(S.ByteString, Float)]
consonantToInitials consonant tab = 
    case D.lookup consonant (enToAn tab) of
      Nothing -> []
      Just kvs -> kvs


isConsonant :: S.ByteString -> ConsonantInitialTable -> Bool
isConsonant consonant tab = 
    case D.lookup  consonant (enToAn tab) of
      Nothing -> False
      Just _  -> True
      
mkConsonantsInitialsPattern :: S.ByteString -> ConsonantInitialTable -> Pat
mkConsonantsInitialsPattern str ciTab = 
    let consonants = toConsonants str
        initialScores :: [[(S.ByteString,Float)]] 
        initialScores = map (\c -> consonantToInitials c ciTab) consonants 
        pattern  = mkSequencePattern (interleave (map (mkChoicePattern . addEmp .  (map mkLitPattern)) initialScores) anyInitials)
    in pattern



-- question: given a funky (singlish) word "xyz" how do I know which english word to match? Trying all seems silly.
-- we need a way to organize all the words that we are interested and compare
--  we use soundex and metaphone for indexing? 
-- answer: we don't need to. we apply the comparison to those OOV words only.