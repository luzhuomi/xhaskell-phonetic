-- The Consonant table records the similarity between the consonants (found in english), which can be used to match against mispelled words or singlish
-- The mkConsonantPattern function construct a pattern which accept consonants
module Text.Phonetic.PDeriv.SoundMap where

import qualified Data.ByteString.Char8 as S
import Data.Maybe

import Text.Phonetic.PDeriv.Common
import qualified Text.Phonetic.PDeriv.Dictionary as D
import Text.Phonetic.PDeriv.Pattern 

data SoundMap = SoundMap  
                      { rowIDs :: [S.ByteString]  
                      , colIDs :: [S.ByteString] 
                      , rowScores :: D.Dictionary [(S.ByteString, Float)] -- ^ rowID -> [(colID, score)]
                      , colScores :: D.Dictionary [(S.ByteString, Float)] -- ^ colID -> [(rowID, score)]
                      }


emptyMap = SoundMap [] [] D.empty D.empty

-- | parse a tsv file into a C table.
-- | the file is in the following in format
-- |   b   p    m   f ...
-- | b 1.0 0.5
-- | p 

parseSoundMap :: FilePath -> IO SoundMap 
parseSoundMap fp = do 
  { c <- S.readFile fp
  ; let lns = S.lines c
  ; case lns of 
     { [] -> error ("The file " ++ fp ++ "is empty.")
     ; (header:lns') -> 
         let (_:columnIDs) = parseHeader header -- the header contains a list of pinyin initials
         in return $ foldl (forEachRow columnIDs) emptyMap{colIDs = columnIDs} lns'
     }
  }                       
  where forEachRow :: [S.ByteString] -> SoundMap -> S.ByteString -> SoundMap
        forEachRow columnIDs tab ln = 
          let (rowID:scores) = bs_splitBy' isTab ln
              row_scores = map (\(x, Just y) -> (x,y)) (filter (\p -> isJust (snd p)) (zip columnIDs (map parseScore scores)))
              rScores = D.insert rowID row_scores (rowScores tab)
              cScores = foldl (\d (colID, score) -> 
                                case D.lookup colID d of
                                  { Nothing  -> D.insert colID [(rowID,score)] d
                                  ; Just kvs -> D.update colID (kvs ++ [(rowID,score)]) d 
                                  } ) (colScores tab) row_scores
              rIDs = rowIDs tab
          in tab{rowIDs = rIDs++[rowID], rowScores = rScores, colScores = cScores}
        parseHeader = bs_splitBy' isTab
        parseScore :: S.ByteString -> Maybe Float
        parseScore s | S.length s > 0 = Just (read (S.unpack s))
                     | otherwise      = Nothing 





-- question: given a funky (singlish) word "xyz" how do I know which english word to match? Trying all seems silly.
-- we need a way to organize all the words that we are interested and compare
--  we use soundex and metaphone for indexing? 
-- answer: we don't need to. we apply the comparison to those OOV words only.