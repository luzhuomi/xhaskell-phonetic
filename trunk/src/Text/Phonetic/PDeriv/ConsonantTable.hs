-- The Consonant table records the similarity between the consonants (found in english), which can be used to match against mispelled words or singlish
-- The mkConsonantPattern function construct a pattern which accept consonants
module Text.Phonetic.PDeriv.ConsonantTable where

import qualified Data.ByteString.Char8 as S
import Data.Maybe

import Text.Phonetic.PDeriv.Common
import qualified Text.Phonetic.PDeriv.Dictionary as D
import Text.Phonetic.PDeriv.Pattern 
import Text.Phonetic.PDeriv.Consonant

type ConsonantTable = D.Dictionary [(S.ByteString, Float)]


emptyTable = D.empty

-- | parse a tsv file into a C table.
-- | the file is in the following in format
-- |   b   p    m   f ...
-- | b 1.0 0.5
-- | p 

parseConsonantTable :: FilePath -> IO ConsonantTable 
parseConsonantTable fp = do 
  { c <- S.readFile fp
  ; let lns = S.lines c
  ; case lns of 
     { [] -> error ("The file " ++ fp ++ "is empty.")
     ; (header:lns') -> 
         let (_:initials) = parseHeader header -- the header contains a list of pinyin initials
         in return $ foldl (forEachRow initials) emptyTable lns'
     }
  }                       
  where forEachRow :: [S.ByteString] -> ConsonantTable -> S.ByteString -> ConsonantTable
        forEachRow initials tab ln = 
          let (consonant:scores) = bs_splitBy' isTab ln
              initials_scores = map (\(x, Just y) -> (x,y)) (filter (\p -> isJust (snd p)) (zip initials (map parseScore scores)))
          in D.insert consonant initials_scores tab
        parseHeader = bs_splitBy' isTab
        parseScore :: S.ByteString -> Maybe Float
        parseScore s | S.length s > 0 = Just (read (S.unpack s))
                     | otherwise      = Nothing 





-- question: given a funky (singlish) word "xyz" how do I know which english word to match? Trying all seems silly.
-- we need a way to organize all the words that we are interested and compare
--  we use soundex and metaphone for indexing? 
-- answer: we don't need to. we apply the comparison to those OOV words only.