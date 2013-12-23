-- | define sound pattern construction of english terms
module Text.Phonetic.PDeriv.Extract where

import qualified Data.ByteString.Char8 as S
import Data.Maybe (isJust)
import Data.List (inits)
import Data.Char (toLower)

import qualified Text.Phonetic.PDeriv.Dictionary as D
import Text.Phonetic.PDeriv.Pattern
import Text.Phonetic.PDeriv.Consonant
import Text.Phonetic.PDeriv.Vowel
import Text.Phonetic.PDeriv.SoundMap


getConsonantsFromSentence :: S.ByteString -> D.Dictionary S.ByteString -> D.Dictionary S.ByteString -> [S.ByteString]
getConsonantsFromSentence str consonants vowels = 
    let ws = S.words str 
    in concatMap (\w -> getConsonantsFromWord w consonants vowels) ws


-- | extract the consonants form a word
-- we first segment the word into a sequence of 
-- consonants and vowels. Drop some 
-- minor consonants that is followed immediately by another consonants
-- e.g. brought -> [ 'b' , 'r', 'ough', 't' ] -> [ 'b', 'r', 't' ]
-- dongfang -> [ 'd', 'f' ]
-- IMPORTANT: the definition of 'minor' consonant is still heuristic. 
-- We need a better rule based system to address this.
getConsonantsFromWord :: S.ByteString -> D.Dictionary S.ByteString -> D.Dictionary S.ByteString -> [S.ByteString]
getConsonantsFromWord w consonants vowels = 
    let segs = segmentWord w consonants vowels 
    in dropMinorConsonantsVowels segs 0


getScores k dict = 
    case D.lookup k dict of
      Nothing -> []
      Just kvs -> kvs


isConsonant :: S.ByteString -> D.Dictionary S.ByteString -> Bool
isConsonant consonant tab = 
    case D.lookup  consonant tab of
      Nothing -> False
      Just _  -> True
      
-- | create a pattern from the ctable assuming the input consonants are the row ids of the consonant table      
mkConsonantPattern :: [S.ByteString]  -- ^ a sequnece of consonants extracted from the sentence or word 
                      -> ConsonantTable -> Pat
mkConsonantPattern consonants cTab = 
    let d = rowScores cTab
        consonantScores :: [[(S.ByteString,Float)]] 
        consonantScores = map (\c -> getScores c d) consonants 
        pattern  = mkSequencePattern (interleave (map (mkChoicePattern . addEmp .  (map mkLitPattern)) consonantScores) anyInitials)
    in pattern


getVowelsFromSentence :: S.ByteString -> D.Dictionary S.ByteString -> D.Dictionary S.ByteString -> [S.ByteString] 
getVowelsFromSentence str consonants vowels = 
  let ws = S.words str
  in concatMap (\w -> getVowelsFromWord w consonants vowels) ws
     
getVowelsFromWord :: S.ByteString -> D.Dictionary S.ByteString -> D.Dictionary S.ByteString-> [S.ByteString]     
getVowelsFromWord w consonants vowels = 
  let segs = segmentWord w consonants vowels
  in dropMinorVowelsConsonants segs 0 
     
     

isVowel :: S.ByteString -> D.Dictionary S.ByteString-> Bool
isVowel vowel tab = 
    case D.lookup  vowel tab of
      Nothing -> False
      Just _  -> True
      
      
-- | create a pattern from the vtable assuming the input vowels are the row ids of the vowel table            
mkVowelPattern :: [S.ByteString]  -- ^ a sequnece of vowels extracted from the sentence or word
                   -> VowelTable -> Pat
mkVowelPattern vowels vTab = 
  let d = rowScores vTab
      vowelScores :: [[(S.ByteString,Float)]] 
      vowelScores = map (\v -> getScores v d) vowels 
      pattern  = mkSequencePattern (interleave (map (mkChoicePattern . addEmp .  (map mkLitPattern)) vowelScores) anyInitials)
  in pattern
     
       
       
     

data Segment = CSeg S.ByteString -- ^ consonant 
             | VSeg S.ByteString -- ^ vowel
               deriving Show

dropMinorConsonantsVowels :: [Segment] -> Int -> [S.ByteString]
dropMinorConsonantsVowels [] _ = []
dropMinorConsonantsVowels ((CSeg x):[]) p -- ^ drop "n" if it's the last element
    | x `elem` eConsonantsList && p > 0
        = []
dropMinorConsonantsVowels ((CSeg x):(segs@((CSeg l):(segs'@((VSeg e):segs''))))) p 
    -- drop l, if l is in between x and e where x is a consonant, e.g. fle -> fe
    | l `elem` wConsonantsList && e `elem` eVowelsList
        = dropMinorConsonantsVowels ((CSeg x):segs') p
dropMinorConsonantsVowels ((CSeg x):(segs@((CSeg y):segs'))) p
    | x `elem` wConsonantsList  -- drop x if x is a weak consonant
        = dropMinorConsonantsVowels segs (p+1)
    | x == y  -- drop x if it is the same as y
        = dropMinorConsonantsVowels segs (p+1)
    | otherwise
        = x:(dropMinorConsonantsVowels segs (p+1))
dropMinorConsonantsVowels ((CSeg x):segs) p -- ^ drop "ps" and "ng" if they are not the first consonant
    | x `elem` mConsonantsList && p > 0
        = dropMinorConsonantsVowels segs (p+1)
    | otherwise
        = x:(dropMinorConsonantsVowels segs (p+1))
dropMinorConsonantsVowels ((VSeg _):segs) p
    = dropMinorConsonantsVowels segs (p+1)



dropMinorVowelsConsonants :: [Segment] -> Int -> [S.ByteString]
dropMinorVowelsConsonants [] _ = []
dropMinorVowelsConsonants ((VSeg x):[]) p -- ^ drop "e" if it's the last element
    | x `elem` eVowelsList && p > 0
        = []
    | otherwise = (x:[])
dropMinorVowelsConsonants ((VSeg x):segs) p
  = x:(dropMinorVowelsConsonants segs (p+1))
dropMinorVowelsConsonants ((CSeg _):segs) p
  = dropMinorVowelsConsonants segs (p+1)




-- | sounds = consonants \cup vowels
getSoundsFromSentence :: S.ByteString -> D.Dictionary S.ByteString -> D.Dictionary S.ByteString -> [S.ByteString]
getSoundsFromSentence str consonants vowels = 
    let ws = S.words str 
    in concatMap (\w -> getSoundsFromWord w consonants vowels) ws
       
getSoundsFromWord :: S.ByteString -> D.Dictionary S.ByteString -> D.Dictionary S.ByteString -> [S.ByteString]
getSoundsFromWord w consonants vowels = 
    map dropSegTag (segmentWord w consonants vowels)
    where dropSegTag (VSeg x) = x
          dropSegTag (CSeg x) = x
       
       
mkSoundPattern :: [S.ByteString]  -- ^ a sequnece of consonants extracted from the sentence or word 
                  -> ConsonantTable 
                  -> VowelTable 
                  -> Pat
mkSoundPattern sounds cTab vTab = 
  let cD = rowScores cTab 
      vD = rowScores vTab
      soundScores :: [[(S.ByteString,Float)]] 
      soundScores = map (\s -> getScores s cD ++ getScores s vD ) sounds
      pattern  = mkSequencePattern (interleave (map (mkChoicePattern . addEmp .  (map mkLitPattern)) soundScores) anyInitials)
    in pattern                    
      

segmentWord :: S.ByteString -> D.Dictionary S.ByteString -> D.Dictionary S.ByteString -> [Segment]
segmentWord w consonantsDict vowelsDict
    | S.null w = []
    | otherwise = 
        let maxLength = maximum (map S.length ((D.values consonantsDict) ++ (D.values vowelsDict)))
            p = S.map toLower (S.take maxLength w)
            prefices = p `seq` reverse $ tail (S.inits p)
            mbConsonants = map (\k -> myLookup k consonantsDict) prefices
            mbVowels     = map (\k -> myLookup k vowelsDict) prefices
        in case (firstJust mbConsonants, firstJust mbVowels) of 
             (Just c, Just v) | S.length v > S.length c -> (VSeg v):(segmentWord (S.drop (S.length v) w) consonantsDict vowelsDict)
                              | otherwise               -> (CSeg c):(segmentWord (S.drop (S.length c) w) consonantsDict vowelsDict)
             (Just c, Nothing) -> (CSeg c):(segmentWord (S.drop (S.length c) w) consonantsDict vowelsDict)
             (Nothing, Just v)  -> ((VSeg v):segmentWord (S.drop (S.length v) w) consonantsDict vowelsDict)
             (Nothing, Nothing) -> segmentWord (S.tail w) consonantsDict vowelsDict
    where myLookup :: S.ByteString -> D.Dictionary S.ByteString -> Maybe S.ByteString
          myLookup k d = case D.lookup k d of
                         Just _ -> Just k
                         Nothing -> Nothing
          firstJust l = let l' = filter isJust l
                        in case l' of 
                             [] -> Nothing
                             (x:_) -> x



-- match with any consonants, but with penalty
anyConsonalts = PStar (mkLitPattern (S.pack "_",-1.0))


-- match with any initials, but with penalty
anyInitials = PStar (mkLitPattern (S.pack "_",-1.0))


-- match with any initials, but with penalty
anyVowels = PStar (mkLitPattern (S.pack "_",-1.0))


mkDict kvs = foldl (\ (d,ml) (k,v) -> let d' = D.insert k v d
                                          l  = S.length k 
                                          ml' | l > ml = l
                                              | otherwise = ml
                                      in (d',ml')) kvs


