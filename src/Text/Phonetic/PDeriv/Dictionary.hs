module Text.Phonetic.PDeriv.Dictionary where


import qualified Data.Hashable as Ha
import qualified Data.IntMap as IM
import Data.Maybe

    
    
newtype Dictionary a = Dictionary (IM.IntMap a)

instance Show a => Show (Dictionary a) where
    show (Dictionary t) = "" -- todo

empty :: Dictionary a
empty = Dictionary IM.empty 

insert :: Ha.Hashable k => k -> a -> Dictionary a -> Dictionary a
insert key val (Dictionary im) = 
    let key_hash = Ha.hash key
    in key_hash `seq` Dictionary (IM.insert key_hash val im)

lookup :: Ha.Hashable k => k -> Dictionary a -> Maybe a
lookup key (Dictionary im) = 
    let key_hash = Ha.hash key
    in key_hash `seq` 
       IM.lookup key_hash im


update :: Ha.Hashable k => k -> a -> Dictionary a -> Dictionary a
update key val (Dictionary im) = 
    let key_hash = Ha.hash key
        im'      = key_hash `seq` IM.update (\_ -> Just val) key_hash  im
    in Dictionary im'


null :: Dictionary a -> Bool
null (Dictionary im) = IM.null im


fromList :: Ha.Hashable k => [(k,a)] -> Dictionary a 
fromList l = foldl (\d (key,val) -> insert key val d) empty l

    
    
isIn :: Ha.Hashable k => k -> Dictionary a -> Bool
isIn k d = isJust (Text.Phonetic.PDeriv.Dictionary.lookup k d)

values :: Dictionary a -> [a] 
values (Dictionary im) = IM.elems im