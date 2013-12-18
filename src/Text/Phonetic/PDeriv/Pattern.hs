module Text.Phonetic.PDeriv.Pattern where

import Data.List
import qualified Data.ByteString.Char8 as S
import qualified Data.Hashable as Ha

data Pat = PPhi 
         | PEmp     -- ^ empty pattern
         | PLit Sym Conf -- ^ literal pattern
         | PSeq Pat Pat -- ^ Sequence pattern
         | PChoice [Pat] -- ^ Choice pattern
         | PStar Pat -- ^ Star pattern
           deriving (Show, Eq)


instance Ha.Hashable Pat where
    hashWithSalt salt PPhi = 1  
    hashWithSalt salt PEmp = 3    
    hashWithSalt salt (PLit sym conf) = Ha.hashWithSalt salt (Ha.hashWithSalt 5 $ Ha.hashWithSalt (Ha.hash sym) conf)
    hashWithSalt salt (PSeq p1 p2) = Ha.hashWithSalt salt $ Ha.hashWithSalt 7 $ Ha.hashWithSalt (Ha.hash p1) p2
    hashWithSalt salt (PChoice ps) = Ha.hashWithSalt salt $ Ha.hashWithSalt 11 ps
    hashWithSalt salt (PStar p1) = Ha.hashWithSalt salt $ Ha.hashWithSalt 13 $ p1


type Sym = S.ByteString
type Conf = Float

type Sound = [Sym] 

-- | returns all symbols appearing in a pattern
sigma :: Pat -> [Sym]
sigma r = let s = (sigmaSub r)
          in s `seq` nub s

sigmaSub (PLit s _) = [s]
sigmaSub (PSeq p1 p2) = (sigmaSub p1) ++ (sigmaSub p2) 
sigmaSub (PChoice ps) = concatMap sigmaSub ps
sigmaSub (PStar p) = sigmaSub p
sigmaSub PPhi = []
sigmaSub PEmp = []


isAny :: Sym -> Bool
isAny s = (S.unpack s) == "_"

       
isEmpty :: Pat -> Bool
isEmpty PEmp = True
isEmpty (PSeq p1 p2) = isEmpty p1 && isEmpty p2
isEmpty (PChoice ps) = any isEmpty ps
isEmpty (PStar _) = True
isEmpty _ = False


mkLitPattern :: (S.ByteString, Float) -> Pat
mkLitPattern (s,c) = PLit s c

mkChoicePattern :: [Pat] -> Pat
mkChoicePattern [] = PPhi
mkChoicePattern [p] = PChoice [p]
mkChoicePattern ps@(_:_) = PChoice (ps)

mkSequencePattern :: [Pat] -> Pat
mkSequencePattern [] = PEmp
mkSequencePattern [p] = p
mkSequencePattern xs@(p:ps) = foldl PSeq p ps

addEmp :: [Pat] -> [Pat]
addEmp ps = PEmp:ps

interleave :: [a] -> a -> [a]
interleave [] y = []
interleave (x:xs) y = x:y:(interleave xs y)
