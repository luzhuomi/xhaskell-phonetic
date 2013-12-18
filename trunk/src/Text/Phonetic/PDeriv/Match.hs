module Text.Phonetic.PDeriv.Match where

import Data.List
import qualified Data.ByteString.Char8 as S
import qualified Data.IntMap as IM
import qualified Data.Hashable as Ha
import System.IO.Unsafe (unsafePerformIO)

import qualified Text.Phonetic.PDeriv.Dictionary as D
import Text.Phonetic.PDeriv.Pattern 

{-
-- | describe the pattern of the phonetic matching
-- PATTERNS     p :: = <> || l || p,p || (p | p) || p*
-- LITERALS     l :: = (s : c)
-- SYMBOLS      s :: = 'b' | 'p' | ... | _ 
-- CONFIDENCE   c :: = <float between -1.0 to 1.0> 
-- 
-- SOUND VAULES v :: = s,...,s 
-- 
-- matching algorithm
-- insights coming from regexp matching
-- 1) a sound is just like a sequence of symbols!
-- the order MATTERS a lot!! Hence the edit-distance matching is not very useful here. (hypothesis!?)
-- 2) 
-- v \match p ~> score
--
-- s \match (s : c) ~> c
--
-- v ~ v1,v2 such that v1 is greedy (or shall we max c3)
-- v1 \match p1 ~> c1 
-- v2 \match p2 ~> c2
-- c1 + c2 = c3
-- ------------------------ 
-- v \match (p1, p2) ~> c3
--
-- v \match p1 ~> c1   v \match p2 ~> c2
-- ------------------------
-- v \match (p1|p2) ~> max(c1,c2)
-- 
 -}




match :: Pat -> Sound -> Conf
match p s
    | length s > 0  =
    let pcs = matchIterator s [(p,0.0)] 
        pcs' = pcs `seq` sortBy (\(p1,c1) (p2,c2) -> compare c2 c1) pcs
    in case pcs' of
         [] -> -1.0
         ((pc@(p,c)):_) -> c 
    | otherwise = -1.0

matchIterator :: Sound -> [(Pat,Conf)] -> [(Pat,Conf)]
matchIterator [] pcs = pcs 
matchIterator (l:ls) pcs = 
    let pcs' = pcs `seq`  nub2 (sort2 [ (p', f' c) | (p, c) <- pcs, (p',f') <- pd l p ])
    in pcs' `seq` matchIterator ls pcs'

nub2 :: Eq a => [(a,b)] -> [(a,b)]
nub2 = nubBy (\(p1,c1) (p2, c2) -> p1 == p2)

sort2  :: Ord b => [(a,b)] -> [(a,b)]
sort2  = sortBy (\ (p1,c1) (p2,c2) -> compare c2 c1)

-- partial derivatives 
pd :: Sym -> Pat -> [(Pat, Conf -> Conf)]
pd s PEmp = []
pd s (PLit s' c) 
    | isAny s'  = [(PEmp, (\x -> c + x))]
    | s == s'   = [(PEmp, (\x -> c + x ))]
    | otherwise = [(PEmp, (\x -> x - 1.0 ))]
pd s (PSeq p1 p2) 
    | isEmpty p1 =
        p1 `seq` p2 `seq` [ (PSeq p1' p2, op) | (p1', op) <- pd s p1 ] ++ (pd s p2)
    | otherwise =  p1 `seq` [ (PSeq p1' p2, op) | (p1', op) <- pd s p1 ] 
pd s (PChoice ps) = ps `seq` concatMap (\p -> pd s p) ps
pd s (PStar p) = p `seq` [ (PSeq p' (PStar p), op) | (p', op) <- pd s p ]





-- test data

s = map S.pack ["a","b","c","d","f","g","h","j"]

p = foldl PSeq anyPat (take 13 (repeat anyPat))
    where anyPat = PStar (mkLitPattern (S.pack "_", -1.0))

-- cp = compilePat s p 



-- compilation scheme

{-
we compile all the possible partial derivative operation into a table
The table maps key to a set of target integer states and their corresponding
binder update functions. 
-}




-- | the table of PDs

type PdTable = IM.IntMap [(Int, Conf -> Conf)]


-- | the FSA
type FSA = (PdTable, [Int])


-- | A function that builds the above table from the pattern

buildPdTable :: Pat -> [Sym] -> (PdTable, [Int])
buildPdTable init sig = 
     let -- sig = sigma init         -- ^ the sigma
         init_dict = init `seq` D.insert init (init,0) D.empty         -- ^ add init into the initial dictionary
         (all, delta, dictionary) = sig `seq` init_dict `seq` init `seq` builder sig [] [] [init] init_dict 1   -- ^ all states and delta
         final = all `seq`  delta `seq` dictionary `seq` [ s | s <- all, isEmpty s]                   -- ^ the final states
         sfinal = map (mapping dictionary) final
         lists = delta `seq` dictionary `seq`
             [ (i,l,jfs) | 
               (p,l, qfs) <- delta, 
               let i = mapping dictionary p
                   jfs = map (\(q,f) -> (mapping dictionary q, f)) qfs
             ]
         hash_table = lists `seq` 
                      foldl (\ dict (p,x,q) -> 
                                 let k = my_hash p x
                                 in case IM.lookup k dict of 
                                      Just ps -> error "Found a duplicate key in the PdPat0Table, this should not happend."
                                      Nothing -> IM.insert k q dict) IM.empty lists
     in hash_table `seq` sfinal `seq` (hash_table, sfinal)

my_hash :: Int -> Sym -> Int
my_hash i x = (fromIntegral $ Ha.hash x) + 256 * i

-- | the lookup function
my_lookup :: Int -> Sym -> IM.IntMap [Int] -> [Int]
my_lookup i x dict = case IM.lookup (my_hash i x) dict of
                     Just ys -> ys
                     Nothing -> []



mapping :: D.Dictionary (Pat,Int) -> Pat -> Int
mapping dictionary x = case D.lookup x dictionary of
  { (Just (_,i)) -> i
  ; Nothing -> error ("this should not happen. looking up " ++ (show x) )
  }
                                            
builder :: [Sym] -- ^ sigma 
        -> [Pat] 
        -> [(Pat, Sym, [(Pat, Conf -> Conf)] )]
        -> [Pat] 
        -> D.Dictionary (Pat,Int)
        -> Int 
        -> ([Pat], [(Pat, Sym, [(Pat, Conf -> Conf)])], D.Dictionary (Pat,Int))
builder sig acc_states acc_delta curr_states dict max_id 
    | null curr_states  = (acc_states, acc_delta, dict)
    | otherwise = 
        let 
            all_sofar_states = acc_states ++ curr_states
            new_delta = curr_states `seq` sig `seq` [ (s, l, sfs) | s <- curr_states, l <- sig, let sfs = pd l s]
            new_states = all_sofar_states `seq` dict `seq` new_delta `seq` 
                         nub [ s' | (_,_,sfs) <- new_delta, (s',f) <- sfs
                                  , not (s' `D.isIn` dict) ]
            acc_delta_next  = acc_delta `seq`  new_delta `seq`  (acc_delta ++ new_delta)
            (dict',max_id') = new_states `seq` foldl (\(d,id) p -> (D.insert p (p,id) d, id + 1) ) (dict,max_id) new_states
        in dict' `seq` max_id' `seq` acc_delta_next `seq` all_sofar_states `seq` builder sig all_sofar_states acc_delta_next new_states dict' max_id' 


compilePat :: [Sym] -> Pat -> (PdTable, [Int])
compilePat sigma p =  
    let io = unsafePerformIO (print p)
    in {- io `seq` -}
       pdStateTable `seq` sfinal `seq`  (pdStateTable, sfinal)
    where 
          (pdStateTable,sfinal) = p `seq` sigma `seq` buildPdTable p sigma

lookupPdTable :: PdTable -> (Int,Conf) -> Sym -> [(Int,Conf)]
lookupPdTable pdTable (i,conf) l = 
    case IM.lookup (my_hash i l) pdTable of
      Just pairs -> 
          [ (j, op conf) | (j, op) <- pairs ]
      Nothing -> []


patMatchesIntStatePd :: PdTable -> Sound -> [(Int,Conf)] -> [(Int,Conf)]
patMatchesIntStatePd pdStateTable  w' eps =
    case w' of 
      [] -> eps 
      (l:w) -> 
          let 
              eps' = nub2 $ sort2 [ ep' | ep <- eps, ep' <- lookupPdTable pdStateTable ep l ] 
          in  pdStateTable `seq` w `seq` eps' `seq` patMatchesIntStatePd pdStateTable  w eps'


patMatchIntStateCompiled :: (PdTable, [Int]) -> Sound -> [Conf]
patMatchIntStateCompiled (pdStateTable,sfinal) w = 
  let
    s = 0 
    allresults' = s `seq` pdStateTable `seq` (patMatchesIntStatePd pdStateTable w [(s,0.0)]) 
    -- allresults = allresults' `seq` map snd (filter (\(i,_) -> i `elem` sfinal) allresults' )
    allresults = allresults' `seq` map snd  allresults' 
  in allresults


matchCompiled :: (PdTable, [Int]) -> Sound -> Conf
matchCompiled compiled w =
     compiled `seq` w `seq` 
              case (patMatchIntStateCompiled compiled w) of
                [] -> -100.0
                (c:_) -> c

