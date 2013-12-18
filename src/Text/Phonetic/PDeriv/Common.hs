module Text.Phonetic.PDeriv.Common where


import qualified Data.ByteString.Char8 as S


bs_splitBy :: (Char -> Bool) -> S.ByteString -> [S.ByteString]
bs_splitBy f bstr | S.null bstr = []
		  | otherwise = let (first, rest) = S.break f bstr
				in first : bs_splitBy f (S.dropWhile f rest)

      


bs_splitBy' :: (Char -> Bool) -> S.ByteString -> [S.ByteString]
bs_splitBy' f l = case S.uncons l of
		   Nothing -> []
		   Just _  -> 
		       let (first, rest) = S.break f l
		       in if S.length rest == 1
			  then first : [ S.empty ]
			  else first : bs_splitBy' f (tail' rest)
    where tail' xs | S.null xs = S.empty
		   | otherwise = S.tail xs

isTab :: Char -> Bool
isTab '\t' = True
isTab _	   = False
