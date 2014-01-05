
import Data.Char (digitToInt)

digsum = f 0 where
	f a 0 = a
	f a n = f (a+r) q where
		(q,r) = n `divMod` 10


main = print $ foldl max 0 (map digsum [n^z | n <- [1..100], z <- [1..100]])


