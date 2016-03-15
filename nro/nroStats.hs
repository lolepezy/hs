import           Data.List       (sortBy)
import           Test.QuickCheck

data Interval i p = Interval i i (Payload p)
  deriving (Show, Eq)

data Payload p = Payload Int p
  deriving (Show, Eq)

priority :: Payload p -> Int
priority (Payload i _) = i

intersect :: Ord i => Interval i p -> Interval i p -> Bool
intersect (Interval b1 e1 _) (Interval b2 e2 _) =
  b1 < e2 && b1 >= b2 || b2 < e1 && b2 >= b1


merge :: (Ord i, Show i, Show p) => [Interval i p] -> [Interval i p]
merge [] = []
merge ints = filter (\(Interval b e _) -> b /= e) $
  concatMap resolve $
  divideToSegments $ sortBy compareI ints
  where
    compareI (Interval b1 _ _) (Interval b2 _ _) = compare b1 b2

    resolve [] = []
    resolve (s:others) = foldl (flip addTo) [s] others

    addTo (Interval bs es ps) dest = filter (\(Interval b e _) -> b /= e) $ go dest
      where
        srcApplied dst@(Interval bd ed pd)
          | bs >= ed                         = [ dst ]
          | bs >= bd && bs <= ed && es > ed  = [ Interval bd bs pd, Interval bs ed (winner ps pd) ]
          | bs >= bd && es <= ed             = [ Interval bd bs pd, Interval bs es (winner ps pd), Interval es ed pd ]
          | bs < bd  && es > ed              = [ Interval bd ed (winner ps pd) ]
          | bs < bd  && es <= ed && es > bd  = [ Interval bd es (winner ps pd) ]
          | es <= bd                          = [ dst ]

        go [] = []
        go [d@(Interval _ ed _)] = srcApplied d ++ [ Interval ed es ps | es > ed ]
        go (d : dests)           = srcApplied d ++ go dests

        winner p1 p2 = if priority p1 < priority p2 then p1 else p2

    divideToSegments [] = []
    divideToSegments (i:iis) =
      case splitIntersecting [i] iis of
        ([], x)             -> [x]
        (x, [])             -> [x]
        (segment, leftover) -> segment : divideToSegments leftover
      where
        splitIntersecting b [] = (b, [])
        splitIntersecting b x@(ii:is)
          | any (`intersect` ii) b = splitIntersecting (b ++ [ii]) is
          | otherwise             = (b, x)



prop_noEmptyIntervals :: [Interval Int Int] -> Bool
prop_noEmptyIntervals ints =
  let merged = merge ints
    in not $ any (\(Interval b e _) -> b == e) merged

prop_nonIntersecting :: [Interval Int Int] -> Bool
prop_nonIntersecting ints =
  let merged = merge ints
    in null [ (i1, i2) | i1 <- merged, i2 <- merged, i1 /= i2 && i1 `intersect` i2]


instance (Ord i, Arbitrary i, Arbitrary p) => Arbitrary (Interval i p) where
  arbitrary = do
    b <- arbitrary
    e <- suchThat arbitrary (>b)
    p <- arbitrary
    x <- arbitrary
    return $ Interval b e (Payload p x)


main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_noEmptyIntervals
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonIntersecting
