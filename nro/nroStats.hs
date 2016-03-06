import Data.List (sortBy)

data Interval i p = Interval i i (Payload p)
  deriving (Show, Eq)

data Payload p = Payload Int p
  deriving (Show, Eq)

priority :: Payload p -> Int
priority (Payload i _) = i

intersect :: Ord i => Interval i p -> Interval i p -> Bool
intersect (Interval b1 e1 _) (Interval b2 e2 _) =
  b1 < e2 && b1 >= b2 || b2 < e1 && b2 >= b1


compareI :: Ord i => Interval i p -> Interval i p -> Ordering
compareI (Interval b1 e1 pay1) (Interval b2 e2 pay2) =
  compareAnd (compare b1 b2) $ compareAnd (compare e1 e2) $ compare (priority pay1) (priority pay2)
  where
    compareAnd c f = case c of
      LT -> LT
      GT -> GT
      EQ -> f


merge :: Ord i => [Interval i p] -> [Interval i p]
merge [] = []
merge ints = concatMap resolve $
  divideToSegments $ sortBy compareI ints


resolve [] = []
resolve (s:others) =
  foldl(\rr s1 -> concatMap (\r -> s1 `apply` r) rr) [s] others

apply :: Ord i => Interval i p -> Interval i p -> [Interval i p]
apply i1@(Interval b1 e1 pay1) i2@(Interval b2 e2 pay2) =
  filter (\(Interval b e _) -> b /= e) $
    if b1 < b2 then
      if e1 <= b2 then [ i1, i2 ]
        else if e1 <= e2
          then [ Interval b1 b2 pay1, Interval b2 e1 (winner pay1 pay2), Interval e1 e2 pay2 ]
          else [ Interval b1 b1 pay1, Interval b2 e2 (winner pay1 pay2), Interval e2 e1 pay1 ]
      else
        apply i2 i1

winner p1 p2 = if priority p1 < priority p2 then p1 else p2

divideToSegments ints =
  let (segment, leftover) = intersectingSubset ints
      intersectingSubset [] = ([], [])
      intersectingSubset (i:is) =
        let s = span (`intersect` i) is in (i:fst s, snd s)
      in case leftover of
          [] -> [segment]
          _  -> segment : divideToSegments leftover


main :: IO ()
main = do
  print $ compareI (Interval 1 3 (Payload 0 ())) (Interval 2 5 (Payload 0 ())) == LT
  print $ compareI (Interval 1 3 (Payload 0 ())) (Interval 1 4 (Payload 0 ())) == LT
  print $ compareI (Interval 1 3 (Payload 0 ())) (Interval 1 3 (Payload 1 ())) == LT
  print $ compareI (Interval 1 3 (Payload 0 ())) (Interval 1 2 (Payload 1 ())) == GT
  print $ compareI (Interval 1 3 (Payload 0 ())) (Interval 1 3 (Payload 0 ())) == EQ
  print $ not $ intersect (Interval 1 2 (Payload 0 ())) (Interval 2 5 (Payload 0 ()))
  print $ intersect (Interval 2 3 (Payload 0 ())) (Interval 2 5 (Payload 0 ()))
  print $ not $ intersect (Interval 2 5 (Payload 0 ())) (Interval 1 2 (Payload 0 ()))

  let i1 = Interval 1 3 (Payload 0 ())
  let i2 = Interval 2 4 (Payload 1 ())
  let t1 = [ i1 ]

  print $ merge [ i1, i2 ]
