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
resolve (s:others) = go [s] others
  where
    go divided [] = divided
    go divided (s1 : ss) = go (s1 `addTo` divided) ss


addTo :: Ord i => Interval i p -> [Interval i p] -> [Interval i p]
addTo src@(Interval bs es ps) dest = filter (\(Interval b e _) -> b /= e) $ go dest
  where
    srcApplied dst@(Interval bd ed pd)
      | bs >= ed                         = [ dst ]
      | bs >= bd && bs <= ed && es > ed  = [ Interval bd bs pd, Interval bs ed (winner ps pd) ]
      | bs >= bd && es <= ed             = [ Interval bd bs pd, Interval bs es (winner ps pd), Interval es ed pd ]
      | bs < bd  && es > ed              = [ Interval bs bd ps, Interval bd ed (winner ps pd), Interval ed es ps ]
      | bs < bd  && es <= ed && es > bd  = [ Interval bs bd ps, Interval bd es (winner ps pd), Interval es ed pd ]
      | es < bd                          = [ dst ]

    go [] = []
    go [d@(Interval _ ed _)] = srcApplied d ++ [ Interval ed es ps | es > ed ]
    go (d : dests)           = srcApplied d ++ go dests

    winner p1 p2 = if priority p1 < priority p2 then p1 else p2

divideToSegments [] = []
divideToSegments (i:ints) =
  case splitIntersecting [i] ints of
    ([], x)             -> [x]
    (x, [])             -> [x]
    (segment, leftover) -> segment : divideToSegments leftover
  where
    splitIntersecting b [] = (b, [])
    splitIntersecting b x@(ii:is)
      | any (`intersect` i) b = splitIntersecting (b ++ [ii]) is
      | otherwise             = (b, x)



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

  let i1 = Interval 0 18 (Payload 2 ())
  let i2 = Interval 5 15 (Payload 0 ())
  let i3 = Interval 7 20  (Payload 1 ())
  let i4 = Interval 20 30 (Payload 1 ())

  -- print $ resolve [ i1, i2, i3 ]
  print $ merge [ i1, i2, i3 ]
