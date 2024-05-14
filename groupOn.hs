import Data.List (unfoldr, partition)
import Data.Maybe (catMaybes)
-- import Criterion.Main (defaultMain, env, bgroup, bench, nf)
-- import System.Random (randomIO)
import Control.Monad (replicateM)

groupOn :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupOn k = unfoldr f . map (\x -> (k x, x))
  where
    f [] = Nothing
    f ((k,x):xs) = Just ((k , x : map snd ys), zs)
      where
        (ys,zs) = partition ((k==) . fst) xs

groupOnOrd :: Ord k => (a -> k) -> [a] -> [(k,[a])]
groupOnOrd k = catMaybes . go . map (\x -> (k x, x))
  where
    go [] = []
    go ((k,x):xs) = Just (k, x : e) : merge m (go l) (go g)
      where
        (e, m, l, g) = foldr split ([],[],[],[]) xs

        split ky@(k',y) ~(e, m, l, g) = case compare k' k of
          LT -> (  e, LT : m, ky : l,      g)
          EQ -> (y:e, EQ : m,      l,      g)
          GT -> (  e, GT : m,      l, ky : g)

    merge []        lt     gt     = []
    merge (EQ : xs) lt     gt     = Nothing : merge xs lt gt
    merge (LT : xs) (l:lt) gt     = l       : merge xs lt gt
    merge (GT : xs) lt     (g:gt) = g       : merge xs lt gt

prop_groupOn xs = groupOn (`rem` n) xs == groupOnOrd (`rem` n) xs
  where n = 5

{-

main =
  defaultMain
    [ env (replicateM m randomIO) $ \xs ->
        bgroup (show m)
          (
          [ bgroup "id"
            [ bench "groupOn"    $ nf (groupOn id) xs
            , bench "groupOnOrd" $ nf (groupOnOrd id) xs
            ]
          ] ++
          [ bgroup (show (n :: Word))
            [ bench "groupOn"    $ nf (groupOn (`rem` n)) xs
            , bench "groupOnOrd" $ nf (groupOnOrd (`rem` n)) xs
            ]
          | n <- [2,3,100,1000], n < toEnum m ]
          )
    | p <- [2,3,4], let m = 10 ^ p ]

-}
