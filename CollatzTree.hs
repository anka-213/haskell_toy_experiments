{-# LANGUAGE TupleSections #-}
module CollatzTree where

import Data.Tree
import Data.List ( findIndex, elemIndex, groupBy, sortOn, group, sort )
import Data.Maybe (fromJust)
import Data.Function ( on )

limitDepth :: Int -> Tree a -> Tree a;
limitDepth 0 (Node x _) = Node x [];
limitDepth n (Node x xs) = Node x (limitDepth (n - 1) <$> xs)

limitDownRight :: Int -> Int -> Tree a -> Tree a
-- limitDownRight 0 _ (Node x _) = Node x []
limitDownRight _ 0 (Node x _) = Node x []
limitDownRight n m (Node x []) = Node x []
limitDownRight n m (Node x [y]) = Node x [limitDownRight n (pred m) y]
limitDownRight n m (Node x [y,z])
  | n == 0    = Node x [limitDownRight n (pred m) y]
  | otherwise = Node x [limitDownRight n (pred m) y, limitDownRight (pred n) m z]
limitDownRight n m (Node x xs) = error "limitDownRight: unexpected size"


bwdStep :: Int -> [Int]
bwdStep 4 = [8]
-- bwdStep n | n `mod` 6 == 4 = [n`div`3, 2*n]
bwdStep n | n `mod` 6 == 4 = [2*n, n`div`3]
          | otherwise = [2*n]

fwdStep :: Int -> Int
fwdStep n
  | even n = n`div` 2
  | otherwise = n*3+1

iterateTree :: (a -> [a]) -> a -> Tree a
iterateTree f x = Node x (iterateTree f <$> f x)

collatzTree :: Tree Int
collatzTree = iterateTree bwdStep 1

layers :: Tree a -> [[a]]
layers (Node x xs) = [x] : foldr (zipWith (++)) (repeat []) (fmap layers xs)

findDepth :: Eq a => a -> Tree a -> Maybe Int
findDepth a = findIndex (a `elem`) . layers

getCollatzDepthSlow :: Int -> Int
getCollatzDepthSlow = fromJust . flip findDepth (iterateTree bwdStep 1)

collatzChain :: Int -> [Int]
collatzChain n = takeWhile (/=1) (iterate fwdStep n) ++ [1]

getCollatzDepth :: Int -> Int
getCollatzDepth a = fromJust $ elemIndex 1 $ iterate fwdStep a

-- >>> getCollatzDepth <$> [1..10]
-- [0,1,7,2,5,8,16,3,19,6]

filterNodes :: (a -> Bool) -> Tree a -> Tree a
filterNodes p (Node x xs) = Node x $ filter (p . rootLabel) $ fmap (filterNodes p) xs
--   | p x = Node x (fliterNodes p)
--   | otherwise =

-- printTree . fmap ((`mod` 6). (`div`3)) . filterNodes ((/=0).(`mod`3)) $ limitDepth 16 collatzTree

skipSingleNodes :: Tree a -> Tree a
skipSingleNodes (Node x [y]) = skipSingleNodes y
skipSingleNodes (Node x xs) = Node x $ skipSingleNodes <$> xs

-- printTree . fmap ((`mod` 6). (`div`3)) . filterNodes ((/=0).(`mod`3)) . skipSingleNodes $ limitDepth 16 collatzTree

skipMatchingSingleNodes :: (a -> Bool) -> Tree a -> Tree a
skipMatchingSingleNodes p (Node x [y]) | p x = skipMatchingSingleNodes p y
skipMatchingSingleNodes p (Node x xs) = Node x $ skipMatchingSingleNodes p <$> xs

-- printTree . fmap ((`mod` 6). (`div`3)) . limitDepth 6 . skipNonBranching $ collatzTree

-- Square drawing
-- printTree . fmap ((`mod` 6). (`div`3)) . limitDownRight 4 5 . skipNonBranching $ collatzTree

skipNonBranching :: Tree Int -> Tree Int
skipNonBranching = skipMatchingSingleNodes (\x -> mod x 6 /= 4) . filterNodes ((/=0).(`mod`3))

printSquareBranches :: Int -> Int -> IO ()
printSquareBranches n m = printTree . fmap ((`mod` 6). (`div`3)) . limitDownRight n m . skipNonBranching $ collatzTree

toBase :: Int -> Int -> [Int]
toBase k 0 = []
toBase k n = mod n k : toBase k (div n k)

toBase3 :: Int -> [Int]
toBase3 = toBase 3

d3 :: Int -> Int
d3 = (`div` 3)
m6 :: Int -> Int
m6 = (`mod` 6)



-- printTree . fmap toBase3 . limitDownRight 1 9 . skipMatchingSingleNodes odd $ collatzTree

-- printTree . fmap (`mod` 6) . limitDepth 6 $ iterateTree bwdStep 64

tree1 :: Tree Int
tree1 = m6 <$> iterateTree bwdStep 64
tree2 :: Tree Int
tree2 = m6 <$> iterateTree bwdStep 10

-- printTree $ limitDepth 8 $ zipWithTree (==) tree1 tree2

-- printTree $ limitDepth 5 $ (zipWithTree (,) `on` (fmap m6 . iterateTree bwdStep . (*4) . d3 . (*4) )) 64 10

dropCommonPrefix :: Eq a => [a] -> [a] -> ([a],([a],[a]))
-- dropCommonPrefix [] as = ([],([],as))
-- dropCommonPrefix as [] = ([],(as , []))
dropCommonPrefix (a : as) (a' : as')
  | a == a' = let ~(pre, rest) = dropCommonPrefix as as' in (a : pre, rest)
--   | otherwise = ([],(a:as, a' : as'))
dropCommonPrefix as bs = ([],(as, bs))

zipWithTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWithTree f (Node x xs) (Node y ys) = Node (f x y) (zipWith (zipWithTree f) xs ys)

-- printTree $ limitDepth 5 $ (zipWithTree dropCommonPrefix `on` (fmap toBase3 . iterateTree bwdStep . (*4) . d3 . (*4))) 64 10

-- What happens when doubling numbers base 2?
-- > mapM_ print [[pad 7 0 $ toBase3 (2^k*n) | n <- [1..13], mod n 3 /= 0, mod n 2 /= 0] | k <- [0..7]]
-- Compare to the tree
-- > printTree . fmap toBase3 . limitDepth 9 $ collatzTre

{-

Idea: separate out the common prefix of two subtrees and only print the difference
      using some kind of trie structure

In this, each column could be its own trie:
> printTree . fmap toBase3 . limitDepth 5 . skipNonBranching $ collatzTree
[1,1] - [1,2,1] - [1,0,1,2] - [1,1,1,0,0,1] - [1,2,2,1,0,1,1] - [1,0,2,1,2,1,2,1]
        |                     |               |
        |                     |               `---------------- [1,2,0,1,2,2]
        |                     |
        |                     `-------------- [1,2,1,0,1,1] - [1,0,1,2,1,2,1]
        |                                     |
        |                                     `-------------- [1,0,1,2,2]
        |
        `-------- [1,0,1] - [1,1,1,1] - [1,2,2,2,1] - [1,0,2,2,1,2]
                            |           |
                            |           `------------ [1,2,2,0,1]
                            |
                            `---------- [1,2,2,1] - [1,0,2,1,2]
                                        |
                                        `---------- [1,2,0,1]

Unclear how to display many tries after each other though
maybe a column with the root in the middle and branches up/down

We can calculate the doubling of the common prefix and then promote it to the differing parts
(obviously not more efficient than just working with numbers, but more fun)

-}

-- data Trie a = Trie [a] [Trie a]
type Trie a = Tree [a]

trieFromForkSimple :: Eq a => [a] -> [a] -> Trie a
trieFromForkSimple a b = Node pre [pure aPost, pure bPost]
    where (pre,(aPost, bPost)) = dropCommonPrefix a b

trieFromFork :: Eq a => Trie a -> Trie a -> Trie a
trieFromFork (Node a as) (Node b bs) = Node pre [Node aPost as, Node bPost bs]
    where (pre,(aPost, bPost)) = dropCommonPrefix a b

treeToTries :: Eq a => Tree [a] -> [Trie a]
treeToTries (Node x []) = [pure x]
treeToTries (Node x [xs]) = pure x : treeToTries xs
treeToTries (Node x [y, z]) = pure x : zipWith trieFromFork (treeToTries y) (treeToTries z)
treeToTries (Node x xs) = pure x : error "not implemented"
  where rest = fmap treeToTries xs

-- >>> (!!3) $ treeToTries $ fmap toBase3 . limitDepth 3 . skipNonBranching $ collatzTree
-- Node {rootLabel = [1,1,1], subForest = [Node {rootLabel = [0,0,1], subForest = []},Node {rootLabel = [1], subForest = []}]}

-- printForest $ treeToTries $ fmap toBase3 . limitDepth 4 . skipNonBranching $ collatzTree
-- This doesn't find commonalities between corresponding deeper branches unless shallow branches also agree

type Base3Digit = Int -- 0, 1 or 2
type Base3 = [Base3Digit]

-- >>> doubleBase3 [1,2,2]
-- [2,1,2,1]
doubleBase3 :: Base3 -> Base3
doubleBase3 = go 0
  where
    go 0 [] = []
    go carry [] = [carry]
    go carry (n : ns) = n' : go carry' ns
      where (carry' , n') = divMod (2*n + carry) 3

-- >>> all prop_doubleBase3 [0..100]
-- True
prop_doubleBase3 :: Int -> Bool
prop_doubleBase3 n = toBase3 (2*n) == doubleBase3 (toBase3 n)

div3Base3 :: Base3 -> Base3
div3Base3 (1:xs) = xs
div3Base3 n = error $ "div3Base3: n - 1 not divisible by 3: " ++ show n

remove2s :: Int -> Int
remove2s n | even n = remove2s (n `div` 2) | otherwise = n

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . drawCompactTree . fmap show
printForest :: Show a => Forest a -> IO ()
printForest = putStrLn . drawCompactForest . fmap (fmap show)

printCollatzTree :: Int -> IO ()
printCollatzTree n = printTree $ limitDepth n $ iterateTree bwdStep 1

drawCompactTree :: Tree String -> String
drawCompactTree  = unlines . draw

drawCompactForest :: [Tree String] -> String
drawCompactForest  = unlines . map drawCompactTree

draw :: Tree String -> [String]
draw (Node x ts0) = x <+ drawSubTrees ts0
  where
    drawSubTrees [] = [""]
    drawSubTrees [t] =
        shift " - " (padding ++ "   ") (draw t)
    drawSubTrees [t,t1] =
        shift " - " ("|  " ++ padding) (draw t) ++ ["| "] ++ shift ("`" ++ ('-'<$x) ++ "- ") ("   " ++ padding) (draw t1)
        -- shift " - " ("|  " ++ padding) (draw t) ++ ["| "] ++ shift "" "" (draw t1)
    drawSubTrees (t:ts) =
        shift "+- " "|  " (draw t) ++ drawSubTrees ts

    padding = ' ' <$ x
    shift first other = zipWith (++) (first : repeat other)
    (<+) :: String -> [String] -> [String]
    x <+ [] = [x]
    x <+ (y:ys) = (x ++ y) : ys



{-


mapM_ print [[(2^k * n) `mod` 18 | k <- [0..5]] | n <- [0..17]]

CollatzTree> [ d3 k `mod` 6 | n <- [2,4..22], let m = d3 (2^n), m6 m /= 3, let k = if m6 m == 1 then 4*m else 2*m]
[1,3,5,5,3,1,1,3]
CollatzTree> [ k `mod` 18 | n <- [2,4..22], let m = d3 (2^n), m6 m /= 3, let k = if m6 m == 1 then 4*m else 2*m]
[4,10,16,16,10,4,4,10]
CollatzTree> [ m `mod` 18 | n <- [2,4..22], let m = d3 (2^n), m6 m /= 3]
[1,5,13,17,7,11,1,5]

CollatzTree> printSquareBranches 1 12
1 - 5 - 3 - 1 - 5 - 3 - 1 - 5 - 3 - 1 - 5 - 3 - 1
    |       |   |       |   |       |   |
    |       |   |       |   |       |   3 - 1 - 5
    |       |   |       |   |       |
    |       |   |       |   |       1 - 5 - 3 - 1
    |       |   |       |   |
    |       |   |       |   1 - 5 - 3 - 1 - 5 - 3
    |       |   |       |
    |       |   |       3 - 1 - 5 - 3 - 1 - 5 - 3
    |       |   |
    |       |   5 - 3 - 1 - 5 - 3 - 1 - 5 - 3 - 1
    |       |
    |       5 - 3 - 1 - 5 - 3 - 1 - 5 - 3 - 1 - 5
    |
    3 - 1 - 5 - 3 - 1 - 5 - 3 - 1 - 5 - 3 - 1 - 5

Prelude Data.Tree> putStrLn . drawTree . fmap show $ limitDepth 10 $ iterateTree bwdStep 1
1
|
`- 2
   |
   `- 4
      |
      `- 8
         |
         `- 16
            |
            +- 5
            |  |
            |  `- 10
            |     |
            |     +- 3
            |     |  |
            |     |  `- 6
            |     |     |
            |     |     `- 12
            |     |        |
            |     |        `- 24
            |     |
            |     `- 20
            |        |
            |        `- 40
            |           |
            |           +- 13
            |           |  |
            |           |  `- 26
            |           |
            |           `- 80
            |              |
            |              `- 160
            |
            `- 32
               |
               `- 64
                  |
                  +- 21
                  |  |
                  |  `- 42
                  |     |
                  |     `- 84
                  |        |
                  |        `- 168
                  |
                  `- 128
                     |
                     `- 256
                        |
                        +- 85
                        |  |
                        |  `- 170
                        |
                        `- 512
                           |
                           `- 1024

Prelude Data.Tree> foldr (:) [] $ limitDepth 10 $ iterateTree bwdStep 1
[1,2,4,8,16,5,10,3,6,12,24,20,40,13,26,80,160,32,64,21,42,84,168,128,256,85,170,512,1024]
-}
{-
Prelude Data.Tree Control.Applicative> take 10 $ layers $ iterateTree bwdStep 1
[[1],[2],[4],[8],[16],[5,32],[10,64],[3,20,21,128],[6,40,42,256],[12,13,80,84,85,512]]
-}
{-
Prelude Data.Tree Control.Applicative Data.List> :t findDepth
findDepth :: Eq a => a -> Tree a -> Maybe Int
Prelude Data.Tree Control.Applicative Data.List> flip findDepth (iterateTree bwdStep 1) <$> [1..5]
[Just 0,Just 1,Just 7,Just 2,Just 5]
Prelude Data.Tree Control.Applicative Data.List> fromJust . flip findDepth (iterateTree bwdStep 1) <$> [1..5]

Prelude Data.Tree Control.Applicative Data.List> :m + Data.Maybe
Prelude Data.Tree Control.Applicative Data.List Data.Maybe> fromJust . flip findDepth (iterateTree bwdStep 1) <$> [1..5]
[0,1,7,2,5,8,16,3,19,6]
Prelude Data.Tree Control.Applicative Data.List Data.Maybe> :bindings
unknown command ':bindings'
use :? for help.
Prelude Data.Tree Control.Applicative Data.List Data.Maybe> :show bindings
limitDepth :: Int -> Tree a -> Tree a = _
bwdStep :: Int -> [Int] = _
iterateTree :: (a -> [a]) -> a -> Tree a = _
layers :: Tree a -> [[a]] = _
findDepth :: Eq a => a -> Tree a -> Maybe Int = _
it :: [Int] = [0,1,7,2,5,....]
(reverse-i-search)`limitDepth :': limitDepth :: Int -> Tree a -> Tree a; limitDepth 0 (Node x _) = Node x []; limitDepth n (Node x xs) = Node x (limitDepth (n - 1) <$> xs)

-- Fair dice for n players

12|34
13|24
14|23

  | 3 | 4
--+---+---
1 | < | <
--+---+---
2 | < | <

  | 2 | 4
--+---+---
1 | < | <
--+---+---
3 | > | <

  | 2 | 3
--+---+---
1 | < | <
--+---+---
4 | > | >

  | 2 | 3 | 6 | 7
--+---+---+---+---
1 | < | < | < | <
--+---+---+---+---
4 | > | > | < | <
--+---+---+---+---
5 | > | > | < | <
--+---+---+---+---
8 | > | > | > | >


-}


-- Explanation of the permutations function from Data.OldList

-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- >>> permutations "abc"
-- ["abc","bac","cba","bca","cab","acb"]
permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []

perms :: [a] -> [a] -> [[a]]
perms []     _  = []
perms (t:ts) is = foldr (interleave t ts) (perms ts (t:is)) (permutations is)
permsBase :: [a] -> [a] -> [[a]]
permsBase (t:ts) is = perms ts (t:is)
permsBase _ _ = undefined
interleave :: a -> [a] -> [a] -> [[a]] -> [[a]]
interleave t ts  xs     r = let (_,zs) = interleave' id xs r in zs -- _ = xs ++ ts
    where
    -- interleave' :: ([a] -> [a]) -> [a] -> [[a]] -> ([a], [[a]])
    interleave' _ []     r = (ts, r)
    interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r -- us = ys ++ ts
                             in  (y:us, f (t:y:us) : zs)

-- >>> perms "bc" "a"
-- ["bac","cba","bca","cab","acb"]

-- >>> permsBase "bc" "a"
-- ["cba","bca","cab","acb"]

-- >>> interleave 'B' "C" "a" (permsBase "bc" "a")
-- ["BaC","cba","bca","cab","acb"]

-- >>> interleave '_' " ..." "abc" ["gh","ij"]
-- ["_abc ...","a_bc ...","ab_c ...","gh","ij"]

-- >>> perms "c" "ba"
-- ["cba","bca","cab","acb"]

-- >>> permsBase "c" "ba"
-- []

-- >>> permutations "ba"
-- ["ba","ab"]

-- >>> interleave 'c' "" "ba"  []
-- >>> interleave 'c' "" "ab" []
-- ["cba","bca"]
-- ["cab","acb"]

-- >>> perms "c" "b"
-- ["cb"]

-- Fair dice problem: https://www.youtube.com/watch?v=-64UT8yikng

permutationsSomeIdentical :: [(a, Int)] -> [[a]]
permutationsSomeIdentical [] = [[]]
permutationsSomeIdentical xs =
  [ x : ys
    | (x, xs_) <- select xs,
      ys <- permutationsSomeIdentical xs_
  ]
  where
    select [] = []
    select ((x, n) : xs) =
      (x, xs_) :
        [ (y, (x, n) : cs)
          | (y, cs) <- select xs
        ]
      where
        xs_
          | 1 == n = xs
          | otherwise = (x, pred n) : xs

-- >>> permutationsSomeIdentical $ [(a,2) | a <- "AB"]
-- ["AABB","ABAB","ABBA","BAAB","BABA","BBAA"]

-- Eliminate symmetry between A and B by aribtrarily (without loss of generality) choosing A as first letter
-- >>> ('A':) <$> permutationsSomeIdentical [('A',1),('B',2)]
-- ["AABB","ABAB","ABBA"]

-- >>> ('A':) <$> permutationsSomeIdentical [('A',1),('B',2),('C',2)]
-- ["AABBCC","AABCBC","AABCCB","AACBBC","AACBCB","AACCBB"
-- ,"ABABCC","ABACBC","ABACCB","ABBACC","ABBCAC","ABBCCA"
-- ,"ABCABC","ABCACB","ABCBAC","ABCBCA","ABCCAB","ABCCBA"
-- ,"ACABBC","ACABCB","ACACBB","ACBABC","ACBACB","ACBBAC"
-- ,"ACBBCA","ACBCAB","ACBCBA","ACCABB","ACCBAB","ACCBBA"]

-- >>> length $ permutationsSomeIdentical [('A',1),('B',2),('C',2)]
-- >>> length $ permutationsSomeIdentical [('A',2),('B',2),('C',2)]
-- 30
-- 90

-- Goal: 15 permutations:
-- ["AABBCC","AABCBC","AABCCB"
-- ,"ABABCC","ABACBC","ABACCB","ABBACC","ABBCAC","ABBCCA"
-- ,"ABCABC","ABCACB","ABCBAC","ABCBCA","ABCCAB","ABCCBA"]

-- ["AABBCC" ,"ABABCC","ABBACC","ABBCAC","ABBCCA"
-- ,"AABCBC" ,"ABACBC","ABCABC","ABCBAC","ABCBCA"
-- ,"AABCCB" ,"ABACCB","ABCACB","ABCCAB","ABCCBA"]

-- map ('A':)
-- ["ABBCC" ,"BABCC","BBACC","BBCAC","BBCCA"
-- ,"ABCBC" ,"BACBC","BCABC","BCBAC","BCBCA"
-- ,"ABCCB" ,"BACCB","BCACB","BCCAB","BCCBA"]

-- >>> length $ permutationsSomeIdentical [('A',2),('B',3)]
-- >>> length $ permutationsSomeIdentical [('A',3),('B',3)]
-- >>> ('A':) <$> permutationsSomeIdentical [('A',2),('B',3)]
-- 10
-- 20
-- ["AAABBB","AABABB","AABBAB","AABBBA","ABAABB","ABABAB","ABABBA","ABBAAB","ABBABA","ABBBAA"]

-- | Same as `permutationSomeIdentical` but don't produce strings that would
--   be identical under letter renaming. E.g. "ABBA" ~ "BAAB"
-- >>> permutationsSomeIdenticalWithRenaming [('A',2),('B',2)]
-- ["AABB","ABAB","ABBA"]
permutationsSomeIdenticalWithRenaming' :: Int -> [a] -> [[a]]
permutationsSomeIdenticalWithRenaming' n = permutationsSomeIdenticalWithRenaming . fmap (,n)
permutationsSomeIdenticalWithRenaming :: [(a, Int)] -> [[a]]
permutationsSomeIdenticalWithRenaming = go . fmap (False,)
  where
    go [] = [[]]
    go xs =
        [ x : ys
            | (x, xs_) <- select xs,
            ys <- go xs_
        ]
    select [] = []
    select ((used,(x, n)) : xs) =
      (x, xs_) :
        [ (y, (used,(x, n)) : cs)
          | used -- If this is the first time we encounter x, always select it
          , (y, cs) <- select xs
        ]
      where
        xs_
          | 1 == n = xs
          | otherwise = (True,(x, pred n)) : xs

-- >>> length $ permutationsSomeIdenticalWithRenaming [('A',3),('B',3)]
-- >>> permutationsSomeIdenticalWithRenaming [('A',3),('B',3)]
-- >>> length $ permutationsSomeIdenticalWithRenaming [('A',2),('B',2),('C',2)]
-- >>> permutationsSomeIdenticalWithRenaming [('A',2),('B',2),('C',2)]
-- 10
-- ["AAABBB","AABABB","AABBAB","AABBBA","ABAABB","ABABAB","ABABBA","ABBAAB","ABBABA","ABBBAA"]
-- 15
-- ["AABBCC","AABCBC","AABCCB","ABABCC","ABACBC","ABACCB","ABBACC","ABBCAC","ABBCCA","ABCABC","ABCACB","ABCBAC","ABCBCA","ABCCAB","ABCCBA"]

fact :: Integer -> Integer
fact n = product [1..n]

-- >>> length $ permutationsSomeIdenticalWithRenaming [('A',6),('B',6)]
-- 462


-- >>> numPermutationsSIWR 2 6
-- 462
numPermutationsSIWR :: Integer -> Integer -> Integer
numPermutationsSIWR dice sides = fact (sides*dice) `div` fact sides^dice `div` fact dice

-- >>> genPermutationsSIWR 2 3
-- >>> genPermutationsSIWR 3 2
-- ["AAABBB","AABABB","AABBAB","AABBBA","ABAABB","ABABAB","ABABBA","ABBAAB","ABBABA","ABBBAA"]
-- ["AABBCC","AABCBC","AABCCB","ABABCC","ABACBC","ABACCB","ABBACC","ABBCAC","ABBCCA","ABCABC","ABCACB","ABCBAC","ABCBCA","ABCCAB","ABCCBA"]
genPermutationsSIWR :: Int -> Int -> [String]
genPermutationsSIWR dice sides = permutationsSomeIdenticalWithRenaming' sides $ take dice ['A'..]

-- >>> numPermutationsSIWR 3 5
-- >>> length $ genPermutationsSIWR 3 5
-- 126126
-- 126126

-- >>> fmap splitPermutation $ genPermutationsSIWR 2 2
-- [[('A',[0,1]),('B',[2,3])]
-- ,[('A',[0,2]),('B',[1,3])]
-- ,[('A',[0,3]),('B',[1,2])]]
splitPermutation :: String -> [(Char,[Int])]
splitPermutation str = fmap (\x -> (fst $ head x, fmap snd x) ) $ groupBy ((==) `on` fst) $ sortOn fst $ zip str [0..]

rollAllDice :: [(Char, [Int])] -> [[(Char, Int)]]
rollAllDice = mapM $ \(name, values) -> (name,) <$> values
outcomesRaw :: [(Char, [Int])] -> [[(Char, Int)]]
outcomesRaw dice = sortOn snd <$> rollAllDice dice
outcomes :: [(Char, [Int])] -> [String]
outcomes dice = fmap fst <$> outcomesRaw dice
outcomesFromString :: String -> [String]
outcomesFromString = outcomes . splitPermutation

-- >>> outcomesFromString $ (!!2) $ genPermutationsSIWR 2 2
-- >>> outcomesRaw $ (!!2) $ fmap splitPermutation $ genPermutationsSIWR 2 2
-- ["AB","AB","BA","BA"]
-- [[('A',0),('B',1)],[('A',0),('B',2)],[('B',1),('A',3)],[('B',2),('A',3)]]

-- >>> frequencies $ outcomes $ (!!2) $ fmap splitPermutation $ genPermutationsSIWR 2 2
-- [("AB",2),("BA",2)]
frequencies :: Ord a => [a] -> [(a,Int)]
frequencies = fmap (\x -> (head x, length x)) . group . sort

-- >>> diceStringToFairnessFreqs <$> genPermutationsSIWR 2 2
-- [[("AB",4)],[("AB",3),("BA",1)],[("AB",2),("BA",2)]]
diceStringToFairnessFreqs :: String -> [(String, Int)]
diceStringToFairnessFreqs = frequencies . outcomesFromString

-- >>> diceStringToFairnessFreqs <$> genPermutationsSIWR 3 2
-- >>> take 5 $ diceStringToFairnessFreqs <$> genPermutationsSIWR 2 4
-- [[("ABC",8)],[("ABC",6),("ACB",2)],[("ABC",4),("ACB",4)],[("ABC",6),("BAC",2)],[("ABC",4),("ACB",2),("BAC",2)],[("ABC",2),("ACB",4),("BAC",2)],[("ABC",4),("BAC",4)],[("ABC",4),("BAC",2),("BCA",2)],[("ABC",4),("BCA",4)],[("ABC",4),("ACB",1),("BAC",1),("BCA",1),("CAB",1)],[("ABC",2),("ACB",3),("BAC",1),("BCA",1),("CAB",1)],[("ABC",3),("ACB",1),("BAC",2),("BCA",1),("CBA",1)],[("ABC",3),("ACB",1),("BCA",3),("CBA",1)],[("ABC",2),("ACB",2),("BCA",2),("CAB",2)],[("ABC",2),("ACB",2),("BCA",2),("CBA",2)]]
-- [[("AB",16)],[("AB",15),("BA",1)],[("AB",14),("BA",2)],[("AB",13),("BA",3)],[("AB",12),("BA",4)]]

-- >>> filter (isFair 2) (genPermutationsSIWR 2 2)
-- >>> filter (isFair 8) (genPermutationsSIWR 2 4)
-- ["ABBA"]
-- ["AABBBBAA","ABABBABA","ABBAABBA","ABBABAAB"]
isFair :: Int -> String -> Bool
isFair expected dice = all ((==expected) . snd) $ diceStringToFairnessFreqs dice

-- >>> take 5 $ diceStringToFairnessFreqs <$> genPermutationsSIWR 3 6
-- [[("ABC",216)],[("ABC",210),("ACB",6)],[("ABC",204),("ACB",12)],[("ABC",198),("ACB",18)],[("ABC",192),("ACB",24)]]

expectedFair :: Int -> Int -> Int
expectedFair dice sides = fromInteger $ sides' ^ dice' `div` fact dice'
  where
    dice' = fromIntegral dice
    sides' = fromIntegral sides

-- >>> findFairDice 2 4
-- >>> diceStringToFairnessFreqs <$> findFairDice 2 4
-- ["AABBBBAA","ABABBABA","ABBAABBA","ABBABAAB"]
-- [[("AB",8),("BA",8)],[("AB",8),("BA",8)],[("AB",8),("BA",8)],[("AB",8),("BA",8)]]
findFairDice :: Int -> Int -> [String]
findFairDice dice sides = filter (isFair (expectedFair dice sides)) $ genPermutationsSIWR dice sides

-- >>> findFairDice 2 6
-- ["AAABBBBBBAAA","AABABBBBABAA","AABBABBABBAA","AABBABBBAABA","AABBBAABBBAA","AABBBABABABA","AABBBABBAAAB","AABBBBAAABBA","AABBBBAABAAB","ABAABBBABBAA","ABAABBBBAABA","ABABABABBBAA","ABABABBABABA","ABABABBBAAAB","ABABBAABBABA","ABABBABAABBA","ABABBABABAAB","ABABBBAAABAB","ABBAAABBBBAA","ABBAABABBABA","ABBAABBAABBA","ABBAABBABAAB","ABBABAABABBA","ABBABAABBAAB","ABBABABAABAB","ABBABBAAAABB","ABBBAAAABBBA","ABBBAAABABAB","ABBBAABAAABB"]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- >>> filter isPalindrome $ findFairDice 2 6
-- >>> filter (not.isPalindrome) $ findFairDice 2 6
-- ["AAABBBBBBAAA","AABABBBBABAA","AABBABBABBAA","AABBBAABBBAA","ABAABBBBAABA","ABABABBABABA","ABABBAABBABA","ABBAABBAABBA","ABBABAABABBA","ABBBAAAABBBA"]
-- ["AABBABBBAABA","AABBBABABABA","AABBBABBAAAB","AABBBBAAABBA","AABBBBAABAAB","ABAABBBABBAA","ABABABABBBAA","ABABABBBAAAB","ABABBABAABBA","ABABBABABAAB","ABABBBAAABAB","ABBAAABBBBAA","ABBAABABBABA","ABBAABBABAAB","ABBABAABBAAB","ABBABABAABAB","ABBABBAAAABB","ABBBAAABABAB","ABBBAABAAABB"]

antiSymmetric :: String -> Bool
antiSymmetric xs = xs == reverse (map flipAB xs)
  where
    flipAB 'A' = 'B'
    flipAB 'B' = 'A'
    flipAB _ = error "invalid"

-- >>> filter antiSymmetric $ findFairDice 2 6
-- >>> filter antiSymmetric $ findFairDice 2 4
-- ["ABABBBAAABAB"]
-- ["ABBABAAB"]

-- Faster solution: All pairs of dice need to be fair pairs too,
-- so add a die to that set such that the result is fair and
-- such that each new pair is part of the set of fair pairs

{-
-- palindromes
["AAA BBB BBB AAA"
,"AAB ABB BBA BAA"
,"AAB BAB BAB BAA"
,"AAB BBA ABB BAA"

,"ABA ABB BBA ABA"
,"ABA BAB BAB ABA"
,"ABA BBA ABB ABA"
,"ABB AAB BAA BBA"
,"ABB ABA ABA BBA"
,"ABB BAA AAB BBA"]

-- asymmetric
["AABB AB BB AA BA"
,"AABB BA BA BA BA"
,"AABB BA BB AA AB"
,"AABB BB AA AB BA"
,"AABB BB AA BA AB"

,"AB AA BB BA BB AA"

,"ABAB AB AB BB AA"
,"ABAB AB BB AA AB"
,"ABAB BA BA AB BA"
,"ABAB BA BA BA AB"

,"AB AB BB AA AB AB" -- antisymmetric

,"ABBA AA BB BB AA"
,"ABBA AB AB BA BA"
,"ABBA AB BA BA AB"
,"ABBA BA AB BA AB"
,"ABBA BA BA AB AB"
,"ABBA BB AA AA BB"

,"ABBBAA AB AB AB"
,"ABBBAA BA AA BB"]
-}

{-

Idea: First generate smaller set of fair die

Idea: Generate fair dice directly by storing the numbers so far during computation
- This is essentially dynamic programming since we can reuse previous calculation to turn n^3 to n^2
- Add one dice at a time to a set of already fair dice

-}
