{-# LANGUAGE BangPatterns #-}
import Data.Function (on)
import Data.List (maximumBy, sort, find)
import Data.Ord (comparing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict (State, gets)
import qualified Control.Monad.State.Strict as State
import Debug.Trace (traceM)




-- You're climbing a staircase, it takes n steps to reach the top
-- Each time either climb 1 or 2 steps. How many ways can you climb?

-- >>> naiveFib 3
-- 3
naiveFib :: Integer -> Integer
naiveFib 0 = 1
naiveFib 1 = 1
naiveFib n = naiveFib (n - 1) + naiveFib (n - 2)

-- >>> take 4 fibs
-- [1,1,2]
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- >>> prop_fibs 20
-- True
prop_fibs :: Int -> Bool
prop_fibs n = ((==) `on` take n) fibs $ map naiveFib [0..]

-- Slow O(n) list memoization
listMemoBad :: (Int -> a) -> Int -> a
listMemoBad f = f'
  where
    xs = f <$> [0..]
    f' n = xs !! n

listMemo :: ((Int -> a) -> Int -> a) -> Int -> a
listMemo f = f'
  where
    xs = f f' <$> [0..]
    f' n = xs !! n

-- >>> naiveFib 30
-- 1346269

-- >>> simpleMemoFib 50
-- 20365011074
simpleMemoFib :: Int -> Integer
simpleMemoFib = listMemo inner
  where
    inner self 0 = 1
    inner self 1 = 1
    inner self n = self (n - 1) + self (n - 2)


-- 5. Longest Palindromic Substring

-- Given a string s, return the longest palindromic substring in s.

{-
Input: s = "babad"
Output: "bab"
Explanation: "aba" is also a valid answer.
-}

type ListZipper a = ([a], [a])

-- >>> allZippers "abcd"
-- [("","abcd"),("a","bcd"),("ba","cd"),("cba","d"),("dcba","")]
allZippers :: [a] -> [ListZipper a]
allZippers = go []
  where
    go old [] = [(old,[])]
    go old (x : xs) = (old,x:xs) : go (x:old) xs

unZipper :: ListZipper a -> [a]
unZipper (left, right) = reverse left ++ right

longestPalindromeAtPointLength :: (Eq a) => ListZipper a -> Int
longestPalindromeAtPointLength (left, []) = 0
longestPalindromeAtPointLength (left, mid : right) =
    go 1 left right `max` go 0 left (mid : right)
  where
    go :: Eq a => Int -> [a] -> [a] -> Int
    go !len (l:ls) (r:rs) | l == r = go (len + 2) ls rs
    go !len _ _ = len

longestPalindromeAtPoint :: (Eq a) => ListZipper a -> (Int,[a])
longestPalindromeAtPoint zp =
    (best, unZipper $ truncateTo best zp)
  where
    best = longestPalindromeAtPointLength zp
    truncateTo :: Int -> ListZipper a -> ListZipper a
    truncateTo n (l,r) = (take (n`div`2) l, take ((n`div`2) + (n`mod`2)) r)
-- longestPalindromeAtPoint (left, []) = (0,[])
-- longestPalindromeAtPoint zp@(left, mid : right) =
--     (best, unZipper $ truncateTo best zp)
--   where
--     best = go 1 left right `max` go 0 left (mid : right)
--     truncateTo :: Int -> ListZipper a -> ListZipper a
--     truncateTo n (l,r) = (take (n`div`2) l, take ((n`div`2) + (n`mod`2)) r)
--     go :: Eq a => Int -> [a] -> [a] -> Int
--     go !len (l:ls) (r:rs) | l == r = go (len + 2) ls rs
--     go !len _ _ = len


-- >>> longestPalindromeLength "babad"
-- 3
longestPalindromeLength :: Eq a => [a] -> Int
longestPalindromeLength = maximum . fmap longestPalindromeAtPointLength . allZippers

-- >>> longestPalindrome "babad"
-- (3,"aba")
longestPalindrome :: Eq a => [a] -> (Int, [a])
longestPalindrome = maximumBy (comparing fst) . fmap longestPalindromeAtPoint . allZippers

-- >>> fmap longestPalindromeAtPoint . allZippers $ "babad"
-- >>> fmap longestPalindromeAtPoint . allZippers $ "baabad"
-- [(1,"b"),(3,"bab"),(3,"aba"),(1,"a"),(1,"d"),(0,"")]
-- [(1,"b"),(1,"a"),(4,"baab"),(3,"aba"),(1,"a"),(1,"d"),(0,"")]


type NimBoard = [Int]

data Winner = Current | Other
  deriving (Eq, Show)

type NimMap = Map NimBoard Winner

initialMapMisere :: NimMap
initialMapMisere = Map.singleton [] Current
initialMapNormal :: NimMap
initialMapNormal = Map.singleton [] Other

validMoves' :: NimBoard -> [NimBoard]
validMoves' [] = []
validMoves' [x] = [] : fmap (:[]) [1..pred x]
validMoves' (x:xs) = xs : fmap (:xs) [1..pred x] ++ fmap (x:) (validMoves' xs)

-- >>> validMoves [1,2]
-- >>> validMoves' [1,2]
-- [[1],[1,1],[1,2],[2]]
-- [[2],[1,2],[1],[1,1],[1,2]]
validMoves'' :: NimBoard -> Set.Set NimBoard
validMoves'' = Set.fromList . fmap sort . validMoves'
validMoves :: NimBoard -> [NimBoard]
validMoves = Set.toList . validMoves''

-- | Breaks as soon as any element of the list satisfies the predicate
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p [] = pure False
anyM p (x:xs) = do
    satisfied <- p x
    if satisfied
        then pure True
        else anyM p xs

isWinningFix :: Monad m => (NimBoard -> m Winner) -> NimBoard -> m Winner
isWinningFix getWinner curr = do
    let toTry = validMoves curr
    -- traceM $ "From " ++ show curr ++ " will try " ++ show toTry
    anySolution <- anyM (fmap (== Other) . getWinner) $ validMoves curr
    -- traceM $ "Got all results " ++ show curr ++ ": " ++ show results
    pure $ if anySolution
        then Current
        else Other

isWinningFixOld :: Monad m => (NimBoard -> m Winner) -> NimBoard -> m Winner
isWinningFixOld getWinner curr = do
    -- TODO: Optimize by removing mapM and making e.g. allM
    let toTry = validMoves curr
    -- traceM $ "From " ++ show curr ++ " will try " ++ show toTry
    results <- mapM getWinner $ validMoves curr
    -- traceM $ "Got all results " ++ show curr ++ ": " ++ show results
    pure $ if Other `elem` results
        then Current
        else Other

-- isWinning' :: NimMap -> NimBoard -> Winner
-- isWinning' f =  _
    -- Map.restrictKeys

isWinningSt :: NimBoard -> State NimMap Winner
isWinningSt b = do
    -- traceM $ "Checking " ++ show b
    cached <- State.gets $ Map.lookup b
    case cached of
        Nothing -> do
            -- Detect infinite loops. Only works with lazy maps
            -- State.modify $ Map.insert b $ error $ "Infinite loop on " ++ show b
            -- traceM $ "Not found. Recursing " ++ show b
            res <- isWinningFix isWinningSt b
            -- traceM $ "Done recursing " ++ show b ++ " got " ++ show res
            State.modify $ Map.insert b res
            pure res
        Just res -> do
            -- traceM $ "Found " ++ show b ++ ": " ++ show res
            pure res

stateMemo :: Ord a => ((a -> State (Map a b) b) -> a -> State (Map a b) b) -> a -> State (Map a b) b
stateMemo fun b = do
    cached <- State.gets $ Map.lookup b
    case cached of
        Nothing -> do
            res <- fun (stateMemo fun) b
            State.modify $ Map.insert b res
            pure res
        Just res -> do pure res

isWinningSt'' :: NimBoard -> State NimMap Winner
isWinningSt'' = stateMemo isWinningFix

-- >>> runNimMisere [1,1,2]
-- (Current,fromList [([],Current),([1],Other),([1,1],Current),([1,1,1],Other),([1,1,2],Current),([1,2],Current),([2],Current)])
runNimMisere :: NimBoard -> (Winner, NimMap)
runNimMisere b = State.runState (isWinningSt $ sort b) initialMapMisere

nimWinnerMisere :: NimBoard -> Winner
nimWinnerMisere b = State.evalState (isWinningSt $ sort b) initialMapMisere

nimExample :: NimMap
nimExample = snd $ runNimMisere [2,2,2,3]

-- >>> allWinningMoves nimExample [2,2,2,1]
-- >>> length . allWinningMoves nimExample <$> Map.keys nimExample
-- >>> Map.keys nimExample !! 9
-- >>> allWinningMoves nimExample [1,1,2,3]
-- [[1,1,2,2]]
-- [0,0,1,0,1,1,1,1,0,2,1,1,1,1,2,0,1,1,0,1,0,1,2,1,1]
-- [1,1,2,3]
-- [[1,1,2,2],[1,2,3]]
allWinningMoves :: NimMap -> NimBoard -> [] NimBoard
allWinningMoves mp b = fst <$> filter ((==Other).snd) moves
  where
    moves = Map.toList $ Map.restrictKeys mp $ validMoves'' b

-- >>> optimalMove nimExample [2,2,2,1]
-- Just [1,1,2,2]
optimalMove :: NimMap -> NimBoard -> Maybe NimBoard
optimalMove mp b = fst <$> find ((==Other).snd) moves
  where
    moves = Map.toList $ Map.restrictKeys mp $ validMoves'' b

outcomes :: NimMap -> NimBoard -> [(NimBoard, Winner)]
outcomes mp b = Map.toList $ Map.restrictKeys mp $ validMoves'' b

-- >>> outcomes nimExample2 [2,3,4,5]
-- [([1,2,3,4],Current),([1,2,3,5],Current),([1,2,4,5],Current),([1,3,4,5],Current),([2,2,3,4],Current),([2,2,3,5],Current),([2,2,4,5],Current),([2,3,3,4],Current),([2,3,3,5],Current),([2,3,4],Current),([2,3,4,4],Current),([2,3,5],Current),([2,4,5],Current),([3,4,5],Current)]

runNimNormalPlay :: NimBoard -> (Winner, NimMap)
runNimNormalPlay b = State.runState (isWinningSt $ sort b) initialMapNormal

nimExample2 :: NimMap
nimExample2 = snd $ runNimNormalPlay [1,2,3,4,5]

-- >>> allWinningMoves nimExample2 [1,2,3,4,5]
-- [[1,2,2,4,5],[1,2,3,4,4],[2,3,4,5]]
-- >>> allWinningMoves nimExample2 [2,2,4,5]
-- [[2,2,4,4]]
-- >>> allWinningMoves nimExample2 [1,2,4,4]
-- [[1,1,4,4]]
-- >>> allWinningMoves nimExample2 [1,4,4]
-- [[4,4]]
-- >>> allWinningMoves nimExample2 [3,4]
-- [[3,3]]


-- >>> allWinningMoves nimExample2 [1,3,4,5]
-- [[1,4,5]]
