{-# OPTIONS_GHC -Wall #-}
-- module HundeSpiel where
import Data.List
import Control.Monad (guard)
import Control.Monad.State ( StateT(StateT) )
import Control.Applicative (Alternative(empty))
import System.Environment (getArgs)
import Data.Tree

-- https://blog.ploeh.dk/2024/10/03/das-verflixte-hunde-spiel/

data Half = Head | Tail deriving (Show, Eq, Ord)

data Pattern = Brown | Grey | Spotted | Umber deriving (Show, Eq, Ord)

data Tile = Tile {
  top :: (Pattern, Half),
  right :: (Pattern, Half),
  bottom :: (Pattern, Half),
  left :: (Pattern, Half) }
  deriving (Show, Eq, Ord)

matches :: (Pattern, Half) -> (Pattern, Half) -> Bool
matches (p1, h1) (p2, h2) = p1 == p2 && h1 /= h2

rotateClockwise :: Tile -> Tile
rotateClockwise (Tile t r b l) = Tile l t r b

rotateCounterClockwise :: Tile -> Tile
rotateCounterClockwise (Tile t r b l) = Tile r b l t

upend :: Tile -> Tile
upend (Tile t r b l) = Tile b l t r

rotations :: Tile -> [Tile]
rotations t = [t, rotateClockwise t, upend t, rotateCounterClockwise t]

-- Eliminate redundancy:
-- - list with number remaining to only choose same tile one way
-- - ignore rotation for the initial center tile

-- Improve performance:
-- - Work in an order that reduces options maximally
--   Specifically, complete each square before starting a new one
-- - Don't list all permutation before eliminating options

type TileSet = [(Tile, Int)]

-- We list the number of duplicates so we can avoid listing the same tile as distinct options
initialTiles :: TileSet
initialTiles = count
  [
    Tile (Brown, Head) (Grey,    Head) (Umber,   Tail) (Spotted, Tail),
    Tile (Brown, Head) (Spotted, Head) (Brown,   Tail) (Umber,   Tail),
    Tile (Brown, Head) (Spotted, Head) (Grey,    Tail) (Umber,   Tail),
    Tile (Brown, Head) (Spotted, Head) (Grey,    Tail) (Umber,   Tail),
    Tile (Brown, Head) (Umber,   Head) (Spotted, Tail) (Grey,    Tail),
    Tile (Grey,  Head) (Brown,   Head) (Spotted, Tail) (Umber,   Tail),
    Tile (Grey,  Head) (Spotted, Head) (Brown,   Tail) (Umber,   Tail),
    Tile (Grey,  Head) (Umber,   Head) (Brown,   Tail) (Spotted, Tail),
    Tile (Grey,  Head) (Umber,   Head) (Grey,    Tail) (Spotted, Tail)
  ]

sorted :: [Tile]
sorted = sort $ minimum . rotations . fst <$> initialTiles

-- >>> count "aabcc"
-- [('a',2),('b',1),('c',2)]
count :: Eq a => [a] -> [(a,Int)]
count xs = [(head x, length x) | x <- group xs]

expand :: [(a, Int)] -> [a]
expand = concatMap $ uncurry (flip replicate)

-- >>> duplicateTiles
-- [Tile {top = (Brown,Head), right = (Spotted,Head), bottom = (Grey,Tail), left = (Umber,Tail)}]
duplicateTiles :: [Tile]
duplicateTiles = [x | [x,_] <- group sorted]

-- All possible ways to take a single tile from a tile set
-- We return the selected tile and the remaining set of tiles
-- >>> chooseTile (count "aabcc")
-- [('a',[('a',1),('b',1),('c',2)])
-- ,('b',[('a',2),('c',2)])
-- ,('c',[('a',2),('b',1),('c',1)])]
chooseTile :: [(a,Int)] -> [(a, [(a, Int)])]
chooseTile [] = []
chooseTile ((t, n) : ts) = (t, ts' n) : [(chosen ,(t,n):unused) | (chosen, unused) <- chooseTile ts]
  where
    ts' 1 = ts
    ts' _ = (t, n - 1) : ts

-- We could also think of this as a state monad:
chooseTileM :: StateT [(a, Int)] [] a
chooseTileM = StateT chooseTile

data Triple a = T a a a
  deriving (Show, Eq)

-- A board is three rows, each with three cells, that may or may not contain a tile
-- Nothing if we haven't chosen a tile yet
type Board = Triple (Triple (Maybe Tile))

-- We deliberately don't rotate the middle tile in order to eliminate
-- whole board rotational symmetry.
-- The middle tile is the only tile we could do this with
selectMiddle :: [(Board, TileSet)]
selectMiddle = do
    (t, rest) <- chooseTile initialTiles
    pure (T (T Nothing Nothing Nothing)
            (T Nothing (Just t) Nothing)
            (T Nothing Nothing Nothing)
         , rest)

-- >>> fst $ head selectMiddle
-- T (T Nothing Nothing Nothing) (T Nothing (Just (Tile {top = (Brown,Head), right = (Grey,Head), bottom = (Umber,Tail), left = (Spotted,Tail)})) Nothing) (T Nothing Nothing Nothing)

chooseLeft :: (Board, TileSet) -> [(Board, TileSet)]
chooseLeft (board, tiles) = do
    (newTile, newTileSet) <- chooseTile tiles
    rotatedTile <- rotations newTile
    let T r1 (T _ middleTile rght) bot = board
    guard $ matchesMb (right rotatedTile) (left <$> middleTile)
    pure (T r1 (T (Just rotatedTile) middleTile rght) bot, newTileSet)


matchesMb :: (Pattern, Half) -> Maybe (Pattern, Half) -> Bool
matchesMb _ Nothing = True
matchesMb a (Just b) = matches a b

topLeft :: Tile -> Board -> [Board]
topLeft tl (T (T _tl tm tr) (T ml mm mr) bot)
  | matchesMb (right  tl) (left <$> tm)
  , matchesMb (bottom tl) (top  <$> ml)
  = pure (T (T (Just tl) tm tr) (T ml mm mr) bot)
  | otherwise = empty

topMid :: Tile -> Board -> [Board]
topMid tm (T (T tl _tm tr) (T ml mm mr) bot)
  | matchesMb (right  tm) (left  <$> tr)
  , matchesMb (left   tm) (right <$> tl)
  , matchesMb (bottom tm) (top   <$> mm)
  = pure (T (T tl (Just tm) tr) (T ml mm mr) bot)
  | otherwise = empty

topRight :: Tile -> Board -> [Board]
topRight tr (T (T tl tm _tr) (T ml mm mr) bot)
  | matchesMb (left  tr) (right <$> tm)
  , matchesMb (bottom tr) (top  <$> mr)
  = pure (T (T tl tm (Just tr)) (T ml mm mr) bot)
  | otherwise = empty

midLeft :: Tile -> Board -> [Board]
midLeft ml (T (T tl tm tr) (T _ml mm mr) (T bl bm br))
  | matchesMb (right  ml) (left   <$> mm)
  , matchesMb (top    ml) (bottom <$> tl)
  , matchesMb (bottom ml) (top    <$> bl)
  = pure (T (T tl tm tr) (T (Just ml) mm mr) (T bl bm br))
  | otherwise = empty

midRight :: Tile -> Board -> [Board]
midRight mr' (T (T tl tm tr) (T ml mm _mr) (T bl bm br))
  | matchesMb (left   mr') (right  <$> mm)
  , matchesMb (top    mr') (bottom <$> tr)
  , matchesMb (bottom mr') (top    <$> br)
  = pure (T (T tl tm tr) (T ml mm mr) (T bl bm br))
  | otherwise = empty
  where mr = Just mr'

botLeft :: Tile -> Board -> [Board]
botLeft bl' (T (T tl tm tr) (T ml mm mr) (T _bl bm br))
  | matchesMb (right  bl') (left   <$> bm)
  , matchesMb (top    bl') (bottom <$> ml)
  = pure (T (T tl tm tr) (T ml mm mr) (T bl bm br))
  | otherwise = empty
  where bl = Just bl'

botMid :: Tile -> Board -> [Board]
botMid bm' (T (T tl tm tr) (T ml mm mr) (T bl _bm br))
  | matchesMb (left   bm') (right  <$> bl)
  , matchesMb (top    bm') (bottom <$> mm)
  , matchesMb (right bm')  (left   <$> br)
  = pure (T (T tl tm tr) (T ml mm mr) (T bl bm br))
  | otherwise = empty
  where bm = Just bm'

botRight :: Tile -> Board -> [Board]
botRight br' (T (T tl tm tr) (T ml mm mr) (T bl bm _br))
  | matchesMb (left   br') (right  <$> bm)
  , matchesMb (top    br') (bottom <$> mr)
  = pure (T (T tl tm tr) (T ml mm mr) (T bl bm br))
  | otherwise = empty
  where br = Just br'

-- In order to insert a new tile, we first choose a tile, then we rotate it and finally we check if it can be inserted
insertTile :: (Tile -> Board -> [Board]) -> (Board, TileSet) -> [(Board, TileSet)]
insertTile inserter (board, tiles) = do
    (newTile, newTileSet) <- chooseTile tiles
    rotatedTile <- rotations newTile
    newBoard <- inserter rotatedTile board
    pure (newBoard, newTileSet)


-- Trace the number of valid options after each step. The order is very important!
-- >>> length <$> scanl (\st pos -> st >>= insertTile pos) selectMiddle [midLeft, topMid, topLeft]
-- >>> length <$> scanl (\st pos -> st >>= insertTile pos) selectMiddle [midLeft, topMid, topRight]
-- >>> length <$> scanl (\st pos -> st >>= insertTile pos) selectMiddle [midLeft, topLeft, topRight]
-- >>> length <$> scanl (\st pos -> st >>= insertTile pos) selectMiddle [botLeft, topLeft, topRight]
-- >>> length <$> scanl (\st pos -> st >>= insertTile pos) selectMiddle [midLeft, topMid, topLeft, midRight, topRight, botMid, botLeft, botRight]
-- >>> sum $ length <$> scanl (\st pos -> st >>= insertTile pos) selectMiddle [midLeft, topMid, topLeft, midRight, topRight, botMid, botLeft, botRight]
-- >>> length <$> scanl (\st pos -> st >>= insertTile pos) selectMiddle bestOrder
-- >>> sum $ length <$> scanl (\st pos -> st >>= insertTile pos) selectMiddle bestOrder
-- [8,26,62,39]
-- [8,26,62,223]
-- [8,26,64,1388]
-- [8,228,5712,123648]
-- [8,26,62,39,117,43,85,17,2]
-- 399
-- [8,22,90,50,148,60,79,13,2]
-- 472

-- One of the many possible optimal orders. The crucial thing is that we maximize the number of constraints as early as possible
bestOrder :: [Tile -> Board -> [Board]]
-- bestOrder = [midLeft, topMid, topLeft, midRight, topRight, botMid, botLeft, botRight]
bestOrder = [topMid, topRight, midRight, botRight, botMid, botLeft, midLeft, topLeft]

-- rotateBoard :: Board -> Board

solutionsTrace :: [[Board]]
solutionsTrace = map fst <$> scanl (\st pos -> st >>= insertTile pos) selectMiddle bestOrder

solutions :: [Board]
solutions = map fst $ foldl' (\st pos -> st >>= insertTile pos) selectMiddle bestOrder
-- >>> prettyBoard $ solutions !! 0
-- ["             Brown head                            Grey head                             Brown head             "
-- ," Umber tail             Spotted head  Spotted tail             Umber head    Umber tail             Spotted head"
-- ,"             Brown tail                            Brown tail                            Grey tail              "
-- ,"             Brown head                            Brown head                            Grey head              "
-- ,"Spotted tail             Grey head     Grey tail               Umber head    Umber tail              Brown head "
-- ,"             Umber tail                           Spotted tail                          Spotted tail            "
-- ,"             Umber head                           Spotted head                          Spotted head            "
-- ," Grey head               Grey tail     Grey head               Brown tail    Brown head              Grey tail  "
-- ,"            Spotted tail                           Umber tail                            Umber tail             "]

-- >>> prettyBoard $ solutions !! 1
-- ["             Brown head                            Grey head                             Brown head             "
-- ," Umber tail             Spotted head  Spotted tail             Umber head    Umber tail             Spotted head"
-- ,"             Grey tail                             Grey tail                             Brown tail             "
-- ,"             Grey head                             Grey head                             Brown head             "
-- ," Umber tail             Spotted head  Spotted tail             Umber head    Umber tail             Spotted head"
-- ,"             Brown tail                            Brown tail                            Grey tail              "
-- ,"             Brown head                            Brown head                            Grey head              "
-- ,"Spotted tail             Grey head     Grey tail               Umber head    Umber tail              Brown head "
-- ,"             Umber tail                           Spotted tail                          Spotted tail            "]

-- * Pretty printing
-- >>> prettyTile $ fst $ head initialTiles
-- ["              Brown head            "
-- ,"Spotted tail               Grey head"
-- ,"              Umber tail            "]
prettyTile :: Tile -> [String]
prettyTile Tile {top, right, bottom, left} =
    [ filler ++ prettySide top ++ filler
    , prettySide left ++ filler ++ prettySide right
    , filler ++ prettySide bottom ++ filler
    ] where filler = replicate 12 ' '

prettySide :: (Pattern, Half) -> String
prettySide (p, h) = center 12 $ show p ++ " " ++ prettyHalf h
  where
    -- prettyPat Brown   = "  Brown"
    -- prettyPat Grey    = "   Grey"
    -- prettyPat Spotted = "Spotted"
    -- prettyPat Umber   = "  Umber"

    prettyHalf Head = "head"
    prettyHalf Tail = "tail"
    center n str = replicate l ' ' ++ str ++ replicate r ' '
      where
        need = n - length str
        l = need `div` 2
        r = need - l

example :: Triple (Maybe Tile)
example = head $ do
  x <- selectMiddle
  y <- chooseLeft x
  (T _ b _, _st) <- insertTile midRight y
  pure b

-- >>> prettyRow example
-- ["             Brown head                            Brown head                            Umber tail             "
-- ," Umber tail             Spotted head  Spotted tail             Grey head     Grey tail               Brown head "
-- ,"             Brown tail                            Umber tail                           Spotted head            "]
prettyRow :: Triple (Maybe Tile) -> [String]
prettyRow (T a b c) = hcat $ map mbTile [a,b,c]
  where
   mbTile Nothing = replicate 3 (replicate 36 ' ')
   mbTile (Just t) = prettyTile t

-- Horizontally concatenate lists of lines
hcat :: [[String]] -> [String]
hcat = map (intercalate "  ") . transpose

-- >>> prettyBoard $ fst $ head $ selectMiddle >>= insertTile midLeft >>= insertTile topMid >>= insertTile topLeft
-- ["             Grey head                             Grey head                                                    "
-- ,"Spotted tail             Umber head    Umber tail             Spotted head                                      "
-- ,"             Brown tail                            Brown tail                                                   "
-- ,"             Brown head                            Brown head                                                   "
-- ," Umber tail             Spotted head  Spotted tail             Grey head                                        "
-- ,"             Brown tail                            Umber tail                                                   "
-- ,"                                                                                                                "
-- ,"                                                                                                                "
-- ,"                                                                                                                "]
prettyBoard :: Board -> [String]
prettyBoard (T a b c) = concatMap prettyRow [a,b,c]

-- * Main

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["solve"] -> mapM_ putStrLn  $ intercalate ["",""] $ map prettyBoard solutions
        ["simpleTrace"] -> do
            mapM_ putStrLn  $ intercalate ["",""] $ [("We have " ++ show (length sols) ++ " partial solutions. For example:") : prettyBoard (head sols) | sols <- solutionsTrace]
        ["fullTrace"] -> traceSolutionFull
        ["showTree"] -> putStrLn $ printSolutionTree bestOrder
        _ -> putStrLn $ unlines
          ["Usage: ./HundeSpiel (solve|simpleTrace|fullTrace|showTree)"
          , ""
          , "solve: print all solutions"
          , "simpleTrace: print the first solution for each step"
          , "fullTrace: print the full trace of each board position we are trying"
          , "showTree: print a tree of all the partial solutions. The number indicates how many tiles are missing"
          ]

--solutionsTrace = map fst <$> scanl (\st pos -> st >>= insertTile pos) selectMiddle bestOrder

-- | Trace all the step of the execution, including all backtacking
-- We backtrack a total of 224 times when trying to find all solutions
traceSolutionFull :: IO ()
traceSolutionFull = mapM_ (traceSolutionStep bestOrder) selectMiddle

traceSolutionStep :: [Tile -> Board -> [Board]] -> (Board, TileSet) -> IO ()
traceSolutionStep [] (board, _) = do
  putStrLn $ replicate (3*36 + 4) '-'
  mapM_ putStrLn $ prettyBoard board
  putStrLn $ mkBanner "Found solution!"

traceSolutionStep (pos : poss) (board, tileset) = do
  putStrLn $ replicate (3*36 + 4) '-'
  mapM_ putStrLn $ prettyBoard board
  let nextStep = insertTile pos (board, tileset)
  if null nextStep
    then do
      -- putStrLn "No valid solution!\n"
      -- putStrLn $ "\n" ++ replicate (3*36 + 4) '-'
      putStrLn $ mkBanner "No valid solution using these tiles:"
      mapM_ putStrLn  $ hcat $ map prettyTile $ uncurry (flip replicate) =<< tileset
    --   putStrLn $ replicate (3*36 + 4) '-' ++ "\n"
    else do
      mapM_ (traceSolutionStep poss) nextStep

mkBanner :: String -> String
mkBanner str = "\n" ++ ('='<$ str) ++ "\n" ++ str ++ "\n" ++ ('='<$ str) ++ "\n"


generateTree :: [a -> [a]] -> a -> Tree a
generateTree [] x = Node x []
generateTree (f:fs) x = Node x $ generateTree fs <$> f x

mkSolutionTree :: [Tile -> Board -> [Board]] -> [Tree (Board, TileSet)]
mkSolutionTree positions = generateTree (insertTile <$> positions) <$> selectMiddle

-- We explore a total of 473 partial solutions, of which 2 are actual solutions
-- >>> length $ mkSolutionTreeNr bestOrder
-- 473

-- In total, we try out 6852 positions, included those discarded because they didn't match
-- >>> sum $ calcUnused $ mkSolutionTreeNr bestOrder
-- 6852
mkSolutionTreeNr :: [Tile -> Board -> [Board]] -> Tree Int
-- mkSolutionTreeNr = Node 9 . (fmap . fmap) (length . expand . snd) . mkSolutionTree
mkSolutionTreeNr = Node (length initialTiles `div` 4) . (fmap . fmap) (length . snd) . mkSolutionTree

-- | The number of potential solutions that were discarded because the tile didn't match what was already placed
calcUnused :: Tree Int -> Tree Int
calcUnused (Node x ts) = Node (4*x - length ts) $ calcUnused <$> ts

-- Print all the partial solutions as a tree. The leaves are cases when we can't add more tiles
printSolutionTree :: [Tile -> Board -> [Board]] -> String
printSolutionTree =  Data.Tree.drawTree . fmap show . calcUnused . mkSolutionTreeNr
-- printSolutionTree positions =  Data.Tree.drawForest $ fmap (show . length . expand . snd) <$> mkSolutionTree positions

truncateTree :: Int -> Tree a -> Tree a
truncateTree 0 (Node x _) = Node x []
truncateTree n (Node x xs) = Node x $ truncateTree (n-1) <$> xs


-- The starting position does matter slightly even if it's one of the optimal positions
-- >>> [(length x, sum x) | let full = mkSolutionTreeNr $ take 8 $ drop 0 $ cycle bestOrder, n <- [0..8], let x = calcUnused $ truncateTree n full]
-- >>> [(length x, sum x) | let full = mkSolutionTreeNr $ take 8 $ drop 2 $ cycle bestOrder, n <- [0..8], let x = calcUnused $ truncateTree n full]
-- >>> [(length x, sum x) | let full = mkSolutionTreeNr $ take 8 $ drop 4 $ cycle bestOrder, n <- [0..8], let x = calcUnused $ truncateTree n full]
-- >>> [(length x, sum x) | let full = mkSolutionTreeNr $ take 8 $ drop 6 $ cycle bestOrder, n <- [0..8], let x = calcUnused $ truncateTree n full]
-- [(1,8),(9,228),(31,762),(121,2640),(171,3522),(319,5626),(379,6266),(458,6815),(471,6854)]
-- [(1,8),(9,228),(45,1092),(176,3805),(241,4960),(375,6846),(428,7405),(477,7744),(494,7795)]
-- [(1,8),(9,228),(41,1004),(131,2874),(181,3756),(273,5056),(323,5590),(423,6274),(440,6325)]
-- [(1,8),(9,228),(35,850),(99,2174),(138,2859),(259,4550),(302,5003),(373,5488),(393,5548)]
