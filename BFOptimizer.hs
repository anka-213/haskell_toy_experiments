{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module BFOptimizer where

import Text.ParserCombinators.ReadP
import Data.List (intercalate)

data Op
    = Inc {offset :: Int, amount :: Int}
    | Set {offset :: Int, value :: Int}
    | MovPtr { amount :: Int}
    | LoopZ { offset :: Int, code :: [Op] } -- [code] -- Inner offsets are relative
    | LoopNTimes { offset:: Int, code :: [Op]} -- Repeat code based on value at offset
    | Print { offset :: Int }
    | Get { offset :: Int }
    deriving (Eq, Show)

{- Ideas:

- Parallel operations where order doesn't matter
- push motions to the right
-

-}

baseBFParser :: ReadP [Op]
baseBFParser = (parseSingle =<< get) <++ ([] <$ eof)
  where
    x <: xs = fmap (x:) xs
    parseSingle c =
        case c of
            '+' -> Inc 0 1     <: baseBFParser
            '-' -> Inc 0 (-1)  <: baseBFParser
            '>' -> MovPtr 1    <: baseBFParser
            '<' -> MovPtr (-1) <: baseBFParser
            '.' -> Print 0     <: baseBFParser
            ',' -> Get 0       <: baseBFParser
            '[' -> (:) . LoopZ 0 <$> baseBFParser <*> baseBFParser
            ']' -> pure []
            _   -> baseBFParser

parseBF :: String -> Maybe [Op]
parseBF str = case readP_to_S baseBFParser str of
    [(ops, "")] -> Just ops
    _           -> Nothing

-- >>> parseBF "++[->+<]"
-- Just [Inc {offset = 0, amount = 1},Inc {offset = 0, amount = 1},LoopZ {offset = 0, code = [Inc {offset = 0, amount = -1},MovPtr {amount = 1},Inc {offset = 0, amount = 1},MovPtr {amount = -1}]}]

optimize:: [Op] -> [Op]
optimize = optimizeInner . handleLoops . propagateOffsets 0

handleLoops :: [Op] -> [Op]
handleLoops (LoopZ off inner : ops) = optimizeLoop off inner ++ handleLoops ops
handleLoops (op:ops) = op : handleLoops ops
handleLoops [] = []

optimizeInner :: [Op] -> [Op]
-- optimizeInner (MovPtr 0 : ops) = optimizeInner ops
-- optimizeInner (MovPtr n : MovPtr m : ops) = optimizeInner $ MovPtr (n + m) : ops
-- optimizeInner (MovPtr n : op : ops) = optimizeInner $ op {offset = offset op + n} : optimizeInner (MovPtr n : ops) -- Inefficient?
optimizeInner (Inc _ 0 : ops) = optimizeInner ops
optimizeInner (Inc off n : Inc off' n' : ops) | off == off' = optimizeInner $ Inc off (n+n') : ops -- TODO: Sort or merge
optimizeInner (Set off n : Inc off' n' : ops) | off == off' = optimizeInner $ Set off (n+n') : ops -- TODO: Sort or merge
optimizeInner (Set off _ : Set off' n' : ops) | off == off' = optimizeInner $ Set off n'     : ops -- TODO: Sort or merge
optimizeInner (Inc off _ : Set off' n' : ops) | off == off' = optimizeInner $ Set off n'     : ops -- TODO: Sort or merge
optimizeInner (op:ops) = op : optimizeInner ops
optimizeInner [] = []

optimizeLoop :: Int -> [Op] -> [Op]
optimizeLoop off innerOps = result
  where
    result = if isSimpleLoop && isStatic
        -- then [LoopNTimes off optCode]
        then splitLoop off optCode
        else [LoopZ off optCode]
    optCode = optimize innerOps
    -- TODO: Check inner loops for current index as well
    isSimpleLoop = [op | op <- optCode, offset op == 0] == [Inc 0 (-1)] -- We only decrement the 0th elem once
    isStatic = all isStaticOp optCode
    isStaticOp (MovPtr{}) = False
    isStaticOp (LoopZ{}) = False -- TODO: Make this better
    isStaticOp (LoopNTimes{}) = False -- TODO: Make this better
    isStaticOp _ = True

-- Split simple loops into individual changes
splitLoop :: Int -> [Op] -> [Op]
splitLoop off = go
  where
    go [] = [Set off 0] -- Always end with a reset
    go (Inc 0 (-1) : ops) = go ops -- Ignore the reset part
    go (Inc off' n : ops) = LoopNTimes off [Inc (off + off') n] : go ops
    go (op : _) = error $ "splitLoop: Unexpected op: " ++ show op


propagateOffsets :: Int -> [Op] -> [Op]
propagateOffsets 0 [] = []
propagateOffsets n [] = [MovPtr n]
propagateOffsets !n (MovPtr m : ops) = propagateOffsets (n + m) ops
propagateOffsets n (op : ops) = op { offset = offset op + n } : propagateOffsets n ops
-- propagateOffsets n (Inc {offset , amount } : ops) = _
-- propagateOffsets n (Set {offset , value } : ops) = _
-- propagateOffsets n (LoopZ { offset , code } : ops) = _
-- propagateOffsets n (LoopNTimes { offset, code } : ops) = _
-- propagateOffsets n (Print { offset } : ops) = _
-- propagateOffsets n (Get { offset } : ops) = _

-- TODO: Split up optimization into phases (e.g. one for offset propagation)
-- TODO: Add a merged thing for many inc and set opers
-- TODO: Handle static vs dynamic loops (Static contains no MovPtr)
-- TODO: Properly handle a MovPtr prior to a loop (what happens if loop is dynamic?)
-- TODO: Use quickcheck to test the optimizations
-- Maybe try doing this in Agda to get proofs?

parseAndOpt :: String -> Maybe [Op]
parseAndOpt = fmap optimize . parseBF

-- >>> parseAndOpt ">++[>>++<<-]"
-- Just [Inc {offset = 1, amount = 2},LoopZ {offset = 1, code = [Inc {offset = 2, amount = 2},Inc {offset = 0, amount = -1}]},MovPtr {amount = 1}]

main :: IO ()
main = case parseAndOpt ">[-]++[>+>++<<-]>[-<+>]" of
    Nothing -> error "Parse failed"
    Just ops -> pp ops

pp :: [Op] -> IO ()
pp = putStrLn . prettyPrint 0

prettyPrint :: Int -> [Op] -> String
prettyPrint indent ops = intercalate "\n"  $ fmap (prettyPrintOp indent) ops
prettyPrintOp :: Int -> Op -> String
prettyPrintOp indent op = replicate indent ' ' ++ case op of
  Set off val -> "a[" ++ show off ++ "] = " ++ show val
  Inc off val -> "a[" ++ show off ++ "] += " ++ show val
  MovPtr val  -> "a += " ++ show val
  LoopZ offset inner -> intercalate "\n"
    [ "a += " ++ show offset
    , replicate indent ' ' ++ "while (a[0] != 0) {"
    , prettyPrint (indent + 2) inner
    , replicate indent ' ' ++ "}"
    , "a -= " ++ show offset
    ]
  LoopNTimes offset inner -> intercalate "\n" -- TODO: Incorrect
    [ "a[" ++ show offset ++ "].times {"
    , prettyPrint (indent + 2) inner
    , replicate indent ' ' ++ "}"
    ]
  Print off -> "print(a["++ show off ++ "])"
  Get off -> "get(a["++ show off ++ "])"


-- >>> unlines ["a","b"]
-- "a\nb\n"
