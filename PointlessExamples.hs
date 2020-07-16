import Data.List (uncons)
import Control.Applicative (liftA2)
import Data.Function (fix)


jonk :: (a -> b) -> ((a -> Int) -> Int) -> ((b -> Int) -> Int)
jonk = flip (.) . flip (.)
-- jonk ab = _
-- jonk ab aii bi = aii $ \a -> bi $ ab a

list :: b -> (a -> [a] -> b) -> [a] -> b
list nil _ [] = nil
list _ cons (x : xs) = cons x xs
-- list =  ((. uncons).) . (. uncurry) . maybe
-- list = (.) (. uncons) . (. uncurry) . maybe
-- list = (.) (. uncons) . \nil -> (. uncurry) $ maybe nil 
-- list = (.) (. uncons) . \nil -> maybe nil . uncurry
-- list nil = (.) (. uncons) $ maybe nil . uncurry
-- list nil = (. uncons) . maybe nil . uncurry
-- list nil cons = (. uncons) $ maybe nil (uncurry cons)
-- list nil cons = maybe nil (uncurry cons) . uncons
-- list nil cons xs = maybe nil (uncurry cons) $ uncons xs

list' :: (a -> [a] -> b) -> b -> [a] -> b
list' = flip list

zoop :: (a -> b -> b) -> b -> [a] -> b
zoop abb b = fix $ \ self xs -> maybe b (uncurry (\a as -> abb a $ self as)) $ uncons xs
-- zoop abb b = fix $ \ self xs -> maybe b (\(a, as) -> abb a $ self as) $ uncons xs
-- zoop = fix $ liftA2 (=<<) ((list' .) . flip (.)) . ((flip (.) .) .)
-- zoop = fix $ liftA2 (=<<) ((list' .) . flip (.)) . \zoop' -> ((flip (.) .) . zoop')
-- zoop = fix $ \zoop' -> liftA2 (=<<) ((list' .) . flip (.)) ((flip (.) .) . zoop')
-- zoop = liftA2 (=<<) ((list' .) . flip (.)) ((flip (.) .) . zoop) 
-- zoop aab = (=<<) ((list' .) $ flip (.) aab) ((flip (.) .) $ zoop aab) 
-- zoop aab = (=<<) (list' . flip (.) aab) (flip (.) . zoop aab) 
-- zoop aab = list' . flip (.) aab =<<  flip (.) . zoop aab 
-- zoop aab = list' =<< flip (.) aab . flip (.) . zoop aab 
-- zoop aab = list <*> flip (.) aab . flip (.) . zoop aab 
-- zoop aab = liftA2 list id (flip (.) aab . flip (.) . zoop aab )
-- zoop aab = list' <$> (flip (.) aab . flip (.) . zoop aab ) <*> id
-- zoop aab b = list' (flip (.) aab . flip (.) . zoop aab $ b) b
-- zoop aab b as = maybe _ _ (uncons as)
-- zoop _ b [] = b
-- zoop abb b (a:as) = abb a $ zoop abb b as