module ReverseInfinite where

reverse' :: [a] -> [a]
reverse' = build $ \c z -> foldl (flip c) z

getFst = _
