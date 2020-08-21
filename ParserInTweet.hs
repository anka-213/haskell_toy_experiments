import Control.Monad.State
import Control.Applicative
import Data.List

data T = T [T] deriving (Show)
type Parser = Parser

ch :: Char -> Parser Char
ch c = mfilter (== c) $ StateT uncons

parens :: Parser T
parens = T <$ ch '(' <*> many parens <* ch ')'

parse :: Parser a -> String -> Maybe (a, String)
parse = runStateT

-- >>> parse parens "(()())"
-- attempting to use module ‘fake_uid:Main’ (/Users/anka/cclaw/dmnmd/languages/haskell/ParserInTweet.hs) which is not loaded

-- $> parse parens "(()())"

-- $> :t StateT uncons