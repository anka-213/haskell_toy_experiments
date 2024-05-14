import Control.Monad.State
import Control.Applicative
import Data.List
import Data.Maybe
import Control.Arrow (Arrow(first))

type Parser = StateT String Maybe

type Number = Double
-- type Number = Expr

data Expr = Plus Expr Expr | Times Expr Expr | Minus Expr Expr | Divide Expr Expr | Lit Double
  deriving (Show, Eq)

instance Num Expr where
    (+) = Plus
    (-) = Minus
    (*) = Times
    abs = undefined
    signum = undefined
    fromInteger = Lit . fromInteger
    -- (/) = Divide

instance Read Expr where
    readsPrec p = (fmap . first) Lit . readsPrec p

ch :: Char -> Parser Char
ch = sat . (==)

str :: String -> Parser String
str = mapM ch

sat :: (Char -> Bool) -> Parser Char
sat p = mfilter p $ StateT uncons

expr :: Parser Number
expr = do
    x <- term
    subExpr x

subExpr :: Number -> Parser Number
subExpr x = do
    do
        op <- oper
        y <- term
        subExpr $ op x y
    <|> pure x

oper :: Parser (Number -> Number -> Number)
oper = msum
    [ (+) <$ ch '+'
    , (-) <$ ch '-'
    , (*) <$ ch '*'
    , (/) <$ ch '/'
    ]

term :: Parser Number
term = ch '(' *> expr <* ch ')' <|> int

int :: Parser Number
int = StateT $ listToMaybe . reads
-- int = read <$> many (sat (element []))

parse :: Parser a -> String -> Maybe (a, String)
parse = runStateT

{-
-- >>> parse parens "(()())"
-- attempting to use module ‘fake_uid:Main’ (/Users/anka/cclaw/dmnmd/languages/haskell/ParserInTweet.hs) which is not loaded

--- $> parse parens "(()())"

-- $> parse expr "3+6"

-- $> parse expr "5-3+3"

--- $> :t StateT uncons

-}