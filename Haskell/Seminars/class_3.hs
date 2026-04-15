import Control.Monad (ap, liftM, MonadPlus)
import Control.Applicative (Alternative(..))
import Data.Char (ord)
-- type Parser = String -> Tree
-- type Parser = String -> (Tree, String)
-- type Parser = String -> [(Tree, String)]
-- type Parser a = String -> [(a, String)]
-- type Parser s a = s -> [(a, s)]
-- newtype Parser a = Parser {runParse :: String -> [(a, String)]}
newtype Parser a = Parser {runParse :: String -> Maybe (a, String)}

pureP :: a -> Parser a
pureP v = Parser $ \inp -> Just (v, inp)
zero :: Parser a
zero = Parser $ const Nothing
item :: Parser Char
item = Parser $ \inp -> case inp of
    [] -> Nothing
    (x : xs) -> Just (x, xs)

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = Parser $ \inp -> case runParse p inp of
    Nothing -> Nothing
    Just (v, inp') -> runParse (f v) inp'

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap = liftM
instance Applicative Parser where
    pure :: a -> Parser a
    pure = pureP
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) = ap
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) = bind
instance Alternative Parser where
    empty :: Parser a
    empty = zero
    (<|>) :: Parser a -> Parser a -> Parser a
    Parser l <|> Parser r = Parser $ \inp ->
        case l inp of
            Nothing ->
                r inp
            lv -> lv

instance MonadPlus Parser

sat :: (Char -> Bool) -> Parser Char
sat f = do
    c <- item
    if f c then pure c else zero

char :: Char -> Parser Char
char c = sat (== c)

lower :: Parser Char
lower = sat (`elem` ['a'..'z'])

upper :: Parser Char
upper = sat (`elem` ['A'..'Z'])

alpha :: Parser Char
alpha = lower <|> upper

num :: Parser Char
num = sat (`elem` ['0'..'9'])

alphaNum :: Parser Char
alphaNum = alpha <|> num

satstring :: [Char -> Bool] -> Parser String
satstring [] = pure ""
satstring (x : xs) = do
    c <- sat x
    cs <- satstring xs
    return $ c : cs

word :: String -> Parser String
word s = satstring $ map (==) s

star1 :: Parser a -> Parser [a]
star1 p = do
    y <- p
    ys <- star p
    return $ y : ys

star :: Parser a -> Parser [a]
star p = star1 p <|> pure []

try :: Parser a -> Parser ()
try (Parser p) = Parser $ \inp -> case p inp of
    Nothing -> Nothing
    Just a -> Just ((), inp)

try' :: Parser a -> Parser ()
try' p = p *> pure () 

data Expression =
    Var |
    Sum Expression Expression |
    Const Int |
    Mult Expression Expression |
    Sin Expression |
    Cos Expression |
    Pow Expression Int
    deriving (Show, Eq)

exprParser :: Parser Expression
exprParser = varParser <|> sumParser <|> cosParse <|> powParser

varParser :: Parser Expression
varParser = do
    char 'x'
    return Var

sumParser :: Parser Expression
sumParser = do
    char '('
    x <- exprParser
    char '+'
    y <- exprParser
    char ')'
    return $ Sum x y

cosParse :: Parser Expression
cosParse = fmap Cos $ word "cos(" *> exprParser <* char ')'

digitToInt :: Char -> Int
digitToInt x = (ord x) - (ord '0')

natParser :: Parser Int
natParser = do
    n <- star1 num
    let ns = map digitToInt n
    return $ foldl (\x y -> x*10 + y) 0 ns

intParser :: Parser Int
intParser = natParser <|> (char '-' *> fmap (*(-1)) natParser)

powParser :: Parser Expression
powParser = do
    char '('
    x <- exprParser
    char '^'
    y <- intParser
    char ')'
    return $ Pow x y


