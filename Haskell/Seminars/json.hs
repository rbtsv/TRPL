{- HLINT ignore "Use lambda-case" -}
import Control.Monad.State (StateT (..))
import Control.Applicative ((<|>), Alternative (empty))
import Data.Char (digitToInt)
type Parser a = StateT String Maybe a

item :: Parser Char
item = StateT $ \s -> case s of
    [] -> Nothing
    (c : cs) -> Just (c, cs)

char :: Char -> Parser Char
char c = item >>= \x -> if c==x then pure x else empty

word :: String -> Parser String
word [] = pure ""
word (x : xs) = do
    y <- char x
    ys <- word xs
    return (y:ys) 

star1 :: Parser a -> Parser [a]
star1 p = do
    y <- p
    ys <- star1 p <|> pure []
    return $ y : ys

star :: Parser a -> Parser [a]
star p = star1 p <|> pure []

sat :: (Char -> Bool) -> Parser Char
sat f = do
    c <- item
    if f c then pure c else empty

num :: Parser Char
num = sat (`elem` ['0'..'9'])

type Member = (String, Value)

data Value = Null
           | Bl Bool
           | Str String
           | Number Int
           | Array [Value]
           | Object [Member]

valueP :: Parser Value
valueP = nullP <|> boolP <|> arrayP <|> objectP

wsP :: Parser String
wsP = star $ char '\SP' <|> char '\HT' <|> char '\LF' <|> char '\CR'

nullP :: Parser Value
nullP = do
    word "null"
    return Null

trueP :: Parser Value
trueP = do
    word "true"
    return $ Bl True

falseP :: Parser Value
falseP = do
    word "false"
    return $ Bl False

boolP :: Parser Value
boolP = trueP <|> falseP

wsC :: Parser a -> Parser a
wsC a = wsP *> a <* wsP

arrayP :: Parser Value
arrayP = do
    wsC $ char '['
    v <- valueP
    let vs = wsC $ char ','
    rest <- star (vs *> valueP)
    wsC $ char ']'
    return $ Array (v : rest)

safeStringP :: Parser String
safeStringP = star $ sat (/= '"')

memberP :: Parser Member
memberP = do
    s <- char '\"' *> safeStringP <* char '\"'
    wsC $ char ':'
    v <- valueP
    return (s, v)

objectP :: Parser Value
objectP = do
    wsC $ char '{'
    v <- memberP
    let vs = wsC $ char ','
    rest <- star (vs *> memberP)
    wsC $ char '}'
    return $ Object (v : rest)

natParser :: Parser Int
natParser = do
    n <- star1 num
    let ns = map digitToInt n
    return $ foldl (\x y -> x*10 + y) 0 ns




