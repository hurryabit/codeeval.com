module Main where

import Data.Ratio
import System.Environment

main = fmap head getArgs >>= readFile >>= mapM_ (print . numerator . evaluate [] . reverse . tokenize) . lines

data Token = Op (Rational -> Rational -> Rational) | Num Rational

token :: String -> Token
token "+" = Op (+)
token "*" = Op (*)
token "/" = Op (/)
token num = Num . fromInteger . read $ num

tokenize :: String -> [Token]
tokenize = map token . words

evaluate :: [Rational] -> [Token] -> Rational
evaluate [x]        []         = x
evaluate xs         (Num x:ts) = evaluate (x:xs) ts
evaluate (x1:x2:xs) (Op f:ts)  = evaluate (f x1 x2:xs) ts
evaluate xs ts = error $ "evaluate: " ++ show xs

{-newtype Parser tok a = Parser { runParser :: [tok] -> Maybe (a,[tok]) }

instance Functor (Parser tok) where
	fmap = (<$>)

instance Applicative (Parser tok) where
	pure x = Parser $ \str -> pure (x,str)
	(Parser pf) <*> (Parser px) = Parser $ \str -> do { (f,str') <- pf str; (x,str'') <- px str'; return (f x,str'') }

instance Alternative (Parser tok) where
	empty = Parser $ \str -> empty
	(Parser px) <|> (Parser py) = Parser $ \str -> px str <|> py str

op :: Parser Token (Rational -> Rational -> Rational)
op = Parser $ \str -> case str of
						  (Op f):str' -> pure (f,str')
						  _           -> empty

num :: Parser Token Rational
num = Parser $ \str -> case str of
						   (Num n):str' -> pure (n,str')
						   _			-> empty

expr :: Parser Token Rational
expr = num <|> (op <*> expr <*> expr)

evaluate2 :: [Token] -> Rational
evaluate2 = fst . fromJust . runParser expr-}