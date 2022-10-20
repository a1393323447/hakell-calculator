module Main where

import Data.List ()
import System.IO ()
import Data.Char (isDigit, isSpace)
import Data.String (IsString(fromString))
import Data.Maybe (fromMaybe)

data BinOp = Add | Sub | Mul | Div
    deriving (Show)

data UnaOp = Neg deriving (Show)

-- AST
data Expr = Val Double  | Bin Expr BinOp Expr | Una UnaOp Expr
    deriving(Show)

safeOper :: Double -> BinOp -> Double -> Maybe Double
safeOper x Div 0 = Nothing
safeOper x op y = case op of
    Add -> Just (x + y)
    Sub -> Just (x - y)
    Mul -> Just (x * y)
    Div -> Just (x / y)

unaOper :: UnaOp -> Double -> Double
unaOper Neg op = -op

eval :: Expr -> Maybe Double
eval (Val x) = Just x
eval (Bin lhs op rhs) = do
    l <- eval lhs
    r <- eval rhs
    safeOper l op r
eval (Una op opend) = do
    opend <- eval opend
    Just (unaOper op opend)

-- Token
data Token =  Operator BinOp    |
              Number String     |
              OpenParens        |
              CloseParens
              deriving (Show)

data LexerState = Start          |
                  Digits         |
                  DigitsAfterDot
                  deriving (Show)

isOper :: Char -> Bool
isOper c = c `elem` ['+', '-', '*', '/']

getOper :: Char -> Maybe BinOp
getOper c = case c of
    '+' -> Just Add
    '-' -> Just Sub
    '*' -> Just Mul
    '/' -> Just Div
    _ -> Nothing

lexer :: [Char] -> LexerState -> [Char] -> Maybe [Token]
-- lex symbol, operators or num
lexer (x:xs) Start []
    | x == '(' = do                              -- lex '('
        tokens <- lexer xs Start []
        Just (OpenParens:tokens)
    | x == ')' = do                              -- lex ')'
        tokens <- lexer xs Start []
        Just (CloseParens:tokens)
    | isOper x = do                              -- lex an operator
        op <- getOper x
        tokens <- lexer xs Start []
        Just (Operator op : tokens)
    | isDigit x = lexer xs Digits [x]            -- start lex a double
    | isSpace x || x == '\n' = lexer xs Start [] -- skip white space and newline
    | otherwise = Nothing                        -- unexpected charater
-- lex num
lexer [] Digits [] = Nothing
lexer [] Digits history = Just [Number history]
lexer (x:xs) Digits history
    | x == '.' = lexer xs DigitsAfterDot (history ++ [x])
    | isDigit x = lexer xs Digits (history ++ [x])
    | otherwise = do
        tokens <- lexer (x:xs) Start []
        Just (Number history : tokens)
lexer (x:xs) DigitsAfterDot history
    | isDigit x = lexer xs DigitsAfterDot (history ++ [x])
    | otherwise = do
        tokens <- lexer xs Start []
        Just (Number history : tokens)
lexer [] DigitsAfterDot history = Just [Number history]
-- unexpected
lexer [] _ [] = Just []
lexer _ _ _ = Nothing

lexTokens :: [Char] -> [Token]
lexTokens str = fromMaybe [] (lexer str Start [])

-- Parser
-- operator level:
-- 0: +  - 
-- 1: *  /  %
-- 2: -
-- final: ()

parseOp0 :: [Token] -> Either [Token] ([Token], BinOp)
parseOp0 (Operator Add : ts) = Right (ts, Add)
parseOp0 (Operator Sub : ts) = Right (ts, Sub)
parseOp0 ts = Left ts

parseOp1 :: [Token] -> Either [Token] ([Token], BinOp)
parseOp1 (Operator Mul : ts) = Right (ts, Mul)
parseOp1 (Operator Div : ts) = Right (ts, Div)
parseOp1 ts = Left ts

parseCloseP :: [Token] -> Maybe [Token]
parseCloseP (t:ts) = case t of
    CloseParens -> Just ts
    _ -> Nothing
parseCloseP _ = Nothing

data ParserState = Zero | One | Two | Final
--  current input tokens               reset tokens 
parse :: [Token] -> ParserState -> Maybe ([Token], Expr)
parse (x:xs) Zero = do
    (rest, lhs) <- parse (x:xs) One
    case parseOp0 rest of
        Left rest -> Just (rest, lhs)
        Right (rest, oper) -> do
            (ts, rhs) <- parse rest Zero
            Just (ts, Bin lhs oper rhs)
parse (x:xs) One = do
    (rest, lhs) <- parse (x:xs) Two
    case parseOp1 rest of
        Left rest -> Just (rest, lhs)
        Right (rest, oper) -> do
            (ts, rhs) <- parse rest One
            Just (ts, Bin lhs oper rhs)
parse (Operator op : xs) Two = case op of
    Sub -> do
        (ts, expr) <- parse xs Two
        Just (ts, Una Neg expr)
    _ -> Nothing
parse tokens Two = parse tokens Final
parse (x:xs) Final = case x of
    Number num -> Just (xs, Val (read num))
    OpenParens -> do
        (ts, expr) <- parse xs Zero
        rest <- parseCloseP ts
        Just (rest, expr)
    _ -> Nothing

parse [] _ = Nothing

parseExpr :: [Char] -> Maybe Expr
parseExpr str = do
    tokens <- lexer str Start []
    (rest, expr) <- parse tokens Zero
    Just expr

calculate :: [Char] -> Maybe Double
calculate str = do
    expr <- parseExpr str
    eval expr

main :: IO ()
main = print (calculate "1 + 2 * (-2 + 3) + 2 / (3 - 1)")
