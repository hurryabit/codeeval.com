module Main where

import Control.Monad
import Data.List
import Data.Ord
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    mapM_ (putStrLn . process) (lines input)

process line
    | null ns   = "-"
    | otherwise = intercalate "," $ map show ns
    where
        [(inst,"")] = readP_to_S parseInstance line
        ns = sort . selItems . head .  sortBy solCmp $ solutions inst
        s1 `solCmp` s2 = case comparing sumPrice s2 s1 of
                                EQ  -> comparing sumWeight s1 s2
                                cmp -> cmp

data Item = Item
    { number :: Int
    , weight :: Double
    , price  :: Int
    }
    deriving Show

data Instance = Instance
    { maxWeight :: Double
    , items     :: [Item]
    }
    deriving Show

data Solution = Solution
    { selItems  :: [Int]
    , sumWeight :: Double
    , sumPrice  :: Int
    }
    deriving Show

solutions :: Instance -> [Solution]
solutions (Instance mw is) = map (liftM3 Solution (map number) (sum . map weight) (sum . map price)) $ solutions' mw $ sortBy (flip $ comparing weight) is

solutions' :: Double -> [Item] -> [[Item]]
solutions' mw is = case dropWhile ((mw <) . weight) is of
    []    -> [[]]
    i:is' -> map (i:) (solutions' (mw-weight i) is') ++ solutions' mw is'

parseInt :: ReadP Int
parseInt = readS_to_P reads

parseDouble :: ReadP Double
parseDouble = readS_to_P reads

parseItem :: ReadP Item
parseItem = between (char '(') (char ')') $ liftM3 Item parseInt (char ',' >> parseDouble) (string ",$" >> parseInt)

parseInstance :: ReadP Instance
parseInstance = liftM2 Instance parseDouble (skipSpaces >> char ':' >> manyTill (skipSpaces >> parseItem) eof)
