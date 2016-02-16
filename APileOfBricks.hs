import Control.Applicative
import Data.List
import System.Environment

import Text.ParserCombinators.ReadP

data Point = Point { _x, _y, _z :: Int }
  deriving (Show)

data Brick = Brick { _id :: Int, _pos1, _pos2 :: Point }
  deriving (Show)

data Pile = Pile { _hole :: Brick, _bricks :: [Brick] }
  deriving (Show)

parens :: Char -> Char -> ReadP a -> ReadP a
parens open close = between (char open) (char close)

parseInt :: ReadP Int
parseInt = readS_to_P (readsPrec 0)

parsePoint2D :: ReadP Point
parsePoint2D = skipSpaces *> parens '[' ']' (Point <$> parseInt <*> (char ',' *> parseInt) <*> pure 0)

parsePoint3D :: ReadP Point
parsePoint3D = skipSpaces *> parens '[' ']' (Point <$> parseInt <*> (char ',' *> parseInt) <*> (char ',' *> parseInt))

parseHole :: ReadP Brick
parseHole = Brick <$> pure 0 <*> parsePoint2D <*> parsePoint2D

parseBrick :: ReadP Brick
parseBrick = parens '(' ')' $ Brick <$> parseInt <*> parsePoint3D <*> parsePoint3D

parsePile :: ReadP Pile
parsePile = Pile <$> parseHole <*> (char '|' *> sepBy1 parseBrick (char ';') <* eof)

runParser :: ReadP a -> String -> a
runParser p s =
  let [(x,"")] = readP_to_S p s
  in  x

covers :: Brick -> Brick -> Bool
covers hole brick =
  let dims br fs = sort [ abs (f (_pos1 br) - f (_pos2 br)) | f <- fs ]
      hole_dims  = dims hole [_x, _y]
      brick_dims = dims brick [_x, _y, _z]
  in  and $ zipWith (<=) brick_dims hole_dims

throwableIds :: Pile -> [Int]
throwableIds pile = map _id $ filter (_hole pile `covers`) (_bricks pile)

prettyIds :: [Int] -> String
prettyIds []  = "-"
prettyIds ids = intercalate "," (map show ids)

main :: IO ()
main = do
  fname:_ <- getArgs
  contents <- readFile fname
  let lns = lines contents
      ids = map (sort . throwableIds . runParser parsePile) lns
  mapM_ (putStrLn . prettyIds) ids
