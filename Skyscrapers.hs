import Control.Applicative
import Control.Monad (guard)
import Data.List (mapAccumL, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

data Skyscraper = Skyscraper { left, height, right :: Int }
  deriving (Show)

type Skyline = [Skyscraper]

parseInt :: ReadP Int
parseInt = readS_to_P reads

parseSkyscraper :: ReadP Skyscraper
parseSkyscraper = between (char '(') (char ')')
    (Skyscraper <$> parseInt <*> (char ',' *> parseInt) <*> (char ',' *> parseInt))

parseSkyline :: ReadP Skyline
parseSkyline = sepBy1 parseSkyscraper (char ';' <* skipSpaces)

runReadP :: ReadP a -> String -> a
runReadP p = fst . head . readP_to_S (p <* eof)

type MultiSet a = Map a Int

singletonMS :: a -> MultiSet a
singletonMS x = Map.singleton x 1

insertMS :: Ord a => a -> MultiSet a -> MultiSet a
insertMS x = Map.insertWith (+) x 1

deleteMS :: Ord a => a -> MultiSet a -> MultiSet a
deleteMS = Map.update (\count -> guard (count > 1) >> return (count-1))

findMaxMS :: Ord a => MultiSet a -> a
findMaxMS = fst . Map.findMax

data Event = Event { lefts, rights :: [Int] }
  deriving (Show)

addEntry :: Either Int Int -> Event -> Event
addEntry (Left  x) evt = evt { lefts  = x : lefts  evt }
addEntry (Right x) evt = evt { rights = x : rights evt }

insertEntry :: Ord k => k -> Either Int Int -> Map k Event -> Map k Event
insertEntry key lr = Map.alter (Just . addEntry lr . fromMaybe (Event [] [])) key

processSkyline :: Skyline -> [Int]
processSkyline sl =
  let insertSkyscraper (Skyscraper l h r) = insertEntry l (Left h) . insertEntry r (Right h)
      events = foldl (flip insertSkyscraper) Map.empty sl
      combine (currHs, lastH) (pos,Event ls rs) =
        let currHs' = foldl (flip insertMS) (foldl (flip deleteMS) currHs rs) ls
            lastH'  = findMaxMS currHs'
            instrs  = if lastH' == lastH then [] else [pos,lastH']
        in  ((currHs',lastH'),instrs)
      (_,instrs) = mapAccumL combine (singletonMS 0,0) (Map.toAscList events)
  in  concat instrs

main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  mapM_ (putStrLn . intercalate " " . map show . processSkyline . runReadP parseSkyline) (lines input)
