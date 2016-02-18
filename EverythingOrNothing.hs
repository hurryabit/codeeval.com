{-# LANGUAGE RecordWildCards #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Char (isSpace, toLower)
import Text.ParserCombinators.ReadP  hiding (get)
import System.Environment (getArgs)

main = do
  arg1:_ <- getArgs
  input <- readFile arg1
  forM_ (lines input) $ \line -> do
    let ops = parseOperationList line
    case evalStateT (mapM_ execute ops) initialRights of
      Just _  -> putStrLn "True"
      Nothing -> putStrLn "False"

newtype User = User { getUserId :: Int }
  deriving (Show, Eq, Ord, Ix)

newtype File = File { getFileid :: Int }
  deriving (Show, Eq, Ord, Ix)

data AccessRight = Grant | Write | Read
  deriving (Show, Eq, Ord, Enum, Ix, Bounded)

type AccessTable = Array (User,File,AccessRight) Bool

data Action = GrantRight AccessRight User | WriteFile | ReadFile
  deriving (Show, Eq, Ord)

data Operation = Operation { user   :: User, file   :: File, action :: Action }
  deriving (Show, Eq, Ord)

execute :: Operation -> StateT AccessTable Maybe ()
execute (Operation { .. }) = do
  table <- get
  case action of
    ReadFile  -> guard (table ! (user,file,Read ))
    WriteFile -> guard (table ! (user,file,Write))
    GrantRight right other_user -> do
      guard (table ! (user,file,Grant))
      put (table // [((other_user,file,right),True)])

initialRights :: AccessTable
initialRights = accumArray (||) False ((User 1,File 1,minBound),(User 6,File 3,maxBound)) $ do
  let table =
        [ (User 1, [(File 1, [Grant, Write, Read]), (File 2, [Grant, Write])])
        , (User 2, [(File 1, [Write, Read]), (File 2, [Write]), (File 3, [Read])])
        , (User 3, [(File 1, [Grant, Read]), (File 2, [Grant]), (File 3, [Grant, Read])])
        , (User 4, [(File 1, [Grant, Write]), (File 2, [Grant, Write, Read]), (File 3, [Grant])])
        , (User 5, [(File 1, [Write, Read]), (File 3, [Write])])
        , (User 6, [(File 1, [Read]), (File 2, [Write]), (File 3, [Write,Read])])
        ]
  (user,file_rights) <- table
  (file,rights) <- file_rights
  right <- rights
  return ((user,file,right),True)
  where

to p = string "=>" *> p

parseRead = readS_to_P reads
parseUser = User <$> (string "user_" *> parseRead)
parseFile = File <$> (string "file_" *> parseRead)
parseAccessRight = foldl1 (<++) (map (\r -> string (map toLower (show r)) *> pure r) [Grant, Write, Read])
parseAction = do
  right <- parseAccessRight
  case right of
    Grant -> GrantRight <$> to parseAccessRight <*> to parseUser
    Write -> pure WriteFile
    Read  -> pure ReadFile
parseOperation = Operation <$> parseUser <*> to parseFile <*> to parseAction
parseOperationList input =
  let [(os,_)] = readP_to_S (sepBy1 parseOperation (munch1 isSpace) <* eof) input
  in  os
