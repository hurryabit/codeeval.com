import Data.Array
import Data.List
import System.Environment (getArgs)

main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  mapM_ (putStrLn . formatCodes . buildCodeTree) (lines input)


data CodeTree =
    Branch CodeTree CodeTree
  | Leaf Char
  deriving (Show, Eq, Ord)

type PQueue = [(Int,CodeTree)]

initial :: String -> PQueue
initial str =
  let histo  = accumArray (+) 0 ('a','z') (map (\c -> (c,1)) str)
      unsrtd = map (\(c,n) -> (n,Leaf c)) (filter ((0 <) . snd) (assocs histo))
  in  sort unsrtd

advance :: PQueue -> PQueue
advance ((n1,t1):(n2,t2):nts) = insert (n1+n2,Branch t1 t2) nts

buildCodeTree :: String -> CodeTree
buildCodeTree = snd . head . until isSingleton advance . initial
  where
    isSingleton xs = case xs of
      [_] -> True
      _   -> False

extractCodes :: CodeTree -> [(Char,String)]
extractCodes tree = case tree of
  Branch left right -> mapSnd ('0':) (extractCodes left) `merge` mapSnd ('1':) (extractCodes right)
  Leaf c            -> [(c,"")]
  where
    mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
    mapSnd f = map (\(x,y) -> (x,f y))
    merge :: Ord a => [a] -> [a] -> [a]
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys) = case x `compare` y of
      LT -> x : merge xs (y:ys)
      EQ -> x : y : merge xs ys
      GT -> y : merge (x:xs) ys

formatCodes :: CodeTree -> String
formatCodes = intercalate " " . map formatCode . extractCodes
  where
    formatCode (c,s) = [c] ++ ": " ++ s ++ ";"
