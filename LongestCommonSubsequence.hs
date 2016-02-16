import Data.Array
import System.Environment (getArgs)

lcs :: Eq a => [a] -> [a] -> [a]
lcs xs ys =
  let lx = length xs
      ly = length ys
      xa = listArray (1,lx) xs
      ya = listArray (1,ly) ys
      bnds = ((0,0),(lx,ly))
      tab = listArray bnds (map fun (range bnds))
      fun (i,j)
        | i == 0 || j == 0 = 0
        | xa ! i == ya ! j = 1 + tab ! (i-1,j-1)
        | otherwise        = max (tab ! (i-1,j)) (tab ! (i,j-1))
      restore (i,j) zs
        | val == 0             = zs
        | val == tab ! (i-1,j) = restore (i-1,j) zs
        | val == tab ! (i,j-1) = restore (i,j-1) zs
        | otherwise            = restore (i-1,j-1) ((xa ! i):zs)
        where
          val = tab ! (i,j)
  in  restore (lx,ly) []

lcsExt :: String -> String
lcsExt line =
  let (xs,_:ys) = break (';' ==) line
  in  lcs xs ys

main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  mapM_ (putStrLn . lcsExt) (lines input)
