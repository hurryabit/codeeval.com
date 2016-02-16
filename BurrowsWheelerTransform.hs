import Data.Array
import Data.Char
import Data.List
import System.Environment

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    mapM_ (putStrLn . unBWT '$' . init) $ lines input

-- All you need to know: http://www.cs.ox.ac.uk/richard.bird/online/BirdMu2004Inverting.pdf
unBWT :: Char -> String -> String
unBWT eof xs = run (snd $ table ! i0) [eof]
    where
--        srtd = sort $ zip xs [0..]
        srtd :: [(Char,Int)]
        srtd = concatMap (\(y,is') -> map ((,) y) (reverse is')) . assocs $ accumArray (flip (:)) [] (chr 32,chr 127) $ zip xs [0..]
        table :: Array Int (Char,Int)
        table = array (0,length xs-1) $ zipWith (\(y,i) j -> (i,(y,j))) srtd [0..]
        Just i0 = lookup eof srtd
        run :: Int -> String -> String
        run i ys
            | i == i0   = ys
            | otherwise = run j (y:ys)
            where (y,j) = table ! i