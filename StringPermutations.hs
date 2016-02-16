import Control.Arrow (second)
import System.Environment (getArgs)

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
	-- print your output to stdout
	-- mapM_ putStrLn $ lines input
    return ()

permutations :: [a] -> [[a]]
permutations xs = concatMap (\(y,ys) -> map (y:) (permutations ys)) 

select :: [a] -> [(a,[a])]
select []     = []
select (x:xs) = (x,xs):map (second (x:)) (select xs)