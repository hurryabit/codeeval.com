import System.Environment (getArgs)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ (putStrLn . process) $ lines input

process str =
    let (cs,'|':is) = break ('|' ==) str
    in  map ((cs !!) . pred . read) (words is)