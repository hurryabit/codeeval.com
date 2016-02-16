import Data.List
import System.Environment (getArgs)

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    mapM_ (putStrLn . maybe (error "does not exists") (:[]) . nonRep1) $ lines input

nonRep1 :: Eq a => [a] -> Maybe a
nonRep1 []     = Nothing
nonRep1 (x:xs) = case partition (x ==) xs of
    ([],_  ) -> Just x
    (_ ,xs') -> nonRep1 xs'