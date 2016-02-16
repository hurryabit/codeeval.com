module Main where

import System.Environment
import qualified Data.ByteString.Char8 as BS

main = fmap head getArgs >>= BS.readFile >>= mapM_ (print . fromEnum . process) . filter (not . BS.null) . BS.lines

process :: BS.ByteString -> Bool
process xs = let (ys,zs) = BS.break (== ',') xs
             in  (BS.tail zs) `BS.isSuffixOf` ys
