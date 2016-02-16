import Data.List

message = "012222 1114142503 0313012513 03141418192102 0113 2419182119021713 06131715070119"
keyed_alphabet = "BHISOECRTMGWYVALUZDNFJKPQX"

unkeyed_alphabet = map snd . sort $ zip keyed_alphabet ['A'..'Z']

decrypt :: [[Int]] -> String
decrypt = unwords . map (map (unkeyed_alphabet !!))

decode :: String -> [[Int]]
decode = map (map read . blocks 2) . words

blocks :: Int -> [a] -> [[a]]
blocks n = unfoldr split
    where split [] = Nothing
          split xs = Just $ splitAt n xs

main = putStrLn $ decrypt (decode message)