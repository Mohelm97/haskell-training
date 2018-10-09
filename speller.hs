speller :: [String] -> String
speller xs = foldl (\acc x -> acc ++ getText (fst x) (snd x) (length xs)) "" (zip xs [1..length xs])

getText :: String -> Int -> Int -> String
getText (x:xs) i l = x : " is for " ++ (x:xs) ++ appendText i l

appendText :: Int -> Int -> String
appendText i l 
    | i == l || l == 1 = ""
    | (i+1) == l = ", and " 
    | otherwise =  ", "

main = do
    print (speller ["abacus"])
    print (speller [])
    print (speller ["apple", "banana", "coconut"])
    print (speller ["whisky", "x-ray"]) 
