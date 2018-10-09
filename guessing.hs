starman word n = turn word ['-' | x<-word] n

turn word display n = 
    do
        if n==0 then
            putStrLn "Loser"
        else 
            if word==display then
                putStrLn "Winner"
            else
                mkguess word display n

mkguess word display n =
    do
        putStrLn (display ++ " " ++ ['*' | x <- [1..n]])
        putStr "Enter your guess: "
        q <- getLine
        let c = q!!0
        let display' = [if x==c then c else y | (x,y) <- zip word display]
        let n' = if display'==display then n-1 else n
        turn word display' n'