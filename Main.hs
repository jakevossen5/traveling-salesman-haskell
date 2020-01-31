module Main where

distance :: Floating a => (a,a) -> (a,a) -> a
distance p1 p2 = sqrt ((fst (p2) - fst(p1))^2 + ((snd p2) - (snd p1)^2))

routes_10 :: [(Int, Int)]
routes_10 = [(22,8), (22, 46), (6, 43), (44, 32), (19, 6), (46, 3), (18, 12), (25, 8),(29, 2), (32, 3)]



main :: IO ()
main = putStrLn "Hello, Haskell!"
