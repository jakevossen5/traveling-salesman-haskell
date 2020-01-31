module Main where
import Data.List

routes_10 :: [(Float, Float)]
routes_10 = [(22,8), (22, 46), (6, 43), (44, 32), (19, 6), (46, 3), (18, 12), (25, 8),(29, 2), (32, 3)]

-- distance :: (Integral a, Floating b) => (a,a) -> (a,a) -> b
-- distance p1 p2 = sqrt ((fst (p2) - fst(p1))^^2 + ((snd p2) - (snd p1))^^2)

distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2


-- Can probably implement this with a fold, not sure how though
traverse :: (Floating b) => [(b,b)] -> b
traverse (p1:p2:[]) = (distance p1 p2)
traverse (p1:p2:ps) = (distance p1 p2) + Main.traverse (p2:ps)

-- Can change graph easily to a different set of input nodes
graph :: [(Float, Float)]
graph = routes_10

all_perms = permutations (tail graph)

traverse_1 = Main.traverse route_1
  where
    route_1 = (graph !! 0):(head all_perms) ++ [(graph !! 0)]

traverse_all = map (\x -> (Main.traverse (add_ends x (graph !! 0)), x)) all_perms

add_ends xs end = end:xs ++ [end]

shortest = minimum traverse_all


main :: IO ()
main = putStrLn (show shortest)
