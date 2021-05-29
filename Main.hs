module Main where

import Data.List

data Stack = A | B | C deriving Show
data Operation = Operation Stack Stack deriving Show
data SortStacks = SortStacks [Int] [Int] [Int] deriving Show 
data TNum = N Int | Top deriving (Show, Eq)

--start = SortStacks [12, 3, 4, 11, 2, 14, 6] [7, 8, 5] [9, 13, 1, 10]
--start = SortStacks [3, 4, 6, 1] [5] []
start = SortStacks [2, 3, 4] [1] []

main :: IO ()
main = runIteration start

runIteration :: SortStacks -> IO ()
runIteration ss = do
    putStr "Current state: "
    print ss
    if isDone ss then
        return ()
    else do
        let info = getAllowedInfo ss
        putStr "Top of stacks:  "
        print info
        let op = step info
        putStr "Operation: "
        print op
        putStrLn  ""
        let (Just next) = applyOperation (step info) ss
        runIteration next

getAllowedInfo :: SortStacks -> (TNum, TNum, TNum)
getAllowedInfo (SortStacks a b c) = (tnum a, tnum b, tnum c) where
    tnum (a:as) = N a
    tnum [] = Top

isDone :: SortStacks -> Bool 
isDone (SortStacks a b c) = sort (a ++ b ++ c) == a

applyOperation :: Operation -> SortStacks -> Maybe SortStacks
applyOperation (Operation A B) (SortStacks (a:as) bs cs) = Just $ SortStacks as (a:bs) cs
applyOperation (Operation A C) (SortStacks (a:as) bs cs) = Just $ SortStacks as bs (a:cs)
applyOperation (Operation A _) (SortStacks [] _ _) = Nothing

applyOperation (Operation B A) (SortStacks as (b:bs) cs) = Just $ SortStacks (b:as) bs cs
applyOperation (Operation B C) (SortStacks as (b:bs) cs) = Just $ SortStacks as bs (b:cs)
applyOperation (Operation B _) (SortStacks _ [] _) = Nothing

applyOperation (Operation C B) (SortStacks as bs (c:cs)) = Just $ SortStacks as (c:bs) cs
applyOperation (Operation C A) (SortStacks as bs (c:cs)) = Just $ SortStacks (c:as) bs cs
applyOperation (Operation C _) (SortStacks _ _ []) = Nothing

applyOperation _ _ = Nothing -- Operations moveing from x to x

(<:) :: TNum -> TNum -> Bool
Top <: _ = False 
_ <: Top = True 
(N a) <: (N b) = a < b

forwards = Operation C A
backwards = Operation A C
storeL = Operation A B
storeR = Operation C B
popL = Operation B A

step :: (TNum, TNum, TNum) -> Operation 
-- sad grab
step (_, Top, Top) = storeL
-- seek forward
step (l, Top , r) | r <: l = forwards
-- grab
step (l, Top , r) | l <: r = storeR
-- sad put
step (Top, x, r) = popL
-- seek backwards
step (l, x, r) | l <: x = backwards
-- sad seek backwards
step (l, x, r) | x <: l && x <: r = backwards
-- put
step (l, x, r) | x <: l && r <: x = popL