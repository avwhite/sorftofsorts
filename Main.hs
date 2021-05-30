module Main where

import Data.List

data Stack = A | B | C deriving Show
data Operation = Operation Stack Stack deriving Show
data SortStacks = SortStacks [Int] [Int] [Int] deriving Show 
data TNum = N Int | Top deriving (Show, Eq)

instance Ord TNum where
    Top <= Top = True 
    x <= Top = True
    Top <= x = False
    N x <= N y = x <= y 

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

forwards = Operation C A
backwards = Operation A C
storeL = Operation A B
storeR = Operation C B
popL = Operation B A

step :: (TNum, TNum, TNum) -> Operation 
-- Seek forward and grab out of place element
step (l, Top, Top)       = storeL   --Grab (from left) if we are at end instead
step (l, Top, r  ) | r < l = forwards --Go forwards when elements are en correct order
step (l, Top, r  ) | l < r = storeR   --Grab from right when they are not
-- Seek backwards and place element in correct place
step (l,   x, r) | l < x          = backwards --Seek backwards while l < x
step (Top, x, r)                  = popL      --Put if we are at end no matter what
step (l,   x, r) | x < l && x < r = backwards --The tricky case, we go back further instead of putting the element in its correct place
step (l,   x, r) | x < l && r < x = popL      --Put the element in its place