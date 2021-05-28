module Main where

import Data.List


start = SortStacks [12, 3, 4, 11, 2, 14, 6] [7, 8, 5] [9, 13, 1, 10]
--start = SortStacks [3, 4, 6, 1] [5] []

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

data Stack = A | B | C deriving Show
data Operation = Operation Stack Stack deriving Show
data SortStacks = SortStacks [Int] [Int] [Int] deriving Show 
data ONum = N Int | Top | Bot deriving (Show, Eq)

isDone :: SortStacks -> Bool 
isDone (SortStacks a b c) = sort (a ++ b ++ c) == a

isNum :: ONum -> Bool
isNum Top = False
isNum Bot = False
isNum (N _) = True

getStack :: Stack -> SortStacks -> [Int]
getStack A (SortStacks a b c) = a
getStack B (SortStacks a b c) = b
getStack C (SortStacks a b c) = c

forwards = Operation C A
backwards = Operation A C
storeL = Operation A B
storeR = Operation C B
popL = Operation B A

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

(<:) :: ONum -> ONum -> Bool
Top <: _ = False 
_ <: Bot = False 
Bot <: _ = True
_ <: Top = True 
(N a) <: (N b) = a < b


step :: (ONum, Maybe Int, ONum) -> Operation 
-- sad grab
step (_, Nothing, Bot) = storeL
-- seek forward
step (l, Nothing , r) | r <: l = forwards
-- grab
step (l, Nothing , r) | l <: r = storeR
-- sad put
step (Top, Just x, r) = popL
-- seek backwards
step (l, Just x, r) | l <: N x = backwards
-- sad seek backwards
step (l, Just x, r) | N x <: l && (N x <: r || r == Bot) = backwards
-- put
step (l, Just x, r) | N x <: l && r <: N x = popL

shead :: [a] -> Maybe a
shead (a:as) = Just a
shead [] = Nothing

getAllowedInfo :: SortStacks -> (ONum, Maybe Int, ONum)
getAllowedInfo (SortStacks a b c) = (tnum $ shead a, shead b, bnum $ shead c) where
    tnum Nothing  = Top
    tnum (Just x) = N x
    bnum Nothing  = Bot
    bnum (Just x) = N x

