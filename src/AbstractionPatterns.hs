module AbstractionPatterns where

import Data.Map (Map)
import qualified Data.Map as M

newtype Address = MkAddress Int
    deriving (Eq, Ord, Show)

-- type Map key val = [(key, val)]

addressMapping :: Map Address Address
addressMapping = M.fromList 
    [(MkAddress 3, MkAddress 7),
    (MkAddress 4, MkAddress 20),
    (MkAddress 5, MkAddress 3),
    (MkAddress 7, MkAddress 14),
    (MkAddress 9, MkAddress 5),
    (MkAddress 16, MkAddress 9)]

threeHopsOriginal :: Address -> Maybe String
threeHopsOriginal address0 = 
    case M.lookup address0 addressMapping of
        Nothing -> Nothing
        Just address1 -> 
            case M.lookup address1 addressMapping of
                Nothing -> Nothing
                Just address2 -> 
                    case M.lookup address2 addressMapping of
                        Nothing -> Nothing
                        Just address3 -> Just (show address3)

threeHops :: Address -> Maybe String
threeHops address0 = 
    M.lookup address0 addressMapping `bindMaybe` \ address1 -> 
    M.lookup address1 addressMapping `bindMaybe` \ address2 -> 
    M.lookup address2 addressMapping `bindMaybe` \ address3 -> 
    returnMaybe (show address3)

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- return :: a -> IO a

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe computation continuation = 
    case computation of 
        Nothing -> Nothing
        Just result -> continuation result

returnMaybe :: a -> Maybe a
returnMaybe x = Just x

data Tree a = Node (Tree a) a (Tree a) | Leaf deriving Show

buildTree :: a -> Int -> Tree a
buildTree x h =
    if h <= 0
        then Leaf
        else
            let
                subTree = buildTree x (h - 1)
            in 
                Node subTree x subTree

labelTree :: Tree a -> Tree (Int, a)
labelTree tree = fst (labelTree' tree 1)

type WithCounter a = Int -> (a, Int)

labelTree' :: Tree a -> WithCounter (Tree (Int, a))
labelTree' Leaf currentLabel = (Leaf, currentLabel)
labelTree' (Node l x r) currentLabel = 
    case labelTree' l currentLabel of
        (l', currentLabel') ->
            let
                labelForX = currentLabel'
                nextLabel = currentLabel' + 1
            in
                case labelTree' r nextLabel of
                    (r', currentLabel'') -> 
                        (Node l' (labelForX, x) r', currentLabel'')
