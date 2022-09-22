module AbstractionPatterns where

import Data.Map (Map)
import qualified Data.Map as M
import Prelude hiding (Monad(..))

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
    M.lookup address0 addressMapping >>= \ address1 -> 
    M.lookup address1 addressMapping >>= \ address2 -> 
    M.lookup address2 addressMapping >>= \ address3 -> 
    return (show address3)

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
labelTree tree = fst (runWithCounter (labelTree' tree) 1)

newtype WithCounter a = MkWithCounter {runWithCounter :: Int -> (a, Int)}

-- MkWithCounter :: (Int -> (a, Int)) -> WithCounter a
-- runWithCounter :: WithCounter a -> (Int -> (a, Int))

labelTree'Orig :: Tree a -> WithCounter (Tree (Int, a))
labelTree'Orig Leaf = MkWithCounter (\ currentLabel -> (Leaf, currentLabel))
labelTree'Orig (Node l x r) = 
    MkWithCounter (\ currentLabel ->
        case runWithCounter (labelTree'Orig l) currentLabel of
            (l', currentLabel') ->
                case runWithCounter tick currentLabel' of
                    (labelForX, nextLabel) ->
                        case runWithCounter (labelTree'Orig r) nextLabel of
                            (r', currentLabel'') -> 
                                (Node l' (labelForX, x) r', currentLabel'')
    )

labelTree' :: Tree a -> WithCounter (Tree (Int, a))
labelTree' Leaf = return Leaf
labelTree' (Node l x r) = 
    labelTree' l >>= \ l' -> 
    tick         >>= \ labelForX ->
    labelTree' r >>= \ r' ->
    return (Node l' (labelForX, x) r')

tick :: WithCounter Int
tick = MkWithCounter (\ current -> (current, current + 1))

bindWithCounter :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
bindWithCounter computation continuation =
    MkWithCounter (\ currentCounter ->
        case runWithCounter computation currentCounter of
            (result, currentCounter') -> runWithCounter (continuation result) currentCounter'
    )

returnWithCounter :: a -> WithCounter a
returnWithCounter x =
    MkWithCounter (\ currentCounter -> (x, currentCounter))


class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
    return = returnMaybe
    (>>=) = bindMaybe

instance Monad WithCounter where
    return = returnWithCounter
    (>>=) = bindWithCounter

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f computation = 
    computation >>= \ a -> return (f a)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f computationa computationb = 
    computationa >>= \ a ->
    computationb >>= \ b -> 
    return (f a b)

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f computationa computationb computationc = 
    computationa >>= \ a ->
    computationb >>= \ b -> 
    computationc >>= \ c -> 
    return (f a b c)

-- ($) :: (a -> b) -> a -> b
-- ($) f x = f x

ap :: Monad m => m (a -> b) -> m a -> m b
ap computationf computationa =
    computationf >>= \ f ->
    computationa >>= \ a ->
    return (f a)
