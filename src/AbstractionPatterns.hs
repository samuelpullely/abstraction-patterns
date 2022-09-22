module AbstractionPatterns where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
-- import Prelude hiding (Monad(..), Applicative(..), Functor(..))

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

data ErrorMessage = AddressLookupFailed Address deriving Show

threeHops' :: Address -> Either ErrorMessage String
threeHops' address0 = 
    do
        address1 <- lookup' address0 addressMapping
        address2 <- lookup' address1 addressMapping
        address3 <- lookup' address2 addressMapping
        return (show address3)

lookup' :: Address -> Map Address a -> Either ErrorMessage a
lookup' address mapping = 
    case M.lookup address mapping of
        Nothing -> Left (AddressLookupFailed address)
        Just value -> Right value

threeHops :: Address -> Maybe String
threeHops address0 = 
    do
        address1 <- M.lookup address0 addressMapping
        address2 <- M.lookup address1 addressMapping
        address3 <- M.lookup address2 addressMapping
        return (show address3)
    -- M.lookup address0 addressMapping >>= \ address1 -> 
    -- M.lookup address1 addressMapping >>= \ address2 -> 
    -- M.lookup address2 addressMapping >>= \ address3 -> 
    -- return (show address3)

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
    do
        l'        <- labelTree' l
        labelForX <- tick
        r'        <- labelTree' r
        return (Node l' (labelForX, x) r')
    -- labelTree' l >>= \ l' -> 
    -- tick         >>= \ labelForX ->
    -- labelTree' r >>= \ r' ->
    -- return (Node l' (labelForX, x) r')

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


-- class Applicative m => Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b

-- instance Monad Maybe where
--     return = returnMaybe
--     (>>=) = bindMaybe

-- instance Applicative Maybe where
--     pure = return
--     (<*>) = ap

-- instance Functor Maybe where
--     fmap = liftM

instance Monad WithCounter where
    return = returnWithCounter
    (>>=) = bindWithCounter

instance Applicative WithCounter where
    pure = return
    (<*>) = ap

instance Functor WithCounter where
    fmap = liftM

-- liftM :: Monad m => (a -> b) -> m a -> m b
-- liftM f computation = 
--     computation >>= \ a -> return (f a)

-- liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- liftM2 f computationa computationb = 
--     computationa >>= \ a ->
--     computationb >>= \ b -> 
--     return (f a b)

-- liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
-- liftM3 f computationa computationb computationc = 
--     computationa >>= \ a ->
--     computationb >>= \ b -> 
--     computationc >>= \ c -> 
--     return (f a b c)

-- -- ($) :: (a -> b) -> a -> b
-- -- ($) f x = f x

-- ap :: Monad m => m (a -> b) -> m a -> m b
-- ap computationf computationa =
--     computationf >>= \ f ->
--     computationa >>= \ a ->
--     return (f a)

-- liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- liftM2' f computationa computationb = 
--     return f `ap` computationa `ap` computationb

-- liftM5' :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m b
-- liftM5' f c1 c2 c3 c4 c5 =
--     return f `ap` c1 `ap` c2 `ap` c3 `ap` c4 `ap` c5

-- class Functor f => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b
