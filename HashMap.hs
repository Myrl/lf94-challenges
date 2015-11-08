{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module HashMap where

import qualified Data.ByteString as B
import Data.Monoid.Null

type Bucket k a = [(k, a)]
type HashMap k a = [(Int, Bucket k a)]

class Eq a => Hashable a where
    hash :: a -> Int

instance Hashable B.ByteString where
    hash = B.foldl (\a x -> a + (fromIntegral x)*33 + c) 0
        where c = 5381 :: Int

fromMaybe :: Monoid m => Maybe m -> m
fromMaybe = maybe mempty id

toMaybe :: MonoidNull m => m -> Maybe m
toMaybe xs
    | Data.Monoid.Null.null xs = Nothing
    | otherwise = Just xs

(!) :: (Hashable s) => HashMap s a -> s -> a
xs ! k = get (get xs (hash k)) k

insert :: (Hashable s) => s -> a -> HashMap s a -> HashMap s a
insert k x = update' (insert' k x . fromMaybe) (hash k)

delete :: (Hashable s) => s -> HashMap s a -> HashMap s a
delete k xs = update (toMaybe . delete' k) (hash k) xs

get :: Eq k => [(k, a)] -> k -> a
get xs k = snd . head . dropWhile (\(k', _) -> k' /= k) $ xs

update' :: Eq k => (Maybe a -> a) -> k -> [(k, a)] -> [(k, a)]
update' f k [] = [(k, f Nothing)]
update' f k ((k', x):xs)
    | k == k' = (k', f (Just x)):xs
    | otherwise = (k', x):update' f k xs

update :: Eq k => (a -> Maybe a) -> k -> [(k, a)] -> [(k, a)]
update _ _ [] = []
update f k ((k', x):xs)
    | k == k' = case f x of
                  Just x' -> (k', x'):xs
                  Nothing -> xs
    | otherwise = (k', x):update f k xs

alter :: Eq k => (Maybe a -> Maybe a) -> k -> [(k, a)] -> [(k, a)]
alter f k [] = maybe [] (\x' -> [(k, x')]) $ f Nothing
alter f k ((k', x):xs)
    | k == k' = maybe xs (\x' -> (k, x'):xs) $ f $ Just x
    | otherwise = (k', x):alter f k xs

insert' :: Eq k => k -> a -> [(k, a)] -> [(k, a)]
insert' k x xs = alter (\case 
                         Nothing -> Just x
                         Just x' -> Just x') k xs

delete' :: Eq k => k -> [(k, a)] -> [(k, a)]
delete' = alter (const Nothing)
