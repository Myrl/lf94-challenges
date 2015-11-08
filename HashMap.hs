{-# LANGUAGE OverloadedStrings #-}
module HashMap where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.String (IsString)
import qualified Data.ByteString as B

type HashMap k a = IntMap.IntMap (Map.Map k a)

class Hashable a where
    hash :: a -> Int

instance Hashable B.ByteString where
    hash = B.foldl (\a x -> a + (fromIntegral x)*33 + c) 0
        where c = 5381 :: Int

fromMaybe :: Monoid m => Maybe m -> m
fromMaybe = maybe mempty id

toMaybe :: (Ord m, Monoid m) => m -> Maybe m
toMaybe xs
    | mempty == xs = Nothing
    | otherwise = Just xs

(!) :: (Hashable s, Ord s, IsString s) => HashMap s a -> s -> a
xs ! k = (xs IntMap.! (hash k)) Map.! k

insert :: (Hashable s, Ord s, IsString s, Ord a) => s -> a -> HashMap s a -> HashMap s a
insert k x xs = IntMap.alter (toMaybe . Map.insert k x . fromMaybe) (hash k) xs

delete :: (Hashable s, Ord s, IsString s, Ord a) => s -> HashMap s a -> HashMap s a
delete k xs = IntMap.update (toMaybe . Map.delete k) (hash k) xs
