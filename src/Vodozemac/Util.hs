module Vodozemac.Util (module Vodozemac.Util, module Data.Functor) where

import Data.Functor
import Data.ByteString (ByteString)
import Prelude

infixl 4 <$$>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

infixl 1 <&&>

(<&&>) :: (Functor f1) => (Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<&&>) = flip (<$$>)

class ToBase64 a where
    toBase64 :: a -> ByteString

class FromBase64 a where
    fromBase64 :: ByteString -> Maybe a
