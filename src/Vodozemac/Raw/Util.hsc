{-# LANGUAGE Safe #-}

module Vodozemac.Raw.Util where

import Foreign
import Foreign.C
import Prelude

ptrSize :: Int
ptrSize = #{ size void* }

ptrAlignment :: Int
ptrAlignment = #{ alignment void* }

peekAndFreeCString :: Ptr CChar -> IO String
peekAndFreeCString ptr = peekCString ptr <* free ptr
