{-# LANGUAGE Safe #-}

module Vodozemac.Raw.KeyId where

import Foreign
import Foreign.C
import Prelude

type KeyId = Ptr ()

foreign import ccall unsafe "keyid_to_base64" to_base64 :: KeyId -> CString -> CSize -> IO ()
