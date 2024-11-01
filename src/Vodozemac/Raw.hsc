{-# LANGUAGE Safe #-}

module Vodozemac.Raw where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr (Ptr)
import Prelude

ptrSize :: Int
ptrSize = #{ size void* }

ptrAlignment :: Int
ptrAlignment = #{ alignment void* }

type Account = Ptr ()

foreign import ccall unsafe "rust_hello" rust_hello :: CInt -> IO CInt

foreign import ccall unsafe "new_account" new_account :: IO Account

foreign import ccall unsafe "free_account" free_account :: Account -> IO ()

type KeyId = Ptr ()

type Curve25519PublicKey = Ptr ()

foreign import ccall unsafe "mark_keys_as_published" mark_keys_as_published :: Account -> IO ()

foreign import ccall unsafe "generate_fallback_key" generate_fallback_key :: Account -> IO Curve25519PublicKey

type FallbackKey = Ptr ()

foreign import ccall unsafe "fallback_key" fallback_key :: Account -> IO FallbackKey

foreign import ccall unsafe "keyid_to_base64" keyid_to_base64 :: KeyId -> CString -> CSize -> IO ()
