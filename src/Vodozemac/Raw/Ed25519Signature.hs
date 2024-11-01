{-# LANGUAGE Safe #-}

module Vodozemac.Raw.Ed25519Signature where

import Foreign
import Foreign.C
import Prelude

type Ed25519Signature = Ptr ()

foreign import ccall unsafe "ed25519signature_to_base64" to_base64 :: Ed25519Signature -> CString -> CSize -> IO ()
