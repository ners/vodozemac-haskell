{-# LANGUAGE Safe #-}

module Vodozemac.Raw.Ed25519PublicKey where

import Foreign
import Foreign.C
import Prelude

type Ed25519PublicKey = Ptr ()

foreign import ccall unsafe "ed25519publickey_to_base64" to_base64 :: Ed25519PublicKey -> CString -> CSize -> IO ()
