{-# LANGUAGE Safe #-}

module Vodozemac.Raw.Curve25519PublicKey where

import Foreign
import Foreign.C
import Prelude

type Curve25519PublicKey = Ptr ()

foreign import ccall unsafe "curve25519publickey_to_base64" to_base64 :: Curve25519PublicKey -> CString -> CSize -> IO ()
