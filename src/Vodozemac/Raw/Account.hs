{-# LANGUAGE Safe #-}

module Vodozemac.Raw.Account where

import Foreign
import Foreign.C
import Vodozemac.Raw.Curve25519PublicKey (Curve25519PublicKey)
import Vodozemac.Raw.Ed25519Signature (Ed25519Signature)
import Vodozemac.Raw.Ed25519PublicKey (Ed25519PublicKey)
import Vodozemac.Raw.FallbackKey (FallbackKey)
import Prelude

type Account = Ptr ()

foreign import ccall unsafe "new_account" new :: IO Account

foreign import ccall unsafe "free_account" free :: Account -> IO ()

foreign import ccall unsafe "mark_keys_as_published" mark_keys_as_published :: Account -> IO ()

foreign import ccall unsafe "generate_fallback_key" generate_fallback_key :: Account -> IO Curve25519PublicKey

foreign import ccall unsafe "fallback_key" fallback_key :: Account -> IO FallbackKey

foreign import ccall unsafe "sign" sign :: Account -> Ptr CChar -> Int -> IO Ed25519Signature

foreign import ccall unsafe "ed25519_key" ed25519_key :: Account -> IO Ed25519PublicKey

foreign import ccall unsafe "curve25519_key" curve25519_key :: Account -> IO Curve25519PublicKey
