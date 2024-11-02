module Vodozemac.Ed25519Signature where

import Foreign
import Vodozemac.Raw.Ed25519Signature qualified as Raw (Ed25519Signature)
import Vodozemac.Raw.Ed25519Signature qualified as Raw.Ed25519Signature
import Vodozemac.Raw.Util qualified as Raw
import Prelude

newtype Ed25519Signature = Ed25519Signature Raw.Ed25519Signature

instance Storable Ed25519Signature where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = Ed25519Signature <$> peek (castPtr ptr)
    poke ptr (Ed25519Signature key) = poke (castPtr ptr) key

toBase64 :: Ed25519Signature -> IO String
toBase64 (Ed25519Signature sig) = Raw.peekAndFreeCString =<< Raw.Ed25519Signature.to_base64 sig
