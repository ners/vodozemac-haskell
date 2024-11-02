module Vodozemac.Ed25519PublicKey where

import Foreign
import Vodozemac.Raw.Ed25519PublicKey qualified as Raw (Ed25519PublicKey)
import Vodozemac.Raw.Ed25519PublicKey qualified as Raw.Ed25519PublicKey
import Vodozemac.Raw.Util qualified as Raw
import Prelude

newtype Ed25519PublicKey = Ed25519PublicKey Raw.Ed25519PublicKey

instance Storable Ed25519PublicKey where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = Ed25519PublicKey <$> peek (castPtr ptr)
    poke ptr (Ed25519PublicKey key) = poke (castPtr ptr) key

toBase64 :: Ed25519PublicKey -> IO String
toBase64 (Ed25519PublicKey key) = Raw.peekAndFreeCString =<< Raw.Ed25519PublicKey.to_base64 key
