module Vodozemac.Curve25519PublicKey where

import Foreign
import Vodozemac.Raw.Curve25519PublicKey qualified as Raw (Curve25519PublicKey)
import Vodozemac.Raw.Curve25519PublicKey qualified as Raw.Curve25519PublicKey
import Vodozemac.Raw.Util qualified as Raw
import Prelude

newtype Curve25519PublicKey = Curve25519PublicKey Raw.Curve25519PublicKey
    deriving stock (Eq, Show)

instance Storable Curve25519PublicKey where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = Curve25519PublicKey <$> peek (castPtr ptr)
    poke ptr (Curve25519PublicKey key) = poke (castPtr ptr) key

toBase64 :: Curve25519PublicKey -> IO String
toBase64 (Curve25519PublicKey key) = Raw.peekAndFreeCString =<< Raw.Curve25519PublicKey.to_base64 key
