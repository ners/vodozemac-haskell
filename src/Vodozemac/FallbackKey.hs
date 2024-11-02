module Vodozemac.FallbackKey where

import Foreign
import Vodozemac.Curve25519PublicKey (Curve25519PublicKey)
import Vodozemac.KeyId (KeyId)
import Vodozemac.Raw.Util qualified as Raw
import Prelude

data FallbackKey = FallbackKey KeyId Curve25519PublicKey

instance Storable FallbackKey where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = FallbackKey <$> peek (castPtr ptr) <*> peek (castPtr ptr `plusPtr` Raw.ptrSize)
    poke ptr (FallbackKey keyId key) = do
        poke (castPtr ptr) keyId
        poke (castPtr $ ptr `plusPtr` Raw.ptrSize) key
