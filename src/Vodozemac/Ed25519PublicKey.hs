module Vodozemac.Ed25519PublicKey where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
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

toBase64 :: Ed25519PublicKey -> IO ByteString
toBase64 (Ed25519PublicKey key) = Raw.cstringToByteString =<< Raw.Ed25519PublicKey.to_base64 key

fromBase64 :: ByteString -> IO Ed25519PublicKey
fromBase64 str = Ed25519PublicKey <$> ByteString.unsafeUseAsCStringLen str (uncurry Raw.Ed25519PublicKey.from_base64)
