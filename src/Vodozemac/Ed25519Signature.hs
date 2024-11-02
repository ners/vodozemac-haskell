module Vodozemac.Ed25519Signature where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
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

toBase64 :: Ed25519Signature -> IO ByteString
toBase64 (Ed25519Signature sig) = Raw.cstringToByteString =<< Raw.Ed25519Signature.to_base64 sig

fromBase64 :: ByteString -> IO Ed25519Signature
fromBase64 str = Ed25519Signature <$> ByteString.unsafeUseAsCStringLen str (uncurry Raw.Ed25519Signature.from_base64)
