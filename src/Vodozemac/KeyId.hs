module Vodozemac.KeyId where

import Data.ByteString (ByteString)
import Foreign
import Vodozemac.Raw.KeyId qualified as Raw (KeyId)
import Vodozemac.Raw.KeyId qualified as Raw.KeyId
import Vodozemac.Raw.Util qualified as Raw
import Prelude

newtype KeyId = KeyId Raw.KeyId

instance Storable KeyId where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = KeyId <$> peek (castPtr ptr)
    poke ptr (KeyId keyId) = poke (castPtr ptr) keyId

toBase64 :: KeyId -> IO ByteString
toBase64 (KeyId keyId) = Raw.cstringToByteString =<< Raw.KeyId.to_base64 keyId
