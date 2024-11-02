module Vodozemac.Megolm.SessionKey where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Foreign
import Vodozemac.Raw.Megolm.SessionKey qualified as Raw.Megolm (SessionKey)
import Vodozemac.Raw.Megolm.SessionKey qualified as Raw.Megolm.SessionKey
import Vodozemac.Raw.Util qualified as Raw
import Prelude

newtype SessionKey = SessionKey Raw.Megolm.SessionKey
    deriving stock (Eq, Show)

instance Storable SessionKey where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = SessionKey <$> peek (castPtr ptr)
    poke ptr (SessionKey key) = poke (castPtr ptr) key

toBase64 :: SessionKey -> IO ByteString
toBase64 (SessionKey key) = Raw.cstringToByteString =<< Raw.Megolm.SessionKey.to_base64 key

fromBase64 :: ByteString -> IO SessionKey
fromBase64 str = SessionKey <$> ByteString.unsafeUseAsCStringLen str (uncurry Raw.Megolm.SessionKey.from_base64)
