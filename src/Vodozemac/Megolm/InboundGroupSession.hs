module Vodozemac.Megolm.InboundGroupSession where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Foreign
import Vodozemac.Megolm.SessionKey (SessionKey (..))
import Vodozemac.Raw.Megolm.InboundGroupSession qualified as Raw.Megolm (InboundGroupSession)
import Vodozemac.Raw.Megolm.InboundGroupSession qualified as Raw.Megolm.InboundGroupSession
import Vodozemac.Raw.Util qualified as Raw
import Prelude

newtype InboundGroupSession = InboundGroupSession Raw.Megolm.InboundGroupSession

instance Storable InboundGroupSession where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = InboundGroupSession <$> peek (castPtr ptr)
    poke ptr (InboundGroupSession sess) = poke (castPtr ptr) sess

new :: SessionKey -> IO InboundGroupSession
new (SessionKey key) = InboundGroupSession <$> Raw.Megolm.InboundGroupSession.new_session key

id :: InboundGroupSession -> IO ByteString
id (InboundGroupSession sess) = Raw.cstringToByteString =<< Raw.Megolm.InboundGroupSession.id sess

decrypt :: InboundGroupSession -> ByteString -> IO (Maybe (ByteString, Word32))
decrypt (InboundGroupSession sess) message = do -- Raw.Megolm.InboundGroupSession.decrypt
    alloca $ \messageIndexPtr ->
        alloca $ \plaintextSizePtr -> do
            plaintextPtr <-
                ByteString.unsafeUseAsCStringLen
                    message
                    ( \(input, inputLen) ->
                        Raw.Megolm.InboundGroupSession.decrypt sess input inputLen messageIndexPtr plaintextSizePtr
                    )
            if plaintextPtr == nullPtr
                then pure Nothing
                else do
                    plaintextSize <- peek plaintextSizePtr
                    messageIndex <- peek messageIndexPtr
                    Just . (, messageIndex) <$> ByteString.unsafePackCStringFinalizer plaintextPtr plaintextSize (Raw.free_bytestring plaintextPtr plaintextSize)
