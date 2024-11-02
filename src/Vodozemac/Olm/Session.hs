module Vodozemac.Olm.Session where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Unsafe qualified as ByteStrign
import Data.ByteString.Unsafe qualified as ByteString
import Data.Maybe (fromJust)
import Foreign
import Vodozemac.Olm.Message qualified as Olm
import Vodozemac.Raw.Olm.Session qualified as Raw.Olm (Session)
import Vodozemac.Raw.Olm.Session qualified as Raw.Olm.Session
import Vodozemac.Raw.Util qualified as Raw
import Prelude

newtype Session = Session Raw.Olm.Session

instance Storable Session where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = Session <$> peek (castPtr ptr)
    poke ptr (Session key) = poke (castPtr ptr) key

id :: Session -> IO ByteString
id (Session sess) = Raw.cstringToByteString =<< Raw.Olm.Session.id sess

hasReceivedMessage :: Session -> IO Bool
hasReceivedMessage (Session sess) = Raw.Olm.Session.has_received_message sess

encrypt :: Session -> ByteString -> IO Olm.Message
encrypt (Session sess) bs = do
    json <- Raw.cstringToByteString =<< ByteString.unsafeUseAsCStringLen bs (uncurry $ Raw.Olm.Session.encrypt sess)
    pure . fromJust $ Aeson.decodeStrict json

decrypt :: Session -> Olm.Message -> IO (Maybe ByteString)
decrypt (Session sess) message = do
    let json = LazyByteString.toStrict $ Aeson.encode message
    alloca $ \size -> do
        ptr <- ByteString.unsafeUseAsCStringLen json (\(ptr, len) -> Raw.Olm.Session.decrypt sess ptr len size)
        if ptr == nullPtr
            then pure Nothing
            else do
                size' <- peek size
                Just <$> ByteStrign.unsafePackCStringFinalizer ptr size' (Raw.free_bytestring ptr size')
