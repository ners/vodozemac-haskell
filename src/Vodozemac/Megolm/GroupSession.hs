module Vodozemac.Megolm.GroupSession where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Foreign
import Vodozemac.Raw.Megolm.GroupSession qualified as Raw.Megolm (GroupSession)
import Vodozemac.Raw.Megolm.GroupSession qualified as Raw.Megolm.GroupSession
import Vodozemac.Raw.Util qualified as Raw
import Prelude
import Vodozemac.Megolm.SessionKey (SessionKey (SessionKey))

newtype GroupSession = GroupSession Raw.Megolm.GroupSession

instance Storable GroupSession where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = GroupSession <$> peek (castPtr ptr)
    poke ptr (GroupSession sess) = poke (castPtr ptr) sess

new :: IO GroupSession
new = GroupSession <$> Raw.Megolm.GroupSession.new_session

key :: GroupSession -> IO SessionKey
key (GroupSession sess) = SessionKey <$> Raw.Megolm.GroupSession.key sess

id :: GroupSession -> IO ByteString
id (GroupSession sess) = Raw.cstringToByteString =<< Raw.Megolm.GroupSession.id sess

encrypt :: GroupSession -> ByteString -> IO ByteString
encrypt (GroupSession sess) bs = Raw.cstringToByteString =<< ByteString.unsafeUseAsCStringLen bs (uncurry $ Raw.Megolm.GroupSession.encrypt sess)

