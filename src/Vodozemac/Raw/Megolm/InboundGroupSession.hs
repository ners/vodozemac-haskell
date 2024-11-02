module Vodozemac.Raw.Megolm.InboundGroupSession where

import Foreign
import Foreign.C
import Vodozemac.Raw.Megolm.SessionKey (SessionKey)
import Prelude

type InboundGroupSession = Ptr ()

foreign import ccall unsafe "megolm_new_inbound_group_session" new_session :: SessionKey -> IO InboundGroupSession

foreign import ccall unsafe "megolm_inbound_group_session_id" id :: InboundGroupSession -> IO CString

foreign import ccall unsafe "megolm_inbound_group_session_decrypt_b64" decrypt :: InboundGroupSession -> Ptr CChar -> Int -> Ptr Word32 -> Ptr Int -> IO (Ptr Word8)
