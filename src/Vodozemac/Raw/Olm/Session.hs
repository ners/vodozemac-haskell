module Vodozemac.Raw.Olm.Session where

import Foreign
import Foreign.C
import Vodozemac.Raw.Olm.Message qualified as Olm
import Prelude

type Session = Ptr ()

foreign import ccall unsafe "olm_session_id" id :: Session -> IO CString

foreign import ccall unsafe "olm_session_has_received_message" has_received_message :: Session -> IO Bool

foreign import ccall unsafe "olm_session_encrypt_to_json" encrypt :: Session -> Ptr CChar -> Int -> IO Olm.Message

foreign import ccall unsafe "olm_session_decrypt_json" decrypt :: Session -> CString -> Int -> Ptr Int -> IO (Ptr Word8)
