module Vodozemac.Raw.Megolm.SessionKey where

import Foreign
import Foreign.C
import Prelude

type SessionKey = Ptr ()

foreign import ccall unsafe "megolm_session_key_to_base64" to_base64 :: SessionKey -> IO CString

foreign import ccall unsafe "megolm_session_key_from_base64" from_base64 :: CString -> Int -> IO SessionKey
