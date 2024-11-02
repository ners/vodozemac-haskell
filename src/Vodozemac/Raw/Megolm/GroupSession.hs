module Vodozemac.Raw.Megolm.GroupSession where

import Foreign
import Foreign.C
import Prelude
import Vodozemac.Raw.Megolm.SessionKey (SessionKey)

type GroupSession = Ptr ()

foreign import ccall unsafe "megolm_new_group_session" new_session :: IO GroupSession

foreign import ccall unsafe "megolm_group_session_key" key :: GroupSession -> IO SessionKey

foreign import ccall unsafe "megolm_group_session_id" id :: GroupSession -> IO CString

foreign import ccall unsafe "megolm_group_session_encrypt_to_b64" encrypt :: GroupSession -> Ptr CChar -> Int -> IO CString
