module Vodozemac.Raw.Olm.Message where

import Foreign.C

-- | We always serialise the messages to JSON
type Message = CString

type PreKeyMessage = CString
