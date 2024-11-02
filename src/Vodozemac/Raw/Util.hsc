module Vodozemac.Raw.Util where

import Foreign
import Foreign.C
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Prelude

ptrSize :: Int
ptrSize = #{ size void* }

ptrAlignment :: Int
ptrAlignment = #{ alignment void* }

foreign import ccall unsafe "free_cstring" free_cstring :: CString -> IO ()

foreign import ccall unsafe "free_bytestring" free_bytestring :: Ptr Word8 -> Int -> IO ()

peekAndFreeCString :: CString -> IO String
peekAndFreeCString ptr = peekCString ptr <* free_cstring ptr

cstringToByteString :: CString -> IO ByteString
cstringToByteString cstr = do
    len <- lengthArray0 0 cstr
    ByteString.unsafePackCStringFinalizer (castPtr cstr) len (free_cstring cstr)
