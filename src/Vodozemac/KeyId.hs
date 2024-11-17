{-# LANGUAGE InstanceSigs #-}

module Vodozemac.KeyId where

import Data.ByteString (ByteString)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import System.IO.Unsafe (unsafePerformIO)
import Vodozemac.Util
import Prelude

data KeyId'

newtype KeyId = KeyId (ForeignPtr KeyId')

setCrateModule

[rust|
use vodozemac::*;
|]

extendContext basic
extendContext ffi
extendContext functions
extendContext pointers
extendContext prelude
extendContext (singleton [ty| KeyId |] [t|KeyId'|])

[rust|
extern "C" fn free (ptr: *mut KeyId) {
    let key = unsafe { Box::from_raw(ptr) };
    drop(key)
}
|]

free :: FunPtr (Ptr KeyId' -> IO ())
free = [rust| extern fn (*mut KeyId) { free }|]

wrap :: Ptr KeyId' -> KeyId
wrap = unsafePerformIO . fmap KeyId . newForeignPtr Vodozemac.KeyId.free

instance ToBase64 KeyId where
    toBase64 :: KeyId -> ByteString
    toBase64 (KeyId ptr) = unsafePerformIO . withForeignPtr ptr $ \key ->
        getByteString
            [rust| (*mut u8, usize) {
                let key = unsafe { &*$(key: *const KeyId) };
                crate::copy_str(key.to_base64())
            }|]
