{-# LANGUAGE InstanceSigs #-}

module Vodozemac.Megolm.SessionKey where

import Data.ByteString (ByteString)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import System.IO.Unsafe (unsafePerformIO)
import Vodozemac.Util
import Prelude

data SessionKey'

newtype SessionKey = SessionKey (ForeignPtr SessionKey')

setCrateModule

[rust|
use vodozemac::*;
use vodozemac::megolm::*;
|]

extendContext basic
extendContext ffi
extendContext functions
extendContext pointers
extendContext prelude
extendContext (singleton [ty| SessionKey |] [t|SessionKey'|])

[rust|
extern "C" fn free (ptr: *mut SessionKey) {
    let key = unsafe { Box::from_raw(ptr) };
    drop(key)
}
|]

free :: FunPtr (Ptr SessionKey' -> IO ())
free = [rust| extern fn (*mut SessionKey) { free }|]

wrap :: Ptr SessionKey' -> SessionKey
wrap = unsafePerformIO . fmap SessionKey . newForeignPtr Vodozemac.Megolm.SessionKey.free

instance ToBase64 SessionKey where
    toBase64 :: SessionKey -> ByteString
    toBase64 (SessionKey ptr) = unsafePerformIO . withForeignPtr ptr $ \key ->
        getByteString
            [rust| (*mut u8, usize) {
                let key = unsafe { &*$(key: *const SessionKey) };
                crate::copy_str(key.to_base64())
            }|]

instance FromBase64 SessionKey where
    fromBase64 :: ByteString -> Maybe SessionKey
    fromBase64 bs = wrap <$> withByteString' bs maybeKey
      where
        maybeKey :: Ptr Word8 -> Word -> Maybe (Ptr SessionKey')
        maybeKey ptr len =
            [rust| Option<*mut SessionKey> {
                let slice = unsafe { std::slice::from_raw_parts($(ptr: *const u8), $(len: usize)) };
                let str = std::str::from_utf8(slice).ok()?;
                let key = SessionKey::from_base64(str).ok()?;
                Some(Box::into_raw(Box::new(key)))
            }|]
