{-# LANGUAGE InstanceSigs #-}

module Vodozemac.Ed25519PublicKey where

import Data.ByteString (ByteString)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import System.IO.Unsafe (unsafePerformIO)
import Vodozemac.Util
import Prelude

data Ed25519PublicKey'

newtype Ed25519PublicKey = Ed25519PublicKey (ForeignPtr Ed25519PublicKey')

setCrateModule

[rust|
use vodozemac::*;
|]

extendContext basic
extendContext ffi
extendContext functions
extendContext pointers
extendContext prelude
extendContext (singleton [ty| Ed25519PublicKey |] [t|Ed25519PublicKey'|])

[rust|
extern "C" fn free (ptr: *mut Ed25519PublicKey) {
    let key = unsafe { Box::from_raw(ptr) };
    drop(key)
}
|]

free :: FunPtr (Ptr Ed25519PublicKey' -> IO ())
free = [rust| extern fn (*mut Ed25519PublicKey) { free }|]

wrap :: Ptr Ed25519PublicKey' -> Ed25519PublicKey
wrap = unsafePerformIO . fmap Ed25519PublicKey . newForeignPtr Vodozemac.Ed25519PublicKey.free

instance ToBase64 Ed25519PublicKey where
    toBase64 :: Ed25519PublicKey -> ByteString
    toBase64 (Ed25519PublicKey ptr) = unsafePerformIO . withForeignPtr ptr $ \key ->
        getByteString
            [rust| (*mut u8, usize) {
                let key = unsafe { &*$(key: *const Ed25519PublicKey) };
                crate::copy_str(key.to_base64())
            }|]

instance FromBase64 Ed25519PublicKey where
    fromBase64 :: ByteString -> Maybe Ed25519PublicKey
    fromBase64 bs = wrap <$> withByteString' bs maybeKey
      where
        maybeKey :: Ptr Word8 -> Word -> Maybe (Ptr Ed25519PublicKey')
        maybeKey ptr len =
            [rust| Option<*mut Ed25519PublicKey> {
                let slice = unsafe { std::slice::from_raw_parts($(ptr: *const u8), $(len: usize)) };
                let str = std::str::from_utf8(slice).ok()?;
                let key = Ed25519PublicKey::from_base64(str).ok()?;
                Some(Box::into_raw(Box::new(key)))
            }|]
