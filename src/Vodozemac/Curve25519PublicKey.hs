{-# LANGUAGE InstanceSigs #-}

module Vodozemac.Curve25519PublicKey where

import Data.ByteString (ByteString)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import System.IO.Unsafe (unsafePerformIO)
import Vodozemac.Util
import Prelude

data Curve25519PublicKey'

newtype Curve25519PublicKey = Curve25519PublicKey (ForeignPtr Curve25519PublicKey')

setCrateModule

[rust|
use vodozemac::*;
|]

extendContext basic
extendContext ffi
extendContext functions
extendContext pointers
extendContext prelude
extendContext (singleton [ty| Curve25519PublicKey |] [t|Curve25519PublicKey'|])

[rust|
extern "C" fn free (ptr: *mut Curve25519PublicKey) {
    let key = unsafe { Box::from_raw(ptr) };
    drop(key)
}
|]

free :: FunPtr (Ptr Curve25519PublicKey' -> IO ())
free = [rust| extern fn (*mut Curve25519PublicKey) { free }|]

wrap :: Ptr Curve25519PublicKey' -> Curve25519PublicKey
wrap = unsafePerformIO . fmap Curve25519PublicKey . newForeignPtr Vodozemac.Curve25519PublicKey.free

instance ToBase64 Curve25519PublicKey where
    toBase64 :: Curve25519PublicKey -> ByteString
    toBase64 (Curve25519PublicKey ptr) = unsafePerformIO . withForeignPtr ptr $ \key ->
        getByteString
            [rust| (*mut u8, usize) {
                let key = unsafe { &*$(key: *const Curve25519PublicKey) };
                crate::copy_str(key.to_base64())
            }|]

instance FromBase64 Curve25519PublicKey where
    fromBase64 :: ByteString -> Maybe Curve25519PublicKey
    fromBase64 bs = wrap <$> withByteString' bs maybeKey
      where
        maybeKey :: Ptr Word8 -> Word -> Maybe (Ptr Curve25519PublicKey')
        maybeKey ptr len =
            [rust| Option<*mut Curve25519PublicKey> {
                let slice = unsafe { std::slice::from_raw_parts($(ptr: *const u8), $(len: usize)) };
                let str = std::str::from_utf8(slice).ok()?;
                let key = Curve25519PublicKey::from_base64(str).ok()?;
                Some(Box::into_raw(Box::new(key)))
            }|]
