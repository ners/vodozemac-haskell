{-# LANGUAGE InstanceSigs #-}

module Vodozemac.Ed25519Signature where

import Data.ByteString (ByteString)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import System.IO.Unsafe (unsafePerformIO)
import Vodozemac.Util
import Prelude

data Ed25519Signature'

newtype Ed25519Signature = Ed25519Signature (ForeignPtr Ed25519Signature')

setCrateModule

[rust|
use vodozemac::*;
|]

extendContext basic
extendContext ffi
extendContext functions
extendContext pointers
extendContext prelude
extendContext (singleton [ty| Ed25519Signature |] [t|Ed25519Signature'|])

[rust|
extern "C" fn free (ptr: *mut Ed25519Signature) {
    let sig = unsafe { Box::from_raw(ptr) };
    drop(sig)
}
|]

free :: FunPtr (Ptr Ed25519Signature' -> IO ())
free = [rust| extern fn (*mut Ed25519Signature) { free }|]

wrap :: Ptr Ed25519Signature' -> Ed25519Signature
wrap = unsafePerformIO . fmap Ed25519Signature . newForeignPtr Vodozemac.Ed25519Signature.free

instance ToBase64 Ed25519Signature where
    toBase64 :: Ed25519Signature -> ByteString
    toBase64 (Ed25519Signature ptr) = unsafePerformIO . withForeignPtr ptr $ \sig ->
        getByteString
            [rust| (*mut u8, usize) {
                let sig = unsafe { &*$(sig: *const Ed25519Signature) };
                crate::copy_str(sig.to_base64())
            }|]

instance FromBase64 Ed25519Signature where
    fromBase64 :: ByteString -> Maybe Ed25519Signature
    fromBase64 bs = wrap <$> withByteString' bs maybesig
      where
        maybesig :: Ptr Word8 -> Word -> Maybe (Ptr Ed25519Signature')
        maybesig ptr len =
            [rust| Option<*mut Ed25519Signature> {
                let slice = unsafe { std::slice::from_raw_parts($(ptr: *const u8), $(len: usize)) };
                let str = std::str::from_utf8(slice).ok()?;
                let sig = Ed25519Signature::from_base64(str).ok()?;
                Some(Box::into_raw(Box::new(sig)))
            }|]
