module Vodozemac.Curve25519PublicKey where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Foreign
import Language.Rust.Inline
import System.IO.Unsafe (unsafePerformIO)
import Prelude

extendContext prelude
extendContext pointers
extendContext basic
setCrateModule

[rust|
use std::ffi::CString;
|]

newtype Curve25519PublicKey = Curve25519PublicKey (ForeignPtr ())

toBase64 :: Curve25519PublicKey -> ByteString
toBase64 (Curve25519PublicKey ptr) = unsafePerformIO $ ByteString.unsafePackCStringFinalizer cstr (fromIntegral len) (pure freeBytestring)
  where
    (cstr, len) =
        [rust| (*const u8, usize) {
            let key: &mut Curve25519PublicKey = unsafe { &mut *ptr };
            copy_str(key.to_base64())
        }|]

fromBase64 :: ByteString -> Maybe Curve25519PublicKey
fromBase64 = undefined
