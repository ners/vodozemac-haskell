module Vodozemac.Util where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Foreign
import Language.Rust.Inline
import System.IO.Unsafe (unsafePerformIO)
import Prelude

infixl 4 <$$>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

infixl 1 <&&>

(<&&>) :: (Functor f1) => (Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<&&>) = flip (<$$>)

extendContext basic
extendContext ffi
extendContext pointers
extendContext prelude
setCrateModule

[rust|
pub fn copy_bytes(source: Vec<u8>) -> (*mut u8, usize) {
    let bytes = Box::leak(source.into_boxed_slice());
    let len = bytes.len();
    (bytes.as_mut_ptr(), len)
}

pub fn copy_str(source: String) -> (*mut u8, usize) {
    copy_bytes(source.into_bytes())
}

pub fn free_bytestring(ptr: *mut u8, size: usize) {
    let bytes = unsafe { Box::from_raw(std::ptr::slice_from_raw_parts_mut(ptr, size)) };
    drop(bytes)
}
|]

freeByteString :: (Integral a) => Ptr Word8 -> a -> IO ()
freeByteString ptr (fromIntegral -> len) = [rustIO| () { free_bytestring($(ptr: *mut u8), $(len: usize)); }|]

getByteString :: (Integral a) => (Ptr Word8, a) -> IO ByteString
getByteString (ptr, len) = ByteString.unsafePackCStringFinalizer (castPtr ptr) (fromIntegral len) (freeByteString ptr len)

withByteString' :: ByteString -> (Ptr Word8 -> Word -> a) -> a
withByteString' bs f = unsafePerformIO $ withByteString bs ((pure .) . f)

class ToBase64 a where
    toBase64 :: a -> ByteString

class FromBase64 a where
    fromBase64 :: ByteString -> Maybe a
