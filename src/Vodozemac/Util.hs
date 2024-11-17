{-# LANGUAGE ViewPatterns #-}

module Vodozemac.Util where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Foreign
import Foreign.C
import Language.Rust.Inline
import System.IO.Unsafe (unsafePerformIO)
import Prelude

extendContext basic
extendContext pointers
extendContext prelude
setCrateModule

[rust|
use std::ffi::CString;

fn copy_str(source: String) -> *mut c_char {
    CString::new(source.into_bytes()).unwrap().into_raw()
}

#[no_mangle]
pub extern "C" fn free_cstring(ptr: *mut c_char) {
    let cstring = unsafe { CString::from_raw(ptr) };
    drop(cstring)
}

// Box<[u8]>
#[no_mangle]
pub extern "C" fn free_bytestring(ptr: *mut u8, size: usize) {
    let bytes = unsafe { Box::from_raw(std::ptr::slice_from_raw_parts_mut(ptr, size)) };
    drop(bytes)
}
|]

freeCstring :: Ptr CChar -> IO ()
freeCstring ptr = pure [rust| () { free_cstring($(ptr: *mut std::ffi::c_char)); }|]

freeBytestring :: Ptr Word8 -> Int -> IO ()
freeBytestring ptr (fromIntegral -> len) = pure [rust| () { free_bytestring($(ptr: *mut u8), $(len: usize)); }|]
