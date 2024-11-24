{-# LANGUAGE InstanceSigs #-}

module Vodozemac.KeyId where

import Data.ByteString (ByteString)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import Vodozemac.Util

data KeyId'

newtype KeyId = KeyId (ForeignPtr KeyId')

setCrateModule

[rust|
use vodozemac::*;
|]

extendContext basic
extendContext ffi
extendContext functions
extendContext foreignPointers
extendContext bytestrings
extendContext prelude
extendContext (singleton [ty| KeyId |] [t|KeyId'|])

instance ToBase64 KeyId where
    toBase64 :: KeyId -> ByteString
    toBase64 (KeyId key) = [rust| Vec<u8> { $(key: &KeyId).to_base64().into_bytes() }|]
