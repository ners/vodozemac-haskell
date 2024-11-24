{-# LANGUAGE InstanceSigs #-}

module Vodozemac.Ed25519Signature where

import Data.ByteString (ByteString)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
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
extendContext foreignPointers
extendContext bytestrings
extendContext prelude
extendContext (singleton [ty| Ed25519Signature |] [t|Ed25519Signature'|])

instance ToBase64 Ed25519Signature where
    toBase64 :: Ed25519Signature -> ByteString
    toBase64 (Ed25519Signature sig) = [rust| Vec<u8> { $(sig: &Ed25519Signature).to_base64().into_bytes() } |]

instance FromBase64 Ed25519Signature where
    fromBase64 :: ByteString -> Maybe Ed25519Signature
    fromBase64 bs =
        Ed25519Signature
            <$> [rust| Option<ForeignPtr<Ed25519Signature>> {
                    let str = std::str::from_utf8($(bs: &[u8])).ok()?;
                    let sig = Ed25519Signature::from_base64(str).ok()?;
                    Some(Box::new(sig).into())
                } |]
