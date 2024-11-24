{-# LANGUAGE InstanceSigs #-}

module Vodozemac.Ed25519PublicKey where

import Data.ByteString (ByteString)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
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
extendContext foreignPointers
extendContext bytestrings
extendContext prelude
extendContext (singleton [ty| Ed25519PublicKey |] [t|Ed25519PublicKey'|])

instance ToBase64 Ed25519PublicKey where
    toBase64 :: Ed25519PublicKey -> ByteString
    toBase64 (Ed25519PublicKey key) = [rust| Vec<u8> { $(key: &Ed25519PublicKey).to_base64().into_bytes() }|]

instance FromBase64 Ed25519PublicKey where
    fromBase64 :: ByteString -> Maybe Ed25519PublicKey
    fromBase64 bs =
        Ed25519PublicKey
            <$> [rust| Option<ForeignPtr<Ed25519PublicKey>> {
                    let str = std::str::from_utf8($(bs: &[u8])).ok()?;
                    let key = Ed25519PublicKey::from_base64(str).ok()?;
                    Some(Box::new(key).into())
                } |]
