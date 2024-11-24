{-# LANGUAGE InstanceSigs #-}

module Vodozemac.Curve25519PublicKey where

import Data.ByteString (ByteString)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
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
extendContext foreignPointers
extendContext bytestrings
extendContext prelude
extendContext (singleton [ty| Curve25519PublicKey |] [t|Curve25519PublicKey'|])

instance ToBase64 Curve25519PublicKey where
    toBase64 :: Curve25519PublicKey -> ByteString
    toBase64 (Curve25519PublicKey ptr) =
        [rust| Vec<u8> {
            $(ptr: &Curve25519PublicKey).to_base64().into_bytes()
        } |]

instance FromBase64 Curve25519PublicKey where
    fromBase64 :: ByteString -> Maybe Curve25519PublicKey
    fromBase64 bs =
        Curve25519PublicKey
            <$> [rust| Option<ForeignPtr<Curve25519PublicKey>> {
                    let str = std::str::from_utf8($(bs: &[u8])).ok()?;
                    let key = Curve25519PublicKey::from_base64(str).ok()?;
                    Some(Box::new(key).into())
                }|]
