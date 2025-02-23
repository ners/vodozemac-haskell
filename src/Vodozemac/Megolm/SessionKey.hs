{-# LANGUAGE InstanceSigs #-}

module Vodozemac.Megolm.SessionKey where

import Data.ByteString (ByteString)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import Vodozemac.Util
import Prelude

data SessionKey'

newtype SessionKey = SessionKey (ForeignPtr SessionKey')

setCrateModule

[rust|
use vodozemac::*;
use vodozemac::megolm::*;
|]

extendContext basic
extendContext ffi
extendContext functions
extendContext foreignPointers
extendContext bytestrings
extendContext prelude
extendContext (singleton [ty| SessionKey |] [t|SessionKey'|])

instance ToBase64 SessionKey where
    toBase64 :: SessionKey -> ByteString
    toBase64 (SessionKey key) =
        [rust| Vec<u8> {
            $(key: &SessionKey).to_base64().into_bytes()
        } |]

instance FromBase64 SessionKey where
    fromBase64 :: ByteString -> Maybe SessionKey
    fromBase64 bs =
        [rust| Option<ForeignPtr<SessionKey>> {
            let str = std::str::from_utf8($(bs: &[u8])).ok()?;
            let key = SessionKey::from_base64(str).ok()?;
            Some(Box::new(key).into())
        }|]
            <&> SessionKey
