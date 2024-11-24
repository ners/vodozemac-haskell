module Vodozemac.Olm.Session where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import Vodozemac.Olm.Message (Message)
import Prelude

data Session'

newtype Session = Session (ForeignPtr Session')

setCrateModule

[rust|
use vodozemac::*;
use vodozemac::olm::*;
|]

extendContext basic
extendContext ffi
extendContext functions
extendContext foreignPointers
extendContext bytestrings
extendContext prelude
extendContext (singleton [ty| Session |] [t|Session'|])

sessionId :: Session -> ByteString
sessionId (Session sess) = [rust| Vec<u8> { $(sess: &Session).session_id().into_bytes() } |]

hasReceivedMessage :: Session -> IO Bool
hasReceivedMessage (Session sess) = [rustIO| bool { $(sess: &Session).has_received_message() } |] <&> (/= 0)

encrypt :: Session -> ByteString -> IO Message
encrypt (Session sess) plaintext =
    [rustIO| Vec<u8> {
        let message = $(sess: &mut Session).encrypt($(plaintext: &[u8]));
        serde_json::to_string(&message).unwrap().into_bytes()
    } |]
        <&> fromJust . Aeson.decodeStrict

decrypt :: Session -> Message -> IO (Maybe ByteString)
decrypt (Session sess) (Aeson.encode -> ByteString.toStrict -> ciphertext) =
    [rustIO| Option<Vec<u8>> {
        let ciphertext = serde_json::from_slice($(ciphertext: &[u8])).ok()?;
        $(sess: &mut Session).decrypt(&ciphertext).ok()
    } |]
