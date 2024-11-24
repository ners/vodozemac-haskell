module Vodozemac.Megolm.InboundGroupSession where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import Vodozemac.Megolm.SessionKey (SessionKey (..), SessionKey')
import Prelude

data InboundGroupSession'

newtype InboundGroupSession = InboundGroupSession (ForeignPtr InboundGroupSession')

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
extendContext (singleton [ty| InboundGroupSession |] [t|InboundGroupSession'|])

newInboundGroupSession :: SessionKey -> IO InboundGroupSession
newInboundGroupSession (SessionKey key) =
    [rustIO| ForeignPtr<InboundGroupSession> {
        Box::new(megolm::InboundGroupSession::new(
            $(key: &SessionKey),
            megolm::SessionConfig::version_1()
        )).into()
    } |]
        <&> InboundGroupSession

sessionId :: InboundGroupSession -> ByteString
sessionId (InboundGroupSession sess) = [rust| Vec<u8> { $(sess: &InboundGroupSession).session_id().into_bytes() }|]

decrypt :: InboundGroupSession -> ByteString -> IO (Maybe (ByteString, Word32))
decrypt (InboundGroupSession sess) ciphertext = do
    messageIndex <- mallocForeignPtr
    plaintext <-
        [rustIO| Option<Vec<u8>> {
        let message = megolm::MegolmMessage::from_bytes($(ciphertext: &[u8])).ok()?;
        let megolm::DecryptedMessage{plaintext, message_index} = $(sess: &mut InboundGroupSession).decrypt(&message).ok()?;
        *$(messageIndex: &mut u32) = message_index;
        Some(plaintext)
    } |]
    withForeignPtr messageIndex peek <&> \idx -> (,idx) <$> plaintext
