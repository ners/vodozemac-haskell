module Vodozemac.Megolm.InboundGroupSession where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import System.IO.Unsafe (unsafePerformIO)
import Vodozemac.Megolm.SessionKey (SessionKey (..), SessionKey')
import Vodozemac.Util
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
extendContext pointers
extendContext prelude
extendContext (singleton [ty| SessionKey |] [t|SessionKey'|])
extendContext (singleton [ty| InboundGroupSession |] [t|InboundGroupSession'|])

[rust|
extern "C" fn free (ptr: *mut InboundGroupSession) {
    let key = unsafe { Box::from_raw(ptr) };
    drop(key)
}
|]

free :: FunPtr (Ptr InboundGroupSession' -> IO ())
free = [rust| extern fn (*mut InboundGroupSession) { free }|]

wrap :: Ptr InboundGroupSession' -> InboundGroupSession
wrap = unsafePerformIO . fmap InboundGroupSession . newForeignPtr Vodozemac.Megolm.InboundGroupSession.free

newInboundGroupSession :: SessionKey -> IO InboundGroupSession
newInboundGroupSession (SessionKey keyPtr) = withForeignPtr keyPtr \key ->
    [rustIO| *mut InboundGroupSession {
        let key = unsafe { &*$(key: *const SessionKey) };
        Box::into_raw(Box::new(megolm::InboundGroupSession::new(
            key,
            megolm::SessionConfig::version_1()
        )))
    } |]
        <&> wrap

sessionId :: InboundGroupSession -> ByteString
sessionId (InboundGroupSession sessPtr) = unsafePerformIO . withForeignPtr sessPtr $ \sess ->
    getByteString
        [rust| (*mut u8, usize) {
            let sess = unsafe { &*$(sess: *const InboundGroupSession) };
            crate::copy_str(sess.session_id())
        } |]

decrypt :: InboundGroupSession -> ByteString -> IO (Maybe (ByteString, Word32))
decrypt (InboundGroupSession sessPtr) ciphertext = withForeignPtr sessPtr \sess -> withByteString ciphertext \dataPtr len ->
    [rustIO| Option<((*mut u8, usize), u32)> {
        let sess = unsafe { &mut *$(sess: *mut InboundGroupSession) };
        let ciphertext = unsafe { std::slice::from_raw_parts($(dataPtr: *const u8), $(len: usize)) };
        let message = megolm::MegolmMessage::from_bytes(ciphertext).ok()?;
        let megolm::DecryptedMessage{plaintext, message_index} = sess.decrypt(&message).ok()?;
        Some((crate::copy_bytes(plaintext), message_index))
    } |]
        >>= mapM \(bs', messageIndex) -> getByteString bs' <&> (,messageIndex)
