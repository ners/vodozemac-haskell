module Vodozemac.Megolm.GroupSession where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import System.IO.Unsafe (unsafePerformIO)
import Vodozemac.Megolm.SessionKey (SessionKey, SessionKey')
import Vodozemac.Megolm.SessionKey qualified as SessionKey
import Vodozemac.Util
import Prelude

data GroupSession'

newtype GroupSession = GroupSession (ForeignPtr GroupSession')

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
extendContext (singleton [ty| GroupSession |] [t|GroupSession'|])

[rust|
extern "C" fn free (ptr: *mut GroupSession) {
    let key = unsafe { Box::from_raw(ptr) };
    drop(key)
}
|]

free :: FunPtr (Ptr GroupSession' -> IO ())
free = [rust| extern fn (*mut GroupSession) { free }|]

wrap :: Ptr GroupSession' -> GroupSession
wrap = unsafePerformIO . fmap GroupSession . newForeignPtr Vodozemac.Megolm.GroupSession.free

newGroupSession :: IO GroupSession
newGroupSession =
    [rustIO| *mut GroupSession {
        Box::into_raw(Box::new(megolm::GroupSession::new(
            megolm::SessionConfig::version_1()
        )))
    } |]
        <&> wrap

{- | Returns the globally unique session ID, in base64-encoded form.
A session ID is the public part of the Ed25519 key pair associated with the group session. Due to the construction, every session ID is (probabilistically) globally unique.
-}
sessionId :: GroupSession -> ByteString
sessionId (GroupSession sessPtr) = unsafePerformIO . withForeignPtr sessPtr $ \sess ->
    getByteString
        [rust| (*mut u8, usize) {
            let sess = unsafe { &*$(sess: *const GroupSession) };
            crate::copy_str(sess.session_id())
        } |]

{- | Export the group session into a session key.
The session key contains the key version constant, the current message index, the ratchet state and the public part of the signing key pair. It is signed by the signing key pair for authenticity.
The session key is in a portable format, suitable for sending over the network. It is typically sent to other group participants so that they can reconstruct an inbound group session in order to decrypt messages sent by this group session.
-}
sessionKey :: GroupSession -> IO SessionKey
sessionKey (GroupSession sessPtr) = withForeignPtr sessPtr $ \sess ->
    [rustIO| *mut SessionKey {
        let sess = unsafe { &*$(sess: *const GroupSession) };
        Box::into_raw(Box::new(sess.session_key()))
    } |]
        <&> SessionKey.wrap

encrypt :: GroupSession -> ByteString -> IO ByteString
encrypt (GroupSession sessPtr) plaintext = withForeignPtr sessPtr \sess -> withByteString plaintext \dataPtr len ->
    [rustIO| (*mut u8, usize) {
        let sess = unsafe { &mut *$(sess: *mut GroupSession) };
        let plaintext = unsafe { std::slice::from_raw_parts($(dataPtr: *const u8), $(len: usize)) };
        let message = sess.encrypt(plaintext);
        crate::copy_bytes(message.to_bytes())
    } |]
        >>= getByteString
