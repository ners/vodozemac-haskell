module Vodozemac.Olm.Session where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import System.IO.Unsafe (unsafePerformIO)
import Vodozemac.Olm.Message (Message)
import Vodozemac.Util
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
extendContext pointers
extendContext prelude
extendContext (singleton [ty| Session |] [t|Session'|])

[rust|
extern "C" fn free (ptr: *mut Session) {
    let key = unsafe { Box::from_raw(ptr) };
    drop(key)
}
|]

free :: FunPtr (Ptr Session' -> IO ())
free = [rust| extern fn (*mut Session) { free }|]

wrap :: Ptr Session' -> Session
wrap = unsafePerformIO . fmap Session . newForeignPtr Vodozemac.Olm.Session.free

sessionId :: Session -> ByteString
sessionId (Session sessPtr) = unsafePerformIO . withForeignPtr sessPtr $ \sess ->
    getByteString
        [rust| (*mut u8, usize) {
            let sess = unsafe { &*$(sess: *const Session) };
            crate::copy_str(sess.session_id())
        } |]

hasReceivedMessage :: Session -> IO Bool
hasReceivedMessage (Session ptr) = withForeignPtr ptr \sess ->
    [rustIO| bool {
        let sess = unsafe { &*$(sess: *const Session) };
        sess.has_received_message()
    } |]
        <&> (/= 0)

encrypt :: Session -> ByteString -> IO Message
encrypt (Session sessPtr) plaintext = withForeignPtr sessPtr \sess -> withByteString plaintext \dataPtr len ->
    [rustIO| (*mut u8, usize) {
        let sess = unsafe { &mut *$(sess: *mut Session) };
        let plaintext = unsafe { std::slice::from_raw_parts($(dataPtr: *const u8), $(len: usize)) };
        let message = sess.encrypt(plaintext);
        crate::copy_str(serde_json::to_string(&message).unwrap())
    } |]
        >>= getByteString
        <&> fromJust . Aeson.decodeStrict

decrypt :: Session -> Message -> IO (Maybe ByteString)
decrypt (Session sessPtr) (Aeson.encode -> ByteString.toStrict -> ciphertext) = withForeignPtr sessPtr \sess -> withByteString ciphertext \dataPtr len ->
    [rustIO| Option<(*mut u8, usize)> {
        let sess = unsafe { &mut *$(sess: *mut Session) };
        let ciphertext_json = unsafe { std::slice::from_raw_parts($(dataPtr: *const u8), $(len: usize)) };
        let ciphertext = serde_json::from_slice(ciphertext_json).ok()?;
        let plaintext = sess.decrypt(&ciphertext).ok()?;
        Some(crate::copy_bytes(plaintext))
    } |]
        >>= mapM getByteString
