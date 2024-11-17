{-# LANGUAGE LambdaCase #-}

module Vodozemac.Olm.Account
    ( Account
    , newAccount
    , curve25519Key
    , ed25519Key
    , sign
    , generateFallbackKey
    , fallbackKey
    , markKeysAsPublished
    , createOutboundSession
    , createInboundSession
    )
where

import Control.Arrow ((***))
import Data.Aeson (Object)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import System.IO.Unsafe (unsafePerformIO)
import Vodozemac.Curve25519PublicKey (Curve25519PublicKey (..), Curve25519PublicKey')
import Vodozemac.Curve25519PublicKey qualified as Curve25519PublicKey
import Vodozemac.Ed25519PublicKey (Ed25519PublicKey (..), Ed25519PublicKey')
import Vodozemac.Ed25519PublicKey qualified as Ed25519PublicKey
import Vodozemac.Ed25519Signature (Ed25519Signature (..), Ed25519Signature')
import Vodozemac.Ed25519Signature qualified as Ed25519Signature
import Vodozemac.KeyId (KeyId (..), KeyId')
import Vodozemac.KeyId qualified as KeyId
import Vodozemac.Olm.Session (Session (..), Session')
import Vodozemac.Olm.Session qualified as Session
import Vodozemac.Util
import Prelude

data Account'

newtype Account = Account (ForeignPtr Account')

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
extendContext (singleton [ty| Curve25519PublicKey |] [t|Curve25519PublicKey'|])
extendContext (singleton [ty| Ed25519PublicKey |] [t|Ed25519PublicKey'|])
extendContext (singleton [ty| Ed25519Signature |] [t|Ed25519Signature'|])
extendContext (singleton [ty| Account |] [t|Account'|])
extendContext (singleton [ty| KeyId |] [t|KeyId'|])
extendContext (singleton [ty| Session |] [t|Session'|])

[rust|
extern "C" fn free (ptr: *mut Account) {
    let acc = unsafe { Box::from_raw(ptr) };
    drop(acc)
}
|]

free :: FunPtr (Ptr Account' -> IO ())
free = [rust| extern fn (*mut Account) { free }|]

wrap :: Ptr Account' -> Account
wrap = unsafePerformIO . fmap Account . newForeignPtr Vodozemac.Olm.Account.free

newAccount :: IO Account
newAccount = wrap <$> [rustIO| *mut Account { Box::into_raw(Box::new(Account::new())) } |]

curve25519Key :: Account -> Curve25519PublicKey
curve25519Key (Account ptr) = unsafePerformIO . withForeignPtr ptr $ \acc ->
    pure $
        Curve25519PublicKey.wrap
            [rust|
            *mut Curve25519PublicKey {
                let acc: &Account = unsafe { &*$(acc: *const Account) };
                let key = acc.curve25519_key();
                Box::into_raw(Box::new(key))
            }
            |]

ed25519Key :: Account -> Ed25519PublicKey
ed25519Key (Account ptr) = unsafePerformIO . withForeignPtr ptr $ \acc ->
    pure $
        Ed25519PublicKey.wrap
            [rust|
            *mut Ed25519PublicKey {
                let acc = unsafe { &*$(acc: *const Account) };
                let key = acc.ed25519_key();
                Box::into_raw(Box::new(key))
            }
            |]

sign :: Account -> ByteString -> Ed25519Signature
sign (Account accPtr) bs = unsafePerformIO . withForeignPtr accPtr $ \acc -> pure . Ed25519Signature.wrap $ withByteString' bs \dataPtr len ->
    [rust|
    *mut Ed25519Signature {
        let acc = unsafe { &*$(acc: *const Account) };
        let message: &[u8] = unsafe { &*std::slice::from_raw_parts($(dataPtr: *const u8), $(len: usize)) };
        let signature = acc.sign(message);
        Box::into_raw(Box::new(signature))
    }
    |]

generateFallbackKey :: Account -> IO (Maybe Curve25519PublicKey)
generateFallbackKey (Account ptr) = withForeignPtr ptr $ \acc ->
    [rustIO|
    Option<*mut Curve25519PublicKey> {
        let acc: &mut Account = unsafe { &mut *$(acc: *mut Account) };
        acc.generate_fallback_key().map(|key| Box::into_raw(Box::new(key)))
    }
    |]
        <&&> Curve25519PublicKey.wrap

fallbackKey :: Account -> IO (Maybe (KeyId, Curve25519PublicKey))
fallbackKey (Account ptr) = withForeignPtr ptr $ \acc ->
    [rustIO|
    Option<(*mut KeyId, *mut Curve25519PublicKey)> {
        let acc = unsafe { &*$(acc: *const Account) };
        let keys = acc.fallback_key();
        if keys.is_empty() {
            return None;
        }
        let (key_id, public_key) = keys.into_iter().collect::<Vec<_>>()[0];
        let key_id = Box::into_raw(Box::new(key_id));
        let public_key = Box::into_raw(Box::new(public_key));
        Some((key_id, public_key))
    }
    |]
        <&&> KeyId.wrap
        *** Curve25519PublicKey.wrap

markKeysAsPublished :: Account -> IO ()
markKeysAsPublished (Account ptr) = withForeignPtr ptr $ \acc ->
    [rustIO|
    () {
        let acc = unsafe { &mut *$(acc: *mut Account) };
        acc.mark_keys_as_published()
    }
    |]

createOutboundSession :: Account -> Curve25519PublicKey -> Curve25519PublicKey -> Session
createOutboundSession (Account accPtr) (Curve25519PublicKey identityKeyPtr) (Curve25519PublicKey oneTimeKeyPtr) =
    unsafePerformIO $ withForeignPtr accPtr \acc -> withForeignPtr identityKeyPtr \identityKey -> withForeignPtr oneTimeKeyPtr \oneTimeKey ->
        pure $
            Session.wrap
                [rust|
        *mut Session {
            let acc = unsafe { &*$(acc: *const Account) };
            let identity_key = unsafe { &*$(identityKey: *const Curve25519PublicKey) };
            let one_time_key = unsafe { &*$(oneTimeKey: *const Curve25519PublicKey) };
            let session = acc.create_outbound_session(
                // matrix only uses version 1
                olm::SessionConfig::version_1(),
                *identity_key,
                *one_time_key,
            );
            Box::into_raw(Box::new(session))
        }
        |]

createInboundSession :: Account -> Curve25519PublicKey -> Object -> IO (Maybe (Session, ByteString))
createInboundSession (Account accPtr) (Curve25519PublicKey theirIdentityKeyPtr) preKeyMessage =
    let bs = ByteString.toStrict $ Aeson.encode preKeyMessage
     in withForeignPtr accPtr \acc -> withForeignPtr theirIdentityKeyPtr \theirIdentityKey -> withByteString bs \dataPtr len -> do
            [rustIO|
        Option<(*mut Session, (*mut u8, usize))> {
            let acc = unsafe { &mut *$(acc: *mut Account) };
            let their_identity_key = unsafe { &*$(theirIdentityKey: *const Curve25519PublicKey) };
            let prekey_message_json =
                unsafe { std::slice::from_raw_parts($(dataPtr: *const u8), $(len: usize)) };
            let olm::OlmMessage::PreKey(prekey_message) = serde_json::from_slice(prekey_message_json).ok()?
            else { return None; };
            let olm::InboundCreationResult { session, plaintext } = acc.create_inbound_session(*their_identity_key, &prekey_message).ok()?;
            Some((Box::into_raw(Box::new(session)), crate::copy_bytes(plaintext)))
        }
        |]
                >>= mapM \(sessPtr, bs') -> (Session.wrap sessPtr,) <$> getByteString bs'
