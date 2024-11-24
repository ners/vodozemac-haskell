{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

import Data.Aeson (Object)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import Vodozemac.Curve25519PublicKey (Curve25519PublicKey (..), Curve25519PublicKey')
import Vodozemac.Ed25519PublicKey (Ed25519PublicKey (..), Ed25519PublicKey')
import Vodozemac.Ed25519Signature (Ed25519Signature (..), Ed25519Signature')
import Vodozemac.KeyId (KeyId (..), KeyId')
import Vodozemac.Olm.Session (Session (..), Session')
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
extendContext foreignPointers
extendContext bytestrings
extendContext prelude
extendContext (singleton [ty| Curve25519PublicKey |] [t|Curve25519PublicKey'|])
extendContext (singleton [ty| Ed25519PublicKey |] [t|Ed25519PublicKey'|])
extendContext (singleton [ty| Ed25519Signature |] [t|Ed25519Signature'|])
extendContext (singleton [ty| Account |] [t|Account'|])
extendContext (singleton [ty| KeyId |] [t|KeyId'|])
extendContext (singleton [ty| Session |] [t|Session'|])

newAccount :: IO Account
newAccount = Account <$> [rustIO| ForeignPtr<Account> { Box::new(Account::new()).into() } |]

curve25519Key :: Account -> Curve25519PublicKey
curve25519Key (Account acc) =
    Curve25519PublicKey
        [rust| ForeignPtr<Curve25519PublicKey> {
            Box::new($(acc: &Account).curve25519_key()).into()
        } |]

ed25519Key :: Account -> Ed25519PublicKey
ed25519Key (Account acc) =
    Ed25519PublicKey
        [rust| ForeignPtr<Ed25519PublicKey> {
            Box::new($(acc: &Account).ed25519_key()).into()
        } |]

sign :: Account -> ByteString -> Ed25519Signature
sign (Account acc) bs =
    Ed25519Signature
        [rust| ForeignPtr<Ed25519Signature> {
            Box::new($(acc: &Account).sign($(bs: &[u8]))).into()
        } |]

generateFallbackKey :: Account -> IO (Maybe Curve25519PublicKey)
generateFallbackKey (Account acc) =
    [rustIO| Option<ForeignPtr<Curve25519PublicKey>> {
        $(acc: &mut Account).generate_fallback_key().map(|key| Box::new(key).into())
    } |]
        <&&> Curve25519PublicKey

fallbackKey :: Account -> IO (Maybe (KeyId, Curve25519PublicKey))
fallbackKey (Account acc) =
    alloca \keyIdPtr -> alloca \keyIdFinalizer -> do
        mpubkey <-
            [rustIO| Option<ForeignPtr<Curve25519PublicKey>> {
                let keys = $(acc: &Account).fallback_key();
                if keys.is_empty() {
                    return None;
                }
                let (key_id, public_key) = keys.into_iter().collect::<Vec<_>>()[0];
                let ForeignPtr(key_id_ptr, key_id_finalizer) = Box::new(key_id).into();
                unsafe {
                    *$(keyIdPtr: *mut *mut KeyId) = key_id_ptr;
                    *$(keyIdFinalizer: *mut extern "C" fn (*mut KeyId)) = key_id_finalizer;
                }
                Some(Box::new(public_key).into())
            } |]
                <&&> Curve25519PublicKey
        case mpubkey of
            Nothing -> pure Nothing
            Just pubkey -> do
                keyIdPtr' <- peek keyIdPtr
                keyIdFinalizer' <- peek keyIdFinalizer
                keyId <- KeyId <$> newForeignPtr keyIdFinalizer' keyIdPtr'
                pure . pure $ (keyId, pubkey)

markKeysAsPublished :: Account -> IO ()
markKeysAsPublished (Account acc) =
    [rustIO| () {
        $(acc: &mut Account).mark_keys_as_published()
    } |]

createOutboundSession :: Account -> Curve25519PublicKey -> Curve25519PublicKey -> Session
createOutboundSession (Account acc) (Curve25519PublicKey identityKey) (Curve25519PublicKey oneTimeKey) =
    Session
        [rust| ForeignPtr<Session> {
        let session = $(acc: &Account).create_outbound_session(
            // matrix only uses version 1
            olm::SessionConfig::version_1(),
            *$(identityKey: &Curve25519PublicKey),
            *$(oneTimeKey: &Curve25519PublicKey),
        );
        Box::new(session).into()
    } |]

createInboundSession :: Account -> Curve25519PublicKey -> Object -> IO (Maybe (Session, ByteString))
createInboundSession (Account acc) (Curve25519PublicKey theirIdentityKey) preKeyMessage =
    let bs = ByteString.toStrict $ Aeson.encode preKeyMessage
     in alloca \sessionPtr -> alloca \sessionFinalizer -> do
            mplaintext <-
                [rustIO| Option<Vec<u8>> {
                let olm::OlmMessage::PreKey(prekey_message) = serde_json::from_slice($(bs: &[u8])).ok()?
                else { return None; };
                let olm::InboundCreationResult { session, plaintext } = $(acc: &mut Account).create_inbound_session(*$(theirIdentityKey: &Curve25519PublicKey), &prekey_message).ok()?;
                let ForeignPtr(session_ptr, session_finalizer) = Box::new(session).into();
                unsafe {
                    *$(sessionPtr: *mut *mut Session) = session_ptr;
                    *$(sessionFinalizer: *mut extern "C" fn (*mut Session)) = session_finalizer;
                }
                Some(plaintext)
            } |]
            case mplaintext of
                Nothing -> pure Nothing
                Just plaintext -> do
                    sessionPtr' <- peek sessionPtr
                    sessionFinalizer' <- peek sessionFinalizer
                    session <- Session <$> newForeignPtr sessionFinalizer' sessionPtr'
                    pure . pure $ (session, plaintext)
