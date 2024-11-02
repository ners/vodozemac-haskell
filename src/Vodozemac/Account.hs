{-# LANGUAGE TupleSections #-}

module Vodozemac.Account where

import Control.Exception (bracket)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Unsafe qualified as ByteString
import Foreign
import Vodozemac.Curve25519PublicKey
import Vodozemac.Ed25519PublicKey
import Vodozemac.Ed25519Signature
import Vodozemac.FallbackKey
import Vodozemac.Olm.Session (Session (..))
import Vodozemac.Raw.Account qualified as Raw (Account)
import Vodozemac.Raw.Account qualified as Raw.Account
import Vodozemac.Raw.Util qualified as Raw
import Prelude

newtype Account = Account Raw.Account
    deriving stock (Eq, Show)

newAccount :: IO Account
newAccount = Account <$> Raw.Account.new

freeAccount :: Account -> IO ()
freeAccount (Account account) = Raw.Account.free account

withAccount :: (Account -> IO a) -> IO a
withAccount = bracket newAccount freeAccount

markKeysAsPublished :: Account -> IO ()
markKeysAsPublished (Account account) = Raw.Account.mark_keys_as_published account

generateFallbackKey :: Account -> IO (Maybe Curve25519PublicKey)
generateFallbackKey (Account account) = do
    ptr <- Raw.Account.generate_fallback_key account
    pure $ if ptr == nullPtr then Nothing else Just (Curve25519PublicKey ptr)

fallbackKey :: Account -> IO (Maybe FallbackKey)
fallbackKey (Account account) = do
    ptr <- Raw.Account.fallback_key account
    if ptr == nullPtr then pure Nothing else Just <$> peek (castPtr ptr)

sign :: Account -> ByteString -> IO Ed25519Signature
sign (Account account) bs = Ed25519Signature <$> ByteString.unsafeUseAsCStringLen bs (uncurry $ Raw.Account.sign account)

curve25519Key :: Account -> IO Curve25519PublicKey
curve25519Key (Account account) = Curve25519PublicKey <$> Raw.Account.curve25519_key account

ed25519Key :: Account -> IO Ed25519PublicKey
ed25519Key (Account account) = Ed25519PublicKey <$> Raw.Account.ed25519_key account

createOutboundSession :: Account -> Curve25519PublicKey -> Curve25519PublicKey -> IO Session
createOutboundSession (Account account) (Curve25519PublicKey identityKey) (Curve25519PublicKey oneTimeKey) =
    Session <$> Raw.Account.create_outbound_session account identityKey oneTimeKey

createInboundSession :: Account -> Curve25519PublicKey -> Aeson.Object -> IO (Maybe (Session, ByteString))
createInboundSession (Account account) (Curve25519PublicKey theirIdentityKey) preKeyMessage = do
    let json = LazyByteString.toStrict $ Aeson.encode preKeyMessage
    alloca $ \plaintextSize ->
        alloca $ \plaintextPtr -> do
            session <-
                ByteString.unsafeUseAsCStringLen
                    json
                    ( \(input, inputLen) ->
                        Raw.Account.create_inbound_session account theirIdentityKey input inputLen plaintextSize plaintextPtr
                    )
            if session == nullPtr
                then pure Nothing
                else do
                    plaintextSize' <- peek plaintextSize
                    plaintextPtr' <- peek plaintextPtr
                    Just . (Session session,) <$> ByteString.unsafePackCStringFinalizer plaintextPtr' plaintextSize' (Raw.free_bytestring plaintextPtr' plaintextSize')
