{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module VodozemacSpec where

import Data.ByteString (ByteString)
import Data.Functor (void)
import Test.Hspec
import Vodozemac.Account qualified
import Vodozemac.Curve25519PublicKey qualified
import Vodozemac.Ed25519PublicKey qualified
import Vodozemac.Ed25519Signature qualified
import Vodozemac.FallbackKey (FallbackKey (..))
import Vodozemac.KeyId qualified
import Vodozemac.Olm.Message (Message (PreKeyMessage))
import Vodozemac.Olm.Session qualified
import Prelude
import Vodozemac.Account (curve25519Key)

shouldReturnOver :: (Show a1, Eq a1) => (a2 -> IO a1) -> IO a2 -> a2 -> IO ()
shouldReturnOver f a b = do
    a' <- f =<< a
    b' <- f b
    a' `shouldBe` b'

shouldBeOver :: (Show a, Eq a) => (t -> IO a) -> t -> t -> IO ()
shouldBeOver f a b = do
    a' <- f a
    b' <- f b
    a' `shouldBe` b'

shouldNotBeOver :: (Show a, Eq a) => (t -> IO a) -> t -> t -> IO ()
shouldNotBeOver f a b = do
    a' <- f a
    b' <- f b
    a' `shouldNotBe` b'

spec :: Spec
spec = do
    it "encodes and decodes" $ Vodozemac.Account.withAccount \account -> do
        curve25519Key <- Vodozemac.Account.curve25519Key account
        shouldReturnOver Vodozemac.Curve25519PublicKey.toBase64
            (Vodozemac.Curve25519PublicKey.toBase64 curve25519Key >>= Vodozemac.Curve25519PublicKey.fromBase64)
            curve25519Key
        ed25519Key <- Vodozemac.Account.ed25519Key account
        shouldReturnOver Vodozemac.Ed25519PublicKey.toBase64
            (Vodozemac.Ed25519PublicKey.toBase64 ed25519Key >>= Vodozemac.Ed25519PublicKey.fromBase64)
            ed25519Key
        ed25519Signature <- Vodozemac.Account.sign account "junk"
        shouldReturnOver Vodozemac.Ed25519Signature.toBase64
            (Vodozemac.Ed25519Signature.toBase64 ed25519Signature >>= Vodozemac.Ed25519Signature.fromBase64)
            ed25519Signature

    it "generates fallback keys" $ Vodozemac.Account.withAccount \account -> do
        void $ Vodozemac.Account.generateFallbackKey account
        Just (FallbackKey keyId1 pubKey1) <- Vodozemac.Account.fallbackKey account
        Just (FallbackKey keyId2 pubKey2) <- Vodozemac.Account.fallbackKey account
        shouldBeOver Vodozemac.KeyId.toBase64 keyId1 keyId2
        shouldBeOver Vodozemac.Curve25519PublicKey.toBase64 pubKey1 pubKey2
        void $ Vodozemac.Account.generateFallbackKey account
        Just (FallbackKey keyId3 pubKey3) <- Vodozemac.Account.fallbackKey account
        Just (FallbackKey keyId4 pubKey4) <- Vodozemac.Account.fallbackKey account
        shouldBeOver Vodozemac.KeyId.toBase64 keyId3 keyId4
        shouldBeOver Vodozemac.Curve25519PublicKey.toBase64 pubKey3 pubKey4
        shouldNotBeOver Vodozemac.KeyId.toBase64 keyId1 keyId3
        shouldNotBeOver Vodozemac.Curve25519PublicKey.toBase64 pubKey1 pubKey3

    it "signs" $ Vodozemac.Account.withAccount \account -> do
        print =<< Vodozemac.Ed25519PublicKey.toBase64 =<< Vodozemac.Account.ed25519Key account
        signature <- Vodozemac.Account.sign account "Hello world!"
        print =<< Vodozemac.Ed25519Signature.toBase64 signature

    it "has keys" $ Vodozemac.Account.withAccount \account -> do
        print =<< Vodozemac.Curve25519PublicKey.toBase64 =<< Vodozemac.Account.curve25519Key account
        print =<< Vodozemac.Ed25519PublicKey.toBase64 =<< Vodozemac.Account.ed25519Key account

    it "sessions" $ Vodozemac.Account.withAccount \sender -> Vodozemac.Account.withAccount \receiver -> do
        senderIdentityKey <- Vodozemac.Account.curve25519Key sender
        receiverIdentityKey <- Vodozemac.Account.curve25519Key receiver

        void $ Vodozemac.Account.generateFallbackKey receiver
        Just (FallbackKey _ receiverFallbackKey) <- Vodozemac.Account.fallbackKey receiver
        Vodozemac.Account.markKeysAsPublished receiver

        senderSession <- Vodozemac.Account.createOutboundSession sender receiverIdentityKey receiverFallbackKey
        let expectedPlaintext = "Hello!" :: ByteString
        PreKeyMessage preKeyMessage <- Vodozemac.Olm.Session.encrypt senderSession expectedPlaintext

        Just (receiverSession, actualPlaintext) <- Vodozemac.Account.createInboundSession receiver senderIdentityKey preKeyMessage
        actualPlaintext `shouldBe` expectedPlaintext

        let expectedPlaintext = "Goodbye ..." :: ByteString
        message <- Vodozemac.Olm.Session.encrypt receiverSession expectedPlaintext
        Just actualPlaintext <- Vodozemac.Olm.Session.decrypt senderSession message
        actualPlaintext `shouldBe` expectedPlaintext

        let expectedPlaintext = "Just one more thing" :: ByteString
        message <- Vodozemac.Olm.Session.encrypt senderSession expectedPlaintext
        Just actualPlaintext <- Vodozemac.Olm.Session.decrypt receiverSession message
        actualPlaintext `shouldBe` expectedPlaintext
