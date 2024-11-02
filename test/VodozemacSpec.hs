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

spec :: Spec
spec = do
    it "generates fallback keys" $ Vodozemac.Account.withAccount \account -> do
        void $ Vodozemac.Account.generateFallbackKey account
        Just (FallbackKey keyId pubKey) <- Vodozemac.Account.fallbackKey account
        putStrLn =<< Vodozemac.KeyId.toBase64 keyId
        putStrLn =<< Vodozemac.Curve25519PublicKey.toBase64 pubKey
        Just (FallbackKey keyId pubKey) <- Vodozemac.Account.fallbackKey account
        putStrLn =<< Vodozemac.KeyId.toBase64 keyId
        putStrLn =<< Vodozemac.Curve25519PublicKey.toBase64 pubKey
        void $ Vodozemac.Account.generateFallbackKey account
        Just (FallbackKey keyId pubKey) <- Vodozemac.Account.fallbackKey account
        putStrLn =<< Vodozemac.KeyId.toBase64 keyId
        putStrLn =<< Vodozemac.Curve25519PublicKey.toBase64 pubKey
        Just (FallbackKey keyId pubKey) <- Vodozemac.Account.fallbackKey account
        putStrLn =<< Vodozemac.KeyId.toBase64 keyId
        putStrLn =<< Vodozemac.Curve25519PublicKey.toBase64 pubKey

    it "signs" $ Vodozemac.Account.withAccount \account -> do
        putStrLn "ed25519_key"
        print =<< Vodozemac.Ed25519PublicKey.toBase64 =<< Vodozemac.Account.ed25519Key account
        putStrLn "trying to sign ..."
        signature <- Vodozemac.Account.sign account "Hello world!"
        putStrLn "SUCCESS"
        putStrLn =<< Vodozemac.Ed25519Signature.toBase64 signature

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
