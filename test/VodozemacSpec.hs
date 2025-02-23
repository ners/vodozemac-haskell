{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module VodozemacSpec where

import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Maybe (fromJust)
import Prelude
import Test.Hspec
import Vodozemac
import Vodozemac.Megolm.GroupSession qualified
import Vodozemac.Megolm.InboundGroupSession qualified
import Vodozemac.Olm.Account qualified
import Vodozemac.Olm.Message (Message (PreKeyMessage))
import Vodozemac.Olm.Session qualified

shouldBeOver :: (Eq b, Show b) => (a -> b) -> a -> a -> IO ()
shouldBeOver f a b = f a `shouldBe` f b

shouldNotBeOver :: (Eq b, Show b) => (a -> b) -> a -> a -> IO ()
shouldNotBeOver f a b = f a `shouldNotBe` f b

shouldBeOverB64 :: (ToBase64 a) => a -> a -> IO ()
shouldBeOverB64 = shouldBeOver toBase64

shouldNotBeOverB64 :: (ToBase64 a) => a -> a -> IO ()
shouldNotBeOverB64 = shouldNotBeOver toBase64

spec :: Spec
spec = do
    it "encodes and decodes" do
        account <- Vodozemac.Olm.Account.newAccount
        let roundtrip :: forall a. (ToBase64 a, FromBase64 a) => a -> Expectation
            roundtrip a = toBase64 a `shouldBe` (toBase64 . fromJust . fromBase64 @a . toBase64) a
        roundtrip $ Vodozemac.Olm.Account.curve25519Key account
        roundtrip $ Vodozemac.Olm.Account.ed25519Key account
        roundtrip $ Vodozemac.Olm.Account.sign account "junk"

    it "generates fallback keys" do
        account <- Vodozemac.Olm.Account.newAccount
        void $ Vodozemac.Olm.Account.generateFallbackKey account
        Just (keyId1, pubKey1) <- Vodozemac.Olm.Account.fallbackKey account
        Just (keyId2, pubKey2) <- Vodozemac.Olm.Account.fallbackKey account
        keyId1 `shouldBeOverB64` keyId2
        pubKey1 `shouldBeOverB64` pubKey2
        void $ Vodozemac.Olm.Account.generateFallbackKey account
        Just (keyId3, pubKey3) <- Vodozemac.Olm.Account.fallbackKey account
        Just (keyId4, pubKey4) <- Vodozemac.Olm.Account.fallbackKey account
        keyId3 `shouldBeOverB64` keyId4
        pubKey3 `shouldBeOverB64` pubKey4
        keyId1 `shouldNotBeOverB64` keyId3
        pubKey1 `shouldNotBeOverB64` pubKey3

    it "olms" do
        sender <- Vodozemac.Olm.Account.newAccount
        receiver <- Vodozemac.Olm.Account.newAccount
        let senderIdentityKey = Vodozemac.Olm.Account.curve25519Key sender
        let receiverIdentityKey = Vodozemac.Olm.Account.curve25519Key receiver

        ([receiverOneTimeKey], []) <- Vodozemac.Olm.Account.generateOneTimeKeys receiver 1
        void $ Vodozemac.Olm.Account.generateFallbackKey receiver
        Just (_, receiverFallbackKey) <- Vodozemac.Olm.Account.fallbackKey receiver
        Vodozemac.Olm.Account.markKeysAsPublished receiver

        let testSessionWith key = do
                let senderSession = Vodozemac.Olm.Account.createOutboundSession sender receiverIdentityKey key
                let expectedPlaintext = "Hello!" :: ByteString
                PreKeyMessage preKeyMessage <- Vodozemac.Olm.Session.encrypt senderSession expectedPlaintext

                Just (receiverSession, actualPlaintext) <- Vodozemac.Olm.Account.createInboundSession receiver senderIdentityKey preKeyMessage
                actualPlaintext `shouldBe` expectedPlaintext

                let expectedPlaintext = "Goodbye ..." :: ByteString
                message <- Vodozemac.Olm.Session.encrypt receiverSession expectedPlaintext
                Just actualPlaintext <- Vodozemac.Olm.Session.decrypt senderSession message
                actualPlaintext `shouldBe` expectedPlaintext

                let expectedPlaintext = "Just one more thing" :: ByteString
                message <- Vodozemac.Olm.Session.encrypt senderSession expectedPlaintext
                Just actualPlaintext <- Vodozemac.Olm.Session.decrypt receiverSession message
                actualPlaintext `shouldBe` expectedPlaintext

        testSessionWith receiverOneTimeKey
        testSessionWith receiverOneTimeKey `shouldThrow` \(_ :: SomeException) -> True
        testSessionWith receiverFallbackKey
        testSessionWith receiverFallbackKey

    it "megolms" do
        senderSession <- Vodozemac.Megolm.GroupSession.newGroupSession
        sessionKey <- Vodozemac.Megolm.GroupSession.sessionKey senderSession
        receiverSession <- Vodozemac.Megolm.InboundGroupSession.newInboundGroupSession sessionKey
        let expectedPlaintext = "I am the walrus" :: ByteString
        message <- Vodozemac.Megolm.GroupSession.encrypt senderSession expectedPlaintext
        Just (actualPlaintext, messageIndex) <- Vodozemac.Megolm.InboundGroupSession.decrypt receiverSession message
        actualPlaintext `shouldBe` expectedPlaintext
        messageIndex `shouldBe` 0
