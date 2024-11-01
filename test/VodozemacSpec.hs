{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module VodozemacSpec where

import Data.Functor (void)
import Test.Hspec (Spec, it)
import Vodozemac qualified
import Prelude
import Data.Maybe (fromJust)

spec :: Spec
spec = do
    it "works" do
        account <- Vodozemac.newAccount
        void $ Vodozemac.generateFallbackKey account
        Just (Vodozemac.FallbackKey keyId pubKey) <- Vodozemac.fallbackKey account
        putStrLn =<< Vodozemac.keyIdToBase64 keyId
        putStrLn =<< Vodozemac.curve25519PublicKeyToBase64 pubKey
        Just (Vodozemac.FallbackKey keyId pubKey) <- Vodozemac.fallbackKey account
        putStrLn =<< Vodozemac.keyIdToBase64 keyId
        putStrLn =<< Vodozemac.curve25519PublicKeyToBase64 pubKey
        void $ Vodozemac.generateFallbackKey account
        Just (Vodozemac.FallbackKey keyId pubKey) <- Vodozemac.fallbackKey account
        putStrLn =<< Vodozemac.keyIdToBase64 keyId
        putStrLn =<< Vodozemac.curve25519PublicKeyToBase64 pubKey
        Just (Vodozemac.FallbackKey keyId pubKey) <- Vodozemac.fallbackKey account
        putStrLn =<< Vodozemac.keyIdToBase64 keyId
        putStrLn =<< Vodozemac.curve25519PublicKeyToBase64 pubKey
        Vodozemac.freeAccount account
    it "signs" do
        account <- Vodozemac.newAccount
        signature <- Vodozemac.sign account "Hello world!"
        putStrLn =<< Vodozemac.ed25519SignatureToBase64 signature
        Vodozemac.freeAccount account
    it "has keys" do
        account <- Vodozemac.newAccount
        print =<< Vodozemac.curve25519PublicKeyToBase64 . fromJust =<< Vodozemac.curve25519Key account
        print =<< Vodozemac.ed25519PublicKeyToBase64 . fromJust =<< Vodozemac.ed25519Key account
        Vodozemac.freeAccount account
