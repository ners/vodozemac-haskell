{-# OPTIONS_GHC -Wno-name-shadowing #-}

module VodozemacSpec where

import Data.Functor (void)
import Test.Hspec (Spec, it)
import Vodozemac qualified
import Prelude

spec :: Spec
spec = it "works" do
    account <- Vodozemac.newAccount
    void $ Vodozemac.generateFallbackKey account
    Just (Vodozemac.FallbackKey keyId _) <- Vodozemac.fallbackKey account
    Vodozemac.keyIdToBase64 keyId >>= putStrLn
    Just (Vodozemac.FallbackKey keyId _) <- Vodozemac.fallbackKey account
    Vodozemac.keyIdToBase64 keyId >>= putStrLn
    void $ Vodozemac.generateFallbackKey account
    Just (Vodozemac.FallbackKey keyId _) <- Vodozemac.fallbackKey account
    Vodozemac.keyIdToBase64 keyId >>= putStrLn
    Just (Vodozemac.FallbackKey keyId _) <- Vodozemac.fallbackKey account
    Vodozemac.keyIdToBase64 keyId >>= putStrLn
    Vodozemac.freeAccount account
