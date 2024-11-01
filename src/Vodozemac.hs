{-# LANGUAGE Safe #-}

module Vodozemac where

import Control.Exception (bracket)
import Foreign (callocBytes, castPtr, free, nullPtr, plusPtr)
import Foreign.C (peekCString)
import Foreign.Storable
import Vodozemac.Raw qualified as Raw
import Prelude

newtype Account = Account Raw.Account

newAccount :: IO Account
newAccount = Account <$> Raw.new_account

freeAccount :: Account -> IO ()
freeAccount (Account account) = Raw.free_account account

withAccount :: (Account -> IO ()) -> IO ()
withAccount a = bracket newAccount a freeAccount

newtype KeyId = KeyId Raw.KeyId

instance Storable KeyId where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = KeyId <$> peek (castPtr ptr)
    poke ptr (KeyId keyId) = poke (castPtr ptr) keyId

newtype Curve25519PublicKey = Curve25519PublicKey Raw.Curve25519PublicKey

instance Storable Curve25519PublicKey where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = Curve25519PublicKey <$> peek (castPtr ptr)
    poke ptr (Curve25519PublicKey key) = poke (castPtr ptr) key

data FallbackKey = FallbackKey KeyId Curve25519PublicKey

instance Storable FallbackKey where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = FallbackKey <$> peek (castPtr ptr) <*> peek (castPtr ptr `plusPtr` Raw.ptrSize)
    poke ptr (FallbackKey keyId key) = do
        poke (castPtr ptr) keyId
        poke (castPtr $ ptr `plusPtr` Raw.ptrSize) key

generateFallbackKey :: Account -> IO (Maybe Curve25519PublicKey)
generateFallbackKey (Account account) = do
    keyPtr <- Raw.generate_fallback_key account
    if keyPtr == nullPtr then pure Nothing else Just <$> peek (castPtr keyPtr)

fallbackKey :: Account -> IO (Maybe FallbackKey)
fallbackKey (Account account) = do
    keyPtr <- Raw.fallback_key account
    if keyPtr == nullPtr then pure Nothing else Just <$> peek (castPtr keyPtr)

keyIdToBase64 :: KeyId -> IO String
keyIdToBase64 (KeyId keyId) = do
    let maxLen = 16 :: Int
    strPtr <- callocBytes maxLen
    Raw.keyid_to_base64 keyId strPtr (fromIntegral maxLen)
    str <- peekCString strPtr
    free strPtr
    pure str
