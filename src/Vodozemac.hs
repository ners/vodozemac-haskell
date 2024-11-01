module Vodozemac where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Foreign (callocBytes, castPtr, nullPtr, plusPtr)
import Foreign.Storable
import Prelude
import Vodozemac.Raw.Account qualified as Raw (Account)
import Vodozemac.Raw.Account qualified as Raw.Account
import Vodozemac.Raw.Curve25519PublicKey qualified as Raw (Curve25519PublicKey)
import Vodozemac.Raw.Curve25519PublicKey qualified as Raw.Curve25519PublicKey
import Vodozemac.Raw.Ed25519PublicKey qualified as Raw (Ed25519PublicKey)
import Vodozemac.Raw.Ed25519PublicKey qualified as Raw.Ed25519PublicKey
import Vodozemac.Raw.Ed25519Signature qualified as Raw (Ed25519Signature)
import Vodozemac.Raw.Ed25519Signature qualified as Raw.Ed25519Signature
import Vodozemac.Raw.KeyId qualified as Raw (KeyId)
import Vodozemac.Raw.KeyId qualified as Raw.KeyId
import Vodozemac.Raw.Util qualified as Raw

newtype Account = Account Raw.Account

newAccount :: IO Account
newAccount = Account <$> Raw.Account.new

freeAccount :: Account -> IO ()
freeAccount (Account account) = Raw.Account.free account

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

instance Storable Ed25519PublicKey where
    sizeOf _ = Raw.ptrSize
    alignment _ = Raw.ptrAlignment
    peek ptr = Ed25519PublicKey <$> peek (castPtr ptr)
    poke ptr (Ed25519PublicKey key) = poke (castPtr ptr) key

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
    ptr <- Raw.Account.generate_fallback_key account
    pure $ if ptr == nullPtr then Nothing else Just (Curve25519PublicKey ptr)

fallbackKey :: Account -> IO (Maybe FallbackKey)
fallbackKey (Account account) = do
    ptr <- Raw.Account.fallback_key account
    if ptr == nullPtr then pure Nothing else Just <$> peek (castPtr ptr)

curve25519Key :: Account -> IO (Maybe Curve25519PublicKey)
curve25519Key (Account account) = do
    ptr <- Raw.Account.curve25519_key account
    pure $ if ptr == nullPtr then Nothing else Just (Curve25519PublicKey ptr)

ed25519Key :: Account -> IO (Maybe Ed25519PublicKey)
ed25519Key (Account account) = do
    ptr <- Raw.Account.ed25519_key account
    pure $ if ptr == nullPtr then Nothing else Just (Ed25519PublicKey ptr)

keyIdToBase64 :: KeyId -> IO String
keyIdToBase64 (KeyId keyId) = do
    let maxLen = 16 :: Int
    strPtr <- callocBytes maxLen
    Raw.KeyId.to_base64 keyId strPtr (fromIntegral maxLen)
    Raw.peekAndFreeCString strPtr

curve25519PublicKeyToBase64 :: Curve25519PublicKey -> IO String
curve25519PublicKeyToBase64 (Curve25519PublicKey key) = do
    let maxLen = 64 :: Int
    strPtr <- callocBytes maxLen
    Raw.Curve25519PublicKey.to_base64 key strPtr (fromIntegral maxLen)
    Raw.peekAndFreeCString strPtr

newtype Ed25519PublicKey = Ed25519PublicKey Raw.Ed25519PublicKey

ed25519PublicKeyToBase64 :: Ed25519PublicKey -> IO String
ed25519PublicKeyToBase64 (Ed25519PublicKey key) = do
    let maxLen = 64 :: Int
    strPtr <- callocBytes maxLen
    Raw.Ed25519PublicKey.to_base64 key strPtr (fromIntegral maxLen)
    Raw.peekAndFreeCString strPtr

newtype Ed25519Signature = Ed25519Signature Raw.Ed25519Signature

ed25519SignatureToBase64 :: Ed25519Signature -> IO String
ed25519SignatureToBase64 (Ed25519Signature sig) = do
    let maxLen = 128 :: Int
    strPtr <- callocBytes maxLen
    Raw.Ed25519Signature.to_base64 sig strPtr (fromIntegral maxLen)
    Raw.peekAndFreeCString strPtr

sign :: Account -> ByteString -> IO Ed25519Signature
sign (Account account) bs = Ed25519Signature <$> ByteString.unsafeUseAsCStringLen bs (uncurry $ Raw.Account.sign account)
