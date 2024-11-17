module Vodozemac
    ( module Vodozemac.Curve25519PublicKey
    , module Vodozemac.Ed25519PublicKey
    , module Vodozemac.Ed25519Signature
    , module Vodozemac.KeyId
    , module Vodozemac.Olm.Account
    , module Vodozemac.Olm.Session
    , module Vodozemac.Util
    , module Vodozemac
    )
where

import Language.Rust.Inline
import Vodozemac.Curve25519PublicKey (Curve25519PublicKey)
import Vodozemac.Ed25519PublicKey (Ed25519PublicKey)
import Vodozemac.Ed25519Signature (Ed25519Signature)
import Vodozemac.KeyId (KeyId)
import Vodozemac.Olm.Account (Account)
import Vodozemac.Olm.Session (Session)
import Vodozemac.Util (FromBase64 (..), ToBase64 (..))

extendContext basic
setCrateRoot
    [ ("vodozemac", "0.8.1")
    , ("serde_json", "1.0.132")
    ]
