module Vodozemac where

import Language.Rust.Inline

import Vodozemac.Curve25519PublicKey (Curve25519PublicKey)

extendContext basic
setCrateRoot
    [ ("vodozemac", "0.8.1")
    , ("serde_json", "1.0.132")
    ]
