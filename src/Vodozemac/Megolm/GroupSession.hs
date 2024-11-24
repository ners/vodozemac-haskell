module Vodozemac.Megolm.GroupSession where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Foreign
import Language.Rust.Inline
import Language.Rust.Quote (ty)
import Vodozemac.Megolm.SessionKey (SessionKey (..), SessionKey')
import Prelude

data GroupSession'

newtype GroupSession = GroupSession (ForeignPtr GroupSession')

setCrateModule

[rust|
use vodozemac::*;
use vodozemac::megolm::*;
|]

extendContext basic
extendContext ffi
extendContext functions
extendContext foreignPointers
extendContext bytestrings
extendContext prelude
extendContext (singleton [ty| SessionKey |] [t|SessionKey'|])
extendContext (singleton [ty| GroupSession |] [t|GroupSession'|])

newGroupSession :: IO GroupSession
newGroupSession = [rustIO| ForeignPtr<GroupSession> { Box::new(megolm::GroupSession::new(megolm::SessionConfig::version_1())).into() } |] <&> GroupSession

{- | Returns the globally unique session ID, in base64-encoded form.
A session ID is the public part of the Ed25519 key pair associated with the group session. Due to the construction, every session ID is (probabilistically) globally unique.
-}
sessionId :: GroupSession -> ByteString
sessionId (GroupSession sess) = [rust| Vec<u8> { $(sess: &GroupSession).session_id().into_bytes() } |]

{- | Export the group session into a session key.
The session key contains the key version constant, the current message index, the ratchet state and the public part of the signing key pair. It is signed by the signing key pair for authenticity.
The session key is in a portable format, suitable for sending over the network. It is typically sent to other group participants so that they can reconstruct an inbound group session in order to decrypt messages sent by this group session.
-}
sessionKey :: GroupSession -> IO SessionKey
sessionKey (GroupSession sess) =
    [rustIO| ForeignPtr<SessionKey> {
        Box::new($(sess: &GroupSession).session_key()).into()
    } |]
        <&> SessionKey

encrypt :: GroupSession -> ByteString -> IO ByteString
encrypt (GroupSession sess) plaintext =
    [rustIO| Vec<u8> {
        $(sess: &mut GroupSession).encrypt($(plaintext: &[u8])).to_bytes()
    } |]
