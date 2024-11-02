use std::ffi::CString;
use std::os::raw::c_char;
use vodozemac::olm::Account;
use vodozemac::*;

fn copy_str(source: String) -> *mut c_char {
    CString::new(source.into_bytes()).unwrap().into_raw()
}

#[no_mangle]
pub extern "C" fn free_cstring(ptr: *mut c_char) {
    let cstring = unsafe { CString::from_raw(ptr) };
    drop(cstring)
}

// Box<[u8]>
#[no_mangle]
pub extern "C" fn free_bytestring(ptr: *mut u8, size: usize) {
    let bytes = unsafe { Box::from_raw(std::ptr::slice_from_raw_parts_mut(ptr, size)) };
    drop(bytes)
}

/*
 * Account
 */

#[no_mangle]
pub extern "C" fn new_account() -> *mut Account {
    Box::into_raw(Box::new(Account::new()))
}

#[no_mangle]
pub extern "C" fn free_account(ptr: *mut Account) {
    let acc: Box<Account> = unsafe { Box::from_raw(ptr) };
    drop(acc);
}

#[no_mangle]
pub extern "C" fn mark_keys_as_published(ptr: *mut Account) {
    let acc: &mut Account = unsafe { &mut *ptr };
    acc.mark_keys_as_published()
}

#[repr(C)]
pub struct FallbackKey {
    key_id: *mut KeyId,
    public_key: *mut Curve25519PublicKey,
}

#[no_mangle]
pub extern "C" fn generate_fallback_key(ptr: *mut Account) -> *mut Curve25519PublicKey {
    let acc: &mut Account = unsafe { &mut *ptr };
    match acc.generate_fallback_key() {
        None => std::ptr::null_mut(),
        Some(key) => Box::into_raw(Box::new(key)),
    }
}

#[no_mangle]
pub extern "C" fn fallback_key(ptr: *mut Account) -> *mut FallbackKey {
    let acc: &mut Account = unsafe { &mut *ptr };
    let keys = acc.fallback_key();
    if keys.is_empty() {
        return std::ptr::null_mut();
    }

    let (key_id, public_key) = keys.into_iter().collect::<Vec<_>>()[0];

    let key_id = Box::into_raw(Box::new(key_id));
    let public_key = Box::into_raw(Box::new(public_key));
    Box::into_raw(Box::new(FallbackKey { key_id, public_key }))
}

#[no_mangle]
pub extern "C" fn sign(ptr: *const Account, data: *const u8, len: usize) -> *mut Ed25519Signature {
    let acc: &Account = unsafe { &*ptr };
    let message: &[u8] = unsafe { &*std::slice::from_raw_parts(data, len) };
    let signed_message = acc.sign(message);
    Box::into_raw(Box::new(signed_message))
}

#[no_mangle]
pub extern "C" fn curve25519_key(ptr: *mut Account) -> *mut Curve25519PublicKey {
    let acc: &mut Account = unsafe { &mut *ptr };
    let key = acc.curve25519_key();
    Box::into_raw(Box::new(key))
}

#[no_mangle]
pub extern "C" fn ed25519_key(ptr: *mut Account) -> *mut Ed25519PublicKey {
    let acc: &mut Account = unsafe { &mut *ptr };
    let key = acc.ed25519_key();
    Box::into_raw(Box::new(key))
}

#[no_mangle]
pub extern "C" fn create_outbound_session(
    ptr: *const Account,
    identity_key_ptr: *const Curve25519PublicKey,
    one_time_key_ptr: *const Curve25519PublicKey,
) -> *mut olm::Session {
    let acc: &Account = unsafe { &*ptr };
    let identity_key = unsafe { &*identity_key_ptr };
    let one_time_key = unsafe { &*one_time_key_ptr };
    let session = acc.create_outbound_session(
        olm::SessionConfig::version_1(),
        *identity_key,
        *one_time_key,
    );
    Box::into_raw(Box::new(session))
}

#[no_mangle]
pub extern "C" fn create_inbound_session(
    ptr: *mut Account,
    their_identity_key_ptr: *const Curve25519PublicKey,
    prekey_message_data: *const u8,
    prekey_message_len: usize,
    plaintext_len_ptr: *mut usize,
    plaintext_data_ptr: *mut *mut u8,
) -> *mut olm::Session {
    let acc: &mut Account = unsafe { &mut *ptr };
    let their_identity_key = unsafe { &*their_identity_key_ptr };
    let prekey_message_json =
        unsafe { std::slice::from_raw_parts(prekey_message_data, prekey_message_len) };
    let Ok(olm::OlmMessage::PreKey(prekey_message)) = serde_json::from_slice(prekey_message_json)
    else {
        return std::ptr::null_mut();
    };

    let session = acc.create_inbound_session(*their_identity_key, &prekey_message);
    match session {
        Err(_) => std::ptr::null_mut(),
        Ok(olm::InboundCreationResult { session, plaintext }) => {
            let plaintext_slice = Box::leak(plaintext.into_boxed_slice());
            unsafe {
                *plaintext_len_ptr = plaintext_slice.len();
                *plaintext_data_ptr = plaintext_slice.as_mut_ptr();
            }
            Box::into_raw(Box::new(session))
        }
    }
}

/*
 * Curve25519PublicKey
 */

#[no_mangle]
pub extern "C" fn keyid_to_base64(ptr: *mut KeyId) -> *mut c_char {
    let key_id: &mut KeyId = unsafe { &mut *ptr };
    copy_str(key_id.to_base64())
}

#[no_mangle]
pub extern "C" fn curve25519publickey_to_base64(ptr: *mut Curve25519PublicKey) -> *mut c_char {
    let key: &mut Curve25519PublicKey = unsafe { &mut *ptr };
    copy_str(key.to_base64())
}

/*
 * Ed25519PublicKey
 */

#[no_mangle]
pub extern "C" fn ed25519publickey_to_base64(ptr: *mut Ed25519PublicKey) -> *mut c_char {
    let key: &mut Ed25519PublicKey = unsafe { &mut *ptr };
    copy_str(key.to_base64())
}

/*
 * Ed25519Signature
 */

#[no_mangle]
pub extern "C" fn ed25519signature_to_base64(ptr: *mut Ed25519Signature) -> *mut c_char {
    let sig: &mut Ed25519Signature = unsafe { &mut *ptr };
    copy_str(sig.to_base64())
}

/*
 * Olm Session
 */

#[no_mangle]
pub extern "C" fn olm_session_id(ptr: *const olm::Session) -> *mut c_char {
    let sess: &olm::Session = unsafe { &*ptr };
    copy_str(sess.session_id())
}

#[no_mangle]
pub extern "C" fn olm_session_has_received_message(ptr: *const olm::Session) -> bool {
    let sess: &olm::Session = unsafe { &*ptr };
    sess.has_received_message()
}

#[no_mangle]
pub extern "C" fn olm_session_encrypt_to_json(
    ptr: *mut olm::Session,
    data: *const u8,
    len: usize,
) -> *mut c_char {
    let sess: &mut olm::Session = unsafe { &mut *ptr };
    let plaintext = unsafe { std::slice::from_raw_parts(data, len) };
    let message = sess.encrypt(plaintext);
    copy_str(serde_json::to_string(&message).unwrap())
}

#[no_mangle]
pub extern "C" fn olm_session_decrypt_json(
    ptr: *mut olm::Session,
    data: *const u8,
    len: usize,
    plaintext_len_ptr: *mut usize,
) -> *mut u8 {
    let sess: &mut olm::Session = unsafe { &mut *ptr };
    let ciphertext_json = unsafe { std::slice::from_raw_parts(data, len) };
    let Ok(ciphertext) = serde_json::from_slice(ciphertext_json) else {
        return std::ptr::null_mut();
    };
    let res = sess.decrypt(&ciphertext);
    match res {
        Err(_) => std::ptr::null_mut(),
        Ok(plaintext) => {
            let plaintext_slice = Box::leak(plaintext.into_boxed_slice());
            unsafe {
                *plaintext_len_ptr = plaintext_slice.len();
            }
            plaintext_slice.as_mut_ptr()
        }
    }
}
