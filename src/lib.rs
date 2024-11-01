use std::os::raw::c_char;
use vodozemac::*;
use vodozemac::olm::Account;

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
pub extern "C" fn generate_fallback_key(ptr: *mut Account) -> *mut Curve25519PublicKey {
    let acc: &mut Account = unsafe { &mut *ptr };
    match acc.generate_fallback_key() {
        None => std::ptr::null_mut(),
        Some(key) => Box::into_raw(Box::new(key)),
    }
}

#[repr(C)]
pub struct FallbackKey {
    key_id: *mut KeyId,
    public_key: *mut Curve25519PublicKey,
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
pub extern "C" fn mark_keys_as_published(ptr: *mut Account) {
    let acc: &mut Account = unsafe { &mut *ptr };
    acc.mark_keys_as_published()
}

fn copy_str(source: String, target: *mut c_char, target_len: usize) {
    assert!(source.len() + 1 <= target_len, "Not enough memory to copy str");
    assert_eq!(std::mem::size_of::<c_char>(), 1);
    unsafe {
        std::ptr::copy_nonoverlapping(source.as_bytes().as_ptr(), target.cast(), source.len());
    }
}

#[no_mangle]
pub extern "C" fn keyid_to_base64(ptr: *mut KeyId, str: *mut c_char, len: usize) {
    let key_id: &mut KeyId = unsafe { &mut *ptr };
    let b64: String = key_id.to_base64();
    copy_str(b64, str, len);
}

#[no_mangle]
pub extern "C" fn curve25519publickey_to_base64(ptr: *mut Curve25519PublicKey, str: *mut c_char, len: usize) {
    let key: &mut Curve25519PublicKey = unsafe { &mut *ptr };
    let b64: String = key.to_base64();
    copy_str(b64, str, len);
}

#[no_mangle]
pub extern "C" fn ed25519publickey_to_base64(ptr: *mut Ed25519PublicKey, str: *mut c_char, len: usize) {
    let key: &mut Ed25519PublicKey = unsafe { &mut *ptr };
    let b64: String = key.to_base64();
    copy_str(b64, str, len);
}

#[no_mangle]
pub extern "C" fn ed25519signature_to_base64(ptr: *mut Ed25519Signature, str: *mut c_char, len: usize) {
    let sig: &mut Ed25519Signature = unsafe { &mut *ptr };
    let b64: String = sig.to_base64();
    copy_str(b64, str, len);
}

#[no_mangle]
pub extern "C" fn sign(ptr: *mut Account, data: *const u8, len: usize) -> *mut Ed25519Signature {
    let acc: &mut Account = unsafe { &mut *ptr };
    let message = unsafe { std::slice::from_raw_parts(data, len) };
    Box::into_raw(Box::new(acc.sign(message)))
}
