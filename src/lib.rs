use vodozemac::olm::Account;
use vodozemac::KeyId;
use vodozemac::Curve25519PublicKey;
use std::os::raw::c_char;

#[no_mangle]
pub extern "C" fn rust_hello(v: i32) -> i32 {
    println!("Hello Rust World: {}", v);
    v + 1
}

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
        panic!("fallback_key: empty map");
    }
    if keys.len() > 1 {
        panic!("fallback_key: map has more than one element");
    }

    let (key_id, public_key) = keys.into_iter().collect::<Vec<_>>()[0];

    let key_id = Box::into_raw(Box::new(key_id));
    let public_key = Box::into_raw(Box::new(public_key));
    Box::into_raw(Box::new(FallbackKey { key_id, public_key }))
}

#[no_mangle]
pub extern "C" fn mark_keys_as_published(ptr: *mut Account) {
    let acc: &mut Account = unsafe { &mut *ptr };
    acc.mark_keys_as_published()
}

#[no_mangle]
pub extern "C" fn keyid_to_base64(ptr: *mut KeyId, str: *mut c_char, len: usize) {
    let key_id: &mut KeyId = unsafe { &mut *ptr };
    let b64: String = key_id.to_base64();
    assert!(b64.len() + 1 <= len, "Not enough memory to copy str");
    assert_eq!(std::mem::size_of::<c_char>(), 1);
    unsafe {
        let b64_ptr = b64.as_bytes().as_ptr();
        std::ptr::copy_nonoverlapping(b64_ptr, str.cast(), b64.len());
    }
}
