use std::os::raw::c_int;

unsafe extern "C" {
    fn ada_add(a: c_int, b: c_int) -> c_int;
}

#[unsafe(no_mangle)]
pub extern "C" fn greet_from_rust() {
    let result = unsafe { ada_add(10, 32) };
    println!("hello from Rust, calling Ada: 10 + 32 = {}", result);
}
