use std::os::raw::c_int;

#[unsafe(no_mangle)]
pub extern "C" fn rust_lib_double(x: c_int) -> c_int {
    x * 2
}
