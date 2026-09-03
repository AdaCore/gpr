use std::os::raw::c_int;

unsafe extern "C" {
    fn rust_lib_double(x: c_int) -> c_int;
}

fn main() {
    let result = unsafe { rust_lib_double(21) };
    println!("Calling a Rust library from a Rust executable!");
    println!("21 * 2 = {}", result);
}
