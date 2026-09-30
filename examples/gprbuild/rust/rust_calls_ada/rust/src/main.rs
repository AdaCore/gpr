use std::os::raw::c_int;

unsafe extern "C" {
    fn ada_add(a: c_int, b: c_int) -> c_int;
}

fn main() {
    let result = unsafe { ada_add(10, 32) };
    println!("Calling encapsulated Ada library from Rust!");
    println!("10 + 32 = {}", result);
}
