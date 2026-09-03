use std::os::raw::c_int;

unsafe extern "C" {
    fn ada_compute(a: c_int, b: c_int) -> c_int;
}

fn main() {
    println!("ada_compute (10, 32) = {}", unsafe { ada_compute(10, 32) });
}
