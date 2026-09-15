use std::os::raw::c_double;

unsafe extern "C" {
    fn ada_root(x: c_double) -> c_double;
}

fn main() {
    println!("ada_root (1764.0) = {}", unsafe { ada_root(1764.0) });
}
