unsafe extern "C" {
    fn ada_result() -> i32;
}

fn main() {
    println!("{}", unsafe { ada_result() });
}
