fn test() -> i32 {
    let x: i32 = 42i32;
    let r1: &i32 = &x;
    let r2: &i32 = &(*r1);  // reborrow
    *r2
}

fn main() -> i32 { test() }
