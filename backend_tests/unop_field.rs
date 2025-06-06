struct S { val: i32 }
fn test() -> i32 {
    let s: S = S { val: 42i32 };
    -s.val
}

fn main() -> i32 { test() }
