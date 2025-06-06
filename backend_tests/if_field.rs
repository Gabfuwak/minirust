struct S { flag: bool }
fn test() -> i32 {
    let s: S = S { flag: true };
    if s.flag { 42i32 } else { 0i32 }
}

fn main() -> i32 { test() }
