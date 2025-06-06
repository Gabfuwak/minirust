struct S { a: i32, b: i32 }
fn test() -> i32 {
    let s: S = S { a: 10i32, b: 20i32 };
    if s.a > s.b { 1i32 } else { 0i32 }
}

fn main() -> i32 { test() }
