struct S { a: i32, b: i32 }
fn test() -> i32 {
    let s: S = S { a: 10i32, b: 20i32 };
    let x: i32 = s.a;
    x
}

fn main() -> i32 { test() }
