struct S { a: i32, b: i32 }
fn test() -> i32 {
    let s: S = S { a: 10i32, b: 20i32 };
    let r: &S = &s;
    (*r).a + (*r).b
}

fn main() -> i32 { test() }
