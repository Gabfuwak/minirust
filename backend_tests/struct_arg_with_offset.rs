struct S { a: i32, b: i32 }
fn test(s: S) -> i32 { s.b }

fn main() -> i32 {
    test(S { a: 10i32, b: 20i32 })
}
