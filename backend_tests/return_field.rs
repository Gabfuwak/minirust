struct S { val: i32 }
fn test(s: S) -> i32 {
    s.val
}

fn main() -> i32 {
    test(S { val: 99i32 })
}
