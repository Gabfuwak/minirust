struct S { val: i32 }
fn test() -> i32 {
    let s: S = S { val: 77i32 };
    let r1: &S = &s;
    let r2: &i32 = &(*r1).val;
    *r2
}

fn main() -> i32 { test() }
