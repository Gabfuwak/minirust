struct S1 {
    s : i32,
    t : i32,
}

fn f(x : S1) -> i32 {
    x.s
}

fn g(x : S1) -> i32 {
    x.t
}

fn main() -> i32 {
    f(S1 { s: 42i32, t: 24i32 })
}
