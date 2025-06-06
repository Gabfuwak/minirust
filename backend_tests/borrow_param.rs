fn get_value<'a>(r: &'a i32) -> i32 {
    *r
}

fn test() -> i32 {
    let x: i32 = 99i32;
    let r: &i32 = &x;
    get_value(r)
}

fn main() -> i32 { test() }
