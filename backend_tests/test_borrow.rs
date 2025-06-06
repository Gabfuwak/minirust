fn test_simple_borrow() -> i32 {
    let x : i32 = 42i32;
    let r : &i32 = &x;
    *r
}
