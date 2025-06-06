struct Inner { val: i32 }
struct Outer { inner: Inner, extra: i32 }
fn test() -> i32 { 
    let o: Outer = Outer { inner: Inner { val: 42i32 }, extra: 10i32 };
    o.inner.val 
}
