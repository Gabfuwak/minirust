struct X<'a> {
    a: &'a Y<'a>
}

struct Y<'a> {
    val: X<'a>    
}

fn main() {
}
