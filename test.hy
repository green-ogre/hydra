fn add(x: i32, y: i32) :: i32 {
    return x + y;
}

fn main() :: i32 {
    let arr: [10] char;
    print(&arr);

    let x = 10i32;
    let y = 2i32;
    return add(x, y);
}


struct :: Core {
    val: i32
}

#[derive(change_apple, default)]
struct :: Apple {
    core: Core,
    skin: u32,
}

fn change_apple(apple: Ast) {

}

fn default(ast: Ast) {

}


fn optional(val: ?i32) {
    if !val {
        return;
    }

    let non_zero_val;

    *val += 12;

    if val {
        *val += 12;
    }

    return;
}
