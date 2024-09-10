
fn print(str: &[char]) {
    // @asm {
    //     ; rsi -- [] char
    //     ; rdx -- buf_len

    //     mov rsi, [rsp+48]
    //     mov rdx, [rsp+56]

    //     mov rax, 1          ; write
    //     mov rdi, 1          ; to stdout
    //     syscall
    // }
}

fn add(x: i32, y: i32) :: i32 {
    return x + y;
}

fn main() :: i32 {
    let test: [10] char;

    print(&test);

    let x = 10i32;
    let y = 2i32;
    return add(x, y);
}
