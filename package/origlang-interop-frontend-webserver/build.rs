use std::fs::{copy as copy_file, read_dir};
use std::path::PathBuf;

fn main() {
    let cd = std::env::current_dir().expect("<fail>");
    println!("cd: {}", cd.display());

    let www = cd.join("www");

    let interop_dir = cd
        .parent()
        .expect("no parent")
        .join("origlang-interop")
        .join("pkg");
    println!("interop_dir: {}", interop_dir.display());

    for interop_file in read_dir(interop_dir).expect("fail") {
        let x = interop_file.expect("1");
        let from = x.path();
        let name = x.file_name();
        if name == *".gitignore" {
            continue;
        }
        let common_parent = from
            .iter()
            .take(from.components().count() - 3)
            .collect::<PathBuf>();
        println!("cpar: {}", common_parent.display());

        let to = www.join(name);

        println!("copy: {from}", from = &from.display());
        println!("copy: {to}", to = &to.display());
        copy_file(from, to).expect("copy failed, please build origlang-interop first.");
    }

    let useful_example_dir = cd
        .parent()
        .expect("parent")
        .parent()
        .expect("parent.parent")
        .join("compile")
        .join("positive")
        .join("non-perf");

    let www_example_dir = www.join("example");
    if !www_example_dir.exists() {
        std::fs::create_dir_all(&www_example_dir).expect("www/example");
    }
    std::fs::copy(
        useful_example_dir.join("99_bottles_of_beer.origlang"),
        www_example_dir.join("99_bottles_of_beer.origlang"),
    )
    .expect("99 bottles");
    std::fs::copy(
        useful_example_dir.join("hello_world.origlang"),
        www_example_dir.join("hello_world.origlang"),
    )
    .expect("Hello world");
    std::fs::copy(
        useful_example_dir.join("fizz_buzz.origlang"),
        www_example_dir.join("fizz_buzz.origlang"),
    )
    .expect("Fizz Buzz");
}
