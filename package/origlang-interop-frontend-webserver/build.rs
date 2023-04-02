use std::fs::{copy as copy_file, read_dir};
use std::path::PathBuf;

fn main() {
    let cd = std::env::current_dir().expect("<fail>");
    println!("cd: {}", cd.display());

    let interop_dir = cd.parent().expect("no parent").join("origlang-interop").join("pkg");
    println!("interop_dir: {}", interop_dir.display());

    for interop_file in read_dir(interop_dir).expect("fail") {
        let x = interop_file.expect("1");
        let from = x.path();
        let name = x.file_name();
        let common_parent = from.iter().take(from.components().count() - 3).collect::<PathBuf>();
        println!("cpar: {}", common_parent.display());

        let to = common_parent
            .join("origlang-interop-frontend-webserver")
            .join("www")
            .join(name);

        println!("copy: {from}", from = &from.display());
        println!("copy: {to}", to = &to.display());
        copy_file(from, to).expect("copy failed");
    }
}