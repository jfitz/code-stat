// #![allow(unused)]

fn main() {
    let string1 = r#"""#; // " 
    let string2 = r#""""""""#; // """"""
    let string3 = r#"He asked,"Is rust awesome?""#; // He asked,"Is rust awesome?" 
    println!("{}", string1);
    println!("{}", string2);
    println!("{}", string3);
}