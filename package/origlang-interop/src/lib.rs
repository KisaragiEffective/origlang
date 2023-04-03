use js_sys::JsString;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen::prelude::wasm_bindgen;
use origlang_compiler::parser::Parser;
use origlang_runtime::{OutputAccumulator, Runtime, TypeBox};

#[wasm_bindgen]
extern {
    fn get_source() -> JsValue;

    fn set_compile_error(s: JsString);

    fn echo(s: JsString);
}

#[wasm_bindgen]
pub fn init_panic_hook() {
    console_error_panic_hook::set_once();
}

#[derive(Debug)]
struct PseudoStdout;

impl OutputAccumulator for PseudoStdout {
    fn output(&mut self, tb: TypeBox) {
        let s: String = match tb {
            TypeBox::NonCoercedInteger(x) => x.to_string(),
            TypeBox::Int8(i) => i.to_string(),
            TypeBox::Int16(i) => i.to_string(),
            TypeBox::Int32(i) => i.to_string(),
            TypeBox::Int64(i) => i.to_string(),
            TypeBox::Boolean(i) => i.to_string(),
            TypeBox::String(s) => s,
            TypeBox::Unit(u) => "()".to_string(),
            TypeBox::Tuple(t) => t.to_string(),
        };

        echo(JsString::from(s))
    }

    fn acc(&self) -> Option<Vec<TypeBox>> {
        None
    }
}

#[wasm_bindgen]
pub fn run() {
    init_panic_hook();

    let src = get_source();
    if src.is_string() {
        let src = src.dyn_ref::<JsString>().expect("this should never fail");
        let src = src.as_string().expect("Source code must not contain invalid surrogate codepoint");
        let parser = Parser::create(&src);
        match parser.parse() {
            Ok(ast) => {
                let runtime = Runtime::create(PseudoStdout);
                let instructions = runtime.what_will_happen(ast);
                for instruction in instructions {
                    instruction.invoke(&runtime);
                }
            }
            Err(e) => {
                set_compile_error(JsString::from(e.to_string()));
            }
        }
    } else {
        panic!("get_source implementation did not return string, this is IMPLEMENTATION BUG")
    }
}

#[wasm_bindgen]
pub fn xyzpris() {}
