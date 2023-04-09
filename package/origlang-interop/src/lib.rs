#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use js_sys::JsString;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen::prelude::wasm_bindgen;
use web_sys::console;
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
            TypeBox::Unit => "()".to_string(),
            TypeBox::Tuple(t) => t.to_string(),
        };

        echo(JsString::from(s))
    }

    fn acc(&self) -> Option<Vec<TypeBox>> {
        None
    }
}

struct Timer<'a> {
    name: &'a str
}

impl<'a> Timer<'a> {
    fn new(name: &'a str) -> Self {
        console::time_with_label(name);
        Self { name }
    }
}

impl<'a> Drop for Timer<'a> {
    fn drop(&mut self) {
        console::time_end_with_label(self.name);
    }
}

#[wasm_bindgen]
pub fn run() {
    let _ = Timer::new("entire");
    let src = {
        let _ = Timer::new("init");
        init_panic_hook();
        get_source()
    };
    if let Some(src) = src.dyn_ref::<JsString>() {
        let src = src.as_string().expect("Source code must not contain invalid surrogate codepoint");
        let parser = {
            let _ = Timer::new("parse.construction");
            Parser::create(&src)
        };
        let res = {
            let _ = Timer::new("parse");
            parser.parse()
        };
        match res {
            Ok(ast) => {
                let _ = Timer::new("runtime");
                let runtime = {
                    let _ = Timer::new("runtime.construction");
                    Runtime::create(PseudoStdout)
                };
                let instructions = {
                    let _ = Timer::new("runtime.intermediate");
                    runtime.what_will_happen(ast)
                };
                let _ = Timer::new("runtime.execution");
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
