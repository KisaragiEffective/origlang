#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use js_sys::JsString;
use thiserror::Error;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen::prelude::wasm_bindgen;
use web_sys::console;
use origlang_compiler::parser::{Parser, SimpleErrorWithPos};
use origlang_compiler::type_check::error::TypeCheckError;
use origlang_compiler::type_check::TypeChecker;
use origlang_ir::IntoVerbatimSequencedIR;
use origlang_ir_optimizer::lower::{LowerStep, TheTranspiler};
use origlang_ir_optimizer::preset::NoOptimization;
use origlang_runtime::{OutputAccumulator, Runtime, TypeBox};
use origlang_typesystem_model::TypedRootAst;

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
            TypeBox::Record(r) => r.to_string(),
        };

        echo(JsString::from(s));
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

#[derive(Error, Debug)]
enum ExecutionError {
    #[error("parser: {0}")]
    Parser(#[from] SimpleErrorWithPos),
    #[error("type: {0}")]
    Type(#[from] TypeCheckError),
}

/// # Panics
/// if [`get_source`] did not return [`JsString`].
#[wasm_bindgen]
pub fn run() {
    let _ = Timer::new("entire");
    let src = {
        let _ = Timer::new("init");
        init_panic_hook();
        get_source()
    };

    src.dyn_ref::<JsString>().map_or_else(
        || {
            panic!("get_source implementation did not return string, this is IMPLEMENTATION BUG")
        },
        |src| {
            let inner: fn(&JsString) -> Result<(), ExecutionError> = |src| {
                let src = src.as_string().expect("Source code must not contain invalid surrogate codepoint");
                let parser = {
                    let _ = Timer::new("parse.construction");
                    Parser::create(&src)
                };
                let res = {
                    let _ = Timer::new("parse");
                    parser.parse()?
                };
                let typed_root: TypedRootAst = {
                    let _ = Timer::new("typeck");
                    TypeChecker::new().check(res)?
                };
                {
                    let _ = Timer::new("runtime");
                    let runtime = {
                        let _ = Timer::new("runtime.construction");
                        Runtime::create(PseudoStdout)
                    };
                    let _ = Timer::new("runtime.ir");
                    let transpiler = TheTranspiler::new(&NoOptimization);
                    let lower = typed_root.into_ir();
                    let _ = Timer::new("runtime.ir.lower");
                    let lower = transpiler.lower(lower);
                    let _ = Timer::new("runtime.start");
                    runtime.start(&lower);
                }

                Ok(())
            };

            if let Err(e) = inner(src) {
                set_compile_error(JsString::from(e.to_string()));
            }
        }
    );
}
