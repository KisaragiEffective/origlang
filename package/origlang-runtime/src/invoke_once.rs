use std::cell::{Cell, RefCell};
use std::mem::{MaybeUninit};


#[derive(Debug)]
pub struct InvokeOnce<T> {
    // Miriでは検知できなかったが、おそらくT=i32などのときにMaybeUninit::uninit()
    // から連れてきたゴミを入れるとUBになるのでMaybeUninitで包む。
    inner: RefCell<MaybeUninit<T>>,
    owned: Cell<bool>,
}

impl<T> InvokeOnce<T> {
    pub const fn new(value: T) -> Self {
        Self {
            inner: RefCell::new(MaybeUninit::new(value)),
            owned: Cell::new(true),
        }
    }

    pub fn try_get(&self) -> Result<T, ()> {
        if self.owned.get() {
            self.owned.set(false);
            let mut ret: MaybeUninit<T> = MaybeUninit::uninit();
            std::mem::swap(&mut *self.inner.borrow_mut(), &mut ret);

            // SAFETY: The only way to initialize Self type is calling `new` constructor.
            //         It moves into already-initialized value to us.
            //         So we own T, we have (loglcally) initialized ret by swap in above call.
            //         If this method was called by twice or more, then returns Err in following
            //         branch, so we do not have to worry about spreading half-baked T from
            //         this method.
            Ok(unsafe { ret.assume_init() })
        } else {
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::invoke_once::InvokeOnce;

    #[test]
    fn usable_once() {
        struct X;

        impl X {
            fn hello(&self) {}
        }

        let k = InvokeOnce::new(X);
        k.try_get().expect("must be successful").hello();
        assert!(k.try_get().is_err());
    }

    #[test]
    fn usable_once_2() {
        let k = InvokeOnce::new(0i32);
        assert_eq!(k.try_get().expect("must be successful") + 1, 1);
        assert!(k.try_get().is_err());
    }
}
