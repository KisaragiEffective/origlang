use std::alloc::{alloc, Layout};
use std::mem::MaybeUninit;

pub type BoxedSlice<I> = Box<[I]>;

pub fn create_initialized_boxed_slice<I>(len: usize, mut create_element: impl FnMut(usize) -> I) -> BoxedSlice<I> {
    let mut slice = Box::new_uninit_slice(len);
    for (i, e) in slice.iter_mut().enumerate() {
        e.write(create_element(i));
    }

    unsafe { slice.assume_init() }
}

pub fn try_create_initialized_boxed_slice<I, E>(len: usize, mut try_create_element: impl FnMut(usize) -> Result<I, E>) -> Result<BoxedSlice<I>, E> {
    let mut slice = Box::new_uninit_slice(len);
    for (i, e) in slice.iter_mut().enumerate() {
        e.write(try_create_element(i)?);
    }
    
    Ok(unsafe { slice.assume_init() })
}
