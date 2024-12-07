use std::alloc::{alloc, Layout};
use std::mem::MaybeUninit;

pub type BoxedSlice<I> = Box<[I]>;

pub fn create_initialized_boxed_slice<I>(len: usize, mut create_element: impl FnMut(usize) -> I) -> BoxedSlice<I> {
    let mut slice = Box::new_uninit_slice(len);
    for (i, e) in slice.iter_mut().enumerate() {
        e.write(create_element(i));
    }

    unsafe { assume_every_elements_are_initialized(slice) }
}

pub fn try_create_initialized_boxed_slice<I, E>(len: usize, mut try_create_element: impl FnMut(usize) -> Result<I, E>) -> Result<BoxedSlice<I>, E> {
    let mut slice = Box::new_uninit_slice(len);
    for (i, e) in slice.iter_mut().enumerate() {
        e.write(try_create_element(i)?);
    }
    
    Ok(unsafe { assume_every_elements_are_initialized(slice) })
}

unsafe fn assume_every_elements_are_initialized<I>(half_baked_slice_in_box: BoxedSlice<MaybeUninit<I>>) -> BoxedSlice<I> {
    let ptr = Box::into_raw(half_baked_slice_in_box);
    // SAFETY: caller must initialize elements in the fed MaybeUninit-slice.
    let slice = ptr as *mut [I];

    unsafe { Box::from_raw(slice) }
}
