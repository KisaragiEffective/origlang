#[derive(Clone, Eq, PartialEq, Debug, Hash)]
#[allow(clippy::module_name_repetitions)]
/// Contains "sorted" values. Unlike [std::collections::BTreeSet], this collection has vector internally,
/// for performance optimization.
pub struct OccurrenceSet<T>(Vec<T>);

fn is_sorted<T: Ord>(slice: &[T]) -> bool {
    if slice.len() <= 1 {
        true
    } else {
        slice.iter().fold((true, &slice[0]), |(b, e), f| {
            (b && e <= f, f)
        }).0
    }
}

impl<T: Ord> OccurrenceSet<T> {
    pub fn new(v: Vec<T>) -> Option<Self> {
        if v.len() <= 1 {
            Some(Self(v))
        } else if Self::invariant_was_satisfied(&v) {
            // SAFETY: we've checked precondition.
            unsafe {
                Some(Self::new_unchecked(v))
            }
        } else {
            None
        }
    }

    fn invariant_was_satisfied(v: &[T]) -> bool {
        is_sorted(v)
    }

    pub unsafe fn new_unchecked(v: Vec<T>) -> Self {
        debug_assert!(Self::invariant_was_satisfied(&v), "invariant was violated");

        Self(v)
    }

    pub fn count_lowers_exclusive(&self, upper: &T) -> usize {
        let mut i = 0;
        let values: &[T] = &self.0;
        let mut run_rest = true;
        if values.len() >= 6400 {
            // if values are too many to being cached in L1 storage,
            // switch strategy to binary_search.
            // This operation always return correct value, as underlying source
            // is guaranteed to be sorted in ascending order.
            return values.binary_search(upper).map_or_else(|x| x, |x| x);
        } else if values.len() >= 8 {
            while i < values.len() - 8 {
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v1 = unsafe { values.get_unchecked(i) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v2 = unsafe { values.get_unchecked(i + 1) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v3 = unsafe { values.get_unchecked(i + 2) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v4 = unsafe { values.get_unchecked(i + 3) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v5 = unsafe { values.get_unchecked(i + 4) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v6 = unsafe { values.get_unchecked(i + 5) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v7 = unsafe { values.get_unchecked(i + 6) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v8 = unsafe { values.get_unchecked(i + 7) };

                let upper = &upper;
                if v8 < upper {
                    // let CPU to guess what is going on, manual _mm_prefetch is inefficient
                    i += 8;
                } else {
                    // v8 >= upper
                    // partition point must be in v1..v8
                    if v8 < upper {
                        i += 8;
                    } else if v7 < upper {
                        i += 7;
                    } else if v6 < upper {
                        i += 6;
                    } else if v5 < upper {
                        i += 5;
                    } else if v4 < upper {
                        i += 4;
                    } else if v3 < upper {
                        i += 3;
                    } else if v2 < upper {
                        i += 2;
                    } else if v1 < upper {
                        i += 1;
                    }

                    run_rest = false;
                    break
                }
            }
        }

        if run_rest {
            let j = i;
            for x in &values[j..] {
                if x < upper {
                    i += 1;
                }
            }
        }

        i
    }

    pub fn max_upper_bounded_exclusive(&self, upper: &T) -> Option<&T> {
        let values: &[T] = &self.0;

        let k = self.count_lowers_exclusive(upper);
        if k == 0 {
            None
        } else {
            values.get(k - 1)
        }
    }
}

// You can construct empty OccurrenceSet even if T: !Default
impl<T> Default for OccurrenceSet<T> {
    fn default() -> Self {
        Self(vec![])
    }
}

#[cfg(test)]
mod tests {
    use crate::chars::occurrence::{is_sorted, OccurrenceSet};

    #[test]
    fn sorted_empty() {
        assert!(is_sorted::<i32>(&[]));
    }

    #[test]
    fn sorted_single() {
        assert!(is_sorted(&[1]));
    }

    #[test]
    fn sorted_double() {
        assert!(is_sorted(&[1, 2]));
    }

    #[test]
    fn sorted_double_negative() {
        assert!(!is_sorted(&[2, 1]));
    }

    #[test]
    fn occurrence_empty() {
        let set = OccurrenceSet::<usize>::new(vec![]);
        assert_eq!(set.expect("must be constructed").count_lowers_exclusive(&0), 0);
    }

    #[test]
    fn occurrence_single_less() {
        let set = OccurrenceSet::<usize>::new(vec![0]);
        assert_eq!(set.expect("must be constructed").count_lowers_exclusive(&1), 1);
    }

    #[test]
    fn occurrence_single_eq() {
        let set = OccurrenceSet::<usize>::new(vec![0]);
        assert_eq!(set.expect("must be constructed").count_lowers_exclusive(&0), 0);
    }

    #[test]
    fn occurrence_single_more() {
        let set = OccurrenceSet::<usize>::new(vec![1]);
        assert_eq!(set.expect("must be constructed").count_lowers_exclusive(&0), 0);
    }

    #[test]
    fn occurrence_8() {
        let set = OccurrenceSet::new(vec![1, 2, 3, 4, 5, 6, 7, 8]);
        assert_eq!(set.expect("must be constructed").count_lowers_exclusive(&10), 8);
    }

    #[test]
    fn occurrence_9() {
        let set = OccurrenceSet::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9]);
        assert_eq!(set.expect("must be constructed").count_lowers_exclusive(&10), 9);
    }

    use std::collections::{BinaryHeap, BTreeSet};
    use std::ops::Deref;
    use std::time::{Instant};
    use rand::distributions::{Distribution, Standard};
    use rand::Rng;
    use crate::chars::occurrence::OccurrenceSet;

    #[test]
    fn bench() {
        const N: usize = 16384;

        // avoids stack overflow in debug mode.
        struct OnHeap<T, const N: usize>(Box<[T; N]>);

        impl<T, const N: usize> Distribution<OnHeap<T, N>> for Standard where Standard: Distribution<T> {
            fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> OnHeap<T, N> {
                OnHeap(Box::new(rng.gen::<[T; N]>()))
            }
        }

        impl<T, const N: usize> Deref for OnHeap<T, N> {
            type Target = [T; N];

            fn deref(&self) -> &Self::Target {
                self.0.as_ref()
            }
        }

        impl<T, const N: usize> IntoIterator for OnHeap<T, N> {
            type Item = T;
            type IntoIter = core::array::IntoIter<T, N>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.into_iter()
            }
        }

        impl<'a, T: 'a, const N: usize> IntoIterator for &'a OnHeap<T, N> {
            type Item = &'a T;
            type IntoIter = core::slice::Iter<'a, T>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.iter()
            }
        }

        let haystack = rand::random::<OnHeap<usize, N>>();
        let find = rand::random::<OnHeap<usize, N>>();
        println!("1");
        let occ_time = {
            let now = Instant::now();
            let mut x = haystack.to_vec();
            x.sort();

            let mut buf = Vec::with_capacity(N);

            // let now = Instant::now();
            let occ = OccurrenceSet::new(x).expect("it is not empty");
            for f in &find {
                buf.push(occ.count_lowers_exclusive(f));
            }

            (buf, now.elapsed())
        };
        println!("1");

        let bt_time = {
            let mut buf = Vec::with_capacity(N);

            let now = Instant::now();
            let mut occ: BTreeSet<usize> = BTreeSet::new();
            occ.extend(&haystack);

            for upper in &find {
                buf.push(occ.iter().filter(|x| *x < upper).count());
            }

            (buf, now.elapsed())
        };
        println!("1");

        let bh_time = {
            let mut buf = Vec::with_capacity(N);

            let now = Instant::now();
            let mut occ: BinaryHeap<usize> = BinaryHeap::new();
            occ.extend(&haystack);

            for upper in &find {
                buf.push(occ.iter().filter(|x| *x < upper).count());
            }

            (buf, now.elapsed())
        };
        println!("1");

        let vec_time = {
            let mut buf = Vec::with_capacity(N);

            let now = Instant::now();
            let mut occ: Vec<usize> = Vec::with_capacity(N);
            occ.extend(&haystack);

            for upper in &find {
                buf.push(occ.iter().filter(|x| *x < upper).count());
            }

            (buf, now.elapsed())
        };


        let vec_ni_time = {
            let mut buf = Vec::with_capacity(N);

            let now = Instant::now();
            let mut occ: Vec<usize> = Vec::with_capacity(N);
            occ.extend(&haystack);

            for upper in find {
                let mut count = 0usize;
                for x in &occ {
                    if *x < upper {
                        count += 1;
                    }
                }

                buf.push(count);
            }

            (buf, now.elapsed())
        };

        println!("1");

        // let vec_ni_time = (vec![1], Duration::new(0, 0));
        assert_eq!(occ_time.0, bt_time.0);
        assert_eq!(bh_time.0, bt_time.0);
        assert_eq!(vec_time.0, bt_time.0);
        assert_eq!(vec_ni_time.0, bt_time.0);

        println!("impl: {o:?} | bin tree:{b:?} | bin heap: {bh:?} | vec_iter: {v:?} | vec: {vi:?}", o = occ_time.1, b = bt_time.1, bh = bh_time.1, v = vec_time.1, vi = vec_ni_time.1);
    }

}
