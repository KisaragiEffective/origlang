#[derive(Clone, Eq, PartialEq, Debug, Hash)]
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

    #[inline(always)]
    fn invariant_was_satisfied(v: &[T]) -> bool {
        is_sorted(v)
    }

    pub unsafe fn new_unchecked(v: Vec<T>) -> Self {
        assert!(Self::invariant_was_satisfied(&v), "invariant was violated");

        Self(v)
    }

    pub fn count_lowers_exclusive(&self, upper: T) -> usize {
        let mut i = 0;
        let values: &[T] = &self.0;
        let mut run_rest = true;
        if values.len() >= 6400 {
            // if values are too many to being cached in L1 storage,
            // switch strategy to binary_search.
            return values.binary_search(&upper).map_or_else(|x| x, |x| x);
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
                if *x < upper {
                    i += 1;
                }
            }
        }

        i
    }

    pub fn max_upper_bounded_exclusive(&self, upper: T) -> Option<&T> {
        let values: &[T] = &self.0;

        let k = self.count_lowers_exclusive(upper);
        if k == 0 {
            None
        } else {
            Some(values.get(k - 1).expect("!"))
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
    use crate::chars::occurrence::is_sorted;

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
}