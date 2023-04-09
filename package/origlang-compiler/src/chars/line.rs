use thiserror::Error;
use origlang_ast::SourcePos;
use crate::chars::boundary::Utf8CharBoundaryStartByte;
use crate::chars::occurrence::OccurrenceSet;

pub struct LineComputation;

impl LineComputation {
    pub fn compute(future_index: Utf8CharBoundaryStartByte, new_line_occurrences: &OccurrenceSet<Utf8CharBoundaryStartByte>) -> Result<SourcePos, LineComputationError> {
        /*
        // This may be an error, however this snippet leads to infinite loop.
        if new_line_occurrences.contains(&future_index) {
            return Err(LineComputationError::PointedOnNewLine)
        }
        */

        let future_line = new_line_occurrences.count_lowers_exclusive(future_index) + 1;

        let most_recent_new_line_occurrence_codepoint = new_line_occurrences
            .max_upper_bounded_exclusive(future_index)
            // if future_index is still on first line, there's no such occurrence - substitute
            // this value with zero to leave future_index as is.
            .copied()
            .unwrap_or(Utf8CharBoundaryStartByte::new(0));

        assert!(future_index >= most_recent_new_line_occurrence_codepoint, "{future_index:?} >= {most_recent_new_line_occurrence_codepoint:?}");
        let future_line_column = future_index.as_usize() - most_recent_new_line_occurrence_codepoint.as_usize();

        Ok(SourcePos {
            line: future_line.try_into().map_err(|_| LineComputationError::LineIsZero)?,
            column: future_line_column.try_into().map_err(|_| LineComputationError::ColumnIsZero)?,
        })
    }
}

#[derive(Error, Debug, Eq, PartialEq, Copy, Clone)]
pub enum LineComputationError {
    #[error("line number is zero")]
    LineIsZero,
    #[error("column number is zero")]
    ColumnIsZero,
}

#[cfg(test)]
mod tests {
    use origlang_ast::SourcePos;
    use crate::chars::boundary::Utf8CharBoundaryStartByte;
    use crate::chars::line::LineComputation;
    use crate::chars::occurrence::OccurrenceSet;

    #[test]
    fn no_newline() {
        assert_eq!(
            LineComputation::compute(Utf8CharBoundaryStartByte::new(12), &OccurrenceSet::default()),
            Ok(SourcePos {
                line: 1.try_into().unwrap(),
                column: 12.try_into().unwrap(),
            })
        );
    }

    #[test]
    fn single_newline_pre() {
        assert_eq!(
            LineComputation::compute(Utf8CharBoundaryStartByte::new(1), &OccurrenceSet::new(
                vec![Utf8CharBoundaryStartByte::new(100)]
            ).unwrap()),
            Ok(SourcePos {
                line: 1.try_into().unwrap(),
                column: 1.try_into().unwrap(),
            })
        )
    }

    #[test]
    fn single_newline_pre_99() {
        assert_eq!(
            LineComputation::compute(Utf8CharBoundaryStartByte::new(99), &OccurrenceSet::new(
                vec![Utf8CharBoundaryStartByte::new(100)]
            ).unwrap()),
            Ok(SourcePos {
                line: 1.try_into().unwrap(),
                column: 99.try_into().unwrap(),
            })
        )
    }

    #[test]
    fn single_newline_post() {
        assert_eq!(
            LineComputation::compute(Utf8CharBoundaryStartByte::new(101), &OccurrenceSet::new(
                vec![Utf8CharBoundaryStartByte::new(100)]
            ).unwrap()),
            Ok(SourcePos {
                line: 2.try_into().unwrap(),
                column: 1.try_into().unwrap(),
            })
        )
    }

    #[test]
    fn single_newline_point_is_not_an_error() {
        assert_eq!(
            LineComputation::compute(Utf8CharBoundaryStartByte::new(100), &OccurrenceSet::new(vec![Utf8CharBoundaryStartByte::new(100)]).unwrap()),
            Ok(SourcePos {
                line: 1.try_into().unwrap(),
                column: 100.try_into().unwrap(),
            })
        )
    }
}