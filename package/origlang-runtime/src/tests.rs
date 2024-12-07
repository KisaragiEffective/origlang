mod display {
    mod record {
        use origlang_ast::Identifier;
        use crate::{DisplayRecordValue, TypeBox};

        #[test]
        fn empty() {
            let x = DisplayRecordValue {
                name: Identifier::new("abcdef".to_string().into_boxed_str()),
                values: vec![],
            };

            let x = format!("{x}");
            assert_eq!(x, "abcdef {}");
        }

        #[test]
        fn one() {
            let x = DisplayRecordValue {
                name: Identifier::new("abcdef".to_string().into_boxed_str()),
                values: vec![
                    TypeBox::String("defg".to_string().into_boxed_str())
                ],
            };

            let x = format!("{x}");
            assert_eq!(x, "abcdef {defg}");
        }

        #[test]
        fn many() {
            let x = DisplayRecordValue {
                name: Identifier::new("abcdef".to_string().into_boxed_str()),
                values: vec![
                    TypeBox::String("abcdef".to_string().into_boxed_str()),
                    TypeBox::String("ghijkl".to_string().into_boxed_str()),
                    TypeBox::String("alice".to_string().into_boxed_str())
                ],
            };

            let x = format!("{x}");
            assert_eq!(x, "abcdef {abcdef, ghijkl, alice}");

        }
    }
    mod tuple {
        use crate::{DisplayTupleValue, TypeBox};

        #[test]
        fn empty() {
            let x = DisplayTupleValue {
                boxes: Box::new([]),
            };

            let x = format!("{x}");
            assert_eq!(x, "()");
        }

        #[test]
        fn one() {
            let x = DisplayTupleValue {
                boxes: Box::new([TypeBox::String("abcd".to_string().into_boxed_str())]),
            };

            let x = format!("{x}");
            assert_eq!(x, "(abcd)");
        }

        #[test]
        fn many() {
            let x = DisplayTupleValue {
                boxes: Box::new([TypeBox::String("abcd".to_string().into_boxed_str()), TypeBox::String("defg".to_string().into_boxed_str())]),
            };

            let x = format!("{x}");
            assert_eq!(x, "(abcd, defg)");

        }
    }
}
