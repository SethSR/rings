
use crate::parser::ast::PathSegment;
use crate::operators::BinaryOp;

use super::*;

fn setup(source: &str) -> Result<Data<SrcPos>, String> {
	let input = crate::input::eval(file!().into(), source.into());

	let lex_data = crate::lexer::eval(&input.source)
		.map_err(|e| e.display(&input))?;

	eval(&input, &lex_data, false)
		.map_err(|e| e.display(&input))
}

#[test]
fn init_procedure_with_target() {
	let data = setup("sh2 main {} z80 sub {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.procedures.len(), 2);
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, Some(Target::SH2));
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 4);
	let proc = &data.procedures[&"sub".id()];
	assert_eq!(proc.target, Some(Target::Z80));
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 4);
}

#[test]
fn procedure_with_return() {
	let data = setup("proc a() -> s8 {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.procedures.len(), 1);
	let proc = &data.procedures[&"a".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::S8);
	assert_eq!(proc.body.len(), 4);
}

#[test]
fn procedure_with_target() {
	let data = setup("sh2 proc a() {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.procedures.len(), 1);
	let proc = &data.procedures[&"a".id()];
	assert_eq!(proc.target, Some(Target::SH2));
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 4);
}

#[test]
fn procedure_with_params() {
	let data = setup("proc a(b: s8, c: s8) {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.procedures.len(), 1);
	let proc = &data.procedures[&"a".id()];
	assert_eq!(proc.target, None);
	assert_eq!(proc.params, [
		("b".id(), Type::S8),
		("c".id(), Type::S8),
	]);
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 4);
}

#[test]
fn main() {
	let data = setup("main{}")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![2.into()]),
	]);
}

#[test]
fn with_typed_params() {
	let data = setup("
		main {
			let a: u16 = 35;
		}
	").unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Int(35),
		AstKind::Ident("a".id()),
		AstKind::Assign { lhs: 2.into(), rhs: 1.into() },
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![3.into(), 5.into()]),
	]);
	assert_eq!(data.types.get(&("main".id(), 1, "a".id())), Some(&Type::U16));
}

#[test]
fn with_inferred_params() {
	let data = setup("
		main {
			let a = 35;
		}
	").unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body.len(), 7);
	assert_eq!(data.types.get(&("main".id(), 1, "a".id())), Some(&Type::Unknown));
}

#[test]
fn with_no_params() {
	let data = setup("main{} proc a() {}")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 4);
	let proc = &data.procedures[&"a".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 4);
}

#[test]
fn with_if() {
	let data = setup("
		proc a(b: bool) -> s8 {
			if b {
				return 1;
			} else {
				return -1;
			}
		}
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.types.get(&("a".id(), 0, "b".id())), Some(&Type::Bool));
	let proc = &data.procedures[&"a".id()];
	assert_eq!(proc.params, [
		("b".id(), Type::Bool),
	]);
	assert_eq!(proc.ret_type, Type::S8);
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Ident("b".id()),
		AstKind::ScopeBegin,
		AstKind::Int(1),
		AstKind::Return(Some(3.into())),
		AstKind::ScopeEnd,
		AstKind::ScopeBegin,
		AstKind::Int(-1),
		AstKind::Return(Some(7.into())),
		AstKind::ScopeEnd,
		AstKind::If {
			cond: 1.into(),
			then_block: vec![4.into()],
			else_block: vec![8.into()],
		},
		AstKind::ScopeEnd,
		AstKind::Block(vec![10.into()]),
	]);
}

#[test]
fn with_while() {
	let data = setup("
		proc a(b: s8) -> s8 {
			while b < 10 {
				b += 3;
			}
			return b;
		}
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.types.get(&("a".id(), 0, "b".id())), Some(&Type::S8));
	let proc = &data.procedures[&"a".id()];
	assert_eq!(proc.params, [
		("b".id(), Type::S8),
	]);
	assert_eq!(proc.ret_type, Type::S8);
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Ident("b".id()),
		AstKind::Int(10),
		AstKind::BinOp { op: BinaryOp::CmpLT, lhs: 1.into(), rhs: 2.into() },
		AstKind::ScopeBegin,
		AstKind::Ident("b".id()),
		AstKind::Int(3),
		AstKind::BinOp { op: BinaryOp::Add, lhs: 5.into(), rhs: 6.into() },
		AstKind::Assign { lhs: 5.into(), rhs: 7.into() },
		AstKind::ScopeEnd,
		AstKind::While { cond: 3.into(), block: vec![8.into()] },
		AstKind::Ident("b".id()),
		AstKind::Return(Some(11.into())),
		AstKind::ScopeEnd,
		AstKind::Block(vec![10.into(), 12.into()]),
	]);
}

#[test]
fn with_multiple_scopes() {
	let data = setup("
		main {
			let a: s16 = 34;
			if a {
				let b: s32 = a + 5;
			} else {
				while false {
					let c: s16 = a * 2 - 3;
				}
			}
		}
	").unwrap_or_else(|e| panic!("{e}"));
	let main_id = "main".id();
	assert_eq!(data.types.get(&(main_id, 1, "a".id())), Some(&Type::S16));
	assert_eq!(data.types.get(&(main_id, 2, "b".id())), Some(&Type::S32));
	assert_eq!(data.types.get(&(main_id, 3, "c".id())), Some(&Type::S16));
}

#[test]
fn with_internal_expressions() {
	let data = setup("main { let a: s8 = 2 + 3; }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Int(2),
		AstKind::Int(3),
		AstKind::BinOp { op: BinaryOp::Add, lhs: 1.into(), rhs: 2.into() },
		AstKind::Ident("a".id()),
		AstKind::Assign { lhs: 4.into(), rhs: 3.into() },
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![5.into(), 7.into()]),
	]);
	assert_eq!(data.types.get(&("main".id(), 1, "a".id())), Some(&Type::S8));
}

#[test]
fn with_internal_sub_expressions() {
	let data = setup("main { let a: s8 = (2 + 3) * (4 - 5); }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Int(2),
		AstKind::Int(3),
		AstKind::BinOp { op: BinaryOp::Add, lhs: 1.into(), rhs: 2.into() },
		AstKind::Int(4),
		AstKind::Int(5),
		AstKind::BinOp { op: BinaryOp::Sub, lhs: 4.into(), rhs: 5.into() },
		AstKind::BinOp { op: BinaryOp::Mul, lhs: 3.into(), rhs: 6.into() },
		AstKind::Ident("a".id()),
		AstKind::Assign { lhs: 8.into(), rhs: 7.into() },
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![9.into(), 11.into()]),
	]);
	assert_eq!(data.types.get(&("main".id(), 1, "a".id())), Some(&Type::S8));
}

#[test]
fn with_return() {
	let data = setup("main{} proc a() -> s8 {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert!(data.procedures.contains_key(&"main".id()));
	let proc = &data.procedures[&"a".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::S8);
	assert_eq!(proc.body.len(), 4);
}

#[test]
fn with_single_param() {
	let data = setup("main{} proc a(b:s8) {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert!(data.procedures.contains_key(&"main".id()));
	let proc = &data.procedures[&"a".id()];
	assert_eq!(proc.target, None);
	assert_eq!(proc.params, [
		("b".id(), Type::S8),
	]);
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 4);
}

#[test]
fn with_multi_params() {
	let data = setup("main{} proc a(b:s8,c:s8) {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert!(data.procedures.contains_key(&"main".id()));
	let proc = &data.procedures[&"a".id()];
	assert_eq!(proc.params, [
		("b".id(), Type::S8),
		("c".id(), Type::S8),
	]);
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 4);
}

#[test]
fn with_basic_for_loop() {
	let data = setup("main { for i in [0..10] {} }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Ident("i".id()),
		AstKind::Int(0),
		AstKind::Int(10),
		AstKind::ScopeBegin,
		AstKind::ScopeEnd,
		AstKind::For {
			indexes: vec![1.into()],
			table: None,
			range_start: Some(2.into()),
			range_end: Some(3.into()),
			block: vec![],
		},
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![6.into(), 8.into()]),
	]);
}

#[test]
fn with_multi_element_for_loop() {
	let data = setup("main { for i,j in [0..10] {} }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Ident("i".id()),
		AstKind::Ident("j".id()),
		AstKind::Int(0),
		AstKind::Int(10),
		AstKind::ScopeBegin,
		AstKind::ScopeEnd,
		AstKind::For {
			indexes: vec![1.into(), 2.into()],
			table: None,
			range_start: Some(3.into()),
			range_end: Some(4.into()),
			block: vec![],
		},
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![7.into(), 9.into()]),
	]);
}

#[test]
fn with_internal_while_loop() {
	let data = setup("main { while true { } }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Int(1),
		AstKind::ScopeBegin,
		AstKind::ScopeEnd,
		AstKind::While { cond: 1.into(), block: vec![], },
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![4.into(), 6.into()]),
	]);
}

#[test]
fn with_record_param() {
	let data = setup("main{} record a {} proc b(c:a) {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert!(data.procedures.contains_key(&"main".id()));
	assert!(data.records.contains_key(&"a".id()));
	let proc = &data.procedures[&"b".id()];
	assert_eq!(proc.target, None);
	assert_eq!(proc.params, [
		("c".id(), Type::Record("a".id())),
	]);
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 4);
}

#[test]
fn with_out_of_order_record_param() {
	let data = setup("main{} proc b(c:a) {} record a {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert!(data.procedures.contains_key(&"main".id()));
	assert!(data.records.contains_key(&"a".id()));
	let proc = &data.procedures[&"b".id()];
	assert_eq!(proc.target, None);
	assert_eq!(proc.params, [
		("c".id(), Type::Record("a".id())),
	]);
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 4);
}

#[test]
fn with_internal_field_assign() {
	let data = setup("main { a.b = 2; }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Access { base_id: "a".id(), path: vec![
			PathSegment::Field("b".id()),
		]},
		AstKind::Int(2),
		AstKind::Assign { lhs: 1.into(), rhs: 2.into() },
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![3.into(), 5.into()]),
	]);
}

#[test]
fn empty_table() {
	let data = setup("table a[10] {}")
			.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.tables.len(), 1);
	let table = data.tables.get(&"a".id())
			.expect("missing table 'a'");
	assert_eq!(table.row_count, 10);
	assert_eq!(table.placement, None);
	assert_eq!(table.fields, []);
}

#[test]
fn table_with_one_field() {
	let data = setup("table a[10] { b: u32 }")
			.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.tables.len(), 1);
	let table = &data.tables[&"a".id()];
	assert_eq!(table.row_count, 10);
	assert_eq!(table.placement, None);
	assert_eq!(table.fields, [
		("b".id(), Type::U32),
	]);
}

#[test]
fn table_with_multiple_field() {
	let data = setup("table a[10] { b: u32, c: s16 }")
			.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.tables.len(), 1);
	let table = &data.tables[&"a".id()];
	assert_eq!(table.row_count, 10);
	assert_eq!(table.placement, None);
	assert_eq!(table.fields, [
		("b".id(), Type::U32),
		("c".id(), Type::S16),
	]);
}

#[test]
fn table_with_user_defined_field() {
	let data = setup("
		record a { a1: s16 }
		table b[10] { b1: a }
	").unwrap_or_else(|e| panic!("{e}"));
	assert!(data.records.contains_key(&"a".id()));
	assert_eq!(data.tables.len(), 1);
	let table = &data.tables[&"b".id()];
	assert_eq!(table.row_count, 10);
	assert_eq!(table.placement, None);
	assert_eq!(table.fields, [
		("b1".id(), Type::Record("a".id())),
	]);
}

#[test]
fn with_table_param() {
	let data = setup("
		main{}
		table a[0] {}
		proc b(c:a) {}
	").unwrap_or_else(|e| panic!("{e}"));
	assert!(data.procedures.contains_key(&"main".id()));
	assert!(data.procedures.contains_key(&"b".id()));
	assert!(data.tables.contains_key(&"a".id()));
	let proc = &data.procedures[&"b".id()];
	assert_eq!(proc.params, [
		("c".id(), Type::Table("a".id())),
	]);
}

#[test]
fn with_internal_table_assign() {
	let data = setup("main { a[3].b = 2; }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Int(3),
		AstKind::Access { base_id: "a".id(), path: vec![
			PathSegment::Index(1.into(), "b".id()),
		]},
		AstKind::Int(2),
		AstKind::Assign { lhs: 2.into(), rhs: 3.into() },
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![4.into(), 6.into()]),
	]);
}

#[test]
fn with_mark() {
	let data = setup("main { mark start in base; }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Mark { region_id: "base".id(), mark_id: "start".id() },
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![1.into(), 3.into()]),
	]);
}

#[test]
fn with_free_mark() {
	let data = setup("main { free start in base; }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Free { region_id: "base".id(), mark_id: Some("start".id()) },
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![1.into(), 3.into()]),
	]);
}

#[test]
fn with_free_region() {
	let data = setup("main { free base; }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Free { region_id: "base".id(), mark_id: None },
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![1.into(), 3.into()]),
	]);
}

#[test]
fn with_use() {
	let data = setup("main { use start in base; }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::ScopeBegin,
		AstKind::Use { region_id: "base".id(), ident: "start".id() },
		AstKind::ScopeEnd,
		AstKind::Return(None),
		AstKind::Block(vec![1.into(), 3.into()]),
	]);
}

#[cfg(feature="forloop")]
#[test]
fn with_table_for_loop() {
	let data = setup("main { for i in a {} }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}

#[cfg(feature="forloop")]
#[test]
fn with_table_index_for_loop() {
	let data = setup("main { for i in a[0..10] {} }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}

#[cfg(feature="forloop")]
#[test]
fn with_table_from_for_loop() {
	let data = setup("main { for i in a[0..] {} }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}

#[cfg(feature="forloop")]
#[test]
fn with_table_to_for_loop() {
	let data = setup("main { for i in a[..10] {} }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}

#[cfg(feature="forloop")]
#[test]
fn with_table_full_for_loop() {
	let data = setup("main { for i in a[..] {} }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}

#[cfg(feature="index")]
#[test]
fn with_internal_table_index() {
	let data = setup("main { return a[10].b; }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}

#[cfg(feature="index")]
#[test]
fn with_internal_table_expression_indexing() {
	let data = setup("main { return a[2 + 4].b; }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}

#[cfg(feature="call")]
#[test]
fn with_internal_proc_call() {
	let data = setup("main { return a(3); }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, []);
}

#[cfg(feature="call")]
#[test]
fn with_internal_proc_call_in_subexpression() {
	let data = setup("main { return 3 * a(3); }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}

#[cfg(feature="call")]
#[test]
fn with_internal_proc_call_with_expression_argument() {
	let data = setup("main { return a(b + 4); }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}

#[cfg(feature="call")]
#[test]
fn with_internal_proc_call_with_multiple_arguments() {
	let data = setup("main { return a(2, 4 / b, b + 4); }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}
