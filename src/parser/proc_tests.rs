
use crate::parser::ast::{AstKind, PathSegment};
use crate::operators::BinaryOp;
use crate::Bounds;

use super::*;

fn setup(source: &str) -> Result<Data, String> {
	let input = crate::input::eval(file!().into(), source.into());

	let lex_data = crate::lexer::eval(&input.source)
		.map_err(|e| e.display(&input))?;

	eval(&input, &lex_data, true)
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
	assert_eq!(proc.body.len(), 2);
	let proc = &data.procedures[&"sub".id()];
	assert_eq!(proc.target, Some(Target::Z80));
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 2);
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
	assert_eq!(proc.body.len(), 2);
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
	assert_eq!(proc.body.len(), 2);
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
	assert_eq!(proc.body.len(), 2);
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
		AstKind::Return(None),
		AstKind::Block(vec![0.into()]),
	]);
}

#[test]
fn with_no_params() {
	let data = setup("main{} proc a() {}")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 2);
	let proc = &data.procedures[&"a".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body.len(), 2);
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
		AstKind::Int(2),
		AstKind::Int(3),
		AstKind::BinOp(BinaryOp::Add, 0.into(), 1.into()),
		AstKind::Ident("a".id()),
		AstKind::Define(3.into(), Type::S8),
		AstKind::Assign(4.into(), 2.into()),
		AstKind::Return(None),
		AstKind::Block(vec![5.into(), 6.into()]),
	]);
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
		AstKind::Int(2),
		AstKind::Int(3),
		AstKind::BinOp(BinaryOp::Add, 0.into(), 1.into()),
		AstKind::Int(4),
		AstKind::Int(5),
		AstKind::BinOp(BinaryOp::Sub, 3.into(), 4.into()),
		AstKind::BinOp(BinaryOp::Mul, 2.into(), 5.into()),
		AstKind::Ident("a".id()),
		AstKind::Define(7.into(), Type::S8),
		AstKind::Assign(8.into(), 6.into()),
		AstKind::Return(None),
		AstKind::Block(vec![9.into(), 10.into()]),
	]);
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
	assert_eq!(proc.body.len(), 2);
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
	assert_eq!(proc.body.len(), 2);
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
	assert_eq!(proc.body.len(), 2);
}

#[test]
fn with_basic_for_loop() {
	let data = setup("main { for i in [0..10] {} }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::For(
			vec!["i".id()],
			None,
			Some(Bounds::Full { start: 0, end: 10}),
			vec![],
		),
		AstKind::Return(None),
		AstKind::Block(vec![0.into(), 1.into()]),
	]);
}

#[test]
fn with_multi_element_for_loop() {
	let data = setup("main { for i,j in [0..10] {} }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::For(
			vec!["i".id(), "j".id()],
			None,
			Some(Bounds::Full { start: 0, end: 10 }),
			vec![],
		),
		AstKind::Return(None),
		AstKind::Block(vec![0.into(), 1.into()]),
	]);
}

#[test]
fn with_internal_while_loop() {
	let data = setup("main { while true { } }")
			.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.body, [
		AstKind::Int(1),
		AstKind::While(
			0.into(),
			vec![],
		),
		AstKind::Return(None),
		AstKind::Block(vec![1.into(), 2.into()]),
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
	assert_eq!(proc.body.len(), 2);
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
	assert_eq!(proc.body.len(), 2);
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
		AstKind::Access("a".id(), vec![
			PathSegment::Field("b".id()),
		]),
		AstKind::Int(2),
		AstKind::Assign(0.into(), 1.into()),
		AstKind::Return(None),
		AstKind::Block(vec![2.into(), 3.into()]),
	]);
}

#[cfg(feature="table")]
#[test]
fn empty_table() {
	let data = setup("table a[10] {}")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.tables.len(), 1);
	let table = &data.tables[&"a".id()];
	assert_eq!(table.row_count, 10);
	assert_eq!(table.column_spec, []);
	assert_eq!(table.size(&data), 0);
	assert_eq!(table.address, None);
}

#[cfg(feature="table")]
#[test]
fn table_with_one_field() {
	let data = setup("table a[10] { b: u32 }")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.tables.len(), 1);
	let table = &data.tables[&"a".id()];
	assert_eq!(table.row_count, 10);
	assert_eq!(table.column_spec, [("b".id(), Type::U32)]);
	assert_eq!(table.size(&data), 40);
	assert_eq!(table.address, None);
}

#[cfg(feature="table")]
#[test]
fn table_with_multiple_field() {
	let data = setup("table a[10] { b: u32, c: s16 }")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.tables.len(), 1);
	let table = &data.tables[&"a".id()];
	assert_eq!(table.row_count, 10);
	assert_eq!(table.column_spec, [
		("b".id(), Type::U32),
		("c".id(), Type::S16),
	]);
	assert_eq!(table.size(&data), 60);
	assert_eq!(table.address, None);
}

#[cfg(feature="table")]
#[test]
fn table_with_user_defined_field() {
	let data = setup("record a { a1: s16 } table b[10] { b1: a }")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(data.tables.len(), 1);
	let table = &data.tables[&"b".id()];
	assert_eq!(table.row_count, 10);
	assert_eq!(table.column_spec, [
		("b1".id(), Type::Record("a".id())),
	]);
	assert_eq!(table.size(&data), 20);
	assert_eq!(table.address, None);
}

#[cfg(feature="table")]
#[test]
fn with_table_param() {
	let data = setup("main{} table a[0] {} proc b(c:a) {}")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"b".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
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

#[cfg(feature="table")]
#[test]
fn with_internal_table_assign() {
	let data = setup("main { a[3].b = 2; }")
		.unwrap_or_else(|e| panic!("{e}"));
	let proc = &data.procedures[&"main".id()];
	assert_eq!(proc.target, None);
	assert!(proc.params.is_empty());
	assert_eq!(proc.ret_type, Type::Void);
	assert_eq!(proc.body, []);
}
