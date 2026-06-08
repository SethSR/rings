
use crate::{input, layout, lexer, packing, parser, tac, type_checker};
use crate::identifier::{Identifier, Map as IdentMap};

use super::*;
use super::interpreter::Sh2Emu;

fn setup(source: &str) -> IdentMap<(Vec<Asm>, Vec<Block>)> {
	let mut source_str = String::new();
	source_str.push_str("region Stack[0] @ 0;");
	source_str.push_str(source);

	let input = input::eval(file!().to_string(), source_str.into());

	let lex_data = lexer::eval(&input.source)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

	let prs_data = parser::eval(&input, &lex_data)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

	let typ_data = type_checker::eval(&input, &lex_data, &prs_data)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

	let pak_data = packing::eval(&prs_data);

	let loc_data = layout::eval(&prs_data, &pak_data)
			.unwrap_or_else(|e| panic!("{}", e.display(&input, &lex_data)));

	let tac_data = tac::eval(&prs_data, &typ_data, &pak_data, &loc_data)
			.map_err(|e| e.into_comp_error(&input, &lex_data, &prs_data.procedures))
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

	let stack_addr = prs_data.regions.get(&"Stack".id())
			.or(prs_data.regions.get(&"DataStack".id()))
			.expect("missing stack address")
			.span.start;

	let mut out = IdentMap::<(Vec<Asm>, Vec<Block>)>::default();

	for (proc_id, tac) in tac_data {
		let proc_name = lex_data.text(&input, &proc_id).to_owned();
		let ret_type = prs_data.procedures[&proc_id].ret_type;
		out.insert(proc_id, lower(&proc_name, tac, stack_addr, ret_type));
	}

	out
}

#[test]
fn div() {
	let data = setup("main {
		let a: s32 = 16 / 4;
	}");
	let proc = &data[&"main".id()];
	let mut emu = Sh2Emu::default();
	emu.interpret(&proc.0);
	assert_eq!(emu.mem[&0], 4);
}

#[test]
fn long_value_placement() {
	let data = setup("main {
		let a: s32 = 0x0330_4400 + 5;
	}");
	let proc = &data[&"main".id()];
	let mut emu = Sh2Emu::default();
	emu.interpret(&proc.0);
	assert_eq!(emu.mem[&0], 0x0330_4405);
}
