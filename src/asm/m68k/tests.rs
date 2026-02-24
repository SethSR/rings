
use crate::{input, layout, lexer, packing, parser, tac, type_checker};
use crate::identifier::{Identifier, Map as IdentMap};

use super::*;
use super::interpreter::{M68kEmu, interpret};

fn setup(source: &str) -> IdentMap<Vec<Asm>> {
	let mut source_str = String::new();
	source_str.push_str("region Stack[0] @ 0;");
	source_str.push_str(source);

	let input = input::eval(file!().to_string(), source_str.into());

	let lex_data = lexer::eval(&input.source)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

	let prs_data = parser::eval(&input, &lex_data, false)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

	let typ_data = type_checker::eval(&input, &lex_data, &prs_data)
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));

	let pak_data = packing::eval(&prs_data);

	let loc_data = layout::eval(&prs_data, &pak_data)
			.unwrap_or_else(|e| panic!("{}", e.display(&input, &lex_data)));

	let tac_data = tac::eval(&prs_data, &typ_data, &pak_data, &loc_data)
			.map_err(|e| e.into_comp_error(&input, &lex_data, &prs_data.procedures))
			.unwrap_or_else(|e| panic!("{}", e.display(&input)));
	//eprintln!("{tac_data:?}");

	let stack_addr = prs_data.regions.get(&"Stack".id())
			.or(prs_data.regions.get(&"DataStack".id()))
			.expect("missing stack address")
			.span.start;

	let mut out = IdentMap::<Vec<Asm>>::default();

	for (proc_id, tac) in tac_data {
		let proc_name = lex_data.text(&input, &proc_id).to_owned();
		out.insert(proc_id, lower(&proc_name, tac, stack_addr));
	}

	out
}

#[test]
fn set_stack_value() {
	let data = setup("main {
			let a: s8 = (2 + 3) * (3 - 1);
		}");
	let main_proc = &data[&"main".id()];
	let emu = interpret(main_proc);
	assert_eq!(emu.mem.len(), 1);
	assert_eq!(emu.mem[&0], 10);
}

#[test]
fn branch() {
	let data = setup("main {}
		m68k proc branch() {
			let b: s8 = 5;
			let c: s8 = 8;
			if (b + c) > 10 {
				b += 2;
			} else {
				c += 1;
			}
		}");
	let main_proc = &data[&"branch".id()];
	let emu = interpret(main_proc);
	assert_eq!(emu.mem.len(), 2);
	assert_eq!(emu.mem[&0], 7);
	assert_eq!(emu.mem[&4], 8);
}

#[test]
fn for_loop() {
	let data = setup("main {}
		m68k proc for_loop() {
			let b: s8 = 4;
			let c: s8 = 0;
			for i in [0..10] {
				c += b * 2;
			}
		}");
	let main_proc = &data[&"for_loop".id()];
	let emu = interpret(main_proc);
	assert_eq!(emu.mem.len(), 2);
	assert_eq!(emu.mem[&0], 4);
	assert_eq!(emu.mem[&4], 80);
}
