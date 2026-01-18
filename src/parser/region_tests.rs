
use crate::identifier::Identifier;

use super::*;

fn setup(source: &str) -> Result<RegionMap, String> {
	let input = crate::input::eval(file!().into(), source.into());
	let lex_data = crate::lexer::eval(source)
		.map_err(|e| e.display(&input))?;
	eval(&input, &lex_data, false)
		.map_err(|e| e.display(&input))
		.map(|data| data.regions)
}

#[test]
fn region_range() {
	let regions = setup("
		region a[0x20_0000..0x20_0400];
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(regions.len(), 1);
	let region = regions[&"a".id()];
	assert_eq!(region.span.start, 0x20_0000);
	assert_eq!(region.span.end, 0x20_0400);
}

#[test]
fn region_address() {
	let regions = setup("
		region a[1024] @ 0x20_0000;
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(regions.len(), 1);
	let region = regions[&"a".id()];
	assert_eq!(region.span.start, 0x20_0000);
	assert_eq!(region.span.end, 0x20_0400);
}

#[test]
fn region_with_expressions() {
	let regions = setup("
		value base = 0x4000;
		region a[base+0x2000..base * 2];
	").unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(regions.len(), 1);
	let region = regions[&"a".id()];
	assert_eq!(region.span.start, 0x6000);
	assert_eq!(region.span.end, 0x8000);
}
