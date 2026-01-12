
use crate::identifier::Identifier;

use super::*;

fn setup(source: &str) -> Result<RegionMap, String> {
	let input = crate::input::eval(file!().into(), source.into());
	let lex_data = crate::lexer::eval(source)
		.map_err(|e| e.display(&input))?;
	eval(&input, &lex_data, true)
		.map_err(|e| e.display(&input))
		.map(|data| data.regions)
}

#[test]
fn region() {
	let regions = setup("region a[1024] @ 0x0020_0000;")
		.unwrap_or_else(|e| panic!("{e}"));
	assert_eq!(regions.len(), 1);
	let region = regions[&"a".id()];
	assert_eq!(region.span.start, 0x20_0000);
	assert_eq!(region.span.end, 0x20_0400);
}
