
use crate::token;
use crate::Data;

pub fn error(data: &mut Data, msg: &str) -> ! {
	data.errors.push(msg.to_string());
	panic!("{data}")
}

pub fn expected(data: &mut Data, expected: &str, found: &str) -> ! {
	error(data, &format!("Expected {expected}, found {found}"))
}

pub fn expected_token(data: &mut Data, expected: &str, found: token::Kind) -> ! {
	error(data, &format!("Expected {expected}, found {found:?}"))
}

