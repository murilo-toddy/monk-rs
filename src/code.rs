use std::collections::HashMap;

pub type Instructions = Vec<u8>;

pub enum Opcode {
    OpConstant = 0,
}

impl Opcode {
    pub fn from(v: u8) -> Option<Opcode> {
        match v {
            0 => Some(Opcode::OpConstant),
            _ => None,
        }
    }
}

pub struct Definition {
    name: &'static str,
    operand_widths: Vec<usize>,
}

pub fn get_definition(opcode: &Opcode) -> Option<Definition> {
    match opcode {
        Opcode::OpConstant => Some(Definition {
            name: "OpConstant",
            operand_widths: vec![2],
        })
    }
}

pub fn lookup(op: u8) -> Option<Definition> {
    Opcode::from(op).map(|opcode| get_definition(&opcode)).flatten()
}

pub fn make(op: Opcode, operands: Vec<i64>) -> Vec<u8> {
    return match get_definition(&op) {
        Some(def) => 
            vec![op as u8].into_iter().chain(
                operands
                    .into_iter()
                    .zip(def.operand_widths.into_iter())
                    .flat_map(|(operand, width)|
                        match width {
                            2 => (operand as i16).to_be_bytes().to_vec(),
                            _ => vec![],
                        }
                    ).collect::<Vec<u8>>()
            ).collect(),
        None => return vec![],
    };
}

#[cfg(test)]
mod code_tests {
    use super::*;
    
    #[test]
    fn test_make() {
        let tests = vec![
            (Opcode::OpConstant, vec![65534], vec![Opcode::OpConstant as u8, 255 as u8, 254 as u8])
        ];
        for (op, operands, expected) in tests {
            let instruction = make(op, operands);
            assert_eq!(instruction, expected, "expected instruction to be {:?} but got {:?}", expected, instruction);
        }
    }
}
