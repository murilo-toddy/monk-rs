use std::convert::TryInto;

pub type Instructions = Vec<u8>;

fn format_instruction(def: Definition, operands: Vec<i64>) -> String {
    let operand_count = def.operand_widths.len();
    if operands.len() != operand_count {
        return format!(
            "ERROR: operand len {} does not match definition {} for {}",
            operands.len(),
            operand_count,
            def.name
        );
    }
    match operand_count {
        0 => def.name.to_owned(),
        1 => format!("{} {}", def.name, operands[0]),
        2 => format!("{} {} {}", def.name, operands[0], operands[1]),
        _ => format!("ERROR: unhandled operand count for {}", def.name),
    }
}

pub fn disassemble(instructions: &Instructions) -> String {
    let mut out = String::new();
    let mut bytes_read = 0;

    while bytes_read < instructions.len() {
        if let Some(def) = lookup(instructions[bytes_read]) {
            let (operands, n) = read_operands(&def, instructions[bytes_read + 1..].to_vec());
            out.push_str(&format!(
                "{:04} {}\n",
                bytes_read,
                format_instruction(def, operands)
            ));
            bytes_read += (1 + n) as usize;
        } else {
            out.push_str(&format!(
                "ERROR: definition for {} not found\n",
                instructions[bytes_read]
            ));
            continue;
        }
    }
    out
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Opcode {
    Constant = 0,
    Pop = 1,
    Add = 2,
    Sub = 3,
    Mul = 4,
    Div = 5,
    True = 6,
    False = 7,
    Equal = 8,
    NotEqual = 9,
    GreaterThan = 10,
    Minus = 11,
    Bang = 12,
    Jump = 13,
    JumpNotTrue = 14,
    Null = 15,
    SetGlobal = 16,
    GetGlobal = 17,
    Array = 18,
    Hash = 19,
    Index = 20,
    Call = 21,
    Return = 22,
    ReturnValue = 23,
    GetLocal = 24,
    SetLocal = 25,
    GetBuiltin = 26,
    Closure = 27,
    GetFree = 28,
}

impl Opcode {
    pub fn from(v: u8) -> Option<Opcode> {
        match v {
            0 => Some(Opcode::Constant),
            1 => Some(Opcode::Pop),
            2 => Some(Opcode::Add),
            3 => Some(Opcode::Sub),
            4 => Some(Opcode::Mul),
            5 => Some(Opcode::Div),
            6 => Some(Opcode::True),
            7 => Some(Opcode::False),
            8 => Some(Opcode::Equal),
            9 => Some(Opcode::NotEqual),
            10 => Some(Opcode::GreaterThan),
            11 => Some(Opcode::Minus),
            12 => Some(Opcode::Bang),
            13 => Some(Opcode::Jump),
            14 => Some(Opcode::JumpNotTrue),
            15 => Some(Opcode::Null),
            16 => Some(Opcode::SetGlobal),
            17 => Some(Opcode::GetGlobal),
            18 => Some(Opcode::Array),
            19 => Some(Opcode::Hash),
            20 => Some(Opcode::Index),
            21 => Some(Opcode::Call),
            22 => Some(Opcode::Return),
            23 => Some(Opcode::ReturnValue),
            24 => Some(Opcode::GetLocal),
            25 => Some(Opcode::SetLocal),
            26 => Some(Opcode::GetBuiltin),
            27 => Some(Opcode::Closure),
            28 => Some(Opcode::GetFree),
            _ => None,
        }
    }
}

pub struct Definition {
    name: &'static str,
    operand_widths: Vec<usize>,
}

pub fn generate_definition(name: &'static str, operand_widths: Vec<usize>) -> Option<Definition> {
    Some(Definition {
        name,
        operand_widths,
    })
}

pub fn get_definition(opcode: &Opcode) -> Option<Definition> {
    match opcode {
        Opcode::Constant => generate_definition("OpConstant", vec![2]),
        Opcode::Pop => generate_definition("OpPop", vec![]),
        Opcode::Add => generate_definition("OpAdd", vec![]),
        Opcode::Sub => generate_definition("OpSub", vec![]),
        Opcode::Mul => generate_definition("OpMul", vec![]),
        Opcode::Div => generate_definition("OpDiv", vec![]),
        Opcode::True => generate_definition("OpTrue", vec![]),
        Opcode::False => generate_definition("OpFalse", vec![]),
        Opcode::Equal => generate_definition("OpEqual", vec![]),
        Opcode::NotEqual => generate_definition("OpNotEqual", vec![]),
        Opcode::GreaterThan => generate_definition("OpGreaterThan", vec![]),
        Opcode::Minus => generate_definition("OpMinus", vec![]),
        Opcode::Bang => generate_definition("OpBang", vec![]),
        Opcode::Jump => generate_definition("OpJump", vec![2]),
        Opcode::JumpNotTrue => generate_definition("OpJumpNotTrue", vec![2]),
        Opcode::Null => generate_definition("OpNull", vec![]),
        Opcode::SetGlobal => generate_definition("OpSetGlobal", vec![2]),
        Opcode::GetGlobal => generate_definition("OpGetGlobal", vec![2]),
        Opcode::Array => generate_definition("OpArray", vec![2]),
        Opcode::Hash => generate_definition("OpHash", vec![2]),
        Opcode::Index => generate_definition("OpHash", vec![]),
        Opcode::Call => generate_definition("OpCall", vec![1]),
        Opcode::Return => generate_definition("OpReturn", vec![]),
        Opcode::ReturnValue => generate_definition("OpReturnValue", vec![]),
        Opcode::GetLocal => generate_definition("OpGetLocal", vec![1]),
        Opcode::SetLocal => generate_definition("OpSetLocal", vec![1]),
        Opcode::GetBuiltin => generate_definition("OpGetBuiltin", vec![1]),
        Opcode::Closure => generate_definition("OpClosure", vec![2, 1]),
        Opcode::GetFree => generate_definition("OpClosure", vec![1]),
    }
}

pub fn lookup(op: u8) -> Option<Definition> {
    Opcode::from(op).and_then(|opcode| get_definition(&opcode))
}

pub fn make(op: Opcode, operands: Vec<i64>) -> Vec<u8> {
    match get_definition(&op) {
        Some(def) => vec![op as u8]
            .into_iter()
            .chain(
                operands
                    .into_iter()
                    .zip(def.operand_widths)
                    .flat_map(|(operand, width)| match width {
                        1 => (operand as u8).to_be_bytes().to_vec(),
                        2 => (operand as i16).to_be_bytes().to_vec(),
                        _ => vec![],
                    })
                    .collect::<Vec<u8>>(),
            )
            .collect(),
        None => vec![],
    }
}

fn read_operands(def: &Definition, instructions: Instructions) -> (Vec<i64>, i64) {
    let mut operands = vec![0; def.operand_widths.len()];
    let mut offset = 0;

    for (i, width) in def.operand_widths.iter().enumerate() {
        match width {
            1 => operands[i] = instructions[offset] as i64,
            2 => {
                operands[i] =
                    u16::from_be_bytes(instructions[offset..offset + 2].try_into().unwrap()) as i64
            }
            _ => {}
        }
        offset += width;
    }
    (operands, offset as i64)
}

#[cfg(test)]
mod code_tests {
    use super::*;

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Opcode::Add, vec![]),
            make(Opcode::Constant, vec![2]),
            make(Opcode::Constant, vec![65535]),
            make(Opcode::GetLocal, vec![1]),
            make(Opcode::Pop, vec![]),
            make(Opcode::Closure, vec![65535, 255]),
        ];

        let expected = "0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
0007 OpGetLocal 1
0009 OpPop
0010 OpClosure 65535 255
";

        let instructions_concat: Instructions = instructions.into_iter().flatten().collect();
        let instructions_str = disassemble(&instructions_concat);
        assert_eq!(
            instructions_str,
            expected.to_owned(),
            "instructions wrongly formatted. expected {} to be {}",
            instructions_str,
            expected
        );
    }

    #[test]
    fn test_read_operands() {
        let tests = vec![
            (Opcode::Constant, vec![65535], 2),
            (Opcode::GetLocal, vec![255], 1),
            (Opcode::Closure, vec![65535, 255], 3),
        ];
        for (opcode, operands, bytes_read) in tests {
            let op = opcode.clone() as u8;
            let def = lookup(op).expect(format!("definition not found for {:?}", op).as_str());
            let instruction = make(opcode, operands.clone());
            let (operands_read, n) = read_operands(&def, instruction[1..].to_vec());
            assert_eq!(
                bytes_read, n,
                "expected {:?} bytes read but got {:?}",
                bytes_read, n
            );
            assert_eq!(
                operands, operands_read,
                "expected {:?} to equal {:?}",
                operands, operands_read
            );
        }
    }

    #[test]
    fn test_make() {
        let tests = vec![
            (
                Opcode::Constant,
                vec![65534],
                vec![Opcode::Constant as u8, 255 as u8, 254 as u8],
            ),
            (Opcode::Add, vec![], vec![Opcode::Add as u8]),
            (
                Opcode::GetLocal,
                vec![255],
                vec![Opcode::GetLocal as u8, 255],
            ),
            (
                Opcode::Closure,
                vec![65534, 255],
                vec![Opcode::Closure as u8, 255 as u8, 254 as u8, 255 as u8],
            ),
        ];
        for (op, operands, expected) in tests {
            let instruction = make(op, operands);
            assert_eq!(
                instruction, expected,
                "expected instruction to be {:?} but got {:?}",
                expected, instruction
            );
        }
    }
}
