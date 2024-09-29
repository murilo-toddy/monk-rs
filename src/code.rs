pub type Instructions = Vec<u8>;

fn format_instruction(def: Definition, operands: Vec<i64>) -> String {
    let operand_count = def.operand_widths.len();
    if operands.len() != operand_count {
        return format!("ERROR: operand len {} does not match definition {} for {}",
                       operands.len(), operand_count, def.name);
    }
    return match operand_count {
        0 => def.name.to_owned(),
        1 => format!("{} {}", def.name, operands[0]),
        _ => format!("ERROR: unhandled operand count for {}", def.name),
    };
}

pub fn disassemble(instructions: &Instructions) -> String {
    let mut out = String::new();
    let mut bytes_read = 0;

    while bytes_read < instructions.len() {
        if let Some(def) = lookup(instructions[bytes_read]) {
            let (operands, n) = read_operands(&def, instructions[bytes_read + 1..].to_vec());
            out.push_str(&format!("{:04} {}\n", bytes_read, format_instruction(def, operands)));
            bytes_read += (1 + n) as usize;
        } else {
            out.push_str(&format!("ERROR: definition for {} not found\n", instructions[bytes_read]));
            continue;
        }
    }
    return out;
}

#[derive(Debug, Clone)]
pub enum Opcode {
    OpConstant = 0,
    OpAdd = 1,
}

impl Opcode {
    pub fn from(v: u8) -> Option<Opcode> {
        match v {
            0 => Some(Opcode::OpConstant),
            1 => Some(Opcode::OpAdd),
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
        }),
        Opcode::OpAdd => Some(Definition {
            name: "OpAdd",
            operand_widths: vec![],
        }),
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

fn read_operands(def: &Definition, instructions: Instructions) -> (Vec<i64>, i64) {
    let mut operands = vec![0; def.operand_widths.len()];
    let mut offset = 0;

    for (i, width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => {
                operands[i] = u16::from_be_bytes(
                    instructions[offset..offset+2].try_into().unwrap()
                ) as i64;
            },
            _ => {}
        }
        offset += width;
    }
    return (operands, offset as i64);
}

#[cfg(test)]
mod code_tests {
    use super::*;
    
    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Opcode::OpAdd, vec![]),
            make(Opcode::OpConstant, vec![2]),
            make(Opcode::OpConstant, vec![65535]),
        ];

        let expected = "0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
";

        let instructions_concat: Instructions = instructions.into_iter().flatten().collect();
        let instructions_str = disassemble(&instructions_concat);
        assert_eq!(instructions_str, expected.to_owned(),
            "instructions wrongly formatted. expected {} to be {}", instructions_str, expected
        );
    }

    #[test]
    fn test_read_operands() {
        let tests = vec![
            (Opcode::OpConstant, vec![65535], 2),
        ];
        for (opcode, operands, bytes_read) in tests {
            let op = opcode.clone() as u8;
            let def = lookup(op).expect(format!("definition not found for {:?}", op).as_str());
            let instruction = make(opcode, operands.clone());
            let (operands_read, n) = read_operands(&def, instruction[1..].to_vec());
            assert_eq!(bytes_read, n, "expected {:?} bytes read but got {:?}", bytes_read, n);
            assert_eq!(operands, operands_read, "expected {:?} to equal {:?}", operands, operands_read);
        }
    }

    #[test]
    fn test_make() {
        let tests = vec![
            (Opcode::OpConstant, vec![65534], vec![Opcode::OpConstant as u8, 255 as u8, 254 as u8]),
            (Opcode::OpAdd, vec![], vec![Opcode::OpAdd as u8]),
        ];
        for (op, operands, expected) in tests {
            let instruction = make(op, operands);
            assert_eq!(instruction, expected, "expected instruction to be {:?} but got {:?}", expected, instruction);
        }
    }
}
