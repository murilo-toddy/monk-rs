use crate::token::Token;

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct Identifier { 
    pub token: Token,
    pub value: &'static str,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Option<Expression>,
    },
    Return {
        token: Token,
        value: Option<Expression>,
    },
    Expression {
        token: Token,
        expression: Option<Expression>,
    },
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum Expression {
    Identifier {
        token: Token,
        value: &'static str,
    },
    Integer {
        token: Token,
        value: i64,
    },
    String {
        token: Token,
        value: &'static str,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    Prefix {
        token: Token,
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        token: Token,
        operator: &'static str,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    If {
        token: Token,
        conditions: Vec<(Expression, BlockStatement)>,
        alternative: Option<BlockStatement>,
    },
    While {
        token: Token,
        condition: Box<Expression>,
        statement: BlockStatement,
    },
    For {
        token: Token,
        declaration: Box<Statement>,
        condition: Box<Expression>,
        operation: Box<Expression>,
        statement: BlockStatement,
    },
    Function {
        token: Token,
        arguments: Vec<Identifier>,
        body: BlockStatement,
    },
    Call {
        token: Token, // Token::Lparen
        function: Box<Expression>, // Idenifier or Function
        arguments: Vec<Expression>,
    },
    Array {
        token: Token, // Token::Lbracket
        elements: Vec<Expression>,
    },
    Hash {
        token: Token,
        pairs: Vec<(Expression, Expression)>
    },
    Index {
        token: Token, // Token::Lbracket
        left: Box<Expression>,
        index: Box<Expression>,
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Statement>);

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Precedence {
    Lowest = 1,
    Assign = 2,      // =
    Or = 3,          // ||
    And = 4,         // &&
    BitOr = 5,       // |
    BitXor = 6,      // ^
    BitAnd = 7,      // &
    Equals = 8,      // ==
    LessGreater = 9, // > or <
    BitShift = 10,   // >> or <<
    Sum = 11,        // +
    Product = 12,    // *
    Prefix = 13,     // -x or !x
    Call = 14,       // func(x)
    Index = 15,      // arr[i]
}


impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let { token, name, value } => {
                write!(f, "{} {} = ", token.as_string(), name)?;
                if let Some(value) = value {
                    write!(f, "{}", value)?;
                }
                write!(f, ";")?;
                Ok(())
            },
            Statement::Return { value, .. } => {
                if let Some(value) = value {
                    write!(f, "{}", value)?;
                }
                Ok(())
            },
            Statement::Expression { expression, .. } => {
                if let Some(expression) = expression {
                    write!(f, "{}", expression)?;
                }
                Ok(())
            },
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier { value, .. } => write!(f, "{}", value),
            Expression::Integer { value, .. } => write!(f, "{}", value),
            Expression::String { value, .. }  => write!(f, "{}", value),
            Expression::Boolean { value, .. } => write!(f, "{}", value),
            Expression::Prefix { operator, right, .. } => {
                write!(f, "({}{})", operator, right)
            },
            Expression::Infix { left, operator, right, .. } => {
                write!(f, "({} {} {})", left, operator, right)
            },
            Expression::If { conditions, alternative, .. } => {
                write!(f, "{}", conditions
                    .iter()
                    .map(|(condition, consequence)| format!("if {} {}", condition, consequence))
                    .collect::<Vec<String>>()
                    .join("else "))?;
                if let Some(alt) = alternative {
                    write!(f, "else {}", alt)?;
                }
                Ok(())
            },
            Expression::While { condition, statement, .. } => {
                write!(f, "while ({}) {}", condition, statement)
            },
            Expression::For { declaration, condition, operation, statement, .. } => {
                write!(f, "for ({}; {}; {}) {}", declaration, condition, operation, statement)
            },
            Expression::Function { arguments, body, .. } => {
                write!(f, "fn(")?;
                write!(f, "{}", arguments
                    .iter()
                    .map(|i| format!("{}", i))
                    .collect::<Vec<String>>()
                    .join(", ").as_str())?;

                write!(f, ") {}", body)
            },
            Expression::Call { function, arguments, .. } => {
                write!(f, "{}(", function)?;
                write!(f, "{}", arguments
                    .iter()
                    .map(|i| format!("{}", i))
                    .collect::<Vec<String>>()
                    .join(", "))?;

                write!(f, ")")
            },
            Expression::Array { elements, .. } => {
                write!(f, "[{}]", elements
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<String>>()
                    .join(", "))
            },
            Expression::Hash { pairs, .. } => {
                write!(f, "{{{}}}", pairs 
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", "))
            },
            Expression::Index { left, index, .. } => {
                write!(f, "({}[{}])", left, index)
            }
        }
    }
}

impl std::fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.0 {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}
