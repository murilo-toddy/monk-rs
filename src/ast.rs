use crate::token::Token;

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Identifier { 
    pub token: Token,
    pub value: String,
}

#[derive(Clone, Eq, PartialEq, Debug)]
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

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expression {
    Identifier {
        token: Token,
        value: String,
    },
    Integer {
        token: Token,
        value: i64,
    },
    String {
        token: Token,
        value: String,
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
        operator: String,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    If {
        token: Token,
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
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
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Statement>);

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Precedence {
    Lowest = 1,
    Equals = 2,      // ==
    LessGreater = 3, // > or <
    Sum = 4,         // +
    Product = 5,     // *
    Prefix = 6,      // -x or !x
    Call = 7,        // func(x)
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
                write!(f, "{} {} = ", token, name)?;
                if let Some(value) = value {
                    write!(f, "{}", value)?;
                }
                write!(f, ";")?;
                Ok(())
            },
            Statement::Return { token, value } => {
                write!(f, "{} ", token)?;
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
            Expression::If { condition, consequence, alternative, .. } => {
                write!(f, "if {} {}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, "else {}", alt)?;
                }
                Ok(())
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
