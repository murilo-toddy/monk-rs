#[derive(Debug, Eq, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null
}

impl Object {
    fn get_type(&self) -> String {
        format!("{:?}", self)
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(value) => value.to_string(),
            Object::Boolean(value) => value.to_string(),
            Object::Null => "null".to_owned(),
        }
    }
}
