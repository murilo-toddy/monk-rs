use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum SymbolScope {
    Builtin,
    Global,
    Local,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Symbol {
    name: &'static str,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolTable {
    store: HashMap<&'static str, Symbol>,
    pub definition_count: usize,

    pub outer: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            definition_count: 0,

            outer: None,
        }
    }

    pub fn enclosing(table: Rc<RefCell<SymbolTable>>) -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            definition_count: 0,

            outer: Some(table),
        }
    }

    pub fn define_builtin(&mut self, index: usize, name: &'static str) -> Symbol {
        let symbol = Symbol { name, index, scope: SymbolScope::Builtin };
        self.store.insert(name, symbol.clone());
        symbol
    }

    pub fn define(&mut self, identifier: &'static str) -> Symbol {
        let scope = match self.outer {
            Some(_) => SymbolScope::Local,
            None => SymbolScope::Global,
        };
        let symbol = Symbol {
            name: identifier,
            index: self.definition_count,
            scope,
        };
        self.store.insert(identifier, symbol.clone());
        self.definition_count += 1;
        symbol
    }

    pub fn resolve(&mut self, identifier: &'static str) -> Option<Symbol> {
        match self.store.get(identifier).cloned() {
            Some(symbol) => Some(symbol),
            None => match self.outer.as_ref() {
                Some(table) => table.borrow_mut().resolve(identifier),
                None => None,
            }
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod symbol_tests {
    use super::*;

    #[test]
    fn test_define() {
        let expected = HashMap::from([
            ("a", Symbol { name: "a", scope: SymbolScope::Global, index: 0 }),
            ("b", Symbol { name: "b", scope: SymbolScope::Global, index: 1 }),
            ("c", Symbol { name: "c", scope: SymbolScope::Local, index: 0 }),
            ("d", Symbol { name: "d", scope: SymbolScope::Local, index: 1 }),
            ("e", Symbol { name: "e", scope: SymbolScope::Local, index: 0 }),
            ("f", Symbol { name: "f", scope: SymbolScope::Local, index: 1 }),
        ]);
        let mut global = SymbolTable::new();
        let a = global.define("a");
        assert_eq!(a, expected["a"], "expected a={:?}, got={:?}", expected["a"], a);
        let b = global.define("b");
        assert_eq!(b, expected["b"], "expected b={:?}, got={:?}", expected["b"], b);

        let mut first_local = SymbolTable::enclosing(Rc::from(RefCell::new(global)));
        let c = first_local.define("c");
        assert_eq!(c, expected["c"], "expected c={:?}, got={:?}", expected["c"], c);
        let b = first_local.define("d");
        assert_eq!(b, expected["d"], "expected b={:?}, got={:?}", expected["b"], b);

        let mut second_local = SymbolTable::enclosing(Rc::from(RefCell::new(first_local)));
        let e = second_local.define("e");
        assert_eq!(e, expected["e"], "expected c={:?}, got={:?}", expected["e"], e);
        let f = second_local.define("f");
        assert_eq!(f, expected["f"], "expected b={:?}, got={:?}", expected["f"], f);
    }

    #[test]
    fn test_resolve() {
        let mut table = SymbolTable::new();
        table.define("a");
        table.define("b");

        let expected = vec![
            Symbol { name: "a", scope: SymbolScope::Global, index: 0 },
            Symbol { name: "b", scope: SymbolScope::Global, index: 1 },
        ];

        for symbol in expected {
            let result = table.resolve(symbol.name);
            match result {
                Some(result) => assert_eq!(result, symbol, "expected {} to resolve to {:?} but got {:?}", symbol.name, symbol, result),
                None => panic!("name {} not resolvable", symbol.name),
            }
        }
    }

    #[test]
    fn test_resolve_local() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut local = SymbolTable::enclosing(Rc::from(RefCell::new(global)));
        local.define("c");
        local.define("d");

        let expected = vec![
            Symbol { name: "a", scope: SymbolScope::Global, index: 0 },
            Symbol { name: "b", scope: SymbolScope::Global, index: 1 },
            Symbol { name: "c", scope: SymbolScope::Local, index: 0 },
            Symbol { name: "d", scope: SymbolScope::Local, index: 1 },
        ];

        for symbol in expected {
            let result = local.resolve(symbol.name);
            match result {
                Some(result) => assert_eq!(result, symbol, "expected {} to resolve to {:?} but got {:?}", symbol.name, symbol, result),
                None => panic!("name {} not resolvable", symbol.name),
            }
        }
    }

    #[test]
    fn test_nested_resolve_local() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::enclosing(Rc::from(RefCell::new(global)));
        first_local.define("c");
        first_local.define("d");

        let mut second_local = SymbolTable::enclosing(Rc::from(RefCell::new(first_local.clone())));
        second_local.define("e");
        second_local.define("f");

        let tests = vec![
            (
                first_local,
                vec![
                    Symbol { name: "a", scope: SymbolScope::Global, index: 0 },
                    Symbol { name: "b", scope: SymbolScope::Global, index: 1 },
                    Symbol { name: "c", scope: SymbolScope::Local, index: 0 },
                    Symbol { name: "d", scope: SymbolScope::Local, index: 1 },
                ],
            ),
            (
                second_local,
                vec![
                    Symbol { name: "a", scope: SymbolScope::Global, index: 0 },
                    Symbol { name: "b", scope: SymbolScope::Global, index: 1 },
                    Symbol { name: "e", scope: SymbolScope::Local, index: 0 },
                    Symbol { name: "f", scope: SymbolScope::Local, index: 1 },
                ],
            ),
        ];

        for (mut table, symbols) in tests {
            for symbol in symbols {
                let result = table.resolve(symbol.name);
                match result {
                    Some(result) => assert_eq!(result, symbol, "expected {} to resolve to {:?} but got {:?}", symbol.name, symbol, result),
                    None => panic!("name {} not resolvable", symbol.name),
                }
            }
        }
    }

    #[test]
    fn test_define_resolve_builtins() {
        let global = Rc::new(RefCell::from(SymbolTable::new()));
        let first_local = Rc::new(RefCell::from(SymbolTable::enclosing(Rc::clone(&global))));
        let second_local = Rc::new(RefCell::from(SymbolTable::enclosing(Rc::clone(&first_local))));

        let expected = [
            Symbol { name: "a", scope: SymbolScope::Builtin, index: 0 },
            Symbol { name: "c", scope: SymbolScope::Builtin, index: 1 },
            Symbol { name: "e", scope: SymbolScope::Builtin, index: 2 },
            Symbol { name: "f", scope: SymbolScope::Builtin, index: 3 },
        ];

        expected.iter().enumerate().for_each(|(i, s)| {
            global.borrow_mut().define_builtin(i, s.name);
        });

        for table in [global, first_local, second_local] {
            for expected_symbol in &expected {
                match table.borrow_mut().resolve(expected_symbol.name) {
                    Some(actual_symbol) => assert_eq!(expected_symbol.name, actual_symbol.name, "expected to resolve to {}, but got {}", expected_symbol.name, actual_symbol.name),
                    None => panic!("could not resolve name {}", expected_symbol.name),
                }
            }
        }
    }
}
