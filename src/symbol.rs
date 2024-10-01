use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq, Clone)]
enum SymbolScope {
    Global,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Symbol {
    name: &'static str,
    scope: SymbolScope,
    pub index: usize,
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    store: HashMap<&'static str, Symbol>,
    definition_count: usize,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            definition_count: 0,
        }
    }

    pub fn define(&mut self, identifier: &'static str) -> Symbol {
        let symbol = Symbol {
            name: identifier,
            scope: SymbolScope::Global,
            index: self.definition_count,
        };
        self.store.insert(identifier, symbol.clone());
        self.definition_count += 1;
        symbol
    }

    pub fn resolve(&mut self, identifier: &'static str) -> Option<Symbol> {
        self.store.get(identifier).cloned()
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
        ]);
        let mut table = SymbolTable::new();
        let a = table.define("a");
        assert_eq!(a, expected["a"], "expected a={:?}, got={:?}", expected["a"], a);
        let b = table.define("b");
        assert_eq!(b, expected["b"], "expected b={:?}, got={:?}", expected["b"], b);
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
}
