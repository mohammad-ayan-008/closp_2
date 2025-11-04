use std::collections::HashMap;

use crate::{
    expressions::Expression,
    statements::{self, Block, Function, Item, Parameter, Program, Statement, Type, Variable},
};

#[derive(Debug)]
pub struct Symbol {
    name: String,
    type_: Type,
    kind: SymbolKind,
    scope_depth: usize,
    is_used: bool,
}

#[derive(Debug)]
pub enum SymbolKind {
    Parameter,
    Variable,
    Function,
}
pub struct FunctionSignature {
    return_type: Type,
    params: Vec<Type>,
}

pub struct SemanticAnalyzer {
    scopes: Vec<HashMap<String, Symbol>>,
    functions: HashMap<String, FunctionSignature>,
    current_fn_return_ty: Option<Type>,
    current_scope: usize,
    main_func_count: usize,
    error: Vec<String>,
}
impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            functions: HashMap::new(),
            current_scope: 0,
            current_fn_return_ty: None,
            error: vec![],
            main_func_count: 0,
        }
    }

    pub fn enter_scope(&mut self) {
        let map = HashMap::new();
        self.scopes.push(map);
        self.current_scope += 1;
    }

    pub fn exit_scope(&mut self) {
        if self.current_scope > 0 {
            self.scopes.pop();
            self.current_scope -= 1;
        }
    }

    pub fn insert(&mut self, name: String, symbol: Symbol) -> Result<(), String> {
        if let Some(scope) = self.scopes.get(self.current_scope)
            && scope.contains_key(&name)
        {
            return Err(format!("Symbol '{}' is already defined ", name));
        }
        if let Some(scope) = self.scopes.get_mut(self.current_scope) {
            scope.insert(name, symbol);
        }
        Ok(())
    }

    pub fn insert_function(
        &mut self,
        name: String,
        funsig: FunctionSignature,
    ) -> Result<(), String> {
        if self.functions.contains_key(&name) {
            return Err(format!("Symbol '{}' already defined ", name));
        }
        self.functions.insert(name, funsig);
        Ok(())
    }

    pub fn look_up(&self, name: &str) -> Option<&Symbol> {
        for i in (0..=self.current_scope).rev() {

            if let Some(symbol) = self.scopes[i].get(name) {
                return Some(symbol);
            }
        }
        None
    }
    pub fn look_function(&self, name: &str) -> Option<&FunctionSignature> {
        self.functions.get(name)
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<String>> {
        for i in &program.items {
            self.collect_fun_details(i);
        }
        self.check_main_function();
        for items in &program.items {
            if let Item::Function(a) = items {
                self.analyze_function(a);
            }
        }
        if !self.error.is_empty() {
            return Err(self.error.clone());
        }
        Ok(())
    }

    pub fn collect_fun_details(&mut self, item: &Item) {
        match item {
            Item::Function(a) => {
                if a.name == "main" {
                    self.main_func_count += 1;
                }

                let params: Vec<Type> = a.params.iter().map(|p| p.type_.clone()).collect();
                let fn_sig = FunctionSignature {
                    return_type: a.return_type.clone(),
                    params,
                };
                if let Err(e) = self.insert_function(a.name.clone(), fn_sig) {
                    self.error.push(e);
                }
                let symbol = Symbol {
                    name: a.name.clone(),
                    type_: a.return_type.clone(),
                    kind: SymbolKind::Function,
                    scope_depth: 0,
                    is_used: false,
                };
                let _ = self.insert(a.name.clone(), symbol);
            }
        }
    }

    pub fn check_main_function(&mut self) {
        if self.main_func_count == 0 {
            self.error.push("No main func found".to_string());
        } else if self.main_func_count > 1 {
            self.error.push("Multiple main func found".to_string());
        }
    }

    pub fn analyze_function(&mut self, func: &Function) {
        if func.name == "main" && func.return_type != Type::Int {
            self.error
                .push("main function must return an int type".to_string());
        }
        self.current_fn_return_ty = Some(func.return_type.clone());
        self.enter_scope();

        for i in &func.params {
            let symbol = Symbol {
                name: i.name.clone(),
                type_: i.type_.clone(),
                is_used: false,
                kind: SymbolKind::Parameter,
                scope_depth: self.current_scope,
            };

            if let Err(e) = self.insert(i.name.clone(), symbol) {
                self.error.push(e);
            }
        }
        self.analyze_block(&func.body);
        self.exit_scope();
        self.current_fn_return_ty = None;
    }

    pub fn analyze_block(&mut self, block: &Block) {
        for i in &block.statements {
            self.analyze_statement(i);
        }
    }

    pub fn analyze_statement(&mut self, statement: &Statement) {
        match &statement {
            Statement::Variable(a) => self.analyze_var(a),
            Statement::Block(a) => todo!(),
            Statement::Return(a) => todo!(),
            Statement::Assignment(a) => todo!(),
            Statement::ExpressionStatement(a) => todo!(),
        }
    }
    pub fn analyze_var(&mut self, var: &Variable) {
        if let Some(a) = &var.expression
            && let Some(a) = self.analyze_expression(a)
            && var.data_type != a
        {
            self.error.push("type mismatch ".to_string());
        }

        let symbol = Symbol {
            name: var.name.clone(),
            type_: var.data_type.clone(),
            kind: SymbolKind::Variable,
            scope_depth: self.current_scope,
            is_used: false,
        };
        if let Err(e) = self.insert(var.name.clone(), symbol) {
            self.error.push(e);
        }
    }

    pub fn analyze_expression(&mut self, expression: &Expression) -> Option<Type> {
        match expression {
            Expression::Int_Literal(_) => Some(Type::Int),
            Expression::Float_Literal(_) => Some(Type::Float),
            Expression::String_Literal(_) => Some(Type::Str),
            Expression::Bool_Literal(_) => Some(Type::Boolean),
            Expression::Identifier(name) => {
                if let Some(a) = self.look_up(&name) {
                    let sym_type = &a.type_;
                    //a.is_used = true;
                    Some(sym_type.clone())
                } else {
                    self.error.push(format!("undeclared variable '{}'", name));
                    None
                }
            },
            Expression::Binary { lhs, op, rhs }=>{
                todo!()
            },
            _ => todo!(),
        }
    }
}
