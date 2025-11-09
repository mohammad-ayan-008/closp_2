use std::collections::HashMap;

use crate::{
    expressions::{Binaryop, Expression, UnaryOP},
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
    ExternFunction,
}
#[derive(Clone)]
pub struct FunctionSignature {
    return_type: Type,
    params: Vec<Type>,
    is_var_args: bool,
}

pub struct SemanticAnalyzer {
    scopes: Vec<HashMap<String, Symbol>>,
    extern_table: HashMap<String, FunctionSignature>,
    functions: HashMap<String, FunctionSignature>,
    current_fn_return_ty: Option<Type>,
    current_scope: usize,
    main_func_count: usize,
    return_count: usize,
    error: Vec<String>,
}
impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        let signature = FunctionSignature {
            return_type: Type::Int,
            params: vec![Type::Str],
            is_var_args: true,
        };
        map.insert("printf".to_string(), signature);
        Self {
            return_count: 0,
            scopes: vec![HashMap::new()],
            functions: map.clone(),
            extern_table: map,
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
            return Err(format!("Symbol fn '{}' already defined ", name));
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
                    is_var_args: false,
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
        if self.return_count == 0 && func.return_type != Type::Void {
            self.error.push("Expected return stmt".to_string());
        }
        self.return_count = 0;
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
            Statement::Block(a) => {
                self.enter_scope();
                self.analyze_block(a);
                self.exit_scope();
            }
            Statement::Return(a) => {
                if a.is_none() {
                    self.error.push("expected return but not found".to_string());
                    return;
                }
                let exp_ty = self.analyze_expression(a.as_ref().unwrap());

                if self.current_fn_return_ty != exp_ty {
                    self.error.push(format!(
                        "Expected return '{:?}' got '{:?}'",
                        self.current_fn_return_ty, exp_ty
                    ));
                }
                if self.return_count == 0 {
                    self.return_count += 1;
                } else {
                    self.error.push("unreachable return ".to_lowercase());
                }
            }
            Statement::Assignment(a) => {
                if matches!(a.target, Expression::FunctionCall { name: _, args: _ }) {
                    self.error
                        .push("Cannot assign a function call statement".to_string());
                }
                if self.analyze_expression(&a.target) != self.analyze_expression(&a.value) {
                    self.error
                        .push("Cannot assign a type with different type".to_string());
                }
            }
            Statement::ExpressionStatement(a) => {
                self.analyze_expression(a);
            }
        }
    }
    pub fn analyze_var(&mut self, var: &Variable) {
        if let Some(a) = &var.expression
            && let Some(type_) = self.analyze_expression(a)
            && var.data_type != type_
        {
            self.error.push(format!(
                "type mismatch lhs '{:?}' rhs '{:?}'",
                var.data_type, type_
            ));
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
            Expression::String_Literal(_) => Some(Type::Pointer(Box::new(Type::Char))),
            Expression::Bool_Literal(_) => Some(Type::Boolean),
            Expression::Char_Literal(_) => Some(Type::Char),
            Expression::Identifier(name) => {
                if let Some(a) = self.look_up(name) {
                    let sym_type = &a.type_;
                    //a.is_used = true;
                    Some(sym_type.clone())
                } else {
                    self.error.push(format!("undeclared variable '{}'", name));
                    None
                }
            }
            Expression::Null => Some(Type::Null),
            Expression::Cast { expected, expr } => self.parse_cast(expected, expr),
            Expression::FunctionCall { name, args } => {
                let fn_sig = match self.functions.get(name) {
                    Some(a) => a,
                    None => {
                        self.error
                            .push(format!("Function '{}' not declared ", name));
                        return None;
                    }
                };

                let ret_ty = fn_sig.return_type.clone();

                //arity check
                if fn_sig.params.len() != args.len() && !fn_sig.is_var_args {
                    self.error.push(format!(
                        "Function '{}' expected '{}' git '{}' ",
                        name,
                        fn_sig.params.len(),
                        args.len()
                    ));
                    return None;
                }
                let args_fn = fn_sig.params.clone();
                let mul_arg = fn_sig.is_var_args;
                let args_ty: Vec<Option<Type>> =
                    args.iter().map(|a| self.analyze_expression(a)).collect();

                for (expr, expected_ty) in args_ty.iter().zip(args_fn) {
                    let exp_ty = match expr {
                        Some(a) => a,
                        None => {
                            self.error.push(format!("Uknown type '{:?}'", expr));
                            return None;
                        }
                    };

                    if mul_arg {
                        break;
                    } else if *exp_ty != expected_ty {
                        self.error.push(format!(
                            "Expected type '{:?}'  found '{:?}'",
                            expected_ty, exp_ty
                        ));
                        return None;
                    }
                }

                Some(ret_ty)

                // let fn_params_ty= args.iter().map(|a| self.analyze_expression(a).clone()).collect::<Vec<Option<Type>>>();
                //
                // let fns = self.functions.get(name);
                //
                // if let Some(a) = fns{
                //     let ret_type = a.return_type.clone();
                //     if args.len() != fn_params_ty.len(){
                //        self.error.push(format!("Function '{}' args mismatch",name));
                //        return None;
                //     }
                //
                //     for (i,ty) in a.params.iter().enumerate(){
                //         if fn_params_ty[i].as_ref() != Some(ty){
                //           self.error.push(format!("Function '{}' args mismatch",name));
                //           return None;
                //         }
                //     }
                //     Some(ret_type)
                // }else {
                //     self.error.push(format!("Function '{}' did not declared",name));
                //     None
                // }
            }
            Expression::Unary { token, exp } => {
                let expr = self.analyze_expression(exp);
                self.analyze_unary(token, expr)
            }
            Expression::Binary { lhs, op, rhs } => {
                let lhs = self.analyze_expression(lhs);
                let rhs = self.analyze_expression(rhs);
                self.analyze_binary_expression(lhs, op, rhs)
            }
        }
    }
    pub fn parse_cast(&mut self, expected: &Type, expr: &Expression) -> Option<Type> {
        let exp_ty = self.analyze_expression(expr);
        match expected {
            Type::Void => {
                self.error.push("Cannot cast to void".to_string());
                None
            }
            Type::Int => {
                if !matches!(exp_ty, Some(Type::Pointer(_) | Type::Int | Type::Char)) {
                    self.error.push("Cannot cast to int type".to_string());
                    return None;
                }
                Some(Type::Int)
            }
            _ => Some(expected.clone()),
        }
    }
    pub fn analyze_unary(&mut self, op: &UnaryOP, expression: Option<Type>) -> Option<Type> {
        match (op, expression) {
            (UnaryOP::Negate, Some(Type::Int)) => Some(Type::Int),
            (UnaryOP::Negate, Some(Type::Float)) => Some(Type::Float),

            (UnaryOP::Not, Some(Type::Int)) => Some(Type::Int),
            (UnaryOP::Not, Some(Type::Boolean)) => Some(Type::Boolean),

            (UnaryOP::Adressof, Some(a)) => Some(Type::Pointer(Box::new(a))),
            (UnaryOP::Dereference, Some(Type::Pointer(a))) => Some(*a),
            (UnaryOP::Dereference, Some(_non_ptr)) => {
                self.error.push("Expected a pointer type".to_string());
                None
            }

            (_, _) => None,
        }
    }
    pub fn analyze_binary_expression(
        &mut self,
        lhs: Option<Type>,
        op: &Binaryop,
        rhs: Option<Type>,
    ) -> Option<Type> {
        match (lhs, op, rhs) {
            (Some(Type::Int), Binaryop::ADD, Some(Type::Int)) => Some(Type::Int),
            (Some(Type::Int), Binaryop::DIV, Some(Type::Int)) => Some(Type::Int),
            (Some(Type::Int), Binaryop::SUB, Some(Type::Int)) => Some(Type::Int),
            (Some(Type::Int), Binaryop::MUL, Some(Type::Int)) => Some(Type::Int),

            (Some(Type::Int), Binaryop::BITAND, Some(Type::Int)) => Some(Type::Int),
            (Some(Type::Int), Binaryop::BITOR, Some(Type::Int)) => Some(Type::Int),

            (Some(Type::Int | Type::Float), Binaryop::GT, Some(Type::Int | Type::Float)) => {
                Some(Type::Boolean)
            }
            (Some(Type::Int | Type::Float), Binaryop::LT, Some(Type::Int | Type::Float)) => {
                Some(Type::Boolean)
            }
            (Some(Type::Int | Type::Float), Binaryop::LTE, Some(Type::Int | Type::Float)) => {
                Some(Type::Boolean)
            }
            (Some(Type::Int | Type::Float), Binaryop::GTE, Some(Type::Int | Type::Float)) => {
                Some(Type::Boolean)
            }
            (
                Some(Type::Int | Type::Float),
                Binaryop::EqualEqual,
                Some(Type::Int | Type::Float),
            ) => Some(Type::Boolean),
            (Some(Type::Int | Type::Float), Binaryop::NotEq, Some(Type::Int | Type::Float)) => {
                Some(Type::Boolean)
            }

            (Some(Type::Int), Binaryop::ADD, Some(Type::Float)) => Some(Type::Float),
            (Some(Type::Float), Binaryop::ADD, Some(Type::Float)) => Some(Type::Float),

            (Some(Type::Int), Binaryop::ADD, Some(Type::Char)) => Some(Type::Int),
            (Some(Type::Char), Binaryop::ADD, Some(Type::Int)) => Some(Type::Int),

            (Some(Type::Boolean), Binaryop::ADD, Some(Type::Int)) => Some(Type::Int),
            (Some(Type::Int), Binaryop::ADD, Some(Type::Boolean)) => Some(Type::Int),

            (_, _, _) => None,
        }
    }
}
