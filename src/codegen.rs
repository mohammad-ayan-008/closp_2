use std::{collections::HashMap, env::set_var, ops::Deref};

use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode,
        PointerValue,
    },
};

use crate::{
    expressions::{self, Expression, UnaryOP},
    semantics,
    statements::{Block, Function, Item, Program, Statement, Type},
};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    builder: Builder<'ctx>,
    scope: Vec<
        HashMap<
            String,
            (
                PointerValue<'ctx>,
                Type,
                BasicTypeEnum<'ctx>,
                BasicValueEnum<'ctx>,
            ),
        >,
    >,
    current_fn: Option<FunctionValue<'ctx>>,
    functions: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn enter_scope(&mut self) {
        self.scope.push(HashMap::new());
    }
    pub fn exit_scope(&mut self) {
        self.scope.pop();
    }

    pub fn declare_variable(
        &mut self,
        name: String,
        type_: Type,
        type_enum: BasicTypeEnum<'ctx>,
        pointer_value: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) {
        if let Some(a) = self.scope.last_mut() {
            a.insert(name, (pointer_value, type_, type_enum, value));
        }
    }

    pub fn look_up(
        &self,
        name: &str,
    ) -> Option<(
        PointerValue<'ctx>,
        Type,
        BasicTypeEnum<'ctx>,
        BasicValueEnum<'ctx>,
    )> {
        for (i, scope) in self.scope.iter().rev().enumerate() {
            let keys: Vec<_> = scope.keys().cloned().collect();

            if let Some(entry) = scope.get(name) {
                return Some(entry.clone());
            }
        }

        None
    }

    pub fn llvm_type(&mut self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, String> {
        match ty {
            Type::Int => Ok(self.context.i64_type().into()),

            Type::Char => Ok(self.context.i8_type().into()),
            Type::Boolean => Ok(self.context.bool_type().into()),

            Type::Pointer(_inner) => Ok(self.context.ptr_type(AddressSpace::default()).into()),

            Type::Null => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Str => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Float => Ok(self.context.f64_type().into()),
            Type::Void => Err("Void dont have a recognised type".to_string()),
        }
    }

    pub fn printf_fn(&mut self) {
        let ptr = self.context.ptr_type(AddressSpace::default());
        let type_ = self.context.i64_type().fn_type(&[ptr.into()], true);
        self.module.add_function("printf", type_, None);
    }

    pub fn new(context: &'ctx Context, mod_name: String) -> Self {
        let module = context.create_module(&mod_name);
        Self {
            context,
            module,
            builder: context.create_builder(),
            scope: vec![HashMap::new()],
            current_fn: None,
            functions: HashMap::new(),
        }
    }
    pub fn fun_ret_type(&mut self, ty: &Type) -> Result<Option<BasicTypeEnum<'ctx>>, String> {
        match ty {
            Type::Void => Ok(None),
            _ => Ok(Some(self.llvm_type(ty)?)),
        }
    }
    pub fn generate(&mut self, program: &Program) {
        self.printf_fn();
        for i in &program.items {
            match i {
                crate::statements::Item::Function(a) => {
                    self.declare_function(a).unwrap();
                }
            }
        }

        for item in &program.items {
            if let Item::Function(func) = item {
                self.generate_function(func);
            }
        }
    }
    pub fn generate_function(&mut self, f: &Function) {
        let func = self.module.get_function(&f.name).unwrap();
        self.current_fn = Some(func);

        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);
        self.enter_scope();
        for (i, j) in f.params.iter().enumerate() {
            let param_ll = func.get_nth_param(i as u32).unwrap();
            let type_ = self.llvm_type(&j.type_).unwrap();
            let alloc = self.builder.build_alloca(type_, "nasty").unwrap();
            self.builder.build_store(alloc, param_ll).unwrap();
            self.declare_variable(j.name.clone(), j.type_.clone(), type_, alloc, param_ll);
        }
        self.generate_block(&f.body);
        self.exit_scope();
        if f.return_type == Type::Void
            && let Some(a) = self.builder.get_insert_block()
            && a.get_terminator().is_none()
        {
            self.builder.build_return(None).unwrap();
        }
        self.mem_2_reg_pass(func);
        self.functions.insert(f.name.to_string(), func);
    }

    pub fn generate_block(&mut self, block: &Block) {
        for i in &block.statements {
            self.generate_stmts(i);
        }
    }

    pub fn generate_stmts(&mut self, st: &Statement) {
        match st {
            Statement::WhileStmt(a) => {
                let fn_ = self.current_fn.unwrap();
                let while_condi = self.context.append_basic_block(fn_, "while_condi");
                let while_body = self.context.append_basic_block(fn_, "while_body");
                let while_end = self.context.append_basic_block(fn_, "while_end");

                self.builder
                    .build_unconditional_branch(while_condi)
                    .unwrap();

                self.builder.position_at_end(while_condi);
                let con_value = self.compile_expressions(&a.condition).unwrap();
                let con_val = con_value.into_int_value();
                self.builder
                    .build_conditional_branch(con_val, while_body, while_end)
                    .unwrap();

                self.builder.position_at_end(while_body);
                self.enter_scope();
                self.generate_block(&a.body);
                self.exit_scope();

                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.builder
                        .build_unconditional_branch(while_condi)
                        .unwrap();
                }
                self.builder.position_at_end(while_end);
            }
            Statement::IFStmt(a) => {
                let fn_ = self.current_fn.unwrap();

                let condition_val = self.compile_expressions(&a.condition).unwrap();
                let con_val = condition_val.into_int_value();

                let then_block = self.context.append_basic_block(fn_, "the_block");
                let else_block = self.context.append_basic_block(fn_, "else_block");
                let merge_block = self.context.append_basic_block(fn_, "merge_block");

                self.builder
                    .build_conditional_branch(con_val, then_block, else_block)
                    .unwrap();

                self.builder.position_at_end(then_block);
                self.enter_scope();
                self.generate_block(&a.then_block);

                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.builder
                        .build_unconditional_branch(merge_block)
                        .unwrap();
                }
                self.exit_scope();

                self.builder.position_at_end(else_block);
                if let Some(a) = &a.else_block {
                    self.enter_scope();
                    self.generate_block(a);
                    self.exit_scope();
                }
                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.builder
                        .build_unconditional_branch(merge_block)
                        .unwrap();
                }
                self.builder.position_at_end(merge_block);
            }
            Statement::ExpressionStatement(a) => {
                self.compile_expressions(a).unwrap();
            }
            Statement::Return(a) => {
                if a.is_none() {
                    self.builder.build_return(None).unwrap();
                } else {
                    let expression = self.compile_expressions(a.as_ref().unwrap()).unwrap();
                    self.builder.build_return(Some(&expression)).unwrap();
                }
            }
            Statement::Block(a) => {
                self.enter_scope();
                for i in &a.statements {
                    self.generate_stmts(i);
                }
                self.exit_scope();
            }
            Statement::Variable(a) => {
                let name = a.name.clone();
                let type_ = &a.data_type;
                let expr = self
                    .compile_expressions(a.expression.as_ref().unwrap())
                    .unwrap();
                let ty = self.llvm_type(type_).unwrap();
                let alloc = self.builder.build_alloca(ty, &name.clone()).unwrap();
                self.builder.build_store(alloc, expr).unwrap();
                self.declare_variable(name.clone(), a.data_type.clone(), ty, alloc, expr);
            }
            Statement::Assignment(a) => {
                let expr = self.compile_expressions(&a.value).unwrap();
                match &a.target {
                    Expression::Identifier(ide) => {
                        let look = self.look_up(ide).unwrap().clone();
                        self.builder.build_store(look.0, expr).unwrap();
                    }

                    Expression::Unary { token: _, exp } => {
                        if let Expression::Identifier(name) = &**exp {
                            let (ptr_to_ptr, _, _, _) = self
                                .look_up(name.as_str())
                                .ok_or_else(|| format!("pointer variable {} not found", name))
                                .unwrap();

                            let ptr_element_type = ptr_to_ptr.get_type(); // BasicTypeEnum

                            let loaded_ptr_val = self
                                .builder
                                .build_load(ptr_element_type, ptr_to_ptr, "load_ptr")
                                .unwrap();

                            let loaded_ptr = loaded_ptr_val.into_pointer_value();

                            self.builder.build_store(loaded_ptr, expr).unwrap();
                        } else {
                            let val = self.compile_expressions(exp).unwrap();
                        }
                    }
                    _ => todo!("{:?}", a.target),
                }
            }
            _ => panic!("no rule"),
        }
    }

    fn resolve_lhs_ptr(&self, target: &Expression) -> PointerValue<'ctx> {
        match target {
            Expression::Identifier(name) => {
                let (ptr, _, _, _) = self.look_up(name).unwrap();
                ptr
            }

            Expression::Unary { token: _, exp } => {
                // recursively get pointer for inner expression
                let inner_ptr = self.resolve_lhs_ptr(exp);

                // load one level of indirection
                let ty = inner_ptr.get_type();

                let loaded_val = self
                    .builder
                    .build_load(ty, inner_ptr, "deref_load")
                    .unwrap();

                loaded_val.into_pointer_value()
            }

            _ => panic!("Unsupported LHS assignment target: {:?}", target),
        }
    }
    pub fn mem_2_reg_pass(&mut self, func: FunctionValue<'ctx>) {
        //let manager = PassManager::create(&self.module);
        // manager.
        //  manager.initialize();
        // manager.run_on(&func);
    }
    pub fn deref_ptr(&mut self, exo: &Expression, level: i32) -> BasicValueEnum<'ctx> {
        match exo {
            Expression::Unary {
                token: UnaryOP::Dereference,
                exp,
            } => self.deref_ptr(exp, level + 1),
            Expression::Identifier(a) => {
                let (pv, ty, btype, value) = self.look_up(a).unwrap();
                let mut val = self.builder.build_load(btype, pv, "deref").unwrap();
                let mut ity = &ty;
                for i in 0..=level {
                    if let Type::Pointer(a) = ity {
                        let type_ll = self.llvm_type(&a).unwrap();
                        val = self
                            .builder
                            .build_load(type_ll, val.into_pointer_value(), "deref ptr")
                            .unwrap();
                        ity = &*a;
                    } else {
                        panic!("expected ptrtype");
                    }
                }
                return val;
            }

            _ => todo!("{:?}", exo),
        }
    }
    pub fn compile_expressions(
        &mut self,
        expr: &Expression,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            Expression::Float_Literal(a) => {
                let val = self.context.f64_type().const_float(*a);
                Ok(val.into())
            }
            Expression::Int_Literal(a) => {
                let val = self.context.i64_type().const_int(*a as u64, true);
                Ok(val.into())
            }
            Expression::Char_Literal(a) => {
                let val = self.context.i8_type().const_int(*a as u64, false);
                Ok(val.into())
            }
            Expression::Unary { token, exp } => {
                match &token {
                    expressions::UnaryOP::Adressof => {
                        let mut exps = exp.deref();

                        if let Expression::Unary { token, exp } = exps {
                            if matches!(**exp,Expression::Unary { ref token, ref exp }) {
                                return self.compile_expressions(exp);
                            } else {
                                exps = exp;
                            }
                        }

                        if let Expression::Identifier(ident) = exps {
                            let ptr = self
                                .look_up(ident)
                                .ok_or_else(|| format!("not found {:?}", ident))?;
                            Ok(ptr.0.as_basic_value_enum()) // pointer i64*
                        } else {
                            Err("Expected identifier".to_string())
                        }
                    }
                    expressions::UnaryOP::Not => {
                        let exps = self.compile_expressions(exp)?;
                        match exps {
                            BasicValueEnum::IntValue(a) => {
                                Ok(self.builder.build_not(a, "not").unwrap().into())
                            }

                            _ => Err("Not supported".into()),
                        }
                    }
                    expressions::UnaryOP::Dereference => {
                        let mut exps = exp.deref();
                        Ok(self.deref_ptr(exps, 0))
                    }
                    _ => todo!(),
                }
            }
            Expression::String_Literal(a) => {
                let unescaped = a
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\\"", "\"")
                    .replace("\\\\", "\\");
                let str = self
                    .builder
                    .build_global_string_ptr(&unescaped, "str_def")
                    .unwrap();
                Ok(str.as_basic_value_enum())
            }
            Expression::Bool_Literal(a) => {
                let val = self.context.bool_type().const_int(*a as u64, false);
                Ok(val.into())
            }

            Expression::FunctionCall { name, args } => {
                let args: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|a| self.compile_expressions(a).map(|a| a.into()))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                if let Some(func) = self.module.get_function(name) {
                    let val = self.builder.build_call(func, &args, "fn call").unwrap();

                    if func.get_type().get_return_type().is_some() {
                        Ok(val.try_as_basic_value().left().unwrap())
                    } else {
                        let null = self.context.ptr_type(AddressSpace::default());
                        let val = null.const_null();
                        Ok(val.as_basic_value_enum())
                    }
                } else {
                    Err("func not found".to_string())
                }
            }
            Expression::Cast { expected, expr } => {
                let exp = self.compile_expressions(expr)?;
                let ty = self.llvm_type(expected)?;
                match (exp, ty) {
                    (BasicValueEnum::IntValue(i), BasicTypeEnum::IntType(dst)) => {
                        let sw = i.get_type().get_bit_width();
                        let dw = dst.get_bit_width();

                        if sw == dw {
                            Ok(i.into())
                        } else if dw > sw {
                            Ok(self
                                .builder
                                .build_int_z_extend(i, dst, "zext")
                                .unwrap()
                                .into())
                        } else {
                            Ok(self
                                .builder
                                .build_int_truncate(i, dst, "trunc")
                                .unwrap()
                                .into())
                        }
                    }
                    (BasicValueEnum::IntValue(i), BasicTypeEnum::FloatType(j)) => Ok(self
                        .builder
                        .build_signed_int_to_float(i, j, "fcast")
                        .unwrap()
                        .into()),
                    (BasicValueEnum::FloatValue(a), BasicTypeEnum::IntType(j)) => Ok(self
                        .builder
                        .build_float_to_signed_int(a, j, "icast")
                        .unwrap()
                        .into()),
                    _ => Err(format!("un supported cast ")),
                }
            }

            Expression::Binary { lhs, op, rhs } => {
                let lhs = self.compile_expressions(lhs)?;
                let rhs = self.compile_expressions(rhs)?;
                let is_float = lhs.is_float_value() || rhs.is_float_value();
                let is_pointer = lhs.is_pointer_value() || rhs.is_pointer_value();
                match op {
                    crate::expressions::Binaryop::ADD => {
                        if is_float {
                            Ok(self
                                .builder
                                .build_float_add(
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "fadd",
                                )
                                .unwrap()
                                .into())
                        } else {
                            Ok(self
                                .builder
                                .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "fadd")
                                .unwrap()
                                .into())
                        }
                    }
                    crate::expressions::Binaryop::SUB => {
                        if is_float {
                            Ok(self
                                .builder
                                .build_float_sub(
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "fadd",
                                )
                                .unwrap()
                                .into())
                        } else {
                            Ok(self
                                .builder
                                .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "fadd")
                                .unwrap()
                                .into())
                        }
                    }
                    expressions::Binaryop::LT => {
                        if is_float {
                            Ok(self
                                .builder
                                .build_float_compare(
                                    FloatPredicate::OLT,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "f_cmp",
                                )
                                .unwrap()
                                .into())
                        } else {
                            Ok(self
                                .builder
                                .build_int_compare(
                                    IntPredicate::SLT,
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "f_cmp",
                                )
                                .unwrap()
                                .into())
                        }
                    }
                    expressions::Binaryop::GT => {
                        if is_float {
                            Ok(self
                                .builder
                                .build_float_compare(
                                    FloatPredicate::OGT,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "f_cmp",
                                )
                                .unwrap()
                                .into())
                        } else {
                            Ok(self
                                .builder
                                .build_int_compare(
                                    IntPredicate::SGT,
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "f_cmp",
                                )
                                .unwrap()
                                .into())
                        }
                    }
                    expressions::Binaryop::LTE => {
                        if is_float {
                            Ok(self
                                .builder
                                .build_float_compare(
                                    FloatPredicate::OLE,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "f_cmp",
                                )
                                .unwrap()
                                .into())
                        } else {
                            Ok(self
                                .builder
                                .build_int_compare(
                                    IntPredicate::SLE,
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "f_cmp",
                                )
                                .unwrap()
                                .into())
                        }
                    }
                    expressions::Binaryop::GTE => {
                        if is_float {
                            Ok(self
                                .builder
                                .build_float_compare(
                                    FloatPredicate::OGE,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "f_cmp",
                                )
                                .unwrap()
                                .into())
                        } else {
                            Ok(self
                                .builder
                                .build_int_compare(
                                    IntPredicate::SGE,
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "f_cmp",
                                )
                                .unwrap()
                                .into())
                        }
                    }
                    expressions::Binaryop::NotEq | expressions::Binaryop::EqualEqual => {
                        if is_pointer {
                            let pred = match op {
                                expressions::Binaryop::NotEq => IntPredicate::NE,
                                expressions::Binaryop::EqualEqual => IntPredicate::EQ,
                                _ => {
                                    return Err("uknown operator expected != or ==".to_string());
                                }
                            };

                            let lhs_val = if lhs.is_pointer_value() {
                                self.builder
                                    .build_ptr_to_int(
                                        lhs.into_pointer_value(),
                                        self.context.i32_type(),
                                        "lol",
                                    )
                                    .unwrap()
                            } else {
                                lhs.into_int_value()
                            };

                            let rhs_val = if rhs.is_pointer_value() {
                                self.builder
                                    .build_ptr_to_int(
                                        rhs.into_pointer_value(),
                                        self.context.i32_type(),
                                        "ptr_cmp",
                                    )
                                    .unwrap()
                            } else {
                                rhs.into_int_value()
                            };
                            Ok(self
                                .builder
                                .build_int_compare(pred, lhs_val, rhs_val, "ptr_cmp")
                                .unwrap()
                                .as_basic_value_enum())
                        } else if is_float {
                            let pred = match op {
                                expressions::Binaryop::NotEq => FloatPredicate::ONE,
                                expressions::Binaryop::EqualEqual => FloatPredicate::OEQ,
                                _ => {
                                    return Err("uknown operator expected != or ==".to_string());
                                }
                            };
                            Ok(self
                                .builder
                                .build_float_compare(
                                    pred,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    "float_cmp",
                                )
                                .unwrap()
                                .as_basic_value_enum())
                        } else {
                            let pred = match op {
                                expressions::Binaryop::NotEq => IntPredicate::NE,
                                expressions::Binaryop::EqualEqual => IntPredicate::EQ,
                                _ => {
                                    return Err("uknown operator expected != or ==".to_string());
                                }
                            };

                            Ok(self
                                .builder
                                .build_int_compare(
                                    pred,
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    "float_cmp",
                                )
                                .unwrap()
                                .as_basic_value_enum())
                        }
                    }
                    _ => todo!(),
                }
            }

            Expression::Identifier(name) => {
                if let Some((alloc_ptr, ty, ty2, _cached_value)) = self.look_up(name) {
                    let ty = self.llvm_type(&ty).unwrap();
                    let loaded = {
                        self.builder
                            .build_load(ty, alloc_ptr, &format!("load_{name}"))
                            .unwrap()
                    };
                    Ok(loaded)
                } else {
                    Err(format!("variable {} not found", name))
                }
            }
            _ => todo!(),
        }
    }
    pub fn declare_function(&mut self, func: &Function) -> Result<(), String> {
        let ret_type = self.fun_ret_type(&func.return_type)?;

        let param_types: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| self.llvm_type(&p.type_).map(|t| t.into()))
            .collect::<Result<Vec<_>, _>>()?;
        let ty_ = if let Some(ret) = ret_type {
            ret.fn_type(&param_types, false)
        } else {
            self.context.void_type().fn_type(&param_types, false)
        };

        let fns = self.module.add_function(&func.name, ty_, None);
        for i in func.params.iter().enumerate() {
            fns.get_nth_param(i.0 as u32).unwrap().set_name(&i.1.name);
        }
        Ok(())
    }
}
