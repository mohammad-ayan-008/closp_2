use std::{alloc, collections::HashMap, u32};

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    module::Module,
    support::enable_llvm_pretty_stack_trace,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StringRadix},
    values::{
        AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
    },
};

use crate::{
    expressions::{self, Expression},
    statements::{Block, Function, Item, Program, Statement, Type},
};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    builder: Builder<'ctx>,
    scope: Vec<HashMap<String, (PointerValue<'ctx>, BasicValueEnum<'ctx>)>>,
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
        pointer_value: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) {
        if let Some(a) = self.scope.last_mut() {
            a.insert(name, (pointer_value, value));
        }
    }

    pub fn look_up(&self, name: &str) -> Option<&(PointerValue<'ctx>, BasicValueEnum<'ctx>)> {
        // search inner -> outer
        for scope in self.scope.iter().rev() {
            if let Some(entry) = scope.get(name) {
                return Some(entry);
            }
        }
        None
    }

    pub fn llvm_type(&mut self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, String> {
        match ty {
            Type::Int => Ok(self.context.i64_type().into()),
            Type::Char => Ok(self.context.ptr_type(AddressSpace::default()).into()),
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
            self.declare_variable(j.name.clone(), alloc, param_ll);
        }
        self.generate_block(&f.body);
        self.exit_scope();
        if f.return_type == Type::Void {
            if let Some(a) = self.builder.get_insert_block() {
                if a.get_terminator().is_none() {
                    self.builder.build_return(None).unwrap();
                }
            }
        }
        self.functions.insert(f.name.to_string(), func);
    }

    pub fn generate_block(&mut self, block: &Block) {
        for i in &block.statements {
            self.generate_stmts(i);
        }
    }

    pub fn generate_stmts(&mut self, st: &Statement) {
        match st {
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
                self.declare_variable(name.clone(), alloc, expr);
            }
            Statement::Assignment(a) => {
                let expr = self.compile_expressions(&a.value).unwrap();
                match &a.target {
                    Expression::Identifier(ide) => {
                        let look = self.look_up(ide).unwrap().clone();
                        self.builder.build_store(look.0, expr).unwrap();
                    }

                    Expression::Unary { token, exp } => {
                        // we expect token == Dereference here
                        if let Expression::Identifier(name) = &**exp {
                            // lookup the alloca that holds the pointer
                            let (ptr_to_ptr, _) = self
                                .look_up(name.as_str())
                                .ok_or_else(|| format!("pointer variable {} not found", name))
                                .unwrap();

                            // 1) load the pointer value (address stored in the variable)
                            // we know ptr_to_ptr is a PointerValue pointing to some pointer type like i64*
                            // to load it we must pass the element type of ptr_to_ptr (which is a pointer type)
                            let ptr_element_type = ptr_to_ptr.get_type(); // BasicTypeEnum
                            // ptr_element_type should be a pointer type (BasicTypeEnum::PointerType)
                            let loaded_ptr_val = self
                                .builder
                                .build_load(ptr_element_type, *ptr_to_ptr, "load_ptr")
                                .unwrap();

                            // convert the loaded value to PointerValue so we can store through it
                            let loaded_ptr = loaded_ptr_val.into_pointer_value();

                            // 2) store the rhs value into the loaded pointer
                            self.builder.build_store(loaded_ptr, expr).unwrap();
                        }
                    }
                    _ => todo!("{:?}", a.target),
                }
            }
            _ => panic!("no rule"),
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
                        if let Expression::Identifier(ident) = &**exp {
                            let ptr = self.look_up(ident).ok_or_else(|| "not found".to_string())?;
                            Ok(ptr.0.as_basic_value_enum()) // pointer i64*
                        } else {
                            Err("Expected identifier".to_string())
                        }
                    }

                    expressions::UnaryOP::Dereference => {
                        if let Expression::Identifier(ident) = &**exp {
                            let (ptr_to_ptr, _) = self.look_up(ident).ok_or("not found")?;

                            // 1️⃣ Load the pointer itself (the address stored in the variable)
                            let loaded_ptr = self
                                .builder
                                .build_load(
                                    self.context.ptr_type(AddressSpace::default()),
                                    *ptr_to_ptr,
                                    "load_ptr",
                                )
                                .unwrap()
                                .into_pointer_value();

                            // 2️⃣ Load the actual value from that address
                            let val = self
                                .builder
                                .build_load(self.context.i64_type(), loaded_ptr, "deref_val")
                                .unwrap();

                            Ok(val.as_basic_value_enum())
                        } else {
                            Err("Expected identifier for deref".to_string())
                        }
                    }
                    _ => todo!(),
                }
            }
            Expression::String_Literal(a) => {
                println!("literal {a}");
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
                    Ok(val.try_as_basic_value().left().unwrap())
                } else {
                    Err("func not found".to_string())
                }
            }
            
            Expression::Binary { lhs, op, rhs } => {
                let lhs = self.compile_expressions(lhs)?;
                let rhs = self.compile_expressions(rhs)?;
                let is_float = lhs.is_float_value() || rhs.is_float_value();
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
                    _ => todo!(),
                }
            }

            Expression::Identifier(name) => {
                if let Some((alloc_ptr, _cached_value)) = self.look_up(name) {
                    let elem_type = alloc_ptr.get_type();
                    let loaded = self
                        .builder
                        .build_load(elem_type, *alloc_ptr, &format!("load_{name}"))
                        .unwrap();
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
