use std::collections::HashMap;


use inkwell::{builder::Builder, context::Context, module::Module, support::enable_llvm_pretty_stack_trace, types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum}, values::{AnyValue, BasicValueEnum}, AddressSpace};

use crate::statements::{Function, Program, Type};




pub struct Codegen<'ctx>{
    context:&'ctx Context,
    pub module:Module<'ctx>,
    builder:Builder<'ctx>,
    scope:Vec<HashMap<String,(Type,BasicValueEnum<'ctx>)>>
}

impl<'ctx> Codegen<'ctx>{
    pub fn enter_scope(&mut self){
        self.scope.push(HashMap::new());
    }
    pub fn exit_scope(&mut self){
        self.scope.pop();
    }

    pub fn declare_variable(&mut self,name:String,type_:Type,value:BasicValueEnum<'ctx>){
        if let Some(a) = self.scope.last_mut(){
            a.insert(name, (type_,value));
        }
    }
    pub fn look_up(&mut self,name:&str)->Option<&(Type,BasicValueEnum<'ctx>)>{
        for  i in 0..=self.scope.len(){
            if let Some(a) = self.scope[i].get(name){
                return Some(a);
            }
        }
        None
    }

    pub fn llvm_type(&mut self,ty:&Type)->Result<BasicTypeEnum<'ctx>,String>{
        match ty{
            Type::Int =>{
                Ok(self.context.i64_type().into())
            },
            Type::Char =>Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Boolean => Ok(self.context.bool_type().into()),
            Type::Pointer(_inner)=>{
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            },
            Type::Null=>{
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            },
            Type::Str =>{
                Ok(self.context.ptr_type(AddressSpace::default()).into())
            },
            Type::Float=>{
              Ok(self.context.f64_type().into())
            },
            Type::Void=>{
                Err("Void dont have a recognised type".to_string())
            }
        }
    }

    pub fn new(context:&'ctx Context,mod_name:String)->Self{
        let module = context.create_module(&mod_name);
        Self { context, module , builder: context.create_builder(), scope: vec![HashMap::new()] }

    }
    pub fn fun_ret_type(&mut self,ty:&Type)->Result<Option<BasicTypeEnum<'ctx>>,String>{
        match ty {
            Type::Void=>Ok(None),
            _=> Ok(Some(self.llvm_type(ty)?))
        }
    }
    pub fn generate(&mut self,program:&Program){
        for i in &program.items{
            match i {
                crate::statements::Item::Function(a)=>{
                    self.declare_function(a).unwrap();
                },
            }
        }
    }
    pub fn declare_function(&mut self,func:&Function)->Result<(),String>{
        let ret_type = self.fun_ret_type(&func.return_type)?;

             let param_types: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| self.llvm_type(&p.type_).map(|t| t.into()))
            .collect::<Result<Vec<_>, _>>()?;
        let ty_=if let Some(ret) = ret_type{
            ret.fn_type(&param_types, false)
        }else{
            self.context.void_type().fn_type(&param_types, false)
        };

        let fns = self.module.add_function(&func.name, ty_, None);
        for i in func.params.iter().enumerate(){
            fns.get_nth_param(i.0 as u32)
            .unwrap()
                .set_name(&i.1.name);
        }
        Ok(())
    }

}
