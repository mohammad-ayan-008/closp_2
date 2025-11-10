; ModuleID = 'mod_rs'
source_filename = "mod_rs"

@str_def = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@str_def.1 = private unnamed_addr constant [5 x i8] c"%d\0A \00", align 1

declare i64 @printf(ptr, ...)

define i64 @main() {
entry:
  %f = alloca double, align 8
  store double 6.010000e+01, ptr %f, align 8
  %fptr2 = alloca ptr, align 8
  store ptr %f, ptr %fptr2, align 8
  %ptr3 = alloca ptr, align 8
  store ptr %fptr2, ptr %ptr3, align 8
  %pr4 = alloca ptr, align 8
  store ptr %ptr3, ptr %pr4, align 8
  %load_ptr = load ptr, ptr %pr4, align 8
  %deref_val = load ptr, ptr %load_ptr, align 8
  %"fn call" = call i64 (ptr, ...) @printf(ptr @str_def, ptr %deref_val)
  %fps = alloca i64, align 8
  store i64 60, ptr %fps, align 4
  %ptr2 = alloca ptr, align 8
  store ptr %fps, ptr %ptr2, align 8
  %load_ptr1 = load ptr, ptr %ptr2, align 8
  %deref_val2 = load i64, ptr %load_ptr1, align 4
  %"fn call3" = call i64 (ptr, ...) @printf(ptr @str_def.1, i64 %deref_val2)
  ret i64 0
}
