; ModuleID = 'test.ll'
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
  %"fn call" = call i64 (ptr, ...) @printf(ptr @str_def, ptr %fptr2)
  %"fn call3" = call i64 (ptr, ...) @printf(ptr @str_def.1, i64 60)
  ret i64 0
}
