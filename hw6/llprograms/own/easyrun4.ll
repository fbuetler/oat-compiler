define i64 @program(i64 %_argc4, { i64, [0 x i8*] }* %_argv1) {
  %_x7 = alloca i64
  %_i9 = alloca i64
  store i64 0, i64* %_x7
  store i64 0, i64* %_i9
  br label %_cond15
_body14:
  %_x16 = load i64, i64* %_x7
  %_bop17 = add i64 %_x16, 2
  store i64 %_bop17, i64* %_x7
  %_i19 = load i64, i64* %_i9
  %_bop20 = add i64 %_i19, 1
  store i64 %_bop20, i64* %_i9
  br label %_cond15
_cond15:
  %_i11 = load i64, i64* %_i9
  %_bop12 = icmp slt i64 %_i11, 3
  br i1 %_bop12, label %_body14, label %_post13
_post13:
  %_x22 = load i64, i64* %_x7
  ret i64 %_x22
}


declare i64* @oat_malloc(i64)
declare i64* @oat_alloc_array(i64)
declare void @oat_assert_not_null(i8*)
declare void @oat_assert_array_length(i64*, i64)
declare { i64, [0 x i64] }* @array_of_string(i8*)
declare i8* @string_of_array({ i64, [0 x i64] }*)
declare i64 @length_of_string(i8*)
declare i8* @string_of_int(i64)
declare i8* @string_cat(i8*, i8*)
declare void @print_string(i8*)
declare void @print_int(i64)
declare void @print_bool(i1)

