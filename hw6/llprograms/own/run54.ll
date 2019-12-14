----------------------------------------------------------------------- run54.ll
define i64 @f(i64 %_x132, i64 %_x229, i64 %_x326, i64 %_x423, i64 %_x520, i64 %_x617, i64 %_x714, i64 %_x811) {
  %_x133 = alloca i64
  %_x230 = alloca i64
  %_x327 = alloca i64
  %_x424 = alloca i64
  store i64 %_x132, i64* %_x133
  store i64 %_x229, i64* %_x230
  store i64 %_x326, i64* %_x327
  store i64 %_x423, i64* %_x424
  %_x135 = load i64, i64* %_x133
  %_x236 = load i64, i64* %_x230
  %_bop37 = add i64 %_x135, %_x236
  %_x338 = load i64, i64* %_x327
  %_bop39 = add i64 %_bop37, %_x338
  %_x440 = load i64, i64* %_x424
  %_bop41 = add i64 %_bop39, %_x440
  ret i64 %_bop41
}

define i64 @program(i64 %_argc4, { i64, [0 x i8*] }* %_argv1) {
  %_result10 = call i64 @f(i64 1, i64 2, i64 3, i64 4, i64 5, i64 -5, i64 -4, i64 -3)
  ret i64 %_result10
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
