define i64 @f(i64 %_x136, i64 %_x233, i64 %_x330, i64 %_x427, i64 %_x524, i64 %_x621, i64 %_x718, i64 %_x815) {
  %_x137 = alloca i64
  %_x234 = alloca i64
  %_x331 = alloca i64
  %_x428 = alloca i64
  %_x525 = alloca i64
  %_x622 = alloca i64
  %_x719 = alloca i64
  %_x816 = alloca i64
  store i64 %_x136, i64* %_x137
  store i64 %_x233, i64* %_x234
  store i64 %_x330, i64* %_x331
  store i64 %_x427, i64* %_x428
  store i64 %_x524, i64* %_x525
  store i64 %_x621, i64* %_x622
  store i64 %_x718, i64* %_x719
  store i64 %_x815, i64* %_x816
  %_x139 = load i64, i64* %_x137
  %_x240 = load i64, i64* %_x234
  %_bop41 = add i64 %_x139, %_x240
  %_x342 = load i64, i64* %_x331
  %_bop43 = add i64 %_bop41, %_x342
  %_x444 = load i64, i64* %_x428
  %_bop45 = add i64 %_bop43, %_x444
  %_x546 = load i64, i64* %_x525
  %_bop47 = add i64 %_bop45, %_x546
  %_x648 = load i64, i64* %_x622
  %_bop49 = add i64 %_bop47, %_x648
  %_x750 = load i64, i64* %_x719
  %_bop51 = add i64 %_bop49, %_x750
  %_x852 = load i64, i64* %_x816
  %_bop53 = add i64 %_bop51, %_x852
  ret i64 %_bop53
}

define i64 @program(i64 %_argc4, { i64, [0 x i8*] }* %_argv1) {
  %_x11 = alloca i64
  %_result10 = call i64 @f(i64 1, i64 2, i64 3, i64 4, i64 5, i64 -5, i64 -4, i64 -3)
  store i64 %_result10, i64* %_x11
  %_x13 = load i64, i64* %_x11
  call void @print_int(i64 %_x13)
  ret i64 41
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

