define i64 @program(i64 %_argc4, { i64, [0 x i8*] }* %_argv1) {
  %_i11 = alloca i64
  %_raw_array9 = call i64* @oat_alloc_array(i64 3)
  %_array10 = bitcast i64* %_raw_array9 to { i64, [0 x i64] }*
  %_bnd_8 = alloca i64
  store i64 3, i64* %_bnd_8
  %_ptr_7 = alloca { i64, [0 x i64] }*
  store { i64, [0 x i64] }* %_array10, { i64, [0 x i64] }** %_ptr_7
  store i64 0, i64* %_i11
  br label %_cond18
_body17:
  %__ptr_719 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_ptr_7
  %_i20 = load i64, i64* %_i11
  %_tmp22 = bitcast { i64, [0 x i64] }* %__ptr_719 to i64*
  call void @oat_assert_array_length(i64* %_tmp22, i64 %_i20)
  %_index_ptr23 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %__ptr_719, i32 0, i32 1, i64 %_i20
  store i64 0, i64* %_index_ptr23
  %_i25 = load i64, i64* %_i11
  %_bop26 = add i64 %_i25, 1
  store i64 %_bop26, i64* %_i11
  br label %_cond18
_cond18:
  %_i13 = load i64, i64* %_i11
  %__bnd_814 = load i64, i64* %_bnd_8
  %_bop15 = icmp slt i64 %_i13, %__bnd_814
  br i1 %_bop15, label %_body17, label %_post16
_post16:
  ret i64 0
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

