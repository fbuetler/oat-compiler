%Pair = type { i1, i1 }

define %Pair* @create_pair(i1 %_a23, i1 %_b20) {
  %_a24 = alloca i1
  %_b21 = alloca i1
  store i1 %_a23, i1* %_a24
  store i1 %_b20, i1* %_b21
  %_raw_struct26 = call i64* @oat_malloc(i64 16)
  %_struct27 = bitcast i64* %_raw_struct26 to %Pair*
  %_a28 = load i1, i1* %_a24
  %_ind29 = getelementptr %Pair, %Pair* %_struct27, i32 0, i32 0
  store i1 %_a28, i1* %_ind29
  %_b31 = load i1, i1* %_b21
  %_ind32 = getelementptr %Pair, %Pair* %_struct27, i32 0, i32 1
  store i1 %_b31, i1* %_ind32
  ret %Pair* %_struct27
}

define i64 @program(i64 %_argc4, { i64, [0 x i8*] }* %_argv1) {
  %_p8 = alloca %Pair*
  %_result7 = call %Pair* @create_pair(i1 1, i1 0)
  store %Pair* %_result7, %Pair** %_p8
  %_p10 = load %Pair*, %Pair** %_p8
  %_index11 = getelementptr %Pair, %Pair* %_p10, i32 0, i32 0
  %_proj12 = load i1, i1* %_index11
  %_p13 = load %Pair*, %Pair** %_p8
  %_index14 = getelementptr %Pair, %Pair* %_p13, i32 0, i32 1
  %_proj15 = load i1, i1* %_index14
  %_bop16 = and i1 %_proj12, %_proj15
  br i1 %_bop16, label %_then19, label %_else18
_else18:
  ret i64 0
_then19:
  ret i64 1
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
