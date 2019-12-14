-------------------------------------------------------------- insertion_sort.ll
define { i64, [0 x i64] }* @insert({ i64, [0 x i64] }* %_partial82, i64 %_len79, i64 %_insertee76) {
  %_partial83 = alloca { i64, [0 x i64] }*
  %_len80 = alloca i64
  %_insertee77 = alloca i64
  %_i91 = alloca i64
  %_inserted110 = alloca { i64, [0 x i64] }*
  %_i112 = alloca i64
  %_not_yet_inserted131 = alloca i1
  %_i150 = alloca i64
  store { i64, [0 x i64] }* %_partial82, { i64, [0 x i64] }** %_partial83
  store i64 %_len79, i64* %_len80
  store i64 %_insertee76, i64* %_insertee77
  %_len87 = load i64, i64* %_len80
  %_bop88 = add i64 %_len87, 1
  %_raw_array89 = call i64* @oat_alloc_array(i64 %_bop88)
  %_array90 = bitcast i64* %_raw_array89 to { i64, [0 x i64] }*
  %_bnd_86 = alloca i64
  store i64 %_bop88, i64* %_bnd_86
  %_ptr_85 = alloca { i64, [0 x i64] }*
  store { i64, [0 x i64] }* %_array90, { i64, [0 x i64] }** %_ptr_85
  store i64 0, i64* %_i91
  br label %_cond98
_body119:
  %_inserted121 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_inserted110
  %_i122 = load i64, i64* %_i112
  %_tmp124 = bitcast { i64, [0 x i64] }* %_inserted121 to i64*
  call void @oat_assert_array_length(i64* %_tmp124, i64 %_i122)
  %_index_ptr125 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_inserted121, i32 0, i32 1, i64 %_i122
  store i64 -1, i64* %_index_ptr125
  %_i128 = load i64, i64* %_i112
  %_bop129 = add i64 %_i128, 1
  store i64 %_bop129, i64* %_i112
  br label %_cond120
_body156:
  %_not_yet_inserted158 = load i1, i1* %_not_yet_inserted131
  br i1 %_not_yet_inserted158, label %_then218, label %_else217
_body97:
  %__ptr_8599 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_ptr_85
  %_i100 = load i64, i64* %_i91
  %_tmp102 = bitcast { i64, [0 x i64] }* %__ptr_8599 to i64*
  call void @oat_assert_array_length(i64* %_tmp102, i64 %_i100)
  %_index_ptr103 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %__ptr_8599, i32 0, i32 1, i64 %_i100
  store i64 0, i64* %_index_ptr103
  %_i105 = load i64, i64* %_i91
  %_bop106 = add i64 %_i105, 1
  store i64 %_bop106, i64* %_i91
  br label %_cond98
_cond120:
  %_i114 = load i64, i64* %_i112
  %_len115 = load i64, i64* %_len80
  %_bop116 = add i64 %_len115, 1
  %_bop117 = icmp slt i64 %_i114, %_bop116
  br i1 %_bop117, label %_body119, label %_post118
_cond157:
  %_i152 = load i64, i64* %_i150
  %_len153 = load i64, i64* %_len80
  %_bop154 = icmp slt i64 %_i152, %_len153
  br i1 %_bop154, label %_body156, label %_post155
_cond98:
  %_i93 = load i64, i64* %_i91
  %__bnd_8694 = load i64, i64* %_bnd_86
  %_bop95 = icmp slt i64 %_i93, %__bnd_8694
  br i1 %_bop95, label %_body97, label %_post96
_else148:
  br label %_merge147
_else201:
  %_inserted188 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_inserted110
  %_i189 = load i64, i64* %_i150
  %_tmp191 = bitcast { i64, [0 x i64] }* %_inserted188 to i64*
  call void @oat_assert_array_length(i64* %_tmp191, i64 %_i189)
  %_index_ptr192 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_inserted188, i32 0, i32 1, i64 %_i189
  %_partial193 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_partial83
  %_i194 = load i64, i64* %_i150
  %_tmp196 = bitcast { i64, [0 x i64] }* %_partial193 to i64*
  call void @oat_assert_array_length(i64* %_tmp196, i64 %_i194)
  %_index_ptr197 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_partial193, i32 0, i32 1, i64 %_i194
  %_index198 = load i64, i64* %_index_ptr197
  store i64 %_index198, i64* %_index_ptr192
  br label %_merge200
_else217:
  %_inserted203 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_inserted110
  %_i204 = load i64, i64* %_i150
  %_bop205 = add i64 %_i204, 1
  %_tmp207 = bitcast { i64, [0 x i64] }* %_inserted203 to i64*
  call void @oat_assert_array_length(i64* %_tmp207, i64 %_bop205)
  %_index_ptr208 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_inserted203, i32 0, i32 1, i64 %_bop205
  %_partial209 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_partial83
  %_i210 = load i64, i64* %_i150
  %_tmp212 = bitcast { i64, [0 x i64] }* %_partial209 to i64*
  call void @oat_assert_array_length(i64* %_tmp212, i64 %_i210)
  %_index_ptr213 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_partial209, i32 0, i32 1, i64 %_i210
  %_index214 = load i64, i64* %_index_ptr213
  store i64 %_index214, i64* %_index_ptr208
  br label %_merge216
_merge147:
  store i64 0, i64* %_i150
  br label %_cond157
_merge200:
  br label %_merge216
_merge216:
  %_i219 = load i64, i64* %_i150
  %_bop220 = add i64 %_i219, 1
  store i64 %_bop220, i64* %_i150
  br label %_cond157
_post118:
  store i1 1, i1* %_not_yet_inserted131
  %_insertee133 = load i64, i64* %_insertee77
  %_partial134 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_partial83
  %_tmp136 = bitcast { i64, [0 x i64] }* %_partial134 to i64*
  call void @oat_assert_array_length(i64* %_tmp136, i64 0)
  %_index_ptr137 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_partial134, i32 0, i32 1, i32 0
  %_index138 = load i64, i64* %_index_ptr137
  %_bop139 = icmp slt i64 %_insertee133, %_index138
  br i1 %_bop139, label %_then149, label %_else148
_post155:
  %_inserted222 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_inserted110
  ret { i64, [0 x i64] }* %_inserted222
_post96:
  store { i64, [0 x i64] }* %_array90, { i64, [0 x i64] }** %_inserted110
  store i64 0, i64* %_i112
  br label %_cond120
_then149:
  store i1 0, i1* %_not_yet_inserted131
  %_inserted141 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_inserted110
  %_tmp143 = bitcast { i64, [0 x i64] }* %_inserted141 to i64*
  call void @oat_assert_array_length(i64* %_tmp143, i64 0)
  %_index_ptr144 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_inserted141, i32 0, i32 1, i32 0
  %_insertee145 = load i64, i64* %_insertee77
  store i64 %_insertee145, i64* %_index_ptr144
  br label %_merge147
_then202:
  store i1 0, i1* %_not_yet_inserted131
  %_inserted168 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_inserted110
  %_i169 = load i64, i64* %_i150
  %_bop170 = add i64 %_i169, 1
  %_tmp172 = bitcast { i64, [0 x i64] }* %_inserted168 to i64*
  call void @oat_assert_array_length(i64* %_tmp172, i64 %_bop170)
  %_index_ptr173 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_inserted168, i32 0, i32 1, i64 %_bop170
  %_insertee174 = load i64, i64* %_insertee77
  store i64 %_insertee174, i64* %_index_ptr173
  %_inserted176 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_inserted110
  %_i177 = load i64, i64* %_i150
  %_tmp179 = bitcast { i64, [0 x i64] }* %_inserted176 to i64*
  call void @oat_assert_array_length(i64* %_tmp179, i64 %_i177)
  %_index_ptr180 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_inserted176, i32 0, i32 1, i64 %_i177
  %_partial181 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_partial83
  %_i182 = load i64, i64* %_i150
  %_tmp184 = bitcast { i64, [0 x i64] }* %_partial181 to i64*
  call void @oat_assert_array_length(i64* %_tmp184, i64 %_i182)
  %_index_ptr185 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_partial181, i32 0, i32 1, i64 %_i182
  %_index186 = load i64, i64* %_index_ptr185
  store i64 %_index186, i64* %_index_ptr180
  br label %_merge200
_then218:
  %_insertee159 = load i64, i64* %_insertee77
  %_partial160 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_partial83
  %_i161 = load i64, i64* %_i150
  %_tmp163 = bitcast { i64, [0 x i64] }* %_partial160 to i64*
  call void @oat_assert_array_length(i64* %_tmp163, i64 %_i161)
  %_index_ptr164 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_partial160, i32 0, i32 1, i64 %_i161
  %_index165 = load i64, i64* %_index_ptr164
  %_bop166 = icmp sgt i64 %_insertee159, %_index165
  br i1 %_bop166, label %_then202, label %_else201
}

define { i64, [0 x i64] }* @insort({ i64, [0 x i64] }* %_unsorted35, i64 %_len32) {
  %_unsorted36 = alloca { i64, [0 x i64] }*
  %_len33 = alloca i64
  %_out42 = alloca { i64, [0 x i64] }*
  %_i54 = alloca i64
  store { i64, [0 x i64] }* %_unsorted35, { i64, [0 x i64] }** %_unsorted36
  store i64 %_len32, i64* %_len33
  %_raw_array38 = call i64* @oat_alloc_array(i64 1)
  %_array39 = bitcast i64* %_raw_array38 to { i64, [0 x i64] }*
  %_ind40 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_array39, i32 0, i32 1, i32 0
  store i64 0, i64* %_ind40
  store { i64, [0 x i64] }* %_array39, { i64, [0 x i64] }** %_out42
  %_out44 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_out42
  %_tmp46 = bitcast { i64, [0 x i64] }* %_out44 to i64*
  call void @oat_assert_array_length(i64* %_tmp46, i64 0)
  %_index_ptr47 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_out44, i32 0, i32 1, i32 0
  %_unsorted48 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_unsorted36
  %_tmp50 = bitcast { i64, [0 x i64] }* %_unsorted48 to i64*
  call void @oat_assert_array_length(i64* %_tmp50, i64 0)
  %_index_ptr51 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_unsorted48, i32 0, i32 1, i32 0
  %_index52 = load i64, i64* %_index_ptr51
  store i64 %_index52, i64* %_index_ptr47
  store i64 1, i64* %_i54
  br label %_cond61
_body60:
  %_unsorted62 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_unsorted36
  %_i63 = load i64, i64* %_i54
  %_tmp65 = bitcast { i64, [0 x i64] }* %_unsorted62 to i64*
  call void @oat_assert_array_length(i64* %_tmp65, i64 %_i63)
  %_index_ptr66 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_unsorted62, i32 0, i32 1, i64 %_i63
  %_index67 = load i64, i64* %_index_ptr66
  %_i68 = load i64, i64* %_i54
  %_out69 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_out42
  %_result70 = call { i64, [0 x i64] }* @insert({ i64, [0 x i64] }* %_out69, i64 %_i68, i64 %_index67)
  store { i64, [0 x i64] }* %_result70, { i64, [0 x i64] }** %_out42
  %_i72 = load i64, i64* %_i54
  %_bop73 = add i64 %_i72, 1
  store i64 %_bop73, i64* %_i54
  br label %_cond61
_cond61:
  %_i56 = load i64, i64* %_i54
  %_len57 = load i64, i64* %_len33
  %_bop58 = icmp slt i64 %_i56, %_len57
  br i1 %_bop58, label %_body60, label %_post59
_post59:
  %_out75 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_out42
  ret { i64, [0 x i64] }* %_out75
}

define i64 @program(i64 %_argc4, { i64, [0 x i8*] }* %_argv1) {
  %_array21 = alloca { i64, [0 x i64] }*
  %_result25 = alloca { i64, [0 x i64] }*
  %_raw_array7 = call i64* @oat_alloc_array(i64 6)
  %_array8 = bitcast i64* %_raw_array7 to { i64, [0 x i64] }*
  %_ind9 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_array8, i32 0, i32 1, i32 0
  store i64 13, i64* %_ind9
  %_ind11 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_array8, i32 0, i32 1, i32 1
  store i64 42, i64* %_ind11
  %_ind13 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_array8, i32 0, i32 1, i32 2
  store i64 32, i64* %_ind13
  %_ind15 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_array8, i32 0, i32 1, i32 3
  store i64 3, i64* %_ind15
  %_ind17 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_array8, i32 0, i32 1, i32 4
  store i64 2, i64* %_ind17
  %_ind19 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_array8, i32 0, i32 1, i32 5
  store i64 6, i64* %_ind19
  store { i64, [0 x i64] }* %_array8, { i64, [0 x i64] }** %_array21
  %_array23 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_array21
  %_result24 = call { i64, [0 x i64] }* @insort({ i64, [0 x i64] }* %_array23, i64 6)
  store { i64, [0 x i64] }* %_result24, { i64, [0 x i64] }** %_result25
  %_result27 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_result25
  %_tmp29 = bitcast { i64, [0 x i64] }* %_result27 to i64*
  call void @oat_assert_array_length(i64* %_tmp29, i64 5)
  %_index_ptr30 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_result27, i32 0, i32 1, i32 5
  %_index31 = load i64, i64* %_index_ptr30
  ret i64 %_index31
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
