@a = global i64 1

define i64 @f() {
  %_a47 = load i64, i64* @a
  %_bop48 = add i64 %_a47, 1
  store i64 %_bop48, i64* @a
  %_a50 = load i64, i64* @a
  ret i64 %_a50
}
