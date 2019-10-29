@gbl = global i64 27

define void @foo() {
  %1 = load i64, i64* @gbl
  %2 = add i64 %1, 15
  store i64 %2, i64* @gbl
  ret void
}

define i64 @main(i64 %argc, i8** %arcv) {
  call void @foo()
  %3 = load i64, i64* @gbl
  ret i64 %3 
}

