define i64 @main(i64 %argc, i8** %arcv) {
  %1 = call i64 @foo(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8)
  ret i64 %1
}

define i64 @foo(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f, i64 %g, i64 %h) {
  ret i64 27
}