%class.A = type { i8 (...)** }

declare i8* @calloc(i32, i32)

define i32 @A__test(i8* %0, i32 %1) {
entry:
    %2 = add i32 %1, 6
    ret i32 %2
}

define i32 @A__foo(i8* %0) {
entry:
    ret i32 5
}
