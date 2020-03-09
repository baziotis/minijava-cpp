%class.A = type { i8 (...)**, i32, i1, i32 }
%class.B = type { %class.A, %class.C*, i32 }
%class.C = type { i8 (...)** }

declare i8* @calloc(i32, i32)

define i32 @A__foo(i8* %0) {
entry:
    ret i32 1
}

define i1 @A__fa(i8* %0) {
entry:
    ret i1 0
}

define i32 @B__foo(i8* %0) {
entry:
    ret i32 2
}

define i1 @B__bla(i8* %0) {
entry:
    ret i1 1
}

