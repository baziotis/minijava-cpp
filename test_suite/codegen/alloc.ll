%class.A = type { i8 (...)** }

declare i8* @calloc(i32, i32)

define %class.A* @A__foo(i8* %0) {
entry:
    %1 = call noalias i8* @calloc(i32 1, i32 8)
    %2 = bitcast i8* %1 to %class.A*
    %3 = call noalias i8* @calloc(i32 1, i32 60)
    %4 = bitcast i8* %3 to i32*
    store i32 14, i32* %4
    ret %class.A* %2
}
