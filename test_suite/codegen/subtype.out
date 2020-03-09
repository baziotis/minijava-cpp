%class.B = type { i8 (...)** }
%class.A = type { %class.B, i32 }

declare i8* @calloc(i32, i32)

define %class.B* @A__test_int(i8* %0) {
entry:
    %1 = call noalias i8* @calloc(i32 1, i32 12)
    %2 = bitcast i8* %1 to %class.A*
    %3 = bitcast %class.A* %2 to %class.B*
    ret %class.B* %3
}

