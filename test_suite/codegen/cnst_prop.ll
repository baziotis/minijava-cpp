%class.A = type { i8 (...)**, i32* }

declare i8* @calloc(i32, i32)

define i32 @A__constant_propagate_integer(i8* %0) {
entry:
    %1 = call noalias i8* @calloc(i32 1, i32 44)
    %2 = bitcast i8* %1 to i32*
    store i32 10, i32* %2
    %3 = getelementptr inbounds i32, i32* %2, i32 9
    store i32 3, i32* %3
    ret i32 1
}
