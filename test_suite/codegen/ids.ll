%class.A = type { i8 (...)**, i32, i1, i32*, %class.A* }

declare i8* @calloc(i32, i32)

define i32 @A__test_int(i8* %0) {
entry:
    %1 = bitcast i8* %0 to i32*
    %2 = load i32, i32* %1, align 4
    ret i32 %2
}

define i1 @A__test_boolean(i8* %0) {
entry:
    %1 = getelementptr inbounds i8, i8* %0, i32 4
    %2 = bitcast i8* %1 to i1*
    %3 = load i1, i1* %2
    ret i1 %3
}

define i32* @A__test_arr(i8* %0) {
entry:
    %1 = getelementptr inbounds i8, i8* %0, i32 5
    %2 = bitcast i8* %1 to i32**
    %3 = load i32*, i32** %2, align 8
    ret i32* %3
}

define %class.A* @A__test_obj(i8* %0) {
entry:
    %1 = getelementptr inbounds i8, i8* %0, i32 13
    %2 = bitcast i8* %1 to %class.A**
    %3 = load %class.A*, %class.A** %2, align 8
    ret %class.A* %3
}

