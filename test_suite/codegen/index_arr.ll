%class.A = type { i8 (...)**, i32* }

declare i8* @calloc(i32, i32)

define i32 @A__field_const_index(i8* %0) {
entry:
    %1 = bitcast i8* %0 to i32**
    %2 = load i32*, i32** %1, align 8
    %3 = getelementptr inbounds i32, i32* %2, i32 2
    store i32 10, i32* %3
    ret i32 1
}

define i32 @A__field_var_index(i8* %0, i32 %1) {
entry:
    %2 = bitcast i8* %0 to i32**
    %3 = load i32*, i32** %2, align 8
    %4 = add i32 %1, 1
    %5 = getelementptr inbounds i32, i32* %3, i32 %4
    store i32 10, i32* %5
    ret i32 2
}

define i32 @A__local_const_index(i8* %0) {
entry:
    %1 = call noalias i8* @calloc(i32 1, i32 84)
    %2 = bitcast i8* %1 to i32*
    store i32 20, i32* %2
    %3 = getelementptr inbounds i32, i32* %2, i32 4
    store i32 7, i32* %3
    ret i32 3
}

define i32 @A__local_var_index(i8* %0, i32 %1) {
entry:
    %2 = call noalias i8* @calloc(i32 1, i32 84)
    %3 = bitcast i8* %2 to i32*
    store i32 20, i32* %3
    %4 = add i32 %1, 1
    %5 = getelementptr inbounds i32, i32* %3, i32 %4
    store i32 7, i32* %5
    ret i32 4
}

