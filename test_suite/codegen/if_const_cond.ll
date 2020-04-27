%class.A = type { i8 (...)** }

declare i8* @calloc(i32, i32)

define i32 @A__foo(i8* %0) {
entry:
    ret i32 2
}

define i32 @A__bar(i8* %0, i32* %1) {
entry:
    %2 = getelementptr inbounds i32, i32* %1, i32 3
    store i32 2, i32* %2
    ret i32 2
}

define i32 @A__fa(i8* %0) {
entry:
    ret i32 3
}

