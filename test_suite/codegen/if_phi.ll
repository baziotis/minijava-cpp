%class.A = type { i8 (...)** }

declare i8* @calloc(i32, i32)

define i32 @A__foo(i8* %0, i1 %1) {
entry:
    br i1 %1, label %if1, label %else1

if1:
    br i1 %1, label %if2, label %else2

if2:
    br label %if_end2

else2:
    br label %if_end2

if_end2:
    %2 = phi i32 [ 2, %if2 ], [ 7, %else2 ]
    br label %if_end1

else1:
    br label %if_end1

if_end1:
    %3 = phi i32 [ %2, %if_end2 ], [ 7, %else1 ]
    ret i32 %3
}

define i32 @A__bar(i8* %0, i1 %1) {
entry:
    br i1 %1, label %if1, label %else1

if1:
    br i1 %1, label %if2, label %else2

if2:
    br label %if_end2

else2:
    br label %if_end2

if_end2:
    %2 = phi i32 [ 7, %if2 ], [ 2, %else2 ]
    br label %if_end1

else1:
    br label %if_end1

if_end1:
    %3 = phi i32 [ %2, %if_end2 ], [ 7, %else1 ]
    ret i32 %3
}

define i32 @A__fa(i8* %0, i1 %1) {
entry:
    br i1 %1, label %if1, label %else1

if1:
    br label %if_end1

else1:
    br label %if_end1

if_end1:
    %2 = phi i32 [ 3, %if1 ], [ 4, %else1 ]
    ret i32 %2
}

define i32 @A__test1(i8* %0, i1 %1) {
entry:
    %2 = call noalias i8* @calloc(i32 1, i32 8)
    %3 = bitcast i8* %2 to %class.A*
    br i1 %1, label %if1, label %else1

if1:
    %4 = call noalias i8* @calloc(i32 1, i32 8)
    %5 = bitcast i8* %4 to %class.A*
    br label %if_end1

else1:
    br i1 %1, label %if2, label %else2

if2:
    br label %if_end2

else2:
    %6 = call noalias i8* @calloc(i32 1, i32 8)
    %7 = bitcast i8* %6 to %class.A*
    br label %if_end2

if_end2:
    %8 = phi %class.A* [ %3, %if2 ], [ %7, %else2 ]
    br label %if_end1

if_end1:
    %9 = phi %class.A* [ %5, %if1 ], [ %8, %if_end2 ]
    ret i32 5
}
