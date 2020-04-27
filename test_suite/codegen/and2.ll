%class.A = type { i8 (...)** }

declare i8* @calloc(i32, i32)

define i1 @A__test(i8* %0, i32 %1) {
entry:
    %2 = add i32 %1, 2
    %3 = icmp slt i32 3, %2
    %4 = icmp eq i1 %3, 0
    br i1 %4, label %and1, label %and_end1

and1:
    br label %and_end1

and_end1:
    %5 = phi i1 [ 0, %entry ], [ 1, %and1 ]
    ret i1 %5
}

