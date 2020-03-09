%class.A = type { i8 (...)** }

declare i8* @calloc(i32, i32)

define i1 @A__test(i8* %0, i1 %1, i1 %2, i1 %3) {
entry:
    br i1 %1, label %and1, label %and_end1

and1:
    br i1 %2, label %and2, label %and_end2

and2:
    br label %and_end2

and_end2:
    %4 = phi i1 [ 0, %and1 ], [ %3, %and2 ]
    br label %and_end1

and_end1:
    %5 = phi i1 [ 0, %entry ], [ %4, %and_end2 ]
    ret i1 %5
}

