%class.A = type { i8 (...)** }

declare i8* @calloc(i32, i32)

define i32 @A__foo(i8* %0) {
entry:
    br label %while1

while1:
    %1 = phi i32 [ 3, %entry ], [ %2, %while1 ]
    %2 = add i32 %1, 1
    %3 = icmp slt i32 %2, 4
    br i1 %3, label %while1, label %while_end1

while_end1:
    ret i32 1
}

define i32 @A__bar(i8* %0, i32 %1, i1 %2) {
entry:
    br i1 %2, label %while1, label %while_end1

while1:
    %3 = phi i32 [ %1, %entry ], [ %4, %if_end2 ]
    br i1 %2, label %if2, label %else2

if2:
    br label %if_end2

else2:
    br label %if_end2

if_end2:
    %4 = phi i32 [ 2, %if2 ], [ %3, %else2 ]
    br i1 %2, label %while1, label %while_end1

while_end1:
    %5 = phi i32 [ %1, %entry ], [ %4, %if_end2 ]
    %6 = phi i32 [ 0, %entry ], [ %3, %if_end2 ]
    ret i32 1
}

