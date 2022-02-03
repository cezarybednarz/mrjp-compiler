%ArrRetVal = type {
  i32,           ; length of array
  i32*,
  i1*,
  i8**
}
declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
declare i8* @malloc(i32) nounwind
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @funkcja_declarations(i32 %r0, i32 %r1, i32 %r2, i32 %r3) {
  br label %L5
L5:                              ; preds = [L0]
  br label %L13
L13:                              ; preds = [L5,L75]
  %r102 = phi i32 [ %r103, %L75 ], [ 0, %L5 ]
  %r99 = phi i32 [ %r79, %L75 ], [ %r1, %L5 ]
  %r96 = phi i32 [ %r81, %L75 ], [ %r2, %L5 ]
  %r93 = phi i32 [ %r77, %L75 ], [ %r0, %L5 ]
  %r90 = phi i32 [ %r90, %L75 ], [ %r3, %L5 ]
  %r87 = phi i32 [ %r83, %L75 ], [ 0, %L5 ]
  %r16 = icmp slt i32 %r87, %r90
  br i1 %r16, label %L17, label %L84
L17:                              ; preds = [L13]
  %r19 = add i32 %r93, 1111
  %r21 = add i32 %r96, 2222
  %r22 = mul i32 %r19, %r21
  %r24 = sub i32 %r93, 3333
  %r26 = add i32 %r99, 4444
  %r27 = mul i32 %r24, %r26
  %r28 = sub i32 %r22, %r27
  %r30 = sub i32 %r93, 1111
  %r32 = sub i32 %r96, 2222
  %r33 = mul i32 %r30, %r32
  %r35 = add i32 %r93, 3333
  %r37 = sub i32 %r99, 4444
  %r38 = mul i32 %r35, %r37
  %r39 = add i32 %r33, %r38
  %r40 = add i32 %r28, %r39
  %r43 = icmp slt i32 %r102, %r40
  br i1 %r43, label %L44, label %L46
L44:                              ; preds = [L17]
  br label %L46
L46:                              ; preds = [L44,L17]
  %r104 = phi i32 [ %r102, %L17 ], [ %r40, %L44 ]
  %r72 = icmp slt i32 %r104, %r40
  br i1 %r72, label %L73, label %L75
L73:                              ; preds = [L46]
  br label %L75
L75:                              ; preds = [L73,L46]
  %r103 = phi i32 [ %r104, %L46 ], [ %r40, %L73 ]
  %r77 = add i32 %r93, 1
  %r79 = sub i32 %r99, 1
  %r81 = add i32 %r96, 1
  %r83 = add i32 %r87, 1
  br label %L13
L84:                              ; preds = [L13]
  ret i32 %r102
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r4 = call i32 @funkcja_declarations(i32 4, i32 5, i32 6, i32 20)
  %r5 = add i32 0, %r4
  call void @printInt(i32 %r5)
  ret i32 0
}

