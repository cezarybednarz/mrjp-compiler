declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @ifac2f(i32 %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r8 = icmp eq i32 %r0, %r1
  br i1 %r8, label %L9, label %L12
L9:                              ; preds = [L3]
  ret i32 %r0
  br label %L12
L12:                              ; preds = [L9,L3]
  %r34 = phi i32 [ %r1, %L3 ], [ %r1, %L9 ]
  %r33 = phi i32 [ %r0, %L3 ], [ %r0, %L9 ]
  %r15 = icmp sgt i32 %r33, %r34
  br i1 %r15, label %L16, label %L18
L16:                              ; preds = [L12]
  ret i32 1
  br label %L18
L18:                              ; preds = [L16,L12]
  %r36 = phi i32 [ %r34, %L12 ], [ %r34, %L16 ]
  %r35 = phi i32 [ %r33, %L12 ], [ %r33, %L16 ]
  %r22 = add i32 %r35, %r36
  %r23 = sdiv i32 %r22, 2
  %r26 = call i32 @ifac2f(i32 %r35, i32 %r23)
  %r28 = add i32 %r23, 1
  %r30 = call i32 @ifac2f(i32 %r28, i32 %r36)
  %r31 = mul i32 %r26, %r30
  ret i32 %r31
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = call i32 @ifac2f(i32 1, i32 2)
  call void @printInt(i32 %r2)
  ret i32 0
}

