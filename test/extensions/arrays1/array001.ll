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

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = mul i32 4, 10
   %r3 = call i8* @malloc(i32 %r2)
   %r4 = bitcast i8* %r3 to i32*
  br label %L6
L6:                              ; preds = [L1,L9]
  %r34 = phi i32 [ %r15, %L9 ], [ 0, %L1 ]
  %r8 = icmp slt i32 %r34, 10
  br i1 %r8, label %L9, label %L16
L9:                              ; preds = [L6]
   %r13 = sext i32 %r34 to i64
   %r12 = getelementptr inbounds i32, i32* %r4, i64 %r13
   store i32 %r34, i32* %r12
  %r15 = add i32 %r34, 1
  br label %L6
L16:                              ; preds = [L6]
  br label %L18
L18:                              ; preds = [L16,L21]
  %r35 = phi i32 [ %r29, %L21 ], [ 0, %L16 ]
  %r20 = icmp slt i32 %r35, 10
  br i1 %r20, label %L21, label %L30
L21:                              ; preds = [L18]
   %r23 = sext i32 %r35 to i64
   %r24 = getelementptr inbounds i32, i32* %r4, i64 %r23
   %r25 = load i32, i32* %r24
  call void @printInt(i32 %r25)
  %r29 = add i32 %r35, 1
  br label %L18
L30:                              ; preds = [L18]
  call void @printInt(i32 45)
  ret i32 0
}

