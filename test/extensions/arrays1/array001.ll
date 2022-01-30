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
  %r5 = alloca i32
  store i32 0, i32* %r5
  br label %L6
L6:                              ; preds = [L1,L9]
  %r7 = load i32, i32* %r5
  %r8 = icmp slt i32 %r7, 10
  br i1 %r8, label %L9, label %L16
L9:                              ; preds = [L6]
  %r10 = load i32, i32* %r5
  %r11 = load i32, i32* %r5
  %r13 = sext i32 %r11 to i64
  %r12 = getelementptr inbounds i32, i32* %r4, i64 %r13
  store i32 %r10, i32* %r12
  %r14 = load i32, i32* %r5
  %r15 = add i32 %r14, 1
  store i32 %r15, i32* %r5
  br label %L6
L16:                              ; preds = [L6]
  %r17 = alloca i32
  store i32 0, i32* %r17
  br label %L18
L18:                              ; preds = [L16,L21]
  %r19 = load i32, i32* %r17
  %r20 = icmp slt i32 %r19, 10
  br i1 %r20, label %L21, label %L30
L21:                              ; preds = [L18]
  %r22 = load i32, i32* %r17
  %r23 = sext i32 %r22 to i64
  %r24 = getelementptr inbounds i32, i32* %r4, i64 %r23
  %r25 = load i32, i32* %r24
  %r26 = alloca i32
  store i32 %r25, i32* %r26
  %r27 = load i32, i32* %r26
  call void @printInt(i32 %r27)
  %r28 = load i32, i32* %r17
  %r29 = add i32 %r28, 1
  store i32 %r29, i32* %r17
  br label %L18
L30:                              ; preds = [L18]
  %r31 = alloca i32
  store i32 45, i32* %r31
  %r32 = load i32, i32* %r31
  call void @printInt(i32 %r32)
  ret i32 0
}

