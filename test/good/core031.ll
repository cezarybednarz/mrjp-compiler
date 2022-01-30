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
  %r2 = sub i32 0, 1
  %r3 = call i32 @f(i32 1, i32 %r2)
  call void @printInt(i32 %r3)
  ret i32 0
}

define i32 @f(i32 %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r7 = icmp sgt i32 %r0, 0
  %r8 = xor i1 %r7, true
  br i1 %r8, label %L13, label %L9
L9:                              ; preds = [L3]
  %r11 = icmp sgt i32 %r1, 0
  %r12 = xor i1 %r11, true
  br label %L13
L13:                              ; preds = [L3]
  %r14 = phi i1 [ %r12, %L9 ], [ true, %L3 ]
  %r15 = xor i1 %r14, true
  br i1 %r15, label %L27, label %L16
L16:                              ; preds = [L13]
  %r18 = icmp slt i32 %r0, 0
  %r19 = xor i1 %r18, true
  br i1 %r19, label %L24, label %L20
L20:                              ; preds = [L16]
  %r22 = icmp slt i32 %r1, 0
  %r23 = xor i1 %r22, true
  br label %L24
L24:                              ; preds = [L16]
  %r25 = phi i1 [ %r23, %L20 ], [ true, %L16 ]
  %r26 = xor i1 %r25, true
  br label %L27
L27:                              ; preds = [L13]
  %r28 = phi i1 [ %r26, %L24 ], [ true, %L13 ]
  br i1 %r28, label %L29, label %L31
L29:                              ; preds = [L27]
  ret i32 7
  br label %L33
L31:                              ; preds = [L27]
  ret i32 42
  br label %L33
L33:                              ; preds = [L31,L29]
  ret i32 0
}

