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
  %r3 = add i32 1, 1
  %r4 = add i32 %r3, 1
  %r7 = icmp sle i32 %r4, 2
  br i1 %r7, label %L8, label %L19
L8:                              ; preds = [L1]
  %r10 = icmp sle i32 %r4, 1
  br i1 %r10, label %L11, label %L17
L11:                              ; preds = [L8]
  %r13 = icmp sle i32 %r4, 0
  br i1 %r13, label %L14, label %L15
L14:                              ; preds = [L11]
  br label %L16
L15:                              ; preds = [L11]
  br label %L16
L16:                              ; preds = [L15,L14]
  %r25 = phi i32 [ 1, %L14 ], [ 2, %L15 ]
  br label %L18
L17:                              ; preds = [L8]
  br label %L18
L18:                              ; preds = [L17,L16]
  %r24 = phi i32 [ %r25, %L16 ], [ 3, %L17 ]
  br label %L20
L19:                              ; preds = [L1]
  br label %L20
L20:                              ; preds = [L19,L18]
  %r23 = phi i32 [ %r24, %L18 ], [ 4, %L19 ]
  call void @printInt(i32 %r23)
  ret i32 0
}

