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
  br label %L4
L4:                              ; preds = [L1,L13]
  %r19 = phi i32 [ %r15, %L13 ], [ 0, %L1 ]
  %r6 = icmp slt i32 %r19, 10
  br i1 %r6, label %L7, label %L16
L7:                              ; preds = [L4]
  %r8 = icmp slt i32 0, 1
  br i1 %r8, label %L9, label %L10
L9:                              ; preds = [L7]
  br label %L10
L10:                              ; preds = [L9,L7]
  %r11 = icmp slt i32 0, 1
  br i1 %r11, label %L12, label %L13
L12:                              ; preds = [L10]
  br label %L13
L13:                              ; preds = [L12,L10]
  %r15 = add i32 %r19, 1
  br label %L4
L16:                              ; preds = [L4]
  call void @printInt(i32 %r19)
  ret i32 0
}

