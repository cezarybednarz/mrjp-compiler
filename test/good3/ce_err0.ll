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
  br i1 0, label %L6, label %L4
L4:                              ; preds = [L1]
  br label %L6
L6:                              ; preds = [L1,L4]
  %r7 = phi i1 [ true, %L1 ], [ 0, %L4 ]
  br i1 %r7, label %L8, label %L9
L8:                              ; preds = [L6]
  call void @printInt(i32 1)
  br label %L9
L9:                              ; preds = [L8,L6]
  ret i32 0
}

