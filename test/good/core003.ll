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

define i32 @f() {
  br label %L1
L1:                              ; preds = [L0]
  ret i32 0
}

define i32 @g() {
  br label %L1
L1:                              ; preds = [L0]
  ret i32 0
}

define void @p() {
  br label %L1
L1:                              ; preds = [L0]
  ret void
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  call void @p()
  ret i32 0
}

