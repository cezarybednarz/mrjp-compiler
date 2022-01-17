declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i32 @__neStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @f() {
  ret i32 0
}

define i32 @g() {
  ret i32 0
}

define void @p() {
  ret void
}

define i32 @main() {
  call void @p()
  ret i32 0
}

