declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %1
1:
  %2 = sub i32 0, 42
  %3 = sub i32 0, 1
  %4 = sdiv i32 %2, %3
  call void @printInt(i32 %4)
  ret i32 0
}
