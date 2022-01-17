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
  %2 = alloca i32
  store i32 0, i32* %2
  %3 = alloca i32
  store i32 0, i32* %3
  store i32 45, i32* %2
  %4 = sub i32 0, 36
  store i32 %4, i32* %3
  %5 = load i32, i32* %2
  call void @printInt(i32 %5)
  %6 = load i32, i32* %3
  call void @printInt(i32 %6)
  ret i32 0
}

