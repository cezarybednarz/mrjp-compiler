declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i32 @__neStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  %1 = alloca i32
  %2 = call i32 @readInt()
  store i32 %2, i32* %1
  %3 = alloca i8*
  %4 = call i8* @readString()
  store i8* %4, i8** %3
  %5 = alloca i8*
  %6 = call i8* @readString()
  store i8* %6, i8** %5
  %7 = load i32, i32* %1
  %8 = sub i32 %7, 5
  call void @printInt(i32 %8)
  %9 = load i8*, i8** %3
  %10 = load i8*, i8** %5
  %11 = call i8* @__concatStrings__(i8* %9, i8* %10)
  call void @printString(i8* %11)
  ret i32 0
}

