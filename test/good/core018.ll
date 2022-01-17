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
  br label %1
1:
  %2 = call i32 @readInt()
  %3 = alloca i32
  store i32 %2, i32* %3
  %4 = call i8* @readString()
  %5 = alloca i8*
  store i8* %4, i8** %5
  %6 = call i8* @readString()
  %7 = alloca i8*
  store i8* %6, i8** %7
  %8 = load i32, i32* %3
  %9 = sub i32 %8, 5
  call void @printInt(i32 %9)
  %10 = load i8*, i8** %5
  %11 = load i8*, i8** %7
  %12 = call i8* @__concatStrings__(i8* %10, i8* %11)
  call void @printString(i8* %12)
  ret i32 0
}

