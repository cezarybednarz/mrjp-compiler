declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @equStrings(i8*, i8*)
declare i32 @neStrings(i8*, i8*)
declare i8* @concatStrings(i8*, i8*)
declare i32 @compareStrings(i8*, i8*)

define i32 @add(i32 %0, i32 %1) {
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = alloca i32
  store i32 %1, i32* %4
  %5 = load i32, i32* %3
  %6 = load i32, i32* %4
  %7 = add i32 %5, %6
  ret i32 %7
}

define i32 @main() {
  %1 = alloca i32
  store i32 4, i32* %1
  %2 = alloca i32
  store i32 3, i32* %2
  %3 = load i32, i32* %1
  %4 = load i32, i32* %2
  %5 = call i32 @add(i32 %3, i32 %4)
  call void @printInt(i32 %5)
  ret i32 0
}

