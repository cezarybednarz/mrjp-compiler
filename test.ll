declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @equStrings(i8*, i8*)
declare i32 @neStrings(i8*, i8*)
declare i8* @concatStrings(i8*, i8*)
declare i32 @compareStrings(i8*, i8*)

define i32 @main() {
  %1 = alloca i32
  store i32 10, i32* %1
  br label %2
2:
  %3 = load i32, i32* %1
  %4 = icmp sgt i32 %3, 0
  br i1 %4, label %5, label %9
5:
  %6 = load i32, i32* %1
  call void @printInt(i32 %6)
  %7 = load i32, i32* %1
  %8 = sub i32 %7, 1
  store i32 %8, i32* %1
  br label %2
9:
  ret i32 0
}

