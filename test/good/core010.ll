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
  %1 = call i32 @fac(i32 5)
  call void @printInt(i32 %1)
  ret i32 0
}

define i32 @fac(i32 %0) {
  %2 = alloca i32
  store i32 %0, i32* %2
  %3 = alloca i32
  store i32 0, i32* %3
  %4 = alloca i32
  store i32 0, i32* %4
  store i32 1, i32* %3
  %5 = load i32, i32* %2
  store i32 %5, i32* %4
  br label %6
6:
  %7 = load i32, i32* %4
  %8 = icmp sgt i32 %7, 0
  br i1 %8, label %9, label %15
9:
  %10 = load i32, i32* %3
  %11 = load i32, i32* %4
  %12 = mul i32 %10, %11
  store i32 %12, i32* %3
  %13 = load i32, i32* %4
  %14 = sub i32 %13, 1
  store i32 %14, i32* %4
  br label %6
15:
  %16 = load i32, i32* %3
  ret i32 %16
}

