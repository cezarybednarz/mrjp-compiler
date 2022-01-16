declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @equStrings(i8*, i8*)
declare i32 @neStrings(i8*, i8*)
declare i8* @concatStrings(i8*, i8*)
declare i32 @compareStrings(i8*, i8*)

define i32 @sub(i32 %0, i32 %1) {
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = alloca i32
  store i32 %1, i32* %4
  %5 = load i32, i32* %3
  %6 = load i32, i32* %4
  %7 = sub i32 %5, %6
  %8 = load i32, i32* %3
  %9 = load i32, i32* %3
  %10 = sub i32 %8, %9
  %11 = add i32 %7, %10
  %12 = load i32, i32* %4
  %13 = load i32, i32* %4
  %14 = sub i32 %12, %13
  %15 = add i32 %11, %14
  %16 = load i32, i32* %3
  %17 = add i32 %15, %16
  %18 = load i32, i32* %3
  %19 = sub i32 0, %18
  %20 = add i32 %17, %19
  ret i32 %20
}

define void @p(i32 %0, i32 %1, i1 %2) {
  %4 = alloca i32
  store i32 %0, i32* %4
  %5 = alloca i32
  store i32 %1, i32* %5
  %6 = alloca i1
  store i1 %2, i1* %6
  %7 = load i32, i32* %4
  %8 = load i32, i32* %5
  %9 = add i32 %7, %8
  call void @printInt(i32 %9)
  ret void
}

define i32 @main() {
  %1 = alloca i32
  store i32 4, i32* %1
  %2 = alloca i32
  store i32 3, i32* %2
  %3 = load i32, i32* %2
  %4 = load i32, i32* %2
  %5 = sub i32 0, %4
  %6 = add i32 %3, %5
  %7 = load i32, i32* %1
  %8 = add i32 %6, %7
  store i32 %8, i32* %1
  %9 = load i32, i32* %1
  %10 = load i32, i32* %1
  %11 = mul i32 %9, %10
  %12 = load i32, i32* %1
  %13 = sdiv i32 %11, %12
  store i32 %13, i32* %1
  %14 = load i32, i32* %1
  %15 = load i32, i32* %2
  %16 = sub i32 %14, %15
  %17 = load i32, i32* %1
  %18 = load i32, i32* %1
  %19 = sub i32 %17, %18
  call void @p(i32 %16, i32 %19, i1 true)
  %20 = load i32, i32* %1
  %21 = load i32, i32* %2
  %22 = call i32 @sub(i32 %20, i32 %21)
  %23 = srem i32 1000, 10
  %24 = add i32 %22, %23
  call void @printInt(i32 %24)
  ret i32 0
}

