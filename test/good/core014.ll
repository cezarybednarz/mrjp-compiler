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
  %4 = alloca i32
  store i32 0, i32* %4
  store i32 1, i32* %2
  %5 = load i32, i32* %2
  store i32 %5, i32* %3
  store i32 5000000, i32* %4
  %6 = load i32, i32* %2
  call void @printInt(i32 %6)
  br label %7
7:
  %8 = load i32, i32* %3
  %9 = load i32, i32* %4
  %10 = icmp slt i32 %8, %9
  br i1 %10, label %11, label %19
11:
  %12 = load i32, i32* %3
  call void @printInt(i32 %12)
  %13 = load i32, i32* %2
  %14 = load i32, i32* %3
  %15 = add i32 %13, %14
  store i32 %15, i32* %3
  %16 = load i32, i32* %3
  %17 = load i32, i32* %2
  %18 = sub i32 %16, %17
  store i32 %18, i32* %2
  br label %7
19:
  ret i32 0
}

