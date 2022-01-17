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
  %2 = call i32 @fac(i32 5)
  call void @printInt(i32 %2)
  ret i32 0
}

define i32 @fac(i32 %0) {
  br label %2
2:
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = alloca i32
  store i32 0, i32* %4
  %5 = alloca i32
  store i32 0, i32* %5
  store i32 1, i32* %4
  %6 = load i32, i32* %3
  store i32 %6, i32* %5
  br label %7
7:
  %8 = load i32, i32* %5
  %9 = icmp sgt i32 %8, 0
  br i1 %9, label %10, label %16
10:
  %11 = load i32, i32* %4
  %12 = load i32, i32* %5
  %13 = mul i32 %11, %12
  store i32 %13, i32* %4
  %14 = load i32, i32* %5
  %15 = sub i32 %14, 1
  store i32 %15, i32* %5
  br label %7
16:
  %17 = load i32, i32* %4
  ret i32 %17
}

