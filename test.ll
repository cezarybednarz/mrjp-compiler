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
  %1 = alloca i1
  br i1 true, label %6, label %2
2:
  br i1 true, label %4, label %3
3:
  br label %4
4:
  %5 = phi i1 [ true, %2 ], [ true, %3 ]
  br label %6
6:
  %7 = phi i1 [ true, %0 ], [ %5, %4 ]
  store i1 %7, i1* %1
  %8 = alloca i1
  %9 = load i1, i1* %1
  %10 = xor i1 %9, true
  br i1 %10, label %13, label %11
11:
  %12 = xor i1 false, true
  br label %13
13:
  %14 = phi i1 [ true, %6 ], [ %12, %11 ]
  %15 = xor i1 %14, true
  %16 = xor i1 %15, true
  store i1 %16, i1* %8
  %17 = load i1, i1* %8
  br i1 %17, label %18, label %22
18:
  %19 = mul i32 1, 1
  %20 = add i32 %19, 1
  %21 = sub i32 %20, 1
  call void @printInt(i32 %21)
  br label %22
22:
  ret i32 0
}

