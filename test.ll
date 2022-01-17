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
  store i32 3, i32* %1
  %2 = alloca i1
  br i1 false, label %13, label %3
3:
  %4 = xor i1 true, true
  br i1 %4, label %7, label %5
5:
  %6 = xor i1 true, true
  br label %7
7:
  %8 = phi i1 [ true, %3 ], [ %6, %5 ]
  %9 = xor i1 %8, true
  br i1 %9, label %11, label %10
10:
  br label %11
11:
  %12 = phi i1 [ true, %7 ], [ false, %10 ]
  br label %13
13:
  %14 = phi i1 [ true, %0 ], [ %12, %11 ]
  store i1 %14, i1* %2
  br i1 true, label %16, label %15
15:
  br label %16
16:
  %17 = phi i1 [ true, %13 ], [ true, %15 ]
  br i1 %17, label %18, label %30
18:
  call void @printInt(i32 1)
  store i32 4, i32* %1
  br i1 false, label %24, label %19
19:
  br i1 false, label %22, label %20
20:
  %21 = load i1, i1* %2
  br label %22
22:
  %23 = phi i1 [ true, %19 ], [ %21, %20 ]
  br label %24
24:
  %25 = phi i1 [ true, %18 ], [ %23, %22 ]
  br i1 %25, label %26, label %28
26:
  call void @printInt(i32 2)
  %27 = alloca i32
  store i32 6, i32* %27
  br label %28
28:
  %29 = alloca i32
  store i32 5, i32* %29
  br label %30
30:
  %31 = load i32, i32* %1
  call void @printInt(i32 %31)
  ret i32 0
}

