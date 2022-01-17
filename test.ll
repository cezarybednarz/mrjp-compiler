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
  br i1 %17, label %18, label %37
18:
  call void @printInt(i32 1)
  store i32 4, i32* %1
  %19 = xor i1 false, true
  br i1 %19, label %29, label %20
20:
  br i1 false, label %26, label %21
21:
  br i1 false, label %24, label %22
22:
  %23 = load i1, i1* %2
  br label %24
24:
  %25 = phi i1 [ true, %21 ], [ %23, %22 ]
  br label %26
26:
  %27 = phi i1 [ true, %20 ], [ %25, %24 ]
  %28 = xor i1 %27, true
  br label %29
29:
  %30 = phi i1 [ true, %18 ], [ %28, %26 ]
  %31 = xor i1 %30, true
  br i1 %31, label %32, label %34
32:
  call void @printInt(i32 2)
  %33 = alloca i32
  store i32 6, i32* %33
  br label %35
34:
  call void @printInt(i32 42)
  br label %35
35:
  %36 = alloca i32
  store i32 5, i32* %36
  br label %37
37:
  %38 = load i32, i32* %1
  call void @printInt(i32 %38)
  ret i32 0
}

