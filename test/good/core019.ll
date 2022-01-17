declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.1 = private unnamed_addr constant [4 x i8] c"foo\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %1
1:
  %2 = alloca i32
  store i32 78, i32* %2
  %3 = alloca i32
  store i32 1, i32* %3
  %4 = load i32, i32* %3
  call void @printInt(i32 %4)
  %5 = load i32, i32* %2
  call void @printInt(i32 %5)
  br label %6
6:
  %7 = load i32, i32* %2
  %8 = icmp sgt i32 %7, 76
  br i1 %8, label %9, label %17
9:
  %10 = load i32, i32* %2
  %11 = sub i32 %10, 1
  store i32 %11, i32* %2
  %12 = load i32, i32* %2
  call void @printInt(i32 %12)
  %13 = load i32, i32* %2
  %14 = add i32 %13, 7
  %15 = alloca i32
  store i32 %14, i32* %15
  %16 = load i32, i32* %15
  call void @printInt(i32 %16)
  br label %6
17:
  %18 = load i32, i32* %2
  call void @printInt(i32 %18)
  %19 = load i32, i32* %2
  %20 = icmp sgt i32 %19, 4
  br i1 %20, label %21, label %24
21:
  %22 = alloca i32
  store i32 4, i32* %22
  %23 = load i32, i32* %22
  call void @printInt(i32 %23)
  br label %25
24:
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0))
  br label %25
25:
  %26 = load i32, i32* %2
  call void @printInt(i32 %26)
  ret i32 0
}

