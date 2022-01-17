declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.5 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.str.4 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.3 = private unnamed_addr constant [14 x i8] c"concatenation\00", align 1
@.str.2 = private unnamed_addr constant [2 x i8] c" \00", align 1
@.str.1 = private unnamed_addr constant [7 x i8] c"string\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %1
1:
  %2 = alloca i32
  store i32 56, i32* %2
  %3 = sub i32 0, 23
  %4 = alloca i32
  store i32 %3, i32* %4
  %5 = load i32, i32* %2
  %6 = load i32, i32* %4
  %7 = add i32 %5, %6
  call void @printInt(i32 %7)
  %8 = load i32, i32* %2
  %9 = load i32, i32* %4
  %10 = sub i32 %8, %9
  call void @printInt(i32 %10)
  %11 = load i32, i32* %2
  %12 = load i32, i32* %4
  %13 = mul i32 %11, %12
  call void @printInt(i32 %13)
  %14 = sdiv i32 45, 2
  call void @printInt(i32 %14)
  %15 = srem i32 78, 3
  call void @printInt(i32 %15)
  %16 = load i32, i32* %2
  %17 = load i32, i32* %4
  %18 = sub i32 %16, %17
  %19 = load i32, i32* %2
  %20 = load i32, i32* %4
  %21 = add i32 %19, %20
  %22 = icmp sgt i32 %18, %21
  call void @printBool(i1 %22)
  %23 = load i32, i32* %2
  %24 = load i32, i32* %4
  %25 = sdiv i32 %23, %24
  %26 = load i32, i32* %2
  %27 = load i32, i32* %4
  %28 = mul i32 %26, %27
  %29 = icmp sle i32 %25, %28
  call void @printBool(i1 %29)
  %30 = call i8* @__concatStrings__(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.1, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  %31 = call i8* @__concatStrings__(i8* %30, i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.3, i32 0, i32 0))
  call void @printString(i8* %31)
  ret i32 0
}

define void @printBool(i1 %0) {
  br label %2
2:
  %3 = alloca i1
  store i1 %0, i1* %3
  %4 = load i1, i1* %3
  br i1 %4, label %5, label %7
5:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.4, i32 0, i32 0))
  ret void
  br label %9
7:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.5, i32 0, i32 0))
  ret void
  br label %9
9:
  ret void
}

