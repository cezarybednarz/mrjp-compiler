declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i32 @__neStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.5 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.str.4 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.3 = private unnamed_addr constant [14 x i8] c"concatenation\00", align 1
@.str.2 = private unnamed_addr constant [2 x i8] c" \00", align 1
@.str.1 = private unnamed_addr constant [7 x i8] c"string\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  %1 = alloca i32
  store i32 56, i32* %1
  %2 = alloca i32
  %3 = sub i32 0, 23
  store i32 %3, i32* %2
  %4 = load i32, i32* %1
  %5 = load i32, i32* %2
  %6 = add i32 %4, %5
  call void @printInt(i32 %6)
  %7 = load i32, i32* %1
  %8 = load i32, i32* %2
  %9 = sub i32 %7, %8
  call void @printInt(i32 %9)
  %10 = load i32, i32* %1
  %11 = load i32, i32* %2
  %12 = mul i32 %10, %11
  call void @printInt(i32 %12)
  %13 = sdiv i32 45, 2
  call void @printInt(i32 %13)
  %14 = srem i32 78, 3
  call void @printInt(i32 %14)
  %15 = load i32, i32* %1
  %16 = load i32, i32* %2
  %17 = sub i32 %15, %16
  %18 = load i32, i32* %1
  %19 = load i32, i32* %2
  %20 = add i32 %18, %19
  %21 = icmp sgt i32 %17, %20
  call void @printBool(i1 %21)
  %22 = load i32, i32* %1
  %23 = load i32, i32* %2
  %24 = sdiv i32 %22, %23
  %25 = load i32, i32* %1
  %26 = load i32, i32* %2
  %27 = mul i32 %25, %26
  %28 = icmp sle i32 %24, %27
  call void @printBool(i1 %28)
  %29 = call i8* @__concatStrings__(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.1, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  %30 = call i8* @__concatStrings__(i8* %29, i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.3, i32 0, i32 0))
  call void @printString(i8* %30)
  ret i32 0
}

define void @printBool(i1 %0) {
  %2 = alloca i1
  store i1 %0, i1* %2
  %3 = load i1, i1* %2
  br i1 %3, label %4, label %6
4:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.4, i32 0, i32 0))
  ret void
  br label %8
6:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.5, i32 0, i32 0))
  ret void
  br label %8
8:
  ret void
}

