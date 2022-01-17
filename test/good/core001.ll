declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i32 @__neStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.4 = private unnamed_addr constant [1 x i8] c"\00", align 1
@.str.3 = private unnamed_addr constant [9 x i8] c"/* world\00", align 1
@.str.2 = private unnamed_addr constant [9 x i8] c"hello */\00", align 1
@.str.1 = private unnamed_addr constant [2 x i8] c"=\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  %1 = call i32 @fac(i32 10)
  call void @printInt(i32 %1)
  %2 = call i32 @rfac(i32 10)
  call void @printInt(i32 %2)
  %3 = call i32 @mfac(i32 10)
  call void @printInt(i32 %3)
  %4 = call i32 @ifac(i32 10)
  call void @printInt(i32 %4)
  %5 = alloca i8*
  store i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.0, i32 0, i32 0), i8** %5
  %6 = alloca i32
  store i32 10, i32* %6
  %7 = alloca i32
  store i32 1, i32* %7
  br label %8
8:
  %9 = load i32, i32* %6
  %10 = icmp sgt i32 %9, 0
  br i1 %10, label %11, label %17
11:
  %12 = load i32, i32* %7
  %13 = load i32, i32* %6
  %14 = mul i32 %12, %13
  store i32 %14, i32* %7
  %15 = load i32, i32* %6
  %16 = sub i32 %15, 1
  store i32 %16, i32* %6
  br label %8
17:
  %18 = load i32, i32* %7
  call void @printInt(i32 %18)
  %19 = call i8* @repStr(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i32 0, i32 0), i32 60)
  call void @printString(i8* %19)
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @.str.2, i32 0, i32 0))
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @.str.3, i32 0, i32 0))
  ret i32 0
}

define i32 @fac(i32 %0) {
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
20:
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
}

define i32 @rfac(i32 %0) {
  ret i32 1
  br label %12
6:
  %7 = load i32, i32* %2
  %8 = load i32, i32* %2
  %9 = sub i32 %8, 1
  %10 = call i32 @rfac(i32 %9)
  %11 = mul i32 %7, %10
  ret i32 %11
  br label %12
12:
17:
  %2 = alloca i32
  store i32 %0, i32* %2
  %3 = load i32, i32* %2
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %5, label %6
}

define i32 @mfac(i32 %0) {
  ret i32 1
  br label %12
6:
  %7 = load i32, i32* %2
  %8 = load i32, i32* %2
  %9 = sub i32 %8, 1
  %10 = call i32 @nfac(i32 %9)
  %11 = mul i32 %7, %10
  ret i32 %11
  br label %12
12:
13:
  %2 = alloca i32
  store i32 %0, i32* %2
  %3 = load i32, i32* %2
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %5, label %6
}

define i32 @nfac(i32 %0) {
  %6 = load i32, i32* %2
  %7 = sub i32 %6, 1
  %8 = call i32 @mfac(i32 %7)
  %9 = load i32, i32* %2
  %10 = mul i32 %8, %9
  ret i32 %10
  br label %12
11:
  ret i32 1
  br label %12
12:
13:
  %2 = alloca i32
  store i32 %0, i32* %2
  %3 = load i32, i32* %2
  %4 = icmp ne i32 %3, 0
  br i1 %4, label %5, label %11
}

define i32 @ifac(i32 %0) {
  %2 = alloca i32
  store i32 %0, i32* %2
  %3 = load i32, i32* %2
  %4 = call i32 @ifac2f(i32 1, i32 %3)
  ret i32 %4
}

define i32 @ifac2f(i32 %0, i32 %1) {
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = alloca i32
  store i32 %1, i32* %4
  %5 = load i32, i32* %3
  %6 = load i32, i32* %4
  %7 = icmp eq i32 %5, %6
  br i1 %7, label %8, label %10
8:
  %9 = load i32, i32* %3
  ret i32 %9
  br label %10
10:
  %11 = load i32, i32* %3
  %12 = load i32, i32* %4
  %13 = icmp sgt i32 %11, %12
  br i1 %13, label %14, label %15
14:
  ret i32 1
  br label %15
15:
  %16 = alloca i32
  store i32 0, i32* %16
  %17 = load i32, i32* %3
  %18 = load i32, i32* %4
  %19 = add i32 %17, %18
  %20 = sdiv i32 %19, 2
  store i32 %20, i32* %16
  %21 = load i32, i32* %3
  %22 = load i32, i32* %16
  %23 = call i32 @ifac2f(i32 %21, i32 %22)
  %24 = load i32, i32* %16
  %25 = add i32 %24, 1
  %26 = load i32, i32* %4
  %27 = call i32 @ifac2f(i32 %25, i32 %26)
  %28 = mul i32 %23, %27
  ret i32 %28
}

define i8* @repStr(i8* %0, i32 %1) {
  %8 = load i32, i32* %6
  %9 = load i32, i32* %4
  %10 = icmp slt i32 %8, %9
  br i1 %10, label %11, label %17
11:
  %12 = load i8*, i8** %5
  %13 = load i8*, i8** %3
  %14 = call i8* @__concatStrings__(i8* %12, i8* %13)
  store i8* %14, i8** %5
  %15 = load i32, i32* %6
  %16 = add i32 %15, 1
  store i32 %16, i32* %6
  br label %7
17:
  %18 = load i8*, i8** %5
  ret i8* %18
29:
  %3 = alloca i8*
  store i8* %0, i8** %3
  %4 = alloca i32
  store i32 %1, i32* %4
  %5 = alloca i8*
  store i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.4, i32 0, i32 0), i8** %5
  %6 = alloca i32
  store i32 0, i32* %6
  br label %7
}

