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
  br label %1
1:
  %2 = call i32 @fac(i32 10)
  call void @printInt(i32 %2)
  %3 = call i32 @rfac(i32 10)
  call void @printInt(i32 %3)
  %4 = call i32 @mfac(i32 10)
  call void @printInt(i32 %4)
  %5 = call i32 @ifac(i32 10)
  call void @printInt(i32 %5)
  %6 = alloca i8*
  store i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.0, i32 0, i32 0), i8** %6
  %7 = alloca i32
  store i32 10, i32* %7
  %8 = alloca i32
  store i32 1, i32* %8
  br label %9
9:
  %10 = load i32, i32* %7
  %11 = icmp sgt i32 %10, 0
  br i1 %11, label %12, label %18
12:
  %13 = load i32, i32* %8
  %14 = load i32, i32* %7
  %15 = mul i32 %13, %14
  store i32 %15, i32* %8
  %16 = load i32, i32* %7
  %17 = sub i32 %16, 1
  store i32 %17, i32* %7
  br label %9
18:
  %19 = load i32, i32* %8
  call void @printInt(i32 %19)
  %20 = call i8* @repStr(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i32 0, i32 0), i32 60)
  call void @printString(i8* %20)
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @.str.2, i32 0, i32 0))
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @.str.3, i32 0, i32 0))
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

define i32 @rfac(i32 %0) {
  br label %2
2:
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = load i32, i32* %3
  %5 = icmp eq i32 %4, 0
  br i1 %5, label %6, label %8
6:
  ret i32 1
  br label %15
8:
  %9 = load i32, i32* %3
  %10 = load i32, i32* %3
  %11 = sub i32 %10, 1
  %12 = call i32 @rfac(i32 %11)
  %13 = mul i32 %9, %12
  ret i32 %13
  br label %15
15:
  ret i32 0
}

define i32 @mfac(i32 %0) {
  br label %2
2:
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = load i32, i32* %3
  %5 = icmp eq i32 %4, 0
  br i1 %5, label %6, label %8
6:
  ret i32 1
  br label %15
8:
  %9 = load i32, i32* %3
  %10 = load i32, i32* %3
  %11 = sub i32 %10, 1
  %12 = call i32 @nfac(i32 %11)
  %13 = mul i32 %9, %12
  ret i32 %13
  br label %15
15:
  ret i32 0
}

define i32 @nfac(i32 %0) {
  br label %2
2:
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = load i32, i32* %3
  %5 = icmp ne i32 %4, 0
  br i1 %5, label %6, label %13
6:
  %7 = load i32, i32* %3
  %8 = sub i32 %7, 1
  %9 = call i32 @mfac(i32 %8)
  %10 = load i32, i32* %3
  %11 = mul i32 %9, %10
  ret i32 %11
  br label %15
13:
  ret i32 1
  br label %15
15:
  ret i32 0
}

define i32 @ifac(i32 %0) {
  br label %2
2:
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = load i32, i32* %3
  %5 = call i32 @ifac2f(i32 1, i32 %4)
  ret i32 %5
}

define i32 @ifac2f(i32 %0, i32 %1) {
  br label %3
3:
  %4 = alloca i32
  store i32 %0, i32* %4
  %5 = alloca i32
  store i32 %1, i32* %5
  %6 = load i32, i32* %4
  %7 = load i32, i32* %5
  %8 = icmp eq i32 %6, %7
  br i1 %8, label %9, label %12
9:
  %10 = load i32, i32* %4
  ret i32 %10
  br label %12
12:
  %13 = load i32, i32* %4
  %14 = load i32, i32* %5
  %15 = icmp sgt i32 %13, %14
  br i1 %15, label %16, label %18
16:
  ret i32 1
  br label %18
18:
  %19 = alloca i32
  store i32 0, i32* %19
  %20 = load i32, i32* %4
  %21 = load i32, i32* %5
  %22 = add i32 %20, %21
  %23 = sdiv i32 %22, 2
  store i32 %23, i32* %19
  %24 = load i32, i32* %4
  %25 = load i32, i32* %19
  %26 = call i32 @ifac2f(i32 %24, i32 %25)
  %27 = load i32, i32* %19
  %28 = add i32 %27, 1
  %29 = load i32, i32* %5
  %30 = call i32 @ifac2f(i32 %28, i32 %29)
  %31 = mul i32 %26, %30
  ret i32 %31
}

define i8* @repStr(i8* %0, i32 %1) {
  br label %3
3:
  %4 = alloca i8*
  store i8* %0, i8** %4
  %5 = alloca i32
  store i32 %1, i32* %5
  %6 = alloca i8*
  store i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.4, i32 0, i32 0), i8** %6
  %7 = alloca i32
  store i32 0, i32* %7
  br label %8
8:
  %9 = load i32, i32* %7
  %10 = load i32, i32* %5
  %11 = icmp slt i32 %9, %10
  br i1 %11, label %12, label %18
12:
  %13 = load i8*, i8** %6
  %14 = load i8*, i8** %4
  %15 = call i8* @__concatStrings__(i8* %13, i8* %14)
  store i8* %15, i8** %6
  %16 = load i32, i32* %7
  %17 = add i32 %16, 1
  store i32 %17, i32* %7
  br label %8
18:
  %19 = load i8*, i8** %6
  ret i8* %19
}

