declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.3 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.str.2 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"apa\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %1
1:
  %2 = alloca i32
  store i32 4, i32* %2
  %3 = load i32, i32* %2
  %4 = icmp sle i32 3, %3
  %5 = xor i1 %4, true
  br i1 %5, label %15, label %6
6:
  %7 = icmp ne i32 4, 2
  %8 = xor i1 %7, true
  br i1 %8, label %11, label %9
9:
  %10 = xor i1 true, true
  br label %11
11:
  %12 = phi i1 [ true, %6 ], [ %10, %9 ]
  %13 = xor i1 %12, true
  %14 = xor i1 %13, true
  br label %15
15:
  %16 = phi i1 [ true, %1 ], [ %14, %11 ]
  %17 = xor i1 %16, true
  br i1 %17, label %18, label %19
18:
  call void @printBool(i1 true)
  br label %20
19:
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0))
  br label %20
20:
  %21 = icmp eq i1 true, true
  br i1 %21, label %24, label %22
22:
  %23 = call i1 @dontCallMe(i32 1)
  br label %24
24:
  %25 = phi i1 [ true, %20 ], [ %23, %22 ]
  call void @printBool(i1 %25)
  %26 = sub i32 0, 5
  %27 = icmp slt i32 4, %26
  %28 = xor i1 %27, true
  br i1 %28, label %32, label %29
29:
  %30 = call i1 @dontCallMe(i32 2)
  %31 = xor i1 %30, true
  br label %32
32:
  %33 = phi i1 [ true, %24 ], [ %31, %29 ]
  %34 = xor i1 %33, true
  call void @printBool(i1 %34)
  %35 = load i32, i32* %2
  %36 = icmp eq i32 4, %35
  %37 = xor i1 %36, true
  br i1 %37, label %48, label %38
38:
  %39 = xor i1 false, true
  %40 = icmp eq i1 true, %39
  %41 = xor i1 %40, true
  br i1 %41, label %44, label %42
42:
  %43 = xor i1 true, true
  br label %44
44:
  %45 = phi i1 [ true, %38 ], [ %43, %42 ]
  %46 = xor i1 %45, true
  %47 = xor i1 %46, true
  br label %48
48:
  %49 = phi i1 [ true, %32 ], [ %47, %44 ]
  %50 = xor i1 %49, true
  call void @printBool(i1 %50)
  %51 = call i1 @implies(i1 false, i1 false)
  call void @printBool(i1 %51)
  %52 = call i1 @implies(i1 false, i1 true)
  call void @printBool(i1 %52)
  %53 = call i1 @implies(i1 true, i1 false)
  call void @printBool(i1 %53)
  %54 = call i1 @implies(i1 true, i1 true)
  call void @printBool(i1 %54)
  ret i32 0
}

define i1 @dontCallMe(i32 %0) {
  br label %2
2:
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = load i32, i32* %3
  call void @printInt(i32 %4)
  ret i1 true
}

define void @printBool(i1 %0) {
  br label %2
2:
  %3 = alloca i1
  store i1 %0, i1* %3
  %4 = load i1, i1* %3
  br i1 %4, label %5, label %6
5:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i32 0, i32 0))
  br label %7
6:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.3, i32 0, i32 0))
  br label %7
7:
  ret void
}

define i1 @implies(i1 %0, i1 %1) {
  br label %3
3:
  %4 = alloca i1
  store i1 %0, i1* %4
  %5 = alloca i1
  store i1 %1, i1* %5
  %6 = load i1, i1* %4
  %7 = xor i1 %6, true
  br i1 %7, label %12, label %8
8:
  %9 = load i1, i1* %4
  %10 = load i1, i1* %5
  %11 = icmp eq i1 %9, %10
  br label %12
12:
  %13 = phi i1 [ true, %3 ], [ %11, %8 ]
  ret i1 %13
}

