declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i32 @__neStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.3 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.str.2 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"apa\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  %1 = alloca i32
  store i32 4, i32* %1
  %2 = load i32, i32* %1
  %3 = icmp sle i32 3, %2
  %4 = xor i1 %3, true
  br i1 %4, label %14, label %5
5:
  %6 = icmp ne i32 4, 2
  %7 = xor i1 %6, true
  br i1 %7, label %10, label %8
8:
  %9 = xor i1 true, true
  br label %10
10:
  %11 = phi i1 [ true, %5 ], [ %9, %8 ]
  %12 = xor i1 %11, true
  %13 = xor i1 %12, true
  br label %14
14:
  %15 = phi i1 [ true, %0 ], [ %13, %10 ]
  %16 = xor i1 %15, true
  br i1 %16, label %17, label %18
17:
  call void @printBool(i1 true)
  br label %19
18:
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0))
  br label %19
19:
  %20 = icmp eq i1 true, true
  br i1 %20, label %23, label %21
21:
  %22 = call i1 @dontCallMe(i32 1)
  br label %23
23:
  %24 = phi i1 [ true, %19 ], [ %22, %21 ]
  call void @printBool(i1 %24)
  %25 = sub i32 0, 5
  %26 = icmp slt i32 4, %25
  %27 = xor i1 %26, true
  br i1 %27, label %31, label %28
28:
  %29 = call i1 @dontCallMe(i32 2)
  %30 = xor i1 %29, true
  br label %31
31:
  %32 = phi i1 [ true, %23 ], [ %30, %28 ]
  %33 = xor i1 %32, true
  call void @printBool(i1 %33)
  %34 = load i32, i32* %1
  %35 = icmp eq i32 4, %34
  %36 = xor i1 %35, true
  br i1 %36, label %47, label %37
37:
  %38 = xor i1 false, true
  %39 = icmp eq i1 true, %38
  %40 = xor i1 %39, true
  br i1 %40, label %43, label %41
41:
  %42 = xor i1 true, true
  br label %43
43:
  %44 = phi i1 [ true, %37 ], [ %42, %41 ]
  %45 = xor i1 %44, true
  %46 = xor i1 %45, true
  br label %47
47:
  %48 = phi i1 [ true, %31 ], [ %46, %43 ]
  %49 = xor i1 %48, true
  call void @printBool(i1 %49)
  %50 = call i1 @implies(i1 false, i1 false)
  call void @printBool(i1 %50)
  %51 = call i1 @implies(i1 false, i1 true)
  call void @printBool(i1 %51)
  %52 = call i1 @implies(i1 true, i1 false)
  call void @printBool(i1 %52)
  %53 = call i1 @implies(i1 true, i1 true)
  call void @printBool(i1 %53)
  ret i32 0
}

define i1 @dontCallMe(i32 %0) {
  %2 = alloca i32
  store i32 %0, i32* %2
  %3 = load i32, i32* %2
  call void @printInt(i32 %3)
  ret i1 true
}

define void @printBool(i1 %0) {
  %2 = alloca i1
  store i1 %0, i1* %2
  %3 = load i1, i1* %2
  br i1 %3, label %4, label %5
4:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i32 0, i32 0))
  br label %6
5:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.3, i32 0, i32 0))
  br label %6
6:
  ret void
}

define i1 @implies(i1 %0, i1 %1) {
  %3 = alloca i1
  store i1 %0, i1* %3
  %4 = alloca i1
  store i1 %1, i1* %4
  %5 = load i1, i1* %3
  %6 = xor i1 %5, true
  br i1 %6, label %11, label %7
7:
  %8 = load i1, i1* %3
  %9 = load i1, i1* %4
  %10 = icmp eq i1 %8, %9
  br label %11
11:
  %12 = phi i1 [ true, %0 ], [ %10, %7 ]
  ret i1 %12
}

