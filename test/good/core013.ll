declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i32 @__neStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.5 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.4 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.str.3 = private unnamed_addr constant [2 x i8] c"!\00", align 1
@.str.2 = private unnamed_addr constant [3 x i8] c"||\00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"&&\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0))
  %1 = sub i32 0, 1
  %2 = call i1 @test(i32 %1)
  %3 = xor i1 %2, true
  br i1 %3, label %7, label %4
4:
  %5 = call i1 @test(i32 0)
  %6 = xor i1 %5, true
  br label %7
7:
  %8 = phi i1 [ true, %0 ], [ %6, %4 ]
  %9 = xor i1 %8, true
  call void @printBool(i1 %9)
  %10 = sub i32 0, 2
  %11 = call i1 @test(i32 %10)
  %12 = xor i1 %11, true
  br i1 %12, label %16, label %13
13:
  %14 = call i1 @test(i32 1)
  %15 = xor i1 %14, true
  br label %16
16:
  %17 = phi i1 [ true, %7 ], [ %15, %13 ]
  %18 = xor i1 %17, true
  call void @printBool(i1 %18)
  %19 = call i1 @test(i32 3)
  %20 = xor i1 %19, true
  br i1 %20, label %25, label %21
21:
  %22 = sub i32 0, 5
  %23 = call i1 @test(i32 %22)
  %24 = xor i1 %23, true
  br label %25
25:
  %26 = phi i1 [ true, %16 ], [ %24, %21 ]
  %27 = xor i1 %26, true
  call void @printBool(i1 %27)
  %28 = call i1 @test(i32 234234)
  %29 = xor i1 %28, true
  br i1 %29, label %33, label %30
30:
  %31 = call i1 @test(i32 21321)
  %32 = xor i1 %31, true
  br label %33
33:
  %34 = phi i1 [ true, %25 ], [ %32, %30 ]
  %35 = xor i1 %34, true
  call void @printBool(i1 %35)
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.2, i32 0, i32 0))
  %36 = sub i32 0, 1
  %37 = call i1 @test(i32 %36)
  br i1 %37, label %40, label %38
38:
  %39 = call i1 @test(i32 0)
  br label %40
40:
  %41 = phi i1 [ true, %33 ], [ %39, %38 ]
  call void @printBool(i1 %41)
  %42 = sub i32 0, 2
  %43 = call i1 @test(i32 %42)
  br i1 %43, label %46, label %44
44:
  %45 = call i1 @test(i32 1)
  br label %46
46:
  %47 = phi i1 [ true, %40 ], [ %45, %44 ]
  call void @printBool(i1 %47)
  %48 = call i1 @test(i32 3)
  br i1 %48, label %52, label %49
49:
  %50 = sub i32 0, 5
  %51 = call i1 @test(i32 %50)
  br label %52
52:
  %53 = phi i1 [ true, %46 ], [ %51, %49 ]
  call void @printBool(i1 %53)
  %54 = call i1 @test(i32 234234)
  br i1 %54, label %57, label %55
55:
  %56 = call i1 @test(i32 21321)
  br label %57
57:
  %58 = phi i1 [ true, %52 ], [ %56, %55 ]
  call void @printBool(i1 %58)
  call void @printString(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.3, i32 0, i32 0))
  call void @printBool(i1 true)
  call void @printBool(i1 false)
  ret i32 0
}

define void @printBool(i1 %0) {
  %2 = alloca i1
  store i1 %0, i1* %2
  %3 = load i1, i1* %2
  %4 = xor i1 %3, true
  br i1 %4, label %5, label %6
5:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.4, i32 0, i32 0))
  br label %7
6:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.5, i32 0, i32 0))
  br label %7
7:
  ret void
}

define i1 @test(i32 %0) {
  %2 = alloca i32
  store i32 %0, i32* %2
  %3 = load i32, i32* %2
  call void @printInt(i32 %3)
  %4 = load i32, i32* %2
  %5 = icmp sgt i32 %4, 0
  ret i1 %5
}

