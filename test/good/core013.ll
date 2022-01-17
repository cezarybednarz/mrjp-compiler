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
  br label %1
1:
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0))
  %2 = sub i32 0, 1
  %3 = call i1 @test(i32 %2)
  %4 = xor i1 %3, true
  br i1 %4, label %8, label %5
5:
  %6 = call i1 @test(i32 0)
  %7 = xor i1 %6, true
  br label %8
8:
  %9 = phi i1 [ true, %1 ], [ %7, %5 ]
  %10 = xor i1 %9, true
  call void @printBool(i1 %10)
  %11 = sub i32 0, 2
  %12 = call i1 @test(i32 %11)
  %13 = xor i1 %12, true
  br i1 %13, label %17, label %14
14:
  %15 = call i1 @test(i32 1)
  %16 = xor i1 %15, true
  br label %17
17:
  %18 = phi i1 [ true, %8 ], [ %16, %14 ]
  %19 = xor i1 %18, true
  call void @printBool(i1 %19)
  %20 = call i1 @test(i32 3)
  %21 = xor i1 %20, true
  br i1 %21, label %26, label %22
22:
  %23 = sub i32 0, 5
  %24 = call i1 @test(i32 %23)
  %25 = xor i1 %24, true
  br label %26
26:
  %27 = phi i1 [ true, %17 ], [ %25, %22 ]
  %28 = xor i1 %27, true
  call void @printBool(i1 %28)
  %29 = call i1 @test(i32 234234)
  %30 = xor i1 %29, true
  br i1 %30, label %34, label %31
31:
  %32 = call i1 @test(i32 21321)
  %33 = xor i1 %32, true
  br label %34
34:
  %35 = phi i1 [ true, %26 ], [ %33, %31 ]
  %36 = xor i1 %35, true
  call void @printBool(i1 %36)
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.2, i32 0, i32 0))
  %37 = sub i32 0, 1
  %38 = call i1 @test(i32 %37)
  br i1 %38, label %41, label %39
39:
  %40 = call i1 @test(i32 0)
  br label %41
41:
  %42 = phi i1 [ true, %34 ], [ %40, %39 ]
  call void @printBool(i1 %42)
  %43 = sub i32 0, 2
  %44 = call i1 @test(i32 %43)
  br i1 %44, label %47, label %45
45:
  %46 = call i1 @test(i32 1)
  br label %47
47:
  %48 = phi i1 [ true, %41 ], [ %46, %45 ]
  call void @printBool(i1 %48)
  %49 = call i1 @test(i32 3)
  br i1 %49, label %53, label %50
50:
  %51 = sub i32 0, 5
  %52 = call i1 @test(i32 %51)
  br label %53
53:
  %54 = phi i1 [ true, %47 ], [ %52, %50 ]
  call void @printBool(i1 %54)
  %55 = call i1 @test(i32 234234)
  br i1 %55, label %58, label %56
56:
  %57 = call i1 @test(i32 21321)
  br label %58
58:
  %59 = phi i1 [ true, %53 ], [ %57, %56 ]
  call void @printBool(i1 %59)
  call void @printString(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.3, i32 0, i32 0))
  call void @printBool(i1 true)
  call void @printBool(i1 false)
  ret i32 0
}

define void @printBool(i1 %0) {
  br label %2
2:
  %3 = alloca i1
  store i1 %0, i1* %3
  %4 = load i1, i1* %3
  %5 = xor i1 %4, true
  br i1 %5, label %6, label %7
6:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.4, i32 0, i32 0))
  br label %8
7:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.5, i32 0, i32 0))
  br label %8
8:
  ret void
}

define i1 @test(i32 %0) {
  br label %2
2:
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = load i32, i32* %3
  call void @printInt(i32 %4)
  %5 = load i32, i32* %3
  %6 = icmp sgt i32 %5, 0
  ret i1 %6
}

