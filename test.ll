declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %1
1:
  %2 = call i1 @test(i32 3)
  %3 = xor i1 %2, true
  br i1 %3, label %8, label %4
4:
  %5 = sub i32 0, 5
  %6 = call i1 @test(i32 %5)
  %7 = xor i1 %6, true
  br label %8
8:
  %9 = phi i1 [ true, %1 ], [ %7, %4 ]
  %10 = xor i1 %9, true
  br i1 %10, label %11, label %13
11:
  ret i32 0
  br label %13
13:
  %14 = call i1 @test(i32 3)
  %15 = xor i1 %14, true
  br i1 %15, label %20, label %16
16:
  %17 = sub i32 0, 5
  %18 = call i1 @test(i32 %17)
  %19 = xor i1 %18, true
  br label %20
20:
  %21 = phi i1 [ true, %13 ], [ %19, %16 ]
  %22 = xor i1 %21, true
  br i1 %22, label %23, label %25
23:
  ret i32 0
  br label %25
25:
  ret i32 0
}

define i1 @test(i32 %0) {
  br label %2
2:
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = load i32, i32* %3
  %5 = icmp sgt i32 %4, 0
  ret i1 %5
}

