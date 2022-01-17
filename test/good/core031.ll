declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i32 @__neStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %1
1:
  %2 = sub i32 0, 1
  %3 = call i32 @f(i32 1, i32 %2)
  call void @printInt(i32 %3)
  ret i32 0
}

define i32 @f(i32 %0, i32 %1) {
  br label %3
3:
  %4 = alloca i32
  store i32 %0, i32* %4
  %5 = alloca i32
  store i32 %1, i32* %5
  %6 = load i32, i32* %4
  %7 = icmp sgt i32 %6, 0
  %8 = xor i1 %7, true
  br i1 %8, label %13, label %9
9:
  %10 = load i32, i32* %5
  %11 = icmp sgt i32 %10, 0
  %12 = xor i1 %11, true
  br label %13
13:
  %14 = phi i1 [ true, %3 ], [ %12, %9 ]
  %15 = xor i1 %14, true
  br i1 %15, label %27, label %16
16:
  %17 = load i32, i32* %4
  %18 = icmp slt i32 %17, 0
  %19 = xor i1 %18, true
  br i1 %19, label %24, label %20
20:
  %21 = load i32, i32* %5
  %22 = icmp slt i32 %21, 0
  %23 = xor i1 %22, true
  br label %24
24:
  %25 = phi i1 [ true, %16 ], [ %23, %20 ]
  %26 = xor i1 %25, true
  br label %27
27:
  %28 = phi i1 [ true, %13 ], [ %26, %24 ]
  br i1 %28, label %29, label %31
29:
  ret i32 7
  br label %33
31:
  ret i32 42
  br label %33
33:
  ret i32 0
}

