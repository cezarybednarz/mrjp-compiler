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
  %2 = alloca i32
  store i32 1, i32* %2
  %3 = alloca i32
  store i32 2, i32* %3
  %4 = alloca i32
  store i32 1, i32* %4
  %5 = alloca i32
  store i32 2, i32* %5
  %6 = alloca i32
  store i32 1, i32* %6
  %7 = alloca i32
  store i32 2, i32* %7
  %8 = alloca i32
  store i32 1, i32* %8
  %9 = alloca i32
  store i32 2, i32* %9
  %10 = alloca i32
  store i32 1, i32* %10
  %11 = alloca i32
  store i32 2, i32* %11
  %12 = alloca i32
  store i32 1, i32* %12
  %13 = alloca i32
  store i32 2, i32* %13
  %14 = alloca i32
  store i32 1, i32* %14
  %15 = alloca i32
  store i32 2, i32* %15
  %16 = load i32, i32* %2
  %17 = load i32, i32* %3
  %18 = load i32, i32* %4
  %19 = load i32, i32* %5
  %20 = load i32, i32* %6
  %21 = load i32, i32* %7
  %22 = load i32, i32* %8
  %23 = load i32, i32* %9
  %24 = load i32, i32* %10
  %25 = load i32, i32* %11
  %26 = load i32, i32* %12
  %27 = load i32, i32* %13
  %28 = load i32, i32* %14
  %29 = load i32, i32* %15
  %30 = call i32 @foo(i32 %16, i32 %17, i32 %18, i32 %19, i32 %20, i32 %21, i32 %22, i32 %23, i32 %24, i32 %25, i32 %26, i32 %27, i32 %28, i32 %29)
  ret i32 %30
}

define i32 @foo(i32 %0, i32 %1, i32 %2, i32 %3, i32 %4, i32 %5, i32 %6, i32 %7, i32 %8, i32 %9, i32 %10, i32 %11, i32 %12, i32 %13) {
  br label %15
15:
  %16 = alloca i32
  store i32 %0, i32* %16
  %17 = alloca i32
  store i32 %1, i32* %17
  %18 = alloca i32
  store i32 %2, i32* %18
  %19 = alloca i32
  store i32 %3, i32* %19
  %20 = alloca i32
  store i32 %4, i32* %20
  %21 = alloca i32
  store i32 %5, i32* %21
  %22 = alloca i32
  store i32 %6, i32* %22
  %23 = alloca i32
  store i32 %7, i32* %23
  %24 = alloca i32
  store i32 %8, i32* %24
  %25 = alloca i32
  store i32 %9, i32* %25
  %26 = alloca i32
  store i32 %10, i32* %26
  %27 = alloca i32
  store i32 %11, i32* %27
  %28 = alloca i32
  store i32 %12, i32* %28
  %29 = alloca i32
  store i32 %13, i32* %29
  %30 = load i32, i32* %16
  %31 = mul i32 2, %30
  %32 = load i32, i32* %17
  %33 = sdiv i32 %32, 2
  %34 = add i32 %31, %33
  %35 = load i32, i32* %18
  %36 = add i32 %34, %35
  %37 = load i32, i32* %19
  %38 = add i32 %36, %37
  %39 = load i32, i32* %20
  %40 = add i32 %38, %39
  %41 = load i32, i32* %21
  %42 = add i32 %40, %41
  %43 = load i32, i32* %22
  %44 = add i32 %42, %43
  %45 = load i32, i32* %23
  %46 = add i32 %44, %45
  %47 = load i32, i32* %24
  %48 = add i32 %46, %47
  %49 = load i32, i32* %25
  %50 = sdiv i32 %49, 2
  %51 = add i32 %48, %50
  %52 = load i32, i32* %26
  %53 = add i32 %51, %52
  %54 = load i32, i32* %27
  %55 = add i32 %53, %54
  %56 = load i32, i32* %28
  %57 = add i32 %55, %56
  %58 = load i32, i32* %29
  %59 = add i32 %57, %58
  %60 = srem i32 %59, 10
  %61 = alloca i32
  store i32 %60, i32* %61
  %62 = load i32, i32* %61
  call void @printInt(i32 %62)
  %63 = load i32, i32* %61
  ret i32 %63
}

