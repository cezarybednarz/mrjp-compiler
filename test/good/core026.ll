declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @d() {
  br label %1
1:
  ret i32 0
}

define i32 @s(i32 %0) {
  br label %2
2:
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = load i32, i32* %3
  %5 = add i32 %4, 1
  ret i32 %5
}

define i32 @main() {
  br label %1
1:
  %2 = call i32 @d()
  %3 = call i32 @s(i32 %2)
  %4 = call i32 @s(i32 %3)
  %5 = call i32 @s(i32 %4)
  %6 = call i32 @s(i32 %5)
  %7 = call i32 @s(i32 %6)
  %8 = call i32 @s(i32 %7)
  %9 = call i32 @s(i32 %8)
  %10 = call i32 @s(i32 %9)
  %11 = call i32 @s(i32 %10)
  %12 = call i32 @s(i32 %11)
  %13 = call i32 @s(i32 %12)
  %14 = call i32 @s(i32 %13)
  %15 = call i32 @s(i32 %14)
  %16 = call i32 @s(i32 %15)
  %17 = call i32 @s(i32 %16)
  %18 = call i32 @s(i32 %17)
  %19 = call i32 @s(i32 %18)
  %20 = call i32 @s(i32 %19)
  %21 = call i32 @s(i32 %20)
  %22 = call i32 @s(i32 %21)
  %23 = call i32 @s(i32 %22)
  %24 = call i32 @s(i32 %23)
  %25 = call i32 @s(i32 %24)
  %26 = call i32 @s(i32 %25)
  %27 = call i32 @s(i32 %26)
  %28 = call i32 @s(i32 %27)
  %29 = call i32 @s(i32 %28)
  %30 = call i32 @s(i32 %29)
  %31 = call i32 @s(i32 %30)
  %32 = call i32 @s(i32 %31)
  %33 = call i32 @s(i32 %32)
  %34 = call i32 @s(i32 %33)
  %35 = call i32 @s(i32 %34)
  %36 = call i32 @s(i32 %35)
  %37 = call i32 @s(i32 %36)
  %38 = call i32 @s(i32 %37)
  %39 = call i32 @s(i32 %38)
  %40 = call i32 @s(i32 %39)
  %41 = call i32 @s(i32 %40)
  %42 = call i32 @s(i32 %41)
  %43 = call i32 @s(i32 %42)
  %44 = call i32 @s(i32 %43)
  %45 = call i32 @s(i32 %44)
  %46 = call i32 @s(i32 %45)
  %47 = call i32 @s(i32 %46)
  %48 = call i32 @s(i32 %47)
  %49 = call i32 @s(i32 %48)
  %50 = call i32 @s(i32 %49)
  %51 = call i32 @s(i32 %50)
  %52 = call i32 @s(i32 %51)
  %53 = call i32 @s(i32 %52)
  %54 = call i32 @s(i32 %53)
  %55 = call i32 @s(i32 %54)
  %56 = call i32 @s(i32 %55)
  %57 = call i32 @s(i32 %56)
  %58 = call i32 @s(i32 %57)
  %59 = call i32 @s(i32 %58)
  %60 = call i32 @s(i32 %59)
  %61 = call i32 @s(i32 %60)
  %62 = call i32 @s(i32 %61)
  %63 = call i32 @s(i32 %62)
  %64 = call i32 @s(i32 %63)
  %65 = call i32 @s(i32 %64)
  %66 = call i32 @s(i32 %65)
  %67 = call i32 @s(i32 %66)
  %68 = call i32 @s(i32 %67)
  %69 = call i32 @s(i32 %68)
  %70 = call i32 @s(i32 %69)
  %71 = call i32 @s(i32 %70)
  %72 = call i32 @s(i32 %71)
  %73 = call i32 @s(i32 %72)
  %74 = call i32 @s(i32 %73)
  %75 = call i32 @s(i32 %74)
  %76 = call i32 @s(i32 %75)
  %77 = call i32 @s(i32 %76)
  %78 = call i32 @s(i32 %77)
  %79 = call i32 @s(i32 %78)
  %80 = call i32 @s(i32 %79)
  %81 = call i32 @s(i32 %80)
  %82 = call i32 @s(i32 %81)
  call void @printInt(i32 %82)
  ret i32 0
}

