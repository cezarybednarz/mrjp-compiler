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
  %2 = alloca i32
  store i32 0, i32* %2
  %3 = alloca i32
  store i32 56, i32* %3
  %4 = load i32, i32* %3
  %5 = add i32 %4, 45
  %6 = icmp sle i32 %5, 2
  br i1 %6, label %7, label %8
7:
  store i32 1, i32* %2
  br label %9
8:
  store i32 2, i32* %2
  br label %9
9:
  %10 = load i32, i32* %2
  call void @printInt(i32 %10)
  ret i32 0
}

