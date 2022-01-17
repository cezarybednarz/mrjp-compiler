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
  %1 = alloca i32
  store i32 0, i32* %1
  %2 = alloca i32
  store i32 56, i32* %2
  %3 = load i32, i32* %2
  %4 = add i32 %3, 45
  %5 = icmp sle i32 %4, 2
  br i1 %5, label %6, label %7
6:
  store i32 1, i32* %1
  br label %8
7:
  store i32 2, i32* %1
  br label %8
8:
  %9 = load i32, i32* %1
  call void @printInt(i32 %9)
  ret i32 0
}

