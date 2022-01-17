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
  store i32 17, i32* %2
  br label %3
3:
  %4 = load i32, i32* %2
  %5 = icmp sgt i32 %4, 0
  br i1 %5, label %6, label %9
6:
  %7 = load i32, i32* %2
  %8 = sub i32 %7, 2
  store i32 %8, i32* %2
  br label %3
9:
  %10 = load i32, i32* %2
  %11 = icmp slt i32 %10, 0
  br i1 %11, label %12, label %14
12:
  call void @printInt(i32 0)
  ret i32 0
  br label %16
14:
  call void @printInt(i32 1)
  ret i32 0
  br label %16
16:
  ret i32 0
}

