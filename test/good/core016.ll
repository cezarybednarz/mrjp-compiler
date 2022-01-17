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
  store i32 17, i32* %1
  br label %2
2:
  %3 = load i32, i32* %1
  %4 = icmp sgt i32 %3, 0
  br i1 %4, label %5, label %8
5:
  %6 = load i32, i32* %1
  %7 = sub i32 %6, 2
  store i32 %7, i32* %1
  br label %2
8:
  %9 = load i32, i32* %1
  %10 = icmp slt i32 %9, 0
  br i1 %10, label %11, label %13
11:
  call void @printInt(i32 0)
  ret i32 0
  br label %15
13:
  call void @printInt(i32 1)
  ret i32 0
  br label %15
15:
}

