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
  %1 = call i32 @ev(i32 17)
  call void @printInt(i32 %1)
  ret i32 0
}

define i32 @ev(i32 %0) {
  %2 = alloca i32
  store i32 %0, i32* %2
  %3 = load i32, i32* %2
  %4 = icmp sgt i32 %3, 0
  br i1 %4, label %5, label %10
5:
  %6 = load i32, i32* %2
  %7 = sub i32 %6, 2
  %8 = call i32 @ev(i32 %7)
  ret i32 %8
  br label %18
10:
  %11 = load i32, i32* %2
  %12 = icmp slt i32 %11, 0
  br i1 %12, label %13, label %15
13:
  ret i32 0
  br label %17
15:
  ret i32 1
  br label %17
17:
  br label %18
18:
  ret i32 0
}

