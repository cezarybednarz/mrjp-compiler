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
  %2 = call i32 @ev(i32 17)
  call void @printInt(i32 %2)
  ret i32 0
}

define i32 @ev(i32 %0) {
  br label %2
2:
  %3 = alloca i32
  store i32 %0, i32* %3
  %4 = load i32, i32* %3
  %5 = icmp sgt i32 %4, 0
  br i1 %5, label %6, label %11
6:
  %7 = load i32, i32* %3
  %8 = sub i32 %7, 2
  %9 = call i32 @ev(i32 %8)
  ret i32 %9
  br label %19
11:
  %12 = load i32, i32* %3
  %13 = icmp slt i32 %12, 0
  br i1 %13, label %14, label %16
14:
  ret i32 0
  br label %18
16:
  ret i32 1
  br label %18
18:
  br label %19
19:
  ret i32 0
}

