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
  %2 = call i32 @foo()
  %3 = alloca i32
  store i32 %2, i32* %3
  %4 = load i32, i32* %3
  call void @printInt(i32 %4)
  ret i32 0
}

define i32 @foo() {
  br label %1
1:
  ret i32 10
}

