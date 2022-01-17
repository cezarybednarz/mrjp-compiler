declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @equStrings(i8*, i8*)
declare i32 @neStrings(i8*, i8*)
declare i8* @concatStrings(i8*, i8*)
declare i32 @compareStrings(i8*, i8*)
@.str.1 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  %1 = alloca i8*
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.1, i32 0, i32 0), i8** %1
  %2 = load i8*, i8** %1
  call void @printString(i8* %2)
  ret i32 0
}

