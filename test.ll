declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i32 @__neStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.7 = private unnamed_addr constant [3 x i8] c"OK\00", align 1
@.str.6 = private unnamed_addr constant [12 x i8] c"hello world\00", align 1
@.str.5 = private unnamed_addr constant [3 x i8] c"ld\00", align 1
@.str.4 = private unnamed_addr constant [4 x i8] c"wor\00", align 1
@.str.3 = private unnamed_addr constant [2 x i8] c" \00", align 1
@.str.2 = private unnamed_addr constant [1 x i8] c"\00", align 1
@.str.1 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  %1 = alloca i8*
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.1, i32 0, i32 0), i8** %1
  %2 = alloca i8*
  %3 = call i8* @__concatStrings__(i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.2, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.3, i32 0, i32 0))
  store i8* %3, i8** %2
  %4 = alloca i8*
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.4, i32 0, i32 0), i8** %4
  %5 = alloca i8*
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.5, i32 0, i32 0), i8** %5
  %6 = load i8*, i8** %1
  %7 = load i8*, i8** %2
  %8 = call i8* @__concatStrings__(i8* %6, i8* %7)
  %9 = load i8*, i8** %4
  %10 = call i8* @__concatStrings__(i8* %8, i8* %9)
  %11 = load i8*, i8** %5
  %12 = call i8* @__concatStrings__(i8* %10, i8* %11)
  %13 = call i32 @__equStrings__(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @.str.6, i32 0, i32 0), i8* %12)
  %14 = icmp eq i32 1, %13
  br i1 %14, label %15, label %16
15:
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.7, i32 0, i32 0))
  br label %16
16:
  ret i32 0
}

