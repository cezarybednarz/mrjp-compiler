declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.2 = private unnamed_addr constant [3 x i8] c"ok\00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"ok\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i8* @getOkString() {
  br label %L1
L1:
  %r2 = alloca i8*
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0), i8** %r2
  %r3 = load i8*, i8** %r2
  ret i8* %r3
}

define i32 @main() {
  br label %L1
L1:
  %r2 = alloca i8*
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.2, i32 0, i32 0), i8** %r2
  %r3 = alloca i32
  store i32 3, i32* %r3
  %r4 = load i32, i32* %r3
  %r5 = add i32 %r4, 3
  store i32 %r5, i32* %r3
  %r6 = load i32, i32* %r3
  %r7 = sub i32 %r6, 6
  %r8 = icmp eq i32 %r7, 1
  br i1 %r8, label %L9, label %L11
L9:
  %r10 = call i8* @getOkString()
  call void @printString(i8* %r10)
  br label %L11
L11:
  ret i32 0
}

