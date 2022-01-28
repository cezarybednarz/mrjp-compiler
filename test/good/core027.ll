declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
declare i8* @malloc(i32) nounwind
@.str.2 = private unnamed_addr constant [5 x i8] c"good\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"bad\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  call void @f(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0))
  ret i32 0
}

define void @f(i8* %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r3 = alloca i8*
  store i8* %r0, i8** %r3
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i32 0, i32 0), i8** %r3
  %r4 = load i8*, i8** %r3
  call void @printString(i8* %r4)
  ret void
}

