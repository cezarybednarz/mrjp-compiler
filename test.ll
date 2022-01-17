declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i32 @__neStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.1 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  call void @f()
  ret i32 0
}

define void @f() {
  br i1 true, label %2, label %1
1:
  br label %2
2:
  %3 = phi i1 [ true, %0 ], [ true, %1 ]
  br i1 %3, label %4, label %6
4:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.1, i32 0, i32 0))
  ret void
  br label %6
6:
  ret void
}

