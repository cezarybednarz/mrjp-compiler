declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.1 = private unnamed_addr constant [3 x i8] c"ok\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:
  br i1 true, label %L3, label %L2
L2:
  br label %L3
L3:
  %r4 = phi i1 [ true, %L2 ], [ true, %L1 ]
  br i1 %r4, label %L5, label %L6
L5:
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0))
  br label %L6
L6:
  ret i32 0
}

