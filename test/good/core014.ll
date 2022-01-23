declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:
  call void @printInt(i32 1)
  br label %L7
L7:
  %r10 = icmp slt i32 %r5, 5000000
  br i1 %r10, label %L11, label %L19
L11:
  %r22 = phi i32 [ 1, %L7 ], [ 1, %L7 ]
  %r21 = phi i32 [ %r5, %L7 ], [ %r5, %L7 ]
  call void @printInt(i32 %r21)
  %r15 = add i32 %r22, %r21
  %r18 = sub i32 %r15, %r22
  br label %L7
L19:
  ret i32 0
}

