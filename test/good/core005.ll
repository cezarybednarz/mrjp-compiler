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
  %r5 = add i32 56, 45
  %r6 = icmp sle i32 %r5, 2
  br i1 %r6, label %L7, label %L8
L7:
  br label %L9
L8:
  br label %L9
L9:
  call void @printInt(i32 %r10)
  ret i32 0
}

