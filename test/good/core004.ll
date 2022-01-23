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
  %r2 = icmp eq i1 true, true
  br i1 %r2, label %L3, label %L4
L3:
  call void @printInt(i32 42)
  br label %L4
L4:
  ret i32 0
}

