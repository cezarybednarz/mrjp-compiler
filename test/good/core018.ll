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
L1:                              ; preds = [L0]
  %r2 = call i32 @readInt()
  %r4 = call i8* @readString()
  %r6 = call i8* @readString()
  %r9 = sub i32 %r2, 5
  call void @printInt(i32 %r9)
  %r12 = call i8* @__concatStrings__(i8* %r4, i8* %r6)
  call void @printString(i8* %r12)
  ret i32 0
}

