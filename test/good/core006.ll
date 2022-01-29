declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
declare i8* @malloc(i32) nounwind
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = alloca i32
  store i32 0, i32* %r2
  %r3 = alloca i32
  store i32 0, i32* %r3
  store i32 45, i32* %r2
  %r4 = sub i32 0, 36
  store i32 %r4, i32* %r3
  %r5 = load i32, i32* %r2
  call void @printInt(i32 %r5)
  %r6 = load i32, i32* %r3
  call void @printInt(i32 %r6)
  ret i32 0
}

