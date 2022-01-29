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
  store i32 56, i32* %r3
  %r4 = load i32, i32* %r3
  %r5 = add i32 %r4, 45
  %r6 = icmp sle i32 %r5, 2
  br i1 %r6, label %L7, label %L8
L7:                              ; preds = [L1]
  store i32 1, i32* %r2
  br label %L9
L8:                              ; preds = [L1]
  store i32 2, i32* %r2
  br label %L9
L9:                              ; preds = [L8,L7]
  %r10 = load i32, i32* %r2
  call void @printInt(i32 %r10)
  ret i32 0
}

