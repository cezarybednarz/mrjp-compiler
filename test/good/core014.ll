%ArrRetVal = type {
  i32,           ; length of array
  i32*,
  i1*,
  i8**
}
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
  %r4 = alloca i32
  store i32 0, i32* %r4
  store i32 1, i32* %r2
  %r5 = load i32, i32* %r2
  store i32 %r5, i32* %r3
  store i32 5000000, i32* %r4
  %r6 = load i32, i32* %r2
  call void @printInt(i32 %r6)
  br label %L7
L7:                              ; preds = [L1,L11]
  %r8 = load i32, i32* %r3
  %r9 = load i32, i32* %r4
  %r10 = icmp slt i32 %r8, %r9
  br i1 %r10, label %L11, label %L19
L11:                              ; preds = [L7]
  %r12 = load i32, i32* %r3
  call void @printInt(i32 %r12)
  %r13 = load i32, i32* %r2
  %r14 = load i32, i32* %r3
  %r15 = add i32 %r13, %r14
  store i32 %r15, i32* %r3
  %r16 = load i32, i32* %r3
  %r17 = load i32, i32* %r2
  %r18 = sub i32 %r16, %r17
  store i32 %r18, i32* %r2
  br label %L7
L19:                              ; preds = [L7]
  ret i32 0
}

