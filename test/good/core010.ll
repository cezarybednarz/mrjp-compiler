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
  %r2 = call i32 @fac(i32 5)
  call void @printInt(i32 %r2)
  ret i32 0
}

define i32 @fac(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r3 = alloca i32
  store i32 %r0, i32* %r3
  %r4 = alloca i32
  store i32 0, i32* %r4
  %r5 = alloca i32
  store i32 0, i32* %r5
  store i32 1, i32* %r4
  %r6 = load i32, i32* %r3
  store i32 %r6, i32* %r5
  br label %L7
L7:                              ; preds = [L2,L10]
  %r8 = load i32, i32* %r5
  %r9 = icmp sgt i32 %r8, 0
  br i1 %r9, label %L10, label %L16
L10:                              ; preds = [L7]
  %r11 = load i32, i32* %r4
  %r12 = load i32, i32* %r5
  %r13 = mul i32 %r11, %r12
  store i32 %r13, i32* %r4
  %r14 = load i32, i32* %r5
  %r15 = sub i32 %r14, 1
  store i32 %r15, i32* %r5
  br label %L7
L16:                              ; preds = [L7]
  %r17 = load i32, i32* %r4
  ret i32 %r17
}

