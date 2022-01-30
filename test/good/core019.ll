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
@.str.1 = private unnamed_addr constant [4 x i8] c"foo\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = alloca i32
  store i32 78, i32* %r2
  %r3 = alloca i32
  store i32 1, i32* %r3
  %r4 = load i32, i32* %r3
  call void @printInt(i32 %r4)
  %r5 = load i32, i32* %r2
  call void @printInt(i32 %r5)
  br label %L6
L6:                              ; preds = [L1,L9]
  %r7 = load i32, i32* %r2
  %r8 = icmp sgt i32 %r7, 76
  br i1 %r8, label %L9, label %L17
L9:                              ; preds = [L6]
  %r10 = load i32, i32* %r2
  %r11 = sub i32 %r10, 1
  store i32 %r11, i32* %r2
  %r12 = load i32, i32* %r2
  call void @printInt(i32 %r12)
  %r13 = load i32, i32* %r2
  %r14 = add i32 %r13, 7
  %r15 = alloca i32
  store i32 %r14, i32* %r15
  %r16 = load i32, i32* %r15
  call void @printInt(i32 %r16)
  br label %L6
L17:                              ; preds = [L6]
  %r18 = load i32, i32* %r2
  call void @printInt(i32 %r18)
  %r19 = load i32, i32* %r2
  %r20 = icmp sgt i32 %r19, 4
  br i1 %r20, label %L21, label %L24
L21:                              ; preds = [L17]
  %r22 = alloca i32
  store i32 4, i32* %r22
  %r23 = load i32, i32* %r22
  call void @printInt(i32 %r23)
  br label %L25
L24:                              ; preds = [L17]
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0))
  br label %L25
L25:                              ; preds = [L24,L21]
  %r26 = load i32, i32* %r2
  call void @printInt(i32 %r26)
  ret i32 0
}

