declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
declare i8* @malloc(i32) nounwind
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define void @printLength(i32* %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r4 = alloca i32
  store i32 0, i32* %r4
  br label %L5
L5:                              ; preds = [L3,L8]
  %r6 = load i32, i32* %r4
  %r7 = icmp slt i32 %r6, %r1
  br i1 %r7, label %L8, label %L17
L8:                              ; preds = [L5]
  %r9 = load i32, i32* %r4
  %r10 = sext i32 %r9 to i64
  %r11 = getelementptr inbounds i32, i32* %r0, i64 %r10
  %r12 = load i32, i32* %r11
  %r13 = alloca i32
  store i32 %r12, i32* %r13
  %r14 = load i32, i32* %r13
  call void @printInt(i32 %r14)
  %r15 = load i32, i32* %r4
  %r16 = add i32 %r15, 1
  store i32 %r16, i32* %r4
  br label %L5
L17:                              ; preds = [L5]
  ret void
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = mul i32 4, 2
  %r3 = call i8* @malloc(i32 %r2)
  %r4 = bitcast i8* %r3 to i32*
  %r5 = getelementptr inbounds i32, i32* %r4, i64 0
  store i32 1, i32* %r5
  %r6 = getelementptr inbounds i32, i32* %r4, i64 1
  store i32 69, i32* %r6
  call void @printLength(i32* %r4, i32 2)
  ret i32 0
}

