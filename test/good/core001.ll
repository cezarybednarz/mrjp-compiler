declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
declare i8* @malloc(i32) nounwind
@.str.4 = private unnamed_addr constant [1 x i8] c"\00", align 1
@.str.3 = private unnamed_addr constant [9 x i8] c"/* world\00", align 1
@.str.2 = private unnamed_addr constant [9 x i8] c"hello */\00", align 1
@.str.1 = private unnamed_addr constant [2 x i8] c"=\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = call i32 @fac(i32 10)
  call void @printInt(i32 %r2)
  %r3 = call i32 @rfac(i32 10)
  call void @printInt(i32 %r3)
  %r4 = call i32 @mfac(i32 10)
  call void @printInt(i32 %r4)
  %r5 = call i32 @ifac(i32 10)
  call void @printInt(i32 %r5)
  %r6 = alloca i8*
  store i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.0, i32 0, i32 0), i8** %r6
  %r7 = alloca i32
  store i32 10, i32* %r7
  %r8 = alloca i32
  store i32 1, i32* %r8
  br label %L9
L9:                              ; preds = [L1,L12]
  %r10 = load i32, i32* %r7
  %r11 = icmp sgt i32 %r10, 0
  br i1 %r11, label %L12, label %L18
L12:                              ; preds = [L9]
  %r13 = load i32, i32* %r8
  %r14 = load i32, i32* %r7
  %r15 = mul i32 %r13, %r14
  store i32 %r15, i32* %r8
  %r16 = load i32, i32* %r7
  %r17 = sub i32 %r16, 1
  store i32 %r17, i32* %r7
  br label %L9
L18:                              ; preds = [L9]
  %r19 = load i32, i32* %r8
  call void @printInt(i32 %r19)
  %r20 = call i8* @repStr(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i32 0, i32 0), i32 60)
  call void @printString(i8* %r20)
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @.str.2, i32 0, i32 0))
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @.str.3, i32 0, i32 0))
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

define i32 @rfac(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r3 = alloca i32
  store i32 %r0, i32* %r3
  %r4 = load i32, i32* %r3
  %r5 = icmp eq i32 %r4, 0
  br i1 %r5, label %L6, label %L8
L6:                              ; preds = [L2]
  ret i32 1
  br label %L15
L8:                              ; preds = [L2]
  %r9 = load i32, i32* %r3
  %r10 = load i32, i32* %r3
  %r11 = sub i32 %r10, 1
  %r12 = call i32 @rfac(i32 %r11)
  %r13 = mul i32 %r9, %r12
  ret i32 %r13
  br label %L15
L15:                              ; preds = [L8,L6]
  ret i32 0
}

define i32 @mfac(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r3 = alloca i32
  store i32 %r0, i32* %r3
  %r4 = load i32, i32* %r3
  %r5 = icmp eq i32 %r4, 0
  br i1 %r5, label %L6, label %L8
L6:                              ; preds = [L2]
  ret i32 1
  br label %L15
L8:                              ; preds = [L2]
  %r9 = load i32, i32* %r3
  %r10 = load i32, i32* %r3
  %r11 = sub i32 %r10, 1
  %r12 = call i32 @nfac(i32 %r11)
  %r13 = mul i32 %r9, %r12
  ret i32 %r13
  br label %L15
L15:                              ; preds = [L8,L6]
  ret i32 0
}

define i32 @nfac(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r3 = alloca i32
  store i32 %r0, i32* %r3
  %r4 = load i32, i32* %r3
  %r5 = icmp ne i32 %r4, 0
  br i1 %r5, label %L6, label %L13
L6:                              ; preds = [L2]
  %r7 = load i32, i32* %r3
  %r8 = sub i32 %r7, 1
  %r9 = call i32 @mfac(i32 %r8)
  %r10 = load i32, i32* %r3
  %r11 = mul i32 %r9, %r10
  ret i32 %r11
  br label %L15
L13:                              ; preds = [L2]
  ret i32 1
  br label %L15
L15:                              ; preds = [L13,L6]
  ret i32 0
}

define i32 @ifac(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r3 = alloca i32
  store i32 %r0, i32* %r3
  %r4 = load i32, i32* %r3
  %r5 = call i32 @ifac2f(i32 1, i32 %r4)
  ret i32 %r5
}

define i32 @ifac2f(i32 %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r4 = alloca i32
  store i32 %r0, i32* %r4
  %r5 = alloca i32
  store i32 %r1, i32* %r5
  %r6 = load i32, i32* %r4
  %r7 = load i32, i32* %r5
  %r8 = icmp eq i32 %r6, %r7
  br i1 %r8, label %L9, label %L12
L9:                              ; preds = [L3]
  %r10 = load i32, i32* %r4
  ret i32 %r10
  br label %L12
L12:                              ; preds = [L9,L3]
  %r13 = load i32, i32* %r4
  %r14 = load i32, i32* %r5
  %r15 = icmp sgt i32 %r13, %r14
  br i1 %r15, label %L16, label %L18
L16:                              ; preds = [L12]
  ret i32 1
  br label %L18
L18:                              ; preds = [L16,L12]
  %r19 = alloca i32
  store i32 0, i32* %r19
  %r20 = load i32, i32* %r4
  %r21 = load i32, i32* %r5
  %r22 = add i32 %r20, %r21
  %r23 = sdiv i32 %r22, 2
  store i32 %r23, i32* %r19
  %r24 = load i32, i32* %r4
  %r25 = load i32, i32* %r19
  %r26 = call i32 @ifac2f(i32 %r24, i32 %r25)
  %r27 = load i32, i32* %r19
  %r28 = add i32 %r27, 1
  %r29 = load i32, i32* %r5
  %r30 = call i32 @ifac2f(i32 %r28, i32 %r29)
  %r31 = mul i32 %r26, %r30
  ret i32 %r31
}

define i8* @repStr(i8* %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r4 = alloca i8*
  store i8* %r0, i8** %r4
  %r5 = alloca i32
  store i32 %r1, i32* %r5
  %r6 = alloca i8*
  store i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.4, i32 0, i32 0), i8** %r6
  %r7 = alloca i32
  store i32 0, i32* %r7
  br label %L8
L8:                              ; preds = [L3,L12]
  %r9 = load i32, i32* %r7
  %r10 = load i32, i32* %r5
  %r11 = icmp slt i32 %r9, %r10
  br i1 %r11, label %L12, label %L18
L12:                              ; preds = [L8]
  %r13 = load i8*, i8** %r6
  %r14 = load i8*, i8** %r4
  %r15 = call i8* @__concatStrings__(i8* %r13, i8* %r14)
  store i8* %r15, i8** %r6
  %r16 = load i32, i32* %r7
  %r17 = add i32 %r16, 1
  store i32 %r17, i32* %r7
  br label %L8
L18:                              ; preds = [L8]
  %r19 = load i8*, i8** %r6
  ret i8* %r19
}

