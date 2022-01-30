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

define %ArrRetVal* @doubleArray(i32* %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r4 = mul i32 4, %r1
  %r5 = call i8* @malloc(i32 %r4)
  %r6 = bitcast i8* %r5 to i32*
  %r7 = alloca i32
  store i32 0, i32* %r7
  %r8 = alloca i32
  store i32 0, i32* %r8
  br label %L9
L9:                              ; preds = [L3,L12]
  %r10 = load i32, i32* %r8
  %r11 = icmp slt i32 %r10, %r1
  br i1 %r11, label %L12, label %L27
L12:                              ; preds = [L9]
  %r13 = load i32, i32* %r8
  %r14 = sext i32 %r13 to i64
  %r15 = getelementptr inbounds i32, i32* %r0, i64 %r14
  %r16 = load i32, i32* %r15
  %r17 = alloca i32
  store i32 %r16, i32* %r17
  %r18 = load i32, i32* %r17
  %r19 = mul i32 2, %r18
  %r20 = load i32, i32* %r7
  %r22 = sext i32 %r20 to i64
  %r21 = getelementptr inbounds i32, i32* %r6, i64 %r22
  store i32 %r19, i32* %r21
  %r23 = load i32, i32* %r7
  %r24 = add i32 %r23, 1
  store i32 %r24, i32* %r7
  %r25 = load i32, i32* %r8
  %r26 = add i32 %r25, 1
  store i32 %r26, i32* %r8
  br label %L9
L27:                              ; preds = [L9]
  %r28 = alloca %ArrRetVal
  %r29 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r28, i32 0, i32 0
  store i32 %r1, i32* %r29
  %r30 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r28, i32 0, i32 1
  store i32* %r6, i32** %r30
  ret %ArrRetVal* %r28
}

define void @shiftLeft(i32* %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r4 = getelementptr inbounds i32, i32* %r0, i64 0
  %r5 = load i32, i32* %r4
  %r6 = alloca i32
  store i32 %r5, i32* %r6
  %r7 = alloca i32
  store i32 0, i32* %r7
  br label %L8
L8:                              ; preds = [L3,L12]
  %r9 = load i32, i32* %r7
  %r10 = sub i32 %r1, 1
  %r11 = icmp slt i32 %r9, %r10
  br i1 %r11, label %L12, label %L23
L12:                              ; preds = [L8]
  %r13 = load i32, i32* %r7
  %r14 = add i32 %r13, 1
  %r15 = sext i32 %r14 to i64
  %r16 = getelementptr inbounds i32, i32* %r0, i64 %r15
  %r17 = load i32, i32* %r16
  %r18 = load i32, i32* %r7
  %r20 = sext i32 %r18 to i64
  %r19 = getelementptr inbounds i32, i32* %r0, i64 %r20
  store i32 %r17, i32* %r19
  %r21 = load i32, i32* %r7
  %r22 = add i32 %r21, 1
  store i32 %r22, i32* %r7
  br label %L8
L23:                              ; preds = [L8]
  %r24 = load i32, i32* %r6
  %r25 = sub i32 %r1, 1
  %r27 = sext i32 %r25 to i64
  %r26 = getelementptr inbounds i32, i32* %r0, i64 %r27
  store i32 %r24, i32* %r26
  ret void
}

define i32 @scalProd(i32* %r0, i32 %r1, i32* %r2, i32 %r3) {
  br label %L5
L5:                              ; preds = [L0]
  %r6 = alloca i32
  store i32 0, i32* %r6
  %r7 = alloca i32
  store i32 0, i32* %r7
  br label %L8
L8:                              ; preds = [L5,L11]
  %r9 = load i32, i32* %r7
  %r10 = icmp slt i32 %r9, %r1
  br i1 %r10, label %L11, label %L25
L11:                              ; preds = [L8]
  %r12 = load i32, i32* %r6
  %r13 = load i32, i32* %r7
  %r14 = sext i32 %r13 to i64
  %r15 = getelementptr inbounds i32, i32* %r0, i64 %r14
  %r16 = load i32, i32* %r15
  %r17 = load i32, i32* %r7
  %r18 = sext i32 %r17 to i64
  %r19 = getelementptr inbounds i32, i32* %r2, i64 %r18
  %r20 = load i32, i32* %r19
  %r21 = mul i32 %r16, %r20
  %r22 = add i32 %r12, %r21
  store i32 %r22, i32* %r6
  %r23 = load i32, i32* %r7
  %r24 = add i32 %r23, 1
  store i32 %r24, i32* %r7
  br label %L8
L25:                              ; preds = [L8]
  %r26 = load i32, i32* %r6
  ret i32 %r26
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = mul i32 4, 5
  %r3 = call i8* @malloc(i32 %r2)
  %r4 = bitcast i8* %r3 to i32*
  %r5 = alloca i32
  store i32 0, i32* %r5
  br label %L6
L6:                              ; preds = [L1,L9]
  %r7 = load i32, i32* %r5
  %r8 = icmp slt i32 %r7, 5
  br i1 %r8, label %L9, label %L16
L9:                              ; preds = [L6]
  %r10 = load i32, i32* %r5
  %r11 = load i32, i32* %r5
  %r13 = sext i32 %r11 to i64
  %r12 = getelementptr inbounds i32, i32* %r4, i64 %r13
  store i32 %r10, i32* %r12
  %r14 = load i32, i32* %r5
  %r15 = add i32 %r14, 1
  store i32 %r15, i32* %r5
  br label %L6
L16:                              ; preds = [L6]
  call void @shiftLeft(i32* %r4, i32 5)
  %r17 = call %ArrRetVal* @doubleArray(i32* %r4, i32 5)
  %r18 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r17, i32 0, i32 0
  %r19 = load i32, i32* %r18
  %r20 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r17, i32 0, i32 1
  %r21 = load i32*, i32** %r20
  %r22 = alloca i32
  store i32 0, i32* %r22
  br label %L23
L23:                              ; preds = [L16,L26]
  %r24 = load i32, i32* %r22
  %r25 = icmp slt i32 %r24, 5
  br i1 %r25, label %L26, label %L35
L26:                              ; preds = [L23]
  %r27 = load i32, i32* %r22
  %r28 = sext i32 %r27 to i64
  %r29 = getelementptr inbounds i32, i32* %r4, i64 %r28
  %r30 = load i32, i32* %r29
  %r31 = alloca i32
  store i32 %r30, i32* %r31
  %r32 = load i32, i32* %r31
  call void @printInt(i32 %r32)
  %r33 = load i32, i32* %r22
  %r34 = add i32 %r33, 1
  store i32 %r34, i32* %r22
  br label %L23
L35:                              ; preds = [L23]
  %r36 = alloca i32
  store i32 0, i32* %r36
  br label %L37
L37:                              ; preds = [L35,L40]
  %r38 = load i32, i32* %r36
  %r39 = icmp slt i32 %r38, %r19
  br i1 %r39, label %L40, label %L49
L40:                              ; preds = [L37]
  %r41 = load i32, i32* %r36
  %r42 = sext i32 %r41 to i64
  %r43 = getelementptr inbounds i32, i32* %r21, i64 %r42
  %r44 = load i32, i32* %r43
  %r45 = alloca i32
  store i32 %r44, i32* %r45
  %r46 = load i32, i32* %r45
  call void @printInt(i32 %r46)
  %r47 = load i32, i32* %r36
  %r48 = add i32 %r47, 1
  store i32 %r48, i32* %r36
  br label %L37
L49:                              ; preds = [L37]
  %r50 = call i32 @scalProd(i32* %r4, i32 5, i32* %r21, i32 %r19)
  call void @printInt(i32 %r50)
  ret i32 0
}

