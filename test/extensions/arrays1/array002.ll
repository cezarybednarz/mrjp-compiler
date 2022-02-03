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
  %r5 = mul i32 4, %r1
   %r6 = call i8* @malloc(i32 %r5)
   %r7 = bitcast i8* %r6 to i32*
  br label %L10
L10:                              ; preds = [L3,L13]
  %r35 = phi i32 [ %r25, %L13 ], [ 0, %L3 ]
  %r34 = phi i32 [ %r34, %L13 ], [ %r1, %L3 ]
  %r33 = phi i32 [ %r27, %L13 ], [ 0, %L3 ]
  %r12 = icmp slt i32 %r33, %r34
  br i1 %r12, label %L13, label %L28
L13:                              ; preds = [L10]
   %r15 = sext i32 %r33 to i64
   %r16 = getelementptr inbounds i32, i32* %r0, i64 %r15
   %r17 = load i32, i32* %r16
  %r20 = mul i32 2, %r17
   %r23 = sext i32 %r35 to i64
   %r22 = getelementptr inbounds i32, i32* %r7, i64 %r23
   store i32 %r20, i32* %r22
  %r25 = add i32 %r35, 1
  %r27 = add i32 %r33, 1
  br label %L10
L28:                              ; preds = [L10]
   %r29 = alloca %ArrRetVal
   %r30 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r29, i32 0, i32 0
   store i32 %r34, i32* %r30
   %r31 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r29, i32 0, i32 1
   store i32* %r7, i32** %r31
   ret %ArrRetVal* %r29
}

define void @shiftLeft(i32* %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
   %r5 = getelementptr inbounds i32, i32* %r0, i64 0
   %r6 = load i32, i32* %r5
  br label %L9
L9:                              ; preds = [L3,L13]
  %r32 = phi i32 [ %r32, %L13 ], [ %r6, %L3 ]
  %r31 = phi i32 [ %r31, %L13 ], [ %r1, %L3 ]
  %r30 = phi i32 [ %r15, %L13 ], [ 0, %L3 ]
  %r11 = sub i32 %r31, 1
  %r12 = icmp slt i32 %r30, %r11
  br i1 %r12, label %L13, label %L24
L13:                              ; preds = [L9]
  %r15 = add i32 %r30, 1
   %r16 = sext i32 %r15 to i64
   %r17 = getelementptr inbounds i32, i32* %r0, i64 %r16
   %r18 = load i32, i32* %r17
   %r21 = sext i32 %r30 to i64
   %r20 = getelementptr inbounds i32, i32* %r0, i64 %r21
   store i32 %r18, i32* %r20
  br label %L9
L24:                              ; preds = [L9]
   %r28 = sext i32 %r11 to i64
   %r27 = getelementptr inbounds i32, i32* %r0, i64 %r28
   store i32 %r32, i32* %r27
  ret void
}

define i32 @scalProd(i32* %r0, i32 %r1, i32* %r2, i32 %r3) {
  br label %L5
L5:                              ; preds = [L0]
  br label %L10
L10:                              ; preds = [L5,L13]
  %r32 = phi i32 [ %r24, %L13 ], [ 0, %L5 ]
  %r31 = phi i32 [ %r31, %L13 ], [ %r1, %L5 ]
  %r30 = phi i32 [ %r26, %L13 ], [ 0, %L5 ]
  %r12 = icmp slt i32 %r30, %r31
  br i1 %r12, label %L13, label %L27
L13:                              ; preds = [L10]
   %r16 = sext i32 %r30 to i64
   %r17 = getelementptr inbounds i32, i32* %r0, i64 %r16
   %r18 = load i32, i32* %r17
   %r20 = sext i32 %r30 to i64
   %r21 = getelementptr inbounds i32, i32* %r2, i64 %r20
   %r22 = load i32, i32* %r21
  %r23 = mul i32 %r18, %r22
  %r24 = add i32 %r32, %r23
  %r26 = add i32 %r30, 1
  br label %L10
L27:                              ; preds = [L10]
  ret i32 %r32
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = mul i32 4, 5
   %r3 = call i8* @malloc(i32 %r2)
   %r4 = bitcast i8* %r3 to i32*
  br label %L6
L6:                              ; preds = [L1,L9]
  %r52 = phi i32 [ %r15, %L9 ], [ 0, %L1 ]
  %r8 = icmp slt i32 %r52, 5
  br i1 %r8, label %L9, label %L16
L9:                              ; preds = [L6]
   %r13 = sext i32 %r52 to i64
   %r12 = getelementptr inbounds i32, i32* %r4, i64 %r13
   store i32 %r52, i32* %r12
  %r15 = add i32 %r52, 1
  br label %L6
L16:                              ; preds = [L6]
  call void @shiftLeft(i32* %r4, i32 5)
   %r17 = call %ArrRetVal* @doubleArray(i32* %r4, i32 5)
   %r18 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r17, i32 0, i32 0
   %r19 = load i32, i32* %r18
   %r20 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r17, i32 0, i32 1
   %r21 = load i32*, i32** %r20
  br label %L23
L23:                              ; preds = [L16,L26]
  %r56 = phi i32 [ %r56, %L26 ], [ %r19, %L16 ]
  %r53 = phi i32 [ %r34, %L26 ], [ 0, %L16 ]
  %r25 = icmp slt i32 %r53, 5
  br i1 %r25, label %L26, label %L35
L26:                              ; preds = [L23]
   %r28 = sext i32 %r53 to i64
   %r29 = getelementptr inbounds i32, i32* %r4, i64 %r28
   %r30 = load i32, i32* %r29
  call void @printInt(i32 %r30)
  %r34 = add i32 %r53, 1
  br label %L23
L35:                              ; preds = [L23]
  br label %L37
L37:                              ; preds = [L35,L40]
  %r55 = phi i32 [ %r55, %L40 ], [ %r56, %L35 ]
  %r54 = phi i32 [ %r48, %L40 ], [ 0, %L35 ]
  %r39 = icmp slt i32 %r54, %r55
  br i1 %r39, label %L40, label %L49
L40:                              ; preds = [L37]
   %r42 = sext i32 %r54 to i64
   %r43 = getelementptr inbounds i32, i32* %r21, i64 %r42
   %r44 = load i32, i32* %r43
  call void @printInt(i32 %r44)
  %r48 = add i32 %r54, 1
  br label %L37
L49:                              ; preds = [L37]
  %r50 = call i32 @scalProd(i32* %r4, i32 5, i32* %r21, i32 %r55)
  call void @printInt(i32 %r50)
  ret i32 0
}

