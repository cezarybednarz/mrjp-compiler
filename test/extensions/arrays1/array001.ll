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

define %ArrRetVal* @same(i32* %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
   %r5 = alloca %ArrRetVal
   %r6 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r5, i32 0, i32 0
   store i32 %r1, i32* %r6
   %r7 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r5, i32 0, i32 1
   store i32* %r0, i32** %r7
   ret %ArrRetVal* %r5
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = mul i32 4, 10
   %r3 = call i8* @malloc(i32 %r2)
   %r4 = bitcast i8* %r3 to i32*
   %r5 = call %ArrRetVal* @same(i32* %r4, i32 10)
   %r6 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r5, i32 0, i32 0
   %r7 = load i32, i32* %r6
   %r8 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r5, i32 0, i32 1
   %r9 = load i32*, i32** %r8
  br label %L11
L11:                              ; preds = [L1,L14]
  %r40 = phi i32 [ %r40, %L14 ], [ %r7, %L1 ]
  %r39 = phi i32 [ %r20, %L14 ], [ 0, %L1 ]
  %r13 = icmp slt i32 %r39, %r40
  br i1 %r13, label %L14, label %L21
L14:                              ; preds = [L11]
   %r18 = sext i32 %r39 to i64
   %r17 = getelementptr inbounds i32, i32* %r9, i64 %r18
   store i32 %r39, i32* %r17
  %r20 = add i32 %r39, 1
  br label %L11
L21:                              ; preds = [L11]
  br label %L23
L23:                              ; preds = [L21,L26]
  %r42 = phi i32 [ %r42, %L26 ], [ %r40, %L21 ]
  %r41 = phi i32 [ %r34, %L26 ], [ 0, %L21 ]
  %r25 = icmp slt i32 %r41, %r42
  br i1 %r25, label %L26, label %L35
L26:                              ; preds = [L23]
   %r28 = sext i32 %r41 to i64
   %r29 = getelementptr inbounds i32, i32* %r9, i64 %r28
   %r30 = load i32, i32* %r29
  call void @printInt(i32 %r30)
  %r34 = add i32 %r41, 1
  br label %L23
L35:                              ; preds = [L23]
  call void @printInt(i32 45)
  ret i32 0
}

