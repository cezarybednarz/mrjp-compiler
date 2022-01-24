declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
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
  br label %L9
L9:                              ; preds = [L1,L12]
  %r23 = phi i32 [ %r15, %L12 ], [ 1, %L1 ]
  %r22 = phi i32 [ %r17, %L12 ], [ 10, %L1 ]
  %r11 = icmp sgt i32 %r22, 0
  br i1 %r11, label %L12, label %L18
L12:                              ; preds = [L9]
  %r15 = mul i32 %r23, %r22
  %r17 = sub i32 %r22, 1
  br label %L9
L18:                              ; preds = [L9]
  call void @printInt(i32 %r23)
  %r20 = call i8* @repStr(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i32 0, i32 0), i32 60)
  call void @printString(i8* %r20)
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @.str.2, i32 0, i32 0))
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @.str.3, i32 0, i32 0))
  ret i32 0
}

define i32 @fac(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  br label %L7
L7:                              ; preds = [L2,L10]
  %r20 = phi i32 [ %r13, %L10 ], [ 1, %L2 ]
  %r19 = phi i32 [ %r15, %L10 ], [ %r0, %L2 ]
  %r9 = icmp sgt i32 %r19, 0
  br i1 %r9, label %L10, label %L16
L10:                              ; preds = [L7]
  %r13 = mul i32 %r20, %r19
  %r15 = sub i32 %r19, 1
  br label %L7
L16:                              ; preds = [L7]
  ret i32 %r20
}

define i32 @rfac(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r5 = icmp eq i32 %r0, 0
  br i1 %r5, label %L6, label %L8
L6:                              ; preds = [L2]
  ret i32 1
  br label %L15
L8:                              ; preds = [L2]
  %r11 = sub i32 %r0, 1
  %r12 = call i32 @rfac(i32 %r11)
  %r13 = mul i32 %r0, %r12
  ret i32 %r13
  br label %L15
L15:                              ; preds = [L8,L6]
  ret i32 0
}

define i32 @mfac(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r5 = icmp eq i32 %r0, 0
  br i1 %r5, label %L6, label %L8
L6:                              ; preds = [L2]
  ret i32 1
  br label %L15
L8:                              ; preds = [L2]
  %r11 = sub i32 %r0, 1
  %r12 = call i32 @nfac(i32 %r11)
  %r13 = mul i32 %r0, %r12
  ret i32 %r13
  br label %L15
L15:                              ; preds = [L8,L6]
  ret i32 0
}

define i32 @nfac(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r5 = icmp ne i32 %r0, 0
  br i1 %r5, label %L6, label %L13
L6:                              ; preds = [L2]
  %r8 = sub i32 %r0, 1
  %r9 = call i32 @mfac(i32 %r8)
  %r11 = mul i32 %r9, %r0
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
  %r5 = call i32 @ifac2f(i32 1, i32 %r0)
  ret i32 %r5
}

define i32 @ifac2f(i32 %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r8 = icmp eq i32 %r0, %r1
  br i1 %r8, label %L9, label %L12
L9:                              ; preds = [L3]
  ret i32 %r0
  br label %L12
L12:                              ; preds = [L9,L3]
  %r34 = phi i32 [ %r1, %L3 ], [ %r1, %L9 ]
  %r33 = phi i32 [ %r0, %L3 ], [ %r0, %L9 ]
  %r15 = icmp sgt i32 %r33, %r34
  br i1 %r15, label %L16, label %L18
L16:                              ; preds = [L12]
  ret i32 1
  br label %L18
L18:                              ; preds = [L16,L12]
  %r36 = phi i32 [ %r34, %L12 ], [ %r34, %L16 ]
  %r35 = phi i32 [ %r33, %L12 ], [ %r33, %L16 ]
  %r22 = add i32 %r35, %r36
  %r23 = sdiv i32 %r22, 2
  %r26 = call i32 @ifac2f(i32 %r35, i32 %r23)
  %r28 = add i32 %r23, 1
  %r30 = call i32 @ifac2f(i32 %r28, i32 %r36)
  %r31 = mul i32 %r26, %r30
  ret i32 %r31
}

define i8* @repStr(i8* %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  br label %L8
L8:                              ; preds = [L3,L12]
  %r24 = phi i8* [ %r24, %L12 ], [ %r0, %L3 ]
  %r23 = phi i8* [ %r15, %L12 ], [ getelementptr inbounds ([1 x i8], [1 x i8]* @.str.4, i32 0, i32 0), %L3 ]
  %r22 = phi i32 [ %r22, %L12 ], [ %r1, %L3 ]
  %r21 = phi i32 [ %r17, %L12 ], [ 0, %L3 ]
  %r11 = icmp slt i32 %r21, %r22
  br i1 %r11, label %L12, label %L18
L12:                              ; preds = [L8]
  %r15 = call i8* @__concatStrings__(i8* %r23, i8* %r24)
  %r17 = add i32 %r21, 1
  br label %L8
L18:                              ; preds = [L8]
  ret i8* %r23
}

