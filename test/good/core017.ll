declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
declare i8* @malloc(i32) nounwind
@.str.3 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.str.2 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"apa\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = alloca i32
  store i32 4, i32* %r2
  %r3 = load i32, i32* %r2
  %r4 = icmp sle i32 3, %r3
  %r5 = xor i1 %r4, true
  br i1 %r5, label %L15, label %L6
L6:                              ; preds = [L1]
  %r7 = icmp ne i32 4, 2
  %r8 = xor i1 %r7, true
  br i1 %r8, label %L11, label %L9
L9:                              ; preds = [L6]
  %r10 = xor i1 true, true
  br label %L11
L11:                              ; preds = [L6]
  %r12 = phi i1 [ %r10, %L9 ], [ true, %L6 ]
  %r13 = xor i1 %r12, true
  %r14 = xor i1 %r13, true
  br label %L15
L15:                              ; preds = [L1]
  %r16 = phi i1 [ %r14, %L11 ], [ true, %L1 ]
  %r17 = xor i1 %r16, true
  br i1 %r17, label %L18, label %L19
L18:                              ; preds = [L15]
  call void @printBool(i1 true)
  br label %L20
L19:                              ; preds = [L15]
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0))
  br label %L20
L20:                              ; preds = [L19,L18]
  %r21 = icmp eq i1 true, true
  br i1 %r21, label %L24, label %L22
L22:                              ; preds = [L20]
  %r23 = call i1 @dontCallMe(i32 1)
  br label %L24
L24:                              ; preds = [L20]
  %r25 = phi i1 [ %r23, %L22 ], [ true, %L20 ]
  call void @printBool(i1 %r25)
  %r26 = sub i32 0, 5
  %r27 = icmp slt i32 4, %r26
  %r28 = xor i1 %r27, true
  br i1 %r28, label %L32, label %L29
L29:                              ; preds = [L24]
  %r30 = call i1 @dontCallMe(i32 2)
  %r31 = xor i1 %r30, true
  br label %L32
L32:                              ; preds = [L24]
  %r33 = phi i1 [ %r31, %L29 ], [ true, %L24 ]
  %r34 = xor i1 %r33, true
  call void @printBool(i1 %r34)
  %r35 = load i32, i32* %r2
  %r36 = icmp eq i32 4, %r35
  %r37 = xor i1 %r36, true
  br i1 %r37, label %L48, label %L38
L38:                              ; preds = [L32]
  %r39 = xor i1 false, true
  %r40 = icmp eq i1 true, %r39
  %r41 = xor i1 %r40, true
  br i1 %r41, label %L44, label %L42
L42:                              ; preds = [L38]
  %r43 = xor i1 true, true
  br label %L44
L44:                              ; preds = [L38]
  %r45 = phi i1 [ %r43, %L42 ], [ true, %L38 ]
  %r46 = xor i1 %r45, true
  %r47 = xor i1 %r46, true
  br label %L48
L48:                              ; preds = [L32]
  %r49 = phi i1 [ %r47, %L44 ], [ true, %L32 ]
  %r50 = xor i1 %r49, true
  call void @printBool(i1 %r50)
  %r51 = call i1 @implies(i1 false, i1 false)
  call void @printBool(i1 %r51)
  %r52 = call i1 @implies(i1 false, i1 true)
  call void @printBool(i1 %r52)
  %r53 = call i1 @implies(i1 true, i1 false)
  call void @printBool(i1 %r53)
  %r54 = call i1 @implies(i1 true, i1 true)
  call void @printBool(i1 %r54)
  ret i32 0
}

define i1 @dontCallMe(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r3 = alloca i32
  store i32 %r0, i32* %r3
  %r4 = load i32, i32* %r3
  call void @printInt(i32 %r4)
  ret i1 true
}

define void @printBool(i1 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r3 = alloca i1
  store i1 %r0, i1* %r3
  %r4 = load i1, i1* %r3
  br i1 %r4, label %L5, label %L6
L5:                              ; preds = [L2]
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i32 0, i32 0))
  br label %L7
L6:                              ; preds = [L2]
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.3, i32 0, i32 0))
  br label %L7
L7:                              ; preds = [L6,L5]
  ret void
}

define i1 @implies(i1 %r0, i1 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r4 = alloca i1
  store i1 %r0, i1* %r4
  %r5 = alloca i1
  store i1 %r1, i1* %r5
  %r6 = load i1, i1* %r4
  %r7 = xor i1 %r6, true
  br i1 %r7, label %L12, label %L8
L8:                              ; preds = [L3]
  %r9 = load i1, i1* %r4
  %r10 = load i1, i1* %r5
  %r11 = icmp eq i1 %r9, %r10
  br label %L12
L12:                              ; preds = [L3]
  %r13 = phi i1 [ %r11, %L8 ], [ true, %L3 ]
  ret i1 %r13
}

