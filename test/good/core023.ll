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
  store i32 1, i32* %r2
  %r3 = alloca i32
  store i32 2, i32* %r3
  %r4 = alloca i32
  store i32 1, i32* %r4
  %r5 = alloca i32
  store i32 2, i32* %r5
  %r6 = alloca i32
  store i32 1, i32* %r6
  %r7 = alloca i32
  store i32 2, i32* %r7
  %r8 = alloca i32
  store i32 1, i32* %r8
  %r9 = alloca i32
  store i32 2, i32* %r9
  %r10 = alloca i32
  store i32 1, i32* %r10
  %r11 = alloca i32
  store i32 2, i32* %r11
  %r12 = alloca i32
  store i32 1, i32* %r12
  %r13 = alloca i32
  store i32 2, i32* %r13
  %r14 = alloca i32
  store i32 1, i32* %r14
  %r15 = alloca i32
  store i32 2, i32* %r15
  %r16 = load i32, i32* %r2
  %r17 = load i32, i32* %r3
  %r18 = load i32, i32* %r4
  %r19 = load i32, i32* %r5
  %r20 = load i32, i32* %r6
  %r21 = load i32, i32* %r7
  %r22 = load i32, i32* %r8
  %r23 = load i32, i32* %r9
  %r24 = load i32, i32* %r10
  %r25 = load i32, i32* %r11
  %r26 = load i32, i32* %r12
  %r27 = load i32, i32* %r13
  %r28 = load i32, i32* %r14
  %r29 = load i32, i32* %r15
  %r30 = call i32 @foo(i32 %r16, i32 %r17, i32 %r18, i32 %r19, i32 %r20, i32 %r21, i32 %r22, i32 %r23, i32 %r24, i32 %r25, i32 %r26, i32 %r27, i32 %r28, i32 %r29)
  ret i32 %r30
}

define i32 @foo(i32 %r0, i32 %r1, i32 %r2, i32 %r3, i32 %r4, i32 %r5, i32 %r6, i32 %r7, i32 %r8, i32 %r9, i32 %r10, i32 %r11, i32 %r12, i32 %r13) {
  br label %L15
L15:                              ; preds = [L0]
  %r16 = alloca i32
  store i32 %r0, i32* %r16
  %r17 = alloca i32
  store i32 %r1, i32* %r17
  %r18 = alloca i32
  store i32 %r2, i32* %r18
  %r19 = alloca i32
  store i32 %r3, i32* %r19
  %r20 = alloca i32
  store i32 %r4, i32* %r20
  %r21 = alloca i32
  store i32 %r5, i32* %r21
  %r22 = alloca i32
  store i32 %r6, i32* %r22
  %r23 = alloca i32
  store i32 %r7, i32* %r23
  %r24 = alloca i32
  store i32 %r8, i32* %r24
  %r25 = alloca i32
  store i32 %r9, i32* %r25
  %r26 = alloca i32
  store i32 %r10, i32* %r26
  %r27 = alloca i32
  store i32 %r11, i32* %r27
  %r28 = alloca i32
  store i32 %r12, i32* %r28
  %r29 = alloca i32
  store i32 %r13, i32* %r29
  %r30 = load i32, i32* %r16
  %r31 = mul i32 2, %r30
  %r32 = load i32, i32* %r17
  %r33 = sdiv i32 %r32, 2
  %r34 = add i32 %r31, %r33
  %r35 = load i32, i32* %r18
  %r36 = add i32 %r34, %r35
  %r37 = load i32, i32* %r19
  %r38 = add i32 %r36, %r37
  %r39 = load i32, i32* %r20
  %r40 = add i32 %r38, %r39
  %r41 = load i32, i32* %r21
  %r42 = add i32 %r40, %r41
  %r43 = load i32, i32* %r22
  %r44 = add i32 %r42, %r43
  %r45 = load i32, i32* %r23
  %r46 = add i32 %r44, %r45
  %r47 = load i32, i32* %r24
  %r48 = add i32 %r46, %r47
  %r49 = load i32, i32* %r25
  %r50 = sdiv i32 %r49, 2
  %r51 = add i32 %r48, %r50
  %r52 = load i32, i32* %r26
  %r53 = add i32 %r51, %r52
  %r54 = load i32, i32* %r27
  %r55 = add i32 %r53, %r54
  %r56 = load i32, i32* %r28
  %r57 = add i32 %r55, %r56
  %r58 = load i32, i32* %r29
  %r59 = add i32 %r57, %r58
  %r60 = srem i32 %r59, 10
  %r61 = alloca i32
  store i32 %r60, i32* %r61
  %r62 = load i32, i32* %r61
  call void @printInt(i32 %r62)
  %r63 = load i32, i32* %r61
  ret i32 %r63
}

