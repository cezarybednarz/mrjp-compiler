declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.5 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.4 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.str.3 = private unnamed_addr constant [2 x i8] c"!\00", align 1
@.str.2 = private unnamed_addr constant [3 x i8] c"||\00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"&&\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0))
  %r2 = sub i32 0, 1
  %r3 = call i1 @test(i32 %r2)
  %r4 = xor i1 %r3, true
  br i1 %r4, label %L8, label %L5
L5:
  %r6 = call i1 @test(i32 0)
  %r7 = xor i1 %r6, true
  br label %L8
L8:
  %r9 = phi i1 [ %r7, %L5 ], [ true, %L1 ]
  %r10 = xor i1 %r9, true
  call void @printBool(i1 %r10)
  %r11 = sub i32 0, 2
  %r12 = call i1 @test(i32 %r11)
  %r13 = xor i1 %r12, true
  br i1 %r13, label %L17, label %L14
L14:
  %r15 = call i1 @test(i32 1)
  %r16 = xor i1 %r15, true
  br label %L17
L17:
  %r18 = phi i1 [ %r16, %L14 ], [ true, %L8 ]
  %r19 = xor i1 %r18, true
  call void @printBool(i1 %r19)
  %r20 = call i1 @test(i32 3)
  %r21 = xor i1 %r20, true
  br i1 %r21, label %L26, label %L22
L22:
  %r23 = sub i32 0, 5
  %r24 = call i1 @test(i32 %r23)
  %r25 = xor i1 %r24, true
  br label %L26
L26:
  %r27 = phi i1 [ %r25, %L22 ], [ true, %L17 ]
  %r28 = xor i1 %r27, true
  call void @printBool(i1 %r28)
  %r29 = call i1 @test(i32 234234)
  %r30 = xor i1 %r29, true
  br i1 %r30, label %L34, label %L31
L31:
  %r32 = call i1 @test(i32 21321)
  %r33 = xor i1 %r32, true
  br label %L34
L34:
  %r35 = phi i1 [ %r33, %L31 ], [ true, %L26 ]
  %r36 = xor i1 %r35, true
  call void @printBool(i1 %r36)
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.2, i32 0, i32 0))
  %r37 = sub i32 0, 1
  %r38 = call i1 @test(i32 %r37)
  br i1 %r38, label %L41, label %L39
L39:
  %r40 = call i1 @test(i32 0)
  br label %L41
L41:
  %r42 = phi i1 [ %r40, %L39 ], [ true, %L34 ]
  call void @printBool(i1 %r42)
  %r43 = sub i32 0, 2
  %r44 = call i1 @test(i32 %r43)
  br i1 %r44, label %L47, label %L45
L45:
  %r46 = call i1 @test(i32 1)
  br label %L47
L47:
  %r48 = phi i1 [ %r46, %L45 ], [ true, %L41 ]
  call void @printBool(i1 %r48)
  %r49 = call i1 @test(i32 3)
  br i1 %r49, label %L53, label %L50
L50:
  %r51 = sub i32 0, 5
  %r52 = call i1 @test(i32 %r51)
  br label %L53
L53:
  %r54 = phi i1 [ %r52, %L50 ], [ true, %L47 ]
  call void @printBool(i1 %r54)
  %r55 = call i1 @test(i32 234234)
  br i1 %r55, label %L58, label %L56
L56:
  %r57 = call i1 @test(i32 21321)
  br label %L58
L58:
  %r59 = phi i1 [ %r57, %L56 ], [ true, %L53 ]
  call void @printBool(i1 %r59)
  call void @printString(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.3, i32 0, i32 0))
  call void @printBool(i1 true)
  call void @printBool(i1 false)
  ret i32 0
}

define void @printBool(i1 %r0) {
  br label %L2
L2:
  %r5 = xor i1 %r0, true
  br i1 %r5, label %L6, label %L7
L6:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.4, i32 0, i32 0))
  br label %L8
L7:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.5, i32 0, i32 0))
  br label %L8
L8:
  ret void
}

define i1 @test(i32 %r0) {
  br label %L2
L2:
  call void @printInt(i32 %r0)
  %r6 = icmp sgt i32 %r0, 0
  ret i1 %r6
}

