declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
declare i8* @malloc(i32) nounwind
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @d() {
  br label %L1
L1:                              ; preds = [L0]
  ret i32 0
}

define i32 @s(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r3 = alloca i32
  store i32 %r0, i32* %r3
  %r4 = load i32, i32* %r3
  %r5 = add i32 %r4, 1
  ret i32 %r5
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = call i32 @d()
  %r3 = call i32 @s(i32 %r2)
  %r4 = call i32 @s(i32 %r3)
  %r5 = call i32 @s(i32 %r4)
  %r6 = call i32 @s(i32 %r5)
  %r7 = call i32 @s(i32 %r6)
  %r8 = call i32 @s(i32 %r7)
  %r9 = call i32 @s(i32 %r8)
  %r10 = call i32 @s(i32 %r9)
  %r11 = call i32 @s(i32 %r10)
  %r12 = call i32 @s(i32 %r11)
  %r13 = call i32 @s(i32 %r12)
  %r14 = call i32 @s(i32 %r13)
  %r15 = call i32 @s(i32 %r14)
  %r16 = call i32 @s(i32 %r15)
  %r17 = call i32 @s(i32 %r16)
  %r18 = call i32 @s(i32 %r17)
  %r19 = call i32 @s(i32 %r18)
  %r20 = call i32 @s(i32 %r19)
  %r21 = call i32 @s(i32 %r20)
  %r22 = call i32 @s(i32 %r21)
  %r23 = call i32 @s(i32 %r22)
  %r24 = call i32 @s(i32 %r23)
  %r25 = call i32 @s(i32 %r24)
  %r26 = call i32 @s(i32 %r25)
  %r27 = call i32 @s(i32 %r26)
  %r28 = call i32 @s(i32 %r27)
  %r29 = call i32 @s(i32 %r28)
  %r30 = call i32 @s(i32 %r29)
  %r31 = call i32 @s(i32 %r30)
  %r32 = call i32 @s(i32 %r31)
  %r33 = call i32 @s(i32 %r32)
  %r34 = call i32 @s(i32 %r33)
  %r35 = call i32 @s(i32 %r34)
  %r36 = call i32 @s(i32 %r35)
  %r37 = call i32 @s(i32 %r36)
  %r38 = call i32 @s(i32 %r37)
  %r39 = call i32 @s(i32 %r38)
  %r40 = call i32 @s(i32 %r39)
  %r41 = call i32 @s(i32 %r40)
  %r42 = call i32 @s(i32 %r41)
  %r43 = call i32 @s(i32 %r42)
  %r44 = call i32 @s(i32 %r43)
  %r45 = call i32 @s(i32 %r44)
  %r46 = call i32 @s(i32 %r45)
  %r47 = call i32 @s(i32 %r46)
  %r48 = call i32 @s(i32 %r47)
  %r49 = call i32 @s(i32 %r48)
  %r50 = call i32 @s(i32 %r49)
  %r51 = call i32 @s(i32 %r50)
  %r52 = call i32 @s(i32 %r51)
  %r53 = call i32 @s(i32 %r52)
  %r54 = call i32 @s(i32 %r53)
  %r55 = call i32 @s(i32 %r54)
  %r56 = call i32 @s(i32 %r55)
  %r57 = call i32 @s(i32 %r56)
  %r58 = call i32 @s(i32 %r57)
  %r59 = call i32 @s(i32 %r58)
  %r60 = call i32 @s(i32 %r59)
  %r61 = call i32 @s(i32 %r60)
  %r62 = call i32 @s(i32 %r61)
  %r63 = call i32 @s(i32 %r62)
  %r64 = call i32 @s(i32 %r63)
  %r65 = call i32 @s(i32 %r64)
  %r66 = call i32 @s(i32 %r65)
  %r67 = call i32 @s(i32 %r66)
  %r68 = call i32 @s(i32 %r67)
  %r69 = call i32 @s(i32 %r68)
  %r70 = call i32 @s(i32 %r69)
  %r71 = call i32 @s(i32 %r70)
  %r72 = call i32 @s(i32 %r71)
  %r73 = call i32 @s(i32 %r72)
  %r74 = call i32 @s(i32 %r73)
  %r75 = call i32 @s(i32 %r74)
  %r76 = call i32 @s(i32 %r75)
  %r77 = call i32 @s(i32 %r76)
  %r78 = call i32 @s(i32 %r77)
  %r79 = call i32 @s(i32 %r78)
  %r80 = call i32 @s(i32 %r79)
  %r81 = call i32 @s(i32 %r80)
  %r82 = call i32 @s(i32 %r81)
  call void @printInt(i32 %r82)
  ret i32 0
}

