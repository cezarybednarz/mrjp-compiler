declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r30 = call i32 @foo(i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2)
  ret i32 %r30
}

define i32 @foo(i32 %r0, i32 %r1, i32 %r2, i32 %r3, i32 %r4, i32 %r5, i32 %r6, i32 %r7, i32 %r8, i32 %r9, i32 %r10, i32 %r11, i32 %r12, i32 %r13) {
  br label %L15
L15:                              ; preds = [L0]
  %r31 = mul i32 2, %r0
  %r33 = sdiv i32 %r1, 2
  %r34 = add i32 %r31, %r33
  %r36 = add i32 %r34, %r2
  %r38 = add i32 %r36, %r3
  %r40 = add i32 %r38, %r4
  %r42 = add i32 %r40, %r5
  %r44 = add i32 %r42, %r6
  %r46 = add i32 %r44, %r7
  %r48 = add i32 %r46, %r8
  %r50 = sdiv i32 %r9, 2
  %r51 = add i32 %r48, %r50
  %r53 = add i32 %r51, %r10
  %r55 = add i32 %r53, %r11
  %r57 = add i32 %r55, %r12
  %r59 = add i32 %r57, %r13
  %r60 = srem i32 %r59, 10
  call void @printInt(i32 %r60)
  ret i32 %r60
}

