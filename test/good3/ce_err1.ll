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

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = call i32 @f(i32 1, i32 2)
  call void @printInt(i32 %r2)
  ret i32 0
}

define i32 @f(i32 %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r11 = add i32 %r0, %r1
  br label %L12
L12:                              ; preds = [L3,L32]
  %r54 = phi i32 [ %r55, %L32 ], [ 0, %L3 ]
  %r51 = phi i32 [ %r51, %L32 ], [ %r1, %L3 ]
  %r48 = phi i32 [ %r48, %L32 ], [ %r0, %L3 ]
  %r45 = phi i32 [ %r45, %L32 ], [ %r11, %L3 ]
  %r42 = phi i32 [ %r34, %L32 ], [ 3, %L3 ]
  %r14 = icmp sge i32 %r42, 0
  br i1 %r14, label %L15, label %L35
L15:                              ; preds = [L12]
  %r17 = icmp sgt i32 %r42, 0
  br i1 %r17, label %L18, label %L32
L18:                              ; preds = [L15]
  %r20 = icmp sgt i32 %r45, 10
  br i1 %r20, label %L21, label %L26
L21:                              ; preds = [L18]
  %r24 = add i32 %r48, %r51
  %r25 = sdiv i32 %r24, 3
  br label %L31
L26:                              ; preds = [L18]
  %r29 = add i32 %r48, %r51
  %r30 = mul i32 3, %r29
  br label %L31
L31:                              ; preds = [L26,L21]
  %r56 = phi i32 [ %r25, %L21 ], [ %r30, %L26 ]
  br label %L32
L32:                              ; preds = [L31,L15]
  %r55 = phi i32 [ %r54, %L15 ], [ %r56, %L31 ]
  %r34 = sub i32 %r42, 1
  br label %L12
L35:                              ; preds = [L12]
  %r38 = add i32 %r48, %r51
  %r40 = add i32 %r38, %r54
  ret i32 %r40
}

