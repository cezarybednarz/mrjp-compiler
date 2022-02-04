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
  %r2 = xor i1 true, true
  br i1 %r2, label %L11, label %L3
L3:                              ; preds = [L1]
  br i1 %r2, label %L7, label %L5
L5:                              ; preds = [L3]
  br label %L7
L7:                              ; preds = [L3,L5]
  %r8 = phi i1 [ true, %L3 ], [ %r2, %L5 ]
  %r9 = xor i1 %r8, true
  %r10 = xor i1 %r9, true
  br label %L11
L11:                              ; preds = [L1,L7]
  %r12 = phi i1 [ true, %L1 ], [ %r10, %L7 ]
  %r13 = xor i1 %r12, true
  br i1 %r13, label %L14, label %L15
L14:                              ; preds = [L11]
  call void @printInt(i32 1)
  br label %L16
L15:                              ; preds = [L11]
  call void @printInt(i32 2)
  br label %L16
L16:                              ; preds = [L15,L14]
  ret i32 0
}

