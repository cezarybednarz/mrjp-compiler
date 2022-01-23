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
  br label %L6
L6:                              ; preds = [L1,L9]
  %r19 = phi i32 [ %r19, %L9 ], [ 1, %L1 ]
  %r18 = phi i32 [ %r18, %L9 ], [ %r5, %L1 ]
  %r8 = icmp sgt i32 %r18, 0
  br i1 %r8, label %L9, label %L15
L9:                              ; preds = [L6]
  %r12 = mul i32 %r19, %r18
  %r14 = sub i32 %r18, 1
  br label %L6
L15:                              ; preds = [L6]
  call void @printInt(i32 %r19)
  ret i32 0
}

