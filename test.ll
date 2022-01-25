declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @f(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r6 = mul i32 %r0, %r0
  ret i32 %r6
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = call i32 @f(i32 1)
  %r5 = add i32 3, %r2
  br i1 true, label %L11, label %L7
L7:                              ; preds = [L1]
  br i1 true, label %L9, label %L8
L8:                              ; preds = [L7]
  br label %L9
L9:                              ; preds = [L7]
  br label %L11
L11:                              ; preds = [L1]
  br i1 true, label %L13, label %L16
L13:                              ; preds = [L11]
  %r15 = add i32 3, %r2
  br label %L19
L16:                              ; preds = [L11]
  %r18 = add i32 3, %r2
  br label %L19
L19:                              ; preds = [L16,L13]
  %r22 = phi i32 [ %r15, %L13 ], [ %r18, %L16 ]
  call void @printInt(i32 %r22)
  ret i32 0
}

