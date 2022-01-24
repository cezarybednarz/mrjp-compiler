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
  %r2 = call i32 @ev(i32 17)
  call void @printInt(i32 %r2)
  ret i32 0
}

define i32 @ev(i32 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  %r5 = icmp sgt i32 %r0, 0
  br i1 %r5, label %L6, label %L11
L6:                              ; preds = [L2]
  %r8 = sub i32 %r0, 2
  %r9 = call i32 @ev(i32 %r8)
  ret i32 %r9
  br label %L19
L11:                              ; preds = [L2]
  %r13 = icmp slt i32 %r0, 0
  br i1 %r13, label %L14, label %L16
L14:                              ; preds = [L11]
  ret i32 0
  br label %L18
L16:                              ; preds = [L11]
  ret i32 1
  br label %L18
L18:                              ; preds = [L16,L14]
  br label %L19
L19:                              ; preds = [L18,L6]
  ret i32 0
}

