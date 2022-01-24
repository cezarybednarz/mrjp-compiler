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
  br label %L5
L5:                              ; preds = [L1,L8]
  %r14 = phi i32 [ %r10, %L8 ],  [ 5, %L1 ]
  %r7 = icmp sgt i32 %r14, 0
  br i1 %r7, label %L8, label %L12
L8:                              ; preds = [L5]
  %r10 = sub i32 %r14, 1
  call void @printInt(i32 %r10)
  br label %L5
L12:                              ; preds = [L5]
  ret i32 0
}

