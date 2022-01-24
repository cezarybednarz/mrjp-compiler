declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.1 = private unnamed_addr constant [4 x i8] c"foo\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  call void @printInt(i32 1)
  call void @printInt(i32 78)
  br label %L6
L6:                              ; preds = [L1,L9]
  %r28 = phi i32 [ %r11, %L9 ], [ 78, %L1 ]
  %r8 = icmp sgt i32 %r28, 76
  br i1 %r8, label %L9, label %L17
L9:                              ; preds = [L6]
  %r11 = sub i32 %r28, 1
  call void @printInt(i32 %r11)
  %r14 = add i32 %r11, 7
  call void @printInt(i32 %r14)
  br label %L6
L17:                              ; preds = [L6]
  call void @printInt(i32 %r28)
  %r20 = icmp sgt i32 %r28, 4
  br i1 %r20, label %L21, label %L24
L21:                              ; preds = [L17]
  call void @printInt(i32 4)
  br label %L25
L24:                              ; preds = [L17]
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0))
  br label %L25
L25:                              ; preds = [L24,L21]
  %r29 = phi i32 [ %r28, %L21 ], [ %r28, %L24 ]
  call void @printInt(i32 %r29)
  ret i32 0
}

