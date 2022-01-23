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
L1:
  br label %L3
L3:
  %r5 = icmp sgt i32 17, 0
  br i1 %r5, label %L6, label %L9
L6:
  %r17 = phi i32 [ 17, %L3 ], [ 17, %L3 ]
  %r8 = sub i32 %r17, 2
  br label %L3
L9:
  %r11 = icmp slt i32 17, 0
  br i1 %r11, label %L12, label %L14
L12:
  call void @printInt(i32 0)
  ret i32 0
  br label %L16
L14:
  call void @printInt(i32 1)
  ret i32 0
  br label %L16
L16:
  ret i32 0
}

