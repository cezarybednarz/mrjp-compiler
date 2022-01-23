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
  %r2 = call i32 @fac(i32 5)
  call void @printInt(i32 %r2)
  ret i32 0
}

define i32 @fac(i32 %r0) {
  br label %L2
L2:
  br label %L7
L7:
  %r9 = icmp sgt i32 %r6, 0
  br i1 %r9, label %L10, label %L16
L10:
  %r20 = phi i32 [ %r6, %L7 ], [ %r6, %L7 ]
  %r19 = phi i32 [ 1, %L7 ], [ 1, %L7 ]
  %r13 = mul i32 %r19, %r20
  %r15 = sub i32 %r20, 1
  br label %L7
L16:
  ret i32 1
}

