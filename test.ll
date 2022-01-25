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
  br i1 true, label %L8, label %L7
L7:                              ; preds = [L1]
  br label %L8
L8:                              ; preds = [L1]
  br i1 true, label %L10, label %L13
L10:                              ; preds = [L8]
  br label %L16
L13:                              ; preds = [L8]
  br label %L16
L16:                              ; preds = [L13,L10]
  %r18 = sub i32 %r2, 1
  call void @printInt(i32 %r5)
  ret i32 0
}

