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
  %r4 = add i32 3, 1
  %r6 = xor i1 true, true
  br i1 %r6, label %L8, label %L7
L7:                              ; preds = [L1]
  br label %L8
L8:                              ; preds = [L1]
  br i1 true, label %L10, label %L13
L10:                              ; preds = [L8]
  br label %L16
L13:                              ; preds = [L8]
  %r15 = add i32 3, 2
  br label %L16
L16:                              ; preds = [L13,L10]
  call void @printInt(i32 %r4)
  ret i32 0
}

