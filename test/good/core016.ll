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
  %r2 = alloca i32
  store i32 17, i32* %r2
  br label %L3
L3:                              ; preds = [L1,L6]
  %r4 = load i32, i32* %r2
  %r5 = icmp sgt i32 %r4, 0
  br i1 %r5, label %L6, label %L9
L6:                              ; preds = [L3]
  %r7 = load i32, i32* %r2
  %r8 = sub i32 %r7, 2
  store i32 %r8, i32* %r2
  br label %L3
L9:                              ; preds = [L3]
  %r10 = load i32, i32* %r2
  %r11 = icmp slt i32 %r10, 0
  br i1 %r11, label %L12, label %L14
L12:                              ; preds = [L9]
  call void @printInt(i32 0)
  ret i32 0
  br label %L16
L14:                              ; preds = [L9]
  call void @printInt(i32 1)
  ret i32 0
  br label %L16
L16:                              ; preds = [L14,L12]
  ret i32 0
}

