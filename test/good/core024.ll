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
@.str.2 = private unnamed_addr constant [5 x i8] c"NOOO\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"yes\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  call void @f(i32 1, i32 2)
  ret i32 0
}

define void @f(i32 %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r8 = icmp sgt i32 %r1, %r0
  br i1 %r8, label %L11, label %L9
L9:                              ; preds = [L3]
  %r10 = call i1 @e()
  br label %L11
L11:                              ; preds = [L3]
  %r12 = phi i1 [ %r10, %L9 ], [ true, %L3 ]
  br i1 %r12, label %L13, label %L14
L13:                              ; preds = [L11]
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0))
  br label %L14
L14:                              ; preds = [L13,L11]
  ret void
}

define i1 @e() {
  br label %L1
L1:                              ; preds = [L0]
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i32 0, i32 0))
  ret i1 false
}

