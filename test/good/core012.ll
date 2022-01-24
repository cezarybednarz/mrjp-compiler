declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.5 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.str.4 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.3 = private unnamed_addr constant [14 x i8] c"concatenation\00", align 1
@.str.2 = private unnamed_addr constant [2 x i8] c" \00", align 1
@.str.1 = private unnamed_addr constant [7 x i8] c"string\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r3 = sub i32 0, 23
  %r7 = add i32 56, %r3
  call void @printInt(i32 %r7)
  %r10 = sub i32 56, %r3
  call void @printInt(i32 %r10)
  %r13 = mul i32 56, %r3
  call void @printInt(i32 %r13)
  %r14 = sdiv i32 45, 2
  call void @printInt(i32 %r14)
  %r15 = srem i32 78, 3
  call void @printInt(i32 %r15)
  %r18 = sub i32 56, %r3
  %r21 = add i32 56, %r3
  %r22 = icmp sgt i32 %r18, %r21
  call void @printBool(i1 %r22)
  %r25 = sdiv i32 56, %r3
  %r28 = mul i32 56, %r3
  %r29 = icmp sle i32 %r25, %r28
  call void @printBool(i1 %r29)
  %r30 = call i8* @__concatStrings__(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.1, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  %r31 = call i8* @__concatStrings__(i8* %r30, i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.3, i32 0, i32 0))
  call void @printString(i8* %r31)
  ret i32 0
}

define void @printBool(i1 %r0) {
  br label %L2
L2:                              ; preds = [L0]
  br i1 %r0, label %L5, label %L7
L5:                              ; preds = [L2]
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.4, i32 0, i32 0))
  ret void
  br label %L9
L7:                              ; preds = [L2]
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.5, i32 0, i32 0))
  ret void
  br label %L9
L9:                              ; preds = [L7,L5]
  ret void
}

