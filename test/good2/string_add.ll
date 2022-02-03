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
@.str.6 = private unnamed_addr constant [6 x i8] c"ERROR\00", align 1
@.str.5 = private unnamed_addr constant [3 x i8] c"OK\00", align 1
@.str.4 = private unnamed_addr constant [5 x i8] c"kota\00", align 1
@.str.3 = private unnamed_addr constant [3 x i8] c"ma\00", align 1
@.str.2 = private unnamed_addr constant [4 x i8] c"ala\00", align 1
@.str.1 = private unnamed_addr constant [10 x i8] c"alamakota\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r3 = call i8* @__concatStrings__(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.2, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.3, i32 0, i32 0))
  %r4 = call i8* @__concatStrings__(i8* %r3, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.4, i32 0, i32 0))
  %r6 = call i32 @__equStrings__(i8* %r4, i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.1, i32 0, i32 0))
  %r7 = icmp eq i32 1, %r6
  br i1 %r7, label %L8, label %L9
L8:                              ; preds = [L1]
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.5, i32 0, i32 0))
  br label %L10
L9:                              ; preds = [L1]
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.6, i32 0, i32 0))
  br label %L10
L10:                              ; preds = [L9,L8]
  ret i32 0
}

