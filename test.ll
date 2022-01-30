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
@.str.2 = private unnamed_addr constant [3 x i8] c"69\00", align 1
@.str.1 = private unnamed_addr constant [2 x i8] c"1\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define %ArrRetVal* @same(i8** %r0, i32 %r1) {
  br label %L3
L3:                              ; preds = [L0]
  %r4 = alloca %ArrRetVal
  %r5 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r4, i32 0, i32 0
  store i32 %r1, i32* %r5
  %r6 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r4, i32 0, i32 3
  store i8** %r0, i8*** %r6
  ret %ArrRetVal* %r4
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = mul i32 8, 2
  %r3 = call i8* @malloc(i32 %r2)
  %r4 = bitcast i8* %r3 to i8**
  %r5 = getelementptr inbounds i8*, i8** %r4, i64 0
  store i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i32 0, i32 0), i8** %r5
  %r6 = getelementptr inbounds i8*, i8** %r4, i64 1
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.2, i32 0, i32 0), i8** %r6
  %r7 = call %ArrRetVal* @same(i8** %r4, i32 2)
  %r8 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r7, i32 0, i32 0
  %r9 = load i32, i32* %r8
  %r10 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r7, i32 0, i32 3
  %r11 = load i8**, i8*** %r10
  %r12 = mul i32 8, %r9
  %r13 = call i8* @malloc(i32 %r12)
  %r14 = bitcast i8* %r13 to i8***
  %r15 = getelementptr inbounds i8*, i8** %r14, i64 1
  %r16 = load i8*, i8** %r15
  call void @printString(i8* %r16)
  ret i32 0
}

