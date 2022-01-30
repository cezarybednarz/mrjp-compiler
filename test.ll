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

define %ArrRetVal* @same(i32* %r0, i32 %r1) {
  %r14 = phi %ArrRetVal* 
  %r13 = phi i32 
  %r12 = phi i32 
  br label %L3
L3:                              ; preds = [L0]
  %r4 = getelementptr inbounds i32, i32* %r0, i64 1
  %r5 = load i32, i32* %r4
  %r6 = add i32 %r12, 1
  %r7 = getelementptr inbounds i32, i32* %r0, i64 1
  %r8 = alloca %ArrRetVal
  %r9 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r8, i32 0, i32 0
  store i32 %r13, i32* %r9
  %r10 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r8, i32 0, i32 1
  store i32* %r0, i32** %r10
  ret %ArrRetVal* %r14
}

define i32 @main() {
  %r16 = phi i32 
  %r15 = phi i32* 
  br label %L1
L1:                              ; preds = [L0]
  %r2 = mul i32 4, 2
  %r3 = call i8* @malloc(i32 %r2)
  %r4 = bitcast i8* %r3 to i32*
  %r5 = getelementptr inbounds i32, i32* %r4, i64 0
  %r6 = getelementptr inbounds i32, i32* %r4, i64 1
  %r7 = call %ArrRetVal* @same(i32* %r15, i32 2)
  %r8 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r7, i32 0, i32 0
  %r9 = load i32, i32* %r8
  %r10 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r7, i32 0, i32 1
  %r11 = load i32*, i32** %r10
  %r12 = getelementptr inbounds i32, i32* %r11, i64 1
  %r13 = load i32, i32* %r12
  call void @printInt(i32 %r16)
  ret i32 0
}

