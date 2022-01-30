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
  br label %L3
L3:                              ; preds = [L0]
  %r4 = alloca %ArrRetVal
  %r5 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r4, i32 0, i32 0
  store i32 %r1, i32* %r5
  %r6 = getelementptr inbounds %ArrRetVal, %ArrRetVal* %r4, i32 0, i32 1
  store i32* %r0, i32** %r6
  ret %ArrRetVal* %r4
}

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = mul i32 4, 2
  %r3 = call i8* @malloc(i32 %r2)
  %r4 = bitcast i8* %r3 to i32*
  %r5 = getelementptr inbounds i32, i32* %r4, i64 0
  store i32 1, i32* %r5
  %r6 = getelementptr inbounds i32, i32* %r4, i64 1
  store i32 69, i32* %r6
  ret i32 0
}

