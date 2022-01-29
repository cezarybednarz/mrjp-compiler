declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
declare i8* @malloc(i32) nounwind
@.str.2 = private unnamed_addr constant [6 x i8] c"world\00", align 1
@.str.1 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:                              ; preds = [L0]
  %r2 = mul i32 8, 2
  %r3 = call i8* @malloc(i32 %r2)
  %r4 = bitcast i8* %r3 to i8**
  %r5 = getelementptr inbounds i8*, i8** %r4, i64 0
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.1, i32 0, i32 0), i8** %r5
  %r6 = getelementptr inbounds i8*, i8** %r4, i64 1
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.2, i32 0, i32 0), i8** %r6
  %r7 = alloca i32
  store i32 0, i32* %r7
  br label %L8
L8:                              ; preds = [L1,L11]
  %r9 = load i32, i32* %r7
  %r10 = icmp slt i32 %r9, 2
  br i1 %r10, label %L11, label %L20
L11:                              ; preds = [L8]
  %r12 = load i32, i32* %r7
  %r13 = sext i32 %r12 to i64
  %r14 = getelementptr inbounds i8*, i8** %r4, i64 %r13
  %r15 = load i8*, i8** %r14
  %r16 = alloca i8*
  store i8* %r15, i8** %r16
  %r17 = load i8*, i8** %r16
  call void @printString(i8* %r17)
  %r18 = load i32, i32* %r7
  %r19 = add i32 %r18, 1
  store i32 %r19, i32* %r7
  br label %L8
L20:                              ; preds = [L8]
  ret i32 0
}

