declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.2 = private unnamed_addr constant [3 x i8] c"ok\00", align 1
@.str.1 = private unnamed_addr constant [6 x i8] c"siema\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %L1
L1:
  %r2 = alloca i8*
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.1, i32 0, i32 0), i8** %r2
  %r3 = call i1 @test(i32 3)
  %r4 = xor i1 %r3, true
  br i1 %r4, label %L9, label %L5
L5:
  %r6 = sub i32 0, 5
  %r7 = call i1 @test(i32 %r6)
  %r8 = xor i1 %r7, true
  br label %L9
L9:
  %r10 = phi i1 [ %r8, %L5 ], [ true, %L1 ]
  %r11 = xor i1 %r10, true
  br i1 %r11, label %L12, label %L14
L12:
  ret i32 0
  br label %L14
L14:
  %r15 = call i1 @test(i32 3)
  br i1 %r15, label %L19, label %L16
L16:
  %r17 = sub i32 0, 5
  %r18 = call i1 @test(i32 %r17)
  br label %L19
L19:
  %r20 = phi i1 [ %r18, %L16 ], [ true, %L14 ]
  br i1 %r20, label %L21, label %L23
L21:
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.2, i32 0, i32 0))
  ret i32 0
  br label %L23
L23:
  ret i32 0
}

define i1 @test(i32 %r0) {
  br label %L2
L2:
  %r3 = alloca i32
  store i32 %r0, i32* %r3
  %r4 = load i32, i32* %r3
  %r5 = icmp sgt i32 %r4, 0
  ret i1 %r5
}

