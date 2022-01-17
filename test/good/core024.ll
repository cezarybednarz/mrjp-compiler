declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i32 @__equStrings__(i8*, i8*)
declare i32 @__neStrings__(i8*, i8*)
declare i8* @__concatStrings__(i8*, i8*)
@.str.2 = private unnamed_addr constant [5 x i8] c"NOOO\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"yes\00", align 1
@.str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32 @main() {
  br label %1
1:
  call void @f(i32 1, i32 2)
  ret i32 0
}

define void @f(i32 %0, i32 %1) {
  br label %3
3:
  %4 = alloca i32
  store i32 %0, i32* %4
  %5 = alloca i32
  store i32 %1, i32* %5
  %6 = load i32, i32* %5
  %7 = load i32, i32* %4
  %8 = icmp sgt i32 %6, %7
  br i1 %8, label %11, label %9
9:
  %10 = call i1 @e()
  br label %11
11:
  %12 = phi i1 [ true, %3 ], [ %10, %9 ]
  br i1 %12, label %13, label %14
13:
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0))
  br label %14
14:
  ret void
}

define i1 @e() {
  br label %1
1:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i32 0, i32 0))
  ret i1 false
}

