	@.str.2 = private unnamed_addr constant [6x i8] c"world\00", align 1
	@.str.1 = private unnamed_addr constant [6x i8] c"hello\00", align 1
declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @concatStrings(i8*, i8*)
declare i32 @compareStrings(i8*, i8*)
declare i32 @equStrings(i8*, i8*)
declare i32 @neStrings(i8*, i8*)

define i8* @add(	i8* %r_1, 	i8* %r_2, 	i1 %r_3) {
	%r_4 = alloca i8*
	store i8* %r_1, i8** %r_4
	%r_5 = alloca i8*
	store i8* %r_2, i8** %r_5
	%r_6 = alloca i1
	store i1 %r_3, i1* %r_6
	%r_7 = load i1, i1* %r_6
	%r_8 = xor i1 %r_7, true
	br i1 %r_8, label %l_9, label %l_10
l_9:
	%r_12 = load i8*, i8** %r_4
	%r_13 = load i8*, i8** %r_5
%r_14 = call i8* @concatStrings (i8* %r_12, i8* %r_13)
	ret i8* %r_14
l_10:
	%r_15 = load i8*, i8** %r_5
	%r_16 = load i8*, i8** %r_4
%r_17 = call i8* @concatStrings (i8* %r_15, i8* %r_16)
	ret i8* %r_17
}
define i32 @main(i32 %argc, i8** %argv) {
	%r_18 = alloca i8*
	store i8* getelementptr inbounds ([6x i8], [6 x i8]* @.str.1, i32 0, i32 0), i8** %r_18
	%r_19 = alloca i8*
	store i8* getelementptr inbounds ([6x i8], [6 x i8]* @.str.2, i32 0, i32 0), i8** %r_19
	%r_20 = load i8*, i8** %r_18
	%r_21 = load i8*, i8** %r_19
%r_22 = call i8* @add (i8* %r_20, i8* %r_21, i1 true)
	call void @printString (i8* %r_22)
	ret i32 0
}