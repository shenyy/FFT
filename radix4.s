.file "radix4.s"

.define N,1024
.define zero,0
.define ie,1
.define wn_h,0x3BC9  ;2*pi/1024
.define wn_l,0x0fdb
.define DEBUG,1

.data
.global val
.align 4
.type const,@object
.size const,256
.type input,@object
.size input,8
const:
	.long 0xad49cba5 ;-1/14!
	.long 0xad49cba5 ;-1/14!
	.long 0xad49cba5 ;-1/14!
	.long 0xad49cba5 ;-1/14!
	.long 0x310f76c7 ;1/12!
	.long 0x310f76c7 ;1/12!
	.long 0x310f76c7 ;1/12!
	.long 0x310f76c7 ;1/12!
	.long 0xb493f27e ;-1/10!
	.long 0xb493f27e ;-1/10!
	.long 0xb493f27e ;-1/10!
	.long 0xb493f27e ;-1/10!
	.long 0x37d00d01 ;1/40320
	.long 0x37d00d01 ;1/40320
	.long 0x37d00d01 ;1/40320
	.long 0x37d00d01 ;1/40320
	.long 0xbab60b61 ;-1/720
	.long 0xbab60b61 ;-1/720
	.long 0xbab60b61 ;-1/720
	.long 0xbab60b61 ;-1/720
	.long 0x3d2aaaab ;1/24
	.long 0x3d2aaaab ;1/24
	.long 0x3d2aaaab ;1/24
	.long 0x3d2aaaab ;1/24
	.long 0xbf000000 ;-1/2
	.long 0xbf000000 ;-1/2
	.long 0xbf000000 ;-1/2
	.long 0xbf000000 ;-1/2
	.long 0x3f800000 ;1
	.long 0x3f800000 ;1
	.long 0x3f800000 ;1
	.long 0x3f800000 ;1
	.long 0xab573f9f ;-1/15!
	.long 0xab573f9f ;-1/15!
	.long 0xab573f9f ;-1/15!
	.long 0xab573f9f ;-1/15!
	.long 0x2f309231 ;1/13!
	.long 0x2f309231 ;1/13!
	.long 0x2f309231 ;1/13!
	.long 0x2f309231 ;1/13!
	.long 0xb2d7322b ;-1/11!
	.long 0xb2d7322b ;-1/11!
	.long 0xb2d7322b ;-1/11!
	.long 0xb2d7322b ;-1/11!
	.long 0x3638ef15 ;1/9!
	.long 0x3638ef15 ;1/9!
	.long 0x3638ef15 ;1/9!
	.long 0x3638ef15 ;1/9!
	.long 0xb9500d00 ;-1/7!
	.long 0xb9500d00 ;-1/7!
	.long 0xb9500d00 ;-1/7!
	.long 0xb9500d00 ;-1/7!
	.long 0x3c088889 ;1/5!
	.long 0x3c088889 ;1/5!
	.long 0x3c088889 ;1/5!
	.long 0x3c088889 ;1/5!
	.long 0xbe2aaaab ;-1/3!
	.long 0xbe2aaaab ;-1/3!
	.long 0xbe2aaaab ;-1/3!
	.long 0xbe2aaaab ;-1/3!
	.long 0x3f800000 ;1
	.long 0x3f800000 ;1
	.long 0x3f800000 ;1
	.long 0x3f800000 ;1

input:
	.long 0xBCE3BCD3	       ;-0.0278
	.long 0x3DC32CA5	       ;0.0953


.text
.align 4
.func fft
.global fft
.type fft,@function

fft:
	setw.32 (.XD)

int:	
	mov.k.32 (.XM) X4,#1		;ie		
||	mov.k.32 (.YM) Y2,#wn_l

	mov.k.32 (.XM) X1,@input
	mov.kh.32 (.XM) X1,@input

	add.32 (.XA) X1,X1,#4
||	mov.kh.32 (.XM) X4,#zero
||	ld.w.32 (.XD) X5,X1		;load the 1st num
||	mov.kh.32 (.YM) Y2,#wn_h
	
	mov.k.32 (.XM) X21,#N
||	ld.w.32 (.XD) X6,X1		;load the 2nd num
||	mov.fq.32 (.YD) Y2,Y2,#1

	mov.kh.32 (.XM) X21,#zero
||	mov.fq.32 (.XD) G0,X5,#0
||	mov.fq.32 (.YD) Y2,Y2,#2

	mov.k.32 (.XM) X2,#zero	
||	mov.fq.32 (.XD) G0,X5,#2
||	mov.fq.32 (.YD) Y2,Y2,#3	

	mov.fq.32 (.XD) G0,X6,#1

	mov.kh.32 (.XM) X2,#zero
||	mov.fq.32 (.XD) G0,X6,#3

	mov.r.32 (.XM) X0,X21

	sub.32 (.XA) X0,X0,#2
||	mov.r.32 (.XM) X3,X21		;n2=N 
	

loadin:	st.q.32 (.XD) *-FP[X7],G0
	sub.32 (.XA) X7,X7,#16
	sub.32 (.XA) X0,X0,#2	
	cmpl.s.32 (.XD) X1,X2,X0	;X0>0? 1:0
	[X1] b.32 (.XD) @loadin

	mov.r.32 (.XM) X20,X21		;k=N
l1:	mov.r.32 (.XM) X5,X3 		;n1=n2
	lsr.32 (.XA) X3,X3,#2		;n2>>2
	
||	mov.r.32 (.XM) X6,X2		;ia1=0

	mov.r.32 (.XM) X19,X2		;j=0

;################cal Wn #################### 
l2:	add.32 (.XA) X7,X6,X6		;ia2=ia1+ia1
||	mov.fq.32 (.XD) G3,X6,#0	;ia1->G3
||	mov.k.32 (.YM) Y5,@const

	add.32 (.XA) X8,X7,X6		;ia3=ia2+ia1
||	mov.fq.32 (.XD) G3,X7,#1	;ia2->G3
||	mov.kh.32 (.YM) Y5,@const	;&a6

	add.32 (.XA) X9,X6,X4		;ia4=ia1+ie
||	mov.fq.32 (.XD) G3,X8,#2	;ia3->G3
||	ld.q.32 (.YD) G5,*Y5		;a6

	add.32 (.XA) X10,X9,X9		;ia5=ia4+ia4
||	dintsp.32 (.XM) G3,G3		
||	mov.fq.32 (.XD) G4,X9,#0	;ia4->G4
||	add.32 (.YA) Y5,Y5,#16		;&a5

	add.32 (.XA) X12,X9,X10		;ia6=ia4+ia5				
||	mov.fq.32 (.XD) G4,X10,#1	;ia5->G4
||	add.32 (.YA) Y5,Y5,#16		;&a4
||	qmpy.sp.32 (.YM) G6,Y2,G3	;x1
||	ld.q.32 (.YD) Y9,*Y5		;a5 for x2

	qmpy.sp.32 (.XM) X13,G6,G6	;x1^2
||	mov.fq.32 (.XD) G4,X12,#2	;ia6->G4
||	movyx.32 (.YD) X14,Y9		;a5 for x1

	dintsp.32 (.XM) G4,G4
||	add.32 (.YA) Y5,Y5,#16		;&a3
||	ld.q.32 (.YD) Y8,*Y5		;a4 for x2

	qmpyadd.sp.32 (.XM) X14,G5,X13	;p1=a5+a6*x1^2
||	qmpy.sp.32 (.YM) Y4,Y2,G4	;x2
||	movyx.32 (.YD) X15,Y8		;a4 for x1

	qmpyadd.sp.32 (.XM) X15,X14,X13	;p2=a4+p1*x1^2
||	add.32 (.YA) Y5,Y5,#16		;&a2
||	qmpy.sp.32 (.YM) Y6,Y4,Y4	;x2^2
||	ld.q.32 (.YD) G0,*Y5		;a3 for x2

	mov.q.32 (.XD) X16,G0		;a3 for x1
||	add.32 (.YA) Y5,Y5,#16		;&a1
||	qmpyadd.sp.32 (.YM) Y9,G5,Y6	;p1=a5+a6*x2^2
||	ld.q.32 (.YD) G5,*Y5		;a2 for x2

	qmpyadd.sp.32 (.XM) X16,X15,X13	;p3=a3+p2*x1^2
||	mov.q.32 (.XD) X17,G5		;a2 for x1
||	add.32 (.YA) Y5,Y5,#16		;&a0
||	qmpyadd.sp.32 (.YM) Y8,Y9,Y6	;p2=a4+p1*x2^2
||	ld.q.32 (.YD) Y9,*Y5		;a1 for x2

	qmpyadd.sp.32 (.YM) G0,Y8,Y6	;p3=a3+p2*x2^2
||	movyx.32 (.YD) X14,Y9		;a1 for x1

	qmpyadd.sp.32 (.XM) X17,X16,X13	;p4=a2+p3*x1
||	add.32 (.YA) Y5,Y5,#16		;&#1
||	qmpyadd.sp.32 (.YM) G5,G0,Y6	;p4=a2+p3*x2
||	ld.q.32 (.YD) Y8,*Y5		;a0 for x2

	qmpyadd.sp.32 (.XM) X14,X17,X13	;p5=a1+p4*x1	
||	add.32 (.YA) Y5,Y5,#16		;&a6
||	qmpyadd.sp.32 (.YM) Y9,G5,Y6	;p5=a1+p4*x2
||	ld.q.32 (.YD) G0,*Y5		;#1 for x2

	mov.q.32 (.XD) X16,G0		;#1 for x1
||	movyx.32 (.YD) X15,Y8		;a0 for x1

	add.32 (.YA) Y5,Y5,#16		;&a5
||	qmpyadd.sp.32 (.YM) Y8,Y9,Y6	;p6=a0+p5*x
||	ld.q.32 (.YD) G5,*Y5		;a6_sin

	qmpyadd.sp.32 (.XM) X15,X14,X13	;p6=a0+p5*x
||	add.32 (.YA) Y5,Y5,#16		;&a4
||	qmpyadd.sp.32 (.YM) G0,Y8,Y6	;co6 co5 co4
||	ld.q.32 (.YD) Y9,*Y5		;a5_sin for x2

	qmpyadd.sp.32 (.XM) X16,X15,X13	;co3 co2 co1
||	movyx.32 (.YD) X14,Y9		;a5 for x1

	ld.q.32 (.YD) Y8,*Y5		;a4_sin for x2

	qmpyadd.sp.32 (.XM) X14,G5,X13	;p1=a5+a6*x1^2
||	add.32 (.YA) Y5,Y5,#16		;&a3
||	qmpyadd.sp.32 (.YM) Y9,G5,Y6	;p1=a5+a6*x2^2
||	movyx.32 (.YD) X15,Y8		;a4 for x1

	mov.q.32 (.XD) X16,G0		;a3 for x1
||	add.32 (.YA) Y5,Y5,#16		;&a2
||	qmpyadd.sp.32 (.YM) Y8,Y9,Y6	;p2=a4+p1*x2^2
||	ld.q.32 (.YD) G0,*Y5		;a3_sin for x2

	qmpyadd.sp.32 (.XM) X15,X14,X13	;p2=a4+p1*x1^2
||	mov.q.32 (.XD) X16,G0		;a3 for x1
||	qmpyadd.sp.32 (.YM) G0,Y8,Y6	;p3=a3+p2*x2^2	

	qmpyadd.sp.32 (.XM) X16,X15,X13	;p3=a3+p2*x1^2
||	add.32 (.YA) Y5,Y5,#16		;&a1
||	ld.q.32 (.YD) G5,*Y5		;a2_sin for x2

	mov.q.32 (.XD) X17,G5		;a2 for x1
||	add.32 (.YA) Y5,Y5,#16		;&a0
||	qmpyadd.sp.32 (.YM) G5,G0,Y6	;p4=a2+p3*x2^2
||	ld.q.32 (.YD) Y9,*Y5		;a1_sin for x2

	qmpyadd.sp.32 (.XM) X17,X16,X13	;p4=a2+p3*x1^2
||	add.32 (.YA) Y5,Y5,#16		;&#1
||	movyx.32 (.YD) X14,Y9		;a1 for x1

	ld.q.32 (.YD) G0,*Y5		;a0 for x2

	qmpyadd.sp.32 (.XM) X14,X17,X13	;p5=a1+p4*x1^2
||	mov.q.32 (.XD) X16,G0		;a0 for x1
||	qmpyadd.sp.32 (.YM) Y9,G5,Y6	;p5=a1+p4*x2^2
||	ld.q.32 (.YD) Y8,*Y5		;#1 for x2

	qmpyadd.sp.32 (.XM) X16,X15,X13	;p6=a0+p5*x1^2
||	qmpyadd.sp.32 (.YM) G0,Y8,Y6	;p6=a0+p5*x2^2
||	mov.q.32 (.YD) G1,Y8		;#1 for x1

	qmpyadd.sp.32 (.XM) G1,X17,X13	;p7=1+p6*x1^2
||	qmpyadd.sp.32 (.YM) Y8,G5,Y6	;p7=1+p6*x2^2

	qmpy.sp.32 (.XM) X14,G1,G6	;si3 si2 si1
||	qmpy.sp.32 (.YM) Y12,Y8,Y4	;si6 si5 si4

	add.32 (.XA) X6,X6,X4 
||	movxy.32 (.XD) Y10,X14		;si3 si2 si1

	add.32 (.XA) X6,X6,X4 		;ia1=ia1+ie*2
||	movxy.32 (.XD) Y9,X16		;co3 co2 co1
||	mov.q.32 (.YD) Y11,G0		;co6 co5 co4

;################### cal FFT ############################
	
	mov.r.32 (.XM) X18,X19		;i0=j
l3:	neg.32 (.XA) X7,X18
||	dpack0.sp.32 (.YM) Y7,Y9,Y11 	;co4 co1

	sub.32 (.XA) X8,X7,X3		;i1
||	ld.q.32 (.XD) G0,*-FP[X7]	;x[2*i4+1] x[2*i4] x[2*i0+1],x[2*i0]
||	movyx.32 (.YD) X12,Y7

	sub.32 (.XA) X9,X8,X3		;i2
||	ld.q.32 (.XD) G1,*-FP[X8]	;x[2*i5+1] x[2*i5] x[2*i1+1] x[2*i1]
||	dpack0.sp.32 (.YM) Y13,Y10,Y12  ;si4 si1

	sub.32 (.XA) X10,X9,X3		;i3
||	ld.q.32 (.XD) G3,*-FP[X9]	;x[2*i6+1] x[2*i6] x[2*i2+1] x[2*i2]
||	movyx.32 (.YD) X13,Y13

	ld.q.32 (.XD) G4,*-FP[X10]	;x[2*i7+1] x[2*i7] x[2*i3+1] x[2*i3]
||	dpack1.sp.32 (.YM) Y14,Y9,Y11 	;co5 co2
||	qadd.sp.32 (.YD) Y5,G0,G3	;xh1_1 xh0_1 xh1_0 xh0_0

	qsub.sp.32 (.XD) G0,G0,G3	;xl1_1 xl0_1 xl1_0 xl0_0
||	dpack1.sp.32 (.YM) Y16,Y10,Y12 	;si5 si2
||	qadd.sp.32 (.YD) Y6,G1,G4	;xh21_1 xh20_1 xh21_0 xh20_0

	qsub.sp.32 (.XD) G1,G1,G4	;xl21_1 xl20_1 xl21_0 xl20_0
||	dpack2.sp.32 (.YM) Y21,Y9,Y11	;co6 co3
||	qadd.sp.32 (.YD) G6,Y6,Y5	;x[2*i4+1] x[2*i4] x[2*i0+1] x[2*i0]

	st.q.32 (.XD) *-FP[X7],G6
||	swap.sp.32 (.YM) Y8,G1    	;xl20_1 xl21_1 xl20_0 xl21_0
||	qsub.sp.32 (.YD) G3,Y5,Y6	;yt0_1 xt0_1 yt0_0 xt0_0	

	dpack2.sp.32 (.YM) Y22,Y10,Y12	;si6 si3
||	qadd.sp.32 (.YD) G4,Y8,G0	;yt2_1 xt1_1 yt2_0 xt1_0

	packodd.sp.32 (.XM) X11,G4  	;xt1_1 xt1_0
||	packodd.sp.32 (.YM) Y13,G3	;xt0_1 xt0_0
||	qsub.sp.32 (.YD) G5,G0,Y8	;yt1_1 xt2_1 yt1_0 xt2_0

	packeven.sp.32 (.XM) X14,G5   	;yt1_1 yt1_0
||	packeven.sp.32 (.YM) Y15,G3  	;yt0_1 yt0_0
	
	qmpy.sp.32 (.XM) X15,X11,X12	;xt1_1*co4 xt1_0*co1
||	qmpy.sp.32 (.YM) Y17,Y13,Y14	;xt0_0*co2

	qmpy.sp.32 (.XM) X16,X14,X13	;yt1_1*si4 yt1_0*si1
||	qmpy.sp.32 (.YM) Y18,Y15,Y16	;yt0_0*si2

	qadd.sp.32 (.XD) X16,X16,X15	;x[2*i5] x[2*i1]
||	qmpy.sp.32 (.YM) Y19,Y13,Y16	;xt0_0*si2
||	qadd.sp.32 (.YD) Y18,Y17,Y18	;x[2*i6] x[2*i2]

	qmpy.sp.32 (.XM) X17,X11,X13	;xt1_1*si4 xt1_0*si1
||	qmpy.sp.32 (.YM) Y20,Y15,Y14	;yt0_0*co2

	qmpy.sp.32 (.XM) X18,X14,X12	;yt1_1*co4 yt1_0*co1
||	packeven.sp.32 (.YM) Y3,G5  	;xt2_1 xt2_0
||	qsub.sp.32 (.YD) Y20,Y20,Y19	;x[2*i6+1] x[2*i2+1]

	qsub.sp.32 (.XD) X18,X18,X17	;x[2*i5+1] x[2*i1+1]
||	qpackl.sp.32 (.YM) Y19,Y20,Y18

	qpackl.sp.32 (.XM) X19,X18,X16
||	packodd.sp.32 (.YM) Y4,G5	;yt2_1 yt2_0
||	mov.q.32 (.YD) G6,Y19		;x[2*i6+1] x[2*i6] x[2*i2+1] x[2*i2]

	st.q.32 (.XD) *-FP[X9],G6
||	qmpy.sp.32 (.YM) Y13,Y3,Y21	;xt2_0*co3

	mov.q.32 (.XD) G0,X19		;x[2*i5+1] x[2*i5] x[2*i1+1] x[2*i1]	
||	qmpy.sp.32 (.YM) Y16,Y4,Y22	;yt2_0*si3
	
	st.q.32 (.XD) *-FP[X8],G0
||	qmpy.sp.32 (.YM) Y14,Y3,Y22	;xt2_0*si3
||	qadd.sp.32 (.YD) Y17,Y13,Y16	;x[2*i7] x[2*i3]

	qmpy.sp.32 (.YM) Y15,Y4,Y21	;yt2_0*co3

	qsub.sp.32 (.YD) Y18,Y15,Y14	;x[2*i7+1] x[2*i3+1]

	add.32 (.XA) X18,X18,X5		;i0
||	qpackl.sp.32 (.YM) Y19,Y17,Y18

	cmpl.s.32 (.XD) G1,X18,X21	;i0<N? 1:0
||	mov.q.32 (.YD) G6,Y19		;x[2*i7+1] x[2*i7] x[2*i3+1] x[2*i3]

	st.q.32 (.XD) *-FP[X10],G6

	[G1] b.32 (.YD) @l3

	add.32 (.XA) X19,X19,#2		;j++
	cmpl.s.32 (.XD) X1,X19,X3	;j<n2? 1:0
	[X1] b.32 (.XD) @l2

	lsr.32 (.XA) X20,X20,#2		;k>>2
	asl.32 (.XA) X4,X4,#2		;ie<<2
	cmpg.s.32 (.XD) X0,X20,#4	;k>4? 1:0
	[X0] b.32 (.XD) @l1

	cmpe.32 (.XD) X0,X20,#4		;k==4? 1:0
	[X0] b.32 (.XD) @a
	cmpe.32 (.XD) X0,X20,#2		;k==2? 1:0
	[X0] b.32 (.XD) @b
	
a:	mov.r.32 (.XM) X19,X2	;j=0
	lsr.32 (.XA) X3,X21,#2	;N/4
||	mov.r.32 (.XM) X18,X2	;i0=0
l4:	neg.32 (.XA) G0,X18
	sub.32 (.XA) G3,G0,#1	;i1
||	ld.d.32 (.XD) G6,*-FP[G0]	;x[2*i0+1],x[2*i0]

	sub.32 (.XA) G4,G3,#1	;i2
||	ld.d.32 (.XD) G6,*-FP[G4]	;x[2*i2+1],x[2*i2]
||	mov.q.32 (.YD) Y3,G6

	sub.32 (.XA) G5,G4,#1	;i3
||	ld.d.32 (.XD) G6,*-FP[G3]	;x[2*i1+1],x[2*i1]
||	mov.q.32 (.YD) Y5,G6

	qadd.sp.32 (.YD) Y7,Y3,Y5	;xh1 xh0

	ld.d.32 (.XD) G6,*-FP[G5]	;x[2*i3+1],x[2*i3]
||	mov.q.32 (.YD) Y4,G6

	qsub.sp.32 (.YD) Y9,Y3,Y5	;xl1 xl0
	mov.q.32 (.YD) Y6,G6

	qadd.sp.32 (.YD) Y8,Y4,Y6	;xh21 xh20
	qsub.sp.32 (.YD) Y10,Y4,Y6	;xl21 xl20
	
	qadd.sp.32 (.YD) Y11,Y7,Y8	;x[2*i0+1] X[2*i0]
	mov.q.32 (.YD) G6,Y11

	st.d.32 (.XD) *-FP[G0],G6
	qsub.sp.32 (.YD) Y11,Y7,Y8	;x[2*i2+1] X[2*i2]

	mov.q.32 (.YD) G6,Y11

	st.d.32 (.XD) *-FP[G4],G6
||	mov.qf.32 (.YD) Y11,Y9,#0	;xl0

	mov.qf.32 (.YD) Y12,Y10,#1	;xl21
	add.sp.32 (.YM) Y13,Y11,Y12
	sub.sp.32 (.YM) Y13,Y11,Y12	
||	mov.q.32 (.YD) G6,Y13

	st.w.32 (.XD) *-FP[G3],G6	;x[2*i1]
||	mov.q.32 (.YD) G6,Y13

	st.w.32 (.XD) *-FP[G5],G6	;x[2*i3]
||	mov.qf.32 (.YD) Y11,Y9,#1	;xl1

	sub.32 (.YA) G3,G3,#4	;i1+1
||	mov.qf.32 (.YD) Y12,Y10,#0	;xl20

	sub.32 (.YA) G5,G5,#4	;i3+1
||	sub.sp.32 (.YM) Y13,Y11,Y12	;x[2*i1+1]
	mov.q.32 (.YD) G6,Y13
	add.32 (.XA) X18,X18,#4		;i0
||	st.w.32 (.XD) *-FP[G3],G6
||	add.sp.32 (.YM) Y13,Y11,Y12	;x[2*i3+1]

	add.32 (.XA) X19,X19,#1		;j
||	cmpl.s.32 (.XD) X0,X19,X3	;j<N/4? 1:0
||	mov.q.32 (.YD) G6,Y13
	st.w.32 (.XD) *-FP[G5],G6

	[!X0] B.32 (.XD) @l4
	[X0] B.32 (.XD) @fftend

b:	mov.r.32 (.XM) X19,X2	;j=0
	mov.r.32 (.XM) X18,X2	;i0=0
	lsr.32 (.XA) X3,X21,#1	;N/2
l5:	neg.32 (.XA) G0,X18

	sub.32 (.XA) G3,G0,#1	;i1
||	ld.d.32 (.XD) G6,*-FP[G0]	;x[2*i0+1],x[2*i0]

	mov.q.32 (.YD) Y3,G6
	ld.d.32 (.XD) G6,*-FP[G3]	;x[2*i1+1],x[2*i1]
	mov.q.32 (.YD) Y5,G6

	qadd.sp.32 (.YD) Y7,Y3,Y5	;xh1 xh0
	mov.q.32 (.YD) G1,Y7
	st.w.32 (.XD) *-FP[G0],G1	;xh0
	qsub.sp.32 (.YD) Y9,Y3,Y5	;xl1 xl0
	sub.32 (.YA) G0,G0,#4
||	mov.q.32 (.YD) G6,Y9
	st.w.32 (.XD) *-FP[G3],G6	;xl0
||	mov.qf.32 (.YD) Y10,Y7,#1	;xh1
	sub.32 (.YA) G3,G3,#4
||	mov.q.32 (.YD) G6,Y10
	st.w.32 (.XD) *-FP[G0],G6	
||	sub.32 (.YA) G3,G3,#4
||	mov.qf.32 (.YD) Y10,Y9,#1	;xl1
	add.32 (.XA) X18,X18,#2		;i0
||	mov.q.32 (.YD) G6,Y10
	add.32 (.XA) X19,X19,#1		;j
||	st.w.32 (.XD) *-FP[G3],G6

	cmpl.s.32 (.XD) X0,X19,X3	;j<N/2? 1:0
	[!X0] B.32 (.XD) @l5
;.endif
fftend:
; ####################### bit reverse ########################
;	mov.r.32 (.XM) X3,X2
;	mov.k.32 (.XM) X8,#0xF800
;	mov.kh.32 (.XM) X8,#0xFFFF	;-2048
;l6:	ld.w.32 (.XD) X4,*-FP[X3]
;	neg.32 (.XA) X11,X3
;	mov.k.32 (.XM) X9,#0x03ff
;	mov.kh.32 (.XM) X9,#zero
;	and.32 (.XA) X5,X11,X9
;	mov.k.32 (.XM) X10,#0x5555
;	mov.kh.32 (.XM) X10,#0x5555
;	and.32 (.XA) X6,X5,X10
;	asl.32 (.XA) X6,X5,#1
;	mov.k.32 (.XM) X9,#0xaaaa
;	mov.kh.32 (.XM) X9,#0xaaaa
;	and.32 (.XA) X7,X5,X9
;	lsr.32 (.XA) X7,X5,#1
;	add.32 (.XA) X5,X6,X7
;	mov.k.32 (.XM) X9,#0x3333
;	mov.kh.32 (.XM) X9,#0x3333
;	and.32 (.XA) X6,X5,X9
;	asl.32 (.XA) X6,X5,#2
;	mov.k.32 (.XM) X9,#0xcccc
;	mov.kh.32 (.XM) X9,#0xcccc
;	and.32 (.XA) X7,X5,X9
;	lsr.32 (.XA) X7,X5,#2
;	add.32 (.XA) X5,X6,X7
;	mov.k.32 (.XM) X9,#0x0f0f
;	mov.kh.32 (.XM) X9,#0x0f0f
;	and.32 (.XA) X6,X5,X9
;	asl.32 (.XA) X6,X5,#4
;	mov.k.32 (.XM) X9,#0xf0f0
;	mov.kh.32 (.XM) X9,#0xf0f0
;	and.32 (.XA) X7,X5,X9
;	lsr.32 (.XA) X7,X5,#4
;	add.32 (.XA) X5,X6,X7
;	mov.k.32 (.XM) X9,#0x00ff
;	mov.kh.32 (.XM) X9,#0x00ff
;	and.32 (.XA) X6,X5,X9
;	asl.32 (.XA) X6,X5,#8
;	mov.k.32 (.XM) X9,#0xff00
;	mov.kh.32 (.XM) X9,#0xff00
;	and.32 (.XA) X7,X5,X9
;	lsr.32 (.XA) X7,X5,#8
;	add.32 (.XA) X5,X6,X7
;	lsr.32 (.XA) X5,X5,#6
;	neg.32 (.XA) X5,X5
;	add.32 (.XA) X5,X5,X8
;	st.w.32 (.XD) *-FP[X5],X4
;	sub.32 (.XA) X3,X3,#1
;	cmpg.s.32 (.XD) X0,X3,X8
;	[X0] B.32 (.XD) @l6
;
.size fft,.-fft
	
.endfunc
















