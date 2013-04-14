; Breakout v0.1 by Joe Wingbermuehle 19981111 (TI-92 Fargo II)
; Converted for use with the TI-89 19990305

	include "doorsos.h"
	include	"util.h"
	include "macros.h"

;---------= Header =---------
	xdef	_main
	xdef	_comment
	xdef	_ti89

;---------= Program =----------
_main:
resetStartLevel:
	move.w	#1,startLevel
startOver:
	clr.w	score
	move.w	#5,lives
showMenu:
	move.w	startLevel,level
	bsr	setScreen
	bsr	dispInfo
	puts	#5,#16,#4,option1
	puts	#5,#24,#4,option2
	puts	#5,#32,#4,option3
	puts	#5,#40,#4,option4
	puts	#5,#48,#4,option5

	clr.l	d0
	move.b	speed,d0
	printD0	#77,#32,#4,#1
menuLoop:
	jsr	util::idle_loop
	cmp.w	#$0110,d0
	bne.s	noQuit
	rts
noQuit:	cmp.w	#$010C,d0
	beq.s	startGame
	cmp.w	#$010D,d0
	beq.s	setStartLevel
	cmpi.w	#$010E,d0
	beq.s	setSpeed
	bra.s	menuLoop

setStartLevel:
	addq.w	#1,startLevel
	cmp.w	#7,startLevel
	bne	showMenu
	bra	resetStartLevel

setSpeed:
	addq.b	#1,speed
	cmpi.b	#5,speed
	ble	showMenu
	move.b	#1,speed
	bra	showMenu

;---------= Start of Game =---------
startGame:
	move.l	#$00000F12,-(a7)
	jsr	doorsos::HeapAlloc
	addq.l	#4,a7
	tst.w	d0
	bne.s	grayOnCont
	rts
grayOnCont:
	move.w	d0,handle
	getHan	d0,a5
	move.l	a5,d0
	addq.l	#7,d0
	lsr.l	#3,d0
	move.w	d0,table
	lsl.l	#3,d0
	move.l	d0,plane0
	move.l	#LCD_MEM,d0
	lsr.l	#3,d0
	move.w	d0,table+2
	move.w	d0,table+4
	clr.w	phase
	clr.w	vbl_phase
	move.w	#$0700,d0
	trap	#1
	move.l	$64,old_interrupt
	bclr.b	#2,$600001
	move.l	#interrupt,$64
	bset.b	#2,$600001
	trap	#1
	bra	begin
wrapLevel:
	move.w	#1,level
begin:	subq.w	#1,level
nextLevel:
	addq.w	#1,level
	cmp.w	#7,level
	beq.s	wrapLevel

;---------= Load the Level =---------
drawLevel:
	lea	level_data(pc),a0
	lea	level_matrix,a1
	clr.w	d1
	move.w	level,d0
	cmp.w	#1,d0
	beq.s	firstLevel
	subq.w	#2,d0
findLevel_l:
	addq.l	#1,a0
	move.b	(a0)+,d1
	adda.w	d1,a0
	adda.w	#1,a0
	dbra	d0,findLevel_l
firstLevel:
	clr.w	d0
	move.b	(a0)+,d0
	move.w	d0,blocksLeft

;---------= Decompress =---------
; Input:  a0->compressed data
;	  a1->location to load data
; Output: data is decompressed
decompress:
	clr.w	d0
	move.b	(a0)+,d0
decompressLoop1:
	clr.w	d1
	move.b	(a0),d1
	lsr.b	#3,d1
	move.b	(a0)+,d2
	andi.b	#%00000111,d2
decompressLoop2:
	move.b	d2,(a1)+
	dbra	d1,decompressLoop2
	dbra	d0,decompressLoop1

;---------= Draw the Level =---------
redrawTheStinkenLevel:
	bsr	setScreen
	move.l	#LCD_MEM,a1
	move.l	plane0,a0
	move.w	#750,d0
copyBuffers:
	move.l	(a1)+,(a0)+
	dbra	d0,copyBuffers
	bsr	dispInfo
	bsr	drawBall
	move.b	#3,padSize
	bsr	drawPaddle
	lea	level_matrix,a2
	clr.w	d6
drawLevelLoop1:
	clr.w	d5
drawLevelLoop2:
	move.b	(a2)+,d2
	move.w	d5,d0
	move.w	d6,d1
	bsr	drawBlock
	addq.w	#1,d5
	cmpi.w	#8,d5
	blt.s	drawLevelLoop2
	addq.w	#1,d6
	cmpi.w	#8,d6
	blt.s	drawLevelLoop1

restartLevel:
	clr.w	over
	move.b	#5,timer
	clr.b	motion

;---------= Main Game Loop =---------
game:	tst.w	blocksLeft	; check for winner/losser
	beq	nextLevel
	tst.w	over
	bne	gameOver

delay:	tst.b	timer		; delay
	bne.s	delay
	move.b	speed,timer

	move.w	#%1111111110111111,d0
	bsr	getKey
	btst	#0,d0		; [ESC]
	beq	quit

;---------= Update Game =---------
	tst.b	motion
	beq.s	notInMotion
	addq.b	#1,balld
	andi.b	#3,balld
	bne.s	notInMotion
	bsr	moveBall
	bsr	checkLocation
	bsr	fallDown
notInMotion:

;---------= Check Arrow Keys =---------
	move.w	#%1111111111111110,d0
	bsr	getKey
	btst	#3,d0
	beq	moveRight
	btst	#1,d0
	beq	moveLeft
	btst	#5,d0		; shift
	beq.s	gameOver
	btst	#4,d0		; 2nd
	bne.s	noMotion
	move.b	#1,motion
noMotion:
	btst	#6,d0		; diamond
	bne.s	noPause
	trap	#4
	move.b	#255,timer
noPause:
	bra	game

;---------= Game Over =---------
gameOver:
	bsr	drawPaddle
	bsr	drawBall
	bsr	setPos
	bsr	drawBall
	bsr	drawPaddle
	subq.w	#1,lives
	bsr	dispLives
	tst.w	lives
	bne	redrawTheStinkenLevel
quit:	move.w	#$0700,d0
	trap	#1
	bclr.b	#2,$600001
	move.l	old_interrupt,$64
	bset.b	#2,$600001
	clr.w	d0
	trap	#1
	move.w	table+2,$600010
	move.w	handle,-(a7)
	jsr	doorsos::HeapFree
	addq.l	#2,a7

	bsr	makeWindow
	setFont	#2
	move.w	highScore,d1
	move.w	score,d0
	cmp.w	d1,d0
	ble	notHighScore
	move.w	d0,highScore
	puts	#32,#44,#4,gameOver_txt
	setFont	#1
	puts	#22,#54,#4,nhs_txt
	jsr	util::idle_loop
	jsr	util::idle_loop
	bsr	makeWindow
	lea	hs_n,a6
	clr.b	d7
	puts	#31,#49,#4,initials_txt
getInitialsLoop:
	puts	#85,#49,#4,hs_n
	jsr	util::idle_loop
	cmpi.b	#13,d0
	beq	startOver
	cmpi.b	#1,d0
	bne.s	noBackSpace
	tst.b	d7
	beq.s	getInitialsLoop
	move.b	#32,-(a6)
	subq.b	#1,d7
	bra.s	getInitialsLoop
noBackSpace:
	cmpi.b	#3,d7
	beq.s	getInitialsLoop
	addq.b	#1,d7
	move.b	d0,(a6)+
	bra.s	getInitialsLoop
notHighScore:
	puts	#32,#47,#4,gameOver_txt
	setFont	#1
quitX:	jsr	util::idle_loop
	jsr	util::idle_loop
	bra	startOver


;---------= Move the Paddle =---------
moveRight:
	bsr	drawPaddle

	move.w	padx,d0
	addq.w	#1,d0
	move.w	d0,d3
	clr.w	d1
	move.b	padSize,d1
	lsl.w	#2,d1
	add.w	d1,d0
	cmpi.w	#111,d0
	beq.s	moveReturn

	move.w	d3,padx
	tst.b	motion
	bne.s	moveReturn
	bsr	drawBall
	addq.w	#1,ballx
	bsr	drawBall
	bra.s	moveReturn
moveLeft:
	bsr	drawPaddle
	move.w	padx,d0
	subq.w	#1,d0
	tst.w	d0
	beq.s	moveReturn
	move.w	d0,padx
	tst.b	motion
	bne.s	moveReturn
	bsr	drawBall
	subq.w	#1,ballx
	bsr	drawBall
moveReturn:
	bsr	drawPaddle
	bra	game

;---------= Draw the Paddle =---------
drawPaddle:
	clr.l	d0
	move.b	padSize,d0
	andi.b	#%00000110,d0
	mulu.w	#20,d0
	lea	padShort(pc),a0
	adda.w	d0,a0
	move.w	padx,d0
	moveq.w	#88,d1
	moveq.w	#4,d2
	move.l	#LCD_MEM,a1
	movem.l	d0-d2/a0-a1,-(a7)
	jsr	putSprite		; A1
	movem.l	(a7)+,d0-d2/a0-a1
	lea	10(a0),a0
	movem.l	d0-d2/a0,-(a7)
	addi.w	#16,d0
	jsr	putSprite		; B1
	movem.l	(a7)+,d0-d2/a0
	move.l	plane0,a1
	lea	10(a0),a0
	movem.l	d0-d2/a0-a1,-(a7)
	jsr	putSprite		; A2
	movem.l	(a7)+,d0-d2/a0-a1
	lea	10(a0),a0
	addi.w	#16,d0
	jmp	putSprite		; B2

;---------= Move the Ball =---------
moveBall:
	move.l	#LCD_MEM+16,a1
	move.l	plane0,a0
	adda.l	#16,a0
	move.w	#95,d0
darkenScore:
	move.l	(a1),(a0)
	lea	30(a0),a0
	lea	30(a1),a1
	dbra	d0,darkenScore
	bsr	drawBall
	move.w	ballx,d0
	add.w	dirx,d0
	move.w	d0,ballx
	cmp.w	#1,d0
	bgt.s	noChg1
	cmpi.w	#0,dirx
	bgt.s	noChg1
	neg.w	dirx
noChg1:	cmp.w	#125,d0
	blt.s	noChg2
	cmp.w	#0,dirx
	blt.s	noChg2
	neg.w	dirx
noChg2:
	move.w	bally,d0
	add.w	diry,d0
	cmp.w	#1,d0
	bgt.s	noChg3
	move.w	#1,diry
noChg3:	cmpi.w	#82,d0
	blt.s	noChg4
	move.w	#-1,diry
	move.w	ballx,d1
	addq.w	#3,d1
	move.w	padx,d2

	moveq.w	#-3,d3
	moveq.w	#5,d4
dirChangeL1:
	clr.w	d5
	move.b	padSize,d5
dirChangeL2:
	cmp.w	d1,d2
	bne.s	dirChangeS1
	move.w	d3,dirx
	bra.s	dirChangeX
dirChangeS1:
	addq.w	#1,d2
	dbra	d5,dirChangeL2

	addq.w	#1,d3
	bne.s	dirChangeS2
	addq.w	#1,d3
dirChangeS2:

	dbra	d4,dirChangeL1
	move.w	#1,over
dirChangeX:
noChg4:	move.w	d0,bally

;---------= Draw the Ball =---------
drawBall:
	move.w	ballx,d0
	move.w	bally,d1
	moveq.w	#5,d2
	lea	ball(pc),a0
	move.l	#LCD_MEM,a1
	movem.w	d0-d2,-(a7)
	jsr	putSprite
	movem.w	(a7)+,d0-d2
	move.l	plane0,a1
	jmp	putSprite

;---------= Add 1 to the Score =---------
incScore:
	subq.w	#1,blocksLeft
	addq.w	#1,score

;---------= Display Score =---------
dispScore:
	clr.l	d0
	move.w	score,d0
	printD0	#133,#8,#4,#4
	rts

;---------= Display Lives =---------
dispLives:
	clr.l	d0
	move.w	lives,d0
	printD0	#133,#40,#4,#4
	rts

;---------= Interrupt =---------
interrupt:
	tst.b	timer
	beq.s	intS1
	subq.b	#1,timer
intS1:	move.w	#$2700,sr
	addq.w	#1,vbl_phase
	andi.w	#3,vbl_phase
	bne.s	intS2
        movem.l d0/a0,-(a7)
	move.w	phase,d0
	lea	table,a0
	move.w	0(a0,d0.w),$600010
	addq.w	#2,d0
	cmpi.w	#6,d0
	bne.s	noWrap
	clr.w	d0
noWrap:	move.w	d0,phase
	movem.l (a7)+,d0/a0
intS2:	rte

;---------= Set Paddle/Ball positions =---------
setPos:	move.w	#53,padx
	move.w	#62,ballx
	move.w	#82,bally
	move.w	#1,dirx
	move.w	#-1,diry
	move.b	#1,balld
	lea	falling,a0
	moveq.w	#47,d0
setPosLoop:
	clr.b	(a0)+
	dbra	d0,setPosLoop
	rts

;---------= Draw the Basics =---------
setScreen:
	jsr	util::clr_scr
	drawBox #0,#0,#131,#93
	drawBox #130,#0,#159,#93
	drawBox #130,#16,#159,#32
	drawBox #130,#48,#159,#70
	setFont	#0
	puts	#132,#2,#4,score_txt
	puts	#132,#18,#4,level_txt
	puts	#132,#34,#4,lives_txt
	puts	#132,#50,#4,hs_txt
	puts	#132,#64,#4,name
	puts	#0,#95,#4,_comment
	setFont	#1
	rts
dispInfo:
	clr.l	d0
	move.w	highScore,d0
	printD0	#133,#56,#4,#4
	clr.l	d0
	move.w	level,d0
	printD0	#133,#24,#4,#4
	bsr	setPos
	bsr	dispScore
	bra	dispLives

;---------= Draw Block =---------
; d0.w,d1.w = x,y (matrix coordinates)
; d2.b = type
; Destroys: a0,a1,d0,d1,d2,d3,d4
drawBlock:
	move.l	plane0,a1
	mulu.w	#16,d0
	mulu.w	#9,d1
	addq.w	#2,d0
	addq.w	#2,d1
	lea	blocks(pc),a0
	ext.w	d2
	lsl.w	#5,d2
	adda.w	d2,a0
	moveq.w	#7,d2
	movem.l	d0-d2,-(a7)
	jsr	putSprite
	movem.l	(a7)+,d0-d2
	move.l	#LCD_MEM,a1

;---------= Draw Sprite =---------
; Input: a0->sprite
;	 a1->video ram
;	 d0.w,d1.w = x,y
;	 d2.w = size
; Destroyed: a0,a1,d0,d1,d2,d3,d4
putSprite:
	mulu.w	#30,d1
	ext.l	d1
	adda.l	d1,a1
	ext.l	d0
	move.l	d0,d3
	andi.l	#15,d3
	andi.l	#$FFFFFFF0,d0
	lsr.l	#3,d0
	adda.l	d0,a1
putSprite_loop1:
	clr.l	d4
	move.w	(a0)+,d4
	swap	d4
	lsr.l	d3,d4
	eor.l	d4,(a1)
	lea	30(a1),a1
	dbra	d2,putSprite_loop1
	rts

;---------= Move Down Falling Block =---------
fallDown:
	lea	falling,a0
	moveq.w	#15,d7
fallDownL1:
	move.b	(a0)+,d2
	tst.b	d2
	beq	fallDownS2
	clr.w	d0
	move.b	(a0)+,d0
	move.b	(a0)+,d1
	bsr	drawFBlock
	addq.b	#1,d1
	cmpi.b	#82,d1
	bne	fallDownS1
	subq.l	#3,a0
	clr.b	(a0)+
	lsl.w	#4,d0
	move.w	padx,d4
	addi.w	#16,d4
	cmp.w	d4,d0
	bgt	fallDownS2
	subi.w	#40,d4
	cmp.w	d4,d0
	blt	fallDownS2

;-----> Collect Bonus
	cmpi.b	#1,d2		; 1
	bne.s	type2
type1:	addi.w	#50,score
type1a:	move.l	a0,-(a7)
	bsr	dispScore
	move.l	(a7)+,a0
	bra	collectedBonus
type2:	cmpi.b	#2,d2		; 2
	bne.s	type3
	cmpi.w	#50,score
	bge.s	type2a
	move.w	#50,score
type2a:	subi.w	#50,score
	bra.s	type1a
type3:	cmpi.b	#3,d2		; 3
	bne.s	type4
	addq.w	#1,lives
type3a:	move.l	a0,-(a7)
	bsr	dispLives
	move.l	(a7)+,a0
	bra	collectedBonus
type4:	cmpi.b	#4,d2		; 4
	bne.s	type5
	cmpi.w	#1,lives
	beq	gameOver
	subq.w	#1,lives
	bra.s	type3a
type5:	cmpi.b	#5,d2		; 5
	bne.s	type6
type5a:	clr.w	blocksLeft
	bra	collectedBonus
type6:	cmpi.w	#6,d2		; 6
	bne.s	type7
	cmpi.w	#1,level
	bne.s	type6a
	move.w	#7,level
type6a:	subq.w	#2,level
	bra.s	type5a
type7:	cmpi.w	#7,d2		; 7
	bne.s	type8
	movem.l	d2/a0,-(a7)
	bsr	drawPaddle
	movem.l	(a7)+,d2/a0
	cmpi.b	#1,padSize
	beq.s	type7a
	subq.b	#2,padSize
type7a:	movem.l	d2/a0,-(a7)
	bsr	drawPaddle
	movem.l	(a7)+,d2/a0
	bra	collectedBonus
type8:				; 8
	movem.l	d2/a0,-(a7)
	bsr	drawPaddle
	movem.l	(a7)+,d2/a0
	cmpi.b	#5,padSize
	beq.s	type7a
	addq.b	#2,padSize
	bra.s	type7a
;<-----

fallDownS1:
	move.b	d1,-(a0)
	bsr.s	drawFBlock
	subq.l	#1,a0
collectedBonus:
fallDownS2:
	addq.l	#2,a0
fallDownS3:
	dbra	d7,fallDownL1
	rts

;---------= Draw Falling Block =---------
; Input:  d0.b,d1.b = x,y (screen coordinates)
;	  d2.b = type
drawFBlock:
	movem.l	d0-d2/a0,-(a7)
	lea	fblocks(pc),a0
	subq.b	#1,d2
	ext.w	d2
	lsl.w	#4,d2
	adda.w	d2,a0
	ext.w	d0
	lsl.w	#4,d0
	ext.w	d1
	move.w	#7,d2
	move.l	#LCD_MEM,a1
	movem.l	d0-d2/a0,-(a7)
	jsr	putSprite
	movem.l	(a7)+,d0-d2/a0
	move.l	plane0,a1
	jsr	putSprite
	movem.l	(a7)+,d0-d2/a0
	rts

;---------= Check if a Block is Hit =---------
checkLocation:
	move.w	bally,d1
	subq.w	#1,d1
	moveq.w	#1,d3
checkLocation_loop2:
	moveq.w	#1,d2
	move.w	ballx,d0
checkLocation_loop1:
	movem.w	d0-d3,-(a7)
	lsr.w	#4,d0
	cmpi.w	#7,d0
	bhi	checkLocation_exit
	move.b	d0,d6
	ext.l	d1
	divu	#9,d1
	cmp.w	#7,d1
	bhi	checkLocation_exit
	move.l	d1,d7
	lea	level_matrix,a0
	move.w	d1,d2
	lsl.w	#3,d2
	adda.w	d2,a0
	adda.w	d0,a0
	move.b	(a0),d2
	tst.b	d2
	beq	checkLocation_exit
	swap	d7
	andi.b	#7,d7
	beq.s	changey
	neg.w	dirx
	bra.s	changex
changey:
	neg.w	diry
changex:
	swap	d7
	cmp.b	#7,d2
	beq	checkLocation_end
	movem.l	d0-d2/a0,-(a7)
	bsr	drawBlock
	movem.l	(a7)+,d0-d2/a0
	cmp.b	#3,d2
	ble.s	removeBlock
	subq.b	#3,d2
	move.b	d2,(a0)
	bsr	drawBlock
	bra.s	keepBlock
removeBlock:
	clr.b	(a0)
keepBlock:
	bsr	incScore
	moveq.w	#4,d0			; 1 in 4 chance of falling block
	jsr	util::random
	tst.w	d0
	bne.s	startFalling_failed
	lea	falling-3,a0
	moveq.w	#15,d0
startFalling:
	addq.l	#3,a0
	tst.b	(a0)
	dbeq.s	d0,startFalling
	cmpi.b	#255,d0
	beq.s	startFalling_failed
	moveq.w	#8,d0			; 8 types
	jsr	util::random
	addq.b	#1,d0
	move.b	d0,(a0)+
	move.b	d0,d2
	mulu.b	#9,d7
	move.b	d6,(a0)+
	move.b	d7,(a0)
	move.b	d6,d0
	move.b	d7,d1
	bsr	drawFBlock
startFalling_failed:
checkLocation_end:
	addq.l	#8,a7
	rts
checkLocation_exit:
	movem.w	(a7)+,d0-d3
	addq.w	#5,d0
	dbra	d2,checkLocation_loop1
	addq.w	#5,d1
	dbra	d3,checkLocation_loop2
	rts

;---------= Convert D0 to String =---------
convD0:	lea	strend,a0
	clr.b	(a0)
convD0Loop1:
	divu	#10,d0
	move.l	d0,d2
	swap	d2
	addi.b	#48,d2
	move.b	d2,-(a0)
	subq.w	#1,d1
	andi.l	#$0000FFFF,d0
	bne.s	convD0Loop1
	tst.w	d1
	beq.s	convD0X
	subq.w	#1,d1
convD0Fill:
	move.b	#'0',-(a0)
	dbra.s	d1,convD0Fill
convD0X:
	rts

;---------= Check for a Key =---------
getKey:	move.w	d0,$600018
	moveq.w	#10,d0
getKeyLoop:
	nop
	dbra.s	d0,getKeyLoop
	move.b	$60001B,d0
	rts

;---------= Make a Window =---------
makeWindow:
	move.l	#LCD_MEM+41*30+2,a0
	moveq.w	#22,d1
makeWinLoop1:
	moveq.w	#12,d0
makeWinLoop2:
	clr.b	(a0)+
	dbra.s	d0,makeWinLoop2
	lea	17(a0),a0
	dbra.s	d1,makeWinLoop1
	drawBox	#15,#41,#119,#63
	drawBox	#16,#42,#118,#62
	rts

;====================> Data <====================
;---------= Static Variables =---------
highScore:	dc.w	0	; high score is written back
speed:		dc.b	3

;---------= Sprites =---------
;-----> Ball (2-level grayscale)
ball:	dc.w	%0011000000000000
	dc.w	%0111100000000000
	dc.w	%1111110000000000
	dc.w	%1111110000000000
	dc.w	%0111100000000000
	dc.w	%0011000000000000
	dc.w	%0011000000000000
	dc.w	%0111100000000000
	dc.w	%1111110000000000
	dc.w	%1111110000000000
	dc.w	%0111100000000000
	dc.w	%0011000000000000


;-----> Paddle (2-level grayscale)
; short paddle
padShort:
	dc.w	%0111111111111110
	dc.w	%1000010110100011
	dc.w	%1000101001010001
	dc.w	%1000010110100011
	dc.w	%0111111111111110
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0111111111111110
	dc.w	%1010000110000101
	dc.w	%1100001001000011
	dc.w	%1010000110000101
	dc.w	%0111111111111110
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
padNorm:
	dc.w	%0111111111111111
	dc.w	%1000001001011010
	dc.w	%1000010010100101
	dc.w	%1000001001011010
	dc.w	%0111111111111111
	dc.w	%1111111000000000
	dc.w	%0100000100000000
	dc.w	%0010000100000000
	dc.w	%0100000100000000
	dc.w	%1111111000000000
	dc.w	%0111111111111111
	dc.w	%1010000000011000
	dc.w	%1100000000100100
	dc.w	%1010000000011000
	dc.w	%0111111111111111
	dc.w	%1111111000000000
	dc.w	%0000010100000000
	dc.w	%0000001100000000
	dc.w	%0000010100000000
	dc.w	%1111111000000000

; long paddle
padLong:
	dc.w	%0111111111111111
	dc.w	%1000000010001001
	dc.w	%1000000100010010
	dc.w	%1000000010001001
	dc.w	%0111111111111111
	dc.w	%1111111111111110
	dc.w	%1001000100000001
	dc.w	%0100100010000001
	dc.w	%1001000100000001
	dc.w	%1111111111111110
	dc.w	%0111111111111111
	dc.w	%1001000000000001
	dc.w	%1010000000000001
	dc.w	%1001000000000001
	dc.w	%0111111111111111
	dc.w	%1111111111111110
	dc.w	%1000000000001001
	dc.w	%0100000000000101
	dc.w	%1000000000001001
	dc.w	%1111111111111110

;-----> Blocks (2 level grayscale)
blocks:
	dc.w	0,0,0,0,0,0,0,0		; 0
	dc.w	0,0,0,0,0,0,0,0

	dc.w	%0111111111111100	; 1 - 3d (1 hit)
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000001010
	dc.w	%1000111111111010
	dc.w	%1000000000000010
	dc.w	%0111111111111100
	dc.w	%0111111111111100
	dc.w	%1000000000000010
	dc.w	%1011111111111010
	dc.w	%1010000000000010
	dc.w	%1010000000000010
	dc.w	%1010000000000010
	dc.w	%1000000000000010
	dc.w	%0111111111111100

	dc.w	%0111111111111100	; 2 - plain (1 hit)
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%0111111111111100
	dc.w	%0111111111111100
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%0111111111111100

	dc.w	%0111111111111100	; 3 - darker plain (1 hit)
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0111111111111100
	dc.w	%0111111111111100
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%0111111111111100

	dc.w	%0111111111111100	; 4 - 3d (2 hit)
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000001010
	dc.w	%1000111111111010
	dc.w	%1000000000000010
	dc.w	%0111111111111100
	dc.w	%0111111111111100
	dc.w	%1111111111111110
	dc.w	%1100000000000110
	dc.w	%1101111111111110
	dc.w	%1101111111110110
	dc.w	%1101000000000110
	dc.w	%1111111111111110
	dc.w	%0111111111111100

	dc.w	%0111111111111100	; 5 - plain (2 hit)
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%1000000000000010
	dc.w	%0111111111111100
	dc.w	%0111111111111100
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0111111111111100

	dc.w	%0111111111111100	; 6 - darker plain (2 hit)
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0111111111111100
	dc.w	%0111111111111100
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0111111111111100

	dc.w	%0111111111111100	; 7, not breakable
	dc.w	%1111111111111110
	dc.w	%1111100000111110
	dc.w	%1111000100011110
	dc.w	%1111000100011110
	dc.w	%1111100000111110
	dc.w	%1111111111111110
	dc.w	%0111111111111100
	dc.w	%0111111111111100
	dc.w	%1111111111111110
	dc.w	%1111111011111110
	dc.w	%1111110001111110
	dc.w	%1111110001111110
	dc.w	%1111111011111110
	dc.w	%1111111111111110
	dc.w	%0111111111111100

;-----> Falling Blocks (monochrome)
fblocks:
	dc.w	%0000000000000000	; 1
	dc.w	%0010001111001110
	dc.w	%0010001000010001
	dc.w	%1111101110010101
	dc.w	%0010000001010101
	dc.w	%0010001001010001
	dc.w	%0000000110001110
	dc.w	%0000000000000000
	dc.w	%0000000000000000	; 2
	dc.w	%0000001111001110
	dc.w	%0000001000010001
	dc.w	%1111101110010101
	dc.w	%0000000001010101
	dc.w	%0000001001010001
	dc.w	%0000000110001110
	dc.w	%0000000000000000
	dc.w	%0001000000000000	; 3
	dc.w	%0001000011001100
	dc.w	%0111110111111110
	dc.w	%0001000111111110
	dc.w	%0001000011111100
	dc.w	%0000000001111000
	dc.w	%0000000000110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000	; 4
	dc.w	%0000000011001100
	dc.w	%0111110111111110
	dc.w	%0000000111111110
	dc.w	%0000000011111100
	dc.w	%0000000001111000
	dc.w	%0000000000110000
	dc.w	%0000000000000000
	dc.w	%0010000000000000	; 5
	dc.w	%0010001000000000
	dc.w	%1111101000000000
	dc.w	%0010001000110101
	dc.w	%0010001001110101
	dc.w	%0000001000110010
	dc.w	%0000001111000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000	; 6
	dc.w	%0000001000000000
	dc.w	%1111101000000000
	dc.w	%0000001000110101
	dc.w	%0000001001110101
	dc.w	%0000001000110010
	dc.w	%0000001111000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000	; 7
	dc.w	%1100000000000110
	dc.w	%0110000000001100
	dc.w	%0011111111111000
	dc.w	%0011111111111000
	dc.w	%0110000000001100
	dc.w	%1100000000000110
	dc.w	%0000000000000000

	dc.w	%0000000000000000	; 8
	dc.w	%0011000000011000
	dc.w	%0110000000001100
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0110000000001100
	dc.w	%0011000000011000
	dc.w	%0000000000000000


;---------= Levels =---------
; Level data format:
;  There are 122 bytes in an uncompressed level. A compressed level
;  will be a maximum of 122 bytes but will most likely be less than
;  half that size.
;  -First byte = number block hits required to beat the level
;  -Second byte = number of bytes of compressed data minus one
;  -The rest of the data is level data. It is set up as follows:
;	-Each byte specifies a run of data.
;		%xxxxxyyy = repeat value yyy xxxxx+1 times.
;		Levels do not need to be compressed to work.
level_data:
level1:	dc.b	24,2
	dc.b	%01111000,%10111001,%10111000
level2:	dc.b	31,28
	dc.b	8,10,8,19,8,10,8,19,8,10,16,11,8,10,16,11,8
	dc.b	18,8,11,16,10,8,11,16,10,8,11,0
level3:	dc.b	28,56
	dc.b	1,0,1,0,1,0,1,0
	dc.b	0,1,0,1,0,1,0,1
	dc.b	1,0,1,0,1,0,1,0
	dc.b	0,1,0,1,0,1,0,1
	dc.b	1,0,1,0,1,0,1,0
	dc.b	0,1,0,1,0,1,0,1
	dc.b	1,0,1,0,1,0,1,0
	dc.b	%00111000
level4:	dc.b	44,27
	dc.b	%00010011,%00001000,%00011011,0,3,%00001000,3,0,%00001011,0
	dc.b	%00011011,0,%00001011,%00101000,%00001011,%00001000,%00001100
	dc.b	%00001000,%00001011,%00101000,%00010011,1,%00001000,1,%00001011
	dc.b	%00010110,%00001000,%00010110
level5:	dc.b	44,56
	dc.b	0,0,0,0,0,0,0,0
	dc.b	0,6,6,6,6,6,6,0
	dc.b	0,6,0,0,0,0,6,0
	dc.b	0,6,0,3,3,0,6,0
	dc.b	0,6,0,3,3,0,6,0
	dc.b	0,6,0,0,0,0,6,0
	dc.b	0,6,6,6,6,6,6,0
	dc.b	%00111000
level6:	dc.b	24,56
	dc.b	2,0,0,0,0,0,0,2
	dc.b	0,2,0,0,0,0,2,0
	dc.b	0,0,2,0,0,2,0,0
	dc.b	0,0,0,3,3,0,0,0
	dc.b	0,0,0,0,0,0,0,0
	dc.b	0,0,0,0,0,0,0,0
	dc.b	0,0,7,7,7,7,0,0
	dc.b	%00111101

;---------= Dialog =---------
_comment:	dc.b	"Breakout v0.1 by Joe Wingbermuehle",0
score_txt:	dc.b	"Score:",0
level_txt:	dc.b	"Level:",0
lives_txt:	dc.b	"Lives:",0
gameOver_txt:	dc.b	"Game Over",0
nhs_txt:	dc.b	"New High Score!",0
initials_txt:	dc.b	"Initials:",0
hs_txt:		dc.b	"HiScore",0
name:		dc.b	"by "
hs_n:		dc.b	"JGW",0
option1:	dc.b	"F1 - Start Game",0
option2:	dc.b	"F2 - Set Level",0
option3:	dc.b	"F3 - Speed:",0
option4:	dc.b	"F4 - Standard Levels",0
option5:	dc.b	"F5 - Exit",0

;---------= Variables =---------
	bss

old_interrupt:	ds.l	1
plane0:		ds.l	1
handle:		ds.w	1
vbl_phase:	ds.w	1
phase:		ds.w	1
table:		ds.w	3
startLevel:	ds.w	1
ballx:		ds.w	1
bally:		ds.w	1
padx:		ds.w	1
dirx:		ds.w	1
diry:		ds.w	1
over:		ds.w	1
score:		ds.w	1
level:		ds.w	1
lives:		ds.w	1
blocksLeft:	ds.w	1
padSize:	ds.b	1	; size of the paddle (width/4-1={1,3,5})
timer:		ds.b	1
balld:		ds.b	1
motion:		ds.b	1
str:		ds.b	16
strend:		ds.b	1
falling:	ds.b	48	; 16x {t,x,y}
level_matrix:	ds.b	64

	end
; Breakout by Joe Wingbermuehle