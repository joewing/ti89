;+---------------------------------------------+
;| Diamonds v0.5 by Joe Wingbermuehle 19990301 |
;+---------------------------------------------+

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;-= Definitions =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;---------= Includes =---------
	include "doorsos.h"
	include	"util.h"

;---------= Header Information =---------
	xdef _main
	xdef _comment
	xdef _ti89

;---------= Macros =---------
;-----> Get File Handle
getHan	macro
	lsl.w	#2,\1
        move.l	doorsos::Heap,\2
	move.l	0(\2,\1.w),\2
	endm

;-----> Print D0
printD0	macro
	move.w	\4,d1
	bsr	convD0
	move.w	\3,-(a7)
	move.l	a0,-(a7)
	move.w	\2,-(a7)
	move.w	\1,-(a7)
	jsr	doorsos::DrawStrXY
	lea	10(a7),a7
	endm

;-----> Write a String
puts	macro
	move.w	\3,-(a7)
	pea	\4
	move.w	\2,-(a7)
	move.w	\1,-(a7)
	jsr	doorsos::DrawStrXY
	lea	10(a7),a7
	ENDM

;-----> Write a String Ax
puta	macro
	move.w	\3,-(a7)
	move.l	\4,-(a7)
	move.w	\2,-(a7)
	move.w	\1,-(a7)
	jsr	doorsos::DrawStrXY
	lea	10(a7),a7
	ENDM

;-----> Draw a Rectangle
drawBox	macro
	move.w	\4,-(a7)
	move.w	\3,-(a7)
	move.w	\2,-(a7)
	move.w	\1,-(a7)
	jsr	util::frame_rect
	addq.l	#8,a7
	endm

;-----> Set Font
setFont	macro
	move.w	\1,-(a7)
	jsr	doorsos::FontSetSys
	addq.l	#2,a7
	endm

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;-= Code -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
_main:	clr.w	fileNumber
	bsr	findFile
	tst.b	d0
	bne.s	fileFound
	rts
fileFound:
	move.b	#1,startLevel

;---------= Begin a Game =---------
menu:	clr.l	score
	clr.w	lifeCount
	move.b	#5,lives
	move.b	#61,bonus
	move.b	#2,btype
	move.b	#1,level
	jsr	util::zap_screen
	bsr	drawFrames
	puts	#8,#24,#4,help1
	puts	#8,#32,#4,help2
	puts	#8,#40,#4,help3
	puts	#8,#48,#4,help4
	puts	#8,#56,#4,help5
	bsr	findData
	addq.l	#8,a0
	puta	#38,#48,#4,a0
	clr.l	d0
	move.b	speed,d0
	subq.b	#2,d0
	printD0	#79,#40,#4,#1
	clr.l	d0
	move.b	startLevel,d0
	printD0	#73,#32,#4,#2
	puts	#1,#85,#4,high_txt
	bsr	findData
	move.l	(a0)+,d0
	move.l	a0,-(a7)
	printD0	#66,#85,#4,#5
	move.l	(a7)+,a0
	puta	#102,#85,#4,a0
menuLoop:
	jsr	util::idle_loop
	cmpi.w	#272,d0
	beq	exit
	cmpi.w	#337,d0
	beq.s	incStart
	cmpi.w	#340,d0
	beq.s	decStart
	cmpi.w	#269,d0
	beq.s	incSpeed
	cmpi.w	#268,d0
	beq.s	startGame
	cmpi.w	#270,d0
	beq.s	changeLevel
	bra.s	menuLoop

incStart:
	addq.b	#1,startLevel
	move.b	numLevels,d0
	cmp.b	startLevel,d0
	bge.s	rMenu
	move.b	#1,startLevel
	bra.s	rMenu

decStart:
	subq.b	#1,startLevel
	bne.s	rMenu
	move.b	numLevels,startLevel
rMenu:	bra	menu

incSpeed:
	addq.b	#1,speed
	cmpi.b	#8,speed
	bne.s	rMenu
	move.b	#3,speed
	bra.s	rMenu

changeLevel:
	move.b	#1,startLevel
	bsr	findFile
	tst.b	d0
	bne.s	rMenu
	clr.w	fileNumber
	bra.s	changeLevel

;---------= Start the Game =----------
startGame:
	move.w	#$0700,d0
	trap	#1
	move.l	$64,old_interrupt
	bclr.b	#2,$600001
	move.l	#interrupt,$64
	bset.b	#2,$600001
	trap	#1
	move.b	startLevel,level
	bra.s	loadLevel
startFirst:
	clr.b	level
nextLevel:
	addq.b	#1,level
	move.b	numLevels,d0
	cmp.b	level,d0
	blt.s	startFirst
;---------= Load a Level =----------
loadLevel:
	bsr	findData
	addq.l	#8,a0
findLevelLoop:
	tst.b	(a0)+
	bne.s	findLevelLoop
	suba.w	#44,a0
	move.b	level,d0
	ext.w	d0
	mulu.w	#44,d0
	adda.w	d0,a0
	lea	board,a1
	moveq.w	#39,d0
loadLevelLoop:
	move.b	(a0),d1
	andi.b	#$F0,d1
	lsr.b	#4,d1
	move.b	d1,(a1)+
	move.b	(a0)+,d1
	andi.b	#$0F,d1
	move.b	d1,(a1)+
	dbra.s	d0,loadLevelLoop
	move.b	(a0)+,levelData1
	move.b	(a0)+,levelData2
	move.b	(a0)+,fblocks
	move.b	(a0),lblocks
	move.b	#61,bonus
	clr.b	btype
	clr.b	key

;---------= Start a Level =---------
restartLevel:
	jsr	util::zap_screen
	clr.w	d0
	move.b	levelData1,d0
	move.w	d0,ballx
	move.b	levelData2,d0
	move.w	d0,bally
	clr.b	over
	clr.b	reverseArrows
	move.b	#1,balld
	cmpi.b	#3,btype
	beq.s	noBlockReset
	move.b	#2,btype
noBlockReset:

;---------= Draw the board =---------
	lea	board,a6
	move.w	#1,d1
	move.w	#9,d4
drawBoardLoop1:
	move.w	#1,d0
	move.w	#7,d3
drawBoardLoop2:
	movem.w	d0-d4,-(a7)
	clr.w	d2
	move.b	(a6)+,d2
	lsl.w	#4,d2
	lea	blocks(pc),a0
	adda.w	d2,a0
	move.w	#7,d2
	bsr	putSprite
	movem.w	(a7)+,d0-d4
	addi.w	#16,d0
	dbra.s	d3,drawBoardLoop2
	addq.w	#8,d1
	dbra.s	d4,drawBoardLoop1
	bsr	drawBall
	bsr	drawFrames

;---------= Main Game Loop =---------
game:	tst.b	timer
	bne.s	game
	move.b	speed,timer
	subq.b	#1,bonusDelay
	bne.s	bonusSkip
	bsr	decBonus
bonusSkip:
	tst.b	lblocks
	beq	levelComplete
	tst.b	over
	bne.s	takeLife
	move.w	#%1111111111111110,d0
	bsr	getKey
	btst	#3,d0
	beq	moveRight
	btst	#1,d0
	beq	moveLeft
	btst	#5,d0
	beq.s	takeLife
	btst	#4,d0
	bne.s	continueMove
	trap	#4
continueMove:
	bsr	moveBall
	move.w	#%1111111110111111,d0
	bsr	getKey
	btst	#0,d0		; [ESC]
	bne.s	game
	bra.s	quit

;---------= Game Over =---------
takeLife:
	subq.b	#1,lives
	bne	restartLevel
quit:	move.w	#$0700,d0
	trap	#1
	bclr.b	#2,$600001
	move.l	old_interrupt,$64
	bset.b	#2,$600001
	trap	#1
	bsr	makeWindow
	setFont	#2
	bsr	findData
	move.l	(a0),d1
	move.l	score,d0
	cmp.l	d1,d0
	ble	notHighScore
	puts	#32,#44,#4,gameOver_txt
	setFont	#1
	puts	#22,#54,#4,nhs_txt
	jsr	util::idle_loop
	jsr	util::idle_loop
	bsr	makeWindow
	bsr	findData
	move.l	a0,a6
	move.l	score,(a6)+
	clr.b	d7
	puts	#31,#49,#4,initials_txt
getInitialsLoop:
	bsr	findData
	addq.l	#4,a0
	puta	#85,#49,#4,a0
	jsr	util::idle_loop
	cmpi.b	#13,d0
	beq	menu
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
	bra	menu
;-----> Exit the Game
exit:	rts

;---------= Level is Complete =---------
levelComplete:
	bsr	makeWindow
	setFont	#2
	puts	#44,#47,#4,bonus_txt
	clr.w	d7
	move.b	bonus,d7
	subq.b	#1,d7
	bmi.s	skipBonus
	setFont	#1
bonusLoop:
	tst.b	timer
	bne.s	bonusLoop
	move.b	#20,timer
	bsr	decBonus
	bsr	incScore
	dbra	d7,bonusLoop
	bra.s	bonusPause
skipBonus:
	puts	#32,#47,#4,noBonus_txt
	setFont	#1
bonusPause:
	move.b	#255,timer
bonusPauseLoop:
	tst.b	timer
	bne.s	bonusPauseLoop
	bra	nextLevel

;---------= Move the Ball Right =---------
moveRight:
	tst.b	reverseArrows
	bne.s	mLeftR
mRightR:
	bsr	drawBall
	cmpi.w	#122,ballx
	bge.s	moveDone
	addi.w	#1,ballx
	moveq.b	#1,d6
	bsr	checkBlock
	bsr.s	checkMove
	beq.s	moveDone
	subq.w	#1,ballx
moveDone:
	bsr	drawBall
	bra	continueMove

;---------= Move the Ball Left =---------
moveLeft:
	tst.b	reverseArrows
	bne.s	mRightR
mLeftR:	bsr	drawBall
	cmpi.w	#1,ballx
	ble.s	moveDone
	subq.w	#1,ballx
	moveq.b	#1,d6
	bsr	checkBlock
	bsr.s	checkMove
	beq.s	moveDone
	addq.w	#1,ballx
	bra.s	moveDone

;---------= Check if Ball can be Moved =---------
; Output: d5=result
checkMove:
	move.w	ballx,d0
	move.w	bally,d1
	clr.b	d7
	movem.w	d0-d1,-(a7)
	bsr	getElement
	or.b	d5,d7
	movem.w	(a7)+,d0-d1
	addq.b	#3,d0
	movem.w	d0-d1,-(a7)
	bsr	getElement
	or.b	d5,d7
	movem.w	(a7)+,d0-d1
	addq.b	#3,d1
	movem.w	d0-d1,-(a7)
	bsr	getElement
	or.b	d5,d7
	movem.w	(a7)+,d0-d1
	subq.b	#3,d0
	bsr	getElement
	or.b	d7,d5
	rts

;---------= Move the Ball =---------
moveBall:
	bsr	drawBall
	move.b	balld,d0
	ext.w	d0
	add.w	d0,bally
	cmpi.w	#74,bally
	blt.s	moveBallS1
	move.b	#-1,balld
moveBallS1:
	cmpi.w	#1,bally
	bgt.s	moveBallS2
	move.b	#1,balld
moveBallS2:
	clr.b	d6
	bsr.s	checkBlock
	bra	drawBall

;---------= Test for Collision with Block =---------
checkBlock:
	move.w	ballx,d0
	move.w	bally,d1
	bsr.s	testCollision
	addq.w	#3,d0
	bsr.s	testCollision
	addq.w	#3,d1
	bsr.s	testCollision
	subq.w	#3,d0
testCollision:
	movem.w	d0-d1,-(a7)
	bsr	getElement
	tst.b	d5
	beq	testCollisionX
	tst.b	d6
	bne.s	testCollisionS1
	neg.b	balld
	moveq.b	#1,d6
testCollisionS1:
	cmpi.b	#$06,d5
	bne.s	notLock
	tst.b	key
	beq.s	notLock
	clr.b	key
	bra.s	haveKey
notLock:
	cmpi.b	#$05,d5
	bne.s	notKey
	tst.b	key
	bne.s	notKey
	move.b	#1,key
	bra.s	haveKey
notKey:	cmpi.b	#7,d5
	bne.s	notSpecial
	eori.b	#1,reverseArrows
	bra.s	haveKey
notSpecial:
	move.b	btype,d4
	cmp.b	d5,d4
	bne.s	testCollisionX
haveKey:
	move.l	a0,-(a7)
	clr.w	d3
	move.b	(a0),d3
	lsl.w	#4,d3
	lsl.w	#4,d0
	lea	blocks(pc),a0
	adda.w	d3,a0
	move.w	#7,d2
	addq.w	#1,d0
	addq.w	#1,d1
	bsr	putSprite
	move.l	(a7)+,a0
	cmpi.b	#$03,(a0)
	bne.s	subFBlock
	subq.b	#1,lblocks
	bra.s	subBlock
subFBlock:
	subq.b	#1,fblocks
	bne.s	subBlock
	clr.b	(a0)
	bsr.s	showBlock
	move.b	#$03,btype
	bsr.s	showBlock
	bra.s	noChangeX
subBlock:
	clr.b	(a0)
	move.l	a0,-(a7)
	bsr.s	incScore
	move.l	(a7)+,a0
testCollisionX:
	move.b	(a0),d0
	cmpi.b	#$04,d0
	bne.s	noDeath
	move.b	#1,over
noDeath:
	cmpi.b	#$03,btype
	beq.s	noChangeX
	btst.b	#3,d0
	beq.s	noChangeX
	bclr.b	#2,d0
	beq.s	noChangeX
	move.w	d0,-(a7)
	bsr.s	showBlock
	move.w	(a7)+,d0
	move.b	d0,btype
	bsr.s	showBlock
noChangeX:
	movem.w	(a7)+,d0-d1
	rts

;---------= Show Current Block Type =---------
showBlock:
	clr.w	d0
	move.b	btype,d0
	mulu.w	#16,d0
	lea	blocks(pc),a0
	adda.w	d0,a0
	move.w	#137,d0
	moveq.w	#68,d1
	moveq.w	#7,d2
	bra	putSprite

;---------= Increment the Score =---------
incScore:
	addq.l	#1,score
	addq.w	#1,lifeCount

;---------= Display the Score =---------
dispScore:
	move.l	score,d0
	moveq.w	#3,d1
	cmpi.l	#100,d0
	blt.s	dispScoreS1
	addq.w	#1,d1
	cmpi.l	#10000,d0
	blt.s	dispScoreS1
	addq.w	#1,d1
dispScoreS1:
	printD0	#130,#58,#4,d1
	cmpi.w	#400,lifeCount
	blt.s	dispScoreX
	clr.w	lifeCount
	cmpi.b	#5,lives
	beq.s	dispScoreX
	bsr.s	dispLives
	addq.b	#1,lives
	bra.s	dispLives
dispScoreX:
	rts

;-----> Display Life Sprites
dispLives:
	move.b	lives,d1
	subq.b	#1,d1
	ext.w	d1
	move.w	#130,d0
displayLivesLoop2:
	movem.w	d0-d1,-(a7)
	lea	ball(pc),a0
	moveq.w	#56,d1
	moveq.w	#5,d2
	bsr	putSprite
	movem.w	(a7)+,d0-d1
	addq.w	#6,d0
	dbra.s	d1,displayLivesLoop2
	rts

;---------= Decrement the Bonus =---------
decBonus:
	move.b	speed,d0
	neg.b	d0
	addi.b	#10,d0
	lsl.b	#4,d0
	move.b	d0,bonusDelay
	tst.b	bonus
	beq.s	decBonusX
	subq.b	#1,bonus
decBonusX:
	clr.l	d0
	move.b	bonus,d0
	printD0	#130,#22,#4,#3
	rts

;---------= Get Matrix Element =---------
; Input: d0=x,d1=y
; Output: a0->element, d5=element
getElement:
	andi.w	#%1111111111111000,d1
	lsr.w	#4,d0
	move.w	d1,d2
	add.w	d0,d2
	ext.l	d2
	lea	board,a0
	adda.l	d2,a0
	move.b	(a0),d5
	rts

;---------= Draw the Ball =---------
drawBall:
	lea	ball(pc),a0
	move.w	ballx,d0
	move.w	bally,d1
	moveq.w	#5,d2

;---------= Draw Sprite =---------
; Input: a0->sprite
;	 a1->video buffer
;	 d0.w,d1.w = x,y
;	 d2.w = size
; Destroyed: a0,a1,d0,d1,d2,d3,d4
putSprite:
	addi.w	#12,d1
	move.w	#LCD_MEM,a1
	mulu.w	#30,d1
	adda.w	d1,a1
	move.w	d0,d3
	andi.w	#$000F,d3
	andi.w	#$FFF0,d0
	lsr.w	#3,d0
	adda.w	d0,a1
putSpriteLoop:
	clr.l	d4
	move.w	(a0)+,d4
	swap	d4
	lsr.l	d3,d4
	eor.l	d4,(a1)
	lea	30(a1),a1
	dbra.s	d2,putSpriteLoop
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
	move.b	#32,-(a0)
	dbra.s	d1,convD0Fill
convD0X:
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

;---------= Draw Screen Basics =---------
drawFrames:
	setFont	#2
	clr.b	author
	puts	#28,#2,#4,_comment
	move.b	#32,author
	bsr	showBlock
	drawBox	#0,#12,#129,#93
	drawBox	#129,#12,#160,#30
	drawBox	#129,#48,#160,#66
	drawBox	#129,#75,#160,#93
	setFont	#1
	puts	#130,#14,#4,bonus_txt
	puts	#130,#32,#4,level_txt
	clr.l	d0
	move.b	level,d0
	printD0	#130,#40,#4,#3
	puts	#130,#50,#4,score_txt
	bsr	dispScore
	bsr	dispLives
	move.b	#255,timer
	bsr	decBonus
	setFont	#0
	puts	#0,#95,#4,_comment
	setFont	#1
	rts

;---------= Check for a Key =---------
getKey:	move.w	d0,$600018
	moveq.w	#10,d0
getKeyLoop:
	nop
	dbra.s	d0,getKeyLoop
	move.b	$60001B,d0
	rts

;---------= Timer Interrupt =---------
interrupt:
	tst.b	timer
	beq.s	interrupt_skip
	subq.b	#1,timer
interrupt_skip:
	rte

;---------= Find a File =---------
; Input:  fileNumber.b = current file number
; Output: fileNumber.b
;	  fileHandle.l 
;	  d0.b=1 if success, else d0.b=0
findFile:
	move.w	#$0009,d0
	getHan	d0,a0
	addq.w	#2,a0
	move.w	(a0)+,d7
	clr.l	d0
	move.w	fileNumber,d0
	mulu	#14,d0
	adda.l	d0,a0
	subq.w	#1,d7
	sub.w	fileNumber,d7
	bmi.s	findFileNone
findFileLoop:
	addq.w	#1,fileNumber
	lea	12(a0),a0
	move.w	(a0)+,d0
	getHan	d0,a2
	adda.l	#98,a2
	lea	detectString(pc),a3
	move.w	#5,d0
findFileLoop2:
	move.b	(a2)+,d2
	cmp.b	(a3)+,d2
	bne.s	findFileSkip
	dbra.s	d0,findFileLoop2
	move.l	a2,fileHandle
	addq.l	#1,a2
	move.b	(a2),numLevels
	move.b	#1,d0
	rts
findFileSkip:
	dbra.s	d7,findFileLoop
findFileNone:
	clr.b	d0
	rts

findData:
	move.l	fileHandle,a0
	addq.l	#2,a0
	rts

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;-= Data -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;---------= Sprites =---------
ball:	dc.w	%0011000000000000
	dc.w	%0111100000000000
	dc.w	%1111110000000000
	dc.w	%1111110000000000
	dc.w	%0111100000000000
	dc.w	%0011000000000000
blocks:	dc.w	%0000000000000000	; type 0 - empty
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0111111111111110	; type 1 - blocker
	dc.w	%1111111111111111
	dc.w	%1110111011101111
	dc.w	%1111111110111111
	dc.w	%1011110111111011
	dc.w	%1111011111011111
	dc.w	%1111111111111111
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type 2 - first
	dc.w	%1000000000000001
	dc.w	%1000000000000001
	dc.w	%1000000000000001
	dc.w	%1000000000000001
	dc.w	%1000000000000001
	dc.w	%1000000000000001
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type 3 - last
	dc.w	%1111111111111111
	dc.w	%1111100000011111
	dc.w	%1111000000001111
	dc.w	%1111000000001111
	dc.w	%1111100000011111
	dc.w	%1111111111111111
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type 4 - killer
	dc.w	%1111001111111111
	dc.w	%1111001111111111
	dc.w	%1000000000000001
	dc.w	%1000000000000001
	dc.w	%1111001111111111
	dc.w	%1111001111111111
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type 5 - key
	dc.w	%1111111111111111
	dc.w	%1111111111100111
	dc.w	%1100000000011011
	dc.w	%1110011111011011
	dc.w	%1110011111100111
	dc.w	%1111111111111111
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type 6 - lock
	dc.w	%1111110000111111
	dc.w	%1111100110011111
	dc.w	%1111101111011111
	dc.w	%1111100000011111
	dc.w	%1111100000011111
	dc.w	%1111100000011111
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type 7 - special
	dc.w	%1111111111111111
	dc.w	%1110111111111111
	dc.w	%1100000011110111
	dc.w	%1110111100000011
	dc.w	%1111111111110111
	dc.w	%1111111111111111
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type 8 - spotted
	dc.w	%1101010101010101
	dc.w	%1010101010101011
	dc.w	%1101010101010101
	dc.w	%1010101010101011
	dc.w	%1101010101010101
	dc.w	%1010101010101011
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type 9 - bordered
	dc.w	%1000000000000001
	dc.w	%1011111111111101
	dc.w	%1011111111111101
	dc.w	%1011111111111101
	dc.w	%1011111111111101
	dc.w	%1000000000000001
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type A - stripped
	dc.w	%1100110011001101
	dc.w	%1110011001100111
	dc.w	%1011001100110011
	dc.w	%1001100110011001
	dc.w	%1100110011001101
	dc.w	%1110011001100111
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type B - custom
	dc.w	%1000000000000001
	dc.w	%1011111111111101
	dc.w	%1010000000000001
	dc.w	%1010000000000001
	dc.w	%1010000000000001
	dc.w	%1000000000000001
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type C - spotted change
	dc.w	%1101010101111111
	dc.w	%1010101011111111
	dc.w	%1101010101111111
	dc.w	%1010101011111111
	dc.w	%1101010101111111
	dc.w	%1010101011111111
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type D - bordered change
	dc.w	%1000000001111111
	dc.w	%1011111111111111
	dc.w	%1011111111111111
	dc.w	%1011111111111111
	dc.w	%1011111111111111
	dc.w	%1000000001111111
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type E - stripped change
	dc.w	%1100110011111111
	dc.w	%1110011001111111
	dc.w	%1011001101111111
	dc.w	%1001100111111111
	dc.w	%1100110011111111
	dc.w	%1110011001111111
	dc.w	%0111111111111110
	dc.w	%0111111111111110	; type F - custom change
	dc.w	%1000000001111111
	dc.w	%1011111101111111
	dc.w	%1010000001111111
	dc.w	%1010000001111111
	dc.w	%1010000001111111
	dc.w	%1000000001111111
	dc.w	%0111111111111110

;---------= Static Variables =---------
speed:		dc.b	5

;---------= Dialog =----------
detectString:	dc.b	"Dia1JW"
_comment:	dc.b	"Diamonds v0.5"
author:		dc.b	" by Joe Wingbermuehle",0
help1:		dc.b	"F1 - Start Game",0
help2:		dc.b	23,24," - Level:",0
help3:		dc.b	"F2 - Speed:",0
help4:		dc.b	"F3 -",0
help5:		dc.b	"F5 - Exit",0
high_txt:	dc.b	"High Score:",0
gameOver_txt:	dc.b	"Game Over",0
nhs_txt:	dc.b	"New High Score!",0
initials_txt:	dc.b	"Initials:",0
noBonus_txt:	dc.b	"No "
bonus_txt:	dc.b	"Bonus!",0
level_txt:	dc.b	"Level",0
score_txt:	dc.b	"Score",0

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;-= Variables =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	bss
old_interrupt:	ds.l	1	; Storage for the TI-OS interrupt
fileHandle:	ds.l	1	; File offset for the current level
score:		ds.l	1	; Score
fileNumber:	ds.w	1	; File number in the VAT
lifeCount:	ds.w	1	; Score mod 400 for extra life
ballx:		ds.w	1	; Ball x coordinate
bally:		ds.w	1	; Ball y coordinate
balld:		ds.b	1	; Ball direction
btype:		ds.b	1	; Current block type
fblocks:	ds.b	1	; Number of First Blocks left
lblocks:	ds.b	1	; Number of Last Blocks left
reverseArrows:	ds.b	1	; 1 if arrows are reversed
bonusDelay:	ds.b	1	; Delay for bonus
bonus:		ds.b	1	; Bonus
lives:		ds.b	1	; Number of lives
over:		ds.b	1	; 1 if cross block hit
level:		dc.b	1	; Current level
numLevels:	dc.b	1	; Number of levels
startLevel:	dc.b	1	; Start level
key:		ds.b	1	; 1 if player has key
levelData1:	ds.b	1	; Ballx start position
levelData2:	ds.b	1	; Bally start position
timer:		ds.b	1	; Timer for delay
str:		ds.b	16	; String for binary->ASCII
strend:		ds.b	1	; End of binary->ASCII string
board:		ds.b	80	; Level matrix 8x10

	end
; Diamonds by Joe Wingbermuehle