

;yousef salman 

LIST 	P=PIC16F877
		include	<P16f877.inc>
 __CONFIG _CP_OFF & _WDT_OFF & _BODEN_OFF & _PWRTE_OFF & _HS_OSC & _WRT_ENABLE_ON & _LVP_OFF & _DEBUG_OFF & _CPD_OFF
 ;---------------------------------------------------------------------------------------;

		org		0x00
reset:	goto	start

		org		0x04
		goto	psika 

		org		0x10
start:	bcf		STATUS, RP0
		bcf		STATUS, RP1			;Bank0 <------
;---------- Initialize area: -----------------------------------------------------------;
		clrf	PORTD
		clrf	PORTA
		clrf	PORTE
		clrf	INTCON ;Disable all interrupts
		clrf	PIR1 ;clear to atd flag
		clrf 0x70
		clrf 0x71
		clrf 0x72
		clrf 0x73

		bsf		STATUS, RP0			;Bank1 <------
;-------------------------------------------------
		clrf	PIE1; clear for the register with atd interrupt
		clrf	TRISE				;porte output
		bsf		PIE1, ADIE			;Enable ADC Interrupt

		movlw	0x02
		movwf	ADCON1			   	; all A analog;all E digital
	
		clrf	TRISD				;portd output
		movlw	0xff
		movwf	TRISA				;porta input

		bcf		STATUS, RP0		 	;Bank0 <------
;-------------------------------------------------
		call init  ;Initialize lcd





		movlw	0x81				;B'10000001'
		movwf	ADCON0				;conersion clock Fosc/32, channel_0 ra0, ADC on
		call	d_20				;Delay TACQ
		bsf		ADCON0, GO			;Start conversion

		bsf 	INTCON, PEIE		;Enable Peripherals interrupts
		bsf		INTCON, GIE			;Enable Interrupts 

;---------- Main program area: ---------------------------------------------------------;


loop:
; empty loop
goto loop

;function that increase 0x70 and check if we reached 250 make it 0 
inc:
call upp
incf 0x70
movlw d'250'
		clrf STATUS
		subwf 0x70,0
		btfsc STATUS,C
		clrf 0x70

return
;function that decrease 0x70 and check if we reach 0 make it 250
dec:
call downn
decf 0x70
movlw d'2'
movwf 0x45
movlw d'0'
		clrf STATUS
		subwf 0x70,0
		btfsc STATUS,Z
		call	make250

return


make250:
movlw d'250'
movwf 0x70
print:

		movlw	B'10000000' 			 ;PLACE for the data on the LCD
		movwf	0x20			;B'10000000'
		call 	lcdc
		call	mdel
clrf 0x71
clrf 0x72
clrf 0x73

	call hundreds

		

		movlw d'48'
		addwf 0x71,W			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel

		movlw d'48'
		addwf	0x72,W			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel

		movlw d'48'
		addwf	0x73,W			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel



return

upp:
	movlw	0xc0 			 ;PLACE for the data on the LCD
		movwf	0x20			;B'10000000'
		call 	lcdc
		call	mdel

movlw	0x55			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
movlw	0x50			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
movlw	0x20			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
movlw	0x20			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
return
downn:
movlw	0xc0 			 ;PLACE for the data on the LCD
		movwf	0x20			;B'10000000'
		call 	lcdc
		call	mdel

movlw	0x44			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
movlw	0x4f			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
movlw	0x57			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
movlw	0x4e			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
return

hundreds:
movf 0x70,W;save the value of 0x70
movwf 0x75	
hundredss:;see how many hundreds in 0x70 and save it 0x71
	movlw d'100'
	clrf STATUS
	subwf 0x70,f
	btfsc STATUS,C
	incf 0x71
	btfsc STATUS,C
	goto hundredss
addwf 0x70


tens:;see how many tens in 0x70 and save it 0x72
movlw d'10'
	clrf STATUS
	subwf 0x70,f
	btfsc STATUS,C
	incf 0x72
	btfsc STATUS,C
	goto tens
	addwf 0x70

ones:;see how many ones in 0x70 and save it 0x73
movlw d'1'
	clrf STATUS
	subwf 0x70,f
	btfsc STATUS,C
	incf 0x73
	btfsc STATUS,C
	goto ones
movf 0x75,W;return the value from 0x75 to 0x70
movwf 0x70
clrf 0x75
return





















;---------- Functions area: -----------------------------------------------------------;

d_20:	movlw	0x20
		movwf	0x22
lulaa1:	decfsz	0x22, f
		goto	lulaa1
		return

d_4:	movlw	0x06
		movwf	0x22
lulaa2:	decfsz	0x22, f
		goto	lulaa2
		return

;---------- Interrupt program: ---------------------------------------------------------;

		
psika:	movwf	0x7A				;store W_reg --> 0x7A
		swapf	STATUS, w
		movwf	0x7B				;store STATUS --> 0x7B

		bcf  	STATUS, RP0
		bcf		STATUS, RP1  		;Bank0 <------

		btfsc 	PIR1, ADIF;check the flag ADC	
		goto	AtD
ERR:	goto 	ERR

AtD:
clrf 0x41
	bcf 	PIR1, ADIF			;Clear AD Flag
		movf 	ADRESH, w
		;check if the value of the voltage between 0.5 and 1.5 and if yes go to do1
		movwf 0x78
		movlw d'25'
		clrf STATUS
		subwf 0x78,0
		btfss STATUS,C
		goto back
		movlw d'76'
		clrf STATUS
		subwf 0x78,0
		btfsc STATUS,C
		goto check
		btfss STATUS,C
		call do1
;check if the value of the voltage between 1.8 and 2.3 and if yes go to do2
check:

		movlw d'91'
		clrf STATUS
		subwf 0x78,0
		btfss STATUS,C
		goto back
		movlw d'117'
		clrf STATUS
		subwf 0x78,0
		btfsc STATUS,C
		goto back
		btfss STATUS,C
		call do2

back:
;check if the value of 0x41 is 1 if yes do the function inc 
		movlw d'1'
		clrf STATUS
		subwf 0x41,0
		btfsc STATUS,Z
		call inc
;check if the value of 0x41 is 2 if yes do the function dec
		movlw d'2'
		clrf STATUS
		subwf 0x41,0
		btfsc STATUS,Z
		call dec

		call print

		
		call	d_4
		bsf 	ADCON0, GO			;Start new conversion

		swapf	0x7B, w
		movwf	STATUS				;restore STATUS <-- 0x7B
		swapf	0x7A, f
		swapf	0x7A, w				;restore W_reg <-- 0x7A
		retfie

;---------------------------------------------------------------------------------------
do1:
call delay_500m
call delay_500m
 movlw d'1'
movwf 0x41

return
do2:
call delay_500m
call delay_500m
movlw d'2'
movwf 0x41
return


delay_500m:					;-----> 500ms delay
		movlw		0x32			
		movwf		0x51
CONT5:	movlw		0x80		
		movwf		0x52
CONT6:	movlw		0x80			
		movwf		0x53
CONT7:	decfsz		0x53, f
		goto		CONT7
		decfsz		0x52, f
		goto		CONT6
		decfsz		0x51, f
		goto		CONT5
		return						; D = (5+4N1+4N1N2+3N1N2N3)*200nsec = (5+4*50+4*50*128+3*50*128*128)*200ns = 496.7ms=~500ms


;subroutine to initialize LCD
;
init	movlw	0x30 ;00011110
		movwf	0x20
		call 	lcdc
		call	del_41

		movlw	0x30
		movwf	0x20
		call 	lcdc
		call	del_01

		movlw	0x30
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x01		; display clear
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x06		;3. ID=1,S=0 increment,no  shift 000001 ID S  
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x0c		;4. D=1,C=B=0 set display ,no cursor, no blinking
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x38		; dl=1 ( 8 bits interface,n=2 lines,f=5x8 dots)
		movwf	0x20
		call 	lcdc
		call	mdel
		return

;










;subroutine to write command to LCD
;

lcdc	movlw	0x00		; E=0,RS=0 
		movwf	PORTE
		movf	0x20,w
		movwf	PORTD
		movlw	0x01		; E=1,RS=0
		movwf	PORTE
        call	sdel
		movlw	0x00		; E=0,RS=0
		movwf	PORTE
		return

;
;subroutine to write data to LCD
;

lcdd	movlw		0x02		; E=0, RS=1
		movwf		PORTE
		movf		0x20,w
		movwf		PORTD
        movlw		0x03		; E=1, rs=1  
		movwf		PORTE
		call		sdel
		movlw		0x02		; E=0, rs=1  
		movwf		PORTE
		return

;----------------------------------------------------------

del_41	movlw		0xcd
		movwf		0x23
lulaa6	movlw		0x20
		movwf		0x22
lulaa7	decfsz		0x22,1
		goto		lulaa7
		decfsz		0x23,1
		goto 		lulaa6 
		return


del_01	movlw		0x20
		movwf		0x22
lulaa8	decfsz		0x22,1
		goto		lulaa8
		return


sdel	movlw		0x19		; movlw = 1 cycle
		movwf		0x23		; movwf	= 1 cycle
lulaaa2	movlw		0xfa
		movwf		0x22
lulaaa1	decfsz		0x22,1		; decfsz= 12 cycle
		goto		lulaaa1		; goto	= 2 cycles
		decfsz		0x23,1
		goto 		lulaaa2 
		return


mdel	movlw		0x0a
		movwf		0x24
lulaa5	movlw		0x19
		movwf		0x23
lulaa4	movlw		0xfa
		movwf		0x22
lulaa3	decfsz		0x22,1
		goto		lulaa3
		decfsz		0x23,1
		goto 		lulaa4 
		decfsz		0x24,1
		goto		lulaa5
		return








		end