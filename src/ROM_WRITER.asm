;=========================================================
; ;;VARIABLES;; ;

 HITS_TO_WIN     EQU 04 ;Normally all 12 ship tiles need to be hit
 ;Bits
 P1_P2 BIT 7FH          ;0 for P1, 1 for P2
 RADAR BIT 7EH          ;Radar mode indicator when updating LCD pages
 TEXT  BIT 7DH          ;Set to display text screen
 FILLER_SHORTCUT BIT 7CH
 SIMUL BIT 7BH

 ;Bytes
 MAP_NO        EQU 30H ;AKA: MAP_ID, SCREEN_ID, etc. -
                  ; 1 for P1 Map, 2 for P2 Map, 3 for Text Screen
 TILE_ROW      EQU 31H
 TILE_COL      EQU 32H
 TILE_ID       EQU 33H ;Use R's? ***

 CTR_I         EQU 07H
 CTR_II        EQU 06H
 CTR_III       EQU 05H

 LCD_COUNTER_1 EQU 04H
 LCD_COUNTER_2 EQU 03H

 INPUT_I       EQU 34H
 INPUT_II      EQU 35H

 P1_SHIPS_LEFT EQU 36H
 P2_SHIPS_LEFT EQU 37H

 ;Locations
 P1_MAP_START  EQU 40H
 P1_MAP_END    EQU 7FH
 
 P2_MAP_START  EQU 80H
 P2_MAP_END    EQU 0BFH

 TEXT_SCR_MAP_START    EQU 0C0H ; AKA: INFO_SCR
 TEXT_SCR_MAP_END      EQU 0FFH

 ;EEPROM
 eeprom_scl_pin  EQU P2.4  ;scl p2.4    a4h
 eeprom_sda_pin  EQU P2.5  ;sda p2.5    a5h
 memory_address1 EQU 38H
 eeprom_data     EQU 39H
 eeprom_page_no  EQU 3AH
 EEPROM_BUFF     EQU 3BH
 
 ;LCD
 RS        EQU P3.7 ;Pin  4    - 1 is for DATA, 0 is for COMMANDS - (RS)
 RD_WR     EQU P3.6 ;Pin  5    - 0 is for WRITE
 ENABLE    EQU P3.5 ;Pin  6    - SETB to SEND ?*
 LCD_PORT  EQU P1   ;Pins 7-14 - MSB at 14
 CS_1      EQU P3.4 ;Pin  15   - 0 = selected ?*
 CS_2      EQU P3.3 ;Pin  16   - 0 = selected ?*
 LCD_RESET EQU P3.2 ;Pin  17

;=========================================================
 MOV MEMORY_ADDRESS1, #0FFH
 MOV EEPROM_PAGE_NO,  #0H
 MOV DPTR, #TABLE_START
 MOV R2, #1
 ACALL CONFIGURE_LCD
 MOV TILE_ID, #RADAR_MISSED
 ACALL WRITE_TILE
 ACALL KEYBOARD
 MOV DPTR, #TABLE_START

 WRITING_LOOP:
 INC MEMORY_ADDRESS1
 MOV A, MEMORY_ADDRESS1
 CJNE A, #190, SAME_PAGE ;End of Page
 INC EEPROM_PAGE_NO
 MOV MEMORY_ADDRESS1, #0H
 
 MOV A, EEPROM_PAGE_NO
 CLR C
 CJNE A, #8, $+3 ;All 7 pages used
 JNC STOP
 SAME_PAGE:
 CLR A
 MOVC A, @A+DPTR
 MOV EEPROM_DATA, A
 ACALL WRITE_DATA
 ACALL EEPROM_DELAY
 ACALL READ_DATA
 ACALL EEPROM_DELAY
 INC DPTR

 MOV TILE_ID, EEPROM_BUFF
 PUSH DPH
 PUSH DPL
 ACALL WRITE_TILE
 POP DPL
 POP DPH
 INC R2
 CJNE R2, #8, WRITING_LOOP
 MOV R2, #100
 DELAYER:
 ACALL EEPROM_DELAY
 DJNZ R2, DELAYER
 MOV R2, #1
 SJMP WRITING_LOOP

 STOP:
 ACALL CONFIGURE_LCD
 MOV TILE_ID, #RADAR_HIT
 ACALL WRITE_TILE
 SJMP $


;=========================================================
; ;;METHODS;; ;
;=========================================================
 SELECT_TILE: ;Moves to R1 the RAM location of the tile at the specified map, row, and col
              ;USES: R7, R6, R1, A, TILE_ROW, TILE_COL, MAP_NO
 MOV A, MAP_NO
 CLR C
 CJNE A, #2, $+3
 JNC NEXT_1
 MOV R1, #P1_MAP_START
 MOV R6, #P1_MAP_END
 SJMP STARTED
 NEXT_1:
 CJNE A, #3, $+3
 JNC NEXT_2
 MOV R1, #P2_MAP_START
 MOV R6, #P2_MAP_END
 SJMP STARTED
 NEXT_2:
 MOV R1, #TEXT_SCR_MAP_START
 MOV R6, #TEXT_SCR_MAP_END

 STARTED:
 JB FILLER_SHORTCUT, FILLER
 
 MOV R7, TILE_ROW
 MOV A, R1
 ADD_LOOP1:
 ADD A, #8
 DJNZ R7, ADD_LOOP1

 MOV R7, TILE_COL
 ADD_LOOP2:
 INC A
 DJNZ R7, ADD_LOOP2
 MOV R1, A
 RET
 
;=========================================================
 FILL_MAP:    ;Fills the selected map entirely with the tile in TILE_ID
 SETB FILLER_SHORTCUT
 SJMP SELECT_TILE
 FILLER:
 CLR FILLER_SHORTCUT

 MOV A, R1
 FILL_LOOP:
 MOV @R1, TILE_ID
 INC R1
 INC A
 CJNE A, 06, FILLER
 MOV @R1, TILE_ID  ;Fills final register
 RET

;=========================================================
 WRITE_TILE:
 MOV R7, #8
 SJMP GET_PIXELS

 NEXT_BYTE:
 CLR A
 MOVC A, @A+DPTR
 ACALL SEND_BYTE
 INC DPTR
 DJNZ R7, NEXT_BYTE
 
 RET
 
;=========================================================
 GET_PIXELS:;(Now part of WRITE_TILE Method)
           ; Moves to DPTR location first of 8 consecutive pixel bytes defining a tile
           ; Uses: A, DPTR, TILE_ID
 MOV A, TILE_ID
 JNB ACC.7, IS_ID
 MOV TILE_ID, #RADAR_TGT
 MOV DPTR, #PICTURES
 SJMP TBL_FOUND
 IS_ID:
 JB ACC.6, PICTURE_ID
 JB ACC.5, LETTER_ID

 DIGIT_SYMBOL_ID:
 MOV DPTR, #DIGITS_SYMBOLS
 SJMP TBL_FOUND

 LETTER_ID:
 MOV DPTR, #LETTERS
 SJMP TBL_FOUND

 PICTURE_ID:
 MOV DPTR, #PICTURES

 TBL_FOUND:
 ANL A, #1FH ;Remove first 3 bits
 JZ END_OF_PROPAGATION

 PROPAGATE:
 INC DPTR
 INC DPTR
 INC DPTR
 INC DPTR
 
 INC DPTR
 INC DPTR
 INC DPTR
 INC DPTR

 DJNZ ACC, PROPAGATE
 END_OF_PROPAGATION:
 
 SJMP NEXT_BYTE
   
;=========================================================
 
;=========================================================
 
;=========================================================
 KEYBOARD: ;takes the key pressed from the keyboard and puts it to A
	MOV	P0, #0ffh	;makes P0 input
 K1:
	MOV	P2, #0	;ground all rows
	MOV	A, P0
	ANL	A, #00001111B
	CJNE	A, #00001111B, K1
 K2:
	ACALL	DELAY
	MOV	A, P0
	ANL	A, #00001111B
	CJNE	A, #00001111B, KB_OVER
	SJMP	K2
 KB_OVER:
	ACALL DELAY
	MOV	A, P0
	ANL	A, #00001111B
	CJNE	A, #00001111B, KB_OVER1
	SJMP	K2
 KB_OVER1:
	MOV	P2, #11111110B
	MOV	A, P0
	ANL	A, #00001111B
	CJNE	A, #00001111B, ROW_0
	MOV	P2, #11111101B
	MOV	A, P0
	ANL	A, #00001111B
	CJNE	A, #00001111B, ROW_1
	MOV	P2, #11111011B
	MOV	A, P0
	ANL	A, #00001111B
	CJNE	A, #00001111B, ROW_2
	MOV	P2, #11110111B
	MOV	A, P0
	ANL	A, #00001111B
	CJNE	A, #00001111B, ROW_3
	LJMP	K2
	
 ROW_0:
	mov	DPTR, #KCODE0
	sjmp	KB_FIND
 ROW_1:
	mov	DPTR, #KCODE1
	sjmp	KB_FIND
 ROW_2:
	mov	DPTR, #KCODE2
	sjmp	KB_FIND
 ROW_3:
	mov	DPTR, #KCODE3
 KB_FIND:
	rrc	A
	jnc	KB_MATCH
	inc	DPTR
	sjmp	KB_FIND
 KB_MATCH:
	clr	A
	movc	A, @A+DPTR; get ASCII code from the table 
	ret

;ASCII look-up table 
 KCODE0:	DB	'1', '2', '3', 'A'
 KCODE1:	DB	'4', '5', '6', 'B'
 KCODE2:	DB	'7', '8', '9', 'C'
 KCODE3:	DB	'G', 'F', 'E', 'D' ;
;=========================================================
 DELAY:
	push 0
	push 1
	mov R7,#50;50 in hardware
 DELAY_OUTER_LOOP:
	mov R6,#150;150 in hardware
	djnz R6,$
	djnz R7,DELAY_OUTER_LOOP
	pop 1
	pop 0
	ret
;=========================================================
;GRAPHIC LCD METHODS
;=========================================================
CONFIGURE_LCD:

 ;   ;;DELAY 32ms (delay why)
 ; MOV TMOD,#01H
 ; MOV TH0, #HIGH (33536)                                                                                                            
 ; MOV TL0, #LOW  (33536)
 ; CLR TF0
 ; SETB TR0
 ; JNB TF0, $
 ; CLR TR0

 CLR LCD_RESET
 NOP
 NOP
 SETB LCD_RESET ;active low

 SETB  CS_1
 SETB  CS_2


 MOV  A, #00111111b; Set disp ON  ( I thınk you should  have changed the  order)
 ACALL SEND_CMD

 MOV A,#0B8H; make x addrs0
 ACALL SEND_CMD
  
 MOV A,#040H;  make  y addrs 0
 ACALL SEND_CMD
 
 MOV  A, #11000000b; Set disp line to first line
 ACALL SEND_CMD
 
 MOV R4,#0;y values
 MOV R3,#10111000B;x values
 VER:MOV A,R3
 ACALL SEND_CMD
 HOR:CLR A
 ACALL SEND_BYTE
 INC R4
 CJNE R4,#64,HOR
 MOV R4,#0
 INC R3
 CJNE R3,#11000000B,VER

 MOV A,#0B8H; make x addrs0
 ACALL SEND_CMD
  
 MOV A,#040H;  make  y addrs 0
 ACALL SEND_CMD
 
 MOV  A, #11000000b; Set disp line to first line
 ACALL SEND_CMD
  
 RET
;=========================================================
 SEND_CMD:
 MOV  LCD_PORT, A
 CLR  RD_WR;
 CLR  RS
 SETB ENABLE
 NOP   ;how much delay is needed
 NOP
 CLR  ENABLE
 RET
;=========================================================
 SEND_BYTE:
 MOV  LCD_PORT, A
 CLR  RD_WR;;  MOV R4,#0;y values
 SETB RS   ; is this DATACOMMAND ?
 SETB ENABLE
 NOP   ;delay calculate
 NOP
 CLR  ENABLE
 RET
;=========================================================
;EEPROM METHODS
;=========================================================
write_data:     push acc ;;;;;;;;;;;;;;;;;;;;;;;;;;   PROTECT DATA 
    call eeprom_start
                mov a, #0A0H
                add a, EEPROM_PAGE_NO       
                add a, EEPROM_PAGE_NO 
                call send_data
                mov a,memory_address1          ;location address
                call send_data
                mov a,eeprom_data              ;data to be send
                call send_data
                call eeprom_stop
                pop acc ;;;;;;;;;;;;;;;;;;;;;;;;;;   PROTECT DATA
                ret   
;=========================================================
read_data:      push acc ;;;;;;;;;;;;;;;;;;;;;;;;;;   PROTECT DATA
    call eeprom_start

                mov a, #0A0H
                add a, EEPROM_PAGE_NO
                add a, EEPROM_PAGE_NO 
                call send_data
                mov a,memory_address1          ;location address
                call send_data
                call eeprom_start
                mov a, #0A1H
                add a, EEPROM_PAGE_NO
                add a, EEPROM_PAGE_NO 
                call send_data
                call get_data
                call eeprom_stop
                pop acc ;;;;;;;;;;;;;;;;;;;;;;;;;;   PROTECT DATA
                ret
;=========================================================
eeprom_start:    setb eeprom_sda_pin
                nop
                setb eeprom_scl_pin
                nop
                nop
                clr eeprom_sda_pin
                nop
                clr eeprom_scl_pin
                ret
;=========================================================
eeprom_stop:     clr eeprom_sda_pin
                nop
                setb eeprom_scl_pin
                nop
                nop
                setb eeprom_sda_pin
                nop
                clr eeprom_scl_pin
                ret
;=========================================================
send_data:      push 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;   PROTECT DATA
    mov r7,#00h
send:           rlc a
               mov eeprom_sda_pin,c
               call clock
               inc r7
               cjne r7,#08,send
               setb eeprom_sda_pin
               jb eeprom_sda_pin,$
              call eeprom_delay
               call clock
               pop 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;   PROTECT DATA
               ret
;=========================================================
get_data:      push 7;;;;;;;;;;;;;;;;;;;;;;;;;   PROTECT DATA
               mov r7,#00h
               setb eeprom_sda_pin
get:            mov c,eeprom_sda_pin
               call clock
               rlc a
               inc r7
               cjne r7,#08,get
               setb eeprom_sda_pin
               call clock
               mov EEPROM_BUFF,a
               pop 7;;;;;;;;;;;;;;;;;;;;;;;;;;   PROTECT DATA
               ret
;=========================================================
clock:         setb eeprom_scl_pin
               nop
               nop
               clr eeprom_scl_pin
               ret
;=========================================================
eeprom_delay:      mov 33h,#11      ;delay of 3 mili seconds 
eeprom_delay_1:    mov 32h,#0ffh
                   djnz 32h,$
                   djnz 33h,eeprom_delay_1
                   ret

;=========================================================
; ;;TILE_ID_TABLE;; ;
CHAR_0          EQU 00H ;000XXXXX: DIGITS & SYMBOLS
CHAR_1          EQU 01H
CHAR_2          EQU 02H
CHAR_3          EQU 03H
CHAR_4          EQU 04H
CHAR_5          EQU 05H
CHAR_6          EQU 06H
CHAR_7          EQU 07H
CHAR_8          EQU 08H
CHAR_9          EQU 09H
CHAR_SPACE      EQU 0AH
CHAR_Q_MARK     EQU 0BH
CHAR_EXCL_MARK  EQU 0CH
CHAR_POUND_SIGN EQU 0DH
CHAR_UNDERSCORE EQU 0EH
CHAR_SPCH_MARKS EQU 0FH
CHAR_APOSTROPHE EQU 10H
CHAR_ASTERISK   EQU 11H
			;12H-20H are unused
CHAR_A          EQU 21H ;001XXXXX: LETTERS
CHAR_B          EQU 22H
CHAR_C          EQU 23H
CHAR_D          EQU 24H
CHAR_E          EQU 25H
CHAR_F          EQU 26H
CHAR_G          EQU 27H
CHAR_H          EQU 28H
CHAR_I          EQU 29H
CHAR_J          EQU 2AH
CHAR_K          EQU 2BH
CHAR_L          EQU 2CH
CHAR_M          EQU 2DH
CHAR_N          EQU 2EH
CHAR_O          EQU 2FH
CHAR_P          EQU 30H
CHAR_Q          EQU 31H
CHAR_R          EQU 32H
CHAR_S          EQU 33H
CHAR_T          EQU 34H
CHAR_U          EQU 35H
CHAR_V          EQU 36H
CHAR_W          EQU 37H
CHAR_X          EQU 38H
CHAR_Y          EQU 39H
CHAR_Z          EQU 3AH
			;3BH-3FH are unused
BLANK_TILE      EQU 40H ;010XXXXX: PICTURES
RADAR_TGT       EQU 41H
RADAR_HIT       EQU 42H
RADAR_MISSED    EQU 43H
OPEN_SEA        EQU 44H ;0 (Java MapMaker Tile ID)
LONE_SHIP_TILE  EQU 45H ;1
SHIP_EASTWARD   EQU 46H ;2
SHIP_NORTHWARD  EQU 47H ;3
SHIP_WESTWARD   EQU 48H ;4
SHIP_SOUTHWARD  EQU 49H ;5
SHIP_VERTICAL   EQU 4AH ;6
SHIP_HORIZONTAL EQU 4BH ;7
SHIP_JUNCTION   EQU 4CH ;8
CORNERS_TILE_I  EQU 4DH ; 
HIT_SEA         EQU 4EH ; = OPEN_SEA + 10
HIT_SJIP_L      EQU 4FH 
HIT_SHIP_E      EQU 50H 
HIT_SHIP_N      EQU 51H 
HIT_SHIP_W      EQU 52H 
HIT_SHIP_S      EQU 53H 
HIT_SHIP_V      EQU 54H 
HIT_SHIP_H      EQU 55H 
HIT_SHIP_J      EQU 56H ; = SHIP_JUNCTION +10
CORNERS_TILE_II EQU 57H
HIT_TWICE       EQU 58H ;If a tile ID equal to or larger than this is noted, a tile was hit twice.
CHECKERED_TILE  EQU 59H ;PIXELS: 0AAH,  55H,   0AAH,  55H,   0AAH,  55H,   0AAH,  55H (for reference)
			; Other ID's until 7FH are not implemented and use no ROM.
;  END OF TABLE  ;

;=========================================================
;TILE_LUT ;**Will be copied over from other file
DIGITS_SYMBOLS:

LETTERS:
 
PICTURES:
DB 081H,  00H,   000H,  00H,   000H,  00H,   000H,  81H ; BLANK
DB  00H,  66H,    42H,  18H,    18H,  42H,    66H,  00H ; TGT
DB  42H, 0E7H,    66H,  18H,    18H,  66H,   0E7H,  42H ; HIT
DB  00H,  66H,    42H,  00H,    00H,  42H,    66H,  00H ; MISSED

DB 081H,  00H,   000H,  00H,   000H,  00H,   000H,  81H ; SEA
DB 081H,  00H,   000H,  18H,   018H,  00H,   000H,  81H ; L
DB 081H,  00H,   0E0H, 0F8H,   0F8H, 0E0H,   000H,  81H ; E
DB 081H,  00H,    00H,  18H,    18H,  3CH,    3CH, 0BDH ; N
DB 081H,  00H,    07H,  1FH,    1FH,  07H,    00H,  81H ; W

DB 0BDH,  3CH,    3CH,  18H,    18H,  00H,    00H,  81H ; S
DB 0BDH,  3CH,    3CH,  3CH,    3CH,  3CH,    3CH, 0BDH ; V
DB 081H,  00H,   0FFH, 0FFH,   0FFH, 0FFH,    00H,  81H ; H
DB 0AAH,  55H,   0AAH,  55H,   0AAH,  55H,   0AAH,  55H ; J*
DB 0AAH,  55H,   0AAH,  55H,   0AAH,  55H,   0AAH,  55H ; C1*

DB  81H,  18H,    24H,  42H,    42H,  24H,    18H,  81H ; HIT_SEA
DB 081H,  00H,   000H,  18H,    18H,  00H,    00H, 0FFH ; L*
DB  81H,  00H,    80H, 0B8H,   0F0H,  40H,    00H,  81H ; E
DB  81H,  00H,    00H,  00H,    08H,  1CH,    38H,  9DH ; N
DB  81H,  00H,    05H,  0FH,    1BH,  02H,    00H,  81H ; W

DB 0B9H,  1CH,    38H,  08H,    00H,  00H,    00H,  81H ; S
DB 0BDH,  1CH,    30H,  30H,    3CH,  1CH,    1CH, 0B5H ; V
DB  81H,  00H,   0CFH, 0DCH,   0FFH,  7BH,    00H,  81H ; H
DB 0AAH,  55H,   0AAH,  55H,   0AAH,  55H,   0AAH,  55H ; J*
DB 0AAH,  55H,   0AAH,  55H,   0AAH,  55H,   0AAH,  55H ; C2*

DB 0AAH,  55H,   0FFH, 0FFH,   0FFH, 0FFH,   0AAH,  55H ; HIT_TWICE
DB 0AAH,  55H,   0AAH,  55H,   0AAH,  55H,   0AAH,  55H ; CH 

;  END OF TABLE  ;
;=========================================================
;ROM TO LOAD
 TABLE_START:
;PAGE 0
DB 080H, 0A7H, 049H, 046H, 04BH, 048H, 0A0H, 047H, 084H, 049H, 0A4H, 049H, 04AH, 0A4H, 04AH, 04AH, 0A4H, 047H, 047H

DB 084H, 049H, 094H, 049H, 04AH, 094H, 04AH, 047H, 090H, 047H, 08FH, 046H, 04BH, 04BH, 048H, 080H, 0E0H, 046H, 048H

DB 082H, 049H, 0BAH, 046H, 04BH, 048H, 04AH, 082H, 04AH, 082H, 047H, 081H, 049H, 0B1H, 046H, 048H, 04AH, 081H, 047H

DB 080H, 08FH, 046H, 04BH, 04BH, 048H, 080H, 0C7H, 049H, 046H, 04BH, 048H, 0C0H, 04AH, 0C0H, 047H, 083H, 046H, 048H

DB 080H, 093H, 049H, 046H, 048H, 0D0H, 049H, 04AH, 0D0H, 04AH, 047H, 0C0H, 047H, 09EH, 046H, 04BH, 04BH, 048H, 080H

DB 0BAH, 046H, 04BH, 048H, 049H, 082H, 047H, 080H, 0C2H, 049H, 049H, 0C2H, 04AH, 04AH, 0C2H, 047H, 04AH, 082H, 047H

DB 090H, 049H, 090H, 04AH, 090H, 047H, 0E1H, 046H, 048H, 049H, 081H, 04AH, 081H, 047H, 09EH, 046H, 04BH, 04BH, 048H

DB 084H, 049H, 0F4H, 046H, 04BH, 048H, 047H, 080H, 0C0H, 049H, 0DEH, 04AH, 046H, 04BH, 04BH, 048H, 0C0H, 047H, 080H

DB 090H, 049H, 090H, 047H, 08EH, 046H, 04BH, 048H, 081H, 049H, 0F1H, 046H, 04BH, 048H, 04AH, 081H, 04AH, 081H, 047H

DB 08AH, 049H, 049H, 08AH, 04AH, 04AH, 08AH, 04AH, 047H, 088H, 047H, 082H, 049H, 08AH, 049H, 04AH, 08AH, 047H, 047H

;PAGE 1

DB 0AAH, 049H, 049H, 049H, 0AAH, 04AH, 04AH, 04AH, 0AAH, 047H, 04AH, 047H, 0C8H, 049H, 047H, 0C0H, 047H, 080H, 080H

DB 09CH, 046H, 04BH, 048H, 0A0H, 049H, 0A2H, 04AH, 049H, 0A2H, 04AH, 04AH, 0A2H, 047H, 047H, 081H, 049H, 081H, 047H

DB 097H, 049H, 046H, 04BH, 048H, 090H, 04AH, 097H, 04AH, 046H, 04BH, 048H, 090H, 047H, 080H, 088H, 049H, 088H, 047H

DB 0A0H, 049H, 0A0H, 04AH, 0A7H, 04AH, 046H, 04BH, 048H, 0A0H, 047H, 08EH, 046H, 04BH, 048H, 081H, 049H, 081H, 047H

DB 080H, 0AEH, 049H, 046H, 04BH, 048H, 0A0H, 047H, 08EH, 046H, 04BH, 048H, 080H, 0BCH, 046H, 04BH, 04BH, 048H, 080H
;  MAP 14  ^:
;  0  0  0  0  0  0  0  
;  0  5  0  2  7  4  0  
;  0  3  0  0  0  0  0  
;  0  0  0  2  7  4  0  
;  0  0  0  0  0  0  0  
;  0  2  7  7  4  0  0  
;  0  0  0  0  0  0  0
DB 0A0H, 049H, 0A0H, 04AH, 0A0H, 047H, 090H, 049H, 091H, 047H, 049H, 081H, 04AH, 0F9H, 046H, 04BH, 04BH, 048H, 047H

DB 0BCH, 046H, 04BH, 04BH, 048H, 080H, 088H, 049H, 088H, 04AH, 088H, 047H, 0B7H, 046H, 048H, 046H, 04BH, 048H, 080H

DB 081H, 049H, 085H, 049H, 04AH, 0C5H, 049H, 04AH, 047H, 0C4H, 04AH, 047H, 0C0H, 04AH, 0C4H, 047H, 049H, 084H, 047H

DB 080H, 080H, 09CH, 046H, 04BH, 048H, 081H, 049H, 09DH, 046H, 04BH, 048H, 04AH, 081H, 04AH, 0E1H, 046H, 048H, 047H

DB 082H, 049H, 0E2H, 046H, 048H, 04AH, 082H, 04AH, 082H, 047H, 091H, 049H, 049H, 091H, 04AH, 04AH, 091H, 047H, 047H 

;Page 2
DB 080H, 0BCH, 046H, 04BH, 04BH, 048H, 081H, 049H, 081H, 047H, 0C2H, 049H, 049H, 0C2H, 04AH, 04AH, 0C2H, 047H, 047H

DB 080H, 080H, 0C7H, 049H, 046H, 04BH, 048H, 0C0H, 04AH, 0D1H, 04AH, 049H, 049H, 0D1H, 047H, 04AH, 047H, 090H, 047H

DB 0F0H, 046H, 04BH, 048H, 080H, 08FH, 046H, 04BH, 04BH, 048H, 0F0H, 046H, 04BH, 048H, 082H, 049H, 082H, 047H, 080H

DB 084H, 049H, 0F5H, 046H, 04BH, 048H, 047H, 049H, 081H, 04AH, 085H, 049H, 047H, 084H, 04AH, 084H, 04AH, 084H, 047H

DB 084H, 049H, 094H, 049H, 04AH, 094H, 04AH, 04AH, 094H, 047H, 047H, 081H, 049H, 0C1H, 049H, 04AH, 0C1H, 047H, 047H


DB 080H, 0B8H, 046H, 04BH, 048H, 080H, 0A8H, 049H, 049H, 0AAH, 04AH, 04AH, 049H, 0AAH, 04AH, 047H, 047H, 0A0H, 047H

DB 0A0H, 049H, 0A7H, 04AH, 046H, 04BH, 048H, 0A0H, 04AH, 0A0H, 047H, 082H, 049H, 0A2H, 049H, 04AH, 0A2H, 047H, 047H

DB 080H, 096H, 049H, 046H, 048H, 090H, 04AH, 090H, 04AH, 097H, 047H, 046H, 04BH, 048H, 080H, 08EH, 046H, 04BH, 048H

DB 090H, 049H, 0D0H, 049H, 04AH, 0D0H, 04AH, 04AH, 0D7H, 047H, 047H, 046H, 04BH, 048H, 088H, 049H, 088H, 047H, 080H

DB 0E1H, 046H, 048H, 049H, 081H, 04AH, 081H, 047H, 082H, 049H, 0F2H, 046H, 04BH, 048H, 04AH, 082H, 04AH, 082H, 047H

;Page 3
DB 082H, 049H, 082H, 04AH, 0BAH, 046H, 04BH, 048H, 04AH, 0C2H, 049H, 047H, 0C0H, 04AH, 0D0H, 047H, 049H, 090H, 047H

DB 0C0H, 049H, 0C0H, 04AH, 0C1H, 047H, 049H, 081H, 04AH, 081H, 04AH, 081H, 047H, 0EEH, 046H, 048H, 046H, 04BH, 048H

DB 0A0H, 049H, 0A0H, 04AH, 0A0H, 047H, 08FH, 046H, 04BH, 04BH, 048H, 0F0H, 046H, 04BH, 048H, 084H, 049H, 084H, 047H

DB 087H, 046H, 04BH, 048H, 080H, 0F0H, 046H, 04BH, 048H, 084H, 049H, 084H, 04AH, 0A4H, 049H, 04AH, 0A4H, 047H, 047H

DB 084H, 049H, 084H, 04AH, 0C4H, 049H, 047H, 0CAH, 04AH, 049H, 049H, 0CAH, 04AH, 047H, 04AH, 0C2H, 047H, 047H, 080H


DB 080H, 09CH, 046H, 04BH, 048H, 0E0H, 046H, 048H, 081H, 049H, 081H, 04AH, 0B9H, 046H, 04BH, 048H, 04AH, 081H, 047H

DB 080H, 0C9H, 049H, 049H, 049H, 0C9H, 04AH, 04AH, 04AH, 0C9H, 047H, 04AH, 047H, 088H, 047H, 0C0H, 049H, 0C0H, 047H

DB 087H, 046H, 04BH, 048H, 098H, 046H, 048H, 080H, 090H, 049H, 097H, 04AH, 046H, 04BH, 048H, 090H, 04AH, 090H, 047H

DB 084H, 049H, 084H, 047H, 082H, 049H, 0F2H, 046H, 04BH, 048H, 04AH, 082H, 04AH, 082H, 047H, 09CH, 046H, 04BH, 048H

DB 08BH, 049H, 046H, 048H, 088H, 04AH, 088H, 047H, 080H, 08FH, 046H, 04BH, 04BH, 048H, 0F0H, 046H, 04BH, 048H, 080H

;Page 4
DB 080H, 089H, 049H, 049H, 089H, 04AH, 047H, 088H, 04AH, 089H, 047H, 049H, 081H, 04AH, 0F1H, 046H, 04BH, 048H, 047H

DB 0A0H, 049H, 0A0H, 047H, 080H, 0F7H, 046H, 04BH, 048H, 046H, 04BH, 048H, 080H, 080H, 08FH, 046H, 04BH, 04BH, 048H

DB 080H, 087H, 046H, 04BH, 048H, 088H, 049H, 088H, 04AH, 088H, 047H, 086H, 046H, 048H, 0F8H, 046H, 04BH, 04BH, 048H

DB 080H, 087H, 046H, 04BH, 048H, 0C0H, 049H, 0C2H, 04AH, 049H, 0C2H, 047H, 04AH, 082H, 04AH, 0E2H, 046H, 048H, 047H

DB 080H, 0B8H, 046H, 04BH, 048H, 087H, 046H, 04BH, 048H, 0F8H, 046H, 04BH, 04BH, 048H, 080H, 0C0H, 049H, 0C0H, 047H


DB 0F0H, 046H, 04BH, 048H, 080H, 0D0H, 049H, 049H, 0D1H, 047H, 04AH, 049H, 091H, 04AH, 04AH, 091H, 047H, 047H, 080H

DB 09EH, 046H, 04BH, 04BH, 048H, 080H, 0B8H, 046H, 04BH, 048H, 080H, 082H, 049H, 0A2H, 049H, 04AH, 0A2H, 047H, 047H

DB 080H, 081H, 049H, 0F1H, 046H, 04BH, 048H, 04AH, 081H, 04AH, 09DH, 046H, 04BH, 048H, 047H, 080H, 0E0H, 046H, 048H

DB 090H, 049H, 090H, 04AH, 092H, 04AH, 049H, 092H, 047H, 04AH, 0E2H, 046H, 048H, 047H, 080H, 08EH, 046H, 04BH, 048H

DB 08FH, 046H, 04BH, 04BH, 048H, 0F0H, 046H, 04BH, 048H, 084H, 049H, 084H, 047H, 081H, 049H, 081H, 04AH, 081H, 047H

;Page 5
DB 08BH, 049H, 046H, 048H, 088H, 04AH, 08AH, 047H, 049H, 082H, 04AH, 082H, 04AH, 082H, 047H, 09CH, 046H, 04BH, 048H

DB 080H, 0A0H, 049H, 0A0H, 04AH, 0AEH, 04AH, 046H, 04BH, 048H, 0A1H, 047H, 049H, 081H, 04AH, 08DH, 046H, 048H, 047H

DB 080H, 080H, 082H, 049H, 0AAH, 049H, 049H, 04AH, 0AAH, 04AH, 04AH, 047H, 0A9H, 04AH, 047H, 049H, 0A1H, 047H, 047H

DB 09CH, 046H, 04BH, 048H, 0C0H, 049H, 0C0H, 04AH, 0C0H, 047H, 0BCH, 046H, 04BH, 04BH, 048H, 080H, 098H, 046H, 048H

DB 080H, 080H, 0C1H, 049H, 049H, 0C5H, 04AH, 049H, 04AH, 0C5H, 047H, 04AH, 047H, 0B4H, 046H, 048H, 04AH, 084H, 047H


DB 080H, 0C0H, 049H, 0C0H, 047H, 0A7H, 049H, 046H, 04BH, 048H, 0A8H, 04AH, 049H, 0A8H, 04AH, 04AH, 0A8H, 047H, 047H

DB 080H, 0F4H, 046H, 04BH, 048H, 049H, 084H, 047H, 0A0H, 049H, 0A4H, 04AH, 049H, 0A4H, 04AH, 04AH, 0A4H, 047H, 047H

DB 084H, 049H, 084H, 04AH, 0F5H, 046H, 04BH, 048H, 047H, 049H, 081H, 047H, 080H, 080H, 09EH, 046H, 04BH, 04BH, 048H

DB 084H, 049H, 084H, 04AH, 084H, 047H, 088H, 049H, 08AH, 04AH, 049H, 08AH, 04AH, 04AH, 0EAH, 046H, 048H, 047H, 047H

DB 090H, 049H, 090H, 04AH, 092H, 047H, 049H, 082H, 04AH, 092H, 049H, 047H, 090H, 047H, 08FH, 046H, 04BH, 04BH, 048H

;Page 6
DB 080H, 080H, 08EH, 046H, 04BH, 048H, 090H, 049H, 094H, 04AH, 049H, 095H, 04AH, 04AH, 049H, 095H, 047H, 047H, 047H

DB 080H, 097H, 049H, 046H, 04BH, 048H, 090H, 04AH, 090H, 047H, 081H, 049H, 0BDH, 046H, 04BH, 04BH, 048H, 047H, 080H

DB 080H, 0F8H, 046H, 04BH, 04BH, 048H, 084H, 049H, 084H, 047H, 0B8H, 046H, 04BH, 048H, 080H, 0F0H, 046H, 04BH, 048H

DB 087H, 046H, 04BH, 048H, 080H, 0F0H, 046H, 04BH, 048H, 084H, 049H, 085H, 04AH, 049H, 085H, 04AH, 047H, 084H, 047H

DB 0A8H, 049H, 049H, 0A8H, 04AH, 04AH, 0A8H, 047H, 04AH, 08BH, 047H, 046H, 048H, 080H, 080H, 0B8H, 046H, 04BH, 048H


DB 080H, 080H, 0A1H, 049H, 049H, 0A1H, 04AH, 04AH, 0A1H, 047H, 04AH, 09DH, 046H, 04BH, 048H, 047H, 0E0H, 046H, 048H

DB 080H, 08FH, 046H, 04BH, 04BH, 048H, 0A0H, 049H, 0A0H, 04AH, 0A1H, 047H, 049H, 091H, 049H, 04AH, 091H, 047H, 047H

DB 080H, 080H, 080H, 09EH, 046H, 04BH, 04BH, 048H, 0A1H, 049H, 049H, 0A9H, 04AH, 049H, 04AH, 0A9H, 047H, 047H, 047H

DB 080H, 09CH, 046H, 04BH, 048H, 0E0H, 046H, 048H, 08FH, 046H, 04BH, 04BH, 048H, 0A0H, 049H, 0A0H, 04AH, 0A0H, 047H

DB 0DCH, 049H, 046H, 04BH, 048H, 0C0H, 04AH, 0C0H, 047H, 080H, 08FH, 046H, 04BH, 04BH, 048H, 0B0H, 046H, 048H, 080H

;Page 7
DB 080H, 0C1H, 049H, 049H, 0C1H, 04AH, 047H, 0DEH, 047H, 046H, 04BH, 04BH, 048H, 0A0H, 049H, 0A0H, 04AH, 0A0H, 047H

DB 0E0H, 046H, 048H, 097H, 049H, 046H, 04BH, 048H, 090H, 04AH, 090H, 047H, 080H, 09EH, 046H, 04BH, 04BH, 048H, 080H

DB 084H, 049H, 0F4H, 046H, 04BH, 048H, 04AH, 084H, 047H, 098H, 046H, 048H, 080H, 0F8H, 046H, 04BH, 04BH, 048H, 080H

DB 086H, 046H, 048H, 088H, 049H, 088H, 04AH, 088H, 047H, 082H, 049H, 0FAH, 046H, 04BH, 04BH, 048H, 04AH, 082H, 047H

DB 080H, 082H, 049H, 0BAH, 046H, 04BH, 048H, 04AH, 082H, 047H, 080H, 0EFH, 046H, 048H, 046H, 04BH, 04BH, 048H, 080H


DB 081H, 049H, 085H, 049H, 04AH, 085H, 04AH, 047H, 084H, 04AH, 0F4H, 046H, 04BH, 048H, 047H, 082H, 049H, 082H, 047H

DB 090H, 049H, 090H, 04AH, 094H, 047H, 049H, 085H, 047H, 049H, 0F9H, 046H, 04BH, 04BH, 048H, 04AH, 081H, 047H, 080H

DB 080H, 088H, 049H, 08BH, 04AH, 046H, 048H, 088H, 047H, 080H, 08FH, 046H, 04BH, 04BH, 048H, 0F0H, 046H, 04BH, 048H

DB 08EH, 046H, 04BH, 048H, 080H, 081H, 049H, 099H, 046H, 048H, 04AH, 0C1H, 049H, 04AH, 0C1H, 04AH, 047H, 0C0H, 047H

DB 09EH, 046H, 04BH, 04BH, 048H, 080H, 080H, 08DH, 046H, 048H, 049H, 081H, 04AH, 09DH, 046H, 04BH, 048H, 047H, 080H
;END OF TABLE
END
