; ;:-* 8052 BattleShip *-:; ;
; EEE212 - Section 4 - Group 9 ;
;https://www.riyas.org/2013/12/online-led-matrix-font-generator-with.html
;=========================================================
; ;;VARIABLES;; ;

 HITS_TO_WIN     EQU 04 ;Normally all 12 ship tiles need to be hit
 ;Bits
 P1_P2 BIT 7FH          ;0 for P1, 1 for P2
 RADAR BIT 7EH          ;Radar mode indicator when updating LCD pages
 TEXT  BIT 7DH          ;Set to display text screen
 FILLER_SHORTCUT BIT 7CH;For FILL_MAP subroutine efficiency
 SIMUL BIT 7BH          ;SET when simulating model (map loading, etc)

 ;Bytes
 MAP_NO        EQU 30H ;AKA: MAP_ID, SCREEN_ID, etc. -
                  ; 1 for P1 Map, 2 for P2 Map, 3 for Text Screen
 TILE_ROW      EQU 31H
 TILE_COL      EQU 32H
 TILE_ID       EQU 33H ;Use R's? ***

 CTR_I         EQU 07H ; Counters
 CTR_II        EQU 06H
 CTR_III       EQU 05H

 LCD_COUNTER_1 EQU 04H ; Counters for LCD subroutines
 LCD_COUNTER_2 EQU 03H

 INPUT_I       EQU 34H
 INPUT_II      EQU 35H

 P1_SHIPS_LEFT EQU 36H ;Remaining ships tracked here
 P2_SHIPS_LEFT EQU 37H

 ;RAM Locations
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
 eeprom_data     EQU 39H   ;Input when writing
 eeprom_page_no  EQU 3AH
 EEPROM_BUFF     EQU 3BH   ;Output when reading
 
 ;LCD
 RS        EQU P3.7 ;Pin  4    - 1 is for DATA, 0 is for COMMANDS - (RS)
 RD_WR     EQU P3.6 ;Pin  5    - 0 is for WRITE
 ENABLE    EQU P3.5 ;Pin  6    - SETB to SEND 
 LCD_PORT  EQU P1   ;Pins 7-14 - MSB at 14
 CS_1      EQU P3.4 ;Pin  15   - 1 = selected
 CS_2      EQU P3.3 ;Pin  16   - 1 = selected
 LCD_RESET EQU P3.2 ;Pin  17

;=========================================================

; ;;PROGRAM;; ;

CLR SIMUL
;SETB SIMUL
JB SIMUL, MAP_SETUP

;Clear All RAM (Set to #0)
 MOV R0,  #255
 CLEAR_LOOP:
 MOV @R0, #0
 DJNZ R0, CLEAR_LOOP
 
;SETUP Maps
 CLR P1_P2
 
 MAP_SETUP:

   JNB SIMUL, SS1
   LJMP EEPROM_ALT ;In simulation mode, the custom maps are loaded
   SS1:
 
   ;;Ask for Map No on Text Screen: PX: Enter Map #
      ;;;Clear Text Screen
 MOV MAP_NO, #3
 MOV TILE_ID, #BLANK_TILE
 ACALL FILL_MAP

      ;;;Display Message "PX Enter Map No"
 MOV MAP_NO, #3
 MOV TILE_ROW, #1
 MOV TILE_COL, #3
 ACALL SELECT_TILE
 MOV @R1, #CHAR_P
 INC R1
 MOV @R1, #CHAR_1
 JNB P1_P2, SKIP1
 MOV @R1, #CHAR_2
 SKIP1:
 MOV TILE_ROW, #2
 MOV TILE_COL, #2
 ACALL SELECT_TILE
 MOV @R1, #CHAR_E
 INC R1
 MOV @R1, #CHAR_N
 INC R1
 MOV @R1, #CHAR_T
 INC R1
 MOV @R1, #CHAR_E
 INC R1
 MOV @R1, #CHAR_R
 MOV TILE_ROW, #3
 MOV TILE_COL, #1
 ACALL SELECT_TILE
 MOV @R1, #CHAR_M
 INC R1
 MOV @R1, #CHAR_A
 INC R1
 MOV @R1, #CHAR_P
 INC R1
 MOV @R1, #CHAR_SPACE
 INC R1
 MOV @R1, #CHAR_N
 INC R1
 MOV @R1, #CHAR_O
 MOV TILE_ROW, #5
 MOV TILE_COL, #3
 ACALL SELECT_TILE
 MOV @R1, #CHAR_UNDERSCORE
 INC R1
 MOV @R1, #CHAR_UNDERSCORE

 SETB TEXT
 ACALL UPDATE_LCD_ALL
 
   ;;Receive Map Number
      ;;;Digit 1
 ENTER1:
 ACALL KEYBOARD
 CJNE A, #'F', SKIP00;F key is used as 0 key
 MOV A, #'0'
 SKIP00:
 CLR C
 SUBB A, #30H
 CLR C
 CJNE A, #8, $+3
 JNC ENTER1
 MOV INPUT_I, A

 MOV TILE_ID, A  ;TILE ID = Integer value for 0-9
 MOV TILE_ROW, #5
 MOV TILE_COL, #3;**
 ACALL SELECT_TILE
 MOV @R1, TILE_ID
 ;ACALL UPDATE_LCD_COL
 SETB TEXT
 ACALL UPDATE_LCD_ALL

      ;;;Digit 2
 ENTER2:
 ACALL KEYBOARD
 CJNE A, #'F', SKIP01;F key is used as 0 key
 MOV A, #'0'
 SKIP01:
 CLR C
 SUBB A, #30H
 CLR C
 CJNE A, #10, $+3
 JNC ENTER2
 MOV INPUT_II, A
	
 
 MOV TILE_ID, A  ;TILE ID = Integer value for 0-9
 MOV TILE_ROW, #5
 MOV TILE_COL, #4
 ACALL SELECT_TILE
 MOV @R1, TILE_ID
 ;ACALL UPDATE_LCD_COL
 SETB TEXT
 ACALL UPDATE_LCD_ALL

   ;;DELAY 1S
 MOV R7, #5 ;15 for 1s
 DELAY1:
 MOV TMOD,#01H
 MOV TH0, #0                                                                                                            
 MOV TL0, #0
 CLR TF0
 SETB TR0
 JNB TF0, $
 CLR TR0
 DJNZ R7, DELAY1
 
   ;;Fetch Map
 ;SJMP EEPROM_ALT ;TO SKIP EEPROM, COMMENT IT***
 
 MOV CTR_I, #19
 MOV R1, #TEXT_SCR_MAP_START
 
 MOV EEPROM_PAGE_NO, INPUT_I
 MOV A, INPUT_II
 MOV B, #19
 MUL AB
 MOV MEMORY_ADDRESS1, A

 READ_LOOP:
 PUSH 01
 ACALL READ_DATA
 POP  01
 MOV @R1, EEPROM_BUFF
 INC R1
 INC MEMORY_ADDRESS1
 DJNZ CTR_I, READ_LOOP

   EEPROM_ALT: ;For running the program without the EEPROM, comment this when not using it
;   MOV R1, #TEXT_SCR_MAP_START ;P1 Map (3 vertical, small horz)
;   MOV @R1, #0A8H
;   INC R1
;   MOV @R1, #49H
;   INC R1
;   MOV @R1, #49H
;   INC R1
;   
;   MOV @R1, #0ABH
;   INC R1
;   MOV @R1, #4AH
;   INC R1
;   MOV @R1, #4AH
;   INC R1
;   MOV @R1, #46H
;   INC R1
;   MOV @R1, #48H
;   INC R1
;   
;   MOV @R1, #0A8H
;   INC R1
;   MOV @R1, #47H
;   INC R1
;   MOV @R1, #4AH
;   INC R1
;   
;   MOV @R1, #89H
;   INC R1
;   MOV @R1, #47H
;   INC R1
;   MOV @R1, #49H
;   INC R1
;   
;   MOV @R1, #81H
;   INC R1
;   MOV @R1, #4AH
;   INC R1
;   
;   MOV @R1, #81H
;   INC R1
;   MOV @R1, #47H
;   INC R1
;   
;   MOV @R1, #80H
;
;   JB P1_P2, LOAD_MAP
;
;   MOV R1, #TEXT_SCR_MAP_START ;P2 Map
;   MOV @R1, #80H
;   
;   INC R1
;   MOV @R1, #80H
;   
;   INC R1
;   MOV @R1, #0B0H
;   INC R1
;   MOV @R1, #46H
;   INC R1
;   MOV @R1, #48H
;   
;   INC R1
;   MOV @R1, #8FH
;   INC R1
;   MOV @R1, #46H
;   INC R1
;   MOV @R1, #4BH
;   INC R1
;   MOV @R1, #4BH
;   INC R1
;   MOV @R1, #48H
;   
;   INC R1
;   MOV @R1, #0F0H
;   INC R1
;   MOV @R1, #46H
;   INC R1
;   MOV @R1, #4BH
;   INC R1
;   MOV @R1, #48H
;   
;   INC R1
;   MOV @R1, #80H
;   
;   INC R1
;   MOV @R1, #87H
;   INC R1
;   MOV @R1, #46H
;   INC R1
;   MOV @R1, #4BH
;   INC R1
;   MOV @R1, #48H
 
   ;;Decode and Load Map
 LOAD_MAP:
      ;;;Initialize map with open sea tiles
 MOV TILE_ID, #OPEN_SEA 
 MOV MAP_NO, #1
 JNB P1_P2, SKIP2
 MOV MAP_NO, #2
 SKIP2:
 ACALL FILL_MAP
 
      ;;;Load Legend
 MOV R1, #P1_MAP_START ; Row 0
 JNB P1_P2, SKIP3
 MOV R1, #P2_MAP_START
 SKIP3:
 MOV @R1, #CHAR_POUND_SIGN
 INC R1
 MOV @R1, #CHAR_1
 INC R1
 MOV @R1, #CHAR_2
 INC R1
 MOV @R1, #CHAR_3
 INC R1
 MOV @R1, #CHAR_4
 INC R1
 MOV @R1, #CHAR_5
 INC R1
 MOV @R1, #CHAR_6
 INC R1
 MOV @R1, #CHAR_7

 MOV R1, #P1_MAP_START ; Row 0
 JNB P1_P2, SKIP4
 MOV R1, #P2_MAP_START
 SKIP4:
 MOV A, R1
 ADD A, #8
 MOV R1, A
 MOV @R1, #CHAR_A
 ADD A, #8
 MOV R1, A
 MOV @R1, #CHAR_B
 ADD A, #8
 MOV R1, A
 MOV @R1, #CHAR_C
 ADD A, #8
 MOV R1, A
 MOV @R1, #CHAR_D
 ADD A, #8
 MOV R1, A
 MOV @R1, #CHAR_E
 ADD A, #8
 MOV R1, A
 MOV @R1, #CHAR_F
 ADD A, #8
 MOV R1, A
 MOV @R1, #CHAR_G 
      ;;;Load Ships
        ;Format: R0 reads the map code,
        ;TILE_ROW Keeps row count
        ;TILE_COL Keeps col count
 MOV R0, #TEXT_SCR_MAP_START; This location is temporarily used for holding encoded map before decoding it.

        ;Encoding is such that if the MSB is 1, the byte maps the blank tiles
        ; in its respectively numbered row. IF the MSB is 0, the byte contains 
        ; the ID of the tile with the order respective to its row mapping byte.
 MOV TILE_ROW, #0 ;0 is normal
 MOV TILE_COL, #1
 
 NEXT_MAP_CODE_BYTE:
 MOV A, @R0
 CLR C
 RLC A  ;MSB is checked
 JC A_IS_ROW_MAPPER
 A_IS_TILE_ID:
 INC R0
 SJMP NEXT_MAP_CODE_BYTE

 A_IS_ROW_MAPPER:
 INC TILE_ROW
 MOV TILE_COL, #0
 RLC A; ;Checking bit 6
 INC TILE_COL ;Column 1
 JNC COL2
 ACALL LOAD_TILE; If the bit is 1, a tile needs to be loaded
 COL2:
 RLC A; ;Checking bit 5
 INC TILE_COL 
 JNC COL3
 ACALL LOAD_TILE
 COL3:
 RLC A; ;Checking bit 4
 INC TILE_COL
 JNC COL4
 ACALL LOAD_TILE
 COL4:
 RLC A; ;Checking bit 3
 INC TILE_COL 
 JNC COL5
 ACALL LOAD_TILE
 COL5:
 RLC A; ;Checking bit 2
 INC TILE_COL
 JNC COL6
 ACALL LOAD_TILE
 COL6:
 RLC A; ;Checking bit 1
 INC TILE_COL
 JNC COL7
 ACALL LOAD_TILE
 COL7:
 RLC A; ;Checking bit 0
 INC TILE_COL
 JNC END_ROW
 ACALL LOAD_TILE
 END_ROW:
 INC R0 ;
 MOV A, TILE_ROW
 CJNE A, #7, NEXT_MAP_CODE_BYTE ;End of Row 7 (Row G) is end of loading
 SJMP MAP_LOADED
 
 LOAD_TILE:
 PUSH ACC
 INC R0
 MOV TILE_ID, @R0
 ACALL SELECT_TILE
 MOV @R1, TILE_ID
 POP ACC
 RET

 
   ;;SETUP P2 MAP
 MAP_LOADED:
 
 CPL P1_P2
 JNB  P1_P2, MAP_2_LOADED
 LJMP MAP_SETUP
 
 MAP_2_LOADED:
 ;Maps are complete.

 
;MAIN GAME
 MOV P1_SHIPS_LEFT, #HITS_TO_WIN
 MOV P2_SHIPS_LEFT, #HITS_TO_WIN
 
 SETB P1_P2; P1 will start

 JNB SIMUL, MAIN_GAME
 ACALL KEYBOARD
 
MAIN_GAME:
 CPL P1_P2 ;
   ;;Start Turn: Load Info screen "Press "9" to begin turn"
 MOV MAP_NO, #3
 
 MOV TILE_ID, #BLANK_TILE
 ACALL FILL_MAP
 
 MOV TILE_ROW, #1
 MOV TILE_COL, #3
 ACALL SELECT_TILE
 MOV @R1, #CHAR_P
 INC R1
 MOV @R1, #CHAR_1
 JNB P1_P2, SKIP77
 MOV @R1, #CHAR_2
 SKIP77:
 MOV TILE_ROW, #2
 MOV TILE_COL, #2
 ACALL SELECT_TILE
 MOV @R1, #CHAR_E
 INC R1
 MOV @R1, #CHAR_N
 INC R1
 MOV @R1, #CHAR_T
 INC R1
 MOV @R1, #CHAR_E
 INC R1
 MOV @R1, #CHAR_R
 MOV TILE_ROW, #3
 MOV TILE_COL, #1
 ACALL SELECT_TILE
 MOV @R1, #CHAR_SPCH_MARKS
 INC R1
 MOV @R1, #CHAR_9
 INC R1
 MOV @R1, #CHAR_SPCH_MARKS
 INC R1
 MOV @R1, #CHAR_SPACE
 INC R1
 MOV @R1, #CHAR_T
 INC R1
 MOV @R1, #CHAR_O
 MOV TILE_ROW, #4
 MOV TILE_COL, #2
 ACALL SELECT_TILE
 MOV @R1, #CHAR_B
 INC R1
 MOV @R1, #CHAR_E
 INC R1
 MOV @R1, #CHAR_G
 INC R1
 MOV @R1, #CHAR_I
 INC R1
 MOV @R1, #CHAR_N
 MOV TILE_ROW, #5
 MOV TILE_COL, #2
 ACALL SELECT_TILE
 MOV @R1, #CHAR_T
 INC R1
 MOV @R1, #CHAR_U
 INC R1
 MOV @R1, #CHAR_R
 INC R1
 MOV @R1, #CHAR_N
 SETB TEXT
 ACALL UPDATE_LCD_ALL
 
   ;;Receive input
 ENTER3:
 ACALL KEYBOARD
 CJNE A, #'9', ENTER3
 
   ;;Load Player Screen
 MOV MAP_NO, #1
 JNB P1_P2, SKIP5
 MOV MAP_NO, #2
 SKIP5:
 CLR TEXT
 ACALL UPDATE_LCD_ALL
 
   ;;Receive Input
 RECEIVE_INPUT:
 MOV INPUT_I, #0FFH
 MOV INPUT_II,#0FFH
 
 ENTER4:
 ACALL KEYBOARD
 CJNE A, #'8', SKIP03 ;Enter 8 to clear
 SJMP RECEIVE_INPUT
 SKIP03:
 CLR C
 CJNE A, #'A', $+3 ;Check if Digit
 JC TO_INPUT_II
 MOV INPUT_I, A
 SJMP INPUT_DONE_I
 TO_INPUT_II:
 MOV INPUT_II, A
 INPUT_DONE_I:

 ENTER5:
 ACALL KEYBOARD
 CJNE A, #'8', SKIP04 ;Enter 8 to clear
 SJMP RECEIVE_INPUT
 SKIP04:
 CLR C
 CJNE A, #'A', $+3 ;Check if Digit
 JC TO_INPUT_II_
 MOV INPUT_I, A
 SJMP INPUT_DONE_II
 TO_INPUT_II_:
 MOV INPUT_II, A
 INPUT_DONE_II:

 MOV A, INPUT_I
 CJNE A, #0FFH, SKIP6
 SJMP RECEIVE_INPUT
 SKIP6:
 MOV A, INPUT_II
 CJNE A, #0FFH, SKIP7
 SJMP RECEIVE_INPUT
 SKIP7:
 
 ACALL KEYBOARD ;Enter 9 to confirm
 CJNE A, #'9', RECEIVE_INPUT
 ;All inputs are considered. INPUT_I has a letter, and II has a number
 
   ;;Update Model
 MOV A, INPUT_II
 CLR C
 SUBB A, #30H
 MOV TILE_COL, A
 MOV A, INPUT_I
 CLR C
 SUBB A, #16 ; 16? 14?
 CLR C
 SUBB A, #30H
 MOV TILE_ROW, A

 MOV MAP_NO, #2
 JNB P1_P2, SKIP8
 MOV MAP_NO, #1
 SKIP8:
 ACALL SELECT_TILE
 MOV A, @R1
 ADD A, #10
 ;A tile that was hit twice is restored to its original state:
 CLR C
 CJNE A, #HIT_TWICE, $+3
 JC SKIP_CORRECTION
 SUBB A, #10
 INC P2_SHIPS_LEFT;P2 is not hit by P1 on a duplicate
 JNB P1_P2, SKIP_CORRECTION
 DEC P2_SHIPS_LEFT ;Undo erronous adjustment
 INC P1_SHIPS_LEFT ;P1 is not hit by P2 on a duplicate
 SKIP_CORRECTION:
 MOV @R1, A
 
 CJNE A, #HIT_SEA, SKIP10
 SJMP SKIP11
 SKIP10:
 
 JB P1_P2, P1_WAS_HIT
 P2_WAS_HIT:
 DEC P2_SHIPS_LEFT
 SJMP SKIP11
 P1_WAS_HIT:
 DEC P1_SHIPS_LEFT
 SKIP11:
 
   ;;Update Radar
 ;MAP_NO?**
 ;SETB RADAR
 ;ACALL UPDATE_LCD_COL
 ;CLR RADAR
 CLR TEXT
 ACALL UPDATE_LCD_ALL
 
   ;;Wait for input "9" to end turn
 ENTER6:
 ACALL KEYBOARD
 CJNE A, #'9', ENTER6
 
   ;;Check for Winner
 MOV A, P1_SHIPS_LEFT
 JZ END_P2_WIN
 MOV A, P2_SHIPS_LEFT
 JZ END_P1_WIN
   ;;Start next turn
 AJMP MAIN_GAME
 
;END OF GAME

 END_P1_WIN:
 CLR P1_P2
 SJMP ANNOUNCE
 
 END_P2_WIN:
 SETB P1_P2
 SJMP ANNOUNCE
 
   ;;Announce winner of info screen
 ANNOUNCE:
 MOV MAP_NO, #3
 MOV TILE_ID, #BLANK_TILE
 ACALL FILL_MAP

 MOV MAP_NO, #3
 MOV TILE_ROW, #1
 MOV TILE_COL, #3
 ACALL SELECT_TILE
 MOV @R1, #CHAR_P
 INC R1
 MOV @R1, #CHAR_1
 JNB P1_P2, SKIP12
 MOV @R1, #CHAR_2
 SKIP12:
 MOV TILE_ROW, #2
 MOV TILE_COL, #2
 ACALL SELECT_TILE
 MOV @R1, #CHAR_W
 INC R1
 MOV @R1, #CHAR_I
 INC R1
 MOV @R1, #CHAR_N
 INC R1
 MOV @R1, #CHAR_S
 SETB TEXT
 ACALL UPDATE_LCD_ALL

 
   ;;Ask for replay?
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
 MOV DPTR, #DIGITS_SYMBOLS;If the tile ID is not generic...
 MOV TILE_ID, #CHAR_Q_MARK
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
 UPDATE_LCD_COL:
 ;WRITES TO LCD AN ENTIRE COLUMN OF THE SELECTED MAP
 ;**MAP NO, RADAR, AND CHIP SELECT NEED TO BE PREPARED BEFORE CALLING THIS METHOD
 MOV TILE_ROW, #7
 ACALL SELECT_TILE; Last Tile_Col is retained
 
 COL_LOOP:
 MOV TILE_ID, @R1
 JB RADAR, RADAR_MODE
 SJMP MOVE_TO_NEXT
 
 RADAR_MODE:
 MOV A, TILE_ID
 CLR C
 CJNE A, #BLANK_TILE, $+3
 JC MOVE_TO_NEXT ;Dont Overwrite axes (ID < #BLANK_TILE)
 CJNE A, #HIT_SEA, NOT_MISS
 MOV TILE_ID, #RADAR_MISSED
 SJMP MOVE_TO_NEXT
 NOT_MISS:
 CLR C
 SUBB A, #HIT_SEA
 CLR C
 CJNE A, #10, $+3 ;Carry is set if A has a hit ship tile ***
 JNC NOT_HIT
 MOV TILE_ID, #RADAR_HIT
 SJMP MOVE_TO_NEXT
 NOT_HIT:
 MOV TILE_ID, #RADAR_TGT
 SJMP MOVE_TO_NEXT
 
  ;;MOVE TO NEXT TILE
 MOVE_TO_NEXT:
 ACALL WRITE_TILE
 MOV A, R1
 CLR C
 SUBB A,#8
 MOV R1, A
 ;DJNZ TILE_ROW, COL_LOOP
 DEC TILE_ROW
 MOV A, TILE_ROW
 CJNE A, #0FFH, COL_LOOP
 
 RET
 
;=========================================================
 UPDATE_LCD_ALL:
 ;;Clear Disp and Select Chip (SET/CLR TEXT BEFOREHAND)
 ACALL CONFIGURE_LCD
 
 SETB CS_1
 CLR  CS_2
 CLR RADAR

 MOV MAP_NO, #3
 JB TEXT, NEXT_3
 MOV MAP_NO, #1
 JNB P1_P2, NEXT_3
 MOV MAP_NO, #2
 NEXT_3:

 MOV R5, #8
 MOV TILE_COL, #0
 MOV LCD_COUNTER_1, #0B7H

 LCD_LOOP:
 INC LCD_COUNTER_1
 MOV A, LCD_COUNTER_1
 ACALL SEND_CMD
 ACALL UPDATE_LCD_COL
 INC TILE_COL
 DJNZ R5, LCD_LOOP

 JB TEXT, SKIP_CHIP_2
 SETB CS_2
 CLR  CS_1
 CPL P1_P2
 SETB RADAR

 MOV MAP_NO, #1
 JNB P1_P2, NEXT_4
 MOV MAP_NO, #2
 NEXT_4:

 MOV R5, #8
 MOV TILE_COL, #0
 MOV LCD_COUNTER_1, #0B7H

 LCD_LOOP_2:
 INC LCD_COUNTER_1
 MOV A, LCD_COUNTER_1
 ACALL SEND_CMD
 ACALL UPDATE_LCD_COL
 INC TILE_COL
 DJNZ R5, LCD_LOOP_2
 
 CPL P1_P2
 SKIP_CHIP_2:
 RET

 
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
 NOP   
 NOP
 CLR  ENABLE
 RET
;=========================================================
 SEND_BYTE:
 MOV  LCD_PORT, A
 CLR  RD_WR;;  MOV R4,#0;y values
 SETB RS   ; is this DATACOMMAND ?
 SETB ENABLE
 NOP   
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
DB  00H,  18H,    24H,  24H,    24H,  24H,    18H,  00H ; 0
DB  00H,  10H,    10H,  10H,    14H,  18H,    10H,  00H ; 1
DB  00H,  7EH,    04H,  08H,    10H,  20H,    1EH,  00H ; 2
DB  00H,  1CH,    20H,  1CH,    10H,  20H,    3CH,  00H ; 3
DB  00H,  10H,    10H,  3EH,    14H,  18H,    10H,  00H ; 4
DB  00H,  0CH,    10H,  20H,    1CH,  04H,    7CH,  00H ; 5
DB  00H,  3CH,    24H,  3CH,    04H,  04H,    3CH,  00H ; 6
DB  00H,  02H,    04H,  08H,    08H,  10H,    3EH,  00H ; 7
DB  00H,  1CH,    22H,  14H,    08H,  24H,    18H,  00H ; 8
DB  00H,  3CH,    20H,  20H,    3CH,  22H,    3CH,  00H ; 9

DB  00H,  00H,    00H,  00H,    00H,  00H,    00H,  00H ;(space)
DB  00H,  00H,    00H,  00H,    00H,  00H,    00H,  00H ; ?
DB  00H,  00H,    00H,  00H,    00H,  00H,    00H,  00H ; !
DB  24H,  24H,    7EH,  24H,    24H,  7EH,    24H,  24H ; #
DB  00H,  7EH,    00H,  00H,    00H,  00H,    00H,  00H ; _
DB  00H,  00H,    00H,  00H,    66H,  66H,    66H,  00H ; "
DB  00H,  00H,    00H,  00H,    80H,  80H,    80H,  00H ; '
DB  00H,  00H,    00H,  00H,    00H,  00H,    00H,  00H ; *

LETTERS:
DB 0FFH, 0FFH,   0FFH, 0FFH,   0FFH, 0FFH,   0FFH, 0FFH ; (BLANK ROW)
DB  00H,  42H,    42H,  7EH,    42H,  42H,    3CH,  00H ; A
DB  0EH,  12H,    22H,  3EH,    22H,  22H,    1EH,  00H ; B
DB  00H,  7CH,    02H,  02H,    02H,  04H,    78H,  00H ; C
DB  00H,  0EH,    12H,  22H,    22H,  22H,    1EH,  00H ; D
DB  00H,  7EH,    02H,  02H,    7EH,  02H,    7EH,  00H ; E
DB  00H,  02H,    02H,  02H,    7EH,  02H,    7EH,  00H ; F

DB 00H,   7EH,    42H,  7AH,    02H,  02H,    7CH,  00H ; G
DB 00H,   42H,    42H,  42H,    7EH,  42H,    42H,  00H ; H
DB 00H,   1CH,    08H,  08H,    08H,  08H,    1CH,  00H ; I
DB 00H,   55H,   0AAH,  55H,   0AAH,  55H,   0AAH,  00H ; J there is no j
DB 00H,   55H,   0AAH,  55H,   0AAH,  55H,   0AAH,  00H ; K there is no k
DB 00H,   7EH,    02H,  02H,    02H,  02H,    02H,  00H ; L
DB 00H,   42H,    42H,  52H,    6AH,  66H,    42H,  00H ; M

DB 00H,   22H,    32H,  32H,    2AH,  26H,   22H,   00H ; N
DB 00H,   1CH,    22H,  42H,    42H,  22H,   3CH,   00H ; O
DB 00H,   02H,    02H,  3EH,    42H,  42H,   3EH,   00H ; P 
DB 00H,   55H,   0AAH,  55H,   0AAH,  55H,  0AAH,   00H ; Q there is no q
DB 00H,   22H,    12H,  0EH,    12H,  22H,   3EH,   00H ; R
DB 00H,   3CH,    40H,  78H,    04H,  04H,   78H,   00H ; S

DB 00H,   08H,    08H,  08H,    08H,  08H,   3EH,   00H ; T
DB 00H,   3CH,    42H,  42H,    42H,  42H,   42H,   00H ; U
DB 00H,   00H,   0AAH,  55H,   0AAH,  55H,  0AAH,   00H ; V there is no v
DB 00H,   42H,    66H,  5AH,    42H,  42H,   42H,   00H ; W
DB 00H,   55H,   0AAH,  55H,   0AAH,  55H,  0AAH,   00H ; X there is no x
DB 00H,   08H,    08H,  08H,    08H,  14H,   22H,   00H ; Y
DB 00H,   55H,   0AAH,  55H,   0AAH,  55H,  0AAH,   00H ; Z there is no z 
 
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
END
