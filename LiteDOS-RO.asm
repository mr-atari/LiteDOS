;-------------------------------------------------------------------
; LITEDOS SOURCE CODE (C) MR-ATARI 2018
;-------------------------------------------------------------------
; SUPPORT LOAD/SAVE/LIST/ENTER ETC CALLED BY USERS
; SUPPORT BINARY LOAD/EXECUTE (COMMAND-LINE)
;
; COMPATIBLE WITH ANYDOS USING VTOC-SECTORS 360-368 AND SUB-DIRECTORIES
;
; TO INCREASE SUPPORT OF LARGER DISKS, CLUSTERS OF 2 SECTORS MINIMUM ARE USED
; TO MINIMIZE MEMORY USAGE IT USES/SHARES THE CASSETTE BUFFER FOR IO
; TO SUPPORT MORE/LESS FILES, VTOC IS LINKED TO CLUSTERSIZE
;
; MAXIMUM OPEN FILES	: 2, 1 READ/DIR SD/DD(*) + 1 WRITE (SD)
; SPECIAL FUNCTIONS 	: NONE
; MAXIMUM FILES ON DISK : 8-510 (DEPENDING ON CLUSTERS/DISK SIZE)
; CLUSTER SIZE		: 2-128  (256b-32k, DEPENDING ON DISK SIZE)
; MAXIMUM DISK SIZE	: READ 8/16Mb SD/DD(*), WRITE 8Mb SD
; DENSITY SUPPORT	: READ SD/DD(*), WRITE SD
; OS-SUPPORT		: 400/800/XL/XE
;
; (*) CAN BE INCOMPATIBLE WITH RUNNING SOFTWARE, USES PAGE 4 AS READ-BUFFER
;-------------------------------------------------------------------
;	FMS SD-SECTOR LAYOUT
;
;BYTE	FUNCTION
;0-124	125 BYTES OF DATA (MAX)
;125	8 BIT LINK-SECTOR-HI		;LAST SECTOR OF FILE READS ZERO
;126	8 BIT LINK-SECTOR-LO		;LAST SECTOR OF FILE READS ZERO
;127	BYTES IN USE		;LAST SECTOR OF FILE READS LESS THEN 125
;-------------------------------------------------------------------
;	VTOC/DIRECTORY LAYOUT
;	SIZE 2-128 SECTORS, PHYSICAL #256-#383, WRAPS IN CLUSTER
;	VTOC SECTOR ALWAYS #360
;	DIRECTORY SECTOR STARTS AT #361 ALWAYS
;
;BYTE	FUNCTION
;0	FLAG			;%0110.0110	$66 LITEDOS-FILE
;				;%0000.0000	EMPTY
;				;%----.---1	RESERVED
;				;%----.--1-	DOS-2 TYPE
;				;%----.-1--	NO FILE-ID IN SECTOR-LINK
;				;%----.1---	FREE
;				;%---1.----	MYDOS SUBDIRECTORY-LINK
;				;%--1-.----	FILE IN USE
;				;%-1--.----	FILE PROTECTED
;				;%1---.----	DELETED
;1,2	SIZE SECTORS		;LO, HI
;3,4	START FILE/SUB (16 BIT)	;LO, HI
;5-12	FILENAME		;8 CHARACTERS, PADDES WITH SPACE
;13-15	EXT			;3 CHARACTERS, PADDED WITH SPACE
;-------------------------------------------------------------------
;	LITEDOS VTOC LAYOUT, SECTOR 360
;
;BYTE	FUNCTION
;0	DOSTYPE			;MINUS+(CLUSTER-SIZE)
;1,2	DISKSIZE		;USABLE SECTORS (720/1040/X-1)
;3-15	"LiteDOS(c)MrA"		;COPYRIGHTS
;16-31	"------VTOC------"	;TEXT 16 CHAR
;32-95	BITMAP LITEDOS		;64 BYTES EQUALS 512 CLUSTERS (510 FILES)
;96-111 "-----512BIT-----"	;TEXT 16 CHAR
;112	RESERVED		;$40
;113,114FREE SECTORS		;FREE SECTORS ON DISK (NOT CLUSTERS)
;115	RESERVED		;UNDEF
;116-127" FreeSectors"		;TEXT 12 CHAR
;-------------------------------------------------------------------
;	SUPPORTED (*) / DEFAULT (D) / USEFULL FORMATS:
;
;	DOS 2 DISK
;
;DISK	SECTORS	CLUSTER-SIZE	VTOC-SIZE	FILES
;810    720       2 SECTORS      45 BYTES        8   *
;810	720	  4 SECTORS    22.5 BYTES	24   D
;810	720	  8 SECTORS   11.25 BYTES	56   *
;
;1050	1040	  2 SECTORS      64 BYTES	24   * SPECIAL, DOS IS HIGH, ON 1024 AND UP
;1050	1040	  4 SECTORS      32 BYTES	24   D SPECIAL
;1050	1040	  8 SECTORS      16 BYTES	56   * SPECIAL
;
;XF551	720D	  2 SECTORS      45 BYTES	 8   *
;XF551	720D	  4 SECTORS    22.5 BYTES	24   *
;XF551	720D	  8 SECTORS   11.25 BYTES	56   D
;
;	NON DOS 2 DISK
;
;XF551	1440D	  4 SECTORS      45 BYTES	24   *
;XF551	1440D	  8 SECTORS    22.5 BYTES	56   D
;
;XF551	2880D	  8 SECTORS      45 BYTES	56   D
;XF551	2880D	 16 SECTORS    22.5 BYTES	120  *
;
;HDD	2048	  4 SECTORS	64  BYTES	24
;HDD	2048	  8 SECTORS	32  BYTES	56   *
;HDD	4096	  8 SECTORS	64  BYTES	56
;HDD	4096	 16 SECTORS	32  BYTES	120  *
;HDD	8192	 16 SECTORS	64  BYTES	120
;HDD	8192	 32 SECTORS	32  BYTES	248  *
;HDD	16384	 32 SECTORS	64  BYTES	248
;HDD	16384	 64 SECTORS	32  BYTES	504  *
;HDD	32768	 64 SECTORS	64  BYTES	504
;HDD	32768	128 SECTORS	32  BYTES	1016 * (510 USABLE)
;HDD	65536	128 SECTORS	64  BYTES	1016 * (510 USABLE DUE TO BITCOUNT)
;-------------------------------------------------------------------
;	STANDARD OS LABELS
	.INCLUDE LABELS.ASM
;-------------------------------------------------------------------
;	VALUES
DOSSIZ	= $0A			;SIZE FOR LITEDOS
VTODOS	= 360			;VTOC SECTOR ANYDOS
DIRSEC	= 361			;FIRST DIRECTORY SECTOR

;CIO-VALUES
DMODE	= ICAX1Z		;2A MODE
DIOCB	= ICIDNO		;2D IOCB*16
DDATA	= CIOCHR		;2F DATA BYTE

;BUFFER (VECTOR)
;DBUF	= BUFADR		;BUFFER-ADDRESS (USED BY DSKINV)

;DOSTYPE
;LITEDOS			;0-------
;OTHERDOS			;1-------
;+FILE-ID			;11------
;LITEDOS, MINIMUM CLUSTERSIZE	;-0000001 =2
;MAXIMUM CLUSTERSIZE		;-1111111 =128

;BUFFERS
DRBUF	= $400			;128  BYTE READ BUFFER, SD/DD
DWBUF	= DRBUF			;128  BYTE WRITE BUFFER, SD/DD
;BUFADR = $15			;2 BYTE USED IN DSKINV/DIR
EDBUF	= $580			;(LBUFF) SCREEN EDITOR BUFFER
;VTOBUF	= $BD5			;32 BYTE RESERVED FOR VTOC-BITMAP
FILEN	= $332			;11 BYTE FILENAME DECODING

;RE-USED
DSIZE	= DSKFMS		;BLOAD (STA/END), WRITE (LEN-H+L/STA-H+L)
RUNAD	= $2E0			;BLOAD
INITAD	= $2E2			;BLOAD

;DIRECTORY
DSPTR	= FMSZPG+0		;10X CURRENT DIRECTORY POINTER
DSMAX	= FMSZPG+3		;5X TEMP, VTOC SECTORS
HISEC	= FMSZPG+4		;5X TEMP, SIZE-H, LEADING SPACES

;READING
RBUFFER	= FMSZPG+1		;5X READ-BUFFER USAGE
DRPTR	= FMSZPG+2		;8X BUFFER POINTER

;DUP
;DRV1	= -CODE-		;3X SOURCE DRIVE

;WRITING
;WBUFFER = RBUFFER		;7X WRITE-BUFFER USAGE
;DWPTR	= DRPTR			;7X BUFFER POINTER
;FMSNE1	= FMSZPG+3		;10X NEXT FREE SECTOR
;FMSNE2	= FMSZPG+4		;6X
;DWDIR	= FMSZPG+5		;4X WRITE FILE-POINTER
;DWSEC	= FMSZPG+6		;2X WRITE DIR-SECTOR

;-------------------------------------------------------------------
;	BOOTLOADER
;-------------------------------------------------------------------

	*= $680

;-------------------------------------------------------------------
;	1-3 DISK BOOT SECTOR, LOADS INTO WRITE-BUFFER.
;-------------------------------------------------------------------
FMS	.BYTE	0
	.BYTE	1
	.WORD	$400	
	.WORD	INIT
;-------------------------------------------------------------------
;LOADER  $400 (BUFFER)
;-------------------------------------------------------------------
;	BOOT ROUTINE
;-------------------------------------------------------------------
	LDX	#WMES-$280&$FF	;SEND OUT WELCOME MESSAGE
	LDY	#WMES-$280/256

	STX	ICBAL		;LO
	STY	ICBAH		;HI
	STX	ICBLL		;LENGTH ($60)
	LDX	#SEIOCB		;SCREEN
	LDA	#PUTREC		;SEND
	STA	ICCOM		;COMMAND
	JSR	CIOV		;DO IT

;	AFTER 1 SECTOR BOOT, WE ONLY NEED TO SET DBUFFER

	LDA	#7		;BUFFER $0700 UP
	STA	DBUFHI

;	CONTINUE BOOTING AFTER THE WELCOME MESSAGE

	LDA	DVSTAT		;STATUS
	BPL	BOOTL
	LDA	#$04		;04 MD SPECIAL FORMAT...
	STA	DAUX2

BOOTL	INC	DAUX1		;NEXT SECTOR $0402 or $0002
	JSR	DSKINV		;GET THIS SECTOR

	CLC
	LDA	DBUFLO		;INC BUFFER
	ADC	DBYTLO
	STA	DBUFLO
	LDA	DBUFHI
	ADC	DBYTHI
	STA	DBUFHI
	CMP	#DOSSIZ		;DONE?
	BNE	BOOTL	

;	SET DUP-VECTOR (DOS-COMMAND)

	LDA	#DUPV&$FF
	STA	DOSVEC
	LDA	#DUPV/256
	STA	DOSVEC+1

;	EXIT BOOT-STRAP

	LDA	DWBUF+$7F	;BOOTFLAG
	BEQ	NOAUTO

	JSR	INIT		;INSTALL D:
	JSR	DUPCOM		;COPY "???????????"
	JSR	BLOAD		;LOAD FIRST FILE IT FINDS....

NOAUTO	CLC			;BOOT SUCCESFUL
	RTS

;-------------------------------------------------------------------
WMES	.BYTE	"LiteDOS-RO (c) Mr.Atari 2019",$9B
;-------------------------------------------------------------------

;-------------------------------------------------------------------
;	LITEDOS STARTS HERE
;-------------------------------------------------------------------

	*= $700

;-------------------------------------------------------------------
;	DEVICE-HANDLER-TABLES
;-------------------------------------------------------------------
HDISK	.WORD	DOP-1		;OPEN FILE
	.WORD	DCL-1		;CLOSE FILE
	.WORD	DGB-1		;GET BYTE
	.WORD	DIL-1		;PUT BYTE (OVERWRITTEN IN DIL-1)
	.WORD	DIL-1		;DST-1;FILE STATUS
	.WORD	DIL-1		;DSP-1;SPECIAL
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	CLOSE COMMAND
;-------------------------------------------------------------------
DCL	CPX	RBUFFER		;CHECK IF VALID CLOSE
	BNE	DCL3		;OTHER IOCB (OS: CLOSE AFTER FAILURE TO OPEN)

FCIOCR	LDA	#$FF		;FREE BUFFER
	STA	RBUFFER

DCL3	LDY	#1		;OK
DGBERR	RTS
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	NOT IN USE...
;-------------------------------------------------------------------
;	STATUS COMMAND
;-------------------------------------------------------------------
;
;-------------------------------------------------------------------
;	SPECIAL COMMAND
;-------------------------------------------------------------------
;	NOT IN USE...
;-------------------------------------------------------------------
;	XIO
;	32	RENAME
;	33	DELETE
;	35	LOCK FILE
;	36	UNLOCK FILE
;	39	LOAD/INIT/RUN EXE/OBJ/COM-FILE
;-------------------------------------------------------------------
DIL	LDY	#146		;HANDLER/FUNCTION NOT IMPLEMENTED
	RTS
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	PUT/GET BYTE COMMAND
;-------------------------------------------------------------------
DGB	CPX	RBUFFER		;CHECK IF VALID IOCB
	BNE	DIL
DGBF	LDY	DMODE		;CHECK READ/DIR
	CPY	#6
	BEQ	GETDIR		;DIR  (SAME)
	;BCC	DGB1		;READ (LOWER)
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	CONTINUE GET BYTE COMMAND
;-------------------------------------------------------------------
DGB1	LDY	DRPTR		;POINTER
	CPY	DRBUF+127	;CHECK BYTES IN BUFFER
	BNE	DGB3		;NOT DONE

;	BUFFER USED, READ NEXT IO-SECTOR
	
	LDA	DRBUF+127-1	;EXTRACT NEXT SECTOR-LO
	STA	DAUX1		;STORE SECTOR-LO
	LDA	DRBUF+127-2	;EXTRACT NEXT SECTOR-HI
	AND	#$03		;ALWAYS
	STA	DAUX2		;STORE SECTOR-HI
	ORA	DAUX1		;CHECK IF NO MORE SECTORS
	BEQ	DIREOF		;SEND OUT EOF

	JSR	REABUF		;READ NEXT SECTOR
	BMI	DGBERR		;IO-ERROR

	DEY			;Y=1
	STY	DRPTR		;RESET POINTER

;	BYTE FROM BUFFER

DGB3	LDA	DRBUF,Y		;DATA

;	EXIT (BOTH)

DGB4	INC	DRPTR		;NEXT BYTE FROM BUFFER
DCL33	BNE	DCL3		;OK
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	GET BYTE COMMAND, FROM DIRECTORY-BUFFER, ATARI DIR-FORMAT...
;-------------------------------------------------------------------
GETNXT	LDA	DSPTR		;CHECK IF FREE SECTORS DONE
	BMI	DIREOF		;$80, YES, DO EOF
	JSR	SEANXT		;NO, READ NEXT ENTRY
	BPL	GETNOK		;OK, Y=0
	JSR	REAVTO		;ON ERROR, GET FREE SECTORS FROM VTOC
	LDA	DRBUF		;CHECK LITEDOS
	BPL	DIREOF		;NO, EXIT
	LDX	#$70		;POINTER IN BUFFER
	JSR	DOSIZ		;DO SIZE, Y=0
	LDA	#$71		;POSITION "XXX FREE SECTORS"
	STA	DSPTR		;STORE
	STY	HISEC		;NO SPACES
GETNOK	STY	DRPTR		;Y=ZERO

GETDIR	LDA	DRPTR		;GET POINTER (ERROR-CODE ON OPEN OR VALID)
	BMI	GETNXT		;ERROR ON OPEN, PAST POS8, EOL.
	CMP	#$0F		;15 CHARACTERS OUTPUT, THEN EOL
	BEQ	GETEOL		;DO EOL
	DEC	HISEC		;LEADING SPACES
	BMI	NOSPACE		;NO
	LDA	#$20		;SPACE
	BNE	DCL33		;SEND OUT (OK)
NOSPACE	LDA	DSPTR		;CHECK WRAPOVER
	TAY			;STORE (IN Y)
	AND	#$0F		;MASK 4 BIT
	BNE	GETCHA		;NO
	TYA			;RESTORE
	SBC	#$0F		;RESET, C=0
	TAY			;IN Y

GETCHA	LDA	DRBUF,Y		;OUTPUT CHR
	INY
	STY	DSPTR		;STORE NEXT POSITION, POSSIBLE WRAP-OVER
	BNE	DGB4		;SEND OUT (ALWAYS)

GETEOL	LDA	#$9B		;EOL CHR
	STA	DRPTR		;MARKER
	BNE	DGB4		;SEND OUT

DIREOF	LDY	#136		;EOF
	RTS
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	SUBROUTINES
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	BINARY LOAD
;-------------------------------------------------------------------
BLOAD	JSR	FCIOOR		;OPEN FOR READ, FAKE IOCB #1
	BMI	DUPCC		;ERROR-Y

	LDA	#NOMEM&$FF	;DEFAULT = RTS
	STA	RUNAD		;RUN-ADDRESS
	LDA	#NOMEM/256
	STA	RUNAD+1

	JSR	GET2BYT		;LOAD HEADER
	BEQ	BLOAD0		;CHECK BINARY FILE
	LDY	#175		;BAD LOAD FILE
DUPCC	RTS			;Y=ERROR-CODE

BLOAD1	JSR	INIDOS		;PARTIAL RUN IF INITAD IS SET

BLOAD0	LDA	#NOMEM&$FF	;DEFAULT = RTS
	STA	INITAD		;INIT-ADDRESS
	LDA	#NOMEM/256
	STA	INITAD+1

BLOAD2	JSR	GET2BYT		;GET 2 BYTES (START-ADDRESS)
	BEQ	BLOAD2		;HEADER $FFFF DETECTED, GET NEXT 2 BYTES

	JSR	FCIOR		;GET NEXT 2 BYTES (END-ADDRESS)
	STA	DSKFMS+2
	JSR	FCIOR
	STA	DSKFMS+3

;	LOAD THIS BLOCK

NEXT	JSR	FCIOR		;GET A BYTE
	BMI	BLOAD3		;DONE ON ANY ERROR
	DEY			;Y=1
	STA	(DSKFMS),Y	;STORE

DONEND	LDA	DSKFMS+0	;CHECK END OF BUFFER
	CMP	DSKFMS+2
	LDA	DSKFMS+1	;HI
	SBC	DSKFMS+3
	BCS	BLOAD1		;YES, RUN THIS CODE / LOAD NEXT BLOCK

GETBYT	INC	DSKFMS+0
	BNE	NEXT
	INC	DSKFMS+1
	BCC	NEXT		;ALWAYS

;	RUN THE LOADED FILE...

BLOAD3	JSR	FCIOC		;CLOSE BUFFER
	JMP	(RUNAD)		;DEFAULT = RTS

;	DO PARTIAL CODE...

INIDOS	JMP	(INITAD)	;DEFAULT = RTS
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	GET 2 BYTES...
;-------------------------------------------------------------------
GET2BYT	JSR	FCIOR
	STA	DSKFMS+0
	JSR	FCIOR
	STA	DSKFMS+1

	AND	DSKFMS+0	;$FF
	CMP	#$FF		;CHECK BINARY FILE/HEADER

NOMEM	RTS
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	IO-READ RELATED ROUTINES
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	READ VTO-SECTOR / DIR-SECTOR / READ
;-------------------------------------------------------------------
REAVTO	LDA	#VTODOS&$FF	;SET READ VTOC
;-------------------------------------------------------------------
READIR	STA	DAUX1		;SET DIR-SECTOR
	LDA	#VTODOS/256
	STA	DAUX2		;FIXED
;-------------------------------------------------------------------
;	READ (DAUX)
;-------------------------------------------------------------------
REABUF	LDX	#READ		;COMMAND
	LDA	#GETDAT		;DATA SIO->ATARI
;-------------------------------------------------------------------
;	WRITE ENTRY
;-------------------------------------------------------------------
WRITEN	STA	DSTATS		;DIRECTION
	STX	DCOMND		;COMMAND

IOREAD	LDA	#$4		;SET CORRECT BUFFER-AREA
	STA	DBUFHI		;$7 OR $F, X=DENSITY-1
	LDY	#$00
	STY	DBUFLO		;BUFLO ALWAYS ZERO...

	LDX	#$80		;DENSITY READ-FILE
RWBUF2	STY	DBYTHI		;(DSCTLN+1)
	STX	DBYTLO		;(DSCTLN)

	LDA	ICDNOZ		;DRIVE ID
	STA	DUNIT		;DRIVE-ID

	LDA	#DIOT		;TIME-OUT
	STA	DTIMLO

	LDA	#DISKID		;DEVICE D:
	STA	DDEVIC

	JMP	SIOV		;SIO-VECTOR/RTS
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	FAST CIO, USES IOCB #1, FORCED
;-------------------------------------------------------------------
;	ZP PARAMETER SETUP, IOCB #1

SETO	LDA	#$31		;DEFAULT DRIVE
	AND	#$0F		;MAKE VALID ID
	STA	ICDNOZ		;SET DRIVE
	STY	DMODE		;SET MODE
	LDX	#$10		;USE IOCB #1
	RTS

DRV1	= SETO+1		;SOURCE DRIVE

;	CLOSE READ/DIR

FCIOC	JSR	FCIOCR		;FREE BUFFER / A=$FF, Y=1
	STA	$350		;CLOSE IOCB #1
	RTS	

;	GET READ/DIR

FCIOR	LDY	$35A		;OPEN TYPE (READ/DIR) FROM IOCB #1
	JSR	SETO		;SETUP ZP
	JMP	DGBF		;GET BYTE ROUTINE

;-------------------------------------------------------------------
;	ABOUT $84F... FROM HERE, ALL CAN BE ERASED, TO RUN A GAME
;-------------------------------------------------------------------

;	OPEN READ/DIR

FCIOOD	LDY	#6		;OPEN TYPE = DIR
	BNE	FCIOO
FCIOOR	LDY	#4		;OPEN TYPE = FILE
FCIOO	JSR	SETO		;SETUP ZP

;	SETUP IOCB ...

	STY	$35A		;OPEN-TYPE	;IOCB-#1 COMPATIBILITY OPEN/READ
	STA	$351		;DRIVE-ID	;IOCB-#1 COMPATIBILITY OPEN/READ
IDIDID	LDY	#$0F		;*		;INIT/RESET SETS CORRECT HANDLER-ID
	STY	$350		;HANDLER-POS	;IOCB-#1 COMPATIBILITY OPEN/READ

;	OPEN FILE

	LDA	#EDBUF+2&$FF	;SET NAME-POINTER
	STA	ICBALZ
	LDA	#EDBUF+2/256	;FCIO DOES NOT USE "Dx:" HEADER
	STA	ICBAHZ
	JMP	DOPF		;DO OPEN
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	LITEDOS-SHELL
;-------------------------------------------------------------------

;OPENED	LDA	$E411
;	PHA
;	LDA	$E410
;	PHA
;	RTS

;-------------------------------------------------------------------
;	DUP ENTRY
;-------------------------------------------------------------------
DUPV	LDA	#EDBUF&$FF	;ALL IO, EDBUF
	STA	ICBAL
	LDA	#EDBUF/256
	STA	ICBAH

;	DIRECTORY

WILD	JSR	DUPCOM		;USE WILDCARD "???????????"

VALIDN	JSR	FCIOOD		;OPEN DIR

DUPDIRC	JSR	FCIOR		;GET CHARACTER
	BMI	NOSAV		;EOF
	DEY			;0 LENGTH
	LDX	#PUTCHR		;PUT
	JSR	SCRCHR		;SCREEN
	BPL	DUPDIRC		;LOOP

;	GET USER INPUT

NOSAV	LDX	#GETREC		;GET RECORD
	JSR	SCRCOM	
DUPDUPL	BMI	NOSAV		;DO NOT ACCEPT BREAK

;	LOOK FOR COMMAND

	LDA	EDBUF+0		;FIRST CHARACTER
	BMI	WILD
	CMP	#$31		;CHECK FOR VALID DRIVE-ID
	BCC	NOTVAL		;NO
	CMP	#$39
	BCS	NOTVAL		;NO

	STA	DRV1		;VALID, CHANGE DEFAULT

	LDX	ICBLL		;LENGTH OF INPUT, CHECK FILENAME
	CPX	#4		;POSITION OF EOL EQUALS +1
	BCS	VALIDN		;YES
	BCC	WILD		;NO

NOTVAL	JSR	BLOAD		;RETURN (RTS) HERE WHEN NO RUN-ADDRESS
	BMI	NOSAV
	JMP	DUPV
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	FILENAME BUFFER
;-------------------------------------------------------------------
DUPCOM	LDY	#10		;COPY FILENAME "???????????" TO BUFFER
	LDA	#'?'		;WILDCARD CHARACTER
DUPCO2	STA	EDBUF+2,Y
	DEY
	BPL	DUPCO2
NOBAD	RTS
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	SCREEN OUTPUT
;-------------------------------------------------------------------
SCRCOM	DEY			;$00->$FF
SCRCHR	STY	ICBLL		;LENGTH
	
	STX	ICCOM		;COMMAND
	LDX	#0		;IOCB-0
	JMP	CIOV		;RTS
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	INIT/RESET (RESPECT OTHER DEVICES, 850 COMPATIBLE)
;-------------------------------------------------------------------
;	CLEAR BUFFER-STATUS, CHECK DEVICE-TABLE (SET LOMEM), INSTALL DEVICE 

INIT	LDX	#$FF
	STX	RBUFFER		;FREE BUFFER(S)
	INX			;X=0

;	SETUP MEMORY

DODODO	LDA	#DOSSIZ		;CHECK IF MEMLO IS ALREADY SET HIGHER
	CMP	MEMLO+1
	BCC	ININXT		;YES
	STA	MEMLO+1		;NO, SET HI
	STX	MEMLO		;SET LO

;	SEARCH DEVICE

ININXT	INX
	INX
	INX

	LDA	HATABS,X	;IS THERE A DEVICE PRESENT ?
	BEQ	INIOKE		;NO
	CMP	#'D'		;CHECK DOUBLE D:
	BNE	ININXT

;	SETUP DEVICE-TABLE

INIOKE	LDA	#'D'
	STA	HATABS,X	;SET D: IN TABLE
	LDA	#HDISK&$FF
	STA	HATABS+1,X	;$0
	LDA	#HDISK/256
	STA	HATABS+2,X	;$8

	STX	IDIDID+1	;STORE HANDLER-ID USED IN FCIO

;	SET DUP-VECTOR (DOS-COMMAND)
;
;	LDA	#DUPV&$FF
;	STA	DOSVEC
;	LDA	#DUPV/256
;	STA	DOSVEC+1

	RTS
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	OPEN COMMAND
;-------------------------------------------------------------------
DOP	LDY	#161		;TO MANY OPEN FILES
	LDA	RBUFFER		;CHECK BUFFER
	BPL	DOPTOO
DOPF	STX	DIOCB		;IOCB

;	LDA	ICDNOZ		;DRIVE-ID
;	STA	DUNIT
;	LDA	#STATC
;	STA	DCOMND
;	JSR	DSKINV		;DO STATUS
;	BMI	DOPER		;BAD DRIVE

REDO	JSR	REAVTO		;GET DOSTYPE, TRIP ON DD-DISK
	BMI	DOPER		;IO-ERROR

	LDA	DMODE		;CHECK OPEN-BITS
	CMP	#%00000111	;READ OR WRITE?
	BCC	DOP1		;LOWER=READ/DIR
	JMP	DIL

DOP1	JSR	SDIR		;SEARCH FILENAME IN (SUB)DIRECTORY
	STY	DRPTR		;ZERO (DIR), POINTER (x0) OR ERROR (-).
		
	LDA	DMODE		;OPEN DIRECTORY, DONE, POINTER SET (OR ERROR-CODE)
	AND	#2
	BNE	DOPDIR		;ERROR-CODE WILL SEND OUT " FreeSectors" FROM SECTOR 360

DOPTOO	TYA			;CHECK ERROR, GET POINTER
	BMI	DOPER

	;LDX	DRDEN		;DENSITY-1
	STA	DRBUF+127-0	;BUFFER SIZE (POINTER; SETS STATUS BUFFER USED)
	LDA	DRBUF+3,Y	;START-SECTOR
	STA	DRBUF+127-1	;LO
	LDA	DRBUF+4,Y	;START-SECTOR
	STA	DRBUF+127-2	;HI

;	ALL IS OK, NOW SET BUFFER IN USE.

DOPDIR	LDA	DIOCB		;IOCB-ID
	STA	RBUFFER		;STORE

;	EXIT

	LDY	#1		;OK
DOPER	RTS
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	DECODE FILENAME, SEARCH DIRECTORY, CALCULATE SIZE
;-------------------------------------------------------------------
SDIR	LDX	#8		;DOS 2.X
	LDA	DRBUF
	BPL	NOLITE
	AND	#$7F		;LITEDOS
	TAX
NOLITE	INX			;ADD 1
	STX	DSMAX		;MAXIMUM DIR-SECTORS +1

SREDO	LDY	#0		;CHAR-POINTER

ODISTA	LDX	#$F5		;FILENAME-POINTER $Fx-56789ABC.EF0

ODICHR	LDA	(ICBALZ),Y	;GET CHR IN FILENAME
	;BNE	ODISKP		;ZERO -> SPACE
	;LDA	#$20
ODISKP	INY			;NEXT INPUT CHARACTER

;	":" <CR>  "."  "*" SPECIAL, REST ARE ALL VALID

	CMP	#':'		;START MARKER
	BEQ	ODISTA		;RESET/START(AGAIN)
 
ODICOD	CMP	#$9B		;CHECK EOL
	BNE	ODINOS		;ACCEPT AS SPACE, FORCES START
ODISPC	LDA	#$20		;PAD WITH SPACE
ODIFIL	DEY			;STAY AT THIS CHR ('CR' '.')

ODINOS	CMP	#'.'		;CHECK '.', FORCE POSITION TO EXT
	BNE	ODINOD
	CPX	#$F5+8		;NAME->EXT
	BCC	ODISPC		;NOT YET, FILL SPACE STAY AT '.'
	;LDX	#$F5+9		;??? TRUNC SURPLUS-NAME TO 8 CHARACTERS
	BCS	ODICHR		;DONE, DON'T USE THIS CHARACTER.

ODINOD	CMP	#'*'		;SPECIAL CHAR, REPLACE '*' BY PADDED '?'
	BNE	ODINAM
	LDA	#'?'		;REPLACE WITH "?"
	CPX	#$F5+7		;LAST CHAR OF NAME
	BEQ	ODINAM
	CPX	#$F5+10		;LAST CHAR OF EXT
	BCC	ODIFIL		;NO, 0-6/8-9 STAY ON THIS CHAR

;	ROUTINE IS NOT PERFECT, IT ACCEPTS ILLIGAL NAMES.

ODINAM	STA	FILEN-$F5,X	;NAME
	INX			;NEXT DECODED CHARACTER
	BNE	ODICHR		;X=0 <11 CHAR> DECODED

;	CONTINUE WITH DIR SECTOR

DIRATA	INC	DAUX1		;NEXT VTOC
	BNE	DIRATA3
	INC	DAUX2
DIRATA3	DEC	DSMAX		;NUMBER OF DIR SECTORS
	BEQ	DIRFNF		;SENDOUT FILE NOT FOUND

;	READ DIR SECTOR

DIROK	JSR	REABUF		;GO READ SECTOR FROM DISK
	BMI	DIRERR		;IO-ERROR
	TYA			;Y=1
	BPL	SEANX3

;	SCAN ENTRIES

SEANXT	LDA	DSPTR		;IN USE...
SEANX2	ADC	#$10		;CLC ???
SEANX3	AND	#$F0		;MASK TOP
	TAY
	BMI	DIRATA		;NEXT SECTOR

;	CHECK FILE-STATUS

	STY	DSPTR		;DIR SEARCH POINTER

;	FIRST, SET CORRECT BUFFER IN CODE, MUST BE PAGE-START...

;	LDX	DMODE		;WHAT TYPE OF MODE

;	GET STATUS

DIRREA	LDA	DRBUF,Y		;READ/WRITE-BUFFER, CHECK IF FILENAME IN USE
;	BEQ	DIRWRI		;EMPTY
;	BPL	DIRSEA1		;FILE PRESENT, CHECK NAME
;
;DIRWRI	CPX	#9		;DMODE	;WRITE-STEP 2, DELETED OR EMPTY FILE?
;	BEQ	DIRERR		;YES, OK, ANY DELETED OR EMPTY FILE WILL DO FINE.
;
;	TAX			;WRITE-STEP 1 OR READ, MINUS OR DELETED ?
	BMI	SEANXT		;DELETED, SKIP THIS FILE.
	BNE	DIRSEA1

;	FILE NOT FOUND, CHECK SUBDIR ACCESS (MYDOS), REMOVED

DIRFNF	
DIRFFF	LDY	#170		;SET FILE NOT FOUND
DIRERR	RTS			;SDIR REPLY (IO-ERROR)

;	CHECK SUB-DIR AND PROCESS IT, SUPPORTS ONLY 1 LEVEL...

DIRSEA1

;	CHECK NAME

DIRSEA2	LDX	#0		;01234567890
CHKCHK	INY			;----0011
CHKCHR	LDA	FILEN,X		;GET CHR IN FILENAME
	CMP	#'?'		;CHECK WILDCARDS
	BEQ	DUMMYOK		;? ALWAYS OK
CHKCHKB	CMP	DRBUF+4,Y	;READ/WRITE-BUFFER
	BNE	SEANXT		;NO, DO NEXT FILE ENTRY
DUMMYOK	INX
	CPX	#11
	BNE	CHKCHK

	LDY	DSPTR		;SDIR REPLY ON SUCCES, POINTER IN Y (PTR)

;	DIRECTORY EXTRAS

	LDA	DMODE
	AND	#2
	BEQ	DIREND		;RTS

;	FORMAT OUTPUT, CALCULATE SIZE AND SET SPACES

	LDX	DSPTR		;MUST BE X, INC $,X IS USED...

DOSIZ	LDA	DRBUF+1,X	;VALUE-LO
	LDY	DRBUF+2,X	;VALUE-HI
	CPY	#3
	BCC	TOOPER		;PERFECT
	BNE	TOOHII		;TOO LARGE
TOOSML	CMP	#$E7		;CHECK LO
	BCC	TOOPER		;PERFECT
TOOHII	LDY	#3		;MAX-HI
	LDA	#$E7		;MAX-LO
TOOPER	STY	HISEC		;STORE ???

	LDY	#3		;FIRST DECTAB
NEXDIG	PHA			;STORE
	LDA	#'0'		;START WITH ZERO
	STA	DRBUF+1,X	;STORE
	PLA			;RELOAD
NEXVAL	CMP	DECTAB-1,Y	;CAN WE SUBTRACT
	BCC	NEXDEC		;NO
NEXHIG	SBC	DECTAB-1,Y	;YES, DO IT
	INC	DRBUF+1,X	;INCREASE
	BNE	NEXVAL		;ANOTHER TIME?
NEXDEC	SEC			;PREPARE TO REDO, SO SET THE CARR...
	DEC	HISEC		;LOOP HERE UNTIL ALL 100'S ARE DONE.
	BPL	NEXHIG		;THIS ONLY HAPPENS WHEN WE ARE DOING 100'S
NEXSML	INX			;NEXT POSITION
	DEY			;NEXT DECTAB
	BNE	NEXDIG		;LOOP

	INX
	INX
	STX	DSPTR		;CHANGE POINTER TO POSITION IN WORD

;	DONE, NOW INSERT SPACE AND SET LEADING SPACES

SIZEND	LDA	#$20		;SPACE
	STA	DRBUF-5,X	;BEFORE
	LDA	#2
	STA	HISEC		;2 SPACES

DIREND	RTS			;Y=0 REPLY WHEN DOING DIR
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;VTOMSK	.BYTE	128,64,32,16,8,4,2;,1	;BIT-VALUES
DECTAB	.BYTE	1,10,100		;DEC-VALUES
;-------------------------------------------------------------------


;-------------------------------------------------------------------
	*= $DFF
	.BYTE	0
;-------------------------------------------------------------------


;-------------------------------------------------------------------
;	LOAD BASIC FILE
;-------------------------------------------------------------------
;
;DUPBAS	ORA	DSKFMS		;BASIC = $00/$00
;DUPEE	BNE	DUPVB		;ERROR, NO BASIC-FILE	
;
;	LDY	#12		;COPY [BASIC PAR]+[MEMLO] -> $80-$8D
;DUPBLO	CLC
;	LDA	DRBUF,Y		;LO
;	ADC	MEMLO
;	STA	$80,Y
;	LDA	DRBUF+1,Y	;HI
;	ADC	MEMLO+1
;	STA	$80+1,Y
;	DEY
;	DEY
;	BPL	DUPBLO		;$80-$8D
;
;	LDA	$82		;LOAD ADDRESS
;	STA	ICBAL+16
;	LDA	$83
;	STA	ICBAH+16
;
;	LDA	#14		;SHORT-CUT...
;	STA	DRPTR		;SET POINTER AFTER HEADER
;
;	SEC			;CALCULATE FILE SIZE
;	LDA	DRBUF+12
;	SBC	DRBUF+2
;	STA	ICBLL+16
;	LDA	DRBUF+13
;	SBC	DRBUF+3
;	STA	ICBLH+16
;
;	JSR	DO1REA		;READ #1
;	BMI	DUPEE		;ERROR
;
;DUPBASE	LDA	$8C
;	STA	$8E
;	STA	$90
;	;STA	$0E		;???
;	LDA	$8D
;	STA	$8F
;	STA	$91
;	;STA	$0F		;???
;
;	JMP	WARMSV		;RESET/RETURN
;
;-------------------------------------------------------------------
;
;	LDA	DRDOS		;18 BYTES
;	CMP	#$C1		;CHECK FOR DOS 1.0
;	BNE	NODOS1		;OTHER TYPE
;	LDA	DRBUF+127	;GET BYTES IN SECTOR, ZERO MEANS FULL SECTOR
;	AND	#$7F		;MASK END OF FILE BIT, MINUS=SHORT SECTOR
;	BNE	DOS1SEC		;VALUE IS NOW OK TO PROCESS
;	LDA	#$7D		;ELSE SET 125, FIXED BYTES
;DOS1SEC	STA	DRBUF+127	;OVERWRITE WITH CORRECT BUFFER-LENGTH
;
;-------------------------------------------------------------------

	.END
