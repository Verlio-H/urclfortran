C*********************************************************************72
C TYPES TO NUMB TABLE
C 1 - CHARACTER (STRING)
C 2 - INTEGER
C 3 - LOGICAL (BOOL) (NOT IMPLIMENTED)

      PROGRAM URCLCOMPILE
      IMPLICIT NONE
C     PROGRAM TO COMPILE FORTRAN TO URCL
      CHARACTER (LEN = 72) LINE
      CHARACTER (LEN = 5) LABEL
      CHARACTER (LEN = 6) VARS(64)
      CHARACTER (LEN = 72) TMPSTR
      CHARACTER (LEN = 72) TMPSR2
      CHARACTER (LEN = 72) TMPSR3
      CHARACTER (LEN = 8) FUNCS(99)
      INTEGER FNTYPE(99)
      INTEGER FNARGS(99)
      INTEGER VARADR(64)
      INTEGER VARTYP(64)
      INTEGER TYPES(64)
      INTEGER N
      INTEGER LINEN
      INTEGER TEMP
      INTEGER TEMP2
      INTEGER CRTVAR
      INTEGER VARPTR
C SHUNTING YARD
      INTEGER TMPI
      INTEGER TMPI2
      INTEGER TMPI3
      INTEGER OUT(128)
      INTEGER STACK(128)
      INTEGER OP
      INTEGER SP
      INTEGER TYPP
      REAL TMPR
      LOGICAL NALLOW
      LOGICAL IFUNCS(99)
      INTEGER RETTYP
      CRTVAR = 0
      LINEN = 0
      VARPTR = 1
      OPEN (UNIT=8, FILE='SOURCE.F', ACTION='READ') 
      OPEN (UNIT=9, FILE='OUTPUT.URCL', ACTION='WRITE')
      WRITE (9, '(A)') 'BITS 32', 'MINREG 4', 'MINHEAP 1024', 
     1'MINSTACK 32', ' '
      TEMP = 1
  999 IFUNCS(TEMP) = .FALSE.
      TEMP = TEMP + 1
      IF (TEMP.NE.100) GOTO 999
      GOTO 50000
 1000 READ (8, '(A)', ERR=1, END=2, IOSTAT=N) LINE 
      LINEN = LINEN + 1
C     WRITE (*, '(A)') LINE 
      IF (LINE(:1).EQ.'C') GOTO 1000 
      IF (LINE(6:6).NE.' ') GOTO 3 
      IF (LINE(:5).NE.'     ') GOTO 1001 
 2001 LINE = ADJUSTL(LINE(6:))
      IF (LINE(:3).EQ.'END') GOTO 51000
      IF (LINE(:4).EQ.'STOP') GOTO 1003
      IF (LINE(:7).EQ.'PROGRAM') GOTO 1000
C TBH PROGRAM DOESN'T REALLY MEAN ANYTHING
      IF (LINE(:9).EQ.'CHARACTER') GOTO 3000
      IF (LINE(:7).EQ.'INTEGER') GOTO 3001
      IF (LINE(:4).EQ.'GOTO') GOTO 4000
      IF (LINE(:2).EQ.'IF') GOTO 5000
      IF (LINE(:5).EQ.'WRITE') GOTO 6000
      GOTO 3002
 9999 CLOSE (8)
      CLOSE (9)
      STOP 
CALCULATE LINE LABELS
 1001 LABEL = ADJUSTL(LINE(:5))
      WRITE (9, '(A)') '.LABEL_'//LABEL
      GOTO 2001
C STOP
 1003 WRITE (9, '(A)') 'HLT'
      GOTO 1000
CHARACTER/STRING VARIABLES
 3000 IF (TRIM(LINE(10:)).EQ.'') GOTO 4
      LINE = ADJUSTL(LINE(10:))
      TEMP = 1
      IF (LINE(1:4).EQ.'(LEN') GOTO 3010
      IF (LINE(1:1).EQ.'*') GOTO 3009
 4010 VARS(VARPTR) = LINE(:6)
      VARADR(VARPTR) = CRTVAR
      TEMP2 = TEMP + 2000
      VARTYP(VARPTR) = TEMP2
      VARPTR = VARPTR + 1
      VARS(VARPTR) = ''
      CRTVAR = CRTVAR + TEMP
      GOTO 1000
 3009 LINE = '    ='//LINE(2:)
 3010 LINE = LINE(5:)
      LINE = ADJUSTL(LINE)
      IF (LINE(1:1).NE.'=') GOTO 4
      LINE(1:) = LINE(2:)
      LINE = ADJUSTL(LINE)
      TEMP = INDEX(LINE,')')-1
      IF (TEMP.EQ.-1) TEMP = INDEX(LINE, ' ')-1
      TMPSTR = LINE(1:TEMP)
      TEMP = TEMP + 2
      LINE = LINE(TEMP:)
      LINE = ADJUSTL(LINE)
      READ (TMPSTR, *) TEMP
      GOTO 4010
C INTEGER VARIABLES
 3001 LINE = ADJUSTL(LINE(8:))
      VARS(VARPTR) = LINE(:6)
      VARADR(VARPTR) = CRTVAR
      VARTYP(VARPTR) = 1000
      VARPTR = VARPTR + 1
      CRTVAR = CRTVAR + 1
      GOTO 1000
C ASSIGNMENT
 3002 TEMP = INDEX(LINE, '=')-1
      TMPSTR = LINE(1:TEMP)
      N = 0
 3102 N = N + 1
      TMPSR2 = VARS(N)
      IF (N.EQ.65) GOTO 4
      IF (TMPSR2(1:6).NE.TMPSTR(1:6)) GOTO 3102
      TEMP = TEMP + 2
      LINE = LINE(TEMP:)
      TEMP2 = 3202
      GOTO 20001
 3202 TEMP = VARTYP(N)
      IF (RETTYP.NE.TEMP/1000) THEN
            WRITE (*, '(A)') 'INVALID TYPES IN ASSIGNMENT'
            STOP
      END IF
      IF (TEMP.GE.3000.OR.TEMP.LT.2000) GOTO 3212
      WRITE (TMPSTR, *) MOD(TEMP, 1000)
      WRITE (TMPSR2, *) VARADR(N)
      WRITE (9, '(A)') 'LOD R1 SP', 'IMM R3 '//TRIM(ADJUSTL(TMPSTR)),
     1'IMM R2 M'//TRIM(ADJUSTL(TMPSR2)), 'ADD R4 SP R1', 'INC SP R4',
     2'CPY R2 R4', 'DEC R3 R3', 'DEC R4 R4', 'INC R2 R2', 'DEC R1 R1',
     3'BRZ ~+6 R3', 'BNZ ~-6 R1', 'STR R2 32', 'INC R2 R2', 'DEC R3 R3'
     4, 'BNZ ~-3 R3'
      GOTO 1000
 3212 WRITE (TMPSTR, *) VARADR(N)
      WRITE (9, '(A)') 'POP R1', 'STR M'//TRIM(ADJUSTL(TMPSTR))//' R1'
      GOTO 1000
C GOTO
 4000 LINE = ADJUSTL(LINE(5:))
      WRITE (9, '(A)') 'JMP .LABEL_'//TRIM(LINE)
      GOTO 1000
C IF
 5000 LINE = ADJUSTL(LINE(3:))
      IF (LINE(1:1).NE.'(') GOTO 4
      LINE = ADJUSTL(LINE(2:))
      IF (INDEX(LINE, '.EQ.').NE.0) GOTO 4
      IF (INDEX(LINE, '.NE.').NE.0) GOTO 4
      IF (INDEX(LINE, '.LE.').NE.0) GOTO 4
      IF (INDEX(LINE, '.LT.').NE.0) GOTO 4
      IF (INDEX(LINE, '.GE.').NE.0) GOTO 4
      IF (INDEX(LINE, '.GT.').NE.0) GOTO 4
C ARITHMETIC IF
      TEMP = INDEX(LINE,')') + 1
      TMPSR2 = LINE(TEMP:)
      TEMP = TEMP - 2
      LINE = LINE(:TEMP)
      TEMP2 = 5200
      GOTO 20001
 5200 WRITE (9, '(A)') 'POP R1'
      LINE = TMPSR2
      TEMP = INDEX(LINE,',')-1
      IF (TEMP.LE.0) GOTO 4
      TMPSTR = ADJUSTL(LINE(:TEMP))
      TEMP = TEMP + 2
      LINE = LINE(TEMP:)
      WRITE (9, '(A)') '//','BRN .LABEL_'//TRIM(TMPSTR)//' R1'
      TEMP = INDEX(LINE,',')-1
      IF (TEMP.LE.0) GOTO 4
      TMPSTR = ADJUSTL(LINE(:TEMP))
      TEMP = TEMP + 2
      LINE = LINE(TEMP:)
      WRITE (9, '(A)') 'BRZ .LABEL_'//TRIM(TMPSTR)//' R1'
      LINE = ADJUSTL(LINE)
      IF (LINE(1:1).EQ.' ') GOTO 4
      WRITE (9, '(A)') 'JMP .LABEL_'//TRIM(LINE)
      GOTO 1000
C WRITE
6000  LINE = ADJUSTL(LINE(6:))
      IF (LINE(1:1).NE.'(') GOTO 4
      LINE = ADJUSTL(LINE(2:))
C     TERMINAL OUTPUT ONLY FOR NOW
      IF (LINE(1:1).NE.'*') GOTO 4
      TEMP = INDEX(LINE,',') + 1
      LINE = ADJUSTL(LINE(TEMP:))
C     ONLY STRING FORMAT SUPPORTED FOR NOW (UNFORMATTED COMING LATER)
      IF (LINE(:5).NE.'''(A)''') GOTO 4
      LINE = ADJUSTL(LINE(6:))
      IF (LINE(:1).NE.')') GOTO 4
      TMPSR3 = LINE(2:)
      WRITE (9, '(A)') 'PSH @MAX'
6010  TEMP = INDEX(TMPSR3,',') + 1
      IF (TEMP.EQ.1) GOTO 6020
      LINE = TMPSR3(:TEMP)
      TMPSR3 = TMPSR3(TEMP:)
      TEMP2 = 6010
      GOTO 20001
6020  LINE = TMPSR3
      TEMP2 = 6100
      GOTO 20001
6100  WRITE (9, '(A)') 'MOV R4 SP','//','LOD R1 R4','BRE ~+16 R1 @MAX',
     1'ADD R2 R1 R4','INC R2 R2','LOD R3 R2','BRE ~+3 R3 @MAX',
     2'MOV R4 R2','JMP ~-7','STR R4 @MAX','DEC R2 R2','LOD R3 R2',
     3'BRE ~+4 R3 @MAX','OUT %TEXT R3','DEC R1 R1','BNZ ~-5 R1',
     4'OUT %TEXT 10','JMP ~-17'
      GOTO 1000
C ERRORS
    1 WRITE(TMPSTR, *) LINEN
      WRITE (*, '(A)') 'I/O ERROR ON LINE ', ADJUSTL(TMPSTR)
      CLOSE (8)
      CLOSE (9)
      STOP
    2 WRITE (*,'(A)') 'EOF REACHED'
      CLOSE (8)
      CLOSE (9)
      STOP
    3 WRITE (*,'(A)') 'CONTINUATION LINES UNSUPPORTED'
      CLOSE (8)
      CLOSE (9)
      STOP
    4 WRITE(TMPSTR, *) LINEN
      WRITE (*,'(A)') 'SYNTAX ERROR ON LINE '//ADJUSTL(TMPSTR)
      CLOSE (8)
      CLOSE (9)
      STOP
    5 WRITE (*, '(A)') 'UNDEFINED FUNCTION: '//FUNCS(TEMP)
      CLOSE (7)
      CLOSE (8)
      CLOSE (9)
      STOP

C SHUNTING YARD
20001 OP = 1
      SP = 0
C ("str1" + " str2" == "str1 str2") && (pow(2, 3)+2.25 == 10.25)
20000 LINE = ADJUSTL(LINE)
      IF (LINE(:1).EQ.' ') GOTO 28998
C OPERATORS
      IF (LINE(:1).LT.'0') GOTO 30000
C VARS AND FUNCS
      IF (LINE(:1).GT.'9') GOTO 40000
C NUMBER
      TMPI = 72
      IF (INDEX(LINE, ' ').NE.0) TMPI = INDEX(LINE, ' ')-1
      IF (INDEX(LINE, '.').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '.')-1)
      IF (INDEX(LINE, ',').NE.0) TMPI = MIN(TMPI, INDEX(LINE, ',')-1)
      IF (INDEX(LINE, '+').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '+')-1)
      IF (INDEX(LINE, '-').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '-')-1)
      IF (INDEX(LINE, '*').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '*')-1)
      IF (INDEX(LINE, '/').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '/')-1)
      IF (INDEX(LINE, ')').NE.0) TMPI = MIN(TMPI, INDEX(LINE, ')')-1)
      IF (INDEX(LINE, ':').NE.0) TMPI = MIN(TMPI, INDEX(LINE, ':')-1)
      TMPSTR = LINE(:TMPI)
      TMPI = TMPI + 1
      LINE = LINE(TMPI:)
      READ (TMPSTR, *) TMPI
C PUSH INTEGER VALUE
      OUT(OP) = 1
      OP = OP + 1
      OUT(OP) = TMPI
      OP = OP + 1
      NALLOW = .FALSE.
      GOTO 20000

C HANDLE OPERATORS
C DEAL WITH LOGICAL RELATED THINGS
30000 IF (LINE(:1).EQ.'.') GOTO 35000
      IF (LINE(:1).EQ.',') THEN
            LINE = LINE(2:)
            GOTO 20000
      END IF
C PARSE STRINGS
      IF (LINE(:1).EQ.'''') GOTO 36000
      IF (LINE(:1).EQ.'(') THEN
            SP = SP + 1
            STACK(SP) = 6
            LINE = LINE(2:)
            GOTO 20000
      END IF
      IF (LINE(:1).EQ.')') THEN
            LINE = LINE(2:)
30010       IF (SP.EQ.0) GOTO 11003
            IF (STACK(SP).EQ.6) THEN
                  SP = SP - 1
                  IF (STACK(SP)/100.EQ.1) THEN
                        OUT(OP) = STACK(SP)
                        SP = SP - 1
                        OP = OP + 1
                  END IF
                  GOTO 20000
            ELSE
                  OUT(OP) = STACK(SP)
                  OP = OP + 1
                  SP = SP - 1
                  GOTO 30010
            END IF
      END IF
      IF (LINE(:1).EQ.'-'.AND.NALLOW) THEN
            OUT(OP) = 1
            OP = OP + 1
            OUT(OP) = 0
            OP = OP + 1
      END IF
      NALLOW = .FALSE.
      IF (LINE(:2).EQ.'**') THEN
            SP = SP + 1
            STACK(SP) = 9
            LINE = LINE(3:)
            GOTO 20000
      END IF
      IF (LINE(:2).EQ.'//') THEN
30300       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 30
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.39.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 30300
                  END IF
                  SP = SP + 2
                  STACK(SP) = 30
            END IF
            LINE = LINE(3:)
            GOTO 20000
      END IF
      IF (LINE(:1).EQ.'*') THEN
30100       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 10
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.19.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 30100
                  END IF
                  SP = SP + 2
                  STACK(SP) = 10
            END IF
            LINE = LINE(2:)
            GOTO 20000
      END IF
      IF (LINE(:1).EQ.'/') THEN
30110       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 11
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.19.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 30110
                  END IF
                  SP = SP + 2
                  STACK(SP) = 11
            END IF
            LINE = LINE(2:)
            GOTO 20000
      END IF
      IF (LINE(:1).EQ.'+') THEN
30200       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 20
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.29.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 30200
                  END IF
                  SP = SP + 2
                  STACK(SP) = 20
            END IF
            LINE = LINE(2:)
            GOTO 20000
      END IF
      IF (LINE(:1).EQ.'-') THEN
30210       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 21
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.29.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 30210
                  END IF
                  SP = SP + 2
                  STACK(SP) = 21
            END IF
            LINE = LINE(2:)
            GOTO 20000
      END IF
      GOTO 11000
35000 NALLOW = .TRUE.
      IF (LINE(:6).EQ.'.TRUE.') THEN
          OUT(OP) = 4
          OP = OP + 1
C ANYTHING NON ZERO
          OUT(OP) = -1
          OP = OP + 1
          LINE = LINE(7:)
          GOTO 20000
      END IF
      IF (LINE(:7).EQ.'.FALSE.') THEN
          OUT(OP) = 4
          OP = OP + 1
          OUT(OP) = 0
          OP = OP + 1
          LINE = LINE(8:)
          GOTO 20000
      END IF
      IF (LINE(:5).EQ.'.NOT.') THEN
35050       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 50
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.59.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35050
                  END IF
                  SP = SP + 2
                  STACK(SP) = 50
            END IF
            LINE = LINE(6:)
            GOTO 20000
      END IF
      IF (LINE(:4).EQ.'.EQ.') THEN
35100       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 40
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.49.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35100
                  END IF
                  SP = SP + 2
                  STACK(SP) = 40
            END IF
            LINE = LINE(5:)
            GOTO 20000
      END IF
      IF (LINE(:4).EQ.'.NE.') THEN
35200       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 41
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.49.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35200
                  END IF
                  SP = SP + 2
                  STACK(SP) = 41
            END IF
            LINE = LINE(5:)
            GOTO 20000
      END IF
      IF (LINE(:4).EQ.'.LT.') THEN
35300       IF (SP.EQ.0) THEN
                SP = SP + 1
                STACK(SP) = 42
            ELSE
                TMPI = STACK(SP)
                SP = SP - 1
                IF (TMPI.LE.49.AND.TMPI.NE.6) THEN
                    OUT(OP) = TMPI
                    OP = OP + 1
                    GOTO 35300
                END IF
                SP = SP + 2
                STACK(SP) = 42
            END IF
            LINE = LINE(5:)
            GOTO 20000
      END IF
      IF (LINE(:4).EQ.'.LE.') THEN
35400       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 43
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.49.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35400
                  END IF
                  SP = SP + 2
                  STACK(SP) = 43
            END IF
            LINE = LINE(5:)
            GOTO 20000
      END IF
      IF (LINE(:4).EQ.'.GT.') THEN
35500       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 44
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.49.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35500
                  END IF
                  SP = SP + 2
                  STACK(SP) = 44
            END IF
            LINE = LINE(5:)
            GOTO 20000
      END IF
      IF (LINE(:4).EQ.'.GE.') THEN
35600       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 45
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.49.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35600
                  END IF
                  SP = SP + 2
                  STACK(SP) = 45
            END IF
            LINE = LINE(5:)
            GOTO 20000
      END IF
      IF (LINE(:5).EQ.'.EQV.') THEN
35700       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 81
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.89.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35700
                  END IF
                  SP = SP + 2
                  STACK(SP) = 81
            END IF
            LINE = LINE(6:)
            GOTO 20000
      END IF
      IF (LINE(:6).EQ.'.NEQV.') THEN
35710       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 82
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.89.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35710
                  END IF
                  SP = SP + 2
                  STACK(SP) = 82
            END IF
            LINE = LINE(7:)
            GOTO 20000
      END IF
      IF (LINE(:5).EQ.'.AND.') THEN
35720       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 60
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.69.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35720
                  END IF
                  SP = SP + 2
                  STACK(SP) = 60
            END IF
            LINE = LINE(6:)
            GOTO 20000
      END IF
      IF (LINE(:4).EQ.'.OR.') THEN
35730       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 70
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.79.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35730
                  END IF
                  SP = SP + 2
                  STACK(SP) = 70
            END IF
            LINE = LINE(5:)
            GOTO 20000
      END IF
      IF (LINE(:5).EQ.'.XOR.') THEN
35740       IF (SP.EQ.0) THEN
                  SP = SP + 1
                  STACK(SP) = 80
            ELSE
                  TMPI = STACK(SP)
                  SP = SP - 1
                  IF (TMPI.LE.89.AND.TMPI.NE.6) THEN
                        OUT(OP) = TMPI
                        OP = OP + 1
                        GOTO 35740
                  END IF
                  SP = SP + 2
                  STACK(SP) = 80
            END IF
            LINE = LINE(6:)
            GOTO 20000
      END IF
C REAL NUMBER
      NALLOW = .FALSE.
      TMPI = INDEX(LINE, ' ')
      TMPSTR = ' '//LINE(2:)
      TMPI2 = INDEX(TMPSTR, '.')
      IF (TMPI2.NE.0) TMPI = MIN(TMPI, TMPI2-1)
      IF (INDEX(LINE, ',').NE.0) TMPI = MIN(TMPI, INDEX(LINE, ',')-1)
      IF (INDEX(LINE, '+').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '+')-1)
      IF (INDEX(LINE, '-').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '-')-1)
      IF (INDEX(LINE, '*').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '*')-1)
      IF (INDEX(LINE, '/').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '/')-1)
      IF (INDEX(LINE, ')').NE.0) TMPI = MIN(TMPI, INDEX(LINE, ')')-1)
      IF (INDEX(LINE, ':').NE.0) TMPI = MIN(TMPI, INDEX(LINE, ':')-1)
      TMPSTR = '1'//LINE(2:TMPI)
      TMPI = TMPI + 1
      LINE = LINE(TMPI:)
      READ(TMPSTR, *) TMPI
      TMPI2 = 1
35999 TMPI2 = TMPI2 * 10
      TMPR = TMPI / FLOAT(TMPI2)
      IF (TMPR.GE.2) GOTO 35999
      TMPI = NINT((TMPR-1)*65536)
      OP = OP - 2
      TMPI2 = OUT(OP)
      OP = OP + 2
      IF (OP.GE.3) THEN
            IF (TMPI2.EQ.1) THEN
                  OP = OP - 2
                  OUT(OP) = 3
                  OP = OP + 1
                  OUT(OP) = OUT(OP)*65536 + TMPI
                  OP = OP + 1
            ELSE
                  OUT(OP) = 3
                  OP = OP + 1
                  OUT(OP) = TMPI
                  OP = OP + 1
            END IF
      ELSE
            OUT(OP) = 3
            OP = OP + 1
            OUT(OP) = TMPI
            OP = OP + 1
      END IF
      GOTO 20000
C STRING VALUES
36000 LINE = LINE(2:)
      TMPI = 0
      OUT(OP) = 2
      OP = OP + 1
      TMPI2 = OP
      OP = OP + 1
      NALLOW = .FALSE.
36001 IF (LINE(:2).EQ.'''''') THEN
            LINE = LINE(3:)
            OUT(OP) = IACHAR('''')
            OP = OP + 1
            TMPI = TMPI + 1
            GOTO 36001
      ELSE IF (LINE(:1).NE.'''') THEN
            OUT(OP) = IACHAR(LINE(:1))
            OP = OP + 1
            LINE = LINE(2:)
            TMPSTR = ADJUSTL(LINE)
            IF (TMPSTR(:1).EQ.' ') GOTO 11004
            TMPI = TMPI + 1
            GOTO 36001
      END IF
      OUT(TMPI2) = TMPI
      LINE = LINE(2:)
      GOTO 20000
C VARS + FUNCS
40000 TMPI = 72
      NALLOW = .FALSE.
      IF (INDEX(LINE, ' ').NE.0) TMPI = INDEX(LINE, ' ')-1
      IF (INDEX(LINE, '.').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '.')-1)
      IF (INDEX(LINE, ',').NE.0) TMPI = MIN(TMPI, INDEX(LINE, ',')-1)
      IF (INDEX(LINE, '+').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '+')-1)
      IF (INDEX(LINE, '-').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '-')-1)
      IF (INDEX(LINE, '*').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '*')-1)
      IF (INDEX(LINE, '/').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '/')-1)
      IF (INDEX(LINE, ')').NE.0) TMPI = MIN(TMPI, INDEX(LINE, ')')-1)
      IF (INDEX(LINE, '(').NE.0) TMPI = MIN(TMPI, INDEX(LINE, '(')-1)
      IF (INDEX(LINE, ':').NE.0) TMPI = MIN(TMPI, INDEX(LINE, ':')-1)
      TMPSTR = LINE(:TMPI)
      TMPI = TMPI + 1
      LINE = LINE(TMPI:)
      TMPI = 0
40001 TMPI = TMPI + 1
      TMPSR2 = VARS(TMPI)
      IF (TMPI.EQ.64) GOTO 41000
      IF (TMPSR2(:1).EQ.' ') GOTO 41000
      IF (TMPSTR(:6).NE.TMPSR2(:6)) GOTO 40001
      OUT(OP) = VARTYP(TMPI)/1000*1000 + TMPI
      OP = OP + 1
      OUT(OP) = 1
      OP = OP + 1
      OUT(OP) = 0
      OP = OP + 1
      IF (LINE(:1).EQ.'(') GOTO 40100
      GOTO 20000
40100 TMPI = INDEX(LINE,':')
      IF (TMPI.NE.0.AND.TMPI.LT.INDEX(LINE,')')) GOTO 40200
      TMPI = INDEX(LINE, ')')-1
      TMPSTR = LINE(2:TMPI)
      TMPI = TMPI + 2
      LINE = LINE(TMPI:)
      TMPI = 0
      IF (TMPSTR(:1).GT.'9'.OR.TMPSTR(:1).LT.'0') THEN
40101       TMPI = TMPI + 1
            TMPSR2 = VARS(TMPI)
            IF (TMPI.EQ.64) GOTO 11005
            IF (TMPSR2(:1).EQ.' ') GOTO 11005
            IF (TMPSTR(:6).NE.TMPSR2(:6)) GOTO 40101
            TMPI = TMPI + 100
            OP = OP - 2
            OUT(OP) = TMPI
            OP = OP + 2
      ELSE
            READ (TMPSTR, *) TMPI
            OP = OP - 2
            OUT(OP) = TMPI
            OP = OP + 2
      END IF
      GOTO 20000
40200 TMPI = INDEX(LINE, ':') - 1
      TMPSTR = LINE(2:TMPI)
      TMPI = TMPI + 2
      LINE = LINE(TMPI:)
      IF (TMPI.NE.3) THEN
            TMPI = 0
            IF (TMPSTR(:1).GT.'9'.OR.TMPSTR(:1).LT.'0') THEN
40201             TMPI = TMPI + 1
                  TMPSR2 = VARS(TMPI)
                  IF (TMPI.EQ.64) GOTO 11005
                  IF (TMPSR2(:1).EQ.' ') GOTO 11005
                  IF (TMPSTR(:6).NE.TMPSR2(:6)) GOTO 40201
                  TMPI = TMPI + 100
                  OP = OP - 2
                  OUT(OP) = TMPI
                  OP = OP + 2
            ELSE
                  READ (TMPSTR, *) TMPI
                  OP = OP - 2
                  OUT(OP) = TMPI
                  OP = OP + 2
            END IF
      END IF
      TMPI = INDEX(LINE, ')') - 1
      TMPSTR = LINE(:TMPI)
      TMPI = TMPI + 2
      LINE = LINE(TMPI:)
      IF (TMPI.NE.2) THEN
            TMPI = 0
            IF (TMPSTR(:1).GT.'9'.OR.TMPSTR(:1).LT.'0') THEN
40202             TMPI = TMPI + 1
                  TMPSR2 = VARS(TMPI)
                  IF (TMPI.EQ.64) GOTO 11005
                  IF (TMPSR2(:1).EQ.' ') GOTO 11005
                  IF (TMPSTR(:6).NE.TMPSR2(:6)) GOTO 40202
                  TMPI = TMPI + 100
                  OP = OP - 1
                  OUT(OP) = TMPI
                  OP = OP + 1
            ELSE
                  READ (TMPSTR, *) TMPI
                  OP = OP - 1
                  OUT(OP) = TMPI
                  OP = OP + 1
            END IF
      END IF
      GOTO 20000

41000 TMPI = 0
41001 TMPI = TMPI + 1
      TMPSR2 = FUNCS(TMPI)
      IF (TMPI.EQ.99) GOTO 11005
      IF (TMPSR2(:1).EQ.' ') GOTO 11005
      IF (TMPSTR(:6).NE.TMPSR2(:6)) GOTO 41001
      SP = SP + 1
      STACK(SP) = 100 + TMPI
      GOTO 20000
C EMIT CODE
28998 IF (SP.LE.0) GOTO 28999
      TMPI = STACK(SP)
      OUT(OP) = TMPI
      SP = SP - 1
      OP = OP + 1
      GOTO 28998
28999 OUT(OP) = 0
      OP = 1
      TYPP = 0
29000 TMPI = OUT(OP)
      TMPI3 = 0
      IF (TMPI.EQ.0) GOTO 28888
      OP = OP + 1
C SCALARS
      IF (TMPI.EQ.1) GOTO 29001
      IF (TMPI.EQ.2) GOTO 29002
      IF (TMPI.EQ.3) GOTO 29003
      IF (TMPI.EQ.4) GOTO 29004
C VARS AND FUNCS)
      IF (TMPI.GT.3000) GOTO 29300
      IF (TMPI.GT.2000) GOTO 29200
      IF (TMPI.GT.1000) GOTO 29300
      IF (TMPI.GT.100) GOTO 29100
      IF (TYPP.EQ.0) GOTO 11002
C MONADIC FUNCTIONS
      IF (TMPI.EQ.50) GOTO 29050
      IF (TYPP.EQ.1) GOTO 11002
C DIADIC FUNCTIONS
      IF (TMPI.EQ.09) GOTO 29009
      IF (TMPI.EQ.10) GOTO 29010
      IF (TMPI.EQ.11) GOTO 29011
      IF (TMPI.EQ.20) GOTO 29020
      IF (TMPI.EQ.21) GOTO 29021
      IF (TMPI.EQ.30) GOTO 29030
      IF (TMPI.EQ.40) GOTO 29040
      IF (TMPI.EQ.41) GOTO 29041
      IF (TMPI.EQ.42) GOTO 29042
      IF (TMPI.EQ.43) GOTO 29043
      IF (TMPI.EQ.44) GOTO 29044
      IF (TMPI.EQ.45) GOTO 29045
      IF (TMPI.EQ.60) GOTO 29060
      IF (TMPI.EQ.70) GOTO 29070
      IF (TMPI.EQ.80) GOTO 29080
      IF (TMPI.EQ.81) GOTO 29081
      IF (TMPI.EQ.82) GOTO 29082
      GOTO 11001
C HANDLE INTS
29001 TMPI = OUT(OP)
      OP = OP + 1
      WRITE(TMPSTR, *) TMPI
      TMPSTR = ADJUSTL(TMPSTR)
      TYPP = TYPP + 1
      TYPES(TYPP) = 1
      WRITE(9, '(A)') 'PSH '//TRIM(TMPSTR)
      GOTO 29000
C HANDLE STRINGS
29002 TMPI = OUT(OP)
      TMPI3 = TMPI
      OP = OP + 1
      TYPP = TYPP + 1
      TYPES(TYPP) = 2
29102 IF (TMPI.NE.0) THEN
            TMPI2 = OUT(OP)
            OP = OP + 1
            WRITE(TMPSTR, *) TMPI2
            WRITE(9, '(A)') 'PSH '//TRIM(ADJUSTL(TMPSTR))
            TMPI = TMPI - 1
            GOTO 29102
      END IF
      WRITE(TMPSTR, *) TMPI3
      WRITE(9, '(A)') 'PSH '//TRIM(ADJUSTL(TMPSTR))
      GOTO 29000
C HANDLE REALS
29003 TMPI = OUT(OP)
      OP = OP + 1
      WRITE(TMPSTR, *) TMPI
      TMPSTR = ADJUSTL(TMPSTR)
      TYPP = TYPP + 1
      TYPES(TYPP) = 3
      WRITE(9, '(A)') 'PSH '//TRIM(TMPSTR)
      GOTO 29000
C HANDLE LOGICALS
29004 TMPI = OUT(OP)
      OP = OP + 1
      WRITE(TMPSTR, *) TMPI
      TMPSTR = ADJUSTL(TMPSTR)
      TYPP = TYPP + 1
      TYPES(TYPP) = 4
      WRITE(9, '(A)') 'PSH '//TRIM(TMPSTR)
      GOTO 29000
C COMPARISON OPERATORS
29040 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      TYPP = TYPP - 1
C CAST INTS TO REALS
      TMPI3 = 29140
      IF (TMPI.EQ.4) TMPI = 1
      IF (TMPI2.EQ.4) TMPI2 = 1
      IF (TMPI.EQ.1.AND.TMPI2.EQ.3) GOTO 28311
      IF (TMPI.EQ.3.AND.TMPI2.EQ.1) GOTO 28310
      IF (TMPI.EQ.2.AND.TMPI2.EQ.2) GOTO 29240
      IF (TMPI.NE.TMPI2) GOTO 11002
29140 TYPP = TYPP + 1
      TYPES(TYPP) = 4
      WRITE(9, '(A)') 'POP R2', 'POP R1', 'SETE R1 R1 R2', 'PSH R1'
      GOTO 29000
29240 TYPP = TYPP + 1
      TYPES(TYPP) = 4
      WRITE(9, '(A)') 'POP R1', 'MOV R7 R0', 'ADD R6 SP R1'
      WRITE(9, '(A)') 'LOD R2 R6', 'ADD R4 R6 R2', 'INC R4 R4'
      WRITE(9, '(A)') 'BNE ~+6 R1 R2', 'POP R3', 'LLOD R5 SP R1'
      WRITE(9, '(A)') 'BNE ~+3 R3 R5', 'BNE ~-3 SP R6', 'IMM R7 -1'
      WRITE(9, '(A)') 'MOV SP R4', 'PSH R7'
      GOTO 29000
29041 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      TYPP = TYPP - 1
C CAST INTS TO REALS
      TMPI3 = 29141
      IF (TMPI.EQ.4) TMPI = 1
      IF (TMPI2.EQ.4) TMPI2 = 1
      IF (TMPI.EQ.1.AND.TMPI2.EQ.3) GOTO 28311
      IF (TMPI.EQ.3.AND.TMPI2.EQ.1) GOTO 28310
      IF (TMPI.EQ.2.AND.TMPI2.EQ.2) GOTO 29241
      IF (TMPI.NE.TMPI2) GOTO 11002
29141 TYPP = TYPP + 1
      TYPES(TYPP) = 4
      WRITE(9, '(A)') 'POP R2', 'POP R1', 'SETNE R1 R1 R2', 'PSH R1'
      GOTO 29000
29241 TYPP = TYPP + 1
      TYPES(TYPP) = 4
      WRITE(9, '(A)') 'POP R1', 'IMM R7 -1', 'ADD R6 SP R1'
      WRITE(9, '(A)') 'LOD R2 R6', 'ADD R4 R6 R2', 'INC R4 R4'
      WRITE(9, '(A)') 'BNE ~+6 R1 R2', 'POP R3', 'LLOD R5 SP R1'
      WRITE(9, '(A)') 'BNE ~+3 R3 R5', 'BNE ~-3 SP R6', 'MOV R7 R0'
      WRITE(9, '(A)') 'MOV SP R4', 'PSH R7'
      GOTO 29000
29042 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      TYPP = TYPP - 1
C CAST INTS TO REALS
      TMPI3 = 29142
      IF (TMPI.EQ.4) TMPI = 1
      IF (TMPI2.EQ.4) TMPI2 = 1
      IF (TMPI.EQ.1.AND.TMPI2.EQ.3) GOTO 28311
      IF (TMPI.EQ.3.AND.TMPI2.EQ.1) GOTO 28310
      IF (TMPI.NE.TMPI2) GOTO 11002
29142 TYPP = TYPP + 1
      TYPES(TYPP) = 4
      WRITE(9, '(A)') 'POP R2', 'POP R1', 'SSETL R1 R1 R2', 'PSH R1'
      GOTO 29000
29043 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      TYPP = TYPP - 1
C CAST INTS TO REALS
      TMPI3 = 29143
      IF (TMPI.EQ.4) TMPI = 1
      IF (TMPI2.EQ.4) TMPI2 = 1
      IF (TMPI.EQ.1.AND.TMPI2.EQ.3) GOTO 28311
      IF (TMPI.EQ.3.AND.TMPI2.EQ.1) GOTO 28310
      IF (TMPI.NE.TMPI2) GOTO 11002
29143 TYPP = TYPP + 1
      TYPES(TYPP) = 4
      WRITE(9, '(A)') 'POP R2', 'POP R1', 'SSETLE R1 R1 R2', 'PSH R1'
      GOTO 29000
29044 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      TYPP = TYPP - 1
C CAST INTS TO REALS
      TMPI3 = 29144
      IF (TMPI.EQ.4) TMPI = 1
      IF (TMPI2.EQ.4) TMPI2 = 1
      IF (TMPI.EQ.1.AND.TMPI2.EQ.3) GOTO 28311
      IF (TMPI.EQ.3.AND.TMPI2.EQ.1) GOTO 28310
      IF (TMPI.NE.TMPI2) GOTO 11002
29144 TYPP = TYPP + 1
      TYPES(TYPP) = 4
      WRITE(9, '(A)') 'POP R2', 'POP R1', 'SSETG R1 R1 R2', 'PSH R1'
      GOTO 29000
29045 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      TYPP = TYPP - 1
C CAST INTS TO REALS
      TMPI3 = 29145
      IF (TMPI.EQ.4) TMPI = 1
      IF (TMPI2.EQ.4) TMPI2 = 1
      IF (TMPI.EQ.1.AND.TMPI2.EQ.3) GOTO 28311
      IF (TMPI.EQ.3.AND.TMPI2.EQ.1) GOTO 28310
      IF (TMPI.NE.TMPI2) GOTO 11002
29145 TYPP = TYPP + 1
      TYPES(TYPP) = 4
      WRITE(9, '(A)') 'POP R2', 'POP R1', 'SSETGE R1 R1 R2', 'PSH R1'
      GOTO 29000
C LOGICAL OPERATORS
29050 IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1'
            TYPES(TYPP) = 4
      END IF
      WRITE(9, '(A)') 'POP R1', 'NOT R1 R1', 'PSH R1'
      GOTO 29000
29060 IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1'
      END IF
      TYPP = TYPP - 1
      IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R2'
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1', 'PSH R2'
            TYPES(TYPP) = 4
      END IF
      WRITE(9, '(A)') 'POP R1', 'POP R2', 'AND R1 R1 R2', 'PSH R1'
      GOTO 29000
29070 IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1'
      END IF
      TYPP = TYPP - 1
      IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R2'
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1', 'PSH R2'
            TYPES(TYPP) = 4
      END IF
      WRITE(9, '(A)') 'POP R1', 'POP R2', 'OR R1 R1 R2', 'PSH R1'
      GOTO 29000
29080 IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1'
      END IF
      TYPP = TYPP - 1
      IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R2'
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1', 'PSH R2'
            TYPES(TYPP) = 4
      END IF
      WRITE(9, '(A)') 'POP R1', 'POP R2', 'XOR R1 R1 R2', 'PSH R1'
      GOTO 29000
29081 IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1'
      END IF
      TYPP = TYPP - 1
      IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R2'
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1', 'PSH R2'
            TYPES(TYPP) = 4
      END IF
      WRITE(9, '(A)') 'POP R2', 'POP R1', 'SETE R1 R1 R2', 'PSH R1'
      GOTO 29000
29082 IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1'
      END IF
      TYPP = TYPP - 1
      IF (TYPES(TYPP).NE.4) THEN
            WRITE(9, '(A)') 'POP R2'
            WRITE(9, '(A)') 'POP R1', '//', 'BRZ ~+2 R1', 'IMM R1 -1'
            WRITE(9, '(A)') 'PSH R1', 'PSH R2'
            TYPES(TYPP) = 4
      END IF
      WRITE(9, '(A)') 'POP R2', 'POP R1', 'SETNE R1 R1 R2', 'PSH R1'
      GOTO 29000
C ARITHMETIC OPERATORS
29009 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      IF (TMPI.NE.TMPI2.OR.TMPI.NE.1) GOTO 11002
      TYPES(TYPP) = 1
      WRITE(9, '(A)') 'POP R3', 'POP R2', 'IMM R1 1', 'BRZ ~+4 R3'
      WRITE(9, '(A)')'MLT R1 R1 R2','DEC R3 R3','BNZ ~-2 R3','PSH R1'
      GOTO 29000
29010 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI3 = 29210
      IF (TMPI.EQ.1.AND.TMPI2.EQ.1) GOTO 29110
      IF (TMPI.EQ.3.AND.TMPI2.EQ.3) GOTO 29210
      IF (TMPI.EQ.1.AND.TMPI2.EQ.3) GOTO 28311
      IF (TMPI.EQ.3.AND.TMPI2.EQ.1) GOTO 28310
      GOTO 11002
29110 TYPP = TYPP + 1
      TYPES(TYPP) = 1
      WRITE(9, '(A)') 'POP R1', 'POP R2', 'MLT R1 R1 R2', 'PSH R1'
      GOTO 29000
29210 TYPP = TYPP + 1
      TYPES(TYPP) = 3
      WRITE(9, '(A)') 'POP R1', 'POP R2'
      WRITE(9, '(A)') '//','MLT R3 R1 R2','BSR R3 R3 16'
      WRITE(9, '(A)') 'SUMLT R1 R1 R2', 'BSL R1 R1 16', 'ADD R1 R1 R3'
      WRITE(9, '(A)') 'PSH R1'
      GOTO 29000
29011 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      IF (TMPI.NE.TMPI2.OR.TMPI.NE.1) GOTO 11002
      TYPES(TYPP) = 1
      WRITE(9, '(A)') 'POP R1', 'POP R2', 'SDIV R1 R2 R1', 'PSH R1'
      GOTO 29000
29020 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      TMPI3 = 29120
      TYPES(TYPP) = 3
      IF (TMPI.EQ.3.AND.TMPI2.EQ.3) GOTO 29120
      IF (TMPI.EQ.1.AND.TMPI2.EQ.3) GOTO 28311
      IF (TMPI.EQ.3.AND.TMPI2.EQ.1) GOTO 28310
      TYPES(TYPP) = 1
      IF (TMPI.NE.1.OR.TMPI2.NE.1) GOTO 11002
29120 WRITE(9, '(A)') 'POP R1', 'POP R2', 'ADD R1 R2 R1', 'PSH R1'
      GOTO 29000
29021 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      TMPI3 = 29121
      TYPES(TYPP) = 3
      IF (TMPI.EQ.3.AND.TMPI2.EQ.3) GOTO 29121
      IF (TMPI.EQ.1.AND.TMPI2.EQ.3) GOTO 28311
      IF (TMPI.EQ.3.AND.TMPI2.EQ.1) GOTO 28310
      TYPES(TYPP) = 1
      IF (TMPI.NE.1.OR.TMPI2.NE.1) GOTO 11002
29121 WRITE(9, '(A)') 'POP R1', 'POP R2', 'SUB R1 R2 R1', 'PSH R1'
      GOTO 29000
29030 TMPI = TYPES(TYPP)
      TYPP = TYPP - 1
      TMPI2 = TYPES(TYPP)
      IF (TMPI.NE.TMPI2.OR.TMPI.NE.2) GOTO 11002
      WRITE(9, '(A)') 'POP R1', '//', 'LLOD R2 SP R1', 'LSTR SP R1 @MAX'
      WRITE(9, '(A)') 'ADD R1 R1 R2', 'PSH R1', 'MOV R2 SP'
      WRITE(9, '(A)') 'LOD R1 SP', 'BRE ~+3 R1 @MAX', 'INC SP SP'
      WRITE(9, '(A)') 'JMP ~-3', 'LLOD R1 SP -1', 'STR SP R1'
      WRITE(9, '(A)') 'DEC SP SP', 'BNE ~-3 SP R2', 'INC SP SP'
      GOTO 29000
C FUNCS
29100 TMPI = TMPI - 100
      TMPI2 = FNARGS(TMPI)
      IF (TYPP.LT.TMPI2) GOTO 11002
      TYPP = TYPP - TMPI2
      TYPP = TYPP + 1
      TYPES(TYPP) = FNTYPE(TMPI)
      IFUNCS(TMPI) = .TRUE.
      TMPSTR = FUNCS(TMPI)
      WRITE(9, '(A)') 'CAL ._'//TRIM(TMPSTR)
      GOTO 29000
C VARS
29200 TMPI = TMPI - 2000
      TYPP = TYPP + 1
      TYPES(TYPP) = 2
      TMPI2 = VARADR(TMPI)
      IF (OUT(OP).GT.100) THEN
            TMPI = OUT(OP) - 100
            TMPI = VARADR(TMPI)
            WRITE (TMPSTR, *) TMPI
            WRITE (9, '(A)') 'LOD R1 M'//TRIM(ADJUSTL(TMPSTR))
      ELSE IF (OUT(OP).NE.0) THEN
            WRITE (TMPSTR, *) OUT(OP)
            WRITE (9, '(A)') 'IMM R1 '//TRIM(ADJUSTL(TMPSTR))
      ELSE
            WRITE (9, '(A)') 'IMM R1 1'
      END IF
      OP = OP + 1
      IF (OUT(OP).GT.100) THEN
            TMPI = OUT(OP) - 100
            TMPI = VARADR(TMPI)
            WRITE (TMPSTR, *) TMPI
            WRITE (9, '(A)') 'LOD R2 M'//TRIM(ADJUSTL(TMPSTR))
      ELSE IF (OUT(OP).NE.0) THEN
            WRITE (TMPSTR, *) OUT(OP)
            WRITE (9, '(A)') 'IMM R2 '//TRIM(ADJUSTL(TMPSTR))
      ELSE
            TMPI3 = VARTYP(TMPI)
            TMPI3 = TMPI3 - 2000
            WRITE (TMPSTR, *) TMPI3
            WRITE (9, '(A)') 'IMM R2 '//TRIM(ADJUSTL(TMPSTR))
      END IF
      OP = OP + 1
      WRITE (TMPSTR, *) TMPI2
      WRITE (9, '(A)') 'ADD R1 R1 M'//TRIM(ADJUSTL(TMPSTR))
      WRITE (9, '(A)') 'ADD R2 R2 M'//TRIM(ADJUSTL(TMPSTR))
      WRITE (9, '(A)') 'DEC R1 R1'
      WRITE (9, '(A)') 'MOV R4 R0','BGE ~+6 R1 R2','LOD R3 R1','PSH R3'
      WRITE (9, '(A)') 'INC R1 R1','INC R4 R4','BRL ~-4 R1 R2','PSH R4'
      GOTO 29000
29300 TMPI2 = TMPI/1000
      TMPI = MOD(TMPI, 1000)
      TYPP = TYPP + 1
      TYPES(TYPP) = TMPI2
      TMPI3 = OUT(OP)
      OP = OP + 2
      IF (TMPI.GT.100) THEN
            TMPI2 = TMPI3 - 100
            TMPI2 = VARADR(TMPI2)
            WRITE (TMPSTR, *) TMPI2
            WRITE (9, '(A)') 'LOD R1 M'//TRIM(ADJUSTL(TMPSTR))
            WRITE (9, '(A)') 'DEC R1 R1'
            WRITE (TMPSTR, *) VARADR(TMPI)
            WRITE (9, '(A)') 'LLOD R1 R1 M'//TRIM(ADJUSTL(TMPSTR))
            WRITE (9, '(A)') 'PSH R1'
            GOTO 29000
      END IF
      WRITE (TMPSTR, *) VARADR(TMPI) + TMPI3 - 1
      WRITE(9, '(A)') 'LOD R1 M'//TRIM(ADJUSTL(TMPSTR)), 'PSH R1'
      GOTO 29000
C ERRORS
11000 WRITE (*, '(A)') 'UNKNOWN OPERATION'
      STOP
11001 WRITE (*, '(A)') 'CODE GEN DOES NOT SUPPORT OPERATION'
      STOP
11002 WRITE (*, '(A)') 'MISSING OR INVALID VALUE(S) TO FUNCTION'
      STOP
11003 WRITE (*, '(A)') 'MISMATCHED PAREN'
      STOP
11004 WRITE (*, '(A)') 'MISMATCHED QUOTES'
      STOP
11005 WRITE (*, '(A)') 'UNDECLARED VARIABLE OR INNEXISTANT FUNCTION'
      STOP
11100 WRITE (*, '(A)') 'INTERNEL ERROR: UNKNOWN RETURN DESTINATION'
      STOP
C CASTING FUNCTIONS
28310 WRITE(9, '(A)') 'POP R2', 'POP R1', 'BSL R1 R1 16'
      WRITE(9, '(A)') 'PSH R1', 'PSH R2'
      GOTO 28888
28311 WRITE(9, '(A)') 'POP R1', 'BSL R1 R1 16', 'PSH R1'
      GOTO 28888
C EVALUATE RETURN
28888 IF (TMPI3.EQ.29210) GOTO 29210
      IF (TMPI3.EQ.29120) GOTO 29120
      IF (TMPI3.EQ.29121) GOTO 29121
      IF (TMPI3.EQ.29140) GOTO 29140
      IF (TMPI3.EQ.29141) GOTO 29141
      IF (TMPI3.EQ.29142) GOTO 29142
      IF (TMPI3.EQ.29143) GOTO 29143
      IF (TMPI3.EQ.29144) GOTO 29144
      IF (TMPI3.EQ.29145) GOTO 29145
      RETTYP = TYPES(1)
      IF (TEMP2.EQ.6010) GOTO 6010
      IF (TEMP2.EQ.6100) GOTO 6100
      IF (TEMP2.EQ.3202) GOTO 3202
      IF (TEMP2.EQ.5200) GOTO 5200
      GOTO 11100
C DEFINE FUNCS
50000 OPEN (UNIT=7, FILE='FUNCTIONS/FUNCS.TXT', ACTION='READ')
50001 LINEN = LINEN + 1
      READ (7, '(A)',END=50002) LINE
      IF (LINE.EQ.'EOF') GOTO 50002
      IF (LINE(1:1).EQ.'-') GOTO 50001
      FUNCS(LINEN) = LINE(:8)
      IF (LINE(9:11).EQ.'INT') FNTYPE(LINEN) = 1
      IF (LINE(9:11).EQ.'STR') FNTYPE(LINEN) = 2
      IF (LINE(9:11).EQ.'REL') FNTYPE(LINEN) = 3
      TMPSTR = LINE(12:16)
      READ (TMPSTR, *) TEMP
      FNARGS(LINEN) = TEMP
      GOTO 50001
50002 FUNCS(LINEN) = ''
      LINEN = 0
      CLOSE(7)
      GOTO 1000
C LOAD FUNCS
51000 TEMP = 0
51001 TEMP = TEMP + 1
      IF (TEMP.EQ.101) GOTO 9999
      IF (IFUNCS(TEMP)) GOTO 51002
      GOTO 51001
51002 TMPSTR = 'FUNCTIONS/'//TRIM(FUNCS(TEMP))//'.URCL'
      OPEN (UNIT=7, FILE=TMPSTR, ACTION='READ', ERR=5)
51003 READ (7, '(A)', END=51001) TMPSTR
      WRITE (9, '(A)') TRIM(TMPSTR)
      GOTO 51003
      END

C POP R1
C DEC 