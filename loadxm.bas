#COMPILE EXE
#DIM NONE

TYPE PATTERNDATA
    note AS STRING * 1
    INST AS STRING * 1
    VC AS STRING * 1
    ET AS STRING * 1
    EP AS STRING * 1
END TYPE

FUNCTION PBMAIN () AS LONG

a$ = COMMAND$(1)
IF a$ = "" THEN EXIT FUNCTION

MKDIR "temp"
KILL "temp\*.*"

OPEN "temp\file.inf" FOR OUTPUT AS #2904

' XM LOADER ROUTINE
filerrr$ = COMMAND$(1)
OPEN COMMAND$(1) FOR BINARY AS #1

a$ = SPACE$(17)
GET #1, , a$
IF a$ <> "Extended Module: " THEN
    PRINT "File not an XM"
    CLOSE
    EXIT FUNCTION
END IF
modname$ = SPACE$(20)
GET #1, , modname$
a$ = SPACE$(1)
GET #1, , a$
trakername$ = SPACE$(20)
GET #1, , trakername$
a$ = SPACE$(2)
GET #1, , a$
a$ = SPACE$(4)
GET #1, , a$
DIM lunghider AS LONG

LUNGHIDER = LDWORD(a$)
HIDER$ = SPACE$(LUNGHIDER - 4)
GET #1, , HIDER$
DIM songlung AS LONG
DIM restart AS LONG
DIM canali AS LONG
DIM numpatterns AS LONG
DIM numinstruments AS LONG
DIM freqtable AS LONG
DIM deftempo AS LONG
DIM defbpm AS LONG

songlung = RWORD(MID$(HIDER$, 1, 2))
restart = RWORD(MID$(HIDER$, 3, 2))
canali = RWORD(MID$(HIDER$, 5, 2))
'Form7.Label1.Caption = canali
PRINT #2904, canali

numpatterns = RWORD(MID$(HIDER$, 7, 2))
numinstruments = RWORD(MID$(HIDER$, 9, 2))
freqtable = RWORD(MID$(HIDER$, 11, 2))
deftempo = RWORD(MID$(HIDER$, 13, 2))
defbpm = RWORD(MID$(HIDER$, 15, 2))

REDIM tpattern(0 TO songlung - 1) AS LONG
REDIM Lunpa(0 TO numpatterns - 1) AS INTEGER ' LUNGHEZZA DEL PATTERN
DIM i AS LONG

FOR i = 0 TO songlung - 1
tpattern(i) = ASC(MID$(HIDER$, 17 + i, 1))
NEXT


FOR i = 0 TO numpatterns - 1

7646 'USELESS LABEL FOR DEBUG PURPOSES

DIM crow AS LONG
DIM ccan AS LONG
DIM ff AS LONG

crow = 0
CCAN = 1
ff = 0
a$ = SPACE$(4)
GET #1, , a$
DIM lider AS LONG
DIM nrow AS LONG

LIDER = LDWORD(a$)

PIDER$ = SPACE$(LIDER - 4)
GET #1, , PIDER$
nrow = RWORD(MID$(PIDER$, 2, 2))
Lunpa(i) = nrow
DIM psize AS LONG

PSIZE = RWORD(MID$(PIDER$, 4, 2))

REDIM PDATA(1 TO canali, 0 TO (nrow - 1)) AS PATTERNDATA

703 IF crow = nrow THEN GOTO 704

a$ = SPACE$(1)
GET #1, , a$
DIM done AS LONG

DONE = 0

DIM bitss AS INTEGER

IF ASC(a$) > 127 THEN
    bitss = ASC(a$)
    IF (bitss AND 1) = 1 THEN
        a$ = SPACE$(1)
        GET #1, , a$
        PDATA(CCAN, crow).note = a$
    ELSE
        PDATA(CCAN, crow).note = CHR$(0)
    END IF
    IF (bitss AND 2) = 2 THEN
        a$ = SPACE$(1)
        GET #1, , a$
        PDATA(CCAN, crow).INST = a$
    ELSE
        PDATA(CCAN, crow).INST = CHR$(0)
    END IF
    IF (bitss AND 4) = 4 THEN
        a$ = SPACE$(1)
        GET #1, , a$
        PDATA(CCAN, crow).VC = a$
    ELSE
        PDATA(CCAN, crow).VC = CHR$(0)
    END IF
    IF (bitss AND 8) = 8 THEN
        a$ = SPACE$(1)
        GET #1, , a$
        PDATA(CCAN, crow).ET = a$
    ELSE
        PDATA(CCAN, crow).ET = CHR$(0)
    END IF
    IF (bitss AND 16) = 16 THEN
        a$ = SPACE$(1)
        GET #1, , a$
        PDATA(CCAN, crow).EP = a$
    ELSE
        PDATA(CCAN, crow).EP = CHR$(0)
    END IF
    DONE = 1
END IF
DIM j AS LONG


IF DONE = 1 THEN
    CCAN = CCAN + 1
    IF CCAN > canali THEN
        CCAN = 1
        IF (crow < nrow) AND (ff = 0) THEN
        FOR j = 1 TO canali
        IF PDATA(j, crow).ET = CHR$(13) THEN Lunpa(i) = crow + 1: ff = 1
        NEXT j
        END IF
        crow = crow + 1
    END IF
END IF
IF DONE = 1 THEN GOTO 703

PDATA(CCAN, crow).note = a$
a$ = SPACE$(4)
GET #1, , a$
PDATA(CCAN, crow).INST = (MID$(a$, 1, 1))
PDATA(CCAN, crow).VC = (MID$(a$, 2, 1))
PDATA(CCAN, crow).ET = (MID$(a$, 3, 1))
PDATA(CCAN, crow).EP = (MID$(a$, 4, 1))

CCAN = CCAN + 1
IF CCAN > canali THEN
    CCAN = 1
    IF (crow < nrow) AND (ff = 0) THEN
    FOR j = 1 TO canali
    IF PDATA(j, crow).ET = CHR$(13) THEN Lunpa(i) = crow + 1: ff = 1
    NEXT j
    END IF
    crow = crow + 1
END IF
GOTO 703
704
DIM x AS LONG
DIM y AS LONG

FOR X = 1 TO canali
OPEN "temp\P" + LTRIM$(STR$(i)) + "C" + LTRIM$(STR$(X)) + ".tmp" FOR BINARY AS (X + 1)
FOR Y = 0 TO Lunpa(i) - 1
a$ = PDATA(X, Y).note + PDATA(X, Y).INST + PDATA(X, Y).VC + PDATA(X, Y).ET + PDATA(X, Y).EP
PUT #(X + 1), , a$
NEXT Y
CLOSE (X + 1)
NEXT X
NEXT i
CLOSE #1

' NOW THE XM FILE IS READ AND I DON'T NEED IT ANYMORE

FOR i = 1 TO canali
OPEN "temp\C" + LTRIM$(STR$(i)) + ".tmp" FOR BINARY AS #2
FOR X = 0 TO songlung - 1
OPEN "temp\P" + LTRIM$(STR$(tpattern(X))) + "C" + LTRIM$(STR$(i)) + ".tmp" FOR BINARY AS #1
a$ = SPACE$(Lunpa(tpattern(X)) * 5)
GET #1, , a$
PUT #2, , a$
CLOSE #1
NEXT X
CLOSE #2
NEXT i

DIM lrestart AS LONG

DIM lunall AS LONG

FOR X = 0 TO songlung - 1
IF X = restart THEN lrestart = lunall
lunall = lunall + Lunpa(tpattern(X))
NEXT X

PRINT #2904, lrestart
PRINT #2904, lunall
CLOSE

END FUNCTION


FUNCTION LDWORD(a$) AS LONG
LDWORD = (ASC(MID$(a$, 4, 1)) * 16777216) + (ASC(MID$(a$, 3, 1)) * 65536) + (ASC(MID$(a$, 2, 1)) * 256&) + (ASC(MID$(a$, 1, 1)))
END FUNCTION

FUNCTION RWORD(a$) AS LONG
RWORD = ((ASC(MID$(a$, 2, 1)) * 256) + ASC(MID$(a$, 1, 1)))
END FUNCTION
