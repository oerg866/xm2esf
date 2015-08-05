#INCLUDE "fbmath.bi"

#MACRO WriteNote(a)

IF ctype(i) = 0 THEN
    curfreq(i) = fmfreq(a)
    PUT #20, , chr(esfchan(i) + &h30)
    PUT #20, , chr(INT(curfreq(i) / 256))
    PUT #20, , chr(INT(curfreq(i) MOD 256))
else
    curfreq(i) = INT((0.5^((a)/12-1))/2*851) 
    if ctype(i) = 3 and noisetype = 0 then PUT #20, , chr(&h3A) else PUT #20, , chr(esfchan(i) + &h30)
    PUT #20, , chr(INT(curfreq(i) MOD 16)) + chr(INT(curfreq(i) / 16))                       
end if

#ENDMACRO

#MACRO WriteCurrentFreq()
IF ctype(i) = 0 THEN
    PUT #20, , chr(esfchan(i) + &h30)
    PUT #20, , chr(INT(curfreq(i) / 256))
    PUT #20, , chr(INT(curfreq(i) MOD 256))
ELSE
    if ctype(i) = 3 and noisetype = 0 then PUT #20, , chr(&h3A) else PUT #20, , chr(esfchan(i) + &h30)
    PUT #20, , chr(INT(curfreq(i) MOD 16)) + chr(INT(curfreq(i) / 16))
END IF
#ENDMACRO



#MACRO WriteVol(a)
IF ctype(i) = 0 THEN
    PUT #20, , chr(esfchan(i) + &h20)
    TEMP = INT(fmvol(quotient(i) * a))
    PUT #20, , chr(TEMP)
ELSE 
    IF ctype(i) = 3 THEN PUT #20, , chr(&H2A) ELSE PUT #20, , chr(esfchan(i) + &H20)
    TEMP = INT(psgvol(quotient(i) * a))
    PUT #20, , chr(TEMP)
END IF
#ENDMACRO

#MACRO WriteCurrentVol()
    WriteVol(curvol(i))
#ENDMACRO

TYPE PATTERNDATA
    note AS STRING * 1
    INST AS STRING * 1
    VC AS STRING * 1
    ET AS STRING * 1
    EP AS STRING * 1
END TYPE

FUNCTION SPLEFT(strn as string) AS STRING
    dim i as long
    dim car as string
    dim cars as string
    dim comp as string
    dim speft as string
    
    i = 1
    car = MID(strn, 1, 1)
    IF car = chr(34) THEN
        comp = chr(34)
        i = 2
        car = MID(strn, 2, 1)
    ELSE
        comp = " "
    END IF
    WHILE car <> comp
        SPEFT = SPEFT + car
        i = i + 1
        car = MID(strn, i, 1)
        IF i > LEN(strn) THEN
            SPLEFT = SPEFT + cars
            EXIT FUNCTION
        END IF
    WEND
    SPLEFT = SPEFT
END FUNCTION


FUNCTION param(strn as string, b as long) AS STRING
    dim i as long
    dim c as long
    dim car as string
    dim cars as string
    dim comp as string
    dim speft as string
    
    c = 0
    i = 1
    car = MID(strn, 1, 1)
    IF car = chr(34) THEN
        comp = chr(34)
        i = 2
        car = MID(strn, 2, 1)
    ELSE
        comp = " "
    END IF
    WHILE c <> b
        WHILE car <> comp
            SPEFT = SPEFT + car
            i = i + 1
            car = MID(strn, i, 1)
            IF i > LEN(strn) THEN
                SPEFT = ""
                param = SPEFT
                EXIT FUNCTION
            END IF
        WEND
        i = i + 1
        c = c + 1
        ' SPEFT$ = MID(strn, i&, 1)
        car = SPEFT
    WEND
    
    
    param = RTRIM(spleft(MID(strn,i)))
    
END FUNCTION

FUNCTION DIVREST(a as double, b as integer) as DOUBLE
    dim last as integer
    dim i as integer
    last = 0
    for i = 0 to a step 1 
        if i / b - int(i / b) = 0  and  i <= a then
            last = i
        end if  
    next i
    divrest = a - last
END FUNCTION

FUNCTION fmfreq(a AS double) AS INTEGER
    dim temp2 as long
    dim temp as long  
    temp2 = INT(644*(2^(divrest(a,12)/12)))
    temp = ((INT(a/12)) * 2048)
    temp2 = INT((temp2 AND 2047) + temp)
    return temp2
END FUNCTION

FUNCTION fmfreq2(a AS integer) AS INTEGER
    fmfreq2 = fmfreq(cast(double,a))
END FUNCTION

FUNCTION fmvol(a AS DOUBLE) AS BYTE
    
    DIM b AS INTEGER
    if a = 0 then
        b = 127 
    else
        b = int((10 * (LOG((64/(a))^2) / LOG(10)))/0.375)
    end if
    
    IF b > 127 THEN b = 127
    IF b < 0 THEN b = 0
    IF a = 64 THEN b = 0
    IF a = 0  THEN b = 127
    
    fmvol= b
    
END FUNCTION

FUNCTION fmvol2(a AS INTEGER) AS BYTE
    
    DIM b AS INTEGER
    if a = 0 then
        b = 127 
    else
        b = int((10 * (LOG((64/(a))^2) / LOG(10)))/0.375)
    end if
    
    IF b > 127 THEN b = 127
    IF b < 0 THEN b = 0
    IF a = 64 THEN b = 0
    IF a = 0  THEN b= 127
    
    fmvol2= b
END FUNCTION


FUNCTION psgvol(a AS DOUBLE) AS BYTE
    
    DIM b AS INTEGER
    b =   -(INT(LOG10(a / 64.0) * 7))
    
    
    IF b > 15 THEN b = 15
    IF a = 64 THEN b = 0
    IF a = 0  THEN b = 15
    psgvol= b
    
END FUNCTION

FUNCTION psgvol2(a AS INTEGER) AS BYTE
    
    DIM b AS INTEGER
    b =   -(INT(LOG10(a / 64.0) * 7))
    
    IF b > 15 THEN b = 15
    IF a = 64 THEN b = 0
    IF a = 0  THEN b = 15
    psgvol2 = b
    
END FUNCTION
'************************************
'* XM2ESF                           *
'************************************
'
' Converts XM-format module music to
' Echo Stream Format
'
' VERSION:

dim builddate as string
dim version as string
builddate = "08-06-2015"
version = "1.1 Beta 2"
#IFDEF __FB_DOS__
version = version + " DOS"
#Endif
#IFDEF __FB_WIN32__
version = version + " Win32"
#Endif
#IFDEF __FB_Linux__
version = version + " Linux"
#Endif

'
' (C) 2009-2015 Oerg866
'
' Developed in compliance with official Echo docs. (C) 2010-2015 Sik, Oerg866, TiTAN

'
RMDIR "./TEMP"
MKDIR "./TEMP"


PRINT "XM2ESF - XM to Echo Stream Format converter"
PRINT ""
PRINT "Version " + version + " " + builddate
PRINT ""
PRINT "Copyright (C) 2011-2015 Oerg866             http://www.mdscene.net"
PRINT "Email: oerg866-at-tototek-dot-com           http://github.com/oerg866/xm2esf"
PRINT ""
PRINT "Echo (C) 2010-2015 Sik, Oerg866, TiTAN      http://echo.mdscene.net"
PRINT ""
PRINT "Please report bugs using email at oerg866-at-tototek-dot-com or use any"
PRINT "other viable method, such as forum threads, IRC, skype or similar."
PRINT ""
PRINT "For license information, please see the included LICENSE.TXT file."
PRINT ""
COLOR 14,0
PRINT "Please acknowledge that this program is not in a stable state yet"
PRINT "and its creator can not be held responsible for consequences"
PRINT "evolving from the usage, be it proper or improper, of this"
PRINT "program."
COLOR 7,0
PRINT ""
PRINT "Also, lots of hours of bugfixing and testing went into this."
PRINT "Please keep this in mind when using this program."
PRINT ""
PRINT ""

SLEEP 1000

IF COMMAND$ = "" THEN
    PRINT "usage: xm2esf <infile> <outfile> -<switches>"
    PRINT ""
    PRINT "<infile>  is an XIF file. An XIF file is a descriptor"
    PRINT "          file that contains all the parameters which xm2esf will"
    PRINT "          respect while creating the resulting ESF file, and also the"
    PRINT "          path and filename to the input XM file."
    PRINT ""
    PRINT "          Creating an XIF file is possible using the bundled xm2esfgui"
    PRINT "          It is very easy to use and creates a XIF file"
    PRINT "          that is perfectly parseable by xm2esf :)"
    PRINT ""
    PRINT "          For other software and information, please visit"
    PRINT "          http://segaretro.org/Echo"
    PRINT ""
    PRINT "<outfile> is the output path ad filename xm2esf will use to create"
    PRINT "          the resulting ESF file. Please note that if this file exists"
    PRINT "          it WILL BE OVERWRITTEN, even if you encounter a bug and the"
    PRINT "          conversion somehow fails ."
    PRINT ""
    print "Switches:"
    print ""
    print "-p        Retrigger volume envelope on PSG channel when the instrument column"
    print "          is filled when there is one of the standard portamento/vibrato"
    print "          effects playing. (1xx, 2xx, 3xx and 4xx)"
    print "-r        Like -p, except it triggers regardless of the effect column's"
    print "          content."
    PRINT ""
    PRINT "Bye :D!"
    
    
    
    END
END IF

dim cmdl as integer

dim psgretrig as byte
dim psgretrigall as byte
dim psgignore as byte

for cmdl = 3 to 255
    if command$(cmdl) = "" then exit for
    select case lcase(command$(cmdl))
    case "-r"
        psgretrigall = 1
    case "-p"
        psgretrig = 1
    case "-i"
        psgignore = 1
    end select
next cmdl

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' INITIALIZE ALL VARIABLES
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


'' XM to ESF channel assignment variables

DIM xmfm(1 TO 6) as long            ' Contains the corresponding XM channel for each PSG channel
DIM xmpsg(1 TO 3) as long           ' Contains the corresponding XM channel for each PSG channel
dim xmnoise as long
dim xmpcm as long
dim xmsync as long

DIM fm as long                      ' Amount of FM channels
DIM psg as long                     ' Amount of PSG channels
dim pcm as long
DIM present(1 TO 11) as long        ' Used to define if a channel is present

'' XM to ESF Instrument assignment variables

DIM esfins(1 TO 256) as long        ' Contains the corresponding Echo Instrument ID (in Pointer List)
' for each XM instrument

'' XM to ESF Song adjustment variables

DIM pitch(1 TO 11) as long          ' Transpose value for each channel
DIM vol(1 TO 11) as long            ' Channel volume for every channel
DIM quotient(1 TO 11) AS DOUBLE     ' Volume conversion quotient

'' XM to ESF Conversion loop and temporary variables:

dim xmnote as long
dim xmvol as long
dim xmeff as long
dim xmeffdat as long
DIM i AS INTEGER                    ' For some loops
DIM currow AS LONG                  ' Current row of entire song that's being processed

'' XM to ESF stream data bridge variables

DIM row(1 TO 11) as string          ' Current XM row for each channel
DIM curins(1 TO 11) as long         ' Current XM instrument
DIM curnote(1 TO 11) as long        ' Current note (XM note with transpose already applied !)
DIM curvol(1 TO 11) as long         ' Current volume
DIM curfreq(1 TO 9) as long         ' Current frequency (Used when writing new freq during portamento, vibrato and arpeggio)

DIM effectdat(1 TO 11) as long      ' Current Effect number
DIM effectval(1 TO 11) as long      ' Current Effect value

DIM xmins as long                   ' Instrument column in current XM row

DIM slidestep(1 TO 11) AS DOUBLE    ' Current step during a slide (effects 1,2 and 3) Unit: XM notes

DIM slidetarget(1 TO 11) as long    ' Target for a slide (effects 1,2, and 3) Unit: XM Notes
DIM slidespeed(1 TO 11) as long     ' Amount of XM notes to slide per tick

DIM volslidepos(1 TO 11) AS DOUBLE  ' Current position in volume slide (effect A), unit: XM volume
DIM volslidespeed(1 TO 11) AS DOUBLE' Amount of volume to slide per tick

DIM arpnote1(1 TO 11) as long       ' Note 1 in arpeggio (effect 0)
DIM arpnote2(1 TO 11) as long       ' Note 2 in arpeggio (effect 0)

DIM vibstep(1 TO 11) AS DOUBLE      ' Current step in a vibrato (note this is NOT in XM notes!
' It's just the amouont of ticks that already have been
' processed in a vibrato! it starts at 0, so that a smooth
' sinewave can be applied which starts at the current note!)

DIM vibspeed(1 TO 11) as long       ' Vibrato speed (1st parameter in effect 4)
DIM vibdepth(1 TO 11) as long       ' Vibrato depth (2nd parameter in effect 4)

DIM conversion AS DOUBLE            ' note to frequency conversion variable


'' XM to ESF Conversion initalization variables

DIM esfchan(1 TO 11) as long        ' This is pretty interesting.
' (and VERY important I might add)

esfchan(1)  = 0                     ' The echo stream format definitions use direct YM2612/PSG
esfchan(2)  = 1                     ' channel values. These are not 0-10 or something like that
esfchan(3)  = 2                     ' but have gaps in them due to the way the chip is constructed.
esfchan(4)  = 4                     ' This bridge variable takes care of this.
esfchan(5)  = 5
esfchan(6)  = 6
esfchan(7)  = 8
esfchan(8)  = 9
esfchan(9)  = 10
esfchan(10) = 12
esfchan(11) = 11

DIM fmnote(0 TO 11) as long         ' Pre-defined FM frequencies for every note (source: echo table)

fmnote(0) = 644
fmnote(1) = 681
fmnote(2) = 722
fmnote(3) = 765
fmnote(4) = 810
fmnote(5) = 858
fmnote(6) = 910
fmnote(7) = 964
fmnote(8) = 1024
fmnote(9) = 1081
fmnote(10) = 1146
fmnote(11) = 1214

DIM psgnote(0 TO 70) as long        ' Pre-defined PSG frequencies for every note (source: echo table)

DIM ctype(1 TO 11)  AS INTEGER      ' Channel type (defines internally which channels from 1 to 11 are FM, PSG, PCM and Noise

DIM loopins(1 TO 11) as long        ' Variables for (re)storing instruments on looping

DIM lastfx(1 TO 11) as long         ' Variables for picking up pitch related effects
DIM lastfd(1 TO 11) as long         ' Variables for picking up pitch related effects

dim fineslide(1 to 11) as byte


''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' INIT CODE BEGINS HERE :)


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

' Please note: If the XM's Tempo is set to 150, every tick is exactly the same length as one echo tick
' Meaning that the adjustable variable is the "Ticks per row". If XM's tempo is 150, you should set
' the ticks per row value as the speed in the XIF input file. If you do that, your XM will be
' exactly as fast as the echo output!!

' most tempo values other than 150 will mean that the actual esf's speed will only be able to approximate
' the actual XM speed.

dim tempo as long

tempo = 7      ' Default is 7. A tempo of 150;7 is almost the same as the default many trackers use (125;6)


KILL COMMAND$(2)

OPEN COMMAND$(1) FOR INPUT AS #1

' This is the XIF file parser. It is long, and the coding style is probably pretty bad. But! it works :P
' The error handler is not very intelligent yet. But, it'll be, sooner or later. Probably later =P

dim setting as string

dim esfloop as long
dim filetype as long
dim xm as string
dim noise as long
dim noisetype as long
dim noisemode as long
dim restart as long
dim total as long
dim temp as long
dim tmp as long
dim pf as long

dim tmpstr as string


WHILE LCASE(setting) <> "[instruments]"
    LINE INPUT #1, setting
    IF MID(setting, 1, 1) <> "#" THEN
        SELECT CASE spleft(setting)
        
        CASE "FILE"
            xm = param(setting, 1)
            PRINT "XM File: " + xm
        CASE "TYPE"
            SELECT CASE param(setting, 1)
            CASE "BGM"
                filetype = 1
            CASE "SFX"
                filetype = 2
            END SELECT
            SELECT CASE param(setting, 2)
            CASE "LOOP"
                
                esfloop = 1
                IF filetype = 2 THEN
                    PRINT "Input file errorneously declares loop while being a SFX. File rejected."
                    CLOSE
                    END
                END IF
            CASE "NOLOOP"
                esfloop = 0
                
                
            END SELECT
            
        CASE "TEMPO"
            tempo = VAL(param(setting, 1))
        CASE "FM"
            fm = VAL(param(setting, 1))
            IF fm > 6 THEN
                PRINT "Declared more than 6 FM channels."
                CLOSE:END
            END IF
        CASE "PSG"
            psg = VAL(param(setting, 1))
            IF psg > 3 THEN
                PRINT "Declared more than 3 PSG channels."
                CLOSE
                END
            END IF
        CASE "PCM"
            pcm = 1
            IF fm = 6 THEN
                PRINT "Input file errorneously declares PCM even though 6 FM channels are used"
                CLOSE
                END
                
            END IF
        CASE "NOISE"
            noise = VAL(param(setting, 1))
        CASE "FM1"
            xmfm(1) = VAL(param(setting, 1))
        CASE "FM2"
            xmfm(2) = VAL(param(setting, 1))
        CASE "FM3"
            xmfm(3) = VAL(param(setting, 1))
        CASE "FM4"
            xmfm(4) = VAL(param(setting, 1))
        CASE "FM5"
            xmfm(5) = VAL(param(setting, 1))
        CASE "FM6"
            xmfm(6) = VAL(param(setting, 1))
        CASE "PCMC"
            xmpcm = VAL(param(setting,1))
        CASE "PSG1"
            xmpsg(1) = VAL(param(setting, 1))
        CASE "PSG2"
            xmpsg(2) = VAL(param(setting, 1))
        CASE "PSG3"
            xmpsg(3) = VAL(param(setting, 1))
        CASE "PSGN"
            xmnoise = VAL(param(setting, 1))
        CASE "NOISEFREQ" ' 1 = stock, 0 = psg3
            noisetype = VAL(param(setting, 1))
        CASE "NOISETYPE"
            noisemode = VAL(param(setting, 1))
        CASE "SYNC"
            print "XMCSIDFA"
            xmsync = VAL(param(setting, 1))
            '1 = white noise, 0 = periodic noise
            
        END SELECT
    END IF
    
WEND

''''''''''''''''''''''''''''''''''''''''''''''''''''
'' This calls a small program called LOADXM.
'' It is a win32 self-contain port of nineko's
'' original XM loader (QB/VB6). Ported and used
'' with permission. Thanks, nineko!
''''''''''''''''''''''''''''''''''''''''''''''''''''
dim s as string

PRINT "loading XM file: " + xm
s = "loadxm " + xm
print "Executing " + s

SHELL s
''  TODO: ERRORLEVEL checking. Because I was lazy and did not
''        bother to check if loadxm actually was successful,
''        xm2esf will still be let lose if loadxm failed.
''        Oh well ^__^"
PRINT "XM pattern data extracted"
''''''''''''''''''''''''''''''''''''''''''''''''''''

'INSTRUMENT ASSIGNMENTS

WHILE LCASE(setting) <> "[pitch]"
    
    IF MID(setting,1,1) <> "#" THEN
        
        LINE INPUT #1, setting
        esfins(VAL(spleft(setting))) = VAL("&H" + param(setting,1))
        
    ELSE
        LINE INPUT #1, setting
        
    END IF
    
WEND

PRINT "Loaded pitches"

' Internal channel to variable assignment help

' 123456 = FM
' 789    = PSG
' 11     = PCM
' 10     = NSE

WHILE LCASE(setting) <> "[volume]"
    LINE INPUT #1, setting
    IF MID(setting,1,1) <> "#" THEN
        
        SELECT CASE spleft(setting)
        CASE "FM1"
            pitch(1) = VAL(param(setting,1))
        CASE "FM2"
            pitch(2) = VAL(param(setting,1))
        CASE "FM3"
            pitch(3) = VAL(param(setting,1))
        CASE "FM4"
            pitch(4) = VAL(param(setting,1))
        CASE "FM5"
            pitch(5) = VAL(param(setting,1))
        CASE "FM6"
            pitch(6) = VAL(param(setting,1))
            
        CASE "PSG1"
            pitch(7) = VAL(param(setting,1))
        CASE "PSG2"
            pitch(8) = VAL(param(setting,1))
        CASE "PSG3"
            pitch(9) = VAL(param(setting,1))
        CASE "PSGN"
            pitch(11) = VAL(param(setting,1))
        END SELECT
    END IF
    
    
WEND

WHILE LCASE(setting) <> "[end]"
    LINE INPUT #1, setting
    IF MID(setting,1,1) <> "#" THEN
        
        SELECT CASE spleft(setting)
        CASE "FM1"
            vol(1) = VAL(param(setting,1))
        CASE "FM2"
            vol(2) = VAL(param(setting,1))
        CASE "FM3"
            vol(3) = VAL(param(setting,1))
        CASE "FM4"
            vol(4) = VAL(param(setting,1))
        CASE "FM5"
            vol(5) = VAL(param(setting,1))
        CASE "FM6"
            vol(6) = VAL(param(setting,1))
            
        CASE "PSG1"
            vol(7) = VAL(param(setting,1))
        CASE "PSG2"
            vol(8) = VAL(param(setting,1))
        CASE "PSG3"
            vol(9) = VAL(param(setting,1))
        CASE "PSGN"
            vol(11) = VAL(param(setting,1))
        END SELECT
    END IF
    
    
WEND
'
CLOSE

OPEN COMMAND(2) FOR BINARY AS #20

FOR i = 1 TO fm
    
    present(i) = 1
    OPEN "TEMP\C" + TRIM(STR(xmfm(i))) + ".tmp" FOR BINARY AS #i
    
NEXT i

FOR i = 7 TO psg + 6
    present(i) = 1
    
    OPEN "TEMP\C" + TRIM(STR(xmpsg(i-6))) + ".tmp" FOR BINARY AS #i
    
NEXT i

IF pcm = 1 THEN
    
    present(10) = 1
    OPEN "TEMP\C" + TRIM(STR(xmpcm)) + ".tmp" FOR BINARY AS #10
    
END IF

IF noise = 1 THEN
    
    present(11) = 1
    OPEN "TEMP\C" + TRIM(STR(xmnoise)) + ".tmp" FOR BINARY AS #11
    
END IF
PRINT "Channels assigned and loaded"

OPEN "TEMP\C" + TRIM(STR(xmsync)) + ".tmp" FOR BINARY AS #12

'''''''''''''''''''''''''''''''''''''''''
'' LOAD ALL PREDEFINED PSG FREQUENCIES ''
'''''''''''''''''''''''''''''''''''''''''


dim t as long

t=0
FOR t = 0 TO 70
    READ psgnote(t)
NEXT t



PRINT "PSG Freq init."

' Define channel types
' 0 = FM (default)
' 1 = PSG
' 2 = PCM
' 3 = Noise

ctype(7) = 1
ctype(8) = 1
ctype(9) = 1
ctype(10) = 2 'pcm
ctype(11) = 3 'noise

dim a as string

i = FREEFILE
OPEN "TEMP\file.inf" FOR INPUT AS #i
LINE INPUT #i, a
LINE INPUT #i, a: restart = VAL(a)
LINE INPUT #i, a: total = VAL(a)
CLOSE #i

PRINT "File info read..."

FOR i = 1 TO 11
    row(i) = "     "
    effectdat(i) = 255
    effectval(i) = 255
NEXT i

FOR i = 1 TO 11
    
    quotient(i) = vol(i) / 64
    
NEXT i

' Some SFX handling right here . . .

IF filetype = 2 THEN
    FOR i =  1 TO 11
        IF present(i) = 1 THEN
            IF i = 11 THEN
                PUT #20,, chr(&hE6)
            ELSE
                PUT #20,, chr(&hE0 + esfchan(i))
            END IF
        END IF
    NEXT i
END IF

PRINT "Starting conversion."

tmpstr = "     "

FOR currow = 1 TO total
'    print currow
    IF currow-1 = restart THEN
        IF esfloop = 1 THEN
            PUT #20,, chr(&hFD)
            ' Store instruments for looping
            FOR i = 1 TO 11
                loopins(i) = curins(i)
            NEXT i
        END IF
    END IF
    
    ' SYNC
    
    GET #12, , tmpstr
    
    xmnote=      ASC(MID(tmpstr, 1, 1))
    xmins =      ASC(MID(tmpstr, 2, 1))
    xmvol =      ASC(MID(tmpstr, 3, 1))
    xmeff =      ASC(MID(tmpstr, 4, 1))
    xmeffdat =   ASC(MID(tmpstr, 5, 1))
    
    if xmnote = 97 then
        put #20, , chr(&hFB)
    end if
    
    
    FOR i = 1 TO 11
        IF present(i) = 1 THEN
            
            
            GET #i, , row(i)
            xmnote=      ASC(MID(row(i), 1, 1))
            xmins =      ASC(MID(row(i), 2, 1))
            xmvol =      ASC(MID(row(i), 3, 1))
            xmeff =      ASC(MID(row(i), 4, 1))
            xmeffdat =   ASC(MID(row(i), 5, 1))
            
            ' XM Effects handled by xm2esf:
            '           0xx     Arpeggio
            '           1xx     Portamento up
            '           2xx     Portamento down
            '           3xx     Tone Portamento
            '           4xx     Vibrato
            '           8xx     Set Panning
            '           Axx     Volume Slide
            '           Cxx     Set volume
            '           Dxx     Pattern break (Always interpreted as D00!)
            '           F0x     Set speed
            
            ' XM Volume column commands handled by xm2esf ( NOT YET !!!! ):
            '           vxx     Set Volume
            '           pxx     Set Panning
            
            '               Process XM Volume column
            
            ' Prepare Set Volume
            ''                if xmvol >= &H10 and xmvol <= &H50 then
            '                   xmvoldat = xmvol - &H10
            '                   xmvol = 1
            '               '''''''''''''''''''''''''''''''''''''''''''''''' <XM Effect> Set Volume
            '                IF ctype(i) = 0 THEN
            '                   PUT #20, , chr(esfchan(i) + &H20)
            '                   TEMP = INT(fmvol(xmvoldat * quotient(i)))
            '                   PUT #20, , chr(TEMP)
            '                   curvol(i) = xmvoldat
            '                ELSEIF ctype(i)=1 THEN
            '                   PUT #20, , chr(esfchan(i) + &H20)
            '                   TEMP = INT(psgvol(xmvoldat * quotient(i)))
            '                   PUT #20, , chr(TEMP)
            '                   curvol(i) = xmvoldat
            '                ELSEIF ctype(i) = 3 THEN
            '                   PUT #20, , chr(&h2B)
            '                   TEMP = INT(psgvol(xmvoldat * quotient(i)))
            '                   PUT #20, , chr(TEMP)
            '                   curvol(i) = xmvoldat
            '                ELSE
            '                   'ignore for pcm +noise
            '                END IF
            '               END IF
            
            ' Prepare Set Panning
            '               if xmvol >= &HC0 and xmvol <= &HCF then
            '                   xmvoldat = (xmvol -&HC0)
            '                   xmvol = 2
            '                   'Set Panning
            '                   IF ctype(i) <> 0 AND ctype(i) <> 2 THEN
            '                       PRINT "WARNING: Panning on PSG or Noise channel! Ignoring..."
            '                   ELSE
            '                       if ctype(i) = 2 then put #20, , chr(&hf6) else PUT #20, , chr(esfchan(i) + &hF0)            
            '                       IF xmvoldat = &h08 THEN
            '                           PUT #20, , chr(&hC0)
            '                       ELSEIF xmvoldat > &h08 THEN
            '                           PUT #20, , chr(&h40)
            '                       ELSE
            '                           PUT #20, , chr(&h80)
            '                       END IF
            '                   END IF
            '               END IF
            
            
            IF effectdat(i) <> 255 THEN
                lastfx(i) = effectdat(i)
                lastfd(i) = effectval(i)
            END IF
            '''''''''''''''''''''''''''''''''''''''''''''''' <XM Effect> Set Speed
            IF xmeff = &hF THEN
                tempo = xmeffdat
            END IF
            '''''''''''''''''''''''''''''''''''''''''''''''' Reset any slide attributes when no related effect is in use
            IF xmeff > 4 OR xmeff = 0 THEN
                slidestep(i) = 0
                slidespeed(i) = 0
                slidetarget(i) = 0
            END IF
            '''''''''''''''''''''''''''''''''''''''''''''''' Reset conversion values for effects when no effect is in use
            IF xmeff = 0 AND xmeffdat = 0 THEN
                effectdat(i) = 255
                effectval(i) = 255
            END IF
            '''''''''''''''''''''''''''''''''''''''''''''''' <XM Effect> Arpeggio
            IF xmeff = 0 AND xmeffdat <> 0 THEN
                effectdat(i) = 0
                effectval(i) = xmeffdat
                arpnote1(i) = INT(xmeffdat / 16)
                arpnote2(i) = INT(xmeffdat MOD 16)
                slidestep(i) = curnote(i)
            END IF
            '''''''''''''''''''''''''''''''''''''''''''''''' <XM Effect> Set Panning
            IF xmeff = 8 THEN
                'Set Panning
                IF ctype(i) <> 0 AND ctype(i) <> 2 THEN
                    PRINT "WARNING: Panning on PSG or Noise channel! Ignoring..."
                ELSE
                    if ctype(i) = 2 then put #20, , chr(&hf6) else PUT #20, , chr(esfchan(i) + &hF0)            
                    IF xmeffdat = &h80 THEN
                        PUT #20, , chr(&hC0)
                    ELSEIF xmeffdat > &h80 THEN
                        PUT #20, , chr(&h40)
                    ELSE
                        PUT #20, , chr(&h80)
                    END IF
                END IF
            END IF
            '''''''''''''''''''''''''''''''''''''''''''''''' <XM Effect> Vibrato
            IF xmeff = 4 THEN
                
                IF effectdat(i) = 4 THEN
                    vibspeed(i) = INT(xmeffdat / 16)
                    vibdepth(i) = INT(xmeffdat MOD 16)
                    effectdat(i) = 4
                    effectval(i) = xmeffdat
                ELSE
                    effectdat(i) = 4
                    effectval(i) = xmeffdat
                    vibspeed(i) = INT(xmeffdat / 16)
                    vibdepth(i) = INT(xmeffdat MOD 16)
                    vibstep(i) = 0
                END IF
            END IF
            '''''''''''''''''''''''''''''''''''''''''''''''' <XM Effect> Tone Portamento
            IF xmeff = 3 THEN
                IF lastfx(i) > 4 THEN slidestep(i) = curnote(i)
                
                IF effectdat(i) = 3 THEN
                    IF slidespeed(i) < 0 THEN
                        slidespeed(i) = (-1) * xmeffdat
                    ELSE
                        slidespeed(I) = xmeffdat
                    END IF
                END IF
                
                effectdat(i) = 3
                effectval(i) = xmeffdat
                
                IF xmnote > 0 AND xmnote < 97 THEN
                    IF slidestep(i) <> 0 THEN
                        IF xmnote + pitch(i) < slidestep(i) THEN slidespeed(i) = (-1) * xmeffdat ELSE slidespeed(i) = xmeffdat
                        
                    ELSE
                        IF xmnote + pitch(i) < curnote(i) THEN slidespeed(i) = (-1) * xmeffdat ELSE slidespeed(i) = xmeffdat
                        slidestep(i) = curnote(i)
                    END IF
                    slidetarget(i) = xmnote + pitch(i)
                END IF
            END IF
            
            
            IF xmnote < 97 AND xmnote > 0 THEN  curnote(i) = xmnote +pitch(i)
            
            '''''''''''''''''''''''''''''''''''''''''''''''' <XM Effect> Portamento Up / Down
            IF xmeff = 1 OR xmeff = 2 THEN
                IF lastfx(i) > 4 THEN slidestep(i) = curnote(i)
                if xmnote > 0 and xmnote <> 97 then slidestep(i) = xmnote + pitch(i)
                effectdat(i) = xmeff
                effectval(i) = xmeffdat
                slidespeed(i) =  -((xmeff-1)*2-1) *xmeffdat
               ' if xmeffdat = &HFF then print "initiating, speed:"; slidespeed(i); " step:"; slidestep(i) 
                slidetarget(i) = -(xmeff-2) * 96 
            END IF
            
            '''''''''''''''''''''''''''''''''''''''''''''''' Set current note variable
            IF xmnote < 97 AND xmnote > 0 THEN  curnote(i) = xmnote + pitch(i)
            
            '''''''''''''''''''''''''''''''''''''''''''''''' Handle a <Note Off>
            IF xmnote = 97 and ((ctype(i) = 0 or ctype(i) = 2) or ((ctype(i) = 1 or ctype(i) = 3) and psgignore <> 0)) THEN
                PUT #20, , chr(esfchan(i)+&h10)
            END IF
            '''''''''''''''''''''''''''''''''''''''''''''''' Handle a new note
            IF xmnote > 0 AND xmnote < 97 THEN
                IF ctype(i) <> 2 THEN
                    IF xmeff <> 3 THEN
                        IF curins(i) <> xmins AND xmins <> 0 THEN
                            curins(i) = xmins
                            PUT #20, , chr(&H40 + esfchan(i))
                            PUT #20, , chr(INT(esfins(curins(i))))
                        END IF
                        
                        IF (xmeff <> 3) AND (xmeff <> &hc) and (xmvol <> 1) THEN
                            curvol(i) = 64
                            WriteCurrentVol()
                            volslidepos(i) = curvol(i)
                        END IF
                        
                        IF ctype(i) = 0 THEN
                            PUT #20, , chr(esfchan(i))
                            PUT #20, , chr(INT(32 * INT(curnote(i) / 12) + (2 * (curnote(i) MOD 12)) + 1))
                            curfreq(i) = fmfreq(curnote(i))
                        ELSEIF ctype(i) = 1 THEN
                            PUT #20, , chr(esfchan(i))
                            PUT #20, , chr(INT(24 * INT(curnote(i) / 12) + (2 * (curnote(i) MOD 12))))
                            curfreq(i) = INT((0.5^((curnote(i))/12-1))/2*851)
                        ELSE
                            tmp = 0
                            IF noisetype = 1 THEN
                                tmp = 3
                                PUT #20, , chr(&hB)
                                IF noisemode = 0 THEN
                                    PUT #20, , chr(&h0)
                                ELSE
                                    PUT #20, , chr(&h4)
                                END IF
                            ELSE
                                tmp = 3
                                curfreq(i) = INT((0.5^((curnote(i))/12-1))/2*851)
                                PUT #20, , chr(&h3A)
                                PUT #20, , chr(INT(curfreq(i) MOD 16)) + chr(INT(curfreq(i) / 16))
                                PUT #20, , chr(&hB)
                                IF noisemode = 0 THEN
                                    PUT #20, , chr(&h3)
                                ELSE
                                    PUT #20, , chr(&h7)
                                END IF
                                
                            END IF
                        END IF
                    END IF
                ELSEIF ctype(i) = 2 THEN
                    curins(i) = xmins
                    PUT #20, , chr(&hC)
                    PUT #20, , chr(esfins(curins(i)))
                END IF
            END IF
            '''''''''''''''''''''''''''''''''''''''''''''''' <Simulate XM Behavior>
            if (psgretrig = 1 and xmeff > 0 and xmeff < 5) or (psgretrigall = 1) then
                
                if xmins > 0 and (ctype(i) = 1) then'or ctype(i) = 3) then
                    curins(i) = xmins
                 '   print "RETRIGGER"
                    PUT #20, , chr(&H40 + esfchan(i))
                    PUT #20, , chr(INT(esfins(curins(i))))
                    put #20, , chr(&H10 + esfchan(i))
                    put #20, , chr(esfchan(i))
                    put #20, , chr(0)
                    curvol(i)=64
                    WriteCurrentFreq()
                    WriteCurrentVol()
                end if
            end if
            '''''''''''''''''''''''''''''''''''''''''''''''' <XM Effect> Volume Slide
            IF xmeff = &HA THEN
                IF ctype(i) <> 2 THEN
                    IF effectdat(i) <> &hA THEN
                        effectdat(i) = &hA
                        volslidepos(i) = curvol(i)
                    END IF
                    IF INT(xmeffdat / 16) = &hF AND INT(xmeffdat MOD 16) > 0 THEN
                        fineslide(i) = 1
                    ELSEIF INT(xmeffdat MOD 16) = &hF AND INT(xmeffdat / 16) > 0 THEN
                        fineslide(i) = 1
                    ELSE
                        fineslide(i) = 0
                        IF INT(xmeffdat MOD 16) > 0 THEN
                            ' Volume slide DOWN
                            volslidespeed(i) = -(INT(xmeffdat MOD 16))
                            effectval(i) = INT(xmeffdat MOD 16)
                        ELSEIF INT(xmeffdat / 16) > 0 THEN
                            volslidespeed(i) = INT(xmeffdat / 16)
                            effectval(i) = INT(xmeffdat / 16) 
                        END IF
                    END IF
                END IF
            END IF
            '''''''''''''''''''''''''''''''''''''''''''''''' <XM Effect> Set Volume
            IF xmeff = &HC THEN
                if ctype(i) = 2 then
                    'ignore for pcm +noise
                    effectdat(i) = 255
                    effectval(i) = 255
                Else
                    effectdat(i)= &HC
                    effectval(i) = xmeffdat
                    curvol(i) = xmeffdat
                    WriteCurrentVol()
                end if
            END IF
        END IF
    NEXT i
    
    '''''''''''''''''''''''''''''''''''''''''''''''' Process current effects inbetween rows
    FOR pf = 1 TO TEMPo
        FOR i = 1 TO 11
            
            SELECT CASE effectdat(i)
            
            
            CASE &h0
                IF effectval(i) <> 0 THEN
                    IF ctype(i) < 2 THEN
                        SELECT CASE pf MOD 3
                        CASE 0
                            slidestep(i) = curnote(i)
                        CASE 1
                            slidestep(i) = curnote(i) + arpnote1(i)
                        CASE 2
                            slidestep(i) = curnote(i) + arpnote2(i)
                        END SELECT
                        WriteNote(slidestep(i))
                    END IF
                END IF
            CASE &hA
                'Volume slide
                    if (fineslide(i) = 1 and pf = 1) or (fineslide(i) = 0) then
                        IF ctype(i) <> 2 THEN
                            IF volslidepos(i) < 65 AND volslidepos(i) > -1 THEN
                                volslidepos(i) = volslidepos(i) + volslidespeed(i)
                                IF volslidepos(i) < 0 THEN volslidepos(i) = 0
                                curvol(i) = volslidepos(i)
                            END IF
                            
                            IF volslidepos(i) < 65 AND volslidepos(i) > -1 THEN
                               WriteVol(volslidepos(i))
                            END IF
                            
                        END IF
                    end if                
            CASE 1 TO 3
                slidestep(i) = slidestep(i) + slidespeed(i) / 16
                IF slidespeed(i) < 0 AND slidetarget(i) > slidestep(i) THEN slidestep(i) = slidetarget(i)
                IF slidespeed(i) > 0 AND slidetarget(i) < slidestep(i) THEN slidestep(i) = slidetarget(i)
                WriteNote(slidestep(i))
            CASE 4
                vibstep(i) = vibstep(i) + vibspeed(i)*4
                conversion = SIN(pi/180 * vibstep(i))*vibdepth(i)/5 + curnote(i)
                slidestep(i) = conversion
                WriteNote(slidestep(i))
            END SELECT
        NEXT i
        
        PUT #20, , chr(&HFE)
        PUT #20, , chr(&H1)
    NEXT pf
NEXT currow

IF esfloop = 0 THEN
    PUT #20, , chr(&hFF)
ELSEif restart > 0 then
    ' Restore instruments on looping
    FOR i = 1 TO 6
        IF loopins(i) <> curins(i) AND i <> 10 THEN
            PUT #20, , chr(&H40 + esfchan(i))
            PUT #20, , chr(INT(esfins(loopins(i))))
        END IF
    NEXT i
    PUT #20, , chr(&hFC)
ELSE  
    PUT #20, , chr(&hFC)        
end if

PRINT "Conversion done!"

CLOSE
END


DATA 851, 803, 758, 715, 675, 637, 601, 568, 536, 506, 477, 450, 425, 401, 379, 357, 337, 318, 300
DATA 284,268,253,238,225,212,200,189,178,168,159,150,142,134,126,119,112,106,100
DATA  94, 89, 84, 79, 75, 71, 67, 63, 59, 56, 53, 50, 47, 44, 42, 39, 37, 35, 33
DATA  31, 29, 28, 26, 25, 23, 22, 21, 19, 18, 17, 16, 15, 14, 14
