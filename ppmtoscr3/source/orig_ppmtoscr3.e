MODULE 'dos/dos',
       'exec/memory',
       'graphics/rastport'
       
DEF fhhead,fhand,fhandin,fhid,scrfile[512]:STRING,okay,headerfile[512]:STRING
DEF ftypeid  /* :STRING[6144] */  /* [147456]:STRING */
DEF cm,os3=FALSE
DEF h,z,x,y,b,l,k,j,i,m,col,bin,blk,a,attr,red,grn,blu,dot
DEF ink[769]:ARRAY OF INT,paper[769]:ARRAY OF INT,best[9]:ARRAY OF INT
DEF fname[50]:STRING
DEF verstring[30]:STRING,saveformat=0,checksum,ppmmem
DEF headerbytes[23]:ARRAY,zx82header[11]:ARRAY,headcheck
DEF thr=20,smooth=1,endmsg[100]:STRING,rompal=0
DEF templ,rdargs,rargs=NIL:PTR TO LONG
DEF bright,brightattr[769]:ARRAY OF INT,availpix,brthr=450,wtthr=200
DEF quiet=0
DEF wintit[50]:STRING
DEF flashred=-1,flashgrn=-1,flashblu=-1,flash[769]:ARRAY OF INT
DEF grnmag=0,blucyn=20,redyel=40,thr3,thr4,filetypeerror[100]:STRING
DEF filename[512]:STRING

ENUM ERR_NONE,ERR_FILE,ERR_DTYP,ERR_NPIC,ERR_MEM,ERR_BTMP,ERR_LIB,ERR_ABOR,ERR_ASL,ERR_ICON,ERR_ARGS,ERR_GTLS,ERR_PPM



PROC main() HANDLE

StrCopy(verstring,'$VER:ppmtoscr 3.0 (28.08.98)')

-> StrCopy(endmsg,'Conversion Failed!')

-> Add obligatory ALS switch to ReadArgs() ???!

rargs:=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
templ:='PPMFILE/A,SCRFILE/A,FORM=SAVEFORMAT/K,COL=COLOURSENSE/K/N,BRT=BRIGHTSENSE/K/N,WHT=WHITESENSE/K/N,GRNMAG=GREENMAGENTA/K/N,BLUCYN=BLUECYAN/K/N,REDYEL=REDYELLOW/K/N,RED=FLASHRED/K/N,GRN=FLASHGREEN/K/N,BLU=FLASHBLUE/K/N,GREYSCALE/S,NOBRIGHT/S,NOSMOOTH/S,OS=ROMREMAP/S,ALTPAL=ALTROMPALETTE/S,QUIET/S'
rdargs:=ReadArgs(templ,rargs,NIL)

-> WriteF('\s\n',rargs)

	IF (ppmmem:=AllocMem(147471, MEMF_CLEAR))=NIL THEN Raise(ERR_MEM)
	/* Note: 147456 = 256*192*3 bytes (enuff room for PPM sans header) */

 IF rdargs

IF rargs[0] THEN StrCopy(filename,rargs[0])
IF rargs[1] THEN StrCopy(scrfile,rargs[1])
UpperStr(rargs[2])
IF StrCmp(rargs[2],'SCR') THEN saveformat:=0
IF StrCmp(rargs[2],'ZX82') THEN saveformat:=1
IF StrCmp(rargs[2],'BYTES') THEN saveformat:=2
IF StrCmp(rargs[2],'TAP') THEN saveformat:=3
IF rargs[3] THEN thr:=Long(rargs[3])
IF rargs[4] THEN brthr:=Long(rargs[4])
IF rargs[5] THEN wtthr:=Long(rargs[5])

IF rargs[6] THEN grnmag:=Long(rargs[6])
IF rargs[7] THEN blucyn:=Long(rargs[7])
IF rargs[8] THEN redyel:=Long(rargs[8])

IF rargs[9] THEN flashred:=Long(rargs[9])
IF rargs[10] THEN flashgrn:=Long(rargs[10])
IF rargs[11] THEN flashblu:=Long(rargs[11])
-> **** Doesn't work! **** IF rargs[12]=FALSE THEN scale:=1
IF rargs[12]=TRUE THEN thr:=5000
IF rargs[13]=TRUE THEN brthr:=5000
IF rargs[14]=TRUE THEN smooth:=0
IF rargs[15]=TRUE THEN os3:=TRUE
IF rargs[16]=TRUE THEN rompal:=TRUE
IF rargs[17]=TRUE THEN quiet:=TRUE
FreeArgs(rdargs)

ELSE
  Raise(ERR_ARGS)
ENDIF

IF quiet=0
  WriteF('ppmtoscr 3.0\nby Chris Young <unsatisfactory@bigfoot.com>\n© 1998 unsatisfactory software\n\n')
IF os3<>FALSE THEN WriteF('You are set to run in OS3 mode...\nBe warned that this may give poor results!\n\n')
ENDIF

os3setup()
ppmtoscr()






EXCEPT DO

	IF exception
->	  IF quiet=0 THEN WriteF('*** ERROR ***\n')
	
	SELECT exception
	CASE ERR_FILE ; StrCopy(endmsg,'Couldn''t open file!')
	CASE ERR_DTYP ; StrCopy(endmsg,'Datatypes error!')
	CASE ERR_NPIC ; StrCopy(endmsg,filetypeerror)
	CASE ERR_MEM  ; StrCopy(endmsg,'Not enough memory!')
	CASE ERR_BTMP ; StrCopy(endmsg,'Could not allocate BitMap!')
	CASE ERR_LIB  ; StrCopy(endmsg,'Could not open datatypes.library v39+')
     CASE ERR_ABOR ; StrCopy(endmsg,'Aborted by user!')
     CASE ERR_ASL  ; StrCopy(endmsg,'Could not open asl.library 37+')
	CASE ERR_ICON ; StrCopy(endmsg,'Could not open icon.library 33+')
	CASE ERR_GTLS ; StrCopy(endmsg,'Could not open gadtools.library 37+')
	CASE ERR_ARGS ; StrCopy(endmsg,'required argument missing')
	CASE ERR_PPM  ; StrCopy(endmsg,'Filetype not supported!  (Not a P6 PPM)')
	ENDSELECT
	
ENDIF
       IF quiet=0 THEN WriteF('\s\n',endmsg)

IF ppmmem THEN FreeMem(ppmmem,147471)

ENDPROC

PROC ppmtoscr()

IF (ftypeid:=AllocMem(10,MEMF_CLEAR))=NIL THEN Raise(ERR_MEM)

IF fhid:=Open(filename,OLDFILE)
IF quiet=0 THEN WriteF('Reading input file...\n')

okay:=Read(fhid,ftypeid,4)

IF okay
 IF quiet=0 THEN WriteF('Read header OK\n')
ENDIF

Close(fhid)
ENDIF

IF StrCmp(ftypeid,'P6',2)
StrCopy(wintit,'Format: P6 (PPM RAWBITS)')
 IF quiet=0 THEN WriteF('\s\n\n',wintit)
ELSE
 Raise(ERR_PPM)
ENDIF

IF ftypeid THEN FreeMem(ftypeid,10)

IF fhandin:=Open(filename,OLDFILE)

IF quiet=0 THEN WriteF('Reading PPM file...\n')

okay:=Read(fhandin,ppmmem,147471)   /*147456   Fgets() */

IF okay
  IF quiet=0 THEN WriteF('Read data OK\n')
ENDIF

Close(fhandin)
ENDIF

IF saveformat=0
 IF quiet=0 THEN WriteF('Save Format: Raw SCR\n')
ENDIF

IF saveformat=2
   IF quiet=0 THEN WriteF('Save Format: .header/.bytes\n')
   dot:=InStr(scrfile,'.')
IF dot=NIL THEN StrCopy(headerfile,scrfile) ELSE StrCopy(headerfile,scrfile,dot)
   StrAdd(headerfile,'.header')
->   dot:=InStr(savereq.file,'.')
->   StrCopy(specname,savereq.file,dot)
ENDIF

IF saveformat=3
  IF quiet=0 THEN WriteF('Save Format: Z80 Tape Image\n')
ENDIF

IF saveformat=1
  IF quiet=0 THEN WriteF('Save Format: ZX82\n')
ENDIF

headcheck:=0

headerbytes[0]:=$13  /* .tap only */
headerbytes[1]:=$00  /* .tap only */
headerbytes[2]:=$00
headerbytes[3]:=$03

-> headcheck:=$F8
headerbytes[4]:=fname[0]
headerbytes[5]:=fname[1]
headerbytes[6]:=fname[2]
headerbytes[7]:=fname[3]
headerbytes[8]:=fname[4]
headerbytes[9]:=fname[5]
headerbytes[10]:=fname[6]
headerbytes[11]:=fname[7]
headerbytes[12]:=fname[8]
headerbytes[13]:=fname[9]

/*
headerbytes[4]:=$50 * start of filename (10 bytes)*
headerbytes[5]:=$50
headerbytes[6]:=$4D
headerbytes[7]:=$74
headerbytes[8]:=$6F
headerbytes[9]:=$53
headerbytes[10]:=$43
headerbytes[11]:=$52
headerbytes[12]:=$20
headerbytes[13]:=$20 *//* end of filename */

headerbytes[14]:=$00
headerbytes[15]:=$1B
headerbytes[16]:=$00
headerbytes[17]:=$40
headerbytes[18]:=$20
headerbytes[19]:=$80
headerbytes[20]:=$EC /* eor of values 2 to 19 */
headerbytes[21]:=$02 /* .tap only */
headerbytes[22]:=$1B /* .tap only */






IF saveformat=2
  IF fhhead:=Open(headerfile,MODE_NEWFILE)
     IF quiet=0 THEN WriteF('Writing \s file...\n',headerfile)
     FOR h:=2 TO 19 /* 20 */
     headcheck:=Eor(headcheck,headerbytes[h])
     FputC(fhhead,headerbytes[h])
     ENDFOR
     FputC(fhhead,headcheck)
     Flush(fhhead)
     Close(fhhead)
  ENDIF
ENDIF

IF fhand:=Open(scrfile,MODE_NEWFILE)

IF saveformat=3
  IF quiet=0 THEN WriteF('Writing TAP header...\n')
     FOR h:=0 TO 1
     FputC(fhand,headerbytes[h])
     ENDFOR
     FOR h:=2 TO 19
     FputC(fhand,headerbytes[h])
     headcheck:=Eor(headcheck,headerbytes[h])
     ENDFOR
     FputC(fhand,headcheck)
     FOR h:=21 TO 22
     FputC(fhand,headerbytes[h])
     ENDFOR
ENDIF

IF saveformat=1
  IF quiet=0 THEN WriteF('Writing ZX82 header...\n')
zx82header[0]:=$5A
zx82header[1]:=$58
zx82header[2]:=$38
zx82header[3]:=$32
zx82header[4]:=$03
zx82header[5]:=$00
zx82header[6]:=$1B
zx82header[7]:=$00
zx82header[8]:=$40
zx82header[9]:=$00
zx82header[10]:=$80
zx82header[11]:=$00

     FOR h:=0 TO 11
     FputC(fhand,zx82header[h])
     ENDFOR
ENDIF

IF saveformat>1
  FputC(fhand,$FF)
ENDIF

checksum:=$FF

      StrCopy(wintit,'Mapping colours & writing image data...')
IF quiet=0 THEN WriteF('\s\n',wintit)

/* temp stuff *********************
Fputs(fhand,header)
Fputs(fhand,data)
Flush(fhand)
Close(fhand)
JUMP finish
********************************/

/*
do i=1 to 147456   /* 71 */

red.i=substr(data,i,1)
grn.i=readch(data,i,1)
blu.i=readch(data,i,1)

end

*/

/*
FOR z:=1 TO 768
-> ink[z]:=7
paper[z]:=0
ENDFOR
*/

 /* 16 */

a:=15 /*-172*/
x:=0

FOR l:=1 TO 3

FOR k:=1 TO 8

/* rows */


/*********** PROGRESS BAR??????? *******************/
/*
 pcent:=pcent+4

rp:=wptr.rport

      StrCopy(wintit,'Mapping colours & writing image data...')
      IF wbmessage<>NIL 
         SetWindowTitles(wptr,wintit,-1)
         RectFill(rp,3,tbar,pcent,10,3)
      ENDIF
*/

FOR j:=1 TO 8

FOR i:=1 TO 32


bin:=0

blk:=i+x

-> WriteF('\d\n',blk)



/* say (c2d(substr(data,a,3))) */


mapcol(0,%10000000)

  
mapcol(3,%01000000)


mapcol(6,%00100000)


mapcol(9,%00010000)


mapcol(12,%00001000)


mapcol(15,%00000100)


mapcol(18,%00000010)


mapcol(21,%00000001)


FputC(fhand,bin)

checksum:=Eor(checksum,bin)

a:=a+24

ENDFOR

/* jump 224 bytes */

a:=a+5376 /* 5376 */ /* 672? */

x:=x+32



ENDFOR

/* back 192 (224?) x 8 bytes */

a:=a-48384   /*  48384  */

x:=x-256

/* -(8x8x32x8x3)+(32x8x3) */
/* +576 / remove 32*8*8 not 32*8*1 ????? */
/* 16384*3? -32*8? (12544*3) / 5376?  ->->->  8*8*32*8  *3 ?????????? */

ENDFOR

/* third of a screen - check this! */

a:=a+43008  /*43008*/    /* 43013 */  /* 43007 . . . 42224  was 48384 */

x:=x+256

-> WriteF('Thirds: \d\n',a)

/*147502    147472 (1)*/     /* say a /* 49167???? */  */

ENDFOR

      StrCopy(wintit,'Writing colour attributes...')
      IF quiet=0 THEN WriteF('\s\n',wintit)


FOR m:=1 TO 768

/* check attr() command! */

/* ink=0
paper=7 */

attr:=(1*ink[m])+(8*paper[m])+brightattr[m]+flash[m]

-> col=d2c(attr)

FputC(fhand,attr)   /*writech('scr',col)*/

checksum:=Eor(checksum,attr)

ENDFOR

IF saveformat>1
  FputC(fhand,checksum)
ENDIF

Flush(fhand)

Close(fhand)

StrCopy(endmsg,'Conversion done - all OK!')

ENDIF

ENDPROC


PROC mapcol(offset,binary)

/*
MidStr(red,data,a+offset,1)
MidStr(grn,data,a+offset+1,1)
MidStr(blu,data,a+offset+2,1)
*/

IF offset=0 AND k=1
b:=0

FOR y:=0 TO 8
best[y]:=0
ENDFOR

FOR y:=1 TO 8
  FOR z:=1 TO 8

  red:=ppmmem[a+b]
  grn:=ppmmem[a+b+1]
  blu:=ppmmem[a+b+2]
  
->  col,bright:=choosecol(red,grn,blu)
  
choosecol()
  
  best[col]:=best[col]+1
  best[8]:=best[8]+bright
  
  b:=b+3
  ENDFOR
b:=b+744
ENDFOR

FOR y:=1 TO 7
IF best[y]>=best[paper[blk]] THEN paper[blk]:=y
ENDFOR

IF ink[blk]=paper[blk]
  ink[blk]:=paper[blk]+1
  IF ink[blk]=8 THEN ink[blk]:=0
ENDIF

FOR y:=7 TO 0 STEP -1
IF y<>paper[blk]
  IF best[y]>=best[ink[blk]] THEN ink[blk]:=y
ENDIF
ENDFOR

availpix:=(64-best[0])/2

IF best[8]>availpix THEN brightattr[blk]:=64 /* ELSE brightattr[blk]:=0 */

ENDIF

red:=ppmmem[a+offset]
grn:=ppmmem[a+offset+1]
blu:=ppmmem[a+offset+2]

-> col:=choosecol(red,grn,blu)

choosecol()

IF flashred=red
  IF flashgrn=grn
    IF flashblu=blu
      flash[blk]:=128
    ENDIF
  ENDIF
ENDIF

-> WriteF('\d \d \d\n',red,grn,blu)


-> IF red=grn AND grn=blu
-> ENDIF


IF paper[blk]<>col
  IF smooth=1
    IF (Abs(paper[blk]-col)>Abs(ink[blk]-col)) THEN bin:=bin+binary
-> v old ink[blk] := col
  ELSE
  bin:=bin+binary
  ENDIF

ENDIF

ENDPROC col

PROC stdchoosecol() /*(red,grn,blu)*/
DEF blured,blugrn,redgrn,redblu,grnred,grnblu,thr2,redgrnblu


bright:=0

redgrnblu:=red+grn+blu

IF redgrnblu > wtthr THEN col:=7 ELSE col:=0 /* black or white 384 */
   


  IF redgrnblu > brthr THEN bright:=1 /* 573 */

blured:=blu-red
blugrn:=blu-grn
redgrn:=red-grn
redblu:=red-blu
grnred:=grn-red
grnblu:=grn-blu
thr2:=thr
thr3:=thr
thr4:=thr

IF redgrn>thr AND blugrn>thr
  col:=3 /* magenta */
  thr2:=thr+grnmag
ENDIF

IF grnred>thr AND blured>thr
  col:=5 /* cyan */
  thr3:=thr+blucyn
ENDIF

IF redblu>thr AND grnblu>thr
  col:=6 /* yellow */
  thr4:=thr+redyel  /* 40 */
ENDIF

IF blured>thr3
   IF blugrn>thr3 THEN col:=1 /* blue */
ENDIF
IF redgrn>thr4
   IF redblu>thr4 THEN col:=2 /* red */
ENDIF
IF grnred>thr2
   IF grnblu>thr2 THEN col:=4 /* green */
ENDIF

ENDPROC /* col,bright */

PROC os3setup()
cm:=GetColorMap(15)

IF rompal=0 /* ZXDT */
  SetRGB32CM(cm,15,0,0,0) /* spec colour 0 (black) */
  SetRGB32CM(cm,1,0,0,0.6)   /* 153 */
  SetRGB32CM(cm,2,0.6,0,0)
  SetRGB32CM(cm,3,0.6,0,0.6)
  SetRGB32CM(cm,4,0,0.6,0)
  SetRGB32CM(cm,5,0,0.6,0.6)
  SetRGB32CM(cm,6,0.6,0.6,0)
  SetRGB32CM(cm,7,0.6,0.6,0.6) /* WHITE */
  SetRGB32CM(cm,8,0,0,0.67) /* 170 Start of BRIGHT */
  SetRGB32CM(cm,9,0.73,0,0) /* 187 */
  SetRGB32CM(cm,10,0.8,0,0.8) /* 204 */
  SetRGB32CM(cm,11,0,0.8,0)
  SetRGB32CM(cm,12,0,0.87,0.87) /* 221 */
  SetRGB32CM(cm,13,0.93,0.93,0) /*238 */
  SetRGB32CM(cm,14,1,1,1) /* 255 */

ELSE

  SetRGB32CM(cm,15,0,0,0)
  SetRGB32CM(cm,8,0,0,1)
  SetRGB32CM(cm,9,1,0,0)
  SetRGB32CM(cm,10,1,0,1)
  SetRGB32CM(cm,11,0,1,0)
  SetRGB32CM(cm,12,0,1,1)
  SetRGB32CM(cm,13,1,1,0)
  SetRGB32CM(cm,14,1,1,1)
  SetRGB32CM(cm,1,0,0,0.87)
  SetRGB32CM(cm,2,0.87,0,0)
  SetRGB32CM(cm,3,0.87,0,0.87)
  SetRGB32CM(cm,4,0,0.87,0)
  SetRGB32CM(cm,5,0,0.87,0.87)
  SetRGB32CM(cm,6,0.87,0.87,0)
  SetRGB32CM(cm,7,0.87,0.87,0.87)
ENDIF
ENDPROC

PROC choosecol()
IF os3=FALSE
   stdchoosecol()
   RETURN 0
ENDIF

bright:=0
red:=!red/255
grn:=!grn/255
blu:=!blu/255

col:=FindColor(cm,red,grn,blu,15)
IF col=15 THEN col:=0
IF col>7
  bright:=1
  col:=col-7
ENDIF
ENDPROC

