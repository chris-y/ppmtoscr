/* 3.3 changes: Added "NOHEADER" switch */

MODULE 'dos/dos',
       'exec/memory',
       'graphics/rastport'
       
DEF fhhead,fhand,fhandin,fhid,scrfile[512]:STRING,okay,headerfile[512]:STRING
DEF ftypeid  /* :STRING[6144] */  /* [147456]:STRING */
DEF cm,os3=FALSE
DEF h,z,x,y,b,l,k,j,i,m,col,bin,blk,a,attr,red[8]:ARRAY OF INT,grn[8]:ARRAY OF INT,blu[8]:ARRAY OF INT,dot
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
DEF filename[512]:STRING,writeheader=1

ENUM ERR_NONE,ERR_FILE,ERR_DTYP,ERR_NPIC,ERR_MEM,ERR_BTMP,ERR_LIB,ERR_ABOR,ERR_ASL,ERR_ICON,ERR_ARGS,ERR_GTLS,ERR_PPM

PROC main() HANDLE

StrCopy(verstring,'$VER:scrtoppm 1.0 (29.11.98)')

-> StrCopy(endmsg,'Conversion Failed!')

-> Add obligatory ALS switch to ReadArgs() ???!

rargs:=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
templ:='SCRFILE/A,PPMFILE/A,FORM=SAVEFORMAT/K,COL=COLOURSENSE/K/N,BRT=BRIGHTSENSE/K/N,WHT=WHITESENSE/K/N,GRNMAG=GREENMAGENTA/K/N,BLUCYN=BLUECYAN/K/N,REDYEL=REDYELLOW/K/N,RED=FLASHRED/K/N,GRN=FLASHGREEN/K/N,BLU=FLASHBLUE/K/N,GREYSCALE/S,NOBRIGHT/S,NOSMOOTH/S,OS=ROMREMAP/S,ALTPAL=ALTROMPALETTE/S,NOHEADER/S,QUIET/S'
rdargs:=ReadArgs(templ,rargs,NIL)

-> WriteF('\s\n',rargs)

	IF (ppmmem:=AllocMem(6913, MEMF_CLEAR))=NIL THEN Raise(ERR_MEM)
	/* Note: 147456 = 256*192*3 bytes (enuff room for PPM sans header) */

 IF rdargs

IF rargs[0] THEN StrCopy(filename,rargs[0])
IF rargs[1] THEN StrCopy(scrfile,rargs[1])
UpperStr(rargs[2])
IF StrCmp(rargs[2],'SCR') THEN saveformat:=0
IF StrCmp(rargs[2],'ZX82') THEN saveformat:=1
IF StrCmp(rargs[2],'BYTES') THEN saveformat:=2
IF StrCmp(rargs[2],'TAP') THEN saveformat:=3
IF StrCmp(rargs[2],'TZX') THEN saveformat:=4
IF rargs[3] THEN thr:=Long(rargs[3])
IF rargs[4] THEN brthr:=Long(rargs[4])
IF rargs[5] THEN wtthr:=Long(rargs[5])

IF rargs[6] THEN grnmag:=Long(rargs[6])
IF rargs[7] THEN blucyn:=Long(rargs[7])
IF rargs[8] THEN redyel:=Long(rargs[8])

IF rargs[9] THEN flashred:=Long(rargs[9])
IF rargs[10] THEN flashgrn:=Long(rargs[10])
IF rargs[11] THEN flashblu:=Long(rargs[11])
IF rargs[12]=TRUE THEN thr:=5000
IF rargs[13]=TRUE THEN brthr:=5000
IF rargs[14]=TRUE THEN smooth:=0
IF rargs[15]=TRUE THEN os3:=TRUE
IF rargs[16]=TRUE THEN rompal:=TRUE
IF rargs[17]=TRUE THEN writeheader:=0
IF rargs[18]=TRUE THEN quiet:=TRUE
FreeArgs(rdargs)

ELSE
  Raise(ERR_ARGS)
ENDIF

IF quiet=0
  WriteF('scrtoppm 1.0\nby Chris Young <unsatisfactory@bigfoot.com>\n© 1998 unsatisfactory software\n\n')
IF os3<>FALSE THEN WriteF('You are set to run in OS3 mode...\nBe warned that this may give poor results!\n\n')
ENDIF

setuppalette()
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

IF ppmmem THEN FreeMem(ppmmem,6913)

ENDPROC

PROC ppmtoscr()

IF fhandin:=Open(filename,OLDFILE)

IF quiet=0 THEN WriteF('Reading PPM file...\n')

okay:=Read(fhandin,ppmmem,6914)   /*147456   Fgets() */

IF okay
  IF quiet=0 THEN WriteF('Read data OK\n')
ENDIF

Close(fhandin)
ENDIF

IF fhand:=Open(scrfile,NEWFILE)
Fputs(fhand,'P6')
FputC(fhand,$0A)
Fputs(fhand,'256 192')
FputC(fhand,$0A)
Fputs(fhand,'255')
FputC(fhand,$0A)

 /* 16 */

a:=0 /*-172*/
x:=0

FOR l:=1 TO 3

FOR k:=1 TO 8

/* rows */

FOR j:=1 TO 8

FOR i:=1 TO 32


bin:=0

blk:=i+x

-> WriteF('\d\n',blk)



/* say (c2d(substr(data,a,3))) */

mapcol(0,%10000000)

  
mapcol(0,%01000000)


mapcol(0,%00100000)


mapcol(0,%00010000)


mapcol(0,%00001000)


mapcol(0,%00000100)


mapcol(0,%00000010)


mapcol(0,%00000001)



a:=a+1

ENDFOR

/* jump 224 bytes */

a:=a+224 -> 224 /* 5376 */ /* 672? */
-> WriteF('\d\n',a)

ENDFOR

/* back 192 (224?) x 8 bytes */

a:=a-1824 -> -1824  ->1344   /*  48384  */

x:=x+32

-> WriteF('2:\d\n',a)

/* -(8x8x32x8x3)+(32x8x3) */
/* +576 / remove 32*8*8 not 32*8*1 ????? */
/* 16384*3? -32*8? (12544*3) / 5376?  ->->->  8*8*32*8  *3 ?????????? */

ENDFOR

/* third of a screen - check this! */



a:=a+256  /*43008*/    /* 43013 */  /* 43007 . . . 42224  was 48384 */

-> WriteF('Thirds: (2049) \d\n',a)

-> x:=x+256

/*147502    147472 (1)*/     /* say a /* 49167???? */  */

ENDFOR

Flush(fhand)

Close(fhand)

StrCopy(endmsg,'Conversion done - all OK!')
ENDIF
ENDPROC

PROC mapcol(offset,binary)
IF (ppmmem[a] AND binary)
	choosecol(0)
ELSE
	choosecol(1)
ENDIF

FputC(fhand,red[col])
FputC(fhand,grn[col])
FputC(fhand,blu[col])
ENDPROC

PROC choosecol(papink)
col:=0

IF And((ppmmem[blk+6144]),64)=64 THEN bright:=1 ELSE bright:=0
IF And((ppmmem[blk+6144]),128)=128 THEN flash:=1 ELSE flash:=0

IF papink=0
	IF And((ppmmem[blk+6144]),1)=1 THEN col:=1
	IF And((ppmmem[blk+6144]),2)=2 THEN col:=2
	IF And((ppmmem[blk+6144]),3)=3 THEN col:=3
	IF And((ppmmem[blk+6144]),4)=4 THEN col:=4
	IF And((ppmmem[blk+6144]),5)=5 THEN col:=5
	IF And((ppmmem[blk+6144]),6)=6 THEN col:=6
	IF And((ppmmem[blk+6144]),7)=7 THEN col:=7
ELSE
	IF And((ppmmem[blk+6144]),8)=8 THEN col:=1
	IF And((ppmmem[blk+6144]),16)=16 THEN col:=2
	IF And((ppmmem[blk+6144]),24)=24 THEN col:=3
	IF And((ppmmem[blk+6144]),32)=32 THEN col:=4
	IF And((ppmmem[blk+6144]),40)=40 THEN col:=5
	IF And((ppmmem[blk+6144]),48)=48 THEN col:=6
	IF And((ppmmem[blk+6144]),56)=56 THEN col:=7
ENDIF

-> WriteF('\d\n',col)
ENDPROC

PROC setuppalette()
red[0]:=0
grn[0]:=0
blu[0]:=0
red[1]:=0
grn[1]:=0
blu[1]:=221
red[2]:=221
grn[2]:=0
blu[2]:=0
red[3]:=221
grn[3]:=0
blu[3]:=221
red[4]:=0
grn[4]:=221
blu[4]:=0
red[5]:=0
grn[5]:=221
blu[5]:=221
red[6]:=221
grn[6]:=221
blu[6]:=0
red[7]:=221
grn[7]:=221
blu[7]:=221
ENDPROC

