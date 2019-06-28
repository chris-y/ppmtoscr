OPT OSVERSION=39,PREPROCESS,LARGE,AMIGAE,NOPTRTOCHAR

/*
(this version: 3.1) marked as 3.0; no real differences.
fixed allelsepaper

ideas: set INK to always be the darkest colour
       no attributes option
       gradient sliders
*/
/*
they only range from 0 to 63.  If it's 0 to 255, then the shift value would
be 24.

In E (just guessing, I used to use E myself):

        amiga_pens[i] = ObtainBestPenA(scr.viewport.colormap,
                                        Shl(red[i],26),
                                        Shl(green[i],26),
                                        Shl(blue[i],26),
                                        NIL);
*/

MODULE 'dos/dos',
->       'other/ecode',
       'asl',
       'libraries/asl',
       'exec/memory',
       'icon',
       'intuition/intuition',
       'graphics/rastport',
       'workbench/startup',
       'workbench/workbench',
       'intuition/screens',
       'graphics/text',
       'libraries/gadtools',
       'gadtools',
       'exec/ports',
->       'als',
       'wb',
       'dos/dosasl',
       'utility/hooks',
       'graphics/view',
       'exec/libraries',
		'rexx/storage',
       'tools/arexx',
       'dos/dostags'

MODULE 'datatypes', 'datatypes/datatypes', 'datatypes/datatypesclass',
       'datatypes/pictureclass',
       'graphics/gfx', 'graphics/scale',
       'intuition/gadgetclass', 'intuition/icclass',
       'intuition/screens',
       'tools/bits',          -> Included in case you don't have it
       'utility/tagitem'

DEF fileid,fhid,fhhead,fhand,savereq:PTR TO filerequester,filereq:PTR TO filerequester,scrfile[1024]:STRING,okay,headerfile[1024]:STRING
DEF arexxreq:PTR TO filerequester
DEF cm,os3=FALSE,filetype[512]:STRING,msg[1024]:STRING
DEF h,z,x,y,b,l,k,j,i,m,col,bin,blk,a,attr,red,grn,blu,dot
DEF ink[769]:ARRAY OF INT,paper[769]:ARRAY OF INT,best[9]:ARRAY OF INT
DEF drw[1024]:STRING,fname[1024]:STRING
DEF verstring[30]:STRING,saveformat=0,checksum
DEF headerbytes[23]:ARRAY OF CHAR,zx82header[12]:ARRAY OF CHAR,headcheck
DEF thr=20,smooth=1,endmsg[100]:STRING,rompal=0
DEF templ,rdargs,rargs=NIL:PTR TO LONG
DEF wbarg:PTR TO wbarg,dobj:PTR TO diskobject,toolarray,tt,wbenchMsg:PTR TO wbstartup
DEF bright,brightattr[769]:ARRAY OF INT,availpix,brthr=450,wtthr=200
DEF scale=1,quiet=0,mysc=NIL:PTR TO screen,chg=NIL
DEF wintit[50]:STRING,tbar,wptr=NIL:PTR TO window
DEF flashred=-1,flashgrn=-1,flashblu=-1,flash[769]:ARRAY OF INT
DEF grnmag=0,blucyn=20,redyel=40,thr3,thr4,filetypeerror[1024]:STRING
DEF filename[1024]:STRING, lock=NIL,filebuf,tmpstr[300]:STRING
DEF o                         -> Datatypes object
DEF dbm=NIL:PTR TO bitmap     -> Destination BitMap
DEF rbm=NIL:PTR TO bitmap     -> DT Remapped Destination BitMap
DEF ppmmem[147456]:ARRAY OF CHAR                    -> Memory pointer to PPM data when finished
DEF deetee:PTR TO datatype           -> Another Datatype thingy
DEF bmh[1]:ARRAY OF bitmapheader, bm[1]:ARRAY OF PTR ->bitmap
DEF gads[50]:ARRAY OF LONG
DEF glist[1]:ARRAY OF PTR TO gadget
DEF menustrip=NIL,vi=NIL
DEF loaddrw[512]:STRING
DEF appwinport=NIL:PTR TO mp,appwin=NIL,appwinmsg:PTR TO appmessage
DEF advopts=0,ppm=NIL
DEF advglist[1]:ARRAY OF PTR TO gadget
DEF pixsep=5,rp:PTR TO rastport,vp:PTR TO viewport,cmap:PTR TO colormap
DEF specname:ARRAY OF CHAR,libver:PTR TO lib
DEF testmode=0,mvscreen=0,flashcol=-1,clipboard=0,clipunit=0
DEF arexxport=NIL:PTR TO mp,pwptr=NIL:PTR TO window,pwrp:PTR TO rastport,pwtbar
DEF curwin:PTR TO window,askover=0
DEF objname[30]:STRING,objauth[30]:STRING,objver[30]:STRING,objanno[30]:STRING,objcopy[30]:STRING
DEF objname2[256]:ARRAY OF CHAR,objauth2[256]:ARRAY OF CHAR,objcopy2[256]:ARRAY OF CHAR,objanno2[256]:ARRAY OF CHAR,objver2[256]:ARRAY OF CHAR,tzxarray[3]:ARRAY OF INT
DEF nastyworkaround=0
DEF writeheader=1,tzxborder=0,nofilter=0,noprogress=0,allelseink=1
DEF autonaming=0,noextscale=0,snapshotwindows=0,winx=0,winy=-1
DEF flashpix=0,gifsave=0,gifavailable=0,gifname[1024]:STRING
DEF obtainbest=0,saveattr=1,dither=0,simpleremap=0,dtype=0,range=0,lowval=255
DEF acontrast=0,remapafterdither=0,aslx=0,asly=0
DEF rexxsysbase:PTR TO lib

ENUM ERR_NONE,ERR_FILE,ERR_DTYP,ERR_NPIC,ERR_MEM,ERR_BTMP,ERR_LIB,
     ERR_ABOR,ERR_ASL,ERR_ICON,ERR_ARGS,ERR_GTLS,ERR_WBLB,ERR_ICNN,
    GADG_LOAD,GADG_SAVE,GADG_START,GADG_LOADSTR,GADG_SAVESTR,GADG_TYPE,
    GADG_SCALE,GADG_ROMREMAP,
    ADV_ZXDTPAL,ADV_COLOUR,ADV_WHITE,ADV_BRIGHT,ADV_COLOURNUM,
    ADV_WHITENUM,ADV_BRIGHTNUM,ADV_REDYEL,ADV_BLUCYN,ADV_GRNMAG,
    ADV_REDYELNUM,ADV_BLUCYNNUM,ADV_GRNMAGNUM,ADV_SMOOTH,
    ADV_FLASHRED,ADV_FLASHGRN,ADV_FLASHBLU,
    ADV_FLASHREDNUM,ADV_FLASHBLUNUM,ADV_FLASHGRNNUM,
    ADV_FLASHCOL
    

#define UNSIGNED(x) ((x) AND $FFFF)
/*
#define PDTA_DESTMODE (DTA_DUMMY+251)
#define PMODE_V43 (1)
#define PDTA_MAXDITHERPENS (DTA_DUMMY+221)
*/

PROC main() HANDLE

Rnd(-1)

StrCopy(verstring,'$VER: PDHFIC 3.2 (12.11.2008)')

-> StrCopy(endmsg,'Conversion Failed!')

rargs:=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
templ:='DTFILE/A,SCRFILE,FORM=SAVEFORMAT/K,COL=COLOURSENSE/K/N,BRT=BRIGHTSENSE/K/N,WHT=WHITESENSE/K/N,GRNMAG=GREENMAGENTA/K/N,BLUCYN=BLUECYAN/K/N,REDYEL=REDYELLOW/K/N,RED=FLASHRED/K/N,GRN=FLASHGREEN/K/N,BLU=FLASHBLUE/K/N,NOSCALE/S,GREYSCALE/S,NOBRIGHT/S,NOSMOOTH/S,OS=ROMREMAP/S,ALTPAL=ALTROMPALETTE/S,NOHEADER/S,NOATTR=NOATTRIBUTES/S,COD=DITHER/S,METHOD=DITHERTYPE/K,CLIPBOARD/S,NOCFG=NOCONFIGFILE/S,QUIET/S'

-> WriteF('\s\n',rargs)

    IF (datatypesbase:=OpenLibrary('datatypes.library', 39))=0 THEN Raise(ERR_LIB)

    IF (rexxsysbase:=OpenLibrary('rexxsyslib.library', 37))=0 THEN Raise(ERR_LIB)

 ->   IF (ppmmem:=AllocMem(147456, MEMF_CLEAR))=NIL THEN Raise(ERR_MEM)
    /* Note: 147456 = 256*192*3 bytes (enuff room for PPM sans header) */
    IF (fileid:=AllocMem(11, MEMF_CLEAR))=NIL THEN Raise(ERR_MEM)

tzxarray[0]:=1
tzxarray[1]:=1
tzxarray[2]:=0

IF wbmessage=NIL

rdargs:=ReadArgs(templ,rargs,NIL)

 IF rdargs

IF rargs[23]=NIL
->    readtooltypes(1)
ENDIF
    
IF rargs[19] THEN saveattr:=0

IF rargs[20]
    clipboard:=1
    IF rargs[0] THEN clipunit:=Val(rargs[0])
    StrCopy(filename,'Clipboard (Unit ')
    StrAdd(filename,RealF(tmpstr,clipunit!!FLOAT,0))
    StrAdd(filename,')')
ELSE
    IF rargs[0] THEN StrCopy(filename,rargs[0])
ENDIF
IF rargs[2]
    UpperStr(rargs[2])
    IF StrCmp(rargs[2],'SCR') THEN saveformat:=0
    IF StrCmp(rargs[2],'ZX82') THEN saveformat:=1
    IF StrCmp(rargs[2],'BYTES') THEN saveformat:=2
    IF StrCmp(rargs[2],'TAP') THEN saveformat:=3
    IF StrCmp(rargs[2],'TZX') THEN saveformat:=4
    IF StrCmp(rargs[2],'PLUS3') THEN saveformat:=5
    IF StrCmp(rargs[2],'GIF')
        saveformat:=0
        gifsave:=1
    ENDIF
ENDIF

IF rargs[1]
    StrCopy(scrfile,rargs[1])
ELSE
    autonaming:=1
    autoname()
ENDIF

IF rargs[3] THEN thr:=Long(rargs[3])
IF rargs[4] THEN brthr:=Long(rargs[4])
IF rargs[5] THEN wtthr:=Long(rargs[5])

IF rargs[6] THEN grnmag:=Long(rargs[6])
IF rargs[7] THEN blucyn:=Long(rargs[7])
IF rargs[8] THEN redyel:=Long(rargs[8])

IF rargs[9] THEN flashred:=Long(rargs[9])
IF rargs[10] THEN flashgrn:=Long(rargs[10])
IF rargs[11] THEN flashblu:=Long(rargs[11])
IF rargs[12]=TRUE THEN scale:=0
IF rargs[13]=TRUE THEN thr:=5000
IF rargs[14]=TRUE THEN brthr:=5000
IF rargs[15]=TRUE THEN smooth:=0
IF rargs[16]=TRUE THEN os3:=TRUE
IF rargs[17]=TRUE THEN rompal:=TRUE
IF rargs[18]=TRUE THEN writeheader:=0
IF rargs[20]=TRUE THEN dither:=1
IF rargs[21]
    UpperStr(rargs[21])
    IF StrCmp(rargs[21],'RANDOM') THEN dtype:=1
ENDIF

IF rargs[24]=TRUE THEN quiet:=TRUE
FreeArgs(rdargs)

ELSE
  Raise(ERR_ARGS)
ENDIF

IF quiet=0
  WriteF('PDHFIC 3.2\nby Chris Young <chris@unsatisfactorysoftware.co.uk>\n© 1998-,2008 Unsatisfactory Software\n\n')
IF os3<>FALSE THEN WriteF('You are set to run in OS3 mode...\n\n')
ENDIF

os3setup()
joemackay()
ppmtoscr()


ELSE  /* started from wb */

    IF (filebuf:=AllocMem(1024, MEMF_CLEAR))=NIL THEN Raise(ERR_MEM)


    IF (aslbase:=OpenLibrary('asl.library',37))=0 THEN Raise(ERR_ASL)
    IF (iconbase:=OpenLibrary('icon.library',33))=NIL THEN Raise(ERR_ICON)
    IF (gadtoolsbase:=OpenLibrary('gadtools.library',37))=NIL THEN Raise(ERR_GTLS)
    IF (workbenchbase:=OpenLibrary('workbench.library',37))=NIL THEN Raise(ERR_WBLB)
->    alsbase:=OpenLibrary('als.library',6)

arexxreq:=AllocAslRequest(ASL_FILEREQUEST,
            [ASLFR_TITLETEXT,'Execute ARexx script...',
            ASLFR_REJECTICONS,TRUE,
            ASLFR_INITIALDRAWER,'REXX:',
            ASLFR_DOPATTERNS,TRUE,
            ASLFR_INITIALPATTERN,'#?.rexx',
            NIL])

    readtooltypes()
    os3setup()
    openwin()


/*
libver:=alsbase

WriteF('\d.\d \s\n',libver.version,libver.revision,libver.idstring)
*/

ENDIF


->finish:


EXCEPT DO

    IF exception
->      IF quiet=0 THEN WriteF('*** ERROR ***\n')
    
    SELECT exception
    CASE ERR_FILE ; StrCopy(endmsg,'Couldn\'t open file!')
    CASE ERR_DTYP ; StrCopy(endmsg,'Datatypes error!')
    CASE ERR_NPIC ; StrCopy(endmsg,filetypeerror)
    CASE ERR_MEM  ; StrCopy(endmsg,'Not enough memory!')
    CASE ERR_BTMP ; StrCopy(endmsg,'Could not allocate BitMap!')
    CASE ERR_LIB  ; StrCopy(endmsg,'Could not open datatypes.library 39+')
     CASE ERR_ABOR ; StrCopy(endmsg,'Aborted by user!')
     CASE ERR_ASL  ; StrCopy(endmsg,'Could not open asl.library 37+')
    CASE ERR_ICON ; StrCopy(endmsg,'Could not open icon.library 33+')
    CASE ERR_ICNN ; StrCopy(endmsg,'Could not open icon.library 36+')
    CASE ERR_GTLS ; StrCopy(endmsg,'Could not open gadtools.library 37+')
    CASE ERR_ARGS ; StrCopy(endmsg,'required argument missing')
    CASE ERR_WBLB ; StrCopy(endmsg,'Could not open workbench.library 37+')
    CASE "DOUB"
        handle_gadgets(1)
        StrCopy(endmsg,'AREXX')
    CASE "SIG"
        handle_gadgets(1)
        StrCopy(endmsg,'AREXX')
    CASE "MEM"    ; StrCopy(endmsg,'Not enough memory!')
	DEFAULT
		StrCopy(endmsg,'Exception occured - Unknown error\n')
    ENDSELECT
    
    IF StrCmp(endmsg,'AREXX')=0
         IF wbmessage=NIL
           IF quiet=0 THEN WriteF('\s\n',endmsg)
         ELSE
           EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC',endmsg,'OK']:easystruct,0,NIL)
         ENDIF

    ENDIF
ENDIF


IF arexxreq THEN FreeAslRequest(arexxreq)
    IF filereq THEN FreeAslRequest(filereq)
    IF savereq THEN FreeAslRequest(savereq)
    IF o THEN DisposeDTObject(o)
    IF dbm THEN FreeBitMap(dbm)       -> AFAIK, bm is freed by picture.datatype
    IF rbm THEN FreeBitMap(rbm)       -> AFAIK, bm is freed by picture.datatype
    IF deetee THEN ReleaseDataType(deetee)
    IF lock   THEN UnLock(lock)
lock:=NIL

IF cm THEN FreeColorMap(cm)
IF arexxport THEN rx_ClosePort(arexxport)
IF fileid THEN FreeMem(fileid,11)
->IF ppmmem THEN FreeMem(ppmmem, 147456)
IF filebuf THEN FreeMem(filebuf,1024)
IF flashcol<>-1 THEN ReleasePen(cmap,flashcol)

IF appwinport
    WHILE appwinmsg:=GetMsg(appwinport) DO ReplyMsg(appwinmsg)
    DeleteMsgPort(appwinport)
ENDIF

IF appwin THEN RemoveAppWindow(appwin)

IF wptr THEN ClearMenuStrip(wptr)

IF wptr THEN CloseWindow(wptr)

-> RemoveGList(wptr,gads[ADV_BRIGHT],-1)
/*
IF glist
   FreeGadgets(glist)
   FreeGadgets(advglist)
   RemoveGList(wptr,gads[GADG_LOAD],-1)
ENDIF
*/
-> 



-> IF menustrip THEN FreeMenus(menustrip)
IF vi THEN FreeVisualInfo(vi)
IF mysc THEN UnlockPubScreen(NIL,mysc)

IF workbenchbase THEN CloseLibrary(workbenchbase)
IF gadtoolsbase THEN CloseLibrary(gadtoolsbase)
IF rexxsysbase THEN CloseLibrary(rexxsysbase)
IF datatypesbase THEN CloseLibrary(datatypesbase)
IF iconbase THEN CloseLibrary(iconbase)
IF aslbase THEN CloseLibrary(aslbase)
->IF alsbase THEN CloseLibrary(alsbase)

-> CleanUp()

ENDPROC

PROC ppmtoscr()
DEF o=50,p=0,tzxstrs,tzxlens,specname2[256]:STRING

      StrCopy(wintit,'Writing file header...')
      IF wbmessage<>NIL THEN SetWindowTitles(curwin,wintit,-1)
IF quiet=0 THEN WriteF('\nOutput File: \s\n',scrfile)

StrCopy(specname2,scrfile)
specname:=FilePart(specname2)
dot:=InStr(specname,'.')
IF dot<>-1 THEN StrCopy(specname,specname,dot)
StrAdd(specname,'          ')

IF gifsave
    StrCopy(gifname,scrfile)
->    StrCopy(scrfile,'T:PDHFIC.tmp')
    dot:=InStr(scrfile,'.')
    IF dot=NIL THEN StrCopy(scrfile,gifname) ELSE StrCopy(scrfile,gifname,dot)
    StrAdd(scrfile,'.scr')
            IF lock:=Lock(scrfile,ACCESS_READ)
                UnLock(lock)
                lock:=NIL
                IF EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','PDHFIC wants to write a\ntemporary file, but it\nalready exists!','Overwrite|Cancel']:easystruct,0,NIL)=0
                    StrCopy(scrfile,gifname)
                    RETURN -1
                ENDIF
            ENDIF
ENDIF

IF saveformat=0
 IF gifsave
  IF quiet=0 THEN WriteF('Save Format: GIF\n')
 ELSE
  IF quiet=0 THEN WriteF('Save Format: Raw SCR\n')
 ENDIF
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

IF saveformat=4
  IF quiet=0 THEN WriteF('Save Format: TZX (ZX Tape)\n')
ENDIF

IF saveformat=5
  IF quiet=0 THEN WriteF('Save Format: +3DOS\n')
ENDIF

headcheck:=0

headerbytes[0]:=$13  /* .tap only */
headerbytes[1]:=$00  /* .tap only */
headerbytes[2]:=$00
headerbytes[3]:=$03

-> headcheck:=$F8
headerbytes[4]:=specname[0]
headerbytes[5]:=specname[1]
headerbytes[6]:=specname[2]
headerbytes[7]:=specname[3]
headerbytes[8]:=specname[4]
headerbytes[9]:=specname[5]
headerbytes[10]:=specname[6]
headerbytes[11]:=specname[7]
headerbytes[12]:=specname[8]
headerbytes[13]:=specname[9]

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

headerbytes[14]:=$00 /* length vv*/
IF saveattr THEN headerbytes[15]:=$1B ELSE headerbytes[15]:=$18
headerbytes[16]:=$00
headerbytes[17]:=$40
headerbytes[18]:=$20
headerbytes[19]:=$80
/* headerbytes[20]:=$EC *//* eor of values 2 to 19 */
headerbytes[21]:=$02 /* .tap only */
IF saveattr THEN headerbytes[22]:=$1B ELSE headerbytes[22]:=$18 /* .tap only */

IF saveformat=2
 IF writeheader
    IF lock:=Lock(headerfile,ACCESS_READ)
        UnLock(lock)
        lock:=NIL
        IF EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','Warning!\n\nA .header for this\nfile already exists!','Overwrite|Cancel']:easystruct,0,NIL)
          IF fhhead:=Open(headerfile,MODE_NEWFILE)
             IF quiet=0 THEN WriteF('Writing \s file...\n',headerfile)
             FOR h:=2 TO 19 /* 20 */
                 headcheck:=Eor(headcheck,headerbytes[h])
                 FputC(fhhead,headerbytes[h])
             ENDFOR
             FputC(fhhead,headcheck)
             Close(fhhead)
          ENDIF
         ENDIF
    ENDIF
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

IF saveformat=5
  IF quiet=0 THEN WriteF('Writing +3DOS header...\n')
    Fputs(fhand,'PLUS3DOS') /* checksum $71 */
    FputC(fhand,26) 
    FputC(fhand,01)
    FputC(fhand,00)

    headcheck:=$6A

/*    IF saveattr THEN */
    FputC(fhand,$80)
    headcheck:=Eor(headcheck,$80)

    FputC(fhand,headerbytes[15]) /* $1B) ELSE FputC(fhand,$18) */
    headcheck:=Eor(headcheck,headerbytes[15])

    /* write header sans filename */
    -> FOR h:=2 TO 3   [2] is part of the tap format, so not required
        FputC(fhand,headerbytes[3])
        headcheck:=Eor(headcheck,headerbytes[3])
    -> ENDFOR
    FOR h:=14 TO 19
        FputC(fhand,headerbytes[h])
        headcheck:=Eor(headcheck,headerbytes[h])
    ENDFOR
    /* apparently this is supposed to be seven bytes but it, erm, isn't */
    FputC(fhand,0) /* this is separate from below as it is part of the header field */
        headcheck:=Eor(headcheck,0)
    FOR h:=1 TO 104
        FputC(fhand,0)
        headcheck:=Eor(headcheck,0) /* I'm not sure there is much point to this line */
    ENDFOR
    FputC(fhand,headcheck)

ENDIF

IF saveformat=4
  IF quiet=0 THEN WriteF('Writing TZX header...\n')
    Fputs(fhand,'ZXTape!')
    FputC(fhand,$1A)
    FputC(fhand,01)
    FputC(fhand,12)

IF tzxarray[0]

    /* archive info */
    
->    objname:=FilePart(filename)
    
    StrCopy(tmpstr,specname,EstrLen(specname)-10)
    
    tzxstrs:=2 -> one block always available
    tzxlens:=58+EstrLen(tmpstr) -> length of promo text + 1 + namelength

    IF EstrLen(objcopy)<>NIL
        tzxstrs:=tzxstrs+1
        tzxlens:=tzxlens+EstrLen(objcopy)
    ENDIF

    IF EstrLen(objauth)<>NIL
        tzxstrs:=tzxstrs+1
        tzxlens:=tzxlens+EstrLen(objauth)
    ENDIF
    
    IF EstrLen(objanno)<>NIL
        -> tzxstrs:=tzxstrs+1 (already included)
        tzxlens:=tzxlens+EstrLen(objanno)+1
    ENDIF


    tzxlens:=tzxlens+(2*tzxstrs)

/*
    WriteF('total length: \d\n',tzxlens)
    WriteF('no of strings: \d\n',tzxstrs)
*/    
    -> no room for objver in tzx 1.11
    
    FputC(fhand,$32)
    FputC(fhand,tzxlens) /* length 2 bytes (I hope it won't go over one)*/
    FputC(fhand,00) /* MSB */
    FputC(fhand,tzxstrs) -> Number of text strings
    
    FputC(fhand,$00)
    FputC(fhand,EstrLen(tmpstr)) -> length of text below
    Fputs(fhand,tmpstr)

    IF EstrLen(objcopy)<>NIL
        FputC(fhand,$01)
        FputC(fhand,EstrLen(objcopy)) -> length of text below
        Fputs(fhand,objcopy)
    ENDIF

    IF EstrLen(objauth)<>NIL
        FputC(fhand,$02)
        FputC(fhand,EstrLen(objauth)) -> length of text below
        Fputs(fhand,objauth)
    ENDIF

-> Let's get the blocks in the right order, eh chaps?

    FputC(fhand,$08)
    FputC(fhand,57) /* length of text below */
    Fputs(fhand,'Converted with PDHFIC v3.2')
    FputC(fhand,13)
    Fputs(fhand,'unsatisfactory.freeserve.co.uk')

-> end of obligatory advert for PDHFIC

    IF EstrLen(objanno)<>NIL
        FputC(fhand,$FF)
        FputC(fhand,(EstrLen(objanno))) -> length of text below
        Fputs(fhand,objanno)
    ENDIF
    


ENDIF -> End Archive Info

IF tzxarray[1]

    /*** start of ID/data etc ***/
    FputC(fhand,$10)
    FputC(fhand,00) /* pause after block (2 bytes) */
    FputC(fhand,10)
    
    FOR h:=0 TO 1
         FputC(fhand,headerbytes[h])
     ENDFOR
     FOR h:=2 TO 19
         FputC(fhand,headerbytes[h])
         headcheck:=Eor(headcheck,headerbytes[h])
     ENDFOR
     FputC(fhand,headcheck)
     
        FputC(fhand,$10)
    FputC(fhand,00) /* pause */
    FputC(fhand,10)

     FOR h:=21 TO 22
         FputC(fhand,headerbytes[h])
     ENDFOR

ENDIF

    IF tzxarray[2] -> Custom SCREEN$ block
        FputC(fhand,$35)
        Fputs(fhand,'Spectrum Screen ')
        FputC(fhand,$1B) -> 20
        FputC(fhand,$20) -> 1B
        FputC(fhand,$00)
        FputC(fhand,$00)
    IF EstrLen(objanno)<>NIL
        FputC(fhand,(EstrLen(objanno))) -> length of text below
        Fputs(fhand,objanno)
    ELSE
        FputC(fhand,30) -> text below
        Fputs(fhand,'Screen created with PDHFIC 3.2')
    ENDIF
        FputC(fhand,tzxborder) -> Border
    ENDIF

ENDIF

IF saveformat=1
  IF quiet=0 THEN WriteF('Writing ZX82 header...\n')
zx82header[0]:=$5A
zx82header[1]:=$58
zx82header[2]:=$38
zx82header[3]:=$32
zx82header[4]:=$03
zx82header[5]:=$00
IF saveattr THEN zx82header[6]:=$1B ELSE zx82header[6]:=$18
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

      StrCopy(wintit,'Mapping colours/writing image...')
      IF wbmessage<>NIL THEN SetWindowTitles(curwin,wintit,-1)
IF quiet=0 THEN WriteF('\s\n',wintit)

/* temp stuff *********************
Fputs(fhand,header)
Fputs(fhand,data)
Flush(fhand)
Close(fhand)
JUMP finish
********************************/

setprogresswin(50)

/*
do i=1 to 147456   /* 71 */

red.i=substr(data,i,1)
grn.i=readch(data,i,1)
blu.i=readch(data,i,1)

end

*/

/* why was this commented out???! */
FOR z:=1 TO 768
ink[z]:=7
paper[z]:=0
brightattr[z]:=0
ENDFOR


o:=50

 /* 16 */

/* a:=15 */ /*-172*/
a:=0
x:=0

FOR l:=1 TO 3

FOR k:=1 TO 8

/* rows */


/*********** PROGRESS BAR??????? *******************/

 o:=o+2

setprogresswin(o)

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

IF saveattr
      StrCopy(wintit,'Writing colour attributes...')
      IF quiet=0 THEN WriteF('\s\n',wintit)
      IF wbmessage<>NIL THEN SetWindowTitles(curwin,wintit,-1)

    o:=97
    p:=0

    FOR m:=1 TO 768

        /* check attr() command! */

        /* ink=0
        paper=7 */

    attr:=(1*ink[m])+(8*paper[m])+brightattr[m]+flash[m]

        -> col=d2c(attr)

        FputC(fhand,attr)   /*writech('scr',col)*/

        checksum:=Eor(checksum,attr)

        IF pwptr
            p:=p+1
            IF p>=200
                o:=o+1
                p:=0
                setprogresswin(o)
            ENDIF
        ENDIF

    ENDFOR
ELSE
    IF saveformat=0

      StrCopy(wintit,'Writing colour attributes...')
      IF quiet=0 THEN WriteF('\s\n',wintit)
      IF wbmessage<>NIL THEN SetWindowTitles(curwin,wintit,-1)

    o:=97
    p:=0

        FOR m:=1 TO 768

            /* ink=0
            paper=7 */

            attr:=56
            FputC(fhand,attr)   /*writech('scr',col)*/

            checksum:=Eor(checksum,attr)

            IF pwptr
                p:=p+1
                IF p>=200
                    o:=o+1
                    p:=0
                    setprogresswin(o)
                ENDIF
            ENDIF

        ENDFOR
    ENDIF
ENDIF

IF pwptr
    o:=o+5
    setprogresswin(o)
ENDIF

IF saveformat>1
    IF saveformat=4
        IF tzxarray[2]=0 THEN FputC(fhand,checksum)
    ELSE
        FputC(fhand,checksum)
    ENDIF
ENDIF

Close(fhand)


ENDIF

IF gifsave
      StrCopy(wintit,'Converting to GIF...')
      IF wbmessage<>NIL THEN SetWindowTitles(curwin,wintit,-1)
      IF quiet=0 THEN WriteF('\s\n',wintit)
    StrCopy(tmpstr,'SCR2GIF ')
    IF quiet=1 THEN StrAdd(tmpstr,'>NIL: ')
    IF flashpix>0 THEN StrAdd(tmpstr,'-f ')
    StrAdd(tmpstr,'"')
    StrAdd(tmpstr,scrfile)
    StrAdd(tmpstr,'"')
    Execute(tmpstr,NIL,NIL)
    DeleteFile(scrfile)
    StrCopy(scrfile,gifname)
ENDIF

IF flashpix>0
    StrAdd(filetype,'\nFLASH pixels: ')
    StrAdd(filetype,RealF(tmpstr,flashpix!!FLOAT,0))
ENDIF

      StrCopy(wintit,'PDHFIC')
      IF wbmessage<>NIL THEN SetWindowTitles(curwin,wintit,-1)

IF quiet=0 THEN WriteF('Conversion done - all OK!\n')

ENDPROC 0


PROC mapcol(offset,binary)
DEF tempbin
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

            IF acontrast
                    red:=((red-lowval)*255)/range
                    grn:=((grn-lowval)*255)/range
                    blu:=((blu-lowval)*255)/range
            ENDIF

->  col,bright:=choosecol(red,grn,blu)

            IF remapafterdither
                SELECT z
                CASE 1
                    tempbin:=%10000000
                CASE 2
                    tempbin:=%01000000
                CASE 3
                    tempbin:=%00100000
                CASE 4
                    tempbin:=%00010000
                CASE 5
                    tempbin:=%00001000
                CASE 6
                    tempbin:=%00000100
                CASE 7
                    tempbin:=%00000010
                CASE 8
                    tempbin:=%00000001
                ENDSELECT

                ditherchoosecol(y,tempbin)
            ELSE
                choosecol()
            ENDIF
  
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

    IF paper[blk]<ink[blk]
        red:=paper[blk]
        paper[blk]:=ink[blk]
        ink[blk]:=red
    ENDIF

    availpix:=(64-best[0])/2
->    availpix:=(64-best[0])/3
-> WriteF('\d/\d\n',best[8],availpix)

    IF best[8]>availpix THEN brightattr[blk]:=64 /* ELSE brightattr[blk]:=0 */

ENDIF

red:=ppmmem[a+offset]
grn:=ppmmem[a+offset+1]
blu:=ppmmem[a+offset+2]

IF acontrast
    red:=((red-lowval)*255)/range
    grn:=((grn-lowval)*255)/range
    blu:=((blu-lowval)*255)/range
ENDIF

-> col:=choosecol(red,grn,blu)

IF dither
    ditherchoosecol(k,binary) -> k*l   j
ELSE
    choosecol()
ENDIF

IF flashred=red
  IF flashgrn=grn
    IF flashblu=blu
      flash[blk]:=128
      flashpix:=flashpix+1
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
  IF allelseink
      bin:=bin+binary
  ELSE
      IF ink[blk]=col THEN bin:=bin+binary
  ENDIF
  ENDIF

ENDIF

ENDPROC col

PROC randomdither()
DEF random=0

random:=Rnd(255)
IF random>=red THEN red:=0
IF random>=grn THEN grn:=0
IF random>=blu THEN blu:=0

ditherremap()
ENDPROC

PROC ditherchoosecol(line,binary)
DEF eline,dull=128,dred,dblu,dgrn

IF dtype=1
    randomdither()
    RETURN
ENDIF

IF line THEN eline:=Even(line)

-> WriteF('\d \d',binary,eline)

-> WriteF('\d,\d,\d = \d\n',red,grn,blu,col)

/* Based on LCD's code */

/******* dithering patterns ******
1



1
  X   X   X   X



  X   X   X   X

2
  X X X X X X X

  X X X X X X X
  
3
  X X X X X X X
   X X X X X X
  X X X X X X X

4-6 are opposite of 3-1

other options:

2.5
X   X   X   X
  X   X   X
X   X   X   X

1.5
X

opposite of 4?

****/

/* dither 6 was after 3 */

dred:=0
IF (red>0) AND (red<64)
    red:=0
ENDIF
IF (red>=64) AND (red<74)
    dred:=8
ENDIF
IF (red>=74) AND (red<84)
    dred:=1
ENDIF
IF (red>=84) AND (red<94)
    dred:=9
ENDIF
IF (red>=94) AND (red<104)
    dred:=2
ENDIF
IF (red>=104) AND (red<114)
    dred:=11
ENDIF
IF (red>=114) AND (red<=142)
    dred:=3
ENDIF
IF (red>=142) AND (red<152)
    dred:=12
ENDIF
IF (red>152) AND (red<=162)
    dred:=4
ENDIF
IF (red>162) AND (red<=172)
    dred:=10
ENDIF
IF (red>172) AND (red<=182)
    dred:=5
ENDIF
IF (red>182) AND (red<192)
    dred:=7
ENDIF

dgrn:=0
IF (grn>0) AND (grn<64)
    grn:=0
ENDIF
IF (grn>=64) AND (grn<74)
    dgrn:=8
ENDIF
IF (grn>=74) AND (grn<84)
    dgrn:=1
ENDIF
IF (grn>=84) AND (grn<94)
    dgrn:=9
ENDIF
IF (grn>=94) AND (grn<104)
    dgrn:=2
ENDIF
IF (grn>=104) AND (grn<114)
    dgrn:=11
ENDIF
IF (grn>=114) AND (grn<=142)
    dgrn:=3
ENDIF

IF (grn>=142) AND (grn<152)
    dgrn:=12
ENDIF
IF (grn>152) AND (grn<=162)
    dgrn:=4
ENDIF
IF (grn>162) AND (grn<=172)
    dgrn:=10
ENDIF
IF (grn>172) AND (grn<=182)
    dgrn:=5
ENDIF
IF (grn>182) AND (grn<192)
    dgrn:=7
ENDIF

dblu:=0
IF (blu>0) AND (blu<64)
    blu:=0
ENDIF
IF (blu>=64) AND (blu<74)
    dblu:=8
ENDIF
IF (blu>=74) AND (blu<84)
    dblu:=1
ENDIF
IF (blu>=84) AND (blu<94)
    dblu:=9
ENDIF
IF (blu>=94) AND (blu<104)
    dblu:=2
ENDIF
IF (blu>=104) AND (blu<114)
    dblu:=11
ENDIF
IF (blu>=114) AND (blu<=142)
    dblu:=3
ENDIF

IF (blu>=142) AND (blu<152)
    dblu:=12
ENDIF
IF (blu>152) AND (blu<=162)
    dblu:=4
ENDIF
IF (blu>162) AND (blu<=172)
    dblu:=10
ENDIF
IF (blu>172) AND (blu<=182)
    dblu:=5
ENDIF
IF (blu>182) AND (blu<192)
    dblu:=7
ENDIF

-> WriteF('\d,\d,\d - \d\n',red,grn,blu,eline)

SELECT binary
CASE %10000000
    IF eline
        IF dred=3 THEN red:=0
        IF dgrn=3 THEN grn:=0
        IF dblu=3 THEN blu:=0
        IF dred=4 THEN red:=0
        IF dgrn=4 THEN grn:=0
        IF dblu=4 THEN blu:=0
        IF dred=11 THEN red:=0
        IF dgrn=11 THEN grn:=0
        IF dblu=11 THEN blu:=0
    ELSE
        IF dred=12 THEN red:=0
        IF dgrn=12 THEN grn:=0
        IF dblu=12 THEN blu:=0
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
        IF dred=6 THEN red:=0
        IF dgrn=6 THEN grn:=0
        IF dblu=6 THEN blu:=0
    ENDIF
    IF (line=1) OR (line=5)
        IF dred=5 THEN red:=0
        IF dgrn=5 THEN grn:=0
        IF dblu=5 THEN blu:=0
        IF dred=10 THEN red:=0
        IF dgrn=10 THEN grn:=0
        IF dblu=10 THEN blu:=0
    ELSE
        IF dred=8 THEN red:=0
        IF dgrn=8 THEN grn:=0
        IF dblu=8 THEN blu:=0
        IF dred=1 THEN red:=0
        IF dgrn=1 THEN grn:=0
        IF dblu=1 THEN blu:=0
        IF dred=9 THEN red:=0
        IF dgrn=9 THEN grn:=0
        IF dblu=9 THEN blu:=0
    ENDIF

CASE %01000000
    IF eline
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
        IF dred=6 THEN red:=0
        IF dgrn=6 THEN grn:=0
        IF dblu=6 THEN blu:=0
    ELSE
        IF dred=3 THEN red:=0
        IF dgrn=3 THEN grn:=0
        IF dblu=3 THEN blu:=0
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
        IF dred=11 THEN red:=0
        IF dgrn=11 THEN grn:=0
        IF dblu=11 THEN blu:=0
    ENDIF
        IF dred=8 THEN red:=0
        IF dgrn=8 THEN grn:=0
        IF dblu=8 THEN blu:=0
        IF dred=1 THEN red:=0
        IF dgrn=1 THEN grn:=0
        IF dblu=1 THEN blu:=0
        IF dred=9 THEN red:=0
        IF dgrn=9 THEN grn:=0
        IF dblu=9 THEN blu:=0
    IF (line=4)
        IF dred=11 THEN red:=0
        IF dgrn=11 THEN grn:=0
        IF dblu=11 THEN blu:=0
    ENDIF
    IF (line=2) OR (line=6)
        IF dred=12 THEN red:=0
        IF dgrn=12 THEN grn:=0
        IF dblu=12 THEN blu:=0
    ENDIF

CASE %00100000
    IF eline
        IF dred=4 THEN red:=0
        IF dgrn=4 THEN grn:=0
        IF dblu=4 THEN blu:=0
        IF dred=3 THEN red:=0
        IF dgrn=3 THEN grn:=0
        IF dblu=3 THEN blu:=0
        IF dred=11 THEN red:=0
        IF dgrn=11 THEN grn:=0
        IF dblu=11 THEN blu:=0
    ELSE
        IF dred=12 THEN red:=0
        IF dgrn=12 THEN grn:=0
        IF dblu=12 THEN blu:=0
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
        IF dred=6 THEN red:=0
        IF dgrn=6 THEN grn:=0
        IF dblu=6 THEN blu:=0
    ENDIF
    IF (line=3) OR (line=7)
        IF dred=10 THEN red:=0
        IF dgrn=10 THEN grn:=0
        IF dblu=10 THEN blu:=0
    ELSE
        IF dred=9 THEN red:=0
        IF dgrn=9 THEN grn:=0
        IF dblu=9 THEN blu:=0
    ENDIF
        IF dred=1 THEN red:=0
        IF dgrn=1 THEN grn:=0
        IF dblu=1 THEN blu:=0
        IF dred=8 THEN red:=0
        IF dgrn=8 THEN grn:=0
        IF dblu=8 THEN blu:=0
CASE %00010000
    IF eline
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
        IF dred=6 THEN red:=0
        IF dgrn=6 THEN grn:=0
        IF dblu=6 THEN blu:=0
    ELSE
        IF dred=3 THEN red:=0
        IF dgrn=3 THEN grn:=0
        IF dblu=3 THEN blu:=0
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
    ENDIF
        IF dred=8 THEN red:=0
        IF dgrn=8 THEN grn:=0
        IF dblu=8 THEN blu:=0
        IF dred=1 THEN red:=0
        IF dgrn=1 THEN grn:=0
        IF dblu=1 THEN blu:=0
        IF dred=9 THEN red:=0
        IF dgrn=9 THEN grn:=0
        IF dblu=9 THEN blu:=0
        IF dred=11 THEN red:=0
        IF dgrn=11 THEN grn:=0
        IF dblu=11 THEN blu:=0
CASE %00001000
    IF eline
        IF dred=3 THEN red:=0
        IF dgrn=3 THEN grn:=0
        IF dblu=3 THEN blu:=0
        IF dred=4 THEN red:=0
        IF dgrn=4 THEN grn:=0
        IF dblu=4 THEN blu:=0
        IF dred=11 THEN red:=0
        IF dgrn=11 THEN grn:=0
        IF dblu=11 THEN blu:=0
    ELSE
        IF dred=12 THEN red:=0
        IF dgrn=12 THEN grn:=0
        IF dblu=12 THEN blu:=0
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
        IF dred=1 THEN red:=0
        IF dgrn=1 THEN grn:=0
        IF dblu=1 THEN blu:=0
        IF dred=6 THEN red:=0
        IF dgrn=6 THEN grn:=0
        IF dblu=6 THEN blu:=0
    ENDIF
    IF (line=1) OR (line=5)
        IF dred=5 THEN red:=0
        IF dgrn=5 THEN grn:=0
        IF dblu=5 THEN blu:=0
        IF dred=10 THEN red:=0
        IF dgrn=10 THEN grn:=0
        IF dblu=10 THEN blu:=0
    ELSE
        IF dred=1 THEN red:=0
        IF dgrn=1 THEN grn:=0
        IF dblu=1 THEN blu:=0
        IF dred=9 THEN red:=0
        IF dgrn=9 THEN grn:=0
        IF dblu=9 THEN blu:=0
    ENDIF
    IF line=5
        IF dred=7 THEN red:=0
        IF dgrn=7 THEN grn:=0
        IF dblu=7 THEN blu:=0
    ELSE
        IF dred=8 THEN red:=0
        IF dgrn=8 THEN grn:=0
        IF dblu=8 THEN blu:=0
    ENDIF

CASE %00000100
    IF eline
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
        IF dred=6 THEN red:=0
        IF dgrn=6 THEN grn:=0
        IF dblu=6 THEN blu:=0
    ELSE
        IF dred=3 THEN red:=0
        IF dgrn=3 THEN grn:=0
        IF dblu=3 THEN blu:=0
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
        IF dred=11 THEN red:=0
        IF dgrn=11 THEN grn:=0
        IF dblu=11 THEN blu:=0
    ENDIF
    IF (line=4)
        IF dred=11 THEN red:=0
        IF dgrn=11 THEN grn:=0
        IF dblu=11 THEN blu:=0
    ENDIF
    IF (line=2) OR (line=6)
        IF dred=12 THEN red:=0
        IF dgrn=12 THEN grn:=0
        IF dblu=12 THEN blu:=0
    ENDIF

        IF dred=1 THEN red:=0
        IF dgrn=1 THEN grn:=0
        IF dblu=1 THEN blu:=0
        IF dred=9 THEN red:=0
        IF dgrn=9 THEN grn:=0
        IF dblu=9 THEN blu:=0
        IF dred=8 THEN red:=0
        IF dgrn=8 THEN grn:=0
        IF dblu=8 THEN blu:=0

CASE %00000010
    IF eline
        IF dred=3 THEN red:=0
        IF dgrn=3 THEN grn:=0
        IF dblu=3 THEN blu:=0
        IF dred=4 THEN red:=0
        IF dgrn=4 THEN grn:=0
        IF dblu=4 THEN blu:=0
        IF dred=11 THEN red:=0
        IF dgrn=11 THEN grn:=0
        IF dblu=11 THEN blu:=0
    ELSE
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
        IF dred=6 THEN red:=0
        IF dgrn=6 THEN grn:=0
        IF dblu=6 THEN blu:=0
        IF dred=12 THEN red:=0
        IF dgrn=12 THEN grn:=0
        IF dblu=12 THEN blu:=0
    ENDIF
    IF (line=3) OR (line=7)
        IF dred=10 THEN red:=0
        IF dgrn=10 THEN grn:=0
        IF dblu=10 THEN blu:=0
    ELSE
        IF dred=9 THEN red:=0
        IF dgrn=9 THEN grn:=0
        IF dblu=9 THEN blu:=0
    ENDIF
        IF dred=1 THEN red:=0
        IF dgrn=1 THEN grn:=0
        IF dblu=1 THEN blu:=0
        IF dred=8 THEN red:=0
        IF dgrn=8 THEN grn:=0
        IF dblu=8 THEN blu:=0
CASE %00000001
    IF eline
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
        IF dred=6 THEN red:=0
        IF dgrn=6 THEN grn:=0
        IF dblu=6 THEN blu:=0
    ELSE
        IF dred=3 THEN red:=0
        IF dgrn=3 THEN grn:=0
        IF dblu=3 THEN blu:=0
        IF dred=2 THEN red:=0
        IF dgrn=2 THEN grn:=0
        IF dblu=2 THEN blu:=0
    ENDIF
        IF dred=1 THEN red:=0
        IF dgrn=1 THEN grn:=0
        IF dblu=1 THEN blu:=0
        IF dred=9 THEN red:=0
        IF dgrn=9 THEN grn:=0
        IF dblu=9 THEN blu:=0
        IF dred=8 THEN red:=0
        IF dgrn=8 THEN grn:=0
        IF dblu=8 THEN blu:=0
        IF dred=11 THEN red:=0
        IF dgrn=11 THEN grn:=0
        IF dblu=11 THEN blu:=0
/*
    IF line=0
        IF eline=0 THEN eline:=1 ELSE eline:=0
    ENDIF
*/
ENDSELECT

-> WriteF('\d,\d,\d = \d\n',red,grn,blu,col)

ditherremap()
ENDPROC

PROC ditherremap()
IF simpleremap=0
    choosecol()
ELSE
/* simplemap *****/
IF (red=0) AND (grn=0) AND (blu=0) THEN col:=0
IF (red=0) AND (grn=0) AND (blu)   THEN col:=1
IF (red)   AND (grn=0) AND (blu=0) THEN col:=2
IF (red)   AND (grn=0) AND (blu)   THEN col:=3
IF (red=0) AND (grn)   AND (blu=0) THEN col:=4
IF (red=0) AND (grn)   AND (blu)   THEN col:=5
IF (red)   AND (grn)   AND (blu=0) THEN col:=6
IF (red)   AND (grn)   AND (blu)   THEN col:=7
ENDIF

/* by this time, bright is already done...
bright:=0
IF red>128 THEN bright:=1
IF grn>128 THEN bright:=1
IF blu>128 THEN bright:=1
*/

->WriteF('\d,\d,\d = \d\n',red,grn,blu,col)
ENDPROC

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
IF cm=NIL THEN cm:=GetColorMap(15)

IF rompal=0 /* ZXDT */
  SetRGB32CM(cm,15,$00000000,$00000000,$00000000) /* spec colour 0 (black) */
  SetRGB32CM(cm,0,$00000000,$00000000,$00000000) /* spec colour 0 (black) */
  SetRGB32CM(cm,1,$00000000,$00000000,Shl(153,24))   /* 153 */
  SetRGB32CM(cm,2,Shl(153,24),$00000000,$00000000)
  SetRGB32CM(cm,3,Shl(153,24),$00000000,Shl(153,24))
  SetRGB32CM(cm,4,$00000000,Shl(153,24),$00000000)
  SetRGB32CM(cm,5,$00000000,Shl(153,24),Shl(153,24))
  SetRGB32CM(cm,6,Shl(153,24),Shl(153,24),$00000000)
  SetRGB32CM(cm,7,Shl(153,24),Shl(153,24),Shl(153,24)) /* WHITE */
  SetRGB32CM(cm,8,$00000000,$00000000,Shl(170,24)) /* 170 Start of BRIGHT */
  SetRGB32CM(cm,9,Shl(187,24),$00000000,$00000000) /* 187 */
  SetRGB32CM(cm,10,Shl(204,24),$00000000,Shl(204,24)) /* 204 */
  SetRGB32CM(cm,11,$00000000,Shl(204,24),$00000000)
  SetRGB32CM(cm,12,$00000000,$DDDDDDDD,$DDDDDDDD) /* 221 */
  SetRGB32CM(cm,13,Shl(238,24),Shl(238,24),$00000000) /*238 */
  SetRGB32CM(cm,14,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF) /* 255 */

ELSE

  SetRGB32CM(cm,15,$00000000,$00000000,$00000000)
  SetRGB32CM(cm,0,$00000000,$00000000,$00000000) /* spec colour 0 (black) */
  SetRGB32CM(cm,8,$00000000,$00000000,$FFFFFFFF)
  SetRGB32CM(cm,9,$FFFFFFFF,$00000000,$00000000)
  SetRGB32CM(cm,10,$FFFFFFFF,$00000000,$FFFFFFFF)
  SetRGB32CM(cm,11,$00000000,$FFFFFFFF,$00000000)
  SetRGB32CM(cm,12,$00000000,$FFFFFFFF,$FFFFFFFF)
  SetRGB32CM(cm,13,$FFFFFFFF,$FFFFFFFF,$00000000)
  SetRGB32CM(cm,14,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF)
  SetRGB32CM(cm,1,$00000000,$00000000,$DDDDDDDD)
  SetRGB32CM(cm,2,$DDDDDDDD,$00000000,$00000000)
  SetRGB32CM(cm,3,$DDDDDDDD,$00000000,$DDDDDDDD)
  SetRGB32CM(cm,4,$00000000,$DDDDDDDD,$00000000)
  SetRGB32CM(cm,5,$00000000,$DDDDDDDD,$DDDDDDDD)
  SetRGB32CM(cm,6,$DDDDDDDD,$DDDDDDDD,$00000000)
  SetRGB32CM(cm,7,$DDDDDDDD,$DDDDDDDD,$DDDDDDDD)
ENDIF

ENDPROC

PROC choosecol()
DEF t

IF os3=FALSE
   stdchoosecol()
   RETURN 0
ENDIF

bright:=0
/*
red:=!red/255
grn:=!grn/255
blu:=!blu/255

red:=red!/255.0
grn:=grn!/255.0
blu:=blu!/255.0
*/

red:=Shl(red,24)
grn:=Shl(grn,24)
blu:=Shl(blu,24)


col:=FindColor(cm,red,grn,blu,15) -> -1 ??? 15


/*
t:=ObtainBestPenA(cm,red,grn,blu,[NIL])

col:=t

ReleasePen(t)
*/

IF col=15 THEN col:=0

-> IF col>7 THEN WriteF('red \h, grn \h, blu \h, col \d\n',red,grn,blu,col)

IF col>7
  bright:=1
  col:=col-7
ENDIF


ENDPROC 0

PROC joemackay()

    init()
->    StrCopy(filename, 'pictures:kingtut.l256') -> Example - change/delete as applicable
     
     IF clipboard=0 THEN identifyfile()
     
    dt2ppm(scale)
    
->    WriteF('\nRaw PPM RGB data at: $\h (for a split second)\nWhich is pretty useless now, unless you incorporate it into #?toSCR...\n', ppmmem)

ENDPROC

PROC init()


    IF quiet=0 THEN WriteF('Datatypes code, © Joe Mackay 1998\n\n')

ENDPROC

/* The actual routine...
   Usage: dt2ppm(scaleornot) - scaleornot means, erm, whether to scale
                               the image to 256x192 or not. Bigger pics
                               will be truncated.
   A pointer to memory containing raw PPM (RGB) data, which is always
   exactly 147456 bytes long, is placed in the variable ppmmem. If any
   errors occur, they are handled in main()'s exception handler above.
   Make sure you include    it and adapt it properly to suit the rest of
   the program. */

PROC identifyfile()
    DEF gid,finfo:fileinfoblock,fsize=NIL

    chg:=FALSE

	IF clipboard=0
	    IF (lock:=Lock(filename, ACCESS_READ))=0
    	   StrCopy(filetypeerror,'Unable to open file!')
	      	IF wbmessage=NIL
    	      IF quiet=0 THEN WriteF('\s\n',filetypeerror)
      		ELSE
          		EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC',filetypeerror,'OK']:easystruct,0,NIL)
      		ENDIF
    	ENDIF
     
    deetee:=ObtainDataTypeA(DTST_FILE, lock, NIL)
    
    gid:=deetee.header.groupid

    StrCopy(filetype,deetee.header.name)
     
    SELECT gid
    CASE GID_SYSTEM     ; StrAdd(filetype,' system file')
    CASE GID_TEXT       ; StrAdd(filetype,' text')
    CASE GID_DOCUMENT   ; StrAdd(filetype,' document')
    CASE GID_SOUND      ; StrAdd(filetype,' sample')
    CASE GID_INSTRUMENT ; StrAdd(filetype,' instrument')
    CASE GID_MUSIC      ; StrAdd(filetype,' music')
    CASE GID_PICTURE    ; StrAdd(filetype,' picture')
    CASE GID_ANIMATION  ; StrAdd(filetype,' animation')
    CASE GID_MOVIE      ; StrAdd(filetype,' movie')
    DEFAULT             ; StrAdd(filetype,' - unknown file type!')
    ENDSELECT
ELSE
	gid:=GID_PICTURE
ENDIF

ppm:=FALSE
        -> Check for PPM
            IF okay:=Examine(lock,finfo)
                IF quiet=0 THEN WriteF('Checking for PPM...\nSize: \d\n',finfo.size)
                IF finfo.size=147471 -> Correct size for 256x192
                    IF fhid:=OpenFromLock(lock)
                        lock:=NIL
                        okay:=Read(fhid,fileid,2)
                        Close(fhid)
                        IF StrCmp('P6',fileid,2)
                            ppm:=TRUE
                            gid:=GID_PICTURE
StrCopy(filetype,'PPM RawBits (P6)\nWidth: 256\nHeight: 192\nDepth: 24 bit (16777216 colours)')
StrCopy(objname,FilePart(filename))
StrCopy(objauth,'')
StrCopy(objanno,'')
StrCopy(objcopy,'')
StrCopy(objver,'')

                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            
     StrCopy(filetypeerror,'File: ')
     StrAdd(filetypeerror,filename)
     StrAdd(filetypeerror,'\nType: ')
     StrAdd(filetypeerror,filetype)
     StrAdd(filetypeerror,'\n\nThis cannot be converted!')

    -> Not DT or (valid) PPM

    IF quiet=0 THEN WriteF('File identified as \s\n',filetype)

    IF gid<>GID_PICTURE
          IF wbmessage=NIL
            IF quiet=0 THEN WriteF('\s\n',filetypeerror)
        ELSE
            EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC',filetypeerror,'OK']:easystruct,0,NIL)
        ENDIF
      StrCopy(filename,'')
     ENDIF
    -> Raise(ERR_NPIC)  -> Quit if not a picture

     IF deetee
         ReleaseDataType(deetee)
         deetee:=NIL
     ENDIF

     IF lock
         UnLock(lock)
         lock:=NIL
     ENDIF

IF gid=GID_PICTURE THEN RETURN 1

ENDPROC 0

PROC dt2ppm(scaleornot)
DEF allok=0,p=0,q=0,olddir=-1,tmpstr2[30]:STRING,specscreen:PTR TO screen

->    DEF gid
     DEF cols[1]:ARRAY OF VALUE, creg[1]:ARRAY OF LONG
    DEF layout:gplayout
    DEF n, bitwork, rgbs:ARRAY OF CHAR, i, j, k, dbmtype=0
->    DEF ppmfh


flashpix:=0

IF ppm
    IF fhid:=Open(filename,OLDFILE)

    IF quiet=0 THEN WriteF('Reading PPM file...\n')

    okay:=Read(fhid,ppmmem,15)  -> header - don't need this
    okay:=Read(fhid,ppmmem,147456)   /*147471 */

    IF okay
      IF quiet=0 THEN WriteF('Read data OK\n')
    ENDIF

    Close(fhid)
    ENDIF

    RETURN 1
ENDIF

        StrCopy(wintit,'Loading datatypes image...') -> 256x192
          IF wbmessage<>NIL THEN SetWindowTitles(curwin,wintit,-1)
        IF quiet=0 THEN WriteF('\s\n',wintit)

    /* Create DataTypes object */
       StrCopy(filetypeerror,'An unexplainable DataTypes error occured!\n\n(possibly the file wasn\'t actually a picture at all, or something)')

/*
    IF (rbm:=AllocBitMap(256, 192, 24, BMF_CLEAR, bm))=0
        StrCopy(filetypeerror,'Could not allocate Bitmap!')
         EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC',filetypeerror,'OK']:easystruct,0,NIL)
    	freeup()
		RETURN allok
    ENDIF
*/

->    IF (rbm:=AllocBitMap(bmh.width, bmh.height, bmh.depth, BMF_CLEAR, bm))=0
/*
specscreen:=OpenScreenTagList(NIL,
        [SA_WIDTH,     256,
         SA_HEIGHT,    192,
         SA_DEPTH,     4,
         SA_BEHIND,    TRUE,
         SA_QUIET,     TRUE,
         SA_SHOWTITLE, TRUE,
         SA_TYPE,      CUSTOMSCREEN,
         SA_EXCLUSIVE, TRUE,
         SA_COLORS32,  [16,$00000000,$00000000,$00000000,
$00000000,$00000000,$00000000,
$00000000,$00000000,$FFFFFFFF,
$FFFFFFFF,$00000000,$00000000,
$FFFFFFFF,$00000000,$FFFFFFFF,
$00000000,$FFFFFFFF,$00000000,
$00000000,$FFFFFFFF,$FFFFFFFF,
$FFFFFFFF,$FFFFFFFF,$00000000,
$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,
$00000000,$00000000,$DDDDDDDD,
$DDDDDDDD,$00000000,$00000000,
$DDDDDDDD,$00000000,$DDDDDDDD,
$00000000,$DDDDDDDD,$00000000,
$00000000,$DDDDDDDD,$DDDDDDDD,
$DDDDDDDD,$DDDDDDDD,$00000000,
$DDDDDDDD,$DDDDDDDD,$DDDDDDDD],
        TAG_DONE]:tagitem)
*/
/*
specscreen:=OpenS(256,192,4,NIL,NIL,        [SA_WIDTH,     256,
         SA_HEIGHT,    192,
         SA_DEPTH,     4,
         SA_BEHIND,    TRUE,
         SA_QUIET,     TRUE,
         SA_SHOWTITLE, FALSE,
         SA_TYPE,      CUSTOMSCREEN,
         SA_EXCLUSIVE, TRUE,
         NIL])

      SetColour(specscreen,0,0,0,0)
      SetColour(specscreen,1,0,0,0)
      SetColour(specscreen,2,0,0,255)
      SetColour(specscreen,3,255,0,0)
      SetColour(specscreen,4,255,0,255)
      SetColour(specscreen,5,0,255,0)
      SetColour(specscreen,6,0,255,255)
      SetColour(specscreen,7,255,255,0)
      SetColour(specscreen,8,255,255,255)
      SetColour(specscreen,9,0,0,221)
      SetColour(specscreen,10,221,0,0)
      SetColour(specscreen,11,221,0,221)
      SetColour(specscreen,12,0,221,0)
      SetColour(specscreen,13,0,221,221)
      SetColour(specscreen,14,221,221,0)
      SetColour(specscreen,15,221,221,221)
*/
IF clipboard=0
    IF (o:=NewDTObjectA(filename,
                       [DTA_SOURCETYPE, DTST_FILE,
                        PDTA_REMAP,     TRUE,
                        PDTA_DESTMODE,    PMODE_V43,
						PDTA_SCALEQUALITY,1,
->                        PDTA_MAXDITHERPENS,    2,
                        PDTA_SCREEN,           specscreen,
->                        PDTA_NUMSPARSE,        16,
->                        PDTA_SPARSETABLE,????,
                        TAG_DONE]))=0
/*
    IF (o:=NewDTObjectA(filename,
                       [DTA_SOURCETYPE, DTST_FILE,
                        PDTA_REMAP,     TRUE,
                        PDTA_NUMSPARSE, 15,
                        PDTA_SPARSETABLE, cm,
                        PDTA_DESTBITMAP, {rbm},
                        TAG_DONE]))=0
*/                      
         IF wbmessage=NIL
              IF quiet=0 THEN WriteF('\s\n',filetypeerror)
        ELSE
            EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC',filetypeerror,'OK']:easystruct,0,NIL)
        ENDIF
    	freeup()
		RETURN allok
    ENDIF
ELSE
    IF (o:=NewDTObjectA(clipunit,
                       [DTA_SOURCETYPE, DTST_CLIPBOARD,
                        PDTA_REMAP,     TRUE,
                        PDTA_DESTMODE,    PMODE_V43,
						PDTA_SCALEQUALITY,1,
                        DTA_GROUPID,GID_PICTURE,
                        TAG_DONE]))=0
                        
               IF wbmessage=NIL
          IF quiet=0 THEN WriteF('\s\n',filetypeerror)
      ELSE
        EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','Clipboard data not ILBM','OK']:easystruct,0,NIL)
    ENDIF
    	freeup()
		RETURN allok
    ENDIF
ENDIF

-> TEMP***** CloseScreen(specscreen)
-> CloseS(specscreen)

setprogresswin(5)

    /* Fill a BitMapHeader with information about the picture */

	/* Render the picture to a BitMap
	layout.methodid:=DTM_PROCLAYOUT
	layout.ginfo   :=NIL
	layout.initial :=TRUE
	IF (DoDTMethodA(o, NIL, NIL, layout))=0 THEN Raise(ERR_DTYP)
 */

->	IF (DoDTMethodA(o, NIL, NIL, [GM_RENDER,NIL,specscreen.rastport,GREDRAW_REDRAW]:gprender))=0 THEN Raise(ERR_DTYP)


    IF (GetDTAttrsA(o, [PDTA_BITMAPHEADER, bmh, PDTA_NUMCOLORS, cols,TAG_DONE]))<>2 THEN Raise(ERR_DTYP)

     StrAdd(filetype,'\nWidth: ')
     StrAdd(filetype,RealF(tmpstr,bmh[0].width!!FLOAT,0))
     StrAdd(filetype,'\nHeight: ')
     StrAdd(filetype,RealF(tmpstr,bmh[0].height!!FLOAT,0))
     StrAdd(filetype,'\nDepth: ')
     StrAdd(filetype,RealF(tmpstr,bmh[0].depth!!FLOAT,0))

    IF quiet=0 THEN WriteF('Width:  \d\nHeight: \d\nDepth:  \d bit\s ',
          bmh[0].width, bmh[0].height, bmh[0].depth, (IF bmh[0].depth=1 THEN ' ' ELSE 's'))

     StrAdd(filetype,' bit (')
     StrAdd(filetype,RealF(tmpstr,cols[0]!!FLOAT,0))
     StrAdd(filetype,' colours)')

-> extract info

StrCopy(objauth,'')
StrCopy(objcopy,'')
StrCopy(objanno,'')



GetDTAttrsA(o,[DTA_OBJNAME,objname2,NIL])



->    StrAdd(filetype,'\nName: ')
->    StrAdd(filetype,objname)

IF GetDTAttrsA(o,[DTA_OBJAUTHOR,objauth2,NIL])
IF objauth2<>NIL
    StrAdd(filetype,'\nAuthor: ')
    StrAdd(filetype,objauth2)
    StrCopy(objauth,objauth2)
ENDIF
ENDIF

IF GetDTAttrsA(o,[DTA_OBJCOPYRIGHT,objcopy2,NIL])
IF objcopy2<>NIL
    StrAdd(filetype,'\nCopyright: ')
    StrAdd(filetype,objcopy2)
StrCopy(objcopy,objcopy2)
ENDIF
ENDIF


IF GetDTAttrsA(o,[DTA_OBJANNOTATION,objanno2,NIL])
IF objanno2<>NIL
    StrAdd(filetype,'\nAnnotation: ')
    StrAdd(filetype,objanno2)
StrCopy(objanno,objanno2)
ENDIF
ENDIF

/*
IF GetDTAttrsA(o,[DTA_OBJVERSION,objver2,NIL])
    StrAdd(filetype,'\nVersion: ')
    StrAdd(filetype,objver)
ENDIF
*/


    -> *** end

    IF quiet=0 THEN WriteF('(\d colours)\n', cols[0])

    IF scaleornot
        StrCopy(wintit,'Scaling to Spectrum screen size...') -> 256x192
          IF wbmessage<>NIL THEN SetWindowTitles(curwin,wintit,-1)
        IF quiet=0 THEN WriteF('\s\n',wintit)

		IdoMethodA(o,[PDTM_SCALE,256,192,0]:pdtscale)

    ELSE
        IF bmh[0].width>256
            i:=256 ; IF quiet=0 THEN WriteF('Picture has been truncated horizontally\n')
        ELSE
            i:=bmh[0].width
        ENDIF
        IF bmh[0].height>192
            j:=192 ; IF quiet=0 THEN WriteF('Picture has been truncated vertically\n')
        ELSE
            j:=bmh[0].height
        ENDIF

    ENDIF
    
    setprogresswin(20)

    /* Finally, convert the BitMap to chunky data */

->    rgbs:=ppmmem

	IdoMethod(o,PDTM_READPIXELARRAY,ppmmem,PBPAFMT_RGB,256*3,0,0,256,192)

    setprogresswin(40)

allok:=1

ENDPROC allok

PROC freeup()
    
    IF o
      DisposeDTObject(o)
      o:=NIL
    ENDIF

    IF dbm
      FreeBitMap(dbm)       -> AFAIK, bm is freed by picture.datatype
      dbm:=NIL
->      bm:=NIL -> *** temp ***
    ENDIF

    IF rbm
      FreeBitMap(rbm)       -> AFAIK, bm is freed by picture.datatype
      rbm:=NIL
    ENDIF
    
/*
    IF deetee
      ReleaseDataType(deetee)
      deetee:=NIL
    ENDIF
    
    IF lock
      UnLock(lock)
      lock:=NIL
    ENDIF
*/

->    Flush(ppmfh)
->    Close(ppmfh)

IF acontrast THEN autocontrast()

ENDPROC

PROC autocontrast()
DEF n,highval=0

lowval:=255

/* auto-contrast detection stuff */
FOR n:=0 TO 49151               -> No. of pixels in a 256x192 piccie
IF ppmmem[n]>highval THEN highval:=ppmmem[n]
IF ppmmem[n]<lowval THEN lowval:=ppmmem[n]
ENDFOR
range:=highval-lowval

ENDPROC

-> ppmmem

PROC readtooltypes(fromcli=0)
DEF olddir=-1,tmplock

IF fromcli
    IF (iconbase:=OpenLibrary('icon.library',36))=0 THEN Raise(ERR_ICNN)
    dobj:=GetDiskObjectNew('PROGDIR:PDHFIC')
ELSE
    wbenchMsg:=wbmessage
    wbarg:=wbenchMsg.arglist
    IF (wbarg.lock) AND (wbarg.name<>0) THEN olddir:=CurrentDir(wbarg.lock)
    dobj:=GetDiskObject(wbarg.name)
    quiet:=1
ENDIF

IF dobj

toolarray:=dobj.tooltypes

IF tt:=FindToolType(toolarray,'COLOURSENSE') THEN thr:=Val(tt)

IF tt:=FindToolType(toolarray,'BRIGHTSENSE') THEN brthr:=Val(tt)

IF tt:=FindToolType(toolarray,'WHITESENSE') THEN wtthr:=Val(tt)

IF tt:=FindToolType(toolarray,'GREENMAGENTA') THEN grnmag:=Val(tt)

IF tt:=FindToolType(toolarray,'BLUECYAN') THEN blucyn:=Val(tt)

IF tt:=FindToolType(toolarray,'REDYELLOW') THEN redyel:=Val(tt)

IF tt:=FindToolType(toolarray,'FLASHRED') THEN flashred:=Val(tt)

IF tt:=FindToolType(toolarray,'FLASHGRN') THEN flashgrn:=Val(tt)

IF tt:=FindToolType(toolarray,'FLASHBLU') THEN flashblu:=Val(tt)

IF tt:=FindToolType(toolarray,'CLIPUNIT') THEN clipunit:=Val(tt)

IF FindToolType(toolarray,'GREYSCALE') THEN thr:=5000

IF FindToolType(toolarray,'NOBRIGHT') THEN brthr:=5000

IF FindToolType(toolarray,'NOSMOOTH') THEN smooth:=1

IF FindToolType(toolarray,'NOSCALE') THEN scale:=0

IF FindToolType(toolarray,'ROMREMAP') THEN os3:=1

IF FindToolType(toolarray,'SIMPLEMAP') THEN simpleremap:=1

IF FindToolType(toolarray,'ALTPALETTE') THEN rompal:=1

IF FindToolType(toolarray,'QUIET') THEN quiet:=1

IF tt:=FindToolType(toolarray,'SAVEFORMAT')
    IF MatchToolValue(tt,'SCR') THEN saveformat:=0
    IF MatchToolValue(tt,'ZX82') THEN saveformat:=1
    IF MatchToolValue(tt,'BYTES') THEN saveformat:=2
    IF MatchToolValue(tt,'TAP') THEN saveformat:=3
    IF MatchToolValue(tt,'TZX') THEN saveformat:=4
    IF MatchToolValue(tt,'PLUS3') THEN saveformat:=5
    IF MatchToolValue(tt,'GIF')
        saveformat:=0
        gifsave:=1
    ENDIF
ENDIF

IF FindToolType(toolarray,'ADVANCED') THEN advopts:=1

IF FindToolType(toolarray,'SCREEN') THEN mvscreen:=1

IF FindToolType(toolarray,'WARNOVERWRITE') THEN askover:=1

IF tt:=FindToolType(toolarray,'TZXBLOCKS')
    tzxarray[0]:=0
    tzxarray[1]:=0
    tzxarray[2]:=0
    IF MatchToolValue(tt,'INFO') THEN tzxarray[0]:=1
    IF MatchToolValue(tt,'CUSTOM') THEN tzxarray[2]:=1 ELSE tzxarray[1]:=1

ENDIF

IF tt:=FindToolType(toolarray,'ODDCOLOURS')
    IF MatchToolValue(tt,'INK') THEN allelseink:=1
    IF MatchToolValue(tt,'PAPER') THEN allelseink:=0
ENDIF

IF FindToolType(toolarray,'NOHEADER') THEN writeheader:=0
IF FindToolType(toolarray,'NOFILTER') THEN nofilter:=1
IF FindToolType(toolarray,'NOPROGRESSBAR') THEN noprogress:=1

autonaming:=1
IF FindToolType(toolarray,'NOAUTONAMING') THEN autonaming:=0

IF FindToolType(toolarray,'NOATTRIBUTES') THEN saveattr:=0 ELSE saveattr:=1
IF FindToolType(toolarray,'AUTOCONTRAST') THEN acontrast:=1 ELSE acontrast:=0
IF FindToolType(toolarray,'REMAPAFTERDITHER') THEN remapafterdither:=1 ELSE remapafterdither:=0

->IF FindToolType(toolarray,'TESTMODE') THEN testmode:=1

IF FindToolType(toolarray,'DITHER') THEN dither:=1

dtype:=0
IF tt:=FindToolType(toolarray,'DITHERTYPE')
    IF MatchToolValue(tt,'RANDOM') THEN dtype:=1
ENDIF

IF tt:=FindToolType(toolarray,'BORDER') THEN tzxborder:=Val(tt)

IF FindToolType(toolarray,'STRANGEMODE') THEN EasyRequestArgs(0,[SIZEOF easystruct,0,'Welcome...','Hello.  I\'m Nigel Blenkinsopp,\nthe long lost explorer of the\nfar east.','Pleased to meet you']:easystruct,0,NIL)

IF tt:=FindToolType(toolarray,'WINX') THEN winx:=Val(tt)
IF tt:=FindToolType(toolarray,'WINY') THEN winy:=Val(tt)
IF tt:=FindToolType(toolarray,'ASLX') THEN aslx:=Val(tt)
IF tt:=FindToolType(toolarray,'ASLY') THEN asly:=Val(tt)

FreeDiskObject(dobj)
dobj:=NIL
ENDIF

-> IF olddir<>-1 THEN CurrentDir(olddir) -> check this is ok!

ENDPROC

PROC gadgets()
DEF gad,type

  mysc:=LockPubScreen(NIL)
->  tbar:=mysc.font.ysize+mysc.wbortop+1 /* "real" tbar */

tbar:=mysc.font.ysize+mysc.wbortop-1

->  winheight:=tbar+10 /* + progress bar height */
->wptr=nil

/*
type[0]:='SCR'
type[1]:='ZX82'
type[2]:='.bytes'
type[3]:='TAP'
*/

  vi:=GetVisualInfoA(mysc, [NIL])
  -> GadTools gadgets require this step to be taken
  gad:=CreateContext(glist)

  -> Create a button gadget centered below the window title
-> gadg 1
  gads[GADG_LOADSTR]:=CreateGadgetA(TEXT_KIND, gad,
                    [70, (pixsep+tbar),
                     128, (mysc.font.ysize+4),
                     'Input:', mysc.font,
                     GADG_LOADSTR, 0,
                     vi, NIL]:newgadget,
                    [GTTX_BORDER,TRUE,NIL]:tagitem)
                    -> GTTX_JUSTIFICATION,GTJ_RIGHT,NIL])

-> ['topaz.font', 8, 0, 0]:textattr

  gads[GADG_LOAD]:=CreateGadgetA(BUTTON_KIND, gads[GADG_LOADSTR],
                    [200, (pixsep+tbar),
                     60, (mysc.font.ysize+4),
                     'Select', mysc.font,
                     GADG_LOAD, 0,
                     vi, NIL]:newgadget,
                    [NIL])

-> STRING_
  gads[GADG_SAVESTR]:=CreateGadgetA(TEXT_KIND, gads[GADG_LOAD],
                    [70, ((pixsep*2)+mysc.font.ysize+tbar), /* 20 */
                     128, (mysc.font.ysize+4),
                     'Output:',mysc.font,
                     GADG_SAVESTR, 0,
                     vi, NIL]:newgadget,
                    [GTTX_BORDER,TRUE,NIL])
                    -> GTTX_JUSTIFICATION,GTJ_RIGHT,NIL])

  gads[GADG_SAVE]:=CreateGadgetA(BUTTON_KIND, gads[GADG_SAVESTR],
                    [200, ((pixsep*2)+mysc.font.ysize+tbar),
                     60, (mysc.font.ysize+4),
                     'Select', mysc.font,
                     GADG_SAVE, 0,
                     vi, NIL]:newgadget,
                    [NIL])

  gads[GADG_START]:=CreateGadgetA(BUTTON_KIND, gads[GADG_SAVE],
                    [160, ((pixsep*5)+(mysc.font.ysize*4)+tbar),
                     100, (mysc.font.ysize+4),
                     'Convert!', mysc.font,
                     GADG_START, 0,
                     vi, NIL]:newgadget,
                    [GA_DISABLED,TRUE,NIL])

/*
type[0]:='SCR'
type[1]:='ZX82'
type[2]:='.bytes'
type[3]:='TAP'
type[4]:='TZX'
IF gifavailable
    type[5]:='GIF'
    type[6]:=NIL
ELSE
    type[5]:=NIL
ENDIF
*/

IF gifavailable
    type:=['SCR','ZX82','.bytes','TAP','TZX','+3DOS','GIF',NIL]
    IF gifsave THEN saveformat:=5
ELSE
    type:=['SCR','ZX82','.bytes','TAP','TZX','+3DOS',NIL]
ENDIF

-> MX_


  gads[GADG_TYPE]:=CreateGadgetA(CYCLE_KIND, gads[GADG_START],
                    [70, ((pixsep*5)+(mysc.font.ysize*4)+tbar),
                     75, (mysc.font.ysize+4),
                     'Save as', mysc.font,
                     GADG_TYPE, 0,
                     vi, NIL]:newgadget,
                    [GTCY_LABELS,type,GTCY_ACTIVE,saveformat,NIL])

/*
                    GTMX_TITLEPLACE,PLACETEXT_ABOVE,
                    NIL])
*/

IF saveformat=5 THEN saveformat:=0

  gads[GADG_ROMREMAP]:=CreateGadgetA(CHECKBOX_KIND, gads[GADG_TYPE],
                    [95, ((pixsep*4)+(mysc.font.ysize*3)+tbar),
                     27, (mysc.font.ysize+4),
                     'ROM Remap', mysc.font,
                     GADG_ROMREMAP, 0,
                     vi, NIL]:newgadget,
                    [GTCB_CHECKED,os3,GTCB_SCALED,TRUE,NIL])

  gads[GADG_SCALE]:=CreateGadgetA(CHECKBOX_KIND, gads[GADG_ROMREMAP],
                    [230, ((pixsep*3)+(mysc.font.ysize*2)+tbar),
                     27, (mysc.font.ysize+4),
                     'Scale Pic', mysc.font,
                     GADG_SCALE, 0,
                     vi, NIL]:newgadget,
                    [GTCB_CHECKED,scale,GTCB_SCALED,TRUE,NIL])

  gads[ADV_SMOOTH]:=CreateGadgetA(CHECKBOX_KIND, gads[GADG_SCALE],
                    [95, ((pixsep*3)+(mysc.font.ysize*2)+tbar),
                     27, (mysc.font.ysize+4),
                     'Smooth', mysc.font,
                     ADV_SMOOTH, 0,
                     vi, NIL]:newgadget,
                    [GTCB_CHECKED,smooth,GTCB_SCALED,TRUE,NIL])

  gads[ADV_ZXDTPAL]:=CreateGadgetA(CHECKBOX_KIND, gads[ADV_SMOOTH],
                    [230, ((pixsep*4)+(mysc.font.ysize*3)+tbar),
                     27, (mysc.font.ysize+4),
                     'Dither', mysc.font,
                     ADV_ZXDTPAL, 0,
                     vi, NIL]:newgadget,
                    [GTCB_CHECKED,dither,
                    GTCB_SCALED,TRUE,NIL])

ENDPROC

PROC menus()
DEF bored=0,advmen,scrmen,askovermen,tzx0men,tzx1men,tzx2men,headermen
DEF bordermen[8]:ARRAY OF LONG,z,filtermen,progressmen
DEF oddcolsmen0,oddcolsmen1,oddcolsmen2,extractmen,snapwinmen,saveattrmen
DEF simplemapmen,zxdtmen,dtypemen0,dtypemen1,radmen,alsbase=NIL

IF alsbase=NIL THEN bored:=NM_ITEMDISABLED
IF advopts THEN advmen:=(CHECKIT OR MENUTOGGLE OR CHECKED) ELSE advmen:=(CHECKIT OR MENUTOGGLE)
IF winy<>-1
    snapwinmen:=(CHECKIT OR MENUTOGGLE OR CHECKED)
    snapshotwindows:=1
ELSE
    snapwinmen:=(CHECKIT OR MENUTOGGLE)
    snapshotwindows:=0
ENDIF
IF mvscreen THEN scrmen:=(CHECKIT OR MENUTOGGLE OR CHECKED) ELSE scrmen:=(CHECKIT OR MENUTOGGLE)
IF askover THEN askovermen:=(CHECKIT OR MENUTOGGLE OR CHECKED) ELSE askovermen:=(CHECKIT OR MENUTOGGLE)
IF writeheader THEN headermen:=(CHECKIT OR MENUTOGGLE OR CHECKED) ELSE headermen:=(CHECKIT OR MENUTOGGLE)
IF tzxarray[0] THEN tzx0men:=(CHECKIT OR MENUTOGGLE OR CHECKED) ELSE tzx0men:=(CHECKIT OR MENUTOGGLE)
IF tzxarray[1] THEN tzx1men:=(CHECKIT OR CHECKED) ELSE tzx1men:=(CHECKIT)
IF tzxarray[2] THEN tzx2men:=(CHECKIT OR CHECKED) ELSE tzx2men:=(CHECKIT)
IF nofilter THEN filtermen:=(CHECKIT OR MENUTOGGLE) ELSE filtermen:=(CHECKIT OR MENUTOGGLE OR CHECKED)
IF noprogress THEN progressmen:=(CHECKIT OR MENUTOGGLE) ELSE progressmen:=(CHECKIT OR MENUTOGGLE OR CHECKED)
IF autonaming THEN extractmen:=(CHECKIT OR MENUTOGGLE OR CHECKED) ELSE extractmen:=(CHECKIT OR MENUTOGGLE)
IF saveattr THEN saveattrmen:=(CHECKIT OR MENUTOGGLE OR CHECKED) ELSE saveattrmen:=(CHECKIT OR MENUTOGGLE)
IF simpleremap THEN simplemapmen:=(CHECKIT OR MENUTOGGLE OR CHECKED) ELSE simplemapmen:=(CHECKIT OR MENUTOGGLE)
IF rompal THEN zxdtmen:=(CHECKIT OR MENUTOGGLE) ELSE zxdtmen:=(CHECKIT OR MENUTOGGLE OR CHECKED)
IF remapafterdither THEN radmen:=(CHECKIT OR MENUTOGGLE OR CHECKED) ELSE radmen:=(CHECKIT OR MENUTOGGLE)


oddcolsmen0:=(CHECKIT OR NM_ITEMDISABLED)
IF allelseink
    oddcolsmen1:=(CHECKIT OR CHECKED)
    oddcolsmen2:=(CHECKIT)
ELSE
    oddcolsmen1:=(CHECKIT)
    oddcolsmen2:=(CHECKIT OR CHECKED)
ENDIF

IF dtype=0
    dtypemen0:=(CHECKIT OR CHECKED)
    dtypemen1:=(CHECKIT)
ENDIF

IF dtype=1
    dtypemen1:=(CHECKIT OR CHECKED)
    dtypemen0:=(CHECKIT)
ENDIF

FOR z:=0 TO 8
    bordermen[z]:=(CHECKIT)
ENDFOR

bordermen[tzxborder]:=(CHECKIT OR CHECKED)

menustrip:=CreateMenusA([NM_TITLE,0,'Project',0,0,0,0,
                         NM_ITEM,0,'About...','A',0,0,0,
                         NM_ITEM,0,NM_BARLABEL,0,0,0,0,
                         NM_ITEM,0,'Quit...','Q',0,0,0,
                         NM_TITLE,0,'Picture',0,0,0,0,
                         NM_ITEM,0,'Select Source...','S',0,0,0,
                         NM_ITEM,0,'From Clipboard','V',0,0,0,
                         NM_ITEM,0,'Display...',0,0,0,0,
                         NM_ITEM,0,NM_BARLABEL,0,0,0,0,
                         NM_ITEM,0,'Select Destination...','D',0,0,0,
                         NM_ITEM,0,'Display...',0,0,0,0,
                         NM_ITEM,0,NM_BARLABEL,0,0,0,0,
                         NM_ITEM,0,'Information...','I',0,0,0,
->                         NM_ITEM,0,NM_BARLABEL,0,0,0,0,
                    NM_TITLE,0,'Settings',0,0,0,0,
                         NM_ITEM,0,'Advanced Options','O',advmen,0,0,
                         NM_ITEM,0,'Use Screen',0,scrmen,0,0,
                         NM_ITEM,0,'Warn Overwrite',0,askovermen,0,0,
                         NM_ITEM,0,'Intelligent Filtering',0,filtermen,0,0,
                         NM_ITEM,0,'Automatic Naming',0,extractmen,0,0,
                         NM_ITEM,0,'Progress Window',0,progressmen,0,0,
                         NM_ITEM,0,NM_BARLABEL,0,0,0,0,
                         NM_ITEM,0,'Map Extra Colours To',0,0,0,0,
                         NM_SUB,0,'Auto',0,oddcolsmen0,0,0,
                         NM_SUB,0,'Ink',0,oddcolsmen1,4,0,
                         NM_SUB,0,'Paper',0,oddcolsmen2,2,0,
                         NM_ITEM,0,'Attributes',0,saveattrmen,0,0,
                         NM_ITEM,0,'ZXDT Palette',0,zxdtmen,0,0,
                         NM_ITEM,0,'Dithering Method',0,0,0,0,
                         NM_SUB,0,'Ordered','C',dtypemen0,2,0,
                         NM_SUB,0,'Random','R',dtypemen1,1,0,
                         NM_ITEM,0,'Dither SimpleMap',0,simplemapmen,0,0,
                         NM_ITEM,0,'Remap After Dither',0,radmen,0,0,
                         NM_ITEM,0,NM_BARLABEL,0,0,0,0,
                         NM_ITEM,0,'TZX Blocks',0,0,0,0,
                         NM_SUB,0,'Archive Info',0,tzx0men,0,0,
                         NM_SUB,0,NM_BARLABEL,0,0,0,0,
                         NM_SUB,0,'Standard',0,tzx1men,8,0,
                         NM_SUB,0,'Custom',0,tzx2men,4,0,
                         NM_ITEM,0,'TZX Custom Block Border',0,0,0,0,
                         NM_SUB,0,'Black','0',bordermen[0],254,0,
                         NM_SUB,0,'Blue','1',bordermen[1],253,0,
                         NM_SUB,0,'Red','2',bordermen[2],251,0,
                         NM_SUB,0,'Cyan','3',bordermen[3],247,0,
                         NM_SUB,0,'Green','4',bordermen[4],239,0,
                         NM_SUB,0,'Magenta','5',bordermen[5],223,0,
                         NM_SUB,0,'Yellow','6',bordermen[6],191,0,
                         NM_SUB,0,'White','7',bordermen[7],127,0,
                         NM_ITEM,0,'Write .header',0,headermen,0,0,
                         NM_ITEM,0,NM_BARLABEL,0,0,0,0,
                         NM_ITEM,0,'Snapshot Window',0,snapwinmen,0,0,
                         NM_ITEM,0,'Save Configuration',0,0,0,0,
->                         NM_ITEM,0,'Flash Colour...','F',0,0,0,
                         NM_TITLE,0,'Help',0,0,0,0,
                         NM_ITEM,0,'Selected Output Format...',0,0,0,0,
                         NM_ITEM,0,'Relieving Boredom...','B',bored,0,0,
                         NM_TITLE,0,'ARexx',0,0,0,0,
                         NM_ITEM,0,'Execute Script...','E',0,0,0,
                         NM_END,0,NIL,0,0,0,0]:newmenu,
                         [GTMN_FULLMENU,TRUE,GTMN_FRONTPEN,1,
                         NIL])

ENDPROC

PROC openwin()
DEF zoompos[5]:ARRAY OF INT,tmplock

/*
IF testmode=1 THEN EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','Er...\n\nThe "TESTMODE" tooltype appears\nto be enabled.\n\nNot good.\n\nThis attempts to scale pics using\nPDHFIC\'s internal routines.\n\nScaling more than one pic\nin test mode will almost\ncertainly crash your Amiga!','Point Taken']:easystruct,0,NIL)

IF tmplock:=Lock('extscale',ACCESS_READ)
    noextscale:=0
    UnLock(tmplock)
ELSE
    noextscale:=1
    IF testmode=0 THEN EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','The program "extscale"\ncould not be found in\nPDHFIC\'s directory\n\nYou will not be able to\nscale more than one pic!','Point Taken']:easystruct,0,NIL)
ENDIF
*/

IF tmplock:=Lock('SCR2GIF',ACCESS_READ)
    gifavailable:=1
    UnLock(tmplock)
ENDIF

gadgets()
advgadgets()

zoompos[0]:=-1
zoompos[1]:=-1
zoompos[2]:=270
zoompos[3]:=tbar+2
zoompos[4]:=NIL

IF winy<>-1

wptr:=OpenWindowTagList(NIL,[WA_TITLE,'PDHFIC',
                           WA_TOP,winy,WA_LEFT,winx,
                           WA_GADGETS,   glist[0], WA_AUTOADJUST,    FALSE,
                           WA_WIDTH,     270,   
                           WA_HEIGHT,((pixsep*6)+(mysc.font.ysize*5)+tbar+3),
                           WA_DRAGBAR,   TRUE,  WA_DEPTHGADGET,   TRUE,
                           WA_ACTIVATE,  TRUE,  WA_CLOSEGADGET,   TRUE,
                           WA_NEWLOOKMENUS, TRUE, ->WA_AUTOADJUST,TRUE,
                           WA_SCREENTITLE,'PDHFIC 3.2  © 1998-2001,2008 Unsatisfactory Software',
                           WA_IDCMP, IDCMP_CLOSEWINDOW OR IDCMP_REFRESHWINDOW OR BUTTONIDCMP OR IDCMP_MENUPICK,
                           WA_ZOOM,zoompos,
                           NIL]:tagitem)
ELSE
wptr:=OpenWindowTagList(NIL,[WA_TITLE,'PDHFIC',
                           WA_GADGETS,   glist[0],
							WA_AUTOADJUST,    FALSE,
                           WA_WIDTH,     270,   
                           WA_HEIGHT,((pixsep*6)+(mysc.font.ysize*5)+tbar+3),
                           WA_DRAGBAR,   TRUE,  WA_DEPTHGADGET,   TRUE,
                           WA_ACTIVATE,  TRUE,  WA_CLOSEGADGET,   TRUE,
                           WA_NEWLOOKMENUS, TRUE, -> WA_AUTOADJUST,TRUE,
                           WA_SCREENTITLE,'PDHFIC 3.2  © 1998-2001,2008 Unsatisfactory Software',
                           WA_IDCMP, IDCMP_CLOSEWINDOW OR IDCMP_REFRESHWINDOW OR BUTTONIDCMP OR IDCMP_MENUPICK,
                           WA_ZOOM,zoompos,
                           NIL]:tagitem)
ENDIF

  Gt_RefreshWindow(wptr, NIL)

menus()

IF LayoutMenusA(menustrip,vi,[NIL])
  SetMenuStrip(wptr,menustrip) -> *IF*
ENDIF

rp:=wptr.rport

IF advopts
  advopts:=0
  advancedoptions()
ENDIF

handle_gadgets()

ENDPROC

PROC askload()
DEF tmp2[1024]:STRING,tmp:ARRAY OF CHAR,myhookfunc[1]:ARRAY OF hook

clipboard:=0
tmp:=NIL

myhookfunc[0].entry := CALLBACK aslhookfunc()
myhookfunc[0].subentry:=NIL
myhookfunc[0].data:=NIL

IF filename
	StrCopy(tmp2,filename)
	tmp:=FilePart(tmp2)
ENDIF

IF nofilter

    IF (aslx) AND (asly)

filereq:=AllocAslRequest(ASL_FILEREQUEST,
            [ASLFR_TITLETEXT,'Select image to convert...',
            ASLFR_REJECTICONS,TRUE,
            ASLFR_INITIALDRAWER,loaddrw,
            ASLFR_INITIALFILE,tmp,
            ASLFR_INITIALWIDTH,aslx,
            ASLFR_INITIALHEIGHT,asly,
            NIL])

    ELSE

filereq:=AllocAslRequest(ASL_FILEREQUEST,
            [ASLFR_TITLETEXT,'Select image to convert...',
            ASLFR_REJECTICONS,TRUE,
            ASLFR_INITIALDRAWER,loaddrw,
            ASLFR_INITIALFILE,tmp,
            NIL])
    ENDIF

ELSE

->myhookfunc:=eCodeASLHook({aslhookfunc})

    IF (aslx) AND (asly)

filereq:=AllocAslRequest(ASL_FILEREQUEST,
            [ASLFR_TITLETEXT,'Select image to convert...',
            ASLFR_REJECTICONS,TRUE,
            ASLFR_INITIALDRAWER,loaddrw,
            ASLFR_INITIALFILE,tmp,
            ASLFR_INITIALWIDTH,aslx,
            ASLFR_INITIALHEIGHT,asly,
            ASLFR_FLAGS1,FRF_FILTERFUNC,
            ASLFR_FILTERFUNC,myhookfunc,
            NIL])

    ELSE
filereq:=AllocAslRequest(ASL_FILEREQUEST,
            [ASLFR_TITLETEXT,'Select image to convert...',
            ASLFR_REJECTICONS,TRUE,
            ASLFR_INITIALDRAWER,loaddrw,
            ASLFR_INITIALFILE,tmp,
            ASLFR_FLAGS1,FRF_FILTERFUNC,
            ASLFR_FILTERFUNC,myhookfunc,
            NIL])

    ENDIF

ENDIF

IF filereq

okay:=AslRequest(filereq,NIL)

IF okay=FALSE THEN RETURN

StrCopy(filename,filereq.drawer)

StrCopy(loaddrw,filereq.drawer)
AddPart(filename,filereq.file,1024)

StrCopy(fname,filename)

/*
StrCopy(drw,filename)
fname:=FilePart(fname)
drw:=PathPart(fname)
*/

FreeAslRequest(filereq)
filereq:=NIL
ENDIF

IF quiet=0 THEN WriteF('Input file: \s\n',filename)

ENDPROC

PROC askoutput()
IF EstrLen(filename)=NIL
StrCopy(msg,'No file selected')
ELSE

IF clipboard=0
    StrCopy(msg,'File: ')
    StrAdd(msg,filename)
    StrAdd(msg,'\nType: ')
ELSE
    StrCopy(msg,'Using Clipboard')
ENDIF

    StrAdd(msg,filetype)

IF chg=1 THEN StrAdd(msg,'\n\nPicture was scaled')
IF chg=2 THEN StrAdd(msg,'\n\nPicture was truncated')

-> IF os3 THEN StrAdd(msg,'\n*** OS3 REMAP MODE! ***')

-> StrAdd(msg,'\n\nPlease select output format')

ENDIF

EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC',msg,'OK']:easystruct,0,NIL)

-> 'ZX82|BYTES|TAP|SCR'


ENDPROC

PROC autoname(updategadgets=0,extonly=0)
DEF tmpfn[1024]:STRING

IF autonaming=0 THEN RETURN
IF EstrLen(filename)=0 THEN RETURN

StrCopy(fname,filename)

StrCopy(tmpfn,FilePart(fname))

IF EstrLen(scrfile)=0
    MidStr(drw,fname,0,EstrLen(fname)-EstrLen(tmpfn))
    extonly:=0
ENDIF

IF extonly THEN StrCopy(tmpfn,FilePart(scrfile))

dot:=InStr(tmpfn,'.')

IF dot<>-1
    StrCopy(fname,tmpfn,dot)
ELSE
    StrCopy(fname,tmpfn)
ENDIF

IF saveformat=0
    IF gifsave
        StrAdd(fname,'.gif')
    ELSE
        StrAdd(fname,'.scr')
    ENDIF
ENDIF
IF saveformat=1 THEN StrAdd(fname,'.ZX82')
IF saveformat=2 THEN StrAdd(fname,'.bytes')
IF saveformat=3 THEN StrAdd(fname,'.tap')
IF saveformat=4 THEN StrAdd(fname,'.tzx')
/* IF saveformat=5 THEN StrAdd(fname,'.plus3')
     sod this, .ZXT is the best I can do, and that's not identical */

StrCopy(scrfile,drw)
AddPart(scrfile,fname,1024)

IF updategadgets
    Gt_SetGadgetAttrsA(gads[GADG_SAVESTR],wptr,NIL,[GTTX_TEXT,FilePart(scrfile),NIL])
->    IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL) THEN Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,FALSE,NIL]) ELSE Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,TRUE,NIL])
ENDIF

ENDPROC

PROC asksave()
DEF tmpfname:ARRAY OF CHAR

/*** now in autoname()

DEF tmpfname[1024]:STRING
-> StrCopy(fname,filename)
-> StrCopy(drw,fname)
-> drw:=PathPart(drw)

tmpfname:=FilePart(fname)
dot:=InStr(tmpfname,'.')

IF dot<>-1
    StrCopy(fname,tmpfname,dot)
ELSE
    StrCopy(fname,tmpfname)
ENDIF

IF saveformat=0 THEN StrAdd(fname,'.scr')
IF saveformat=1 THEN StrAdd(fname,'.ZX82')
IF saveformat=2 THEN StrAdd(fname,'.bytes')
IF saveformat=3 THEN StrAdd(fname,'.tap')
IF saveformat=4 THEN StrAdd(fname,'.tzx')

***/

IF (aslx) AND (asly)

savereq:=AllocAslRequest(ASL_FILEREQUEST,
[ASLFR_TITLETEXT,'Enter a filename to save Spectrum screen as...',
ASLFR_REJECTICONS,TRUE,
ASLFR_INITIALDRAWER,drw,
ASLFR_INITIALFILE,fname,
ASLFR_DOSAVEMODE,TRUE,
ASLFR_INITIALWIDTH,aslx,
ASLFR_INITIALHEIGHT,asly,
NIL])

ELSE
savereq:=AllocAslRequest(ASL_FILEREQUEST,
[ASLFR_TITLETEXT,'Enter a filename to save Spectrum screen as...',
ASLFR_REJECTICONS,TRUE,
ASLFR_INITIALDRAWER,drw,
ASLFR_INITIALFILE,fname,
ASLFR_DOSAVEMODE,TRUE,
NIL])
ENDIF



okay:=AslRequest(savereq,NIL)

IF okay=FALSE THEN RETURN

StrCopy(scrfile,savereq.drawer)

IF StrLen(savereq.file)=0
    tmpfname:=FilePart(filename)
    dot:=InStr(tmpfname,'.')

    IF dot<>-1
        StrCopy(fname,tmpfname,dot)
    ELSE
        StrCopy(fname,tmpfname)
    ENDIF

    IF saveformat=0
        IF gifsave
            StrAdd(fname,'.gif')
        ELSE
            StrAdd(fname,'.scr')
        ENDIF
    ENDIF
    IF saveformat=1 THEN StrAdd(fname,'.ZX82')
    IF saveformat=2 THEN StrAdd(fname,'.bytes')
    IF saveformat=3 THEN StrAdd(fname,'.tap')
    IF saveformat=4 THEN StrAdd(fname,'.tzx')

    AddPart(scrfile,fname,1024)
ELSE
    AddPart(scrfile,savereq.file,1024)
ENDIF

StrCopy(fname,savereq.file)
StrCopy(drw,savereq.drawer)

IF savereq THEN FreeAslRequest(savereq)
savereq:=NIL

-> ENDIF




ENDPROC

PROC handle_gadgets(noarexx=0)
  DEF imsg:PTR TO intuimessage, gad:PTR TO gadget, terminated=FALSE, class
  DEF menunumber,menunum,itemnum,subnum,signal=NIL,winsig,appwinsig
  DEF argptr:PTR TO wbarg,item:PTR TO menuitem,arexxsig,rexxcmd,arexxmsg:PTR TO rexxmsg
  DEF rc,result[128]:STRING,quote,tzxsize,stop=0

-> noarexx:=1 -> temp for portablE port


IF noarexx=0
    arexxport,arexxsig:=rx_OpenPort('PDHFIC')
ELSE
    arexxport:=NIL
    arexxsig:=NIL
     EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','ARexx port could\nnot be allocated','OK']:easystruct,0,NIL)
ENDIF


curwin:=wptr
vp:=mysc.viewport
cmap:=vp.colormap
-> stdrast:=rp

flashcol:=ObtainPen(cmap,-1,Shl(flashred,24),Shl(flashgrn,24),Shl(flashblu,24),PEN_EXCLUSIVE OR PEN_NO_SETCOLOR)

IF flashcol<>-1
        IF (flashred<>-1) AND (flashgrn<>-1) AND (flashblu<>-1)
            SetColour(mysc,flashcol,flashred,flashgrn,flashblu)
        ENDIF
    SetAPen(rp,flashcol)
ELSE
    IF flashcol:=ObtainBestPenA(cmap,Shl(flashred,24),Shl(flashgrn,24),Shl(flashblu,24),[OBP_PRECISION,PRECISION_GUI,NIL])
        obtainbest:=1
        SetAPen(rp,flashcol)
    ELSE
        SetAPen(rp,1) -> 0
    ENDIF
ENDIF

appwinport:=CreateMsgPort()
appwin:=AddAppWindowA(1,0,wptr,appwinport,NIL)


winsig:=Shl(1,wptr.userport.sigbit)
appwinsig:=Shl(1,appwinport.sigbit)
-> arexxsig:=Shl(1,arexxport.sigbit)

  REPEAT
    signal:=Wait(winsig OR appwinsig OR arexxsig)

IF signal AND winsig

    -> Use Gt_GetIMsg() and Gt_ReplyIMsg() for handling IntuiMessages
    -> with GadTools gadgets.
    WHILE (terminated=FALSE) AND (imsg:=Gt_GetIMsg(wptr.userport))
      -> Gt_ReplyIMsg() at end of loop
      class:=imsg.class
      SELECT class
      CASE IDCMP_GADGETUP  -> Buttons only report GADGETUP
        gad:=imsg.iaddress
        IF gad.gadgetid=GADG_LOAD
        SetWindowPointerA(wptr,[WA_BUSYPOINTER,TRUE,WA_POINTERDELAY,TRUE,NIL])
          askload()
          identifyfile()
          Gt_SetGadgetAttrsA(gads[GADG_LOADSTR],wptr,NIL,[GTTX_TEXT,FilePart(filename),NIL])
        autoname(1) -> autonaming
        IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL) THEN Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,FALSE,NIL]) ELSE Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,TRUE,NIL])
        SetWindowPointerA(wptr,[NIL])
        ENDIF
        IF gad.gadgetid=GADG_SAVE
        SetWindowPointerA(wptr,[WA_BUSYPOINTER,TRUE,WA_POINTERDELAY,TRUE,NIL])
        asksave()
-> GTST_STRING
          Gt_SetGadgetAttrsA(gads[GADG_SAVESTR],wptr,NIL,[GTTX_TEXT,FilePart(scrfile),NIL])
        IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL) THEN Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,FALSE,NIL]) ELSE Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,TRUE,NIL])
        SetWindowPointerA(wptr,[NIL])
        ENDIF
/*
        IF gad.gadgetid=GADG_SAVESTR
          Gt_GetGadgetAttrsA(gads[GADG_SAVESTR],wptr,NIL,[GTST_STRING,filename,NIL])
        ENDIF
        IF gad.gadgetid=GADG_LOADSTR
          Gt_GetGadgetAttrsA(gads[GADG_LOADSTR],wptr,NIL,[GTST_STRING,filename,NIL])
          identifyfile()
          Gt_SetGadgetAttrsA(gads[GADG_LOADSTR],wptr,NIL,[GTST_STRING,filename,NIL])
        ENDIF
*/
        IF gad.gadgetid=GADG_START
        IF askover
            IF lock:=Lock(scrfile,ACCESS_READ)
                stop:=0
                IF EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','Warning!\n\nThat file already exists,\nand will be overwritten!','Overwrite|Cancel']:easystruct,0,NIL)=0 THEN stop:=1
                UnLock(lock)
                lock:=NIL
            ENDIF
        ENDIF
        IF stop=0
            SetWindowPointerA(wptr,[WA_BUSYPOINTER,TRUE,WA_POINTERDELAY,TRUE,NIL])
            openprogresswin()
            identifyfile() -> TEMP???!
               IF dt2ppm(scale) THEN ppmtoscr()
              closeprogresswin()
            SetWindowPointerA(wptr,[NIL])
        ENDIF
        stop:=0
        ENDIF
        IF gad.gadgetid=GADG_TYPE
        IF imsg.code=6
            saveformat:=0
            gifsave:=1
        ELSE
            saveformat:=imsg.code
            gifsave:=0
        ENDIF
        autoname(1,1)
        ENDIF
        IF gad.gadgetid=GADG_ROMREMAP
          os3:=imsg.code
             -> Gt_SetGadgetAttrsA(gads[ADV_ZXDTPAL],wptr,NIL,[GA_DISABLED,(os3-1),NIL])

            IF advopts
                Gt_SetGadgetAttrsA(gads[ADV_BRIGHT],wptr,NIL,[GA_DISABLED,(os3),NIL])
                Gt_SetGadgetAttrsA(gads[ADV_COLOUR],wptr,NIL,[GA_DISABLED,(os3),NIL])
                Gt_SetGadgetAttrsA(gads[ADV_WHITE],wptr,NIL,[GA_DISABLED,(os3),NIL])
                Gt_SetGadgetAttrsA(gads[ADV_BLUCYN],wptr,NIL,[GA_DISABLED,(os3),NIL])
                Gt_SetGadgetAttrsA(gads[ADV_GRNMAG],wptr,NIL,[GA_DISABLED,(os3),NIL])
                Gt_SetGadgetAttrsA(gads[ADV_REDYEL],wptr,NIL,[GA_DISABLED,(os3),NIL])
            ENDIF
        ENDIF

        IF gad.gadgetid=GADG_SCALE THEN scale:=imsg.code
        IF gad.gadgetid=ADV_ZXDTPAL
            dither:=imsg.code
        ENDIF
      
        IF gad.gadgetid=ADV_SMOOTH
            smooth:=imsg.code
        ENDIF
      
      /* Advanced Options */
      
        IF gad.gadgetid=ADV_BRIGHT
           brthr:=imsg.code
          Gt_SetGadgetAttrsA(gads[ADV_BRIGHTNUM],wptr,NIL,[GTNM_NUMBER,imsg.code,NIL])
        ENDIF
        
        IF gad.gadgetid=ADV_COLOUR
            thr:=imsg.code
          Gt_SetGadgetAttrsA(gads[ADV_COLOURNUM],wptr,NIL,[GTNM_NUMBER,imsg.code,NIL])
        ENDIF
        
        IF gad.gadgetid=ADV_WHITE
            wtthr:=imsg.code
          Gt_SetGadgetAttrsA(gads[ADV_WHITENUM],wptr,NIL,[GTNM_NUMBER,imsg.code,NIL])
        ENDIF

        IF gad.gadgetid=ADV_BLUCYN
            blucyn:=imsg.code
          Gt_SetGadgetAttrsA(gads[ADV_BLUCYNNUM],wptr,NIL,[GTNM_NUMBER,imsg.code,NIL])
        ENDIF
        
        IF gad.gadgetid=ADV_GRNMAG
            grnmag:=imsg.code
          Gt_SetGadgetAttrsA(gads[ADV_GRNMAGNUM],wptr,NIL,[GTNM_NUMBER,imsg.code,NIL])
        ENDIF
        
        IF gad.gadgetid=ADV_REDYEL
            redyel:=imsg.code
          Gt_SetGadgetAttrsA(gads[ADV_REDYELNUM],wptr,NIL,[GTNM_NUMBER,imsg.code,NIL])
        ENDIF

        IF gad.gadgetid=ADV_FLASHRED
            flashred:=imsg.code
          Gt_SetGadgetAttrsA(gads[ADV_FLASHREDNUM],wptr,NIL,[GTNM_NUMBER,imsg.code,NIL])
->        SetRGB32(vp,flashcol,(flashred!/255),(flashgrn!/255),(flashblu!/255))
drawflashcolour()
        ENDIF

        IF gad.gadgetid=ADV_FLASHGRN
            flashgrn:=imsg.code
          Gt_SetGadgetAttrsA(gads[ADV_FLASHGRNNUM],wptr,NIL,[GTNM_NUMBER,imsg.code,NIL])
->        SetRGB32(vp,flashcol,(flashred/255)!,(flashgrn/255)!,(flashblu/255)!)
drawflashcolour()
        ENDIF

        IF gad.gadgetid=ADV_FLASHBLU
            flashblu:=imsg.code
          Gt_SetGadgetAttrsA(gads[ADV_FLASHBLUNUM],wptr,NIL,[GTNM_NUMBER,imsg.code,NIL])
->        SetRGB32(vp,flashcol,(flashred/255)!,(flashgrn/255)!,(flashblu/255)!)
drawflashcolour()
        ENDIF

      /**/
      
      CASE IDCMP_CLOSEWINDOW
/*
        IF EasyRequestArgs(0,[SIZEOF easystruct,0,'WHAT???!','Do you REALLY want to quit?','Yes|No']:easystruct,0,NIL) THEN 
*/
terminated:=TRUE
      CASE IDCMP_REFRESHWINDOW
        -> This handling is REQUIRED with GadTools.
        IF advopts
            DrawBevelBoxA(rp,6,((pixsep*6)+(mysc.font.ysize*5)+tbar+1),258,((pixsep*7)+(mysc.font.ysize*6)+2),
            [GTBB_RECESSED,TRUE,
             GTBB_FRAMETYPE,BBFT_RIDGE,
             GT_VISUALINFO,vi,
             NIL])
             
            DrawBevelBoxA(rp,227, ((pixsep*14)+(mysc.font.ysize*11)+tbar), 35, ((pixsep*2)+(mysc.font.ysize*3)+4),
            [GTBB_RECESSED,TRUE,
             GT_VISUALINFO,vi,
             NIL])

            drawflashcolour()
            
            ENDIF
            
        Gt_BeginRefresh(wptr)
        Gt_EndRefresh(wptr, TRUE)

      CASE IDCMP_MENUPICK
        SetWindowPointerA(wptr,[WA_BUSYPOINTER,TRUE,WA_POINTERDELAY,TRUE,NIL])
        menunumber:=UNSIGNED(imsg.code)
        WHILE(menunumber<>MENUNULL)
            item:=ItemAddress(menustrip,menunumber)
            menunum:=MENUNUM(menunumber)
            itemnum:=ITEMNUM(menunumber)
            subnum:=SUBNUM(menunumber)
            
->            WriteF('\d\n\d\n',menunum,itemnum)
            
            IF menunum=0 -> Project
              IF itemnum=0 THEN about()
              IF itemnum=2
/*                IF EasyRequestArgs(0,[SIZEOF easystruct,0,'WHAT???!','Do you REALLY want to quit?','Yes|No'],0,NIL) THEN */
                terminated:=TRUE
              ENDIF
            ENDIF
            IF menunum=1 -> Picture
                IF itemnum=0
                  askload()
                  identifyfile()
                  Gt_SetGadgetAttrsA(gads[GADG_LOADSTR],wptr,NIL,[GTTX_TEXT,FilePart(filename),NIL])
                  autoname(1)
                IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL) THEN Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,FALSE,NIL]) ELSE Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,TRUE,NIL])
                ENDIF

            IF itemnum=1
                     clipboard:=1
                     IF EstrLen(filename)=0 THEN StrCopy(filename,'Clipboard')
/*                     ELSE
                         MidStr(tmpstr,filename,0,EstrLen(filename)-EstrLen(FilePart(filename)))
                         StrCopy(filename,tmpstr)
                     ENDIF
*/
                     StrCopy(fname,'Clipboard')

                     StrCopy(filetype,'\nUnit: ')
                     StrAdd(filetype,RealF(tmpstr,clipunit!!FLOAT,0))
                     chg:=0
                  Gt_SetGadgetAttrsA(gads[GADG_LOADSTR],wptr,NIL,[GTTX_TEXT,'CLIPBOARD',NIL])
->                  autoname(1)
            ENDIF

            IF itemnum=2
                IF EstrLen(filename)<>NIL
                    StrCopy(tmpstr,'Run >NIL: SYS:Utilities/Multiview ')
                    IF clipboard=0
                        StrAdd(tmpstr,'"')
                        StrAdd(tmpstr,filename)
                        StrAdd(tmpstr,'"')
                    ELSE
                        StrAdd(tmpstr,'CLIPBOARD')
                    ENDIF
                    IF mvscreen THEN StrAdd(tmpstr,' SCREEN')
                    SystemTagList(tmpstr,NIL)
->                    Execute(tmpstr,0,0)
                ENDIF
            ENDIF

            IF itemnum=4
            asksave()
              Gt_SetGadgetAttrsA(gads[GADG_SAVESTR],wptr,NIL,[GTTX_TEXT,FilePart(scrfile),NIL])
            IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL) THEN Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,FALSE,NIL]) ELSE Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,TRUE,NIL])
            ENDIF

            IF itemnum=5
                IF EstrLen(scrfile)<>NIL
                    StrCopy(tmpstr,'Run >NIL: SYS:Utilities/Multiview ')
                    IF clipboard=0
                        StrAdd(tmpstr,'"')
                        StrAdd(tmpstr,scrfile)
                        StrAdd(tmpstr,'"')
                    ELSE
                        StrAdd(tmpstr,'CLIPBOARD')
                    ENDIF
                    IF mvscreen THEN StrAdd(tmpstr,' SCREEN')
                    SystemTagList(tmpstr,NIL)
->                    Execute(tmpstr,0,0)
                ENDIF
            ENDIF

              IF itemnum=7 THEN askoutput()

            ENDIF
            
                IF menunum=2 -> Prefs
                      IF itemnum=0
                          IF (item.flags AND CHECKED)
                              IF advopts=0 THEN advancedoptions()
                          ELSE
                              IF advopts THEN advancedoptions()
                          ENDIF
                      ENDIF
                      IF itemnum=1
                          IF (item.flags AND CHECKED) THEN mvscreen:=1 ELSE mvscreen:=0
                      ENDIF
                      IF itemnum=2
                          IF (item.flags AND CHECKED) THEN askover:=1 ELSE askover:=0
                      ENDIF
                      
                      IF itemnum=3
                          IF (item.flags AND CHECKED) THEN nofilter:=0 ELSE nofilter:=1
                      ENDIF
                      
                IF itemnum=4
                          IF (item.flags AND CHECKED) THEN autonaming:=1 ELSE autonaming:=0
                      ENDIF

                      IF itemnum=5
                          IF (item.flags AND CHECKED) THEN noprogress:=0 ELSE noprogress:=1
                      ENDIF
                      
                      IF itemnum=7
                          IF subnum=1
                              IF (item.flags AND CHECKED) THEN allelseink:=1 ELSE allelseink:=0
                          ENDIF
                          IF subnum=2
                              IF (item.flags AND CHECKED) THEN allelseink:=0 ELSE allelseink:=1
                          ENDIF
                      ENDIF
                      
                IF itemnum=8
                          IF (item.flags AND CHECKED) THEN saveattr:=1 ELSE saveattr:=0
                ENDIF
                
                IF itemnum=11
                          IF (item.flags AND CHECKED) THEN simpleremap:=1 ELSE simpleremap:=0
                ENDIF

                      IF itemnum=10
                          IF subnum=0
                              IF (item.flags AND CHECKED) THEN dtype:=0 ELSE dtype:=1
                          ENDIF
                          IF subnum=1
                              IF (item.flags AND CHECKED) THEN dtype:=1 ELSE dtype:=0
                          ENDIF
                      ENDIF


                IF itemnum=9
                          IF (item.flags AND CHECKED) THEN rompal:=0 ELSE rompal:=1
                           os3setup()
                ENDIF

                IF itemnum=12
                          IF (item.flags AND CHECKED) THEN remapafterdither:=1 ELSE remapafterdither:=0
                ENDIF


                      IF itemnum=14
                          IF subnum=0
                        IF (item.flags AND CHECKED) THEN tzxarray[0]:=1 ELSE tzxarray[0]:=0
                    ENDIF
                          IF subnum=2
                        IF (item.flags AND CHECKED)
                            tzxarray[1]:=1
                            tzxarray[2]:=0
                        ELSE
                            tzxarray[1]:=0
                            tzxarray[2]:=1
                        ENDIF
                    ENDIF
                          IF subnum=3
                        IF (item.flags AND CHECKED)
                            tzxarray[2]:=1
                            tzxarray[1]:=0
                        ELSE
                            tzxarray[2]:=0
                            tzxarray[1]:=1
                        ENDIF
                    ENDIF
                      ENDIF
                      
                      IF itemnum=15
                          IF (item.flags AND CHECKED) THEN tzxborder:=subnum
                      ENDIF
                      
                      IF itemnum=16
                    IF (item.flags AND CHECKED) THEN writeheader:=1 ELSE writeheader:=0
                ENDIF
                      
                      IF itemnum=18
                    IF (item.flags AND CHECKED) THEN snapshotwindows:=1 ELSE snapshotwindows:=0
                ENDIF
                      
                IF itemnum=19 THEN saveconfig()
            ENDIF
            
            IF menunum=3 -> Help
        
              IF itemnum=0
                IF saveformat=0
                    IF gifsave
                        EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','GIF\n\nThis saver is a bit special: it\nuses SCR2GIF to output a Spectrum\ntype image, but suitable for\nweb pages.\n\nSize: <6912\n\nUsed by: WWW and a decreasing\namount of other programs.','OK']:easystruct,0,NIL)
                    ELSE
                        EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','SCR\n\nThis is simply the raw data taken\nfrom the Spectrum\'s memory at\naddress 16384.\n\nSize: 6912\n\nUsed by: ZX Datatype, ZXAM','OK']:easystruct,0,NIL)
                    ENDIF
                ENDIF
                  IF saveformat=1 THEN EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','ZX82\n\nThis format is really a disk-based\nformat, as it does not contain\nan original Spectrum tape header.\n\nSize: 6924\n\nUsed by: Speculator, xfs, ZXMIT\n\nMore information available from\nSpeculator documentation','OK']:easystruct,0,NIL)
                  IF saveformat=2 THEN EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','BYTES (.header/.bytes)\n\nThese are nothing other than\nindividual blocks of data as a\nSpectrum would save to tape.\n\nSize: 19 (.header), 6914 (.bytes)\n\nUsed by: Peter McGavin\'s Spectrum, ZXAM','OK']:easystruct,0,NIL)
                     IF saveformat=3 THEN EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','TAP (Z80 Tape Image)\n\nTAPs are tape files which can store\nseveral data blocks in one file.\n\nSize: 6937\n\nUsed by: ZXAM, ASp','OK']:easystruct,0,NIL)
                  IF saveformat=5 THEN EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','+3DOS\n\nThis is a +3DOS file, intended for\nuse on +3 disks\n\nSize: 7040\n\nUsed by: +3','OK']:easystruct,0,NIL)
                  IF saveformat=4
                      StrCopy(tmpstr,'TZX (ZX Tape)\n\nThis is a "digital tape" format, which\nsupports several different types of\nblock including turbo loaders.\n\nSize: ')
                      IF tzxarray[1] THEN StrAdd(tmpstr,'6953')
                      IF tzxarray[2] THEN StrAdd(tmpstr,'6976')
                      IF tzxarray[0] THEN StrAdd(tmpstr,' + archive info (varies)')
                      StrAdd(tmpstr,'\n\nUsed by: TZX -> TAP convertors :-)\n\nMore information available from WOS at\nhttp://www.void.demon.nl/spectrum.html')
                      EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC',tmpstr,'OK']:easystruct,0,NIL)
                  ENDIF
            ENDIF
        
              IF itemnum=1
/*
                IF alsbase
                    Als(0)
                ELSE
                    EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','Could not open als.library 6+','OK']:easystruct,0,NIL)
                ENDIF
*/
              ENDIF
        ENDIF

            IF menunum=4 -> ARexx
                IF itemnum=0 THEN arexxecute()
            ENDIF

            menunumber:=UNSIGNED(item.nextselect)
            ENDWHILE
        SetWindowPointerA(wptr,[NIL])
      ENDSELECT
      -> Use the toolkit message-replying function here...
      Gt_ReplyIMsg(imsg)
    ENDWHILE
    
ENDIF

IF signal AND appwinsig
    WHILE appwinmsg:=GetMsg(appwinport)
        argptr:=appwinmsg.arglist
        /* only interested in first msg */
->        WriteF('\s\n',argptr.name)
        SetWindowPointerA(wptr,[WA_BUSYPOINTER,TRUE,WA_POINTERDELAY,TRUE,NIL])
        NameFromLock(argptr.lock,filebuf,1024)
        StrCopy(filename,filebuf)
        StrCopy(loaddrw,filename)
          AddPart(filename,argptr.name,1024)
          StrCopy(fname,argptr.name)
        ReplyMsg(appwinmsg)
          identifyfile()
          Gt_SetGadgetAttrsA(gads[GADG_LOADSTR],wptr,NIL,[GTTX_TEXT,FilePart(filename),NIL])
        clipboard:=0
          autoname(1)
        IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL) THEN Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,FALSE,NIL]) ELSE Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,TRUE,NIL])
        SetWindowPointerA(wptr,[NIL])
    ENDWHILE
ENDIF


IF arexxsig<>NIL
    IF signal AND arexxsig
        REPEAT
            SetWindowPointerA(wptr,[WA_BUSYPOINTER,TRUE,WA_POINTERDELAY,TRUE,NIL])
            arexxmsg,rexxcmd:=rx_GetMsg(arexxport)
            rc:=0
            StrCopy(result,'')
            IF arexxmsg<>NIL
                
                UpperStr(rexxcmd)
                IF StrCmp(rexxcmd,'HELLO')
                    EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','Hello!','OK']:easystruct,0,NIL)
                    StrCopy(result,'© 1998-2001,2008 Unsatisfactory Software')
                ENDIF

                IF StrCmp(rexxcmd,'QUICKCONVERT',12)
                clipboard:=0
                dot:=InStr(rexxcmd,'"',1)+1
                quote:=InStr(rexxcmd,'"',(dot+1))
                MidStr(filename,rexxcmd,dot,(quote-dot))
                dot:=InStr(rexxcmd,'"',quote+1)+1
                quote:=InStr(rexxcmd,'"',(dot+1))
                MidStr(scrfile,rexxcmd,dot,quote-dot)
                  Gt_SetGadgetAttrsA(gads[GADG_LOADSTR],wptr,NIL,[GTTX_TEXT,FilePart(filename),NIL])
                  Gt_SetGadgetAttrsA(gads[GADG_SAVESTR],wptr,NIL,[GTTX_TEXT,FilePart(scrfile),NIL])
                IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL) THEN Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,FALSE,NIL]) ELSE Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,TRUE,NIL])
                rc:=1
                IF identifyfile()
                    rc:=2
                       IF dt2ppm(scale)
                           rc:=0
                           ppmtoscr()
                       ENDIF
                   ENDIF
            ENDIF
            
            IF StrCmp(rexxcmd,'SELECTSOURCE',12)
                dot:=InStr(rexxcmd,'"',1)+1
                quote:=InStr(rexxcmd,'"',(dot+1))
                IF quote=-1
                    askload()
                ELSE
                    clipboard:=0
                    MidStr(filename,rexxcmd,dot,(quote-dot))
                ENDIF
                
                identifyfile()
                  Gt_SetGadgetAttrsA(gads[GADG_LOADSTR],wptr,NIL,[GTTX_TEXT,FilePart(filename),NIL])
                  autoname(1)
                IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL) THEN Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,FALSE,NIL]) ELSE Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,TRUE,NIL])
                StrCopy(result,filename)
                IF EstrLen(filename)=0 THEN rc:=1
            ENDIF
            
            IF StrCmp(rexxcmd,'SELECTDEST',10)
                dot:=InStr(rexxcmd,'"',1)+1
                quote:=InStr(rexxcmd,'"',(dot+1))
                IF dot<>0
                    MidStr(filename,rexxcmd,dot,(quote-dot))
                ELSE
                    asksave()
                ENDIF
                  Gt_SetGadgetAttrsA(gads[GADG_LOADSTR],wptr,NIL,[GTTX_TEXT,FilePart(filename),NIL])
                IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL) THEN Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,FALSE,NIL]) ELSE Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,TRUE,NIL])
                StrCopy(result,scrfile)
                IF EstrLen(scrfile)=0 THEN rc:=1
            ENDIF

            IF StrCmp(rexxcmd,'CONVERT')
                rc:=1
                IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL)
                    rc:=2
                    identifyfile()
                       IF dt2ppm(scale)
                           rc:=0
                           ppmtoscr()
                       ENDIF
                   ENDIF
               ENDIF
               
               IF StrCmp(rexxcmd,'SCALE')
                   scale:=1
                Gt_SetGadgetAttrsA(gads[GADG_SCALE],wptr,NIL,[GTCB_CHECKED,TRUE])
               ENDIF

               IF StrCmp(rexxcmd,'NOSCALE')
                   scale:=0
                Gt_SetGadgetAttrsA(gads[GADG_SCALE],wptr,NIL,[GTCB_CHECKED,FALSE])
               ENDIF

               IF StrCmp(rexxcmd,'ROMREMAP')
                   os3:=1
                Gt_SetGadgetAttrsA(gads[GADG_ROMREMAP],wptr,NIL,[GTCB_CHECKED,TRUE])
               ENDIF

               IF StrCmp(rexxcmd,'STDREMAP')
                   os3:=0
                Gt_SetGadgetAttrsA(gads[GADG_ROMREMAP],wptr,NIL,[GTCB_CHECKED,FALSE])
               ENDIF
               
                  IF StrCmp(rexxcmd,'ALTPALETTE')
                   rompal:=1
                Gt_SetGadgetAttrsA(gads[ADV_ZXDTPAL],wptr,NIL,[GTCB_CHECKED,FALSE])
                os3setup()
               ENDIF

               IF StrCmp(rexxcmd,'ZXDTPALETTE')
                   rompal:=0
->                Gt_SetGadgetAttrsA(gads[ADV_ZXDTPAL],wptr,NIL,[GTCB_CHECKED,TRUE])
               ENDIF

               IF StrCmp(rexxcmd,'DITHER')
                   dither:=1
                Gt_SetGadgetAttrsA(gads[ADV_ZXDTPAL],wptr,NIL,[GTCB_CHECKED,TRUE])
               ENDIF

               IF StrCmp(rexxcmd,'SMOOTH')
                   smooth:=1
                Gt_SetGadgetAttrsA(gads[ADV_SMOOTH],wptr,NIL,[GTCB_CHECKED,TRUE])
               ENDIF

               IF StrCmp(rexxcmd,'NOSMOOTH')
                   smooth:=0
                Gt_SetGadgetAttrsA(gads[ADV_SMOOTH],wptr,NIL,[GTCB_CHECKED,FALSE])
               ENDIF

            IF StrCmp(rexxcmd,'BRIGHTSENSE',11)
                MidStr(tmpstr,rexxcmd,12,ALL)
                brthr:=Val(tmpstr)
                Gt_SetGadgetAttrsA(gads[ADV_BRIGHT],wptr,NIL,[GTSL_LEVEL,brthr])
                Gt_SetGadgetAttrsA(gads[ADV_BRIGHTNUM],wptr,NIL,[GTNM_NUMBER,brthr,NIL])
            ENDIF

            IF StrCmp(rexxcmd,'COLOURSENSE',11)
                MidStr(tmpstr,rexxcmd,12,ALL)
                thr:=Val(tmpstr)
                Gt_SetGadgetAttrsA(gads[ADV_COLOUR],wptr,NIL,[GTSL_LEVEL,thr])
                Gt_SetGadgetAttrsA(gads[ADV_COLOURNUM],wptr,NIL,[GTNM_NUMBER,thr,NIL])
            ENDIF

            IF StrCmp(rexxcmd,'WHITESENSE',10)
                MidStr(tmpstr,rexxcmd,11,ALL)
                wtthr:=Val(tmpstr)
                Gt_SetGadgetAttrsA(gads[ADV_WHITE],wptr,NIL,[GTSL_LEVEL,wtthr])
                Gt_SetGadgetAttrsA(gads[ADV_WHITENUM],wptr,NIL,[GTNM_NUMBER,wtthr,NIL])
            ENDIF
            
            IF StrCmp(rexxcmd,'REDYELLOW',9)
                MidStr(tmpstr,rexxcmd,10,ALL)
                redyel:=Val(tmpstr)
                Gt_SetGadgetAttrsA(gads[ADV_REDYEL],wptr,NIL,[GTSL_LEVEL,redyel])
                Gt_SetGadgetAttrsA(gads[ADV_REDYELNUM],wptr,NIL,[GTNM_NUMBER,redyel,NIL])
            ENDIF

            IF StrCmp(rexxcmd,'BLUECYAN',8)
                MidStr(tmpstr,rexxcmd,9,ALL)
                blucyn:=Val(tmpstr)
                Gt_SetGadgetAttrsA(gads[ADV_BLUCYN],wptr,NIL,[GTSL_LEVEL,blucyn])
                Gt_SetGadgetAttrsA(gads[ADV_BLUCYNNUM],wptr,NIL,[GTNM_NUMBER,blucyn,NIL])
            ENDIF

            IF StrCmp(rexxcmd,'GREENMAGENTA',12)
                MidStr(tmpstr,rexxcmd,13,ALL)
                grnmag:=Val(tmpstr)
                Gt_SetGadgetAttrsA(gads[ADV_GRNMAG],wptr,NIL,[GTSL_LEVEL,grnmag])
                Gt_SetGadgetAttrsA(gads[ADV_GRNMAGNUM],wptr,NIL,[GTNM_NUMBER,grnmag,NIL])
            ENDIF

            IF StrCmp(rexxcmd,'FLASHRED',8)
                MidStr(tmpstr,rexxcmd,9,ALL)
                flashred:=Val(tmpstr)
                Gt_SetGadgetAttrsA(gads[ADV_FLASHRED],wptr,NIL,[GTSL_LEVEL,flashred])
                Gt_SetGadgetAttrsA(gads[ADV_FLASHREDNUM],wptr,NIL,[GTNM_NUMBER,flashred,NIL])
drawflashcolour()
            ENDIF

            IF StrCmp(rexxcmd,'FLASHGREEN',10)
                MidStr(tmpstr,rexxcmd,11,ALL)
                flashgrn:=Val(tmpstr)
                Gt_SetGadgetAttrsA(gads[ADV_FLASHGRN],wptr,NIL,[GTSL_LEVEL,flashgrn])
                Gt_SetGadgetAttrsA(gads[ADV_FLASHGRNNUM],wptr,NIL,[GTNM_NUMBER,flashgrn,NIL])
drawflashcolour()
            ENDIF

            IF StrCmp(rexxcmd,'FLASHBLUE',9)
                MidStr(tmpstr,rexxcmd,10,ALL)
                flashblu:=Val(tmpstr)
                Gt_SetGadgetAttrsA(gads[ADV_FLASHBLU],wptr,NIL,[GTSL_LEVEL,flashblu])
                Gt_SetGadgetAttrsA(gads[ADV_FLASHBLUNUM],wptr,NIL,[GTNM_NUMBER,flashblu,NIL])
drawflashcolour()
            ENDIF

            IF StrCmp(rexxcmd,'VERSION')
                StrCopy(result,'3.2')
            ENDIF
            
            IF StrCmp(rexxcmd,'SAVEFORMAT',10)
                MidStr(tmpstr,rexxcmd,11,ALL)
                saveformat:=Val(tmpstr)
                Gt_SetGadgetAttrsA(gads[GADG_TYPE],wptr,NIL,[GTCY_ACTIVE,saveformat])
                IF saveformat=6
                    saveformat:=0
                    gifsave:=1
                ELSE
                    gifsave:=0
                ENDIF
            ENDIF

            IF StrCmp(rexxcmd,'GETSAVEFORMAT')
                IF saveformat=0
                    IF gifsave
                        StrCopy(result,'GIF')
                    ELSE
                        StrCopy(result,'SCR')
                    ENDIF
                ENDIF
                IF saveformat=1 THEN StrCopy(result,'ZX82')
                IF saveformat=2 THEN StrCopy(result,'BYTES')
                IF saveformat=3 THEN StrCopy(result,'TAP')
                IF saveformat=4 THEN StrCopy(result,'TZX')
                IF saveformat=5 THEN StrCopy(result,'+3DOS')
            ENDIF
            
            IF StrCmp(rexxcmd,'GETSOURCE')
                StrCopy(result,filename)
            ENDIF
            
            IF StrCmp(rexxcmd,'GETDEST')
                StrCopy(result,scrfile)
            ENDIF
            
            IF StrCmp(rexxcmd,'GETBRIGHT')
                StrCopy(result,RealF(tmpstr,brthr!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETCOLOUR')
                StrCopy(result,RealF(tmpstr,thr!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETWHITE')
                StrCopy(result,RealF(tmpstr,wtthr!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETREDYEL')
                StrCopy(result,RealF(tmpstr,redyel!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETBLUCYN')
                StrCopy(result,RealF(tmpstr,blucyn!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETGRNMAG')
                StrCopy(result,RealF(tmpstr,grnmag!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETFLASHRED')
                StrCopy(result,RealF(tmpstr,flashred!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETFLASHGRN')
                StrCopy(result,RealF(tmpstr,flashgrn!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETFLASHBLU')
                StrCopy(result,RealF(tmpstr,flashblu!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETSCALE')
                StrCopy(result,RealF(tmpstr,scale!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETSMOOTH')
                StrCopy(result,RealF(tmpstr,smooth!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETDITHER')
                StrCopy(result,RealF(tmpstr,dither!!FLOAT,0))
            ENDIF

            IF StrCmp(rexxcmd,'GETALTPAL')
                StrCopy(result,RealF(tmpstr,rompal!!FLOAT,0))
            ENDIF
            
            IF StrCmp(rexxcmd,'GETROMREMAP')
                StrCopy(result,RealF(tmpstr,os3!!FLOAT,0))
            ENDIF
            
               IF StrCmp(rexxcmd,'QUIT')
                   terminated:=TRUE
               ENDIF

            -> WriteF('ARexx: \s\n',rexxcmd)
            rx_ReplyMsg(arexxmsg,rc,result)
        ENDIF
        SetWindowPointerA(wptr,[NIL])
    UNTIL arexxmsg=NIL
ENDIF
ENDIF

  UNTIL terminated

ENDPROC

PROC advancedoptions()
IF wptr.height=(tbar+2) THEN ZipWindow(wptr)

IF advopts
/*    SizeWindow(wptr,0,-((pixsep*8)+(mysc.font.ysize*6))) *//* 240,0 */
    ChangeWindowBox(wptr,wptr.leftedge,wptr.topedge,270,((pixsep*6)+(mysc.font.ysize*5)+tbar+3))
    RemoveGList(wptr,gads[ADV_BRIGHTNUM],-1)
    advopts:=0
ELSE
/*
    IF mysc.height-(wptr.topedge+wptr.height) < ((pixsep*8)+(mysc.font.ysize*6))
        MoveWindow(wptr,0,-((pixsep*8)+(mysc.font.ysize*6)-(mysc.height-(wptr.topedge+wptr.height))))
    ENDIF

    SizeWindow(wptr,0,((pixsep*8)+(mysc.font.ysize*6))) /* 240,0 */
*/

->    ChangeWindowBox(wptr,wptr.leftedge,wptr.topedge,270,((pixsep*14)+(mysc.font.ysize*11)+tbar+2))
    ChangeWindowBox(wptr,wptr.leftedge,wptr.topedge,270,((pixsep*17)+(mysc.font.ysize*14)+tbar+4))
advgadgets()
    AddGList(wptr,gads[ADV_BRIGHTNUM],-1,-1,NIL)
    RefreshGList(gads[ADV_BRIGHTNUM],wptr,NIL,-1)
    Gt_RefreshWindow(wptr,NIL)
    advopts:=1
ENDIF

ENDPROC

PROC advgadgets()
DEF gad,top

top:=((pixsep*6)+(mysc.font.ysize*5))

->  mysc:=LockPubScreen(NIL)
->  tbar:=mysc.font.ysize+mysc.wbortop-1 /* true value is +2 */

->  vi:=GetVisualInfoA(mysc, [NIL])
  -> GadTools gadgets require this step to be taken
  gad:=CreateContext(advglist)

  gads[ADV_BRIGHTNUM]:=CreateGadgetA(NUMBER_KIND, gad,
                    [215, (pixsep+tbar+top),
                     40, (mysc.font.ysize+4),
                     '', mysc.font,
                     ADV_BRIGHTNUM, 0,
                     vi, NIL]:newgadget,
                    [GTNM_NUMBER,brthr,GTNM_BORDER,TRUE,NIL])

  gads[ADV_COLOURNUM]:=CreateGadgetA(NUMBER_KIND, gads[ADV_BRIGHTNUM],
                    [215, ((pixsep*2)+(mysc.font.ysize)+tbar+top),
                     40, (mysc.font.ysize+4),
                     '', mysc.font,
                     ADV_COLOURNUM, 0,
                     vi, NIL]:newgadget,
                    [GTNM_NUMBER,thr,GTNM_BORDER,TRUE,NIL])

  gads[ADV_WHITENUM]:=CreateGadgetA(NUMBER_KIND, gads[ADV_COLOURNUM],
                    [215, ((pixsep*3)+(mysc.font.ysize*2)+tbar+top),
                     40, (mysc.font.ysize+4),
                     '', mysc.font,
                     ADV_WHITENUM, 0,
                     vi, NIL]:newgadget,
                    [GTNM_NUMBER,wtthr,GTNM_BORDER,TRUE,NIL])

  gads[ADV_BRIGHT]:=CreateGadgetA(SLIDER_KIND, gads[ADV_WHITENUM],
                    [85, (pixsep+tbar+top),
                     130, (mysc.font.ysize+4),
                     'Bright:', mysc.font,
                     ADV_BRIGHT, 0,
                     vi, NIL]:newgadget,
                    [GA_DISABLED,(os3),GA_RELVERIFY,TRUE,GTSL_MIN,0,GTSL_MAX,765,GTSL_LEVEL,brthr,GTSL_MAXLEVELLEN,3,GTSL_LEVELPLACE,PLACETEXT_RIGHT,NIL]) /*8*/

  gads[ADV_COLOUR]:=CreateGadgetA(SLIDER_KIND, gads[ADV_BRIGHT],
                    [85, ((pixsep*2)+(mysc.font.ysize)+tbar+top),
                     130, (mysc.font.ysize+4),
                     'Colour:', mysc.font,
                     ADV_COLOUR, 0,
                     vi, NIL]:newgadget,
                    [GA_DISABLED,(os3),GA_RELVERIFY,TRUE,GTSL_MIN,0,GTSL_MAX,255,GTSL_LEVEL,thr,GTSL_MAXLEVELLEN,3,GTSL_LEVELPLACE,PLACETEXT_RIGHT,NIL])

  gads[ADV_WHITE]:=CreateGadgetA(SLIDER_KIND, gads[ADV_COLOUR],
                    [85, ((pixsep*3)+(mysc.font.ysize*2)+tbar+top),
                     130, (mysc.font.ysize+4),
                     'White:', mysc.font,
                     ADV_WHITE, 0,
                     vi, NIL]:newgadget,
                    [GA_DISABLED,(os3),GA_RELVERIFY,TRUE,GTSL_MIN,0,GTSL_MAX,765,GTSL_LEVEL,wtthr,GTSL_MAXLEVELLEN,3,GTSL_LEVELPLACE,PLACETEXT_RIGHT,NIL])

  gads[ADV_REDYEL]:=CreateGadgetA(SLIDER_KIND, gads[ADV_WHITE],
                    [85, ((pixsep*4)+(mysc.font.ysize*3)+tbar+top),
                     130, (mysc.font.ysize+4),
                     'Red/Yel:', mysc.font,
                     ADV_REDYEL, 0,
                     vi, NIL]:newgadget,
                    [GA_DISABLED,(os3),GA_RELVERIFY,TRUE,GTSL_MIN,0,GTSL_MAX,255,GTSL_LEVEL,redyel,GTSL_MAXLEVELLEN,3,GTSL_LEVELPLACE,PLACETEXT_RIGHT,NIL]) /*8*/

  gads[ADV_BLUCYN]:=CreateGadgetA(SLIDER_KIND, gads[ADV_REDYEL],
                    [85, ((pixsep*5)+(mysc.font.ysize*4)+tbar+top),
                     130, (mysc.font.ysize+4),
                     'Blu/Cyn:', mysc.font,
                     ADV_BLUCYN, 0,
                     vi, NIL]:newgadget,
                    [GA_DISABLED,(os3),GA_RELVERIFY,TRUE,GTSL_MIN,0,GTSL_MAX,255,GTSL_LEVEL,blucyn,GTSL_MAXLEVELLEN,3,GTSL_LEVELPLACE,PLACETEXT_RIGHT,NIL])

  gads[ADV_GRNMAG]:=CreateGadgetA(SLIDER_KIND, gads[ADV_BLUCYN],
                    [85, ((pixsep*6)+(mysc.font.ysize*5)+tbar+top),
                     130, (mysc.font.ysize+4),
                     'Grn/Mag:', mysc.font,
                     ADV_GRNMAG, 0,
                     vi, NIL]:newgadget,
                    [GA_DISABLED,(os3),GA_RELVERIFY,TRUE,GTSL_MIN,0,GTSL_MAX,255,GTSL_LEVEL,grnmag,GTSL_MAXLEVELLEN,3,GTSL_LEVELPLACE,PLACETEXT_RIGHT,NIL])

  gads[ADV_REDYELNUM]:=CreateGadgetA(NUMBER_KIND, gads[ADV_GRNMAG],
                    [215, ((pixsep*4)+(mysc.font.ysize*3)+tbar+top),
                     40, (mysc.font.ysize+4),
                     '', mysc.font,
                     ADV_REDYELNUM, 0,
                     vi, NIL]:newgadget,
                    [GTNM_NUMBER,redyel,GTNM_BORDER,TRUE,NIL])

  gads[ADV_BLUCYNNUM]:=CreateGadgetA(NUMBER_KIND, gads[ADV_REDYELNUM],
                    [215, ((pixsep*5)+(mysc.font.ysize*4)+tbar+top),
                     40, (mysc.font.ysize+4),
                     '', mysc.font,
                     ADV_BLUCYNNUM, 0,
                     vi, NIL]:newgadget,
                    [GTNM_NUMBER,blucyn,GTNM_BORDER,TRUE,NIL])

  gads[ADV_GRNMAGNUM]:=CreateGadgetA(NUMBER_KIND, gads[ADV_BLUCYNNUM],
                    [215, ((pixsep*6)+(mysc.font.ysize*5)+tbar+top),
                     40, (mysc.font.ysize+4),
                     '', mysc.font,
                     ADV_GRNMAGNUM, 0,
                     vi, NIL]:newgadget,
                    [GTNM_NUMBER,grnmag,GTNM_BORDER,TRUE,NIL])

-> IF mysc THEN UnlockPubScreen(mysc,NIL)

  gads[ADV_FLASHRED]:=CreateGadgetA(SLIDER_KIND, gads[ADV_GRNMAGNUM],
                    [85, ((pixsep*8)+(mysc.font.ysize*6)+tbar+top),
                     100, (mysc.font.ysize+4),
                     'Flash: Red', mysc.font,
                     ADV_FLASHRED, 0,
                     vi, NIL]:newgadget,
                    [GA_RELVERIFY,TRUE,GTSL_MIN,-1,GTSL_MAX,255,GTSL_LEVEL,flashred,GTSL_MAXLEVELLEN,3,GTSL_LEVELPLACE,PLACETEXT_RIGHT,NIL]) /*8*/

  gads[ADV_FLASHGRN]:=CreateGadgetA(SLIDER_KIND, gads[ADV_FLASHRED],
                    [85, ((pixsep*9)+(mysc.font.ysize*7)+tbar+top),
                     100, (mysc.font.ysize+4),
                     'Green', mysc.font,
                     ADV_FLASHGRN, 0,
                     vi, NIL]:newgadget,
                    [GA_RELVERIFY,TRUE,GTSL_MIN,-1,GTSL_MAX,255,GTSL_LEVEL,flashgrn,GTSL_MAXLEVELLEN,3,GTSL_LEVELPLACE,PLACETEXT_RIGHT,NIL])

  gads[ADV_FLASHBLU]:=CreateGadgetA(SLIDER_KIND, gads[ADV_FLASHGRN],
                    [85, ((pixsep*10)+(mysc.font.ysize*8)+tbar+top),
                     100, (mysc.font.ysize+4),
                     'Blue', mysc.font,
                     ADV_FLASHBLU, 0,
                     vi, NIL]:newgadget,
                    [GA_RELVERIFY,TRUE,GTSL_MIN,-1,GTSL_MAX,255,GTSL_LEVEL,flashblu,GTSL_MAXLEVELLEN,3,GTSL_LEVELPLACE,PLACETEXT_RIGHT,NIL])

  gads[ADV_FLASHREDNUM]:=CreateGadgetA(NUMBER_KIND, gads[ADV_FLASHBLU],
                    [185, ((pixsep*8)+(mysc.font.ysize*6)+tbar+top),
                     40, (mysc.font.ysize+4),
                     '', mysc.font,
                     ADV_FLASHREDNUM, 0,
                     vi, NIL]:newgadget,
                    [GTNM_NUMBER,flashred,GTNM_BORDER,TRUE,NIL])

  gads[ADV_FLASHGRNNUM]:=CreateGadgetA(NUMBER_KIND, gads[ADV_FLASHREDNUM],
                    [185, ((pixsep*9)+(mysc.font.ysize*7)+tbar+top),
                     40, (mysc.font.ysize+4),
                     '', mysc.font,
                     ADV_FLASHGRNNUM, 0,
                     vi, NIL]:newgadget,
                    [GTNM_NUMBER,flashgrn,GTNM_BORDER,TRUE,NIL])

  gads[ADV_FLASHBLUNUM]:=CreateGadgetA(NUMBER_KIND, gads[ADV_FLASHGRNNUM],
                    [185, ((pixsep*10)+(mysc.font.ysize*8)+tbar+top),
                     40, (mysc.font.ysize+4),
                     '', mysc.font,
                     ADV_FLASHBLUNUM, 0,
                     vi, NIL]:newgadget,
                    [GTNM_NUMBER,flashblu,GTNM_BORDER,TRUE,NIL])

/*
  gads[ADV_FLASHCOL]:=CreateGadgetA(PALETTE_KIND, gads[ADV_FLASHBLUNUM],
                    [220, ((pixsep*7)+(mysc.font.ysize*6)+tbar+top),
                     30, ((mysc.font.ysize+4)*3)+(pixsep*2),
                     '', mysc.font,
                     ADV_FLASHBLUNUM, 0,
                     vi, NIL]:newgadget,
                    [GTPA_COLOR,flashcol,GTPA_COLOROFFSET,flashcol,GTPA_NUMCOLORS,1,NIL])
*/

ENDPROC

PROC saveconfig()
DEF olddir=-1,oldtooltypes,newtooltypes[40]:ARRAY OF LONG,tmpstr2[100]:STRING
DEF ttstr1[20]:STRING
DEF ttstr2[20]:STRING
DEF ttstr3[20]:STRING
DEF ttstr4[20]:STRING
DEF ttstr5[20]:STRING
DEF ttstr6[20]:STRING
DEF ttstr7[20]:STRING
DEF ttstr8[20]:STRING
DEF ttstr9[20]:STRING
DEF ttstr10[20]:STRING
DEF ttstr11[30]:STRING
DEF ttstr12[30]:STRING
DEF ttstr13[30]:STRING
DEF ttstr14[20]:STRING
DEF ttstr15[20]:STRING
DEF ttstr16[20]:STRING
DEF ttstr17[20]:STRING
DEF ttstr18[20]:STRING
DEF ttstr19[20]:STRING
DEF ttstr20[20]:STRING
DEF ttstr21[20]:STRING
DEF ttstr22[20]:STRING
DEF ttstr23[20]:STRING
DEF ttstr24[20]:STRING
DEF ttstr25[20]:STRING
DEF ttstr26[20]:STRING
DEF ttstr27[20]:STRING
DEF ttstr28[20]:STRING

-> wbenchMsg:=wbmessage
-> wbarg:=wbenchMsg.arglist

IF (wbarg.lock) AND (wbarg.name<>0) THEN olddir:=CurrentDir(wbarg.lock)

IF dobj:=GetDiskObject(wbarg.name)

oldtooltypes:=dobj.tooltypes

StringF(ttstr16,'COLOURSENSE=\d',thr)
newtooltypes[0]:=ttstr16
StringF(ttstr17,'BRIGHTSENSE=\d',brthr)
newtooltypes[1]:=ttstr17
StringF(ttstr18,'WHITESENSE=\d',wtthr)
newtooltypes[2]:=ttstr18
StringF(ttstr19,'GREENMAGENTA=\d',grnmag)
newtooltypes[3]:=ttstr19
StringF(ttstr20,'BLUECYAN=\d',blucyn)
newtooltypes[4]:=ttstr20
StringF(ttstr21,'REDYELLOW=\d',redyel)
newtooltypes[5]:=ttstr21
StringF(ttstr22,'FLASHRED=\d',flashred)
newtooltypes[6]:=ttstr22
StringF(ttstr23,'FLASHGRN=\d',flashgrn)
newtooltypes[7]:=ttstr23
StringF(ttstr24,'FLASHBLU=\d',flashblu)
newtooltypes[8]:=ttstr24

/* greyscale, nobright not needed */
IF smooth=0 THEN newtooltypes[9]:='NOSMOOTH' ELSE newtooltypes[9]:='(NOSMOOTH)'
IF scale=0 THEN newtooltypes[10]:='NOSCALE' ELSE newtooltypes[10]:='(NOSCALE)'
IF os3 THEN newtooltypes[11]:='ROMREMAP' ELSE newtooltypes[11]:='(ROMREMAP)'
IF rompal=1 THEN newtooltypes[12]:='ALTPALETTE' ELSE newtooltypes[12]:='(ALTPALETTE)'
IF saveformat=0
    IF gifsave
        newtooltypes[13]:='SAVEFORMAT=GIF'
    ELSE
        newtooltypes[13]:='SAVEFORMAT=SCR'
    ENDIF
ENDIF
IF saveformat=1 THEN newtooltypes[13]:='SAVEFORMAT=ZX82'
IF saveformat=2 THEN newtooltypes[13]:='SAVEFORMAT=BYTES'
IF saveformat=3 THEN newtooltypes[13]:='SAVEFORMAT=TAP'
IF saveformat=4 THEN newtooltypes[13]:='SAVEFORMAT=TZX'
IF saveformat=5 THEN newtooltypes[13]:='SAVEFORMAT=PLUS3'
IF mvscreen THEN newtooltypes[14]:='SCREEN' ELSE newtooltypes[14]:='(SCREEN)'
IF advopts THEN newtooltypes[15]:='ADVANCED' ELSE newtooltypes[15]:='(ADVANCED)'
StringF(ttstr25,'CLIPUNIT=\d',clipunit)
newtooltypes[16]:=ttstr25
IF askover THEN newtooltypes[17]:='WARNOVERWRITE' ELSE newtooltypes[17]:='(WARNOVERWRITE)'

StrCopy(ttstr11,'TZXBLOCKS=')
IF tzxarray[0] THEN StrAdd(ttstr11,'INFO|')
IF tzxarray[1] THEN StrAdd(ttstr11,'STANDARD') ELSE StrAdd(ttstr11,'CUSTOM')
newtooltypes[18]:=ttstr11

IF writeheader THEN newtooltypes[19]:='(NOHEADER)' ELSE newtooltypes[19]:='NOHEADER'

StringF(ttstr26,'BORDER=\d',tzxborder)
newtooltypes[20]:=ttstr26

IF nofilter THEN newtooltypes[21]:='NOFILTER' ELSE newtooltypes[21]:='(NOFILTER)'
IF noprogress THEN newtooltypes[22]:='NOPROGRESSBAR' ELSE newtooltypes[22]:='(NOPROGRESSBAR)'

StrCopy(ttstr13,'ODDCOLOURS=')
IF allelseink THEN StrAdd(ttstr13,'INK') ELSE StrAdd(ttstr13,'PAPER')
newtooltypes[23]:=ttstr13

IF autonaming THEN newtooltypes[24]:='(NOAUTONAMING)' ELSE newtooltypes[24]:='NOAUTONAMING'

IF testmode THEN newtooltypes[26]:='TESTMODE' ELSE newtooltypes[26]:='(TESTMODE)'

IF saveattr THEN newtooltypes[25]:='(NOATTRIBUTES)' ELSE newtooltypes[25]:='NOATTRIBUTES'

IF dither THEN newtooltypes[27]:='DITHER' ELSE newtooltypes[27]:='(DITHER)'
IF simpleremap THEN newtooltypes[28]:='SIMPLEMAP' ELSE newtooltypes[28]:='(SIMPLEMAP)'

IF dtype=1 THEN newtooltypes[29]:='DITHERTYPE=RANDOM' ELSE newtooltypes[29]:='DITHERTYPE=ORDERED'

IF remapafterdither THEN newtooltypes[30]:='REMAPAFTERDITHER' ELSE newtooltypes[30]:='(REMAPAFTERDITHER)'

IF snapshotwindows
    StringF(ttstr27,'WINX=\d',wptr.leftedge)
    StringF(ttstr28,'WINY=\d',wptr.topedge)
newtooltypes[31]:=ttstr27
newtooltypes[32]:=ttstr28

    newtooltypes[33]:=NIL
ELSE
    newtooltypes[31]:=NIL
ENDIF

dobj.tooltypes:=newtooltypes

PutDiskObject(wbarg.name,dobj)

dobj.tooltypes:=oldtooltypes
FreeDiskObject(dobj)
ENDIF
IF olddir<>-1 THEN CurrentDir(olddir)

ENDPROC

/* 1348 */

PROC aslhookfunc(hook,fr:PTR TO filerequester,frobj:PTR TO anchorpathold)
DEF finfo:PTR TO fileinfoblock,gid,ac:PTR TO achain,name[1024]:STRING,ppm=NIL

finfo:=frobj.info
ac:=frobj.last
-> finfo:=ac.info

IF finfo.direntrytype>0 THEN RETURN -1
->IF finfo.size=0 THEN RETURN -1

-> NameFromLock(ac.lock,filebuf,1024)  -> **** hit **** // achain problem?
StrCopy(name,fr.drawer)
AddPart(name,finfo.filename,1024)

IF (lock:=Lock(name, ACCESS_READ))
     
    deetee:=ObtainDataTypeA(DTST_FILE, lock, NIL)
    gid:=deetee.header.groupid

     ReleaseDataType(deetee)

     UnLock(lock)
	lock:=NIL

    IF gid=GID_PICTURE
        RETURN -1
    ELSE /* check if PPM */
        IF finfo.size=147471 -> Correct size for 256x192
            IF fhid:=Open(name,OLDFILE)
                okay:=Read(fhid,fileid,2)
                Close(fhid)
                IF StrCmp('P6',fileid,2) THEN ppm:=TRUE
            ENDIF
            IF ppm=TRUE THEN RETURN -1 -> ELSE RETURN -1
        ENDIF
    ENDIF
ENDIF
ENDPROC 0

PROC about()
EasyRequestArgs(0,[SIZEOF easystruct,0,'About PDHFIC...','PDHFIC version 3.2 by Chris Young\nDithering based on code by LCD\nOriginal Datatypes code by Joe Mackay\nOS4 port made possible with PortablE by Chris Handley\n\n© 1998-2001,2008 Unsatisfactory Software\nemail: chris@unsatisfactorysoftware.co.uk\nhttp://www.unsatisfactorysoftware.co.uk\n\nThe Amiga -> Spectrum image conversion utility!\n\nLoads: Datatypes, PPM\nSaves: SCR, TAP, TZX, ZX82, .header/.bytes, GIF, +3DOS','Oh, great']:easystruct,0,NIL)
ENDPROC

PROC openprogresswin()
DEF pwtop,pwleft

IF noprogress=0

pwtbar:=mysc.font.ysize+mysc.wbortop+1
pwtop:=(wptr.topedge+(wptr.height/2)-(((mysc.font.ysize*2)+pwtbar+1)/2))
pwleft:=(wptr.leftedge+(wptr.width/2))-135

pwptr:=OpenWindowTagList(NIL,[WA_TITLE,'PDHFIC',
                    WA_TOP,pwtop,   WA_LEFT,pwleft,
                           WA_WIDTH,     270,   
                           WA_HEIGHT,pwtbar+23,
                           WA_AUTOADJUST,TRUE,
                           WA_DRAGBAR,   TRUE,  WA_DEPTHGADGET,  TRUE,
                           WA_ACTIVATE,  TRUE,  WA_CLOSEGADGET,  FALSE,
                           WA_BUSYPOINTER,TRUE,
                           WA_SCREENTITLE,'PDHFIC 3.2  © 1998-2001,2008 Unsatisfactory Software',
                           NIL]:tagitem)

pwrp:=pwptr.rport
curwin:=pwptr

            DrawBevelBoxA(pwrp,6,pwtbar+2,258,mysc.font.ysize+4,
            [GTBB_RECESSED,TRUE,
             GT_VISUALINFO,vi,
             NIL])

SetAPen(pwrp,3)
ELSE
    IF wbmessage THEN curwin:=wptr
ENDIF
ENDPROC

PROC setprogresswin(percent)
DEF barsize

barsize:=(percent*2.58)  -> 258 -> ((percent/254.0)*100.0)

IF pwptr THEN RectFill(pwrp,8,pwtbar+3,barsize,pwtbar+mysc.font.ysize+4)

ENDPROC

PROC closeprogresswin()
IF pwptr
    CloseWindow(pwptr)
    pwptr:=NIL
ENDIF

IF wbmessage THEN curwin:=wptr
ENDPROC

PROC arexxecute()
DEF tmpstr2[1024]:STRING

IF arexxreq

okay:=AslRequest(arexxreq,NIL)

IF okay=FALSE THEN RETURN

StrCopy(tmpstr2,arexxreq.drawer)

AddPart(tmpstr2,arexxreq.file,1024)

StrCopy(tmpstr,'c:run c:rx "')
StrAdd(tmpstr,tmpstr2)
StrAdd(tmpstr,'"')

WriteF('\s\n',tmpstr)

Execute(tmpstr,NIL,NIL)

ENDIF

ENDPROC


PROC drawcross()
                Move(rp,229,((pixsep*14)+(mysc.font.ysize*11)+tbar+1))
                Draw(rp,259, ((pixsep*16)+(mysc.font.ysize*14)+tbar+2))
                Move(rp,259,((pixsep*14)+(mysc.font.ysize*11)+tbar+1))
                Draw(rp,229,((pixsep*16)+(mysc.font.ysize*14)+tbar+2))

ENDPROC

PROC drawflashcolour()

->        IF (flashred<>-1) AND (flashgrn<>-1) AND (flashblu<>-1)

            IF obtainbest=0
                IF flashcol=-1
                    SetAPen(rp,0)
                    drawcross()
                    RETURN
                ENDIF
            ELSE
                IF flashcol<>-1 THEN ReleasePen(cmap,flashcol)
                flashcol:=-1
            ENDIF


                IF (flashred=-1) OR (flashgrn=-1) OR (flashblu=-1)
                    SetAPen(rp,0)
                ELSE
                    IF obtainbest
                        IF (flashcol:=ObtainBestPenA(cmap,Shl(flashred,24),Shl(flashgrn,24),Shl(flashblu,24),[OBP_PRECISION,PRECISION_GUI,NIL]))=-1 THEN SetAPen(rp,1)
                    ELSE
                        SetColour(mysc,flashcol,flashred,flashgrn,flashblu)
                    ENDIF
                    SetAPen(rp,flashcol)
                ENDIF
                 RectFill(rp,229, ((pixsep*14)+(mysc.font.ysize*11)+tbar+1), 259, ((pixsep*16)+(mysc.font.ysize*14)+tbar+2))
                IF (flashred=-1) OR (flashgrn=-1) OR (flashblu=-1)
                 SetAPen(rp,1)
                     IF flashred=-1 THEN drawcross()
                     IF flashgrn=-1 THEN drawcross()
                     IF flashblu=-1 THEN drawcross()
                ENDIF




ENDPROC

