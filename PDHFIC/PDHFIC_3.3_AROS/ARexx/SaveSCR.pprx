/* PDHFIC Saver for Personal Paint */

/* $VER: SaveSCR 1.1 (27.09.98)
*/

/** ENG
 SaveSCR for PPaint/PDHFIC

 By Chris Young 1998

 This (very) short script will allow you to save pictures from PPaint into
 any Spectrum format supported by PDHFIC 2.0 or higher.

 PDHFIC must be running for this to work.

*/

if ~show(ports,'PPAINT') then do
	ADDRESS COMMAND 'requestchoice >nil: "LoadDT.zxam" "PPaint is not running!" "Oops."'
	exit
end

if ~show(ports,'PDHFIC') then do
	RequestNotify '"PDHFIC SaveSCR" "PDHFIC is not running!"'
	exit
end

ADDRESS PPAINT
OPTIONS RESULTS

LockGUI

RequestFile '"Save SCR as..."'

savename = result

SaveImage 'FILE t:pdhfic.tmp FORMAT ILBM'
/* PBM & resize if poss  OPTIONS "AUTO=0" "BINARY=1" */

ADDRESS PDHFIC 'QUICKCONVERT "t:pdhfic.tmp" ' || savename

ADDRESS COMMAND 'delete >NIL: t:pdhfic.tmp'

UnlockGUI

