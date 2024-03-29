
/*** PE/CPP/base ***/
/* PortablE target module for C++ */ 



#define _CRT_SECURE_NO_DEPRECATE 1	//silence depreciated warnings of Visual C++
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <string.h>

#define NULLA NULL
#define NULLS NULL
#define NULLL NULL
#define EMPTY (void)0
#define TRUE -1
#define FALSE 0
#define QuadChara(a, b, c, d) ((a << 24) | (b << 16) | (c << 8) | d)
typedef signed char BOOLEAN;	//enum BOOLEAN {FALSE=0, TRUE=-1};
class eException {} eException;

void* FastNew(long size, BOOLEAN noClear);
void* FastDispose(void* mem, long size);

int    main_argc;
char** main_argv;

const long ALL=(long) -1;
class object;
class class_base;
class function;

/* non-native code */

//possible cast functions
//PROC Bool(value)  IS value <> FALSE
//PROC Byte(value)  IS (IF value>=0 THEN (value AND $7F) ELSE -(value AND $7F))!!BYTE
//PROC Char(value)  IS (value AND $FF)!!CHAR
//PROC Int(value)   IS (IF value>=0 THEN (value AND $7FFF) ELSE -(value AND $7FFF))!!INT
//PROC Long(value)  IS value!!LONG
//PROC Quad(value)  IS value!!QUAD
//PROC Float(value) IS value!!FLOAT
//PROC Ptr(value)   IS value!!PTR


class object {
public:
	void* operator new(size_t size) {
		return FastNew(size, FALSE);
	}
	void operator delete(void* mem) {
		FastDispose(mem, -999);
		return;
	}
};

class class_base: public object {
public:
	BOOLEAN notCalledDestructor;
public:
	class_base() {notCalledDestructor=1;}
	virtual ~class_base() {if(notCalledDestructor) {end_class(); notCalledDestructor=0;}}
	virtual void end_class() ;
	virtual long InfoClassType() ;
	virtual BOOLEAN IsSameClassTypeAs(long type) ;
	virtual BOOLEAN IsOfClassType(long parent) ;
};
const long TYPEOF_class_base = (long) "class_base";

class function: public class_base {
public:
	virtual ~function() {if(notCalledDestructor) {end_class(); notCalledDestructor=0;}}
	virtual void new_function() ;
	virtual BOOLEAN IsOfClassType(long parent) ;
	virtual long InfoClassType() ;
};
const long TYPEOF_function = (long) "function";

char* pe_TargetLanguage;

/* system globals */

long exception;
char* exceptionInfo;
float retFloat2; long ret2; float retFloat3; long ret3; float retFloat4; long ret4; float retFloat5; long ret5;
short Inp(void* fileHandle);
long FileLength(char* path);
BOOLEAN StrCmp(char* first, char* second, long len=ALL, long firstOffset=0, long secondOffset=0);
BOOLEAN StrCmpNoCase(char* first, char* second, long len=ALL, long firstOffset=0, long secondOffset=0);
long Val(char* string, long* addrRead=NULLA);
long InStr(char* haystack, char* needle, long startPos=0);
char* UpperStr(char* string);
char* LowerStr(char* string);
void AstrCopy(void* destination, char* source, long destSize);
void* NewR(long size, BOOLEAN noClear=FALSE);
void CleanUp(long returnValue=0);
long Rnd(long max);
long Mod(long a2, long b2);
long Pow(long a2, long b2);
float RealVal(char* string);
void Throw(long a2, char* b2=NULLA);
void Raise(long a2);
void new_base() ;
short Inp(void* fileHandle) {
	short char2;
	char2 = getc((FILE*)fileHandle );
	if( char2 == EOF  ) { char2 = -1;}
	return char2;
} 
long FileLength(char* path) {
	long size;
	
	FILE* stream = fopen(path ,"rb");
	if( stream== NULL) {
		size = -1;
	} else {
		fseek(stream, 0, SEEK_END);
		size = ftell(stream);
		fclose(stream);
	}
	return size;
} 

BOOLEAN StrCmp(char* first, char* second, long len, long firstOffset, long secondOffset) {
//IS 0 = (IF len=ALL THEN NATIVE {strcmp(} firstOffset*SIZEOF CHAR + first {,} secondOffset*SIZEOF CHAR + second {)} ENDNATIVE) ELSE (0 = NATIVE {strncmp(} firstOffset*SIZEOF CHAR + first {,} secondOffset*SIZEOF CHAR + second {,} len {)} ENDNATIVE))
	BOOLEAN match;
	if( len == ALL) {
		match = - (strcmp((char*) (firstOffset*(long) sizeof( char )+ first ),(char*) (secondOffset*(long) sizeof( char )+ second ))== 0) 	;//!!BYTE
	} else {
		match = - (strncmp((char*) (firstOffset*(long) sizeof( char )+ first ),(char*) (secondOffset*(long) sizeof( char )+ second ),len )== 0) 	;//!!BYTE
	}
	return match;
} 

BOOLEAN StrCmpNoCase(char* first, char* second, long len, long firstOffset, long secondOffset) {
//IS 0 = (IF len=ALL THEN NATIVE {strcasecmp(} firstOffset*SIZEOF CHAR + first {,} secondOffset*SIZEOF CHAR + second {)} ENDNATIVE) ELSE (0 = NATIVE {strncasecmp(} firstOffset*SIZEOF CHAR + first {,} secondOffset*SIZEOF CHAR + second {,} len {)} ENDNATIVE))
	BOOLEAN match;
	if( len == ALL) {
		match = - (strcasecmp((char*) (firstOffset*(long) sizeof( char )+ first ),(char*) (secondOffset*(long) sizeof( char )+ second ))== 0) 	;//!!BYTE
	} else {
		match = - (strncasecmp((char*) (firstOffset*(long) sizeof( char )+ first ),(char*) (secondOffset*(long) sizeof( char )+ second ),len )== 0) 	;//!!BYTE
	}
	return match;
} 

long Val(char* string, long* addrRead) {
	long value, read2;
	char* str=NULL; long i2;
	char* final=NULL; signed char base, isNegative;
	long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;
	
	//find start of number (skip any spaces & tabs)
	/*i := 0
	WHILE (string[i]=" ") OR (string[i]="\t")
		i++
	ENDWHILE*/
	i2 = strspn(string ," \t");
	str = (char*) (string + i2 * (long) sizeof( char));
	
	//determine sign & base of number (and skip their symbols)
	if( (isNegative = - (str[0] == '-')  )) { str++;}
	
	if(      str[0] == '\000') { base =  0	;//string is empty
	} else if( str[0] == '%' ) { base =  2 ; str++;
	} else if( str[0] == '$' ) { base = 16 ; str++;
	} else {                  base = 10;
	}
	
	//interpret value
	if( base == 0) {
		value = 0;
		read2  = 0;
	} else {
		errno = 0;
		value = strtol(str ,&final ,base );
		if( errno== 0) {
			read2  = (long) ((long) (final - (long) string) / (long) sizeof( char ));
		} else {
			value = 0;
			read2  = 0;
		}
	}
	
	if( addrRead ) { addrRead[0] = read2;}
	if( isNegative ) { value = -value;}
} catch(...) {}
	ret2 = read2;
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return value;
} 

long InStr(char* haystack, char* needle, long startPos) {
	long foundPos;
	char* start=NULL; char* found=NULL;
	
	start = (char*) (haystack + startPos * (long) sizeof( char));
	
	found = strstr(start ,needle );
	if( found) {
		foundPos = (long) (found - (long) haystack) / (long) sizeof( char);
	} else {
		foundPos = -1;
	}
	return foundPos;
} 
char* UpperStr(char* string) {
	long i2; char chara;
	i2 = 0;
	while( chara = string[i2]) {
		if( - (chara >= 'a')  & - (chara <= 'z')  ) { string[i2] = chara + 'A' - 'a';}
		i2++;
	}
	return string;
} 
char* LowerStr(char* string) {
	long i2; char chara;
	i2 = 0;
	while( chara = string[i2]) {
		if( - (chara >= 'A')  & - (chara <= 'Z')  ) { string[i2] = chara + 'a' - 'A';}
		i2++;
	}
	return string;
} 
void AstrCopy(void* destination, char* source, long destSize) {
	strncpy((char*) destination ,source ,destSize-(long) 1 );
	((char*) destination )[destSize ] = 0;
	return ;
}
void* NewR(long size, BOOLEAN noClear) {
	void* mem=NULL;
	mem = ( noClear )? malloc(size ): memset(malloc(size ), 0,size );
	if( mem == NULL ) { Raise(QuadChara(0,'M','E','M'));}
	return mem;
} 
void CleanUp(long returnValue) {
	Throw(-1, (char*) returnValue)	;//use reserved exception -1 for CleanUp()
	return ;
}
long Rnd(long max) {
	long num;
	if( max >= 0) {
		num = Mod( rand(), max );
	} else {
		srand(abs(max ));
		num = 0;
	}
	return num;
} 
long Mod(long a2, long b2) {
	long c, d;
	long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;
	d = a2 / b2;
	c = a2 - d * b2;
} catch(...) {}
	ret2 = d;
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return c;
} 
/*->ldiv() simply does not work on too many compilers
PROC  Mod(a, b)
	DEF c, d
	{ldiv_t temp = ldiv(} a {,} b {)}
	c := {temp.rem}!!VALUE
	d := {temp.quot}!!VALUE
ENDPROC c, d*/
long Pow(long a2, long b2) {
	long c;
	c = 1;
	while( b2 > 0) {
		c = c * a2;
		b2--;
	}
	return c;
} 
float RealVal(char* string) {
	float value; long read2;
	char* final=NULL;
	long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;
	
	errno = 0;
	value = strtod(string ,&final );
	if( errno== 0) {
		read2  = (long) (final - (long) string) / (long) sizeof( char);
	} else {
		value = 0.0;
		read2  = 0;
	}
} catch(...) {}
	ret2 = read2;
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return value;
} 

//Does not work in StormC: IS (exception := a) BUT (exceptionInfo := b) BUT {throw new Exception()}
void Throw(long a2, char* b2) {
	exception     = a2;
	exceptionInfo = b2;
	throw eException;
	return ;
}
void Raise(long a2) {
	exception = a2;
	throw eException;
	return ;
}
void new_base()  {
	pe_TargetLanguage = (char*) "CPP";
	return ;
}
void class_base::end_class()  {
	EMPTY;
}
long class_base::InfoClassType()  {
	return TYPEOF_class_base;
}
BOOLEAN class_base::IsSameClassTypeAs(long type)  {
	return - (type == this->InfoClassType());
}
BOOLEAN class_base::IsOfClassType(long parent)  {
	return - (parent == TYPEOF_class_base);
}
void function::new_function()  {
	EMPTY;
}
BOOLEAN function::IsOfClassType(long parent)  {
	return ( parent == TYPEOF_function)? TRUE : this ->class_base:: IsOfClassType(parent );
}
long function::InfoClassType()  {
	return TYPEOF_function;
}

/*** target/PE/base ***/
/* PortablE target module for C++ AROS */ 

/* system constants */

const short OLDFILE=1005;
const short NEWFILE=1006;

char* pe_TargetOS;
void* SetStdIn(void* fileHandle);
void* SetStdOut(void* fileHandle);
void new_base2() ;
/* stdin & stdout */

void* SetStdIn(void* fileHandle) {
	void* oldstdin=NULL;
	oldstdin = (void*) stdin;
	stdin = (FILE*) (long) fileHandle;
	return oldstdin;
} 

void* SetStdOut(void* fileHandle) {
	void* oldstdout=NULL;
	oldstdout = (void*) stdout;
	stdout = (FILE*) (long) fileHandle;
	return oldstdout;
} 
void new_base2()  {
	pe_TargetOS = (char*) "AROS";
	return ;
}

/*** PE/pSemaphores_iface ***/
/* interface of pSemaphores */ 
void* NewSemaphore() ;
void* DisposeSemaphore(void* sem2) ;
void SemLock(void* sem2) ;
void SemUnlock(void* sem2) ;
BOOLEAN SemTryLock(void* sem2) ;

/*** PE/EndianLittle ***/
/* PE/EndianLittle.e 10-02-09
   Swaps endianness, for use on little-endian machines.
*/ 
long long EndianSwapBIGVALUE(long long in);
/*PROC EndianSwapLONG(in:LONG)
	DEF out:LONG
	DEF p1:LONG, p2:LONG, p3:LONG, p4:LONG
	p1 := in SHL 24 AND $FF000000
	p2 := in SHL  8 AND $00FF0000
	p3 := in SHR  8 AND $0000FF00
	p4 := in SHR 24 AND $000000FF
	out := p1 OR p2 OR p3 OR p4
ENDPROC out*/

long long EndianSwapBIGVALUE(long long in) {
	long long out;
	long long* bigValue=NULL; long buffer[2]; long temp;
	
	bigValue = (long long*) buffer ;
	bigValue[0] = in;
	
	temp      = (long) (buffer[0] << 24 & 0xFF000000  | buffer[0] <<  8 & 0x00FF0000  | buffer[0] >>  8 & 0x0000FF00  | (long) ((short) (buffer[0] >> 24 )& 0x000000FF) );
	buffer[0] = (long) (buffer[1] << 24 & 0xFF000000  | buffer[1] <<  8 & 0x00FF0000  | buffer[1] >>  8 & 0x0000FF00  | (long) ((short) (buffer[1] >> 24 )& 0x000000FF) );
	buffer[1] = temp;
	
	out = bigValue[0];
	return out;
} 

/*** PE/Char ***/
/* PE/Char.e 06-06-08
*/ 

/*** PE/OstrCmp ***/



//Replacement for AmigaE's OstrCmp(), which (possibly erratically) seems to incorrectly think these two characters are the same:
// "A" = $41 = 065 = %01000001
// "�" = $AB = 171 = %10101011

signed char OstrCmp(char* string1, char* string2, long max=ALL, long string1Offset=0, long string2Offset=0);
signed char OstrCmpNoCase(char* string1, char* string2, long max=ALL, long string1Offset=0, long string2Offset=0);

signed char OstrCmp(char* string1, char* string2, long max, long string1Offset, long string2Offset) {
	signed char sign;
	short order; char char1; long index;
	
	string1 = (char*) (string1Offset*(long) sizeof( char )+ string1);
	string2 = (char*) (string2Offset*(long) sizeof( char )+ string2);
	
	index = 0;
	if( - (index < max)  | - (max==ALL)) {
		do {
			char1 = string1[index];
			order = string2[index] - char1		;//sign indicates order
			
			index++;
		} while(( - (order!=0)  | - (char1==0)  | - (index >= max)  & - (max!=ALL) 	)==0);//char1=0 catches case where both strings are same length
	} else {
		order = 0;
	}
	
	sign = ( order==0 )? 0 : ( order<0 )? -1 : 1;
	return sign;
} 

//This is like OstrCmp() but it does not care about letter case
signed char OstrCmpNoCase(char* string1, char* string2, long max, long string1Offset, long string2Offset) {
	short order; char char1, char2; long index;
	
	string1 = (char*) (string1Offset*(long) sizeof( char )+ string1);
	string2 = (char*) (string2Offset*(long) sizeof( char )+ string2);
	
	index = 0;
	if( - (index < max)  | - (max==ALL)) {
		do {
			char1=string1[index];
			char2=string2[index];
			
			if( - (char1>='a')  & - (char1<='z')  ) { char1 = char1 - 'a' + 'A';}
			if( - (char2>='a')  & - (char2<='z')  ) { char2 = char2 - 'a' + 'A';}
			
			order=char2 - char1	;//sign indicates order
			
			index++;
		} while(( - (order!=0)  | - (char1==0)  | - (index >= max)  & - (max!=ALL) 	)==0);//char1=0 catches case where both strings are same length
	} else {
		order = 0;
	}
	return ( order==0 )? 0 : ( order<0 )? -1 : 1;
} 

/*** PE/CPP/FastMem_emulated ***/
/* PE/CPP/FastMem_emulated.e 31-03-09
   A re-implementation of AmigaE's fast memory functions,
   based upon Tomasz Wiszkowski's description of how AmigaE implemented it.
   
   By Christopher S Handley.
   06-01-08 - Completed and put in the Public Domain.
   02-04-08 - Replaced "NEW" by "MEM".
   12-10-08 - Replaced Shr() by SHR.
   11-03-09 - Fixed memory alignment issue reported by Daniel Westerberg.
   31-03-09 - Made thread-safe with help from Daniel Westerberg.
*/ 


const short MAX_FAST_SIZE=256			;//must be a multiple of 4
const signed char HEADER_SIZE=sizeof( signed char);
const signed char ALIGN_SIZE=sizeof( long		);//must be a power of 2, and >= 4

const signed char ALIGN_SIZE_M1=ALIGN_SIZE - 1;
const signed char MISALIGN_FOR_HEADER=ALIGN_SIZE - HEADER_SIZE;

void* freeListArray[MAX_FAST_SIZE/4+1];
void* chopMem=NULLA; long chopLeft;
void* sem;
void new_fastmem_emulated();
void end_fastmem_emulated();
void* FastNew(long size, BOOLEAN noClear=FALSE) ;
void* FastDispose(void* mem, long size);


void new_fastmem_emulated() {
	long i2;
	for( i2 = 0 ; i2 <=(long) (short) MAX_FAST_SIZE/4 ; i2 ++) { freeListArray[i2] = NULLA;}
	sem = NewSemaphore();
	return ;
}

void end_fastmem_emulated() {
	sem = DisposeSemaphore(sem);
	return ;
}


/*NATIVE {FastNew}*/
void* FastNew(long size, BOOLEAN noClear)  {
	void* mem=NULL;
	signed char index;
	
	if( size <= 0 ) { Raise(QuadChara(0,'M','E','M'));}
	
	size = size + (long) HEADER_SIZE;
	if( size > MAX_FAST_SIZE) {
		mem = (void*) ((char*) NewR(size + (long) MISALIGN_FOR_HEADER, noClear) + MISALIGN_FOR_HEADER	);//mis-align memory, so that header re-aligns it
		index = 0;
	} else {
		size = size + (long) ALIGN_SIZE_M1  & (long) ~ ALIGN_SIZE_M1		;//round-up to a multiple of ALIGN_SIZE
		index = (signed char) (size >> 2 );
		
		SemLock(sem);
		mem = freeListArray[index];
		if( mem != NULLA) {
			freeListArray[index] = (*(void**) mem );
		} else {
			if( - (chopMem == NULLA)  | - (chopLeft < size)) {
				chopLeft = 65536;
				chopMem  = (void*) ((char*) NewR(chopLeft + (long) MISALIGN_FOR_HEADER, /*noClear=*/ TRUE) + MISALIGN_FOR_HEADER	);//mis-align memory, so that header re-aligns it
				//memory will be freed automagically upon exit
			}
			
			mem = chopMem;
			chopMem  = (void*) ((char*) chopMem  + size);
			chopLeft = chopLeft - size;
		}
		SemUnlock(sem);
		
		if( noClear == FALSE ) { memset(mem , 0,size );}
	}
	(*(signed char*) mem =index );
	mem = (void*) ((char*) mem + HEADER_SIZE	);//memory is now aligned
	return mem ;
} 

/*NATIVE {FastDispose}*/
void* FastDispose(void* mem, long size) {
	signed char index;
	
	if( mem) {
		if( - (size <= 0)  & - (size != -999)  ) { Throw(QuadChara(0,'M','E','M'), (char*) "FastDispose(); size<=0");}
		
		mem   = (void*) ((char*) mem - HEADER_SIZE);
		index = (*(signed char*) mem );
		
		if( size != -999) {
			size = size + (long) HEADER_SIZE;
			if( size > MAX_FAST_SIZE) {
				if( index != 0 ) { Throw(QuadChara(0,'M','E','M'), (char*) "FastDispose(); wrong size supplied or memory header corrupted");}
			} else {
				size = size + (long) ALIGN_SIZE_M1  & (long) ~ ALIGN_SIZE_M1;
				if( index != size >> 2  ) { Throw(QuadChara(0,'M','E','M'), (char*) "FastDispose(); wrong size supplied or memory header corrupted");}
			}
		}
		
		if( index == 0) {
			free((void*) ((char*) mem - MISALIGN_FOR_HEADER) );
			NULLA;
		} else {
			SemLock(sem);
			(*(void**) mem =freeListArray[index] );
			freeListArray[index] = mem;
			SemUnlock(sem);
		}
	}
	return NULLA;
} 

/*** PE/EString_partial ***/
/* PE/EString.e 19-04-09
   A re-implementation of AmigaE's E-string functions.
   
   By Christopher S Handley:
   Mostly completed 19-03-02, started 10-03-02.
   Ported to PortablE from 11-07-06 to 14-07-06.
   Put in the Public Domain on 14-07-06.
   Updated to use the STRING type on 27-07-06.
   Fixed Next() & Link() bugs on 27-10-07.
   StrJoin() added earlier but not documented until 19-04-09.
*/ /* Emulated procedures:
NewString(maxLen) RETURNS eString:STRING
DisposeString(eString:STRING) RETURNS NILS
StrCopy( eString:STRING, string:ARRAY OF CHAR, len=ALL, pos=0) RETURNS eString:STRING
StrAdd(  eString:STRING, string:ARRAY OF CHAR, len=ALL, pos=0) RETURNS eString:STRING
StrJoin(s1=NILA:ARRAY OF CHAR, s2=NILA:ARRAY OF CHAR, s3=NILA:ARRAY OF CHAR, s4=NILA:ARRAY OF CHAR, s5=NILA:ARRAY OF CHAR, s6=NILA:ARRAY OF CHAR, s7=NILA:ARRAY OF CHAR, s8=NILA:ARRAY OF CHAR, s9=NILA:ARRAY OF CHAR, s10=NILA:ARRAY OF CHAR, s11=NILA:ARRAY OF CHAR) RETURNS newString:STRING
EstrLen( eString:STRING) RETURNS len:VALUE
StrMax(  eString:STRING) RETURNS max:VALUE
RightStr(eString:STRING, eString2:ARRAY OF CHAR, n) RETURNS eString:STRING
MidStr(  eString:STRING, string:ARRAY OF CHAR, pos, len=ALL) RETURNS eString:STRING
SetStr(  eString:STRING, newLen)
Link(    complex:STRING, tail:STRING) RETURNS complex:STRING
Next(    complex:STRING) RETURNS tail:STRING
Forward( complex:STRING, num) RETURNS tail:STRING

On-purposely missing procedures:
ReadStr(fileHandle:PTR, eString:STRING) RETURNS fail:BOOL
StringF(eString:STRING, fmtString:ARRAY OF CHAR, ...) RETURNS eString:STRING, len
RealF(  eString:STRING, value:FLOAT, decimalPlaces=8:BYTE) RETURNS eString:STRING
*/
class pEString;


class pEString: public object {
public:
	long length;                 	//length of actual string (excluding terminating zero)
	long size;                  	//max length of string    (including terminating zero)
	pEString* next;	//points to next string header, not the actual string
};
char* NewString(long maxLen);
char* DisposeString(char* eString);
char* StrCopy(char* eString, char* string, long len=ALL, long pos=0);
char* StrAdd(char* eString, char* string, long len=ALL, long pos=0);
char* StrJoin(char* s1=NULLA, char* s2=NULLA, char* s3=NULLA, char* s4=NULLA, char* s5=NULLA, char* s6=NULLA, char* s7=NULLA, char* s8=NULLA, char* s9=NULLA, char* s10=NULLA, char* s11=NULLA, char* s12=NULLA, char* s13=NULLA, char* s14=NULLA, char* s15=NULLA, char* s16=NULLA, char* s17=NULLA, char* s18=NULLA, char* s19=NULLA);
long EstrLen(char* eString);
long StrMax(char* eString);
char* RightStr(char* eString, char* eString2, long n);
char* MidStr(char* eString, char* string, long pos, long len=ALL);
void SetStr(char* eString, long newLen);
char* Link(char* complex, char* tail);
char* Next(char* complex);
char* Forward(char* complex, long num);


char* NewString(long maxLen) {
	char* eString=NULL;
	pEString* pEString2=NULL;	
	long sizeOfEString;
	
	//use check
	if( maxLen < 0 ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; NewString(); maxLen<0");}
	
	//allocate eString
	sizeOfEString = (maxLen + (long) 1 )* (long) sizeof( char)  + sizeof( pEString);
	pEString2 = (pEString*) FastNew(sizeOfEString, TRUE);
	
	//init
	pEString2->length = 0;
	pEString2->size   = maxLen + (long) 1;
	pEString2->next   = (pEString*) NULL;
	
	//retrieve string after header
	eString = (char*) ((char*) pEString2 + sizeof( pEString ));
	
	//zero-terminate empty string
	eString[0] = '\000';
	return eString;
} 

char* DisposeString(char* eString) {
	pEString* pEString2=NULL;
	pEString* next=NULL;
	
	if( eString) {
		//retrieve string header
		pEString2 = (pEString*) (eString - sizeof( pEString ));
		
		//loop through all strings in linked list
		do {
			//store any string tail
			next = pEString2->next;
			
			//dealloc string
			pEString2 = (pEString*) FastDispose(pEString2, pEString2->size * (long) sizeof( char)  + sizeof( pEString));
			
			//move to tail
			pEString2 = next;
		} while(( (void*) pEString2 == NULL)==0);
	}
	return NULLS;
} 

char* StrCopy(char* eString, char* string, long len, long pos) {
	pEString* pEString2=NULL;
	
	//use check
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; StrCopy(); eString=NILS");}
	if(  (void*) string == NULLA ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; StrCopy(); string=NILA");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; StrCopy(); len<0");}
	if( pos < 0) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; StrCopy(); pos<0");}
	
	//retrieve string header
	pEString2 = (pEString*) (eString - sizeof( pEString ));
	
	//empty e-string before appending to it
	pEString2->length = 0;
	StrAdd(eString, string, len, pos);
	return eString;
} 

char* StrAdd(char* eString, char* string, long len, long pos) {
	pEString* pEString2=NULL;
	long readIndex, maxReadIndex;
	long writeIndex, maxWriteIndex;
	
	//use check
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; StrAdd(); eString=NILS");}
	if(  (void*) string == NULLA ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; StrAdd(); string=NILA");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; StrAdd(); len<0");}
	if( pos < 0) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; StrAdd(); pos<0");}
	
	//retrieve string header
	pEString2 = (pEString*) (eString - sizeof( pEString ));
	
	//calc end of string reading from & writing to (inc zero termination)
	maxReadIndex  = pos + (( len==ALL )? pEString2->size - (long) 1 : len);
	maxWriteIndex = pEString2->size - (long) 1;
	
	//copy all characters that will fit
	readIndex  = pos;
	writeIndex = pEString2->length	;//start writing past end of string
	while( - (string[readIndex] != 0)  & - (writeIndex < maxWriteIndex)  & - (readIndex < maxReadIndex)) {
		eString[writeIndex] = string[readIndex];
		
		writeIndex++;
		readIndex++;
	}
	
	//update string's stored length
	pEString2->length = writeIndex;
	eString[writeIndex] = '\000';
	return eString;
} 

char* StrJoin(char* s1, char* s2, char* s3, char* s4, char* s5, char* s6, char* s7, char* s8, char* s9, char* s10, char* s11, char* s12, char* s13, char* s14, char* s15, char* s16, char* s17, char* s18, char* s19) {
	char* newString=NULL;
	long len;
	
	len = 0;
	if( s1 ) { len = len + strlen(s1 );}
	if( s2 ) { len = len + strlen(s2 );}
	if( s3 ) { len = len + strlen(s3 );}
	if( s4 ) { len = len + strlen(s4 );}
	if( s5 ) { len = len + strlen(s5 );}
	if( s6 ) { len = len + strlen(s6 );}
	if( s7 ) { len = len + strlen(s7 );}
	if( s8 ) { len = len + strlen(s8 );}
	if( s9 ) { len = len + strlen(s9 );}
	if( s10 ) { len = len + strlen(s10 );}
	if( s11 ) { len = len + strlen(s11 );}
	if( s12 ) { len = len + strlen(s12 );}
	if( s13 ) { len = len + strlen(s13 );}
	if( s14 ) { len = len + strlen(s14 );}
	if( s15 ) { len = len + strlen(s15 );}
	if( s16 ) { len = len + strlen(s16 );}
	if( s17 ) { len = len + strlen(s17 );}
	if( s18 ) { len = len + strlen(s18 );}
	if( s19 ) { len = len + strlen(s19 );}
	
	newString= NewString(len);
	if( s1 ) { StrAdd(newString, s1);}
	if( s2 ) { StrAdd(newString, s2);}
	if( s3 ) { StrAdd(newString, s3);}
	if( s4 ) { StrAdd(newString, s4);}
	if( s5 ) { StrAdd(newString, s5);}
	if( s6 ) { StrAdd(newString, s6);}
	if( s7 ) { StrAdd(newString, s7);}
	if( s8 ) { StrAdd(newString, s8);}
	if( s9 ) { StrAdd(newString, s9);}
	if( s10 ) { StrAdd(newString, s10);}
	if( s11 ) { StrAdd(newString, s11);}
	if( s12 ) { StrAdd(newString, s12);}
	if( s13 ) { StrAdd(newString, s13);}
	if( s14 ) { StrAdd(newString, s14);}
	if( s15 ) { StrAdd(newString, s15);}
	if( s16 ) { StrAdd(newString, s16);}
	if( s17 ) { StrAdd(newString, s17);}
	if( s18 ) { StrAdd(newString, s18);}
	if( s19 ) { StrAdd(newString, s19);}
	return newString;
} 

long EstrLen(char* eString) {
	long len;
	pEString* pEString2=NULL;
	
	//use check
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; EstrLen(); eString=NILS");}
	
	//retrieve string header
	pEString2 = (pEString*) (eString - sizeof( pEString ));
	len = pEString2->length;
	return len;
} 

long StrMax(char* eString) {
	long max;
	pEString* pEString2=NULL;
	
	//use check
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; StrMax(); eString=NILS");}
	
	//retrieve string header
	pEString2 = (pEString*) (eString - sizeof( pEString ));
	max = pEString2->size - (long) 1;
	return max;
} 

char* RightStr(char* eString, char* eString2, long n) {
	pEString* pEString2=NULL;
	char* readString=NULL;
	
	//use check
	if( eString  == NULLS ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; RightStr(); eString=NILS");}
	if( eString2 == NULLS ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; RightStr(); eString2=NILS");}
	if( n < 0           ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; RightStr(); n<0");}
	
	//retrieve string header
	pEString2 = (pEString*) (eString2 - sizeof( pEString ));
	
	//restrict n to sensible range
	if( n > pEString2->length ) { n = pEString2->length;}
	
	//move to start of n characters
	readString = (char*) (eString2 + (pEString2->length - n) * (long) sizeof( char)  );
	
	//use strCopy procedure
	StrCopy(eString, readString, n);
	return eString;
} 

char* MidStr(char* eString, char* string, long pos, long len) {
	long index; char* readString=NULL;
	
	//use check
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; MidStr(); eString=NILS");}
	if(  (void*) string == NULLA ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; MidStr(); string=NILA");}
	if( pos < 0        ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; MidStr(); pos<0");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; MidStr(); len<0");}
	
	//find correct start position SAFELY (which is more than AmigaE does!)
	index = 0;
	while( - (string[index] != 0)  & - (pos > 0)) {
		index++;
		pos--;
	}
	
	readString = (char*) (string + index * (long) sizeof( char)  );
	
	//copy specified part of string
	StrCopy(eString, readString, len);
	return eString;
} 

void SetStr(char* eString, long newLen) {
	pEString* pEString2=NULL;
	
	//use check
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; SetStr(); eString=NILS");}
	if( newLen < 0     ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; SetStr(); newLen<0");}
	
	//retrieve string header
	pEString2 = (pEString*) (eString - sizeof( pEString ));
	
	//additional use check
	if( newLen >= pEString2->size ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; SetStr(); newLen exceeds string size");}
	
	//set length
	pEString2->length = newLen;
	eString[newLen] = '\000';
	return ;
}

char* Link(char* complex, char* tail) {
	pEString* pEString2=NULL;
	
	//use check
	if( complex == NULLS ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; Link(); complex=NILS");}
	
	//retrieve string header
	pEString2 = (pEString*) (complex - sizeof( pEString ));
	
	//store tail's header
	pEString2->next = (pEString*) (( tail )? (void*) (tail - sizeof( pEString )): NULL);
	return complex;
} 

char* Next(char* complex) {
	char* tail=NULL;
	pEString* pEString2=NULL;
	long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;
	
	if( complex == NULLS ) {
		tail= NULLS;
		Raise(0);
	}
	
	//retrieve string header
	pEString2 = (pEString*) (complex - sizeof( pEString ));
	
	//return tail with hidden header
	pEString2 = pEString2->next;
	if( (void*) pEString2 == NULL ) {
		tail = NULLS ;
	} else { 
		tail = (char*) ((char*) pEString2 + sizeof( pEString ));
	}
} catch(...) {}
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return tail;
} 

char* Forward(char* complex, long num) {
	char* tail=NULL;
	pEString* pEString2=NULL;
	long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;
	
	//use check
	if( num < 0        ) { Throw(QuadChara(0,'E','P','U'), (char*) "EString; Forward(); num<0");}
	
	if( complex == NULLS ) {
		tail= NULLS;
		Raise(0);
	}
	
	//retrieve string header
	pEString2 = (pEString*) (complex - sizeof( pEString ));
	
	while( - ((void*) pEString2 != NULL)  & - (num > 0)) {
		pEString2 = pEString2->next;
		num--;
	}
	
	//retrieve string after header
	if( (void*) pEString2 == NULL ) {
		tail = NULLS ;
	} else { 
		tail = (char*) ((char*) pEString2 + sizeof( pEString ));
	}
} catch(...) {}
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return tail;
} 

/*** PE/EList ***/
/* PE/EList.e 14-08-06
   A re-implementation of AmigaE's E-list functions.
   
   By Christopher S Handley:
   Completed 30-07-06, started 27-07-06.
   Put in the Public Domain on 30-07-06.
   Updated to use the ILIST type on 14-08-06.
   Fixed DisposeList() bug on 07-01-08.
*/ /* Emulated procedures:
NewList(maxLen) RETURNS list:LIST
DisposeList(list:LIST) RETURNS NILL
ListCopy(list: LIST, other:ILIST, len=ALL) RETURNS list:LIST
ListAdd( list: LIST, other:ILIST, len=ALL) RETURNS list:LIST
ListCmp( list:ILIST, other:ILIST, len=ALL) RETURNS match:BOOL
ListMax( list: LIST) RETURNS max:VALUE
ListLen( list:ILIST) RETURNS len:VALUE
ListItem(list:ILIST, index) RETURNS value
SetList( list: LIST, newLen)
*/
class pEList;


class pEList: public object {
public:
	long length;		//length of actual list
	long size;  		//max length of list
};
long* NewList_elist(long maxLen);
long* DisposeList(long* list);
long* ListCopy(long* list, long* other, long len=ALL);
long* ListAdd(long* list, long* other, long len=ALL);
BOOLEAN ListCmp(long* list, long* other, long len=ALL);
long ListMax(long* list);
long ListLen(long* list);
long ListItem(long* list, long index);
void SetList(long* list, long newLen);


long* NewList_elist(long maxLen) {
	long* list=NULL;
	pEList* pEList2=NULL;	
	long sizeOfEList;
	
	//use check
	if( maxLen < 0 ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; NewList(); maxLen<0");}
	
	//allocate e-list
	sizeOfEList = maxLen * (long) sizeof( long)  + sizeof( pEList);
	pEList2 = (pEList*) FastNew(sizeOfEList, TRUE);
	
	//init
	pEList2->length = 0;
	pEList2->size   = maxLen;
	
	//retrieve list after header
	list = (long*) ((char*) pEList2 + sizeof( pEList ));
	return list;
} 

long* DisposeList(long* list) {
	pEList* pEList2=NULL;
	
	if( list) {
		//retrieve list header
		pEList2 = (pEList*) ((char*) list - sizeof( pEList ));
		
		//dealloc list
		pEList2 = (pEList*) FastDispose(pEList2, pEList2->size * (long) sizeof( long)  + sizeof( pEList));
	}
	return NULLL;
} 

long* ListCopy(long* list, long* other, long len) {
	pEList* pEList2=NULL;
	
	//use check
	if(  list == NULLL ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListCopy(); list=NILL");}
	if( other == NULLL ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListCopy(); other=NILL");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListCopy(); len<0");}
	
	//retrieve list header
	pEList2 = (pEList*) ((char*) list - sizeof( pEList ));
	
	//empty e-list before appending to it
	pEList2->length = 0;
	ListAdd(list, other, len);
	return list;
} 

long* ListAdd(long* list, long* other, long len) {
	pEList* pEList2=NULL; pEList* pEOther=NULL;
	long readIndex, maxReadIndex;
	long writeIndex, maxWriteIndex;
	
	//use check
	if(  list == NULLL ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListAdd(); list=NILL");}
	if( other == NULLL ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListAdd(); other=NILL");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListAdd(); len<0");}
	
	//retrieve list header
	pEList2  =  (pEList*) ((char*) list - sizeof( pEList ));
	pEOther = (pEList*) ((char*) other - sizeof( pEList ));
	
	//calc end of list reading from & writing to
	maxReadIndex  = ( len==ALL )? pEOther->size : ( len<pEOther->size )? len : pEOther->size;
	maxWriteIndex = pEList2->size;
	
	//copy all characters that will fit
	readIndex  = 0;
	writeIndex = pEList2->length		;//start writing past end of list
	while( - (writeIndex < maxWriteIndex)  & - (readIndex < maxReadIndex)) {
		list[writeIndex] = other[readIndex];
		
		writeIndex++;
		readIndex++;
	}
	
	//update list's stored length
	pEList2->length = writeIndex;
	return list;
} 

BOOLEAN ListCmp(long* list, long* other, long len) {
	BOOLEAN match;
	pEList* pEList2=NULL; pEList* pEOther=NULL;
	long index, maxIndex;
	
	//use check
	if(  list == NULLL ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListCmp(); list=NILL");}
	if( other == NULLL ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListCmp(); other=NILL");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListCmp(); len<0");}
	
	//retrieve list header
	pEList2  =  (pEList*) ((char*) list - sizeof( pEList ));
	pEOther = (pEList*) ((char*) other - sizeof( pEList ));
	
	if( pEList2->length != pEOther->length) {
		match = FALSE;
	} else {
		//calc where should stop comparison
		maxIndex = ( pEList2->size<pEOther->size )? pEList2->size : pEOther->size;
		if( len != ALL ) { maxIndex = ( len<maxIndex )? len : maxIndex;}
		
		//compare all characters
		match = TRUE;
		index = 0;
		while( index < maxIndex) {
			if( list[index] != other[index] ) { match = FALSE;}
			
			index++;
		if( match == FALSE) break;
		} 
	}
	return match;
} 

long ListMax(long* list) {
	long max;
	pEList* pEList2=NULL;
	
	//use check
	if( list == NULLL ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListMax(); list=NILL");}
	
	//retrieve list header
	pEList2 = (pEList*) ((char*) list - sizeof( pEList ));
	max = pEList2->size;
	return max;
} 

long ListLen(long* list) {
	long len;
	pEList* pEList2=NULL;
	
	//use check
	if( list == NULLL ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListLen(); list=NILL");}
	
	//retrieve list header
	pEList2 = (pEList*) ((char*) list - sizeof( pEList ));
	len = pEList2->length;
	return len;
} 

long ListItem(long* list, long index) {
	long value;
	pEList* pEList2=NULL;
	
	//use check
	if( list == NULLL ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListLen(); list=NILL");}
	
	//retrieve list header
	pEList2 = (pEList*) ((char*) list - sizeof( pEList ));
	
	//additional use check
	if( - (index < 0)  | - (index >= pEList2->length)  ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; ListLen(); index exceeds list bounds");}
	
	value = list[index];
	return value;
} 

void SetList(long* list, long newLen) {
	pEList* pEList2=NULL;
	
	//use check
	if( list == NULLL ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; SetList(); list=NILL");}
	if( newLen < 0  ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; SetList(); newLen<0");}
	
	//retrieve list header
	pEList2 = (pEList*) ((char*) list - sizeof( pEList ));
	
	//additional use check
	if( newLen > pEList2->size ) { Throw(QuadChara(0,'E','P','U'), (char*) "EList; SetList(); newLen exceeds list size");}
	
	//set length
	pEList2->length = newLen;
	return ;
}

/*** PE/CPP/EString ***/
/* PortablE target module that completes EStrings */ /* missing E-string functions */
char* StringF(char* eString, char* fmtString, long arg1=0, long arg2=0, long arg3=0, long arg4=0, long arg5=0, long arg6=0, long arg7=0, long arg8=0);
BOOLEAN ReadStr(void* fileHandle, char* eString);
char* RealF(char* eString, float value, signed char decimalPlaces=8);
char* appendDecimal(char* eString, long value, long minWidth);

char* StringF(char* eString, char* fmtString, long arg1, long arg2, long arg3, long arg4, long arg5, long arg6, long arg7, long arg8) {
	long len; char tempString[1000];
	long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;
	
	if( StrMax(eString) > 1000) {
		sprintf(    eString ,fmtString ,arg1 ,arg2 ,arg3 ,arg4 ,arg5 ,arg6 ,arg7 ,arg8 );
	} else {
		sprintf( tempString ,fmtString ,arg1 ,arg2 ,arg3 ,arg4 ,arg5 ,arg6 ,arg7 ,arg8 );
		StrCopy(eString, tempString);
	}
	len = EstrLen(eString);
} catch(...) {}
	ret2 = len;
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return eString;
	
	/*DEF len
	{snprintf(} eString {,} StrMax(eString)+1 {,} fmtString {,} arg1 {,} arg2 {,} arg3 {,} arg4 {,} arg5 {,} arg6 {,} arg7 {,} arg8 {)}
	len := StrLen(eString)
	SetStr(eString, len)
	*/
} 

BOOLEAN ReadStr(void* fileHandle, char* eString) {
	BOOLEAN fail;
	fail = - (fgets(eString ,StrMax(eString)+(long) 1 , (FILE*) fileHandle )== NULL);
	SetStr(eString, strlen(eString ));
	return fail;
} 

/*->sprintf() simply does not work on too many compilers, for floating-point
PROC RealF(eString:STRING, value:FLOAT, decimalPlaces=8:BYTE)
	DEF tempString[1000]:ARRAY OF CHAR
	
	IF StrMax(eString) > 1000
		{sprintf(}    eString {, "%.*f",} decimalPlaces {,} value {)}
	ELSE
		{sprintf(} tempString {, "%.*f",} decimalPlaces {,} value {)}
		StrCopy(eString, tempString)
	ENDIF
	
	->{snprintf(} eString {,} StrMax(eString)+1 {, "%.*f",} decimalPlaces {,} value {)}
	->SetStr(eString, StrLen(eString))
ENDPROC eString*/

char* RealF(char* eString, float value, signed char decimalPlaces) {
	float integer; signed char nextDecimalPlaces;
	char* temp_ARRAY_OF_CHAR=NULL;
	
	if( value>0 ) {
		temp_ARRAY_OF_CHAR = (char*) "";
	} else { 
		temp_ARRAY_OF_CHAR = (char*) "-";
	}
	StrCopy(eString, temp_ARRAY_OF_CHAR );
	value = fabs(value );
	
	integer = floor(value );
	appendDecimal(eString, (long) integer, 0);
	
	if( decimalPlaces > 0) {
		StrAdd(eString, (char*) ".");
		
		value = value - integer;
		do {
			nextDecimalPlaces = (signed char) (( decimalPlaces<9 )? (long) decimalPlaces : (long) 9);
			decimalPlaces = (signed char) ((short) decimalPlaces - (short) nextDecimalPlaces );
			
			value = value * (float) Pow(10, nextDecimalPlaces);
			integer = floor(value );
			appendDecimal(eString, (long) integer, nextDecimalPlaces);
			value = value - integer;
		} while(( decimalPlaces <= 0)==0);
	}
	return eString;
} 


char* appendDecimal(char* eString, long value, long minWidth) {
	BOOLEAN isNegative; char temp[12]; signed char pos; long digit;
	
	//use check
	if( (void*) eString == NULLA ) { Throw(QuadChara(0,'E','P','U'), (char*) "PE/CPP/EString; appendDecimal(); eString=NILA");}
	
	//force value to be positive
	if( value >= 0) {
		isNegative = FALSE;
	} else {
		isNegative = TRUE;
		value = (long) 0 - value		;//make value positive
	}
	
	//end string with terminating zero to make it valid
	pos = 11;
	temp[pos] = '\000';
	
	//write string (from end of string) representing value as decimal digits
	if( value == 0) {
		pos--;
		temp[pos] = '0';
		minWidth--;
	} else {
		do {
			//extract right-most digit then remove from value
			digit= Mod(value, 10);
			value = ret2 ;
			
			//write digit as character
			pos--;
			temp[pos] = (char) ((long) '0' + digit );
			minWidth--;
		} while(( value == 0)==0);
		
		//prepend minus sign if was negative
		if( isNegative) {
			pos--;
			temp[pos] = '-';
		}
	}
	
	//prepend required 0 digits
	while( minWidth > 0) {
		pos--;
		temp[pos] = '0';
		minWidth--;
	}
	
	//now append string representation to target E-string
	StrAdd(eString, temp, ALL, pos);
	return eString;
} 

/*** targetShared/CPP/pCallback ***/
/* PortablE callback kludge for C++ */ 
long call0many (void* func) ;
long call1many (void* func, long p1) ;
long call2many (void* func, long p1, long p2) ;
long call3many (void* func, long p1, long p2, long p3) ;
long call4many (void* func, long p1, long p2, long p3, long p4) ;
long call5many (void* func, long p1, long p2, long p3, long p4, long p5) ;
long call6many (void* func, long p1, long p2, long p3, long p4, long p5, long p6) ;
long call0many (void* func)  {
	return ((long (*)(void))func )();
}
long call1many (void* func, long p1)  {
	return ((long (*)(long))func )(p1 );
}
long call2many (void* func, long p1, long p2)  {
	return ((long (*)(long,long))func )(p1 ,p2 );
}
long call3many (void* func, long p1, long p2, long p3)  {
	return ((long (*)(long,long,long))func )(p1 ,p2 ,p3 );
}
long call4many (void* func, long p1, long p2, long p3, long p4)  {
	return ((long (*)(long,long,long,long))func )(p1 ,p2 ,p3 ,p4 );
}
long call5many (void* func, long p1, long p2, long p3, long p4, long p5)  {
	return ((long (*)(long,long,long,long,long))func )(p1 ,p2 ,p3 ,p4 ,p5 );
}
long call6many (void* func, long p1, long p2, long p3, long p4, long p5, long p6)  {
	return ((long (*)(long,long,long,long,long,long))func )(p1 ,p2 ,p3 ,p4 ,p5 ,p6 );
}

/*** target/rexx/errors ***/



#include <rexx/errors.h>

/*** target/graphics/collide ***/
/* $Id: collide.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/collide.h>

/*** target/exec/errors ***/
/* $Id: errors.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <exec/errors.h>

const BOOLEAN ERR_OPENDEVICE=-1	;//IOERR_OPENFAIL

/*** target/exec/alerts ***/
/* $Id: alerts.h 13706 2002-03-19 21:42:44Z falemagn $ */ 

#include <exec/alerts.h>

/*** target/dos/doshunks ***/
/* $Id: doshunks.h 12757 2001-12-08 22:23:57Z chodorowski $/ */ 

#include <dos/doshunks.h>

/*** tools/bits ***/
/* A re-implementation of the 'tools/bits' module, by Chris Handley on 07-07-2008.
   Based upon documention for bits.m v1.0 (07-Jun-96).
*/ 

const BOOLEAN SWAP_LONG=0; const signed char SWAP_HIGH=1; const signed char SWAP_LOW=2; const signed char SWAP_INNER=3; const signed char SWAP_OUTER=4; const signed char SWAPMAX=5;

const signed char SIZE_BYTE=8; const signed char SIZE_WORD=16; const signed char SIZE_LONG=32;
long bitset(long value, long bit) ;
long bitclear(long value, long bit) ;
signed char bittest(long value, long bit) ;
long bitchange(long value, long bit) ;
long bitdset(long value, long bit, long dep) ;
long swap(long value, long what);
char* bintostr(long value, long size, char* str);
long strtobin(char* str, long size);

long bitset(long value, long bit)  {
	return 0x1/*1*/ << bit| value;
}

long bitclear(long value, long bit)  {
	return ~ (0x1/*1*/ << bit)& value;
}

signed char bittest(long value, long bit)  {
	return (signed char) (value >> bit& (long) 0x1/*1*/ );
}
//alternative implementation: PROC bittest(value,bit) IS IF Shl(%1,bit) AND value THEN 1 ELSE 0

long bitchange(long value, long bit)  {
	return ( bittest(value,bit) )? bitclear(value,bit) : bitset(value,bit);
}

long bitdset(long value, long bit, long dep)  {
	return ( dep )? bitset(value,bit) : bitclear(value,bit);
}

long swap(long value, long what) {
	long newValue;
	switch(  what) {
	case SWAP_LONG  : newValue = (value & 0x0000FFFF )<< 16| (value & 0xFFFF0000 )>> 16;
		break;
	case SWAP_HIGH  : newValue = (value & 0x00FF0000 )<< 8| (value & 0xFF000000 )>> 8| value & 0x0000FFFF;
		break;
	case SWAP_LOW   : newValue = (value & (long) 0x000000FF )<< 8| (value & 0x0000FF00 )>> 8| value & 0xFFFF0000;
		break;
	case SWAP_INNER : newValue = (value & 0x0000FF00 )<< 8| (value & 0x00FF0000 )>> 8| value & 0xFF0000FF;
		break;
	case SWAP_OUTER : newValue = (value & (long) 0x000000FF )<< 24| (value & 0xFF000000 )>> 24| value & 0x00FFFF00;
		break;
	default:          newValue = value;
		break;
	}
	return newValue;
} 

char* bintostr(long value, long size, char* str) {
	long i2;
	for( i2 = 0 ; i2 <=(long) size-(long) 1 ; i2 ++) { str[i2] = (char) ((long) '0' + (long) bittest(value,size-(long) 1-i2) );}
	str[size] = 0;
	return str;
} 

long strtobin(char* str, long size) {
	long value;
	long i2; char bit;
	value = 0;
	i2 = 0;
	while( bit = str[i2++]) {
		value = value << 1| (long) (bit - '0');
	if( i2 >= size) break;
	} 
	return value;
} 

/*** target/aros/_timeval ***/




#include <aros/types/timeval_s.h>

/*** target/exec/initializers ***/
/* $Id: initializers.h 18668 2003-07-19 02:59:06Z iaint $ */ 

#include <exec/initializers.h>

/*** target/exec/types ***/
/* $Id: types.h 25077 2006-12-12 12:23:33Z falemagn $ */ 

#include <exec/types.h>

/*** target/prefs/printergfx ***/
/* placeholder module */ 

#include <prefs/printergfx.h>

/*** target/prefs/printertxt ***/
/* placeholder module */ 

#include <prefs/printertxt.h>

/*** target/intuition/iprefs ***/
/* $Id: iprefs.h 21385 2004-03-25 22:20:00Z falemagn $ */ 

#include <intuition/iprefs.h>

/*** target/hardware/blit ***/
/* placeholder module */ 

#include <hardware/blit.h>

/*** target/graphics/rpattr ***/
/* $Id: rpattr.h 23343 2005-05-31 22:20:48Z stegerg $ */ 

#include <graphics/rpattr.h>

/*** target/graphics/gels ***/
/* $Id: gels.h 22331 2004-09-04 11:59:49Z verhaegs $ */ 

#include <graphics/gels.h>

/*** target/exec/avl ***/




#include <exec/avl.h>

/*** target/dos/stdio ***/
/* $Id: stdio.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <dos/stdio.h>

/*** target/hardware/custom ***/
/* placeholder module */ 

#include <hardware/custom.h>

/*** target/exec/nodes ***/
/* $Id: nodes.h 27659 2008-01-05 22:10:30Z neil $ */ 

#include <exec/nodes.h>

/*** target/dos/bptr ***/




#include <dos/bptr.h>

/*** target/utility/tagitem ***/
/* $Id: tagitem.h 28320 2008-04-15 20:16:32Z schulz $ */ 

#include <utility/tagitem.h>

/*** target/graphics/gfx ***/
/* $Id: gfx.h 20142 2003-11-18 18:06:36Z stegerg $ */ 

#include <graphics/gfx.h>

/*** target/dos/var ***/
/* $Id: var.h 28624 2008-05-05 10:31:44Z sszymczy $ */ 

#include <dos/var.h>

/*** target/dos/rdargs ***/
/* $Id: rdargs.h 28622 2008-05-04 23:44:11Z sszymczy $ */ 

#include <dos/rdargs.h>

/*** target/exec/lists ***/
/* Structures and macros for exec lists. */ 

#include <exec/lists.h>

/*** target/exec/libraries ***/
/* $Id: libraries.h 18668 2003-07-19 02:59:06Z iaint $ */ 

#include <exec/libraries.h>

/*** target/utility/hooks ***/
/* $Id: hooks.h 18327 2003-07-04 14:37:43Z chodorowski $ */ 

#include <utility/hooks.h>

/*** target/exec/memory ***/
/* $Id: memory.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <exec/memory.h>

/*** target/dos/dos ***/
/* $Id: dos.h 20404 2003-12-24 15:27:23Z falemagn $ */ 

#include <dos/dos.h>
char* dosname;
void new_dos() ;
void new_dos()  {
	dosname = (char*) "dos.library";
	return ;
}

/*** target/graphics/videocontrol ***/
/* $Id: videocontrol.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/videocontrol.h>

/*** target/graphics/layersext ***/
/* $Id: layersext.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/layersext.h>

/*** target/exec/resident ***/
/* $Id: resident.h 23527 2005-08-15 10:49:04Z stegerg $ */ 

#include <exec/resident.h>

/*** target/intuition/icclass ***/
/* $Id: icclass.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <intuition/icclass.h>

/*** target/dos/dostags ***/
/* $Id: dostags.h 15065 2002-07-30 12:50:52Z falemagn $ */ 

#include <dos/dostags.h>

/*** target/graphics/sprite ***/
/* $Id: sprite.h 19503 2003-08-29 21:37:33Z bergers $ */ 

#include <graphics/sprite.h>

/*** target/graphics/regions ***/
/* $Id: regions.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/regions.h>

/*** target/graphics/scale ***/
/* $Id: scale.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/scale.h>

/*** target/aros/arossupportbase ***/




#include <aros/arossupportbase.h>

/*** target/exec/interrupts ***/
/* $Id: interrupts.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <exec/interrupts.h>

const long SF_SAR=0x8000;
const signed char SIH_QUEUES=5;
const short SF_SINT=0x2000;
const short SF_TQE=0x4000;

/*** target/devices/keymap ***/
/* $Id: keymap.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <devices/keymap.h>

/*** target/exec/ports ***/
/* $Id: ports.h 25583 2007-03-26 23:38:53Z dariusb $ */ 

#include <exec/ports.h>

const signed char MP_SOFTINT=16;

/*** target/dos/exall ***/
/* $Id: exall.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <dos/exall.h>

/*** target/exec/memheaderext ***/
/* $Id: memheaderext.h 22869 2005-02-08 21:52:05Z falemagn $ */ 

#include <exec/memheaderext.h>

/*** target/dos/record ***/
/* $Id: record.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <dos/record.h>

/*** target/dos/datetime ***/
/* $Id: datetime.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <dos/datetime.h>

/*** target/dos/dosasl ***/
/* $Id: dosasl.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <dos/dosasl.h>

/*** target/exec/devices ***/
/* $Id: devices.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <exec/devices.h>

/*** target/exec/tasks ***/
/* $Id: tasks.h 20558 2004-01-08 22:13:16Z stegerg $ */ 

#include <exec/tasks.h>

const long SYS_TRAPALLOC=0x8000;
const long SYS_SIGALLOC=0xFFFF;

/*** target/workbench/startup ***/




#include <workbench/startup.h>

/*** target/exec/io ***/
/* $Id: io.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <exec/io.h>

/*** target/exec/execbase ***/
/* $Id: execbase.h 28407 2008-04-20 18:58:52Z schulz $ */ 

#include <exec/execbase.h>

/*** target/dos/notify ***/
/* $Id: notify.h 21273 2004-03-18 07:25:48Z chodorowski $ */ 

#include <dos/notify.h>

/*** target/exec/semaphores ***/
/* $Id: semaphores.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <exec/semaphores.h>

const signed char SM_LOCKMSG=16;

/*** target/workbench/handler ***/




#include <workbench/handler.h>

/*** target/devices/serial ***/
/* $Id: serial.h 23386 2005-06-23 08:17:21Z neil $ */ 


#define DEVICES_SERIAL_H_OBSOLETE
#include <devices/serial.h>

char* serialname;
void new_serial() ;
void new_serial()  {
	serialname = (char*) "serial.device";
	return ;
}

/*** target/devices/parallel ***/
/* $Id: parallel.h 23386 2005-06-23 08:17:21Z neil $ */ 

#include <devices/parallel.h>
char* parallelname;
void new_parallel() ;
void new_parallel()  {
	parallelname = (char*) "parallel.device";
	return ;
}

/*** target/devices/clipboard ***/
/* $Id: clipboard.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <devices/clipboard.h>

/*** target/devices/timer ***/
/* $Id: timer.h 19888 2003-10-01 01:33:00Z falemagn $ */ 

#include <devices/timer.h>
char* timername;
void new_timer() ;
void new_timer()  {
	timername = (char*) "timer.device";
	return ;
}

/*** target/exec ***/








#include <proto/exec.h>

struct ExecBase* SysBase = NULL;


/*** target/libraries/iffparse ***/





#include <libraries/iffparse.h>

/*** target/dos/dosextens ***/
/* $Id: dosextens.h 28978 2008-07-04 06:07:27Z sonic $ */ 

#include <dos/dosextens.h>

/*** target/intuition/preferences ***/
/* $Id: preferences.h 20855 2004-02-10 09:34:48Z chodorowski $ */ 

#include <intuition/preferences.h>

/*** target/graphics/gfxnodes ***/
/* $Id: gfxnodes.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/gfxnodes.h>

/*** target/graphics/copper ***/
/* $Id: copper.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/copper.h>

/*** target/graphics/layers ***/
/* $Id: layers.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/layers.h>

/*** target/graphics/clip ***/
/* $Id: clip.h 19924 2003-10-08 02:25:29Z bergers $ */ 

#include <graphics/clip.h>

/*** target/graphics/text ***/
/* $Id: text.h 14170 2002-04-15 22:06:37Z stegerg $ */ 

#include <graphics/text.h>

/*** target/graphics/rastport ***/
/* $Id: rastport.h 16374 2003-02-04 22:15:47Z dlc $ */ 

#include <graphics/rastport.h>

/*** target/arossupport ***/




#include <proto/arossupport.h>

/*** targetShared/CPP/AmigaOS/CtrlC ***/





/*** targetShared/CPP/AmigaOS/pStack ***/




long FreeStack() ;
long StackSize() ;

long FreeStack()  {
	long bytes;
	struct Task* task=NULL; long size;
	task = FindTask((char*) NULLA );
	size = (long) ((char*) (void*) task->tc_SPUpper - (long) task->tc_SPLower);
	
	bytes = (long) ((char*) (&bytes )- (long) task->tc_SPLower);
	if( - (bytes < 0)  | - (bytes > size)) {
		bytes = (long) ((char*) (void*) task->tc_SPReg - (long) task->tc_SPLower);
		if( - (bytes < 0)  | - (bytes > size)) {
			bytes = size;
		}
	}
	return bytes ;
} 

long StackSize()  {
	long bytes;
	struct Task* task=NULL;
	task = FindTask((char*) NULLA );
	bytes = (long) ((char*) (void*) task->tc_SPUpper - (long) task->tc_SPLower);
	return bytes ;
} 

/*** targetShared/AmigaOS/exec ***/




void* NewM(long size, long flags);

void* NewM(long size, long flags) {
	void* mem=NULL;
	
	if( flags & (long) ((short) (MEMF_CHIP | MEMF_FAST | MEMF_PUBLIC )| MEMF_LOCAL | MEMF_24BITDMA)) {
		printf("NewM() emulation was passed an unsupported flag\n" ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 );
		Raise(QuadChara(0,'M','E','M'));
	}
	
	mem = memset(malloc(size ), 0,size );
	if( mem == NULL ) { Raise(QuadChara(0,'M','E','M'));}
	return mem;
} 

/*** targetShared/AmigaOS/pSemaphores ***/
/* AmigaOS C++ implementation of pSemaphores */ 
class semaphore;


class semaphore: public object {
public:
	struct SignalSemaphore ss;
};


void* NewSemaphore()  {
	void* sem2=NULL; 
	sem2 = memset(malloc(sizeof( semaphore) ), 0,sizeof( semaphore) );
	InitSemaphore(& ((semaphore*) sem2)->ss );
	return sem2 ;
} 

void* DisposeSemaphore(void* sem2)  {
	void* nil=NULL; 
	free(sem2 );
	nil = NULLA;
	return nil ;
} 

void SemLock(void* sem2)  {
	ObtainSemaphore(& ((semaphore*) sem2)->ss );
	return ;
}

void SemUnlock(void* sem2)  {
	ReleaseSemaphore(& ((semaphore*) sem2)->ss );
	return ;
}

BOOLEAN SemTryLock(void* sem2)  {
	BOOLEAN success; 
	success = - ((long) AttemptSemaphore(& ((semaphore*) sem2)->ss )!= (long) 0);
	return success ;
} 

/*** target/datatypes/datatypes ***/





#include <datatypes/datatypes.h>

/*** target/dos/filehandler ***/
/* $Id: filehandler.h 25328 2007-03-04 12:57:35Z rob $ */ 

#include <dos/filehandler.h>

/*** target/rexx/storage ***/





#include <rexx/storage.h>

/*** target/graphics/monitor ***/
/* $Id: monitor.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/monitor.h>
char* default_monitor_name;
char* ntsc_monitor_name;
char* pal_monitor_name;
char* vga_monitor_name;
void new_monitor() ;
void new_monitor()  {
	default_monitor_name = (char*) "default.monitor";
	ntsc_monitor_name = (char*) "ntsc.monitor";
	pal_monitor_name = (char*) "pal.monitor";
	vga_monitor_name = (char*) "vga.monitor";
	return ;
}

/*** target/graphics/gfxmacros ***/
/* $Id: gfxmacros.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/gfxmacros.h>

/*** target/dos/filesystem ***/
/* $Id: filesystem.h 28392 2008-04-20 00:20:39Z neil $ */ 

#include <dos/filesystemids.h>

/*** target/rexx/rexxcall ***/





#include <rexx/rexxcall.h>

/*** target/rexx/rxslib ***/





#include <rexx/rxslib.h>
char* rxsname;
char* rxsdir;
char* rxstname;
void new_rxslib() ;
void new_rxslib()  {
	rxsname = (char*) "rexxsyslib.library";
	rxsdir = (char*) "REXX";
	rxstname = (char*) "ARexx";
	return ;
}

/*** target/graphics/displayinfo ***/
/* $Id: displayinfo.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/displayinfo.h>

/*** target/dos ***/








#include <proto/dos.h>

struct DosLibrary* DOSBase = NULL;

void new_dos2();
void end_dos();

//automatic opening of dos library
void new_dos2() {
	DOSBase = (struct DosLibrary*) (long) OpenLibrary("dos.library" ,(ULONG) 39 );
	if( (void*) DOSBase==NULL ) { CleanUp(RETURN_ERROR);}
	return ;
}

//automatic closing of dos library
void end_dos() {
	CloseLibrary((struct Library*) DOSBase );
	return ;
}

/*** target/rexxsyslib ***/







#include <proto/rexxsyslib.h>

struct RxsLib* RexxSysBase = NULL;


/*** target/graphics/modeid ***/
/* $Id: modeid.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/modeid.h>

/*** target/graphics/view ***/
/* $Id: view.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <graphics/view.h>

/*** target/libraries/asl ***/




#include <libraries/asl.h>
char* aslname;
void new_asl() ;
void new_asl()  {
	aslname = (char*) "asl.library";
	return ;
}

/*** targetShared/CPP/AmigaOS/dos ***/




char* arg;


char* argString=NULLS;
void new_dos3();
void end_dos2();


void new_dos3() {
	char* args=NULL; long len;
	
	if( (args = GetArgStr())) {
		len = strlen(args );
		argString= NewString(len);
		
		StrCopy(argString, args);
		if( len > 0) {
			if( argString[len-(long) 1] == '\n' ) { SetStr(argString, len-(long) 1);}
		}
	}
	
	arg = argString;
	return ;
}

void end_dos2() {
	argString= DisposeString(argString);
	return ;
}

/*** target/graphics/gfxbase ***/
/* $Id: gfxbase.h 21159 2004-03-04 12:58:56Z falemagn $ */ 

#include <graphics/gfxbase.h>
char* graphicsname;
void new_gfxbase() ;
void new_gfxbase()  {
	graphicsname = (char*) "graphics.library";
	return ;
}

/*** target/asl ***/







#include <proto/asl.h>

struct Library* AslBase = NULL;


/*** tools/arexx ***/
/* History:
03-03-2009: Disabled "rexxsysbase:=mes.libbase" as it appears to be in error, and is not supported by AROS either.  AROS issue reported by Chris Young.
12-11-2008: Added auto-opening of rexxsyslib library, as Chris Young says it is done by his copy of this AmigaE module.
11-11-2008: Released as part of PortablE r3 beta1.
*/ 
void new_arexx();
void end_arexx();
struct MsgPort* rx_OpenPort(char* portname);
void rx_ClosePort(struct MsgPort* port);
struct RexxMsg* rx_GetMsg(struct MsgPort* port);
void rx_ReplyMsg(struct RexxMsg* mes, long rc=0, char* resultstring=NULLA);
void rx_HandleAll(void* interpret_proc, char* portname);

void new_arexx() {
	RexxSysBase = (struct RxsLib*) (long) OpenLibrary(rxsname ,(ULONG) 39 );
	if( (void*) RexxSysBase==NULL ) { CleanUp(RETURN_ERROR);}
	return ;
}

void end_arexx() {
	CloseLibrary((struct Library*) RexxSysBase );
	return ;
}


struct MsgPort* rx_OpenPort(char* portname) {
  struct MsgPort* port=NULL; signed char sig; long exc; struct Node* ln=NULL;
  long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;
  port = new struct MsgPort;
  exc=0;
  Forbid();
  if( FindPort(portname )) {
    exc=QuadChara('D','O','U','B');
  } else {
    port->mp_SigTask=FindTask((char*) NULLA );
    port->mp_Flags=(UBYTE) PA_SIGNAL;
    ln = (struct Node*) port ;
    ln->ln_Name=portname;
    ln->ln_Type=(UBYTE) NT_MSGPORT;
    if( (long) (sig=AllocSignal(-1 ))==(long) NULL) {
      exc=QuadChara(0,'S','I','G');
    } else {
      port->mp_SigBit=(UBYTE) sig ;
      AddPort(port );
    }
  }
  Permit();
  if( exc ) { Raise(exc);}
} catch(...) {}
	ret2 = 1 << sig;
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return port;
} 

void rx_ClosePort(struct MsgPort* port) {
  if( port) {
    FreeSignal((long) port->mp_SigBit );
    RemPort(port );
    delete port; port=NULL; 
  }
	return ;
}

struct RexxMsg* rx_GetMsg(struct MsgPort* port) {
  struct RexxMsg* mes=NULL; char* arg2=NULL;
  long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;
  if( (mes=(struct RexxMsg*) GetMsg(port ))) {
    //rexxsysbase:=mes.libbase
    arg2 = (char*) mes->rm_Args[0] ;
  } else {
    arg2 = (char*) NULLA;
  }
} catch(...) {}
	ret2 = (long) arg2;
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return mes;
} 

void rx_ReplyMsg(struct RexxMsg* mes, long rc, char* resultstring) {
	UBYTE* temp_ARRAY_OF_UBYTE__CHAR_=NULL;
  mes->rm_Result1=rc;
  mes->rm_Result2=(IPTR) (long) NULL;
  if( mes->rm_Action & RXFF_RESULT & (long) - (rc==0)  & (long) - (resultstring!=NULL)) {
    temp_ARRAY_OF_UBYTE__CHAR_ = CreateArgstring((UBYTE*) resultstring ,(ULONG) strlen(resultstring ) );
    if( NULL== temp_ARRAY_OF_UBYTE__CHAR_ ) { Raise(QuadChara(0,'M','E','M') );}
    mes->rm_Result2=(IPTR) (long) temp_ARRAY_OF_UBYTE__CHAR_ ;
  }
  ReplyMsg((struct Message*) mes  );
	return ;
}

void rx_HandleAll(void* interpret_proc, char* portname) {
  struct MsgPort* port=NULL; long sig, quit; struct RexxMsg* mes=NULL; long s, rc; char* rs=NULL; long rsv;
  long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;
  port=(struct MsgPort*) NULL;
  quit=FALSE;
  port=rx_OpenPort(portname);
  sig= ret2 ;
  do {
    Wait((ULONG) sig );
    do {
      mes=rx_GetMsg(port);
      s= ret2 ;
      if( mes ) {
        quit=call1many(interpret_proc, s) ;
        rc= ret2 ;
        rsv= ret3 ; rs=(char*) rsv;
        rx_ReplyMsg(mes,rc,rs);
      }
    } while(( - ((void*) mes==NULL)  | - (quit==TRUE))==0);
  } while(( quit)==0);
  Raise(0);
} catch(...) {}
  rx_ClosePort(port);
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;
}

/*** target/graphics ***/







#include <proto/graphics.h>

struct GfxBase* GfxBase = NULL;

void new_graphics();
void end_graphics();

//automatic opening of gfx library
void new_graphics() {
	GfxBase = (struct GfxBase*) (long) OpenLibrary("graphics.library" ,(ULONG) 39 );
	if( (void*) GfxBase==NULL ) { CleanUp(RETURN_ERROR);}
	return ;
}

//automatic closing of gfx library
void end_graphics() {
	CloseLibrary((struct Library*) GfxBase );
	return ;
}

/*** target/intuition/classusr ***/
/* $Id: classusr.h 25583 2007-03-26 23:38:53Z dariusb $ */ 

#include <intuition/classusr.h>
char* rootclass;
char* imageclass;
char* frameiclass;
char* sysiclass;
char* fillrectclass;
char* gadgetclass;
char* propgclass;
char* strgclass;
char* buttongclass;
char* frbuttonclass;
char* groupgclass;
char* icclass;
char* modelclass;
char* itexticlass;
char* pointerclass;
char* menubarlabelclass;
char* windecorclass;
char* scrdecorclass;
char* menudecorclass;
void new_classusr() ;
void new_classusr()  {
	rootclass = (char*) "rootclass";
	imageclass = (char*) "imageclass";
	frameiclass = (char*) "frameiclass";
	sysiclass = (char*) "sysiclass";
	fillrectclass = (char*) "fillrectclass";
	gadgetclass = (char*) "gadgetclass";
	propgclass = (char*) "propgclass";
	strgclass = (char*) "strgclass";
	buttongclass = (char*) "buttongclass";
	frbuttonclass = (char*) "frbuttonclass";
	groupgclass = (char*) "groupgclass";
	icclass = (char*) "icclass";
	modelclass = (char*) "modelclass";
	itexticlass = (char*) "itexticlass";
	pointerclass = (char*) "pointerclass";
	menubarlabelclass = (char*) "menubarlabelclass";
	windecorclass = (char*) "windecorclass";
	scrdecorclass = (char*) "scrdecorclass";
	menudecorclass = (char*) "menudecorclass";
	return ;
}

/*** target/intuition/screens ***/
/* $Id: screens.h 25583 2007-03-26 23:38:53Z dariusb $ */ 

#include <intuition/screens.h>

/*** targetShared/CPP/AmigaOS/graphics ***/





struct RastPort* stdrast=NULL;
void Plot(long x2, long y2, long colour=1);
void Line_graphics(long x1, long y1, long x2, long y2, long colour=1);
void Box(long x1, long y1, long x2, long y2, long colour=1);
void Colour(long foreground, long background=0);
long TextF(long x2, long y2, char* fmtString, long arg1=0, long arg2=0, long arg3=0, long arg4=0, long arg5=0, long arg6=0, long arg7=0, long arg8=0);
struct RastPort* SetStdRast(struct RastPort* rast);
void SetTopaz(short size=8);

void Plot(long x2, long y2, long colour) {
	if( stdrast) {
		SetAPen(stdrast ,(ULONG) colour );
		WritePixel(stdrast ,x2 ,y2 );
	}
	return ;
}

void Line_graphics(long x1, long y1, long x2, long y2, long colour) {
	if( stdrast) {
		SetAPen(stdrast ,(ULONG) colour );
		Move(stdrast ,(short) x1  ,(short) y1  );
		Draw(stdrast ,x2 ,y2 );
	}
	return ;
}

void Box(long x1, long y1, long x2, long y2, long colour) {
	long xmin, ymin, xmax, ymax;
	if( stdrast) {
		SetAPen(stdrast ,(ULONG) colour );
		xmin = ( x1<x2 )? x1 : x2;
		xmax = ( x1>x2 )? x1 : x2;
		ymin = ( y1<y2 )? y1 : y2;
		ymax = ( y1>y2 )? y1 : y2;
		RectFill(stdrast ,xmin ,ymin ,xmax ,ymax );
	}
	return ;
}

void Colour(long foreground, long background) {
	if( stdrast) {
		SetAPen(stdrast ,(ULONG) foreground );
		SetBPen(stdrast ,(ULONG) background );
	}
	return ;
}

long TextF(long x2, long y2, char* fmtString, long arg1, long arg2, long arg3, long arg4, long arg5, long arg6, long arg7, long arg8) {
	long length;
	char* string=NULL;
	
	if( stdrast) {
		string= NewString(strlen(fmtString )*(long) 2 + (long) 100 );
		StringF(string, fmtString, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
		
		length = EstrLen(string);
		Move(stdrast ,(short) x2  ,(short) y2  );
		Text(stdrast ,string ,(ULONG) length );
		
		string= DisposeString(string);
	}
	return length;
} 

struct RastPort* SetStdRast(struct RastPort* rast) {
	struct RastPort* oldstdrast=NULL;
	oldstdrast = stdrast;
	stdrast = rast;
	return stdrast;
} 

void SetTopaz(short size) {
	struct TextFont* font=NULL;
	struct TextAttr* temp_ARRAY_OF_textattr=NULL; long temp_QUAD;
try {
	temp_ARRAY_OF_textattr = (struct TextAttr*) calloc(1 ,sizeof( struct TextAttr) );
	temp_ARRAY_OF_textattr [0].ta_Name = (char*) "topaz.font";
	temp_ARRAY_OF_textattr [0].ta_Style = (UBYTE) FS_NORMAL;
	temp_ARRAY_OF_textattr [0].ta_Flags = (UBYTE) (FPF_PROPORTIONAL | FPF_DESIGNED);
	temp_QUAD = exception ;
	exception = 0 ;
	
	if( stdrast) {
		temp_ARRAY_OF_textattr [0].ta_YSize = (UWORD) (long) size;
		font = OpenFont((struct TextAttr*) temp_ARRAY_OF_textattr  );
		SetFont(stdrast ,font );
		//CloseFont(font)
	}
} catch(...) {}
	free(temp_ARRAY_OF_textattr );
	NULLA;
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;
}

/*** target/intuition/classes ***/
/* $Id: classes.h 25583 2007-03-26 23:38:53Z dariusb $ */ 

#include <intuition/classes.h>

/*** target/devices/inputevent ***/
/* $Id: inputevent.h 25643 2007-04-02 10:31:37Z schulz $ */ 

#include <devices/inputevent.h>

/*** target/boopsistubs ***/




#include <clib/boopsistubs.h>

/*** target/libraries/commodities ***/





#include <libraries/commodities.h>

/*** target/intuition/intuition ***/
/* $Id: intuition.h 28542 2008-04-28 22:58:18Z sszymczy $ */ 

#include <intuition/intuition.h>
char* intuitionname;
void new_intuition() ;
void new_intuition()  {
	intuitionname = (char*) "intuition.library";
	return ;
}

/*** target/intuition/sghooks ***/
/* $Id: sghooks.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <intuition/sghooks.h>

/*** target/intuition/pointerclass ***/
/* $Id: pointerclass.h 14209 2002-04-20 14:19:18Z falemagn $ */ 

#include <intuition/pointerclass.h>

/*** target/intuition/iobsolete ***/
/* $Id: iobsolete.h 23386 2005-06-23 08:17:21Z neil $ */ 

#include <intuition/iobsolete.h>

/*** target/intuition/intuitionbase ***/
/* $Id: intuitionbase.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <intuition/intuitionbase.h>

/*** target/intuition/extensions ***/
/* $Id: extensions.h 18593 2003-07-13 13:07:39Z chodorowski $ */ 

#include <intuition/extensions.h>

/*** target/intuition/cghooks ***/
/* $Id: cghooks.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <intuition/cghooks.h>

/*** target/devices/prtbase ***/
/* $Id: prtbase.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <devices/prtbase.h>

/*** target/devices/printer ***/
/* $Id: printer.h 12757 2001-12-08 22:23:57Z chodorowski $ */ 

#include <devices/printer.h>

/*** target/intuition/imageclass ***/
/* $Id: imageclass.h 25583 2007-03-26 23:38:53Z dariusb $ */ 

#include <intuition/imageclass.h>

/*** target/intuition/gadgetclass ***/
/* $Id: gadgetclass.h 25583 2007-03-26 23:38:53Z dariusb $ */ 

#include <intuition/gadgetclass.h>

/*** target/workbench/workbench ***/





#include <workbench/workbench.h>
char* workbenchname;
void new_workbench() ;
void new_workbench()  {
	workbenchname = (char*) "workbench.library";
	return ;
}

/*** target/intuition/windecorclass ***/
/* $Id: windecorclass.h 12757 2001-12-08 22:23:57Z dariusb $ */ 

#include <intuition/windecorclass.h>

/*** target/intuition/scrdecorclass ***/
/* $Id: screendecorclass.h 12757 2001-12-08 22:23:57Z dariusb $ */ 

#include <intuition/scrdecorclass.h>

/*** target/datatypes/datatypesclass ***/





#include <datatypes/datatypesclass.h>
char* datatypesclass;
void new_datatypesclass() ;
void new_datatypesclass()  {
	datatypesclass = (char*) "datatypesclass";
	return ;
}

/*** target/libraries/gadtools ***/





#include <libraries/gadtools.h>
char* gadtoolsname;
void new_gadtools() ;
void new_gadtools()  {
	gadtoolsname = (char*) "gadtools.library";
	return ;
}

/*** target/intuition/menudecorclass ***/
/* $Id: menudecorclass.h 12757 2001-12-08 22:23:57Z dariusb $ */ 

#include <intuition/menudecorclass.h>

/*** target/datatypes/textclass ***/





#include <datatypes/textclass.h>
char* textdtclass;
void new_textclass() ;
void new_textclass()  {
	textdtclass = (char*) "text.datatype";
	return ;
}

/*** target/datatypes/soundclass ***/






#include <datatypes/soundclass.h>
char* sounddtclass;
void new_soundclass() ;
void new_soundclass()  {
	sounddtclass = (char*) "sound.datatype";
	return ;
}

/*** target/datatypes/amigaguideclass ***/
/* $VER: amigaguideclass.h 1.1 (04.09.03) */ 

#include <datatypes/amigaguideclass.h>
char* amigaguidedtcass;
void new_amigaguideclass() ;
void new_amigaguideclass()  {
	amigaguidedtcass = (char*) "amigaguide.datatype";
	return ;
}

/*** target/datatypes/pictureclass ***/






#include <datatypes/pictureclass.h>
char* picturedtclass;
void new_pictureclass() ;
void new_pictureclass()  {
	picturedtclass = (char*) "picture.datatype";
	return ;
}

/*** target/amigalib ***/





#include <clib/alib_protos.h>
IPTR hookEntry( struct Hook* hook, APTR obj, APTR param )		;//allow CALLBACK without IS
IPTR hookEntry( struct Hook* hook, APTR obj, APTR param )		 {//allow CALLBACK without IS
	return HookEntry(hook ,obj ,param );
} 

/*** target/gadtools ***/







#include <proto/gadtools.h>

struct Library* GadToolsBase = NULL;


/*** target/intuition ***/







#include <proto/intuition.h>

struct IntuitionBase* IntuitionBase = NULL;

void new_intuition2();
void end_intuition();

//automatic opening of intuition library
void new_intuition2() {
	IntuitionBase = (struct IntuitionBase*) (long) OpenLibrary("intuition.library" ,(ULONG) 39 );
	if( (void*) IntuitionBase==NULL ) { CleanUp(RETURN_ERROR);}
	return ;
}

//automatic closing of intuition library
void end_intuition() {
	CloseLibrary((struct Library*) IntuitionBase );
	return ;
}

/*** target/datatypes/soundclassext ***/




#include <datatypes/soundclassext.h>

/*** target/datatypes/animationclass ***/





#include <datatypes/animationclass.h>
char* animationdtclass;
void new_animationclass() ;
void new_animationclass()  {
	animationdtclass = (char*) "animation.datatype";
	return ;
}

/*** target/workbench/icon ***/





#include <workbench/icon.h>
char* iconname;
void new_icon() ;
void new_icon()  {
	iconname = (char*) "icon.library";
	return ;
}

/*** target/AmigaLib/boopsi ***/

/*PUBLIC*/ 




/*** targetShared/CPP/AmigaOS/intuition ***/





const signed char GADGETSIZE=120;



long code; long qual; APTR iaddr;
struct TextAttr* tempGlobal_ARRAY_OF_textattr;
void SetColour(struct Screen* screen, UBYTE colourreg, UBYTE r, UBYTE g, UBYTE b2);
struct Window* OpenW(short x2, short y2, short width, short height, long idcmp, long wflags, char* title, struct Screen* screen, short sflags, struct Gadget* gadgets2, struct TagItem* taglist=NULLA);
void CloseW(struct Window* wptr2);
struct Screen* OpenS(short width, short height, short depth, short sflags, char* title, struct TagItem* taglist=NULLA);
void CloseS(struct Screen* sptr);
void* Gadget_intuition(void* buffer, void* glist2, short id, long flags, short x2, short y2, short width, char* string);
long Mouse();
BOOLEAN LeftMouse(struct Window* win);
void WaitLeftMouse(struct Window* win);
long MouseX(struct Window* win) ;
long MouseY(struct Window* win) ;
long WaitIMessage(struct Window* win);
long MsgCode() ;
long MsgQualifier() ;
APTR MsgIaddr() ;
void new_intuition3() ;
void end_intuition2() ;

void SetColour(struct Screen* screen, UBYTE colourreg, UBYTE r, UBYTE g, UBYTE b2) {
	SetRGB32(& screen->ViewPort ,(ULONG) (long) colourreg ,(ULONG) ((long) r << 32-8) ,(ULONG) ((long) g << 32-8) ,(ULONG) ((long) b2 << 32-8) );
	return ;
	//or SetRGB4(screen.viewport, colourreg, r, g, b)
}


struct Window* OpenW(short x2, short y2, short width, short height, long idcmp, long wflags, char* title, struct Screen* screen, short sflags, struct Gadget* gadgets2, struct TagItem* taglist) {
	struct Window* wptr2=NULL;
	struct TagItem* temp_ARRAY_OF_tagitem=NULL; long temp_QUAD;
try {
	temp_ARRAY_OF_tagitem = (struct TagItem*) calloc(12 ,sizeof( struct TagItem) );
	temp_ARRAY_OF_tagitem [0].ti_Tag = (Tag) WA_Left;
	temp_ARRAY_OF_tagitem [1].ti_Tag = (Tag) WA_Top;
	temp_ARRAY_OF_tagitem [2].ti_Tag = (Tag) WA_Width;
	temp_ARRAY_OF_tagitem [3].ti_Tag = (Tag) WA_Height;
	temp_ARRAY_OF_tagitem [4].ti_Tag = (Tag) WA_DetailPen;
	temp_ARRAY_OF_tagitem [4].ti_Data = (STACKIPTR) (long) 0xFF ;
	temp_ARRAY_OF_tagitem [5].ti_Tag = (Tag) WA_BlockPen;
	temp_ARRAY_OF_tagitem [5].ti_Data = (STACKIPTR) (long) 0xFF ;
	temp_ARRAY_OF_tagitem [6].ti_Tag = (Tag) WA_IDCMP;
	temp_ARRAY_OF_tagitem [7].ti_Tag = (Tag) WA_Flags;
	temp_ARRAY_OF_tagitem [9].ti_Tag = (Tag) WA_Title;
	temp_QUAD = exception ;
	exception = 0 ;
	temp_ARRAY_OF_tagitem [0].ti_Data = (STACKIPTR) (long) x2;
	temp_ARRAY_OF_tagitem [1].ti_Data = (STACKIPTR) (long) y2;
	temp_ARRAY_OF_tagitem [2].ti_Data = (STACKIPTR) (long) width;
	temp_ARRAY_OF_tagitem [3].ti_Data = (STACKIPTR) (long) height;
	temp_ARRAY_OF_tagitem [6].ti_Data = (STACKIPTR) idcmp;
	temp_ARRAY_OF_tagitem [7].ti_Data = (STACKIPTR) wflags;
	temp_ARRAY_OF_tagitem [8].ti_Tag = (Tag) (( gadgets2 )? WA_Gadgets : (long) TAG_SKIP);
	temp_ARRAY_OF_tagitem [8].ti_Data = (STACKIPTR) (long) gadgets2;
	temp_ARRAY_OF_tagitem [9].ti_Data = (STACKIPTR) (long) title;
	temp_ARRAY_OF_tagitem [10].ti_Tag = (Tag) (( screen )? WA_CustomScreen : (long) TAG_SKIP);
	temp_ARRAY_OF_tagitem [10].ti_Data = (STACKIPTR) (long) screen;
	temp_ARRAY_OF_tagitem [11].ti_Tag = (Tag) (( taglist )? TAG_MORE : TAG_DONE);
	temp_ARRAY_OF_tagitem [11].ti_Data = (STACKIPTR) (long) taglist;
	wptr2 = OpenWindowTagList((struct NewWindow*) NULL ,temp_ARRAY_OF_tagitem  );
	/*unused*/ sflags = 0;
	stdrast = wptr2->RPort;
} catch(...) {}
	free(temp_ARRAY_OF_tagitem );
	NULLA;
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return wptr2;
} 

void CloseW(struct Window* wptr2) {
	if( wptr2) {
		if( wptr2->RPort == stdrast ) { stdrast = (struct RastPort*) NULL;}
		CloseWindow(wptr2 );
	}
	return ;
}

struct Screen* OpenS(short width, short height, short depth, short sflags, char* title, struct TagItem* taglist) {
	struct Screen* sptr=NULL;
	struct TagItem* temp_ARRAY_OF_tagitem=NULL; long temp_QUAD;
try {
	temp_ARRAY_OF_tagitem = (struct TagItem*) calloc(10 ,sizeof( struct TagItem) );
	temp_ARRAY_OF_tagitem [0].ti_Tag = (Tag) SA_Left;
	temp_ARRAY_OF_tagitem [0].ti_Data = (STACKIPTR) 0;
	temp_ARRAY_OF_tagitem [1].ti_Tag = (Tag) SA_Top;
	temp_ARRAY_OF_tagitem [1].ti_Data = (STACKIPTR) 0;
	temp_ARRAY_OF_tagitem [2].ti_Tag = (Tag) SA_Width;
	temp_ARRAY_OF_tagitem [3].ti_Tag = (Tag) SA_Height;
	temp_ARRAY_OF_tagitem [4].ti_Tag = (Tag) SA_Depth;
	temp_ARRAY_OF_tagitem [5].ti_Tag = (Tag) SA_DetailPen;
	temp_ARRAY_OF_tagitem [5].ti_Data = (STACKIPTR) DETAILPEN;
	temp_ARRAY_OF_tagitem [6].ti_Tag = (Tag) SA_BlockPen;
	temp_ARRAY_OF_tagitem [6].ti_Data = (STACKIPTR) BLOCKPEN;
	temp_ARRAY_OF_tagitem [7].ti_Tag = (Tag) SA_Type;
	temp_ARRAY_OF_tagitem [7].ti_Data = (STACKIPTR) (CUSTOMSCREEN | SHOWTITLE);
	temp_ARRAY_OF_tagitem [8].ti_Tag = (Tag) SA_Title;
	temp_QUAD = exception ;
	exception = 0 ;
	temp_ARRAY_OF_tagitem [2].ti_Data = (STACKIPTR) (long) width;
	temp_ARRAY_OF_tagitem [3].ti_Data = (STACKIPTR) (long) height;
	temp_ARRAY_OF_tagitem [4].ti_Data = (STACKIPTR) (long) depth;
	temp_ARRAY_OF_tagitem [8].ti_Data = (STACKIPTR) (long) title;
	temp_ARRAY_OF_tagitem [9].ti_Tag = (Tag) (( taglist )? TAG_MORE : TAG_DONE);
	temp_ARRAY_OF_tagitem [9].ti_Data = (STACKIPTR) (long) taglist;
	sptr = OpenScreenTagList((struct NewScreen*) NULL ,temp_ARRAY_OF_tagitem  );
	/*unused*/ sflags = 0;
	stdrast = & sptr->RastPort;
} catch(...) {}
	free(temp_ARRAY_OF_tagitem );
	NULLA;
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return sptr;
} 

void CloseS(struct Screen* sptr) {
	if( sptr) {
		if( & sptr->RastPort == stdrast ) { stdrast = (struct RastPort*) NULL;}
		-CloseScreen(sptr );
	}
	return ;
}

void* Gadget_intuition(void* buffer, void* glist2, short id, long flags, short x2, short y2, short width, char* string) {
	void* nextbuffer=NULL;
	struct Gadget* gadget=NULL; struct Border* border=NULL; struct IntuiText* intuitext=NULL; short* borderxy=NULL;
	short height; struct Gadget* firstgadget=NULL;
	signed char* zero=NULL; long i2;
	
	nextbuffer = buffer;
	height = 12;
	
	//clear memory (probably not necessary, but does no harm)
	zero = (signed char*) nextbuffer;
	for( i2 = 0 ; i2 <=(long) sizeof( struct Gadget )+ sizeof( struct Border )+ sizeof( struct IntuiText )- (long) 1; i2 ++) {
		zero[i2] = 0;
	}
	
	//init Gadget
	gadget = (struct Gadget*) nextbuffer ;
	gadget->NextGadget = (struct Gadget*) NULL;
	gadget->LeftEdge = x2;
	gadget->TopEdge  = y2;
	gadget->Width    = width;
	gadget->Height   = height;
	gadget->Flags      = (UWORD) (long) ((short) GFLG_GADGHCOMP | (( flags & (long) 2 )? GFLG_SELECTED : (short) 0));
	gadget->Activation = (UWORD) (long) ((short) GACT_RELVERIFY | (( flags & (long) 1 )? GACT_TOGGLESELECT : (short) 0));
	gadget->GadgetType = (UWORD) GTYP_BOOLGADGET;
	//gadget.gadgetrender
	gadget->SelectRender = (APTR) (long) NULL;
	//gadget.gadgettext
	gadget->MutualExclude = 0;
	gadget->SpecialInfo   = (APTR) (long) NULL;
	gadget->GadgetID = (UWORD) 0;
	gadget->UserData = (APTR) (long) id ;
	nextbuffer = (void*) ((char*) nextbuffer + sizeof( struct Gadget));
	
	//init Border
	border = (struct Border*) nextbuffer ;
	border->LeftEdge = 0;
	border->TopEdge  = 0;
	border->FrontPen = (UBYTE) 1;
	border->BackPen  = (UBYTE) 0;
	border->DrawMode = (UBYTE) 0;
	border->Count = 5;
	//border.xy
	border->NextBorder = (struct Border*) NULL;
	nextbuffer = (void*) ((char*) nextbuffer + sizeof( struct Border));
	
	//init IntuiText
	intuitext = (struct IntuiText*) nextbuffer ;
	intuitext->FrontPen = (UBYTE) 1;
	intuitext->BackPen  = (UBYTE) 0;
	intuitext->DrawMode = (UBYTE) 1;
	intuitext->LeftEdge = (short) (((long) width - (long) 8 * strlen(string )) / (long) 2 );
	intuitext->TopEdge  = 2;
	intuitext->ITextFont= (struct TextAttr*) tempGlobal_ARRAY_OF_textattr ;
	intuitext->IText    = (UBYTE*) string;
	intuitext->NextText = (struct IntuiText*) NULL;
	nextbuffer = (void*) ((char*) nextbuffer + sizeof( struct IntuiText));
	
	//init BorderXY
	borderxy = (short*) nextbuffer ;
	borderxy[0] = 0         ; borderxy[1] = 0;
	borderxy[2] = 0         ; borderxy[3] = height - (short) 1;
	borderxy[4] = width - (short) 1 ; borderxy[5] = height - (short) 1;
	borderxy[6] = width - (short) 1 ; borderxy[7] = 0;
	borderxy[8] = 0         ; borderxy[9] = 0;
	nextbuffer = (void*) ((char*) nextbuffer + 10 * sizeof( short));
	
	//link objects to each other
	gadget->GadgetRender = (APTR) (long) border;
	gadget->GadgetText   = intuitext;
	border->XY = borderxy;
	
	//add Gadget to list
	if( glist2) {
		firstgadget = (struct Gadget*) glist2;
		gadget->NextGadget      = firstgadget->NextGadget;
		firstgadget->NextGadget = gadget;
	}
	
	if( (long) ((char*) nextbuffer - (long) buffer) > (long) GADGETSIZE ) { Throw(QuadChara(0,'B','U','G'), (char*) "Gadget(); GADGETSIZE is too small");}
	nextbuffer = (void*) ((char*) buffer + GADGETSIZE);
	return nextbuffer;
} 

long Mouse() {
	long code2;
	code2 = 0;
	if( (*(signed char*) 0xBFE001  )&   64 ) { code2 = code2 | (long) 1;}
	if( (*(short*) 0xDFF016  )& 1024 ) { code2 = code2 | (long) 2;}
	if( (*(short*) 0xDFF016  )&  256 ) { code2 = code2 | (long) 4;}
	return code2;
} 

BOOLEAN LeftMouse(struct Window* win) {
	BOOLEAN button;
	struct IntuiMessage* msg2=NULL;
	
	-ModifyIDCMP(win ,(ULONG) IDCMP_MOUSEBUTTONS );
	msg2 = (struct IntuiMessage*) GetMsg(win->UserPort );
	
	if( msg2) {
		button = - ((long) msg2->Class == (long) IDCMP_MOUSEBUTTONS)  & - ((short) (long) msg2->Code == (short) SELECTDOWN);
		ReplyMsg(& msg2->ExecMessage );
	} else {
		button = FALSE;
	}
	return button;
} 

void WaitLeftMouse(struct Window* win) {
	struct IntuiMessage* msg2=NULL; long class2, code2;
	
	-ModifyIDCMP(win ,(ULONG) IDCMP_MOUSEBUTTONS );
	msg2 = (struct IntuiMessage*) GetMsg(win->UserPort );
	do {
		if( (void*) msg2 == NULL) {
			WaitPort(win->UserPort );
			msg2 = (struct IntuiMessage*) GetMsg(win->UserPort );
		}
		
		class2 = (long) msg2->Class;
		code2  = (long) msg2->Code;
		ReplyMsg(& msg2->ExecMessage );
		msg2 = (struct IntuiMessage*) NULL;
	} while(( - (class2 == IDCMP_MOUSEBUTTONS)  & - (code2 == SELECTDOWN))==0);
	return ;
}

long MouseX(struct Window* win)  {
	return (long) win->MouseY ;
}

long MouseY(struct Window* win)  {
	return (long) win->MouseX ;
}


long WaitIMessage(struct Window* win) {
	long class2;
	struct MsgPort* port=NULL; struct IntuiMessage* mes=NULL;
	
	port = win->UserPort;
	while( (void*) (mes = (struct IntuiMessage*) GetMsg(port )) == NULL) {
		WaitPort(port );
	}
	
	class2 = (long) mes->Class;
	code  = (long) mes->Code;
	qual  = (long) mes->Qualifier;
	iaddr = mes->IAddress;
	ReplyMsg(& mes->ExecMessage );
	return class2;
} 

long MsgCode()  {
	return code;
}

long MsgQualifier()  {
	return qual;
}

APTR MsgIaddr()  {
	return iaddr;
}
void new_intuition3()  {
	tempGlobal_ARRAY_OF_textattr = (struct TextAttr*) calloc(1 ,sizeof( struct TextAttr) );
	tempGlobal_ARRAY_OF_textattr [0].ta_Name = (char*) "topaz.font";
	tempGlobal_ARRAY_OF_textattr [0].ta_YSize = (UWORD) 8;
	tempGlobal_ARRAY_OF_textattr [0].ta_Style = (UBYTE) FS_NORMAL;
	tempGlobal_ARRAY_OF_textattr [0].ta_Flags = (UBYTE) (FPF_PROPORTIONAL | FPF_DESIGNED);
	return ;
}
void end_intuition2()  {
	free(tempGlobal_ARRAY_OF_textattr );
	NULLA;
	return ;
}

/*** target/datatypes ***/







#include <proto/datatypes.h>

struct Library* DataTypesBase = NULL;


/*** target/wb ***/







#include <proto/wb.h>

struct Library* WorkbenchBase = NULL;


/*** target/icon ***/






#include <proto/icon.h>

struct Library* IconBase = NULL;


/*** targetShared/CPP/AmigaOS/wb ***/





struct WBStartup* wbmessage=NULL;
void new_wb();


void new_wb() {
	if( main_argc == 0) {
		wbmessage = (struct WBStartup*) main_argv ;
	}
	return ;
}

/*** PE/compatibility ***/
/* PE/compatibility.e 16-04-09
   The module is used by OPT AMIGAE.
*/ 
void WriteF(char* fmtString, long arg1=0, long arg2=0, long arg3=0, long arg4=0, long arg5=0, long arg6=0, long arg7=0, long arg8=0);

void WriteF(char* fmtString, long arg1, long arg2, long arg3, long arg4, long arg5, long arg6, long arg7, long arg8) {
	printf(fmtString ,arg1 ,arg2 ,arg3 ,arg4 ,arg5 ,arg6 ,arg7 ,arg8 );
	fflush(stdout);
	return ;
}

/*** Files:/Projects/PDHFIC/dt2scr2_asl_aros ***/


/***************************************************
 * IF COMPILING FOR AROS SET FORCENOFILTER=1 BELOW *
 ***************************************************/

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







const BOOLEAN ERR_NONE=0 ; const signed char ERR_FILE=1 ; const signed char ERR_DTYP=2 ; const signed char ERR_NPIC=3 ; const signed char ERR_MEM=4 ; const signed char ERR_BTMP=5 ; const signed char ERR_LIB=6 ;
     const signed char ERR_ABOR=7 ; const signed char ERR_ASL=8 ; const signed char ERR_ICON=9 ; const signed char ERR_ARGS=10 ; const signed char ERR_GTLS=11 ; const signed char ERR_WBLB=12 ; const signed char ERR_ICNN=13 ;
    const signed char GADG_LOAD=14 ; const signed char GADG_SAVE=15 ; const signed char GADG_START=16 ; const signed char GADG_LOADSTR=17 ; const signed char GADG_SAVESTR=18 ; const signed char GADG_TYPE=19 ;
    const signed char GADG_SCALE=20 ; const signed char GADG_ROMREMAP=21 ;
    const signed char ADV_ZXDTPAL=22 ; const signed char ADV_COLOUR=23 ; const signed char ADV_WHITE=24 ; const signed char ADV_BRIGHT=25 ; const signed char ADV_COLOURNUM=26 ;
    const signed char ADV_WHITENUM=27 ; const signed char ADV_BRIGHTNUM=28 ; const signed char ADV_REDYEL=29 ; const signed char ADV_BLUCYN=30 ; const signed char ADV_GRNMAG=31 ;
    const signed char ADV_REDYELNUM=32 ; const signed char ADV_BLUCYNNUM=33 ; const signed char ADV_GRNMAGNUM=34 ; const signed char ADV_SMOOTH=35 ;
    const signed char ADV_FLASHRED=36 ; const signed char ADV_FLASHGRN=37 ; const signed char ADV_FLASHBLU=38 ;
    const signed char ADV_FLASHREDNUM=39 ; const signed char ADV_FLASHBLUNUM=40 ; const signed char ADV_FLASHGRNNUM=41 ;
    const signed char ADV_FLASHCOL=42 ;

long forcenofilter=1;
long fileid; long fhid; long fhhead; long fhand; struct FileRequester* savereq; struct FileRequester* filereq; char* scrfile; long okay; char* headerfile;
struct FileRequester* arexxreq;
long cm; long os3=FALSE; char* filetype; char* msg;
long h; long z; long x; long y; long b; long l; long k; long j; long i; long m; long col; long bin; long blk; long a; long attr; long red; long grn; long blu; long dot;
short ink[769]; short paper[769]; short best[9];
char* drw; char* fname;
char* verstring; long saveformat=0; long checksum;
char headerbytes[23]; char zx82header[12]; long headcheck;
long thr=20; long smooth=1; char* endmsg; long rompal=0;
long templ; long rdargs; long* rargs=NULL;
struct WBArg* wbarg; struct DiskObject* dobj; long toolarray; long tt; struct WBStartup* wbenchMsg;
long bright; short brightattr[769]; long availpix; long brthr=450; long wtthr=200;
long scale=1; long quiet=0; struct Screen* mysc=NULL; long chg=NULL;
char* wintit; long tbar; struct Window* wptr=NULL;
long flashred=-1; long flashgrn=-1; long flashblu=-1; short flash[769];
long grnmag=0; long blucyn=20; long redyel=40; long thr3; long thr4; char* filetypeerror;
char* filename; long lock=NULL; long filebuf; char* tmpstr;
long o;                         // Datatypes object
struct BitMap* dbm=NULL;     // Destination BitMap
struct BitMap* rbm=NULL;     // DT Remapped Destination BitMap
char ppmmem[147456];                    // Memory pointer to PPM data when finished
struct DataType* deetee;           // Another Datatype thingy
struct BitMapHeader bmh[1]; void* bm[1]; //bitmap
long gads[50];
struct Gadget* glist[1];
long menustrip=NULL; long vi=NULL;
char* loaddrw;
struct MsgPort* appwinport=NULL; long appwin=NULL; struct AppMessage* appwinmsg;
long advopts=0; long ppm=NULL;
struct Gadget* advglist[1];
long pixsep=5; struct RastPort* rp; struct ViewPort* vp; struct ColorMap* cmap;
char* specname; struct Library* libver;
long testmode=0; long mvscreen=0; long flashcol=-1; long clipboard=0; long clipunit=0;
struct MsgPort* arexxport=NULL; struct Window* pwptr=NULL; struct RastPort* pwrp; long pwtbar;
struct Window* curwin; long askover=0;
char* objname; char* objauth; char* objver; char* objanno; char* objcopy;
char objname2[256]; char objauth2[256]; char objcopy2[256]; char objanno2[256]; char objver2[256]; short tzxarray[3];
long nastyworkaround=0;
long writeheader=1; long tzxborder=0; long nofilter=0; long noprogress=0; long allelseink=1;
long autonaming=0; long noextscale=0; long snapshotwindows=0; long winx=0; long winy=-1;
long flashpix=0; long gifsave=0; long gifavailable=0; char* gifname;
long obtainbest=0; long saveattr=1; long dither=0; long simpleremap=0; long dtype=0; long range=0; long lowval=255;
long acontrast=0; long remapafterdither=0; long aslx=0; long asly=0;
struct Library* rexxsysbase;
long* tempGlobal_ILIST; long* tempGlobal_ILIST2; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct2; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct3; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct4; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct5; long* tempGlobal_ILIST3; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct6; long* tempGlobal_ILIST4; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct7; long* tempGlobal_ILIST5; long* tempGlobal_ILIST6; long* tempGlobal_ILIST7; long* tempGlobal_ILIST8; long* tempGlobal_ILIST9; struct pdtScale* tempGlobal_ARRAY_OF_pdtscale; struct pdtBlitPixelArray* tempGlobal_ARRAY_OF_pdtblitpixelarray; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct8; long* tempGlobal_ILIST10; struct NewGadget* tempGlobal_ARRAY_OF_newgadget; struct TagItem* tempGlobal_ARRAY_OF_tagitem; struct NewGadget* tempGlobal_ARRAY_OF_newgadget2; long* tempGlobal_ILIST11; struct NewGadget* tempGlobal_ARRAY_OF_newgadget3; long* tempGlobal_ILIST12; struct NewGadget* tempGlobal_ARRAY_OF_newgadget4; long* tempGlobal_ILIST13; struct NewGadget* tempGlobal_ARRAY_OF_newgadget5; long* tempGlobal_ILIST14; long* tempGlobal_ILIST15; long* tempGlobal_ILIST16; struct NewGadget* tempGlobal_ARRAY_OF_newgadget6; long* tempGlobal_ILIST17; struct NewGadget* tempGlobal_ARRAY_OF_newgadget7; long* tempGlobal_ILIST18; struct NewGadget* tempGlobal_ARRAY_OF_newgadget8; long* tempGlobal_ILIST19; struct NewGadget* tempGlobal_ARRAY_OF_newgadget9; long* tempGlobal_ILIST20; struct NewGadget* tempGlobal_ARRAY_OF_newgadget10; long* tempGlobal_ILIST21; struct NewMenu* tempGlobal_ARRAY_OF_newmenu; long* tempGlobal_ILIST22; struct TagItem* tempGlobal_ARRAY_OF_tagitem2; struct TagItem* tempGlobal_ARRAY_OF_tagitem3; long* tempGlobal_ILIST23; long* tempGlobal_ILIST24; long* tempGlobal_ILIST25; long* tempGlobal_ILIST26; long* tempGlobal_ILIST27; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct9; long* tempGlobal_ILIST28; long* tempGlobal_ILIST29; long* tempGlobal_ILIST30; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct10; long* tempGlobal_ILIST31; long* tempGlobal_ILIST32; long* tempGlobal_ILIST33; long* tempGlobal_ILIST34; long* tempGlobal_ILIST35; long* tempGlobal_ILIST36; long* tempGlobal_ILIST37; long* tempGlobal_ILIST38; long* tempGlobal_ILIST39; long* tempGlobal_ILIST40; long* tempGlobal_ILIST41; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct11; long* tempGlobal_ILIST42; long* tempGlobal_ILIST43; long* tempGlobal_ILIST44; long* tempGlobal_ILIST45; long* tempGlobal_ILIST46; long* tempGlobal_ILIST47; long* tempGlobal_ILIST48; long* tempGlobal_ILIST49; long* tempGlobal_ILIST50; long* tempGlobal_ILIST51; long* tempGlobal_ILIST52; long* tempGlobal_ILIST53; long* tempGlobal_ILIST54; long* tempGlobal_ILIST55; long* tempGlobal_ILIST56; long* tempGlobal_ILIST57; long* tempGlobal_ILIST58; long* tempGlobal_ILIST59; long* tempGlobal_ILIST60; long* tempGlobal_ILIST61; long* tempGlobal_ILIST62; long* tempGlobal_ILIST63; long* tempGlobal_ILIST64; long* tempGlobal_ILIST65; long* tempGlobal_ILIST66; long* tempGlobal_ILIST67; long* tempGlobal_ILIST68; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct12; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct13; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct14; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct15; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct16; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct17; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct18; long* tempGlobal_ILIST69; long* tempGlobal_ILIST70; long* tempGlobal_ILIST71; long* tempGlobal_ILIST72; long* tempGlobal_ILIST73; long* tempGlobal_ILIST74; long* tempGlobal_ILIST75; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct19; long* tempGlobal_ILIST76; long* tempGlobal_ILIST77; long* tempGlobal_ILIST78; long* tempGlobal_ILIST79; long* tempGlobal_ILIST80; long* tempGlobal_ILIST81; long* tempGlobal_ILIST82; long* tempGlobal_ILIST83; long* tempGlobal_ILIST84; long* tempGlobal_ILIST85; long* tempGlobal_ILIST86; long* tempGlobal_ILIST87; long* tempGlobal_ILIST88; long* tempGlobal_ILIST89; long* tempGlobal_ILIST90; long* tempGlobal_ILIST91; long* tempGlobal_ILIST92; long* tempGlobal_ILIST93; long* tempGlobal_ILIST94; long* tempGlobal_ILIST95; long* tempGlobal_ILIST96; long* tempGlobal_ILIST97; long* tempGlobal_ILIST98; long* tempGlobal_ILIST99; long* tempGlobal_ILIST100; long* tempGlobal_ILIST101; long* tempGlobal_ILIST102; long* tempGlobal_ILIST103; long* tempGlobal_ILIST104; long* tempGlobal_ILIST105; long* tempGlobal_ILIST106; long* tempGlobal_ILIST107; long* tempGlobal_ILIST108; long* tempGlobal_ILIST109; long* tempGlobal_ILIST110; long* tempGlobal_ILIST111; long* tempGlobal_ILIST112; long* tempGlobal_ILIST113; struct NewGadget* tempGlobal_ARRAY_OF_newgadget11; long* tempGlobal_ILIST114; struct NewGadget* tempGlobal_ARRAY_OF_newgadget12; long* tempGlobal_ILIST115; struct NewGadget* tempGlobal_ARRAY_OF_newgadget13; long* tempGlobal_ILIST116; struct NewGadget* tempGlobal_ARRAY_OF_newgadget14; long* tempGlobal_ILIST117; struct NewGadget* tempGlobal_ARRAY_OF_newgadget15; long* tempGlobal_ILIST118; struct NewGadget* tempGlobal_ARRAY_OF_newgadget16; long* tempGlobal_ILIST119; struct NewGadget* tempGlobal_ARRAY_OF_newgadget17; long* tempGlobal_ILIST120; struct NewGadget* tempGlobal_ARRAY_OF_newgadget18; long* tempGlobal_ILIST121; struct NewGadget* tempGlobal_ARRAY_OF_newgadget19; long* tempGlobal_ILIST122; struct NewGadget* tempGlobal_ARRAY_OF_newgadget20; long* tempGlobal_ILIST123; struct NewGadget* tempGlobal_ARRAY_OF_newgadget21; long* tempGlobal_ILIST124; struct NewGadget* tempGlobal_ARRAY_OF_newgadget22; long* tempGlobal_ILIST125; struct NewGadget* tempGlobal_ARRAY_OF_newgadget23; long* tempGlobal_ILIST126; struct NewGadget* tempGlobal_ARRAY_OF_newgadget24; long* tempGlobal_ILIST127; struct NewGadget* tempGlobal_ARRAY_OF_newgadget25; long* tempGlobal_ILIST128; struct NewGadget* tempGlobal_ARRAY_OF_newgadget26; long* tempGlobal_ILIST129; struct NewGadget* tempGlobal_ARRAY_OF_newgadget27; long* tempGlobal_ILIST130; struct NewGadget* tempGlobal_ARRAY_OF_newgadget28; long* tempGlobal_ILIST131; struct EasyStruct* tempGlobal_ARRAY_OF_easystruct20; struct TagItem* tempGlobal_ARRAY_OF_tagitem4; long* tempGlobal_ILIST132; long* tempGlobal_ILIST133;
int main(int argc, char** argv) ;
BOOLEAN ppmtoscr();
long mapcol(long offset, long binary);
void randomdither();
void ditherchoosecol(long line, long binary);
void ditherremap();
void stdchoosecol() /*(red,grn,blu)*/;
void os3setup();
BOOLEAN choosecol();
void joemackay();
void init();
BOOLEAN identifyfile();
long dt2ppm(long scaleornot);
void freeup();
void autocontrast();
void readtooltypes(long fromcli=0);
void gadgets();
void menus();
void openwin();
void askload();
void askoutput();
void autoname(long updategadgets=0, long extonly=0);
void asksave();
void handle_gadgets(long noarexx=0);
void advancedoptions();
void advgadgets();
void saveconfig();
BOOLEAN aslhookfunc(long hook, struct FileRequester* fr, struct AnchorPath* frobj);
void about();
void openprogresswin();
void setprogresswin(long percent);
void closeprogresswin();
void arexxecute();
void drawcross();
void drawflashcolour();
void new_dt2scr2_asl_aros() ;
void end_dt2scr2_asl_aros() ;
    

/*
#define PDTA_DESTMODE (DTA_DUMMY+251)
#define PMODE_V43 (1)
#define PDTA_MAXDITHERPENS (DTA_DUMMY+221)
*/

int main(int argc, char** argv)  {
	char* temp_ARRAY_OF_CHAR=NULL; char* temp_ARRAY_OF_CHAR2=NULL;


//finish:


main_argc = argc;


//finish:


main_argv = argv;


//finish:


try {
	new_base();
	new_base2();
	new_fastmem_emulated();
	new_dos();
	new_serial();
	new_parallel();
	new_timer();
	new_monitor();
	new_rxslib();
	new_dos2();
	new_asl();
	new_dos3();
	new_gfxbase();
	new_arexx();
	new_graphics();
	new_classusr();
	new_intuition();
	new_workbench();
	new_datatypesclass();
	new_gadtools();
	new_textclass();
	new_soundclass();
	new_amigaguideclass();
	new_pictureclass();
	new_intuition2();
	new_animationclass();
	new_icon();
	new_intuition3();
	new_wb();
	new_dt2scr2_asl_aros();

Rnd(-1);

StrCopy(verstring,(char*) "$VER: PDHFIC 3.3 (05.10.2009)");

// StrCopy(endmsg,'Conversion Failed!')

rargs=tempGlobal_ILIST ;
templ=(long) "DTFILE/A,SCRFILE,FORM=SAVEFORMAT/K,COL=COLOURSENSE/K/N,BRT=BRIGHTSENSE/K/N,WHT=WHITESENSE/K/N,GRNMAG=GREENMAGENTA/K/N,BLUCYN=BLUECYAN/K/N,REDYEL=REDYELLOW/K/N,RED=FLASHRED/K/N,GRN=FLASHGREEN/K/N,BLU=FLASHBLUE/K/N,NOSCALE/S,GREYSCALE/S,NOBRIGHT/S,NOSMOOTH/S,OS=ROMREMAP/S,ALTPAL=ALTROMPALETTE/S,NOHEADER/S,NOATTR=NOATTRIBUTES/S,COD=DITHER/S,METHOD=DITHERTYPE/K,CLIPBOARD/S,NOCFG=NOCONFIGFILE/S,QUIET/S";

// WriteF('\s\n',rargs)

    if( (long) (DataTypesBase=OpenLibrary("datatypes.library" ,(ULONG) 39 ))==(long) 0 ) { Raise(ERR_LIB);}

    if( (long) (rexxsysbase=OpenLibrary("rexxsyslib.library" ,(ULONG) 37 ))==(long) 0 ) { Raise(ERR_LIB);}

 //   IF (ppmmem:=AllocMem(147456, MEMF_CLEAR))=NIL THEN Raise(ERR_MEM)
    /* Note: 147456 = 256*192*3 bytes (enuff room for PPM sans header) */
    if( (fileid=(long) AllocMem((ULONG) 11 ,(ULONG) MEMF_CLEAR ))==(long) NULL ) { Raise(ERR_MEM);}

tzxarray[0]=1;
tzxarray[1]=1;
tzxarray[2]=0;

if( (void*) wbmessage==NULL) {

rdargs=(long) ReadArgs((char*) templ ,(IPTR*) rargs ,(struct RDArgs*) NULL );

 if( rdargs) {

if( rargs[23]==(long) NULL) {
//    readtooltypes(1)
}
    
if( rargs[19] ) { saveattr=0;}

if( rargs[20]) {
    clipboard=1;
    if( rargs[0] ) { clipunit=Val((char*) rargs[0]);}
    StrCopy(filename,(char*) "Clipboard (Unit ");
    StrAdd(filename,RealF(tmpstr,(float) clipunit,0));
    StrAdd(filename,(char*) ")");
} else {
    if( rargs[0] ) { StrCopy(filename,(char*) rargs[0]);}
}
if( rargs[2]) {
    if( (char*) rargs[2]) {
    temp_ARRAY_OF_CHAR = UpperStr((char*) rargs[2]) ;
    } else { 
    temp_ARRAY_OF_CHAR = (char*) rargs[2];
    }
    temp_ARRAY_OF_CHAR ;
    if( StrCmp((char*) rargs[2],(char*) "SCR") ) { saveformat=0;}
    if( StrCmp((char*) rargs[2],(char*) "ZX82") ) { saveformat=1;}
    if( StrCmp((char*) rargs[2],(char*) "BYTES") ) { saveformat=2;}
    if( StrCmp((char*) rargs[2],(char*) "TAP") ) { saveformat=3;}
    if( StrCmp((char*) rargs[2],(char*) "TZX") ) { saveformat=4;}
    if( StrCmp((char*) rargs[2],(char*) "PLUS3") ) { saveformat=5;}
    if( StrCmp((char*) rargs[2],(char*) "GIF")) {
        saveformat=0;
        gifsave=1;
    }
}

if( rargs[1]) {
    StrCopy(scrfile,(char*) rargs[1]);
} else {
    autonaming=1;
    autoname();
}

if( rargs[3] ) { thr=(*(long*) rargs[3] );}
if( rargs[4] ) { brthr=(*(long*) rargs[4] );}
if( rargs[5] ) { wtthr=(*(long*) rargs[5] );}

if( rargs[6] ) { grnmag=(*(long*) rargs[6] );}
if( rargs[7] ) { blucyn=(*(long*) rargs[7] );}
if( rargs[8] ) { redyel=(*(long*) rargs[8] );}

if( rargs[9] ) { flashred=(*(long*) rargs[9] );}
if( rargs[10] ) { flashgrn=(*(long*) rargs[10] );}
if( rargs[11] ) { flashblu=(*(long*) rargs[11] );}
if( rargs[12]==TRUE ) { scale=0;}
if( rargs[13]==TRUE ) { thr=5000;}
if( rargs[14]==TRUE ) { brthr=5000;}
if( rargs[15]==TRUE ) { smooth=0;}
if( rargs[16]==TRUE ) { os3=TRUE;}
if( rargs[17]==TRUE ) { rompal=TRUE;}
if( rargs[18]==TRUE ) { writeheader=0;}
if( rargs[20]==TRUE ) { dither=1;}
if( rargs[21]) {
    if( (char*) rargs[21]) {
    temp_ARRAY_OF_CHAR2 = UpperStr((char*) rargs[21]) ;
    } else { 
    temp_ARRAY_OF_CHAR2 = (char*) rargs[21];
    }
    temp_ARRAY_OF_CHAR2 ;
    if( StrCmp((char*) rargs[21],(char*) "RANDOM") ) { dtype=1;}
}

if( rargs[24]==TRUE ) { quiet=TRUE;}
FreeArgs((struct RDArgs*) rdargs );

} else {
  Raise(ERR_ARGS);
}

if( quiet==0) {
  WriteF((char*) "PDHFIC 3.3\nby Chris Young <chris@unsatisfactorysoftware.co.uk>\n\251 1998-2001, 2008-2009 Unsatisfactory Software\n\n");
if( os3!=FALSE ) { WriteF((char*) "You are set to run in OS3 mode...\n\n");}
}

os3setup();
joemackay();
ppmtoscr();


} else {  /* started from wb */

    if( (filebuf=(long) AllocMem((ULONG) (long) 1024 ,(ULONG) MEMF_CLEAR ))==(long) NULL ) { Raise(ERR_MEM);}


    if( (long) (AslBase=OpenLibrary("asl.library" ,(ULONG) 37 ))==(long) 0 ) { Raise(ERR_ASL);}
    if( (void*) (IconBase=OpenLibrary("icon.library" ,(ULONG) 33 ))==NULL ) { Raise(ERR_ICON);}
    if( (void*) (GadToolsBase=OpenLibrary("gadtools.library" ,(ULONG) 37 ))==NULL ) { Raise(ERR_GTLS);}
    if( (void*) (WorkbenchBase=OpenLibrary("workbench.library" ,(ULONG) 37 ))==NULL ) { Raise(ERR_WBLB);}
//    alsbase:=OpenLibrary('als.library',6)

arexxreq=(struct FileRequester*) AllocAslRequest((ULONG) ASL_FileRequest ,(struct TagItem*) tempGlobal_ILIST2  );

    readtooltypes();
    os3setup();
    openwin();


/*
libver:=alsbase

WriteF('\d.\d \s\n',libver.version,libver.revision,libver.idstring)
*/

}


//finish:


} catch(...) {}

    if( exception) {
//      IF quiet=0 THEN WriteF('*** ERROR ***\n')
    
    switch( exception) {
    case ERR_FILE : StrCopy(endmsg,(char*) "Couldn\'t open file!");
    	break;
    case ERR_DTYP : StrCopy(endmsg,(char*) "Datatypes error!");
    	break;
    case ERR_NPIC : StrCopy(endmsg,filetypeerror);
    	break;
    case ERR_MEM  : StrCopy(endmsg,(char*) "Not enough memory!");
    	break;
    case ERR_BTMP : StrCopy(endmsg,(char*) "Could not allocate BitMap!");
    	break;
    case ERR_LIB  : StrCopy(endmsg,(char*) "Could not open datatypes.library 39+");
    	break;
     case ERR_ABOR : StrCopy(endmsg,(char*) "Aborted by user!");
     	break;
     case ERR_ASL  : StrCopy(endmsg,(char*) "Could not open asl.library 37+");
     	break;
    case ERR_ICON : StrCopy(endmsg,(char*) "Could not open icon.library 33+");
    	break;
    case ERR_ICNN : StrCopy(endmsg,(char*) "Could not open icon.library 36+");
    	break;
    case ERR_GTLS : StrCopy(endmsg,(char*) "Could not open gadtools.library 37+");
    	break;
    case ERR_ARGS : StrCopy(endmsg,(char*) "required argument missing");
    	break;
    case ERR_WBLB : StrCopy(endmsg,(char*) "Could not open workbench.library 37+");
    	break;
    case QuadChara('D','O','U','B'):
        handle_gadgets(1);
        StrCopy(endmsg,(char*) "AREXX");
    	break;
    case QuadChara(0,'S','I','G'):
        handle_gadgets(1);
        StrCopy(endmsg,(char*) "AREXX");
    	break;
    case QuadChara(0,'M','E','M')    : StrCopy(endmsg,(char*) "Not enough memory!");
    	break;
	default:
		StrCopy(endmsg,(char*) "Exception occured - Unknown error\n");
    	break;
    }
    
    if( StrCmp(endmsg,(char*) "AREXX")==0) {
         if( (void*) wbmessage==NULL) {
           if( quiet==0 ) { WriteF((char*) "%s\n",(long) endmsg);}
         } else {
           tempGlobal_ARRAY_OF_easystruct [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
           tempGlobal_ARRAY_OF_easystruct [0].es_TextFormat = (CONST_STRPTR) (long) endmsg;
           EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct  ,(ULONG*) 0 ,(APTR) (long) NULL );
         }

    }
}


if( arexxreq ) { FreeAslRequest((APTR) (long) arexxreq );}
    if( filereq ) { FreeAslRequest((APTR) (long) filereq );}
    if( savereq ) { FreeAslRequest((APTR) (long) savereq );}
    if( o ) { DisposeDTObject((Object*) o );}
    if( dbm ) { FreeBitMap(dbm );}
    if( rbm ) { FreeBitMap(rbm );}
    if( deetee ) { ReleaseDataType(deetee );}
    if( lock   ) { (BOOL)(0!=Close((BPTR) lock ));}
lock=(long) NULL;

if( cm ) { FreeColorMap((struct ColorMap*) cm );}
if( arexxport ) { rx_ClosePort(arexxport);}
if( fileid ) { FreeMem((APTR) fileid ,(ULONG) 11 );}
//IF ppmmem THEN FreeMem(ppmmem, 147456)
if( filebuf ) { FreeMem((APTR) filebuf ,(ULONG) (long) 1024 );}
if( flashcol!=-1 ) { ReleasePen(cmap ,(ULONG) flashcol );}

if( appwinport) {
    while( appwinmsg=(struct AppMessage*) GetMsg(appwinport )) { ReplyMsg((struct Message*) appwinmsg );}
    DeleteMsgPort(appwinport );
}

if( appwin ) { -RemoveAppWindow((struct AppWindow*) appwin );}

if( wptr ) { ClearMenuStrip(wptr );}

if( wptr ) { CloseWindow(wptr );}

// RemoveGList(wptr,gads[ADV_BRIGHT],-1)
/*
IF glist
   FreeGadgets(glist)
   FreeGadgets(advglist)
   RemoveGList(wptr,gads[GADG_LOAD],-1)
ENDIF
*/
// 



// IF menustrip THEN FreeMenus(menustrip)
if( vi ) { FreeVisualInfo((APTR) vi );}
if( mysc ) { UnlockPubScreen((UBYTE*) NULL ,mysc );}

if( WorkbenchBase ) { CloseLibrary(WorkbenchBase );}
if( GadToolsBase ) { CloseLibrary(GadToolsBase );}
if( rexxsysbase ) { CloseLibrary(rexxsysbase );}
if( DataTypesBase ) { CloseLibrary(DataTypesBase );}
if( IconBase ) { CloseLibrary(IconBase );}
if( AslBase ) { CloseLibrary(AslBase );}
	end_dt2scr2_asl_aros();
	end_intuition2();
	end_intuition();
	end_graphics();
	end_arexx();
	end_dos2();
	end_dos();
	end_fastmem_emulated();
	if (exception==-1) return (int) exceptionInfo; //finish the CleanUp() call
//IF alsbase THEN CloseLibrary(alsbase)

// CleanUp()

}

BOOLEAN ppmtoscr() {
long o2=50, p=0, tzxstrs, tzxlens; char* specname2=NULL;
BOOLEAN temp_BOOL; long temp_QUAD;
try {
	specname2 = NewString(256);
	temp_QUAD = exception ;
	exception = 0 ;

      StrCopy(wintit,(char*) "Writing file header...");
      if( (void*) wbmessage!=NULL ) { SetWindowTitles(curwin ,wintit ,(char*) -1 );}
if( quiet==0 ) { WriteF((char*) "\nOutput File: %s\n",(long) scrfile);}

StrCopy(specname2,scrfile);
specname=FilePart(specname2 );
dot=InStr(specname,(char*) ".");
if( dot!=-1 ) { StrCopy(specname,specname,dot);}
StrAdd(specname,(char*) "          ");

if( gifsave) {
    StrCopy(gifname,scrfile);
//    StrCopy(scrfile,'T:PDHFIC.tmp')
    dot=InStr(scrfile,(char*) ".");
    if( dot==(long) NULL ) { StrCopy(scrfile,gifname) ;} else { StrCopy(scrfile,gifname,dot);}
    StrAdd(scrfile,(char*) ".scr");
            if( (lock=(long) Lock(scrfile ,ACCESS_READ ))) {
                (BOOL)(0!=Close((BPTR) lock ));
                lock=(long) NULL;
                tempGlobal_ARRAY_OF_easystruct2 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
                if( EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct2  ,(ULONG*) 0 ,(APTR) (long) NULL )==0) {
                    StrCopy(scrfile,gifname);
                    temp_BOOL = -1;
                    Raise(0);
                }
            }
}

if( saveformat==0) {
 if( gifsave) {
  if( quiet==0 ) { WriteF((char*) "Save Format: GIF\n");}
 } else {
  if( quiet==0 ) { WriteF((char*) "Save Format: Raw SCR\n");}
 }
}

if( saveformat==2) {
   if( quiet==0 ) { WriteF((char*) "Save Format: .header/.bytes\n");}
   dot=InStr(scrfile,(char*) ".");
if( dot==(long) NULL ) { StrCopy(headerfile,scrfile) ;} else { StrCopy(headerfile,scrfile,dot);}
   StrAdd(headerfile,(char*) ".header");
//   dot:=InStr(savereq.file,'.')
//   StrCopy(specname,savereq.file,dot)
}

if( saveformat==3) {
  if( quiet==0 ) { WriteF((char*) "Save Format: Z80 Tape Image\n");}
}

if( saveformat==1) {
  if( quiet==0 ) { WriteF((char*) "Save Format: ZX82\n");}
}

if( saveformat==4) {
  if( quiet==0 ) { WriteF((char*) "Save Format: TZX (ZX Tape)\n");}
}

if( saveformat==5) {
  if( quiet==0 ) { WriteF((char*) "Save Format: +3DOS\n");}
}

headcheck=0;

headerbytes[0]=0x13  /* .tap only */;
headerbytes[1]=0x00  /* .tap only */;
headerbytes[2]=0x00;
headerbytes[3]=0x03;

// headcheck:=$F8
headerbytes[4]=specname[0];
headerbytes[5]=specname[1];
headerbytes[6]=specname[2];
headerbytes[7]=specname[3];
headerbytes[8]=specname[4];
headerbytes[9]=specname[5];
headerbytes[10]=specname[6];
headerbytes[11]=specname[7];
headerbytes[12]=specname[8];
headerbytes[13]=specname[9];

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

headerbytes[14]=0x00 /* length vv*/;
if( saveattr ) { headerbytes[15]=0x1B ;} else { headerbytes[15]=0x18;}
headerbytes[16]=0x00;
headerbytes[17]=0x40;
headerbytes[18]=0x20;
headerbytes[19]=0x80;
/* headerbytes[20]:=$EC *//* eor of values 2 to 19 */
headerbytes[21]=0x02 /* .tap only */;
if( saveattr ) { headerbytes[22]=0x1B ;} else { headerbytes[22]=0x18 /* .tap only */;}

if( saveformat==2) {
 if( writeheader) {
    if( (lock=(long) Lock(headerfile ,ACCESS_READ ))) {
        (BOOL)(0!=Close((BPTR) lock ));
        lock=(long) NULL;
        tempGlobal_ARRAY_OF_easystruct3 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
        if( EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct3  ,(ULONG*) 0 ,(APTR) (long) NULL )) {
          if( (fhhead=(long) Open(headerfile ,MODE_NEWFILE ))) {
             if( quiet==0 ) { WriteF((char*) "Writing %s file...\n",(long) headerfile);}
             for( h=2 ; h<=(long) 19 /* 20 */; h++) {
                 headcheck=headcheck ^ (long) headerbytes[h];
                 FPutC((BPTR) fhhead ,headerbytes[h] );
             }
             FPutC((BPTR) fhhead ,headcheck );
             (BOOL)(0!=Close((BPTR) fhhead ));
          }
         }
    }
}
}


if( (fhand=(long) Open(scrfile ,MODE_NEWFILE ))) {

if( saveformat==3) {
  if( quiet==0 ) { WriteF((char*) "Writing TAP header...\n");}
     for( h=0 ; h<=(long) 1; h++) {
     FPutC((BPTR) fhand ,headerbytes[h] );
     }
     for( h=2 ; h<=(long) 19; h++) {
     FPutC((BPTR) fhand ,headerbytes[h] );
     headcheck=headcheck ^ (long) headerbytes[h];
     }
     FPutC((BPTR) fhand ,headcheck );
     for( h=21 ; h<=(long) 22; h++) {
     FPutC((BPTR) fhand ,headerbytes[h] );
     }
}

if( saveformat==5) {
  if( quiet==0 ) { WriteF((char*) "Writing +3DOS header...\n");}
    FPuts((BPTR) fhand ,"PLUS3DOS" );
    FPutC((BPTR) fhand ,26 );
    FPutC((BPTR) fhand ,01 );
    FPutC((BPTR) fhand ,00 );

    headcheck=0x6A;

/*    IF saveattr THEN */
    FPutC((BPTR) fhand ,0x80 );
    headcheck=headcheck ^ (long) 0x80;

    FPutC((BPTR) fhand ,headerbytes[15] );
    headcheck=headcheck ^ (long) headerbytes[15];

    /* write header sans filename */
    // FOR h:=2 TO 3   [2] is part of the tap format, so not required
        FPutC((BPTR) fhand ,headerbytes[3] );
        headcheck=headcheck ^ (long) headerbytes[3];
    // ENDFOR
    for( h=14 ; h<=(long) 19; h++) {
        FPutC((BPTR) fhand ,headerbytes[h] );
        headcheck=headcheck ^ (long) headerbytes[h];
    }
    /* apparently this is supposed to be seven bytes but it, erm, isn't */
    FPutC((BPTR) fhand ,0 );
        headcheck=headcheck ^ (long) 0;
    for( h=1 ; h<=(long) 104; h++) {
        FPutC((BPTR) fhand ,0 );
        headcheck=headcheck ^ (long) 0;
    }
    FPutC((BPTR) fhand ,headcheck );

}

if( saveformat==4) {
  if( quiet==0 ) { WriteF((char*) "Writing TZX header...\n");}
    FPuts((BPTR) fhand ,"ZXTape!" );
    FPutC((BPTR) fhand ,0x1A );
    FPutC((BPTR) fhand ,01 );
    FPutC((BPTR) fhand ,12 );

if( tzxarray[0]) {

    /* archive info */
    
//    objname:=FilePart(filename)
    
    StrCopy(tmpstr,specname,EstrLen(specname)-(long) 10);
    
    tzxstrs=2 ;// one block always available
    tzxlens=(long) 58+EstrLen(tmpstr) ;// length of promo text + 1 + namelength

    if( EstrLen(objcopy)!=(long) NULL) {
        tzxstrs=tzxstrs+(long) 1;
        tzxlens=tzxlens+EstrLen(objcopy);
    }

    if( EstrLen(objauth)!=(long) NULL) {
        tzxstrs=tzxstrs+(long) 1;
        tzxlens=tzxlens+EstrLen(objauth);
    }
    
    if( EstrLen(objanno)!=(long) NULL) {
        // tzxstrs:=tzxstrs+1 (already included)
        tzxlens=tzxlens+EstrLen(objanno)+(long) 1;
    }


    tzxlens=tzxlens+(long) 2*tzxstrs;

/*
    WriteF('total length: \d\n',tzxlens)
    WriteF('no of strings: \d\n',tzxstrs)
*/    
    // no room for objver in tzx 1.11
    
    FPutC((BPTR) fhand ,0x32 );
    FPutC((BPTR) fhand ,tzxlens );
    FPutC((BPTR) fhand ,00 );
    FPutC((BPTR) fhand ,tzxstrs );
    
    FPutC((BPTR) fhand ,0x00 );
    FPutC((BPTR) fhand ,EstrLen(tmpstr) );
    FPuts((BPTR) fhand ,tmpstr );

    if( EstrLen(objcopy)!=(long) NULL) {
        FPutC((BPTR) fhand ,0x01 );
        FPutC((BPTR) fhand ,EstrLen(objcopy) );
        FPuts((BPTR) fhand ,objcopy );
    }

    if( EstrLen(objauth)!=(long) NULL) {
        FPutC((BPTR) fhand ,0x02 );
        FPutC((BPTR) fhand ,EstrLen(objauth) );
        FPuts((BPTR) fhand ,objauth );
    }

// Let's get the blocks in the right order, eh chaps?

    FPutC((BPTR) fhand ,0x08 );
    FPutC((BPTR) fhand ,57 );
    FPuts((BPTR) fhand ,"Converted with PDHFIC v3.3" );
    FPutC((BPTR) fhand ,13 );
    FPuts((BPTR) fhand ,"unsatisfactory.freeserve.co.uk" );

// end of obligatory advert for PDHFIC

    if( EstrLen(objanno)!=(long) NULL) {
        FPutC((BPTR) fhand ,0xFF );
        FPutC((BPTR) fhand ,(EstrLen(objanno)) );
        FPuts((BPTR) fhand ,objanno );
    }
    


} // End Archive Info

if( tzxarray[1]) {

    /*** start of ID/data etc ***/
    FPutC((BPTR) fhand ,0x10 );
    FPutC((BPTR) fhand ,00 );
    FPutC((BPTR) fhand ,10 );
    
    for( h=0 ; h<=(long) 1; h++) {
         FPutC((BPTR) fhand ,headerbytes[h] );
     }
     for( h=2 ; h<=(long) 19; h++) {
         FPutC((BPTR) fhand ,headerbytes[h] );
         headcheck=headcheck ^ (long) headerbytes[h];
     }
     FPutC((BPTR) fhand ,headcheck );
     
        FPutC((BPTR) fhand ,0x10 );
    FPutC((BPTR) fhand ,00 );
    FPutC((BPTR) fhand ,10 );

     for( h=21 ; h<=(long) 22; h++) {
         FPutC((BPTR) fhand ,headerbytes[h] );
     }

}

    if( tzxarray[2] ) {// Custom SCREEN$ block
        FPutC((BPTR) fhand ,0x35 );
        FPuts((BPTR) fhand ,"Spectrum Screen " );
        FPutC((BPTR) fhand ,0x1B );
        FPutC((BPTR) fhand ,0x20 );
        FPutC((BPTR) fhand ,0x00 );
        FPutC((BPTR) fhand ,0x00 );
    if( EstrLen(objanno)!=(long) NULL) {
        FPutC((BPTR) fhand ,(EstrLen(objanno)) );
        FPuts((BPTR) fhand ,objanno );
    } else {
        FPutC((BPTR) fhand ,30 );
        FPuts((BPTR) fhand ,"Screen created with PDHFIC 3.3" );
    }
        FPutC((BPTR) fhand ,tzxborder );
    }

}

if( saveformat==1) {
  if( quiet==0 ) { WriteF((char*) "Writing ZX82 header...\n");}
zx82header[0]=0x5A;
zx82header[1]=0x58;
zx82header[2]=0x38;
zx82header[3]=0x32;
zx82header[4]=0x03;
zx82header[5]=0x00;
if( saveattr ) { zx82header[6]=0x1B ;} else { zx82header[6]=0x18;}
zx82header[7]=0x00;
zx82header[8]=0x40;
zx82header[9]=0x00;
zx82header[10]=0x80;
zx82header[11]=0x00;

     for( h=0 ; h<=(long) 11; h++) {
     FPutC((BPTR) fhand ,zx82header[h] );
     }
}

if( saveformat>1) {
  FPutC((BPTR) fhand ,0xFF );
}

checksum=0xFF;

      StrCopy(wintit,(char*) "Mapping colours/writing image...");
      if( (void*) wbmessage!=NULL ) { SetWindowTitles(curwin ,wintit ,(char*) -1 );}
if( quiet==0 ) { WriteF((char*) "%s\n",(long) wintit);}

/* temp stuff *********************
Fputs(fhand,header)
Fputs(fhand,data)
Flush(fhand)
Close(fhand)
JUMP finish
********************************/

setprogresswin(50);

/*
do i=1 to 147456   ** 71 **

red.i=substr(data,i,1)
grn.i=readch(data,i,1)
blu.i=readch(data,i,1)

end

*/

/* why was this commented out???! */
for( z=1 ; z<=(long) 768; z++) {
ink[z]=7;
paper[z]=0;
brightattr[z]=0;
}


o2=50;

 /* 16 */

/* a:=15 */ /*-172*/
a=0;
x=0;

for( l=1 ; l<=(long) 3; l++) {

for( k=1 ; k<=(long) 8; k++) {

/* rows */


/*********** PROGRESS BAR??????? *******************/

 o2=o2+(long) 2;

setprogresswin(o2);

for( j=1 ; j<=(long) 8; j++) {

for( i=1 ; i<=(long) 32; i++) {


bin=0;

blk=i+x;

// WriteF('\d\n',blk)



/* say (c2d(substr(data,a,3))) */


mapcol(0,0x80/*10000000*/);

  
mapcol(3,0x40/*01000000*/);


mapcol(6,0x20/*00100000*/);


mapcol(9,0x10/*00010000*/);


mapcol(12,0x8/*00001000*/);


mapcol(15,0x4/*00000100*/);


mapcol(18,0x2/*00000010*/);


mapcol(21,0x1/*00000001*/);


FPutC((BPTR) fhand ,bin );

checksum=checksum ^ bin;

a=a+(long) 24;

}

/* jump 224 bytes */

a=a+(long) 5376 /* 5376 */ /* 672? */;

x=x+(long) 32;



}

/* back 192 (224?) x 8 bytes */

a=a-48384   /*  48384  */;

x=x-(long) 256;

/* -(8x8x32x8x3)+(32x8x3) */
/* +576 / remove 32*8*8 not 32*8*1 ????? */
/* 16384*3? -32*8? (12544*3) / 5376?  ->->->  8*8*32*8  *3 ?????????? */

}

/* third of a screen - check this! */

a=a+43008  /*43008*/    /* 43013 */  /* 43007 . . . 42224  was 48384 */;

x=x+(long) 256;

// WriteF('Thirds: \d\n',a)

/*147502    147472 (1)*/     /* say a ** 49167???? **  */

}

if( saveattr) {
      StrCopy(wintit,(char*) "Writing colour attributes...");
      if( quiet==0 ) { WriteF((char*) "%s\n",(long) wintit);}
      if( (void*) wbmessage!=NULL ) { SetWindowTitles(curwin ,wintit ,(char*) -1 );}

    o2=97;
    p=0;

    for( m=1 ; m<=(long) 768; m++) {

        /* check attr() command! */

        /* ink=0
        paper=7 */

    attr=(long) ((short) 1*ink[m])+(long) ((short) 8*paper[m])+(long) brightattr[m]+(long) flash[m];

        // col=d2c(attr)

        FPutC((BPTR) fhand ,attr );

        checksum=checksum ^ attr;

        if( pwptr) {
            p=p+(long) 1;
            if( p>=200) {
                o2=o2+(long) 1;
                p=0;
                setprogresswin(o2);
            }
        }

    }
} else {
    if( saveformat==0) {

      StrCopy(wintit,(char*) "Writing colour attributes...");
      if( quiet==0 ) { WriteF((char*) "%s\n",(long) wintit);}
      if( (void*) wbmessage!=NULL ) { SetWindowTitles(curwin ,wintit ,(char*) -1 );}

    o2=97;
    p=0;

        for( m=1 ; m<=(long) 768; m++) {

            /* ink=0
            paper=7 */

            attr=56;
            FPutC((BPTR) fhand ,attr );

            checksum=checksum ^ attr;

            if( pwptr) {
                p=p+(long) 1;
                if( p>=200) {
                    o2=o2+(long) 1;
                    p=0;
                    setprogresswin(o2);
                }
            }

        }
    }
}

if( pwptr) {
    o2=o2+(long) 5;
    setprogresswin(o2);
}

if( saveformat>1) {
    if( saveformat==4) {
        if( tzxarray[2]==0 ) { FPutC((BPTR) fhand ,checksum );}
    } else {
        FPutC((BPTR) fhand ,checksum );
    }
}

(BOOL)(0!=Close((BPTR) fhand ));


}

if( gifsave) {
      StrCopy(wintit,(char*) "Converting to GIF...");
      if( (void*) wbmessage!=NULL ) { SetWindowTitles(curwin ,wintit ,(char*) -1 );}
      if( quiet==0 ) { WriteF((char*) "%s\n",(long) wintit);}
    StrCopy(tmpstr,(char*) "SCR2GIF ");
    if( quiet==1 ) { StrAdd(tmpstr,(char*) ">NIL: ");}
    if( flashpix>0 ) { StrAdd(tmpstr,(char*) "-f ");}
    StrAdd(tmpstr,(char*) "\"");
    StrAdd(tmpstr,scrfile);
    StrAdd(tmpstr,(char*) "\"");
    (BOOL)(0!=Execute(tmpstr ,(BPTR) (long) NULL ,(BPTR) (long) NULL ));
    (BOOL)(0!=DeleteFile(scrfile ));
    StrCopy(scrfile,gifname);
}

if( flashpix>0) {
    StrAdd(filetype,(char*) "\nFLASH pixels: ");
    StrAdd(filetype,RealF(tmpstr,(float) flashpix,0));
}

      StrCopy(wintit,(char*) "PDHFIC");
      if( (void*) wbmessage!=NULL ) { SetWindowTitles(curwin ,wintit ,(char*) -1 );}

if( quiet==0 ) { WriteF((char*) "Conversion done - all OK!\n");}
	temp_BOOL = 0;
} catch(...) {}
	DisposeString(specname2 );
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return temp_BOOL ;

} 


long mapcol(long offset, long binary) {
long tempbin;
/*
MidStr(red,data,a+offset,1)
MidStr(grn,data,a+offset+1,1)
MidStr(blu,data,a+offset+2,1)
*/

if( ((long) - (offset==0 )& k)==1) {
    b=0;

    for( y=0 ; y<=(long) 8; y++) {
        best[y]=0;
    }

    for( y=1 ; y<=(long) 8; y++) {
      for( z=1 ; z<=(long) 8; z++) {

          red=ppmmem[a+b];
          grn=ppmmem[a+b+(long) 1];
          blu=ppmmem[a+b+(long) 2];

            if( acontrast) {
                    red=(red-lowval)*(long) 255/range;
                    grn=(grn-lowval)*(long) 255/range;
                    blu=(blu-lowval)*(long) 255/range;
            }

//  col,bright:=choosecol(red,grn,blu)

            if( remapafterdither) {
                switch( z) {
                case 1:
                    tempbin=0x80/*10000000*/;
                	break;
                case 2:
                    tempbin=0x40/*01000000*/;
                	break;
                case 3:
                    tempbin=0x20/*00100000*/;
                	break;
                case 4:
                    tempbin=0x10/*00010000*/;
                	break;
                case 5:
                    tempbin=0x8/*00001000*/;
                	break;
                case 6:
                    tempbin=0x4/*00000100*/;
                	break;
                case 7:
                    tempbin=0x2/*00000010*/;
                	break;
                case 8:
                    tempbin=0x1/*00000001*/;
                	break;
                }

                ditherchoosecol(y,tempbin);
            } else {
                choosecol();
            }
  
          best[col]=best[col]+(short) 1;
          best[8]=(long) best[8]+bright;
  
          b=b+(long) 3;
      }
    b=b+(long) 744;
    }

    for( y=1 ; y<=(long) 7; y++) {
        if( best[y]>=best[paper[blk]] ) { paper[blk]=y;}
    }

    if( ink[blk]==paper[blk]) {
      ink[blk]=paper[blk]+(short) 1;
      if( ink[blk]==8 ) { ink[blk]=0;}
    }

    for( y=7 ; y>=(long) 0 ; y+=-1) {
        if( y!=paper[blk]) {
              if( best[y]>=best[ink[blk]] ) { ink[blk]=y;}
        }
    }

    if( paper[blk]<ink[blk]) {
        red=paper[blk];
        paper[blk]=ink[blk];
        ink[blk]=red;
    }

    availpix=((short) 64-best[0])/(short) 2;
//    availpix:=(64-best[0])/3
// WriteF('\d/\d\n',best[8],availpix)

    if( best[8]>availpix ) { brightattr[blk]=64 /* ELSE brightattr[blk]:=0 */;}

}

red=ppmmem[a+offset];
grn=ppmmem[a+offset+(long) 1];
blu=ppmmem[a+offset+(long) 2];

if( acontrast) {
    red=(red-lowval)*(long) 255/range;
    grn=(grn-lowval)*(long) 255/range;
    blu=(blu-lowval)*(long) 255/range;
}

// col:=choosecol(red,grn,blu)

if( dither) {
    ditherchoosecol(k,binary) ;// k*l   j
} else {
    choosecol();
}

if( flashred==red) {
  if( flashgrn==grn) {
    if( flashblu==blu) {
      flash[blk]=128;
      flashpix=flashpix+(long) 1;
    }
  }
}

// WriteF('\d \d \d\n',red,grn,blu)


// IF red=grn AND grn=blu
// ENDIF

if( paper[blk]!=col) {
  if( smooth==1) {
    if( labs((long) paper[blk]-col )>labs((long) ink[blk]-col )) { bin=bin+binary;}
// v old ink[blk] := col
  } else {
  if( allelseink) {
      bin=bin+binary;
  } else {
      if( ink[blk]==col ) { bin=bin+binary;}
  }
  }

}
	return col;

} 

void randomdither() {
long random=0;

random=Rnd(255);
if( random>=red ) { red=0;}
if( random>=grn ) { grn=0;}
if( random>=blu ) { blu=0;}

ditherremap();
	return ;
}

void ditherchoosecol(long line, long binary) {
long eline, dull=128, dred, dblu, dgrn;
long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;

if( dtype==1) {
    randomdither();
    Raise(0);
}

if( line ) { eline=- ((line & (long) 1)==0);}

// WriteF('\d \d',binary,eline)

// WriteF('\d,\d,\d = \d\n',red,grn,blu,col)

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

dred=0;
if( - (red>0)  & - (red<64)) {
    red=0;
}
if( - (red>=64)  & - (red<74)) {
    dred=8;
}
if( - (red>=74)  & - (red<84)) {
    dred=1;
}
if( - (red>=84)  & - (red<94)) {
    dred=9;
}
if( - (red>=94)  & - (red<104)) {
    dred=2;
}
if( - (red>=104)  & - (red<114)) {
    dred=11;
}
if( - (red>=114)  & - (red<=142)) {
    dred=3;
}
if( - (red>=142)  & - (red<152)) {
    dred=12;
}
if( - (red>152)  & - (red<=162)) {
    dred=4;
}
if( - (red>162)  & - (red<=172)) {
    dred=10;
}
if( - (red>172)  & - (red<=182)) {
    dred=5;
}
if( - (red>182)  & - (red<192)) {
    dred=7;
}

dgrn=0;
if( - (grn>0)  & - (grn<64)) {
    grn=0;
}
if( - (grn>=64)  & - (grn<74)) {
    dgrn=8;
}
if( - (grn>=74)  & - (grn<84)) {
    dgrn=1;
}
if( - (grn>=84)  & - (grn<94)) {
    dgrn=9;
}
if( - (grn>=94)  & - (grn<104)) {
    dgrn=2;
}
if( - (grn>=104)  & - (grn<114)) {
    dgrn=11;
}
if( - (grn>=114)  & - (grn<=142)) {
    dgrn=3;
}

if( - (grn>=142)  & - (grn<152)) {
    dgrn=12;
}
if( - (grn>152)  & - (grn<=162)) {
    dgrn=4;
}
if( - (grn>162)  & - (grn<=172)) {
    dgrn=10;
}
if( - (grn>172)  & - (grn<=182)) {
    dgrn=5;
}
if( - (grn>182)  & - (grn<192)) {
    dgrn=7;
}

dblu=0;
if( - (blu>0)  & - (blu<64)) {
    blu=0;
}
if( - (blu>=64)  & - (blu<74)) {
    dblu=8;
}
if( - (blu>=74)  & - (blu<84)) {
    dblu=1;
}
if( - (blu>=84)  & - (blu<94)) {
    dblu=9;
}
if( - (blu>=94)  & - (blu<104)) {
    dblu=2;
}
if( - (blu>=104)  & - (blu<114)) {
    dblu=11;
}
if( - (blu>=114)  & - (blu<=142)) {
    dblu=3;
}

if( - (blu>=142)  & - (blu<152)) {
    dblu=12;
}
if( - (blu>152)  & - (blu<=162)) {
    dblu=4;
}
if( - (blu>162)  & - (blu<=172)) {
    dblu=10;
}
if( - (blu>172)  & - (blu<=182)) {
    dblu=5;
}
if( - (blu>182)  & - (blu<192)) {
    dblu=7;
}

// WriteF('\d,\d,\d - \d\n',red,grn,blu,eline)

switch( binary) {
case 0x80/*10000000*/:
    if( eline) {
        if( dred==3 ) { red=0;}
        if( dgrn==3 ) { grn=0;}
        if( dblu==3 ) { blu=0;}
        if( dred==4 ) { red=0;}
        if( dgrn==4 ) { grn=0;}
        if( dblu==4 ) { blu=0;}
        if( dred==11 ) { red=0;}
        if( dgrn==11 ) { grn=0;}
        if( dblu==11 ) { blu=0;}
    } else {
        if( dred==12 ) { red=0;}
        if( dgrn==12 ) { grn=0;}
        if( dblu==12 ) { blu=0;}
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
        if( dred==6 ) { red=0;}
        if( dgrn==6 ) { grn=0;}
        if( dblu==6 ) { blu=0;}
    }
    if( - (line==1)  | - (line==5)) {
        if( dred==5 ) { red=0;}
        if( dgrn==5 ) { grn=0;}
        if( dblu==5 ) { blu=0;}
        if( dred==10 ) { red=0;}
        if( dgrn==10 ) { grn=0;}
        if( dblu==10 ) { blu=0;}
    } else {
        if( dred==8 ) { red=0;}
        if( dgrn==8 ) { grn=0;}
        if( dblu==8 ) { blu=0;}
        if( dred==1 ) { red=0;}
        if( dgrn==1 ) { grn=0;}
        if( dblu==1 ) { blu=0;}
        if( dred==9 ) { red=0;}
        if( dgrn==9 ) { grn=0;}
        if( dblu==9 ) { blu=0;}
    }
	break;

case 0x40/*01000000*/:
    if( eline) {
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
        if( dred==6 ) { red=0;}
        if( dgrn==6 ) { grn=0;}
        if( dblu==6 ) { blu=0;}
    } else {
        if( dred==3 ) { red=0;}
        if( dgrn==3 ) { grn=0;}
        if( dblu==3 ) { blu=0;}
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
        if( dred==11 ) { red=0;}
        if( dgrn==11 ) { grn=0;}
        if( dblu==11 ) { blu=0;}
    }
        if( dred==8 ) { red=0;}
        if( dgrn==8 ) { grn=0;}
        if( dblu==8 ) { blu=0;}
        if( dred==1 ) { red=0;}
        if( dgrn==1 ) { grn=0;}
        if( dblu==1 ) { blu=0;}
        if( dred==9 ) { red=0;}
        if( dgrn==9 ) { grn=0;}
        if( dblu==9 ) { blu=0;}
    if( line==4) {
        if( dred==11 ) { red=0;}
        if( dgrn==11 ) { grn=0;}
        if( dblu==11 ) { blu=0;}
    }
    if( - (line==2)  | - (line==6)) {
        if( dred==12 ) { red=0;}
        if( dgrn==12 ) { grn=0;}
        if( dblu==12 ) { blu=0;}
    }

	break;

case 0x20/*00100000*/:
    if( eline) {
        if( dred==4 ) { red=0;}
        if( dgrn==4 ) { grn=0;}
        if( dblu==4 ) { blu=0;}
        if( dred==3 ) { red=0;}
        if( dgrn==3 ) { grn=0;}
        if( dblu==3 ) { blu=0;}
        if( dred==11 ) { red=0;}
        if( dgrn==11 ) { grn=0;}
        if( dblu==11 ) { blu=0;}
    } else {
        if( dred==12 ) { red=0;}
        if( dgrn==12 ) { grn=0;}
        if( dblu==12 ) { blu=0;}
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
        if( dred==6 ) { red=0;}
        if( dgrn==6 ) { grn=0;}
        if( dblu==6 ) { blu=0;}
    }
    if( - (line==3)  | - (line==7)) {
        if( dred==10 ) { red=0;}
        if( dgrn==10 ) { grn=0;}
        if( dblu==10 ) { blu=0;}
    } else {
        if( dred==9 ) { red=0;}
        if( dgrn==9 ) { grn=0;}
        if( dblu==9 ) { blu=0;}
    }
        if( dred==1 ) { red=0;}
        if( dgrn==1 ) { grn=0;}
        if( dblu==1 ) { blu=0;}
        if( dred==8 ) { red=0;}
        if( dgrn==8 ) { grn=0;}
        if( dblu==8 ) { blu=0;}

	break;
case 0x10/*00010000*/:
    if( eline) {
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
        if( dred==6 ) { red=0;}
        if( dgrn==6 ) { grn=0;}
        if( dblu==6 ) { blu=0;}
    } else {
        if( dred==3 ) { red=0;}
        if( dgrn==3 ) { grn=0;}
        if( dblu==3 ) { blu=0;}
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
    }
        if( dred==8 ) { red=0;}
        if( dgrn==8 ) { grn=0;}
        if( dblu==8 ) { blu=0;}
        if( dred==1 ) { red=0;}
        if( dgrn==1 ) { grn=0;}
        if( dblu==1 ) { blu=0;}
        if( dred==9 ) { red=0;}
        if( dgrn==9 ) { grn=0;}
        if( dblu==9 ) { blu=0;}
        if( dred==11 ) { red=0;}
        if( dgrn==11 ) { grn=0;}
        if( dblu==11 ) { blu=0;}
	break;
case 0x8/*00001000*/:
    if( eline) {
        if( dred==3 ) { red=0;}
        if( dgrn==3 ) { grn=0;}
        if( dblu==3 ) { blu=0;}
        if( dred==4 ) { red=0;}
        if( dgrn==4 ) { grn=0;}
        if( dblu==4 ) { blu=0;}
        if( dred==11 ) { red=0;}
        if( dgrn==11 ) { grn=0;}
        if( dblu==11 ) { blu=0;}
    } else {
        if( dred==12 ) { red=0;}
        if( dgrn==12 ) { grn=0;}
        if( dblu==12 ) { blu=0;}
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
        if( dred==1 ) { red=0;}
        if( dgrn==1 ) { grn=0;}
        if( dblu==1 ) { blu=0;}
        if( dred==6 ) { red=0;}
        if( dgrn==6 ) { grn=0;}
        if( dblu==6 ) { blu=0;}
    }
    if( - (line==1)  | - (line==5)) {
        if( dred==5 ) { red=0;}
        if( dgrn==5 ) { grn=0;}
        if( dblu==5 ) { blu=0;}
        if( dred==10 ) { red=0;}
        if( dgrn==10 ) { grn=0;}
        if( dblu==10 ) { blu=0;}
    } else {
        if( dred==1 ) { red=0;}
        if( dgrn==1 ) { grn=0;}
        if( dblu==1 ) { blu=0;}
        if( dred==9 ) { red=0;}
        if( dgrn==9 ) { grn=0;}
        if( dblu==9 ) { blu=0;}
    }
    if( line==5) {
        if( dred==7 ) { red=0;}
        if( dgrn==7 ) { grn=0;}
        if( dblu==7 ) { blu=0;}
    } else {
        if( dred==8 ) { red=0;}
        if( dgrn==8 ) { grn=0;}
        if( dblu==8 ) { blu=0;}
    }
	break;

case 0x4/*00000100*/:
    if( eline) {
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
        if( dred==6 ) { red=0;}
        if( dgrn==6 ) { grn=0;}
        if( dblu==6 ) { blu=0;}
    } else {
        if( dred==3 ) { red=0;}
        if( dgrn==3 ) { grn=0;}
        if( dblu==3 ) { blu=0;}
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
        if( dred==11 ) { red=0;}
        if( dgrn==11 ) { grn=0;}
        if( dblu==11 ) { blu=0;}
    }
    if( line==4) {
        if( dred==11 ) { red=0;}
        if( dgrn==11 ) { grn=0;}
        if( dblu==11 ) { blu=0;}
    }
    if( - (line==2)  | - (line==6)) {
        if( dred==12 ) { red=0;}
        if( dgrn==12 ) { grn=0;}
        if( dblu==12 ) { blu=0;}
    }

        if( dred==1 ) { red=0;}
        if( dgrn==1 ) { grn=0;}
        if( dblu==1 ) { blu=0;}
        if( dred==9 ) { red=0;}
        if( dgrn==9 ) { grn=0;}
        if( dblu==9 ) { blu=0;}
        if( dred==8 ) { red=0;}
        if( dgrn==8 ) { grn=0;}
        if( dblu==8 ) { blu=0;}

	break;

case 0x2/*00000010*/:
    if( eline) {
        if( dred==3 ) { red=0;}
        if( dgrn==3 ) { grn=0;}
        if( dblu==3 ) { blu=0;}
        if( dred==4 ) { red=0;}
        if( dgrn==4 ) { grn=0;}
        if( dblu==4 ) { blu=0;}
        if( dred==11 ) { red=0;}
        if( dgrn==11 ) { grn=0;}
        if( dblu==11 ) { blu=0;}
    } else {
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
        if( dred==6 ) { red=0;}
        if( dgrn==6 ) { grn=0;}
        if( dblu==6 ) { blu=0;}
        if( dred==12 ) { red=0;}
        if( dgrn==12 ) { grn=0;}
        if( dblu==12 ) { blu=0;}
    }
    if( - (line==3)  | - (line==7)) {
        if( dred==10 ) { red=0;}
        if( dgrn==10 ) { grn=0;}
        if( dblu==10 ) { blu=0;}
    } else {
        if( dred==9 ) { red=0;}
        if( dgrn==9 ) { grn=0;}
        if( dblu==9 ) { blu=0;}
    }
        if( dred==1 ) { red=0;}
        if( dgrn==1 ) { grn=0;}
        if( dblu==1 ) { blu=0;}
        if( dred==8 ) { red=0;}
        if( dgrn==8 ) { grn=0;}
        if( dblu==8 ) { blu=0;}

	break;
case 0x1/*00000001*/:
    if( eline) {
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
        if( dred==6 ) { red=0;}
        if( dgrn==6 ) { grn=0;}
        if( dblu==6 ) { blu=0;}
    } else {
        if( dred==3 ) { red=0;}
        if( dgrn==3 ) { grn=0;}
        if( dblu==3 ) { blu=0;}
        if( dred==2 ) { red=0;}
        if( dgrn==2 ) { grn=0;}
        if( dblu==2 ) { blu=0;}
    }
        if( dred==1 ) { red=0;}
        if( dgrn==1 ) { grn=0;}
        if( dblu==1 ) { blu=0;}
        if( dred==9 ) { red=0;}
        if( dgrn==9 ) { grn=0;}
        if( dblu==9 ) { blu=0;}
        if( dred==8 ) { red=0;}
        if( dgrn==8 ) { grn=0;}
        if( dblu==8 ) { blu=0;}
        if( dred==11 ) { red=0;}
        if( dgrn==11 ) { grn=0;}
        if( dblu==11 ) { blu=0;}
/*
    IF line=0
        IF eline=0 THEN eline:=1 ELSE eline:=0
    ENDIF
*/
	break;
}

// WriteF('\d,\d,\d = \d\n',red,grn,blu,col)

ditherremap();
} catch(...) {}
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;
}

void ditherremap() {
if( simpleremap==0) {
    choosecol();
} else {
/* simplemap *****/
if( - (red==0)  & - (grn==0)  & - (blu==0)  ) { col=0;}
if( (long) (- (red==0)  & - (grn==0)  )& blu    ) { col=1;}
if( red    & (long) - (grn==0)  & (long) - (blu==0)  ) { col=2;}
if( red    & (long) - (grn==0)  & blu    ) { col=3;}
if( (long) - (red==0)  & grn    & (long) - (blu==0)  ) { col=4;}
if( (long) - (red==0)  & grn    & blu    ) { col=5;}
if( red    & grn    & (long) - (blu==0)  ) { col=6;}
if( red    & grn    & blu    ) { col=7;}
}
	return ;

/* by this time, bright is already done...
bright:=0
IF red>128 THEN bright:=1
IF grn>128 THEN bright:=1
IF blu>128 THEN bright:=1
*/

//WriteF('\d,\d,\d = \d\n',red,grn,blu,col)
}

void stdchoosecol() /*(red,grn,blu)*/ {
long blured, blugrn, redgrn, redblu, grnred, grnblu, thr2, redgrnblu;


bright=0;

redgrnblu=red+grn+blu;

if( redgrnblu > wtthr ) { col=7 ;} else { col=0 /* black or white 384 */;}
   


  if( redgrnblu > brthr ) { bright=1 /* 573 */;}

blured=blu-red;
blugrn=blu-grn;
redgrn=red-grn;
redblu=red-blu;
grnred=grn-red;
grnblu=grn-blu;
thr2=thr;
thr3=thr;
thr4=thr;

if( ((long) - (redgrn>thr )& blugrn)>thr) {
  col=3 /* magenta */;
  thr2=thr+grnmag;
}

if( ((long) - (grnred>thr )& blured)>thr) {
  col=5 /* cyan */;
  thr3=thr+blucyn;
}

if( ((long) - (redblu>thr )& grnblu)>thr) {
  col=6 /* yellow */;
  thr4=thr+redyel  /* 40 */;
}

if( blured>thr3) {
   if( blugrn>thr3 ) { col=1 /* blue */;}
}
if( redgrn>thr4) {
   if( redblu>thr4 ) { col=2 /* red */;}
}
if( grnred>thr2) {
   if( grnblu>thr2 ) { col=4 /* green */;}
}
	return ;

} /* col,bright */

void os3setup() {
if( cm==(long) NULL ) { cm=(long) GetColorMap((ULONG) 15 );}

if( rompal==0 /* ZXDT */) {
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 15 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 0 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 1 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 ,(ULONG) (153 << 24) );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 2 ,(ULONG) (153 << 24) ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 3 ,(ULONG) (153 << 24) ,(ULONG) 0x00000000 ,(ULONG) (153 << 24) );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 4 ,(ULONG) 0x00000000 ,(ULONG) (153 << 24) ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 5 ,(ULONG) 0x00000000 ,(ULONG) (153 << 24) ,(ULONG) (153 << 24) );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 6 ,(ULONG) (153 << 24) ,(ULONG) (153 << 24) ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 7 ,(ULONG) (153 << 24) ,(ULONG) (153 << 24) ,(ULONG) (153 << 24) );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 8 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 ,(ULONG) (170 << 24) );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 9 ,(ULONG) (187 << 24) ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 10 ,(ULONG) (204 << 24) ,(ULONG) 0x00000000 ,(ULONG) (204 << 24) );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 11 ,(ULONG) 0x00000000 ,(ULONG) (204 << 24) ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 12 ,(ULONG) 0x00000000 ,(ULONG) 0xDDDDDDDD ,(ULONG) 0xDDDDDDDD );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 13 ,(ULONG) (238 << 24) ,(ULONG) (238 << 24) ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 14 ,(ULONG) 0xFFFFFFFF ,(ULONG) 0xFFFFFFFF ,(ULONG) 0xFFFFFFFF );

} else {

  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 15 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 0 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 8 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 ,(ULONG) 0xFFFFFFFF );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 9 ,(ULONG) 0xFFFFFFFF ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 10 ,(ULONG) 0xFFFFFFFF ,(ULONG) 0x00000000 ,(ULONG) 0xFFFFFFFF );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 11 ,(ULONG) 0x00000000 ,(ULONG) 0xFFFFFFFF ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 12 ,(ULONG) 0x00000000 ,(ULONG) 0xFFFFFFFF ,(ULONG) 0xFFFFFFFF );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 13 ,(ULONG) 0xFFFFFFFF ,(ULONG) 0xFFFFFFFF ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 14 ,(ULONG) 0xFFFFFFFF ,(ULONG) 0xFFFFFFFF ,(ULONG) 0xFFFFFFFF );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 1 ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 ,(ULONG) 0xDDDDDDDD );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 2 ,(ULONG) 0xDDDDDDDD ,(ULONG) 0x00000000 ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 3 ,(ULONG) 0xDDDDDDDD ,(ULONG) 0x00000000 ,(ULONG) 0xDDDDDDDD );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 4 ,(ULONG) 0x00000000 ,(ULONG) 0xDDDDDDDD ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 5 ,(ULONG) 0x00000000 ,(ULONG) 0xDDDDDDDD ,(ULONG) 0xDDDDDDDD );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 6 ,(ULONG) 0xDDDDDDDD ,(ULONG) 0xDDDDDDDD ,(ULONG) 0x00000000 );
  SetRGB32CM((struct ColorMap*) cm ,(ULONG) 7 ,(ULONG) 0xDDDDDDDD ,(ULONG) 0xDDDDDDDD ,(ULONG) 0xDDDDDDDD );
}
	return ;

}

BOOLEAN choosecol() {
long t;
BOOLEAN temp_BOOL; long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;

if( os3==FALSE) {
   stdchoosecol();
   temp_BOOL = 0;
   Raise(0);
}

bright=0;
/*
red:=!red/255
grn:=!grn/255
blu:=!blu/255

red:=red!/255.0
grn:=grn!/255.0
blu:=blu!/255.0
*/

red=red << 24;
grn=grn << 24;
blu=blu << 24;


col=(long) FindColor((struct ColorMap*) cm ,(ULONG) red ,(ULONG) grn ,(ULONG) blu ,(ULONG) 15 );


/*
t:=ObtainBestPenA(cm,red,grn,blu,[NIL])

col:=t

ReleasePen(t)
*/

if( col==15 ) { col=0;}

// IF col>7 THEN WriteF('red \h, grn \h, blu \h, col \d\n',red,grn,blu,col)

if( col>7) {
  bright=1;
  col=col-(long) 7;
}
	temp_BOOL = 0;
} catch(...) {}
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return temp_BOOL ;


} 

void joemackay() {

    init();
//    StrCopy(filename, 'pictures:kingtut.l256') -> Example - change/delete as applicable
     
     if( clipboard==0 ) { identifyfile();}
     
    dt2ppm(scale);
	return ;
    
//    WriteF('\nRaw PPM RGB data at: $\h (for a split second)\nWhich is pretty useless now, unless you incorporate it into #?toSCR...\n', ppmmem)

}

void init() {


    if( quiet==0 ) { WriteF((char*) "Datatypes code, \251 Joe Mackay 1998\n\n");}
	return ;

}

/* The actual routine...
   Usage: dt2ppm(scaleornot) - scaleornot means, erm, whether to scale
                               the image to 256x192 or not. Bigger pics
                               will be truncated.
   A pointer to memory containing raw PPM (RGB) data, which is always
   exactly 147456 bytes long, is placed in the variable ppmmem. If any
   errors occur, they are handled in main()'s exception handler above.
   Make sure you include    it and adapt it properly to suit the rest of
   the program. */

BOOLEAN identifyfile() {
    long gid; struct FileInfoBlock finfo; long fsize=NULL;
    BOOLEAN temp_BOOL; long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;

    chg=FALSE;

	if( clipboard==0) {
	    if( (lock=(long) Lock(filename ,ACCESS_READ ))==0) {
    	   StrCopy(filetypeerror,(char*) "Unable to open file!");
	      	if( (void*) wbmessage==NULL) {
    	      if( quiet==0 ) { WriteF((char*) "%s\n",(long) filetypeerror);}
      		} else {
          		tempGlobal_ARRAY_OF_easystruct4 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
          		tempGlobal_ARRAY_OF_easystruct4 [0].es_TextFormat = (CONST_STRPTR) (long) filetypeerror;
          		EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct4  ,(ULONG*) 0 ,(APTR) (long) NULL );
      		}
    	}
     
    deetee=ObtainDataTypeA((ULONG) DTST_FILE ,(APTR) lock ,(struct TagItem*) NULL );
    
    gid=(long) deetee->dtn_Header->dth_GroupID;

    StrCopy(filetype,deetee->dtn_Header->dth_Name);
     
    switch( gid) {
    case GID_SYSTEM     : StrAdd(filetype,(char*) " system file");
    	break;
    case GID_TEXT       : StrAdd(filetype,(char*) " text");
    	break;
    case GID_DOCUMENT   : StrAdd(filetype,(char*) " document");
    	break;
    case GID_SOUND      : StrAdd(filetype,(char*) " sample");
    	break;
    case GID_INSTRUMENT : StrAdd(filetype,(char*) " instrument");
    	break;
    case GID_MUSIC      : StrAdd(filetype,(char*) " music");
    	break;
    case GID_PICTURE    : StrAdd(filetype,(char*) " picture");
    	break;
    case GID_ANIMATION  : StrAdd(filetype,(char*) " animation");
    	break;
    case GID_MOVIE      : StrAdd(filetype,(char*) " movie");
    	break;
    default:              StrAdd(filetype,(char*) " - unknown file type!");
    	break;
    }
} else {
	gid=GID_PICTURE;
}

ppm=FALSE;
        // Check for PPM
            if( (okay=(BOOL)(0!=Examine((BPTR) lock ,& finfo )))) {
                if( quiet==0 ) { WriteF((char*) "Checking for PPM...\nSize: %ld\n",finfo.fib_Size);}
                if( finfo.fib_Size==147471 ) {// Correct size for 256x192
                    if( (fhid=(long) OpenFromLock((BPTR) lock ))) {
                        lock=(long) NULL;
                        okay=Read((BPTR) fhid ,(APTR) fileid ,2 );
                        (BOOL)(0!=Close((BPTR) fhid ));
                        if( StrCmp((char*) "P6",(char*) fileid,2)) {
                            ppm=TRUE;
                            gid=GID_PICTURE;
StrCopy(filetype,(char*) "PPM RawBits (P6)\nWidth: 256\nHeight: 192\nDepth: 24 bit (16777216 colours)");
StrCopy(objname,FilePart(filename ));
StrCopy(objauth,(char*) "");
StrCopy(objanno,(char*) "");
StrCopy(objcopy,(char*) "");
StrCopy(objver,(char*) "");

                        }
                    }
                }
            }
            
     StrCopy(filetypeerror,(char*) "File: ");
     StrAdd(filetypeerror,filename);
     StrAdd(filetypeerror,(char*) "\nType: ");
     StrAdd(filetypeerror,filetype);
     StrAdd(filetypeerror,(char*) "\n\nThis cannot be converted!");

    // Not DT or (valid) PPM

    if( quiet==0 ) { WriteF((char*) "File identified as %s\n",(long) filetype);}

    if( gid!=GID_PICTURE) {
          if( (void*) wbmessage==NULL) {
            if( quiet==0 ) { WriteF((char*) "%s\n",(long) filetypeerror);}
        } else {
            tempGlobal_ARRAY_OF_easystruct5 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
            tempGlobal_ARRAY_OF_easystruct5 [0].es_TextFormat = (CONST_STRPTR) (long) filetypeerror;
            EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct5  ,(ULONG*) 0 ,(APTR) (long) NULL );
        }
      StrCopy(filename,(char*) "");
     }
    // Raise(ERR_NPIC)  -> Quit if not a picture

     if( deetee) {
         ReleaseDataType(deetee );
         deetee=(struct DataType*) NULL;
     }

     if( lock) {
         (BOOL)(0!=Close((BPTR) lock ));
         lock=(long) NULL;
     }

if( gid==GID_PICTURE ) {

	temp_BOOL = 1;

	Raise(0);

}
	temp_BOOL = 0;
} catch(...) {}
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return temp_BOOL ;

} 

long dt2ppm(long scaleornot) {
long allok=0, p=0, q=0, olddir=-1; char* tmpstr2=NULL; struct Screen* specscreen=NULL;

//    DEF gid
     long cols[1]; long creg[1];
    struct gpLayout layout;
    long n, bitwork; char* rgbs=NULL; long i2, j2, k2, dbmtype=0;
char* temp_ARRAY_OF_CHAR=NULL; long temp_QUAD;
try {
	tmpstr2 = NewString(30);
	temp_QUAD = exception ;
	exception = 0 ;
//    DEF ppmfh


flashpix=0;

if( ppm) {
    if( (fhid=(long) Open(filename ,OLDFILE ))) {

    if( quiet==0 ) { WriteF((char*) "Reading PPM file...\n");}

    okay=Read((BPTR) fhid ,(APTR) (long) ppmmem ,15 );
    okay=Read((BPTR) fhid ,(APTR) (long) ppmmem ,147456 );

    if( okay) {
      if( quiet==0 ) { WriteF((char*) "Read data OK\n");}
    }

    (BOOL)(0!=Close((BPTR) fhid ));
    }

    allok= 1;

    Raise(0);
}

        StrCopy(wintit,(char*) "Loading datatypes image...") ;// 256x192
          if( (void*) wbmessage!=NULL ) { SetWindowTitles(curwin ,wintit ,(char*) -1 );}
        if( quiet==0 ) { WriteF((char*) "%s\n",(long) wintit);}

    /* Create DataTypes object */
       StrCopy(filetypeerror,(char*) "An unexplainable DataTypes error occured!\n\n(possibly the file wasn\'t actually a picture at all, or something)");

/*
    IF (rbm:=AllocBitMap(256, 192, 24, BMF_CLEAR, bm))=0
        StrCopy(filetypeerror,'Could not allocate Bitmap!')
         EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC',filetypeerror,'OK']:easystruct,0,NIL)
    	freeup()
		RETURN allok
    ENDIF
*/

//    IF (rbm:=AllocBitMap(bmh.width, bmh.height, bmh.depth, BMF_CLEAR, bm))=0
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
if( clipboard==0) {
    tempGlobal_ILIST3 [9]= (long) specscreen;
    if( (o=(long) NewDTObjectA((APTR) (long) filename ,(struct TagItem*) tempGlobal_ILIST3  ))==0) {
/*
    IF (o:=NewDTObjectA(filename,
                       [DTA_SOURCETYPE, DTST_FILE,
                        PDTA_REMAP,     TRUE,
                        PDTA_NUMSPARSE, 15,
                        PDTA_SPARSETABLE, cm,
                        PDTA_DESTBITMAP, {rbm},
                        TAG_DONE]))=0
*/                      
         if( (void*) wbmessage==NULL) {
              if( quiet==0 ) { WriteF((char*) "%s\n",(long) filetypeerror);}
        } else {
            tempGlobal_ARRAY_OF_easystruct6 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
            tempGlobal_ARRAY_OF_easystruct6 [0].es_TextFormat = (CONST_STRPTR) (long) filetypeerror;
            EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct6  ,(ULONG*) 0 ,(APTR) (long) NULL );
        }
    	freeup();
		Raise(0);
    }
} else {
    if( (o=(long) NewDTObjectA((APTR) clipunit ,(struct TagItem*) tempGlobal_ILIST4  ))==0) {
                        
               if( (void*) wbmessage==NULL) {
          if( quiet==0 ) { WriteF((char*) "%s\n",(long) filetypeerror);}
      } else {
        tempGlobal_ARRAY_OF_easystruct7 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
        EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct7  ,(ULONG*) 0 ,(APTR) (long) NULL );
    }
    	freeup();
		Raise(0);
    }
}

// TEMP***** CloseScreen(specscreen)
// CloseS(specscreen)

setprogresswin(5);

    /* Fill a BitMapHeader with information about the picture */

	/* Render the picture to a BitMap
	layout.methodid:=DTM_PROCLAYOUT
	layout.ginfo   :=NIL
	layout.initial :=TRUE
	IF (DoDTMethodA(o, NIL, NIL, layout))=0 THEN Raise(ERR_DTYP)
 */

//	IF (DoDTMethodA(o, NIL, NIL, [GM_RENDER,NIL,specscreen.rastport,GREDRAW_REDRAW]:gprender))=0 THEN Raise(ERR_DTYP)


    tempGlobal_ILIST5 [3]= (long) cols;


    if( (long) GetDTAttrsA((Object*) o ,(struct TagItem*) tempGlobal_ILIST5  )!=(long) 2 ) { Raise(ERR_DTYP);}

     StrAdd(filetype,(char*) "\nWidth: ");
     StrAdd(filetype,RealF(tmpstr,(float) (long) bmh[0].bmh_Width,0));
     StrAdd(filetype,(char*) "\nHeight: ");
     StrAdd(filetype,RealF(tmpstr,(float) (long) bmh[0].bmh_Height,0));
     StrAdd(filetype,(char*) "\nDepth: ");
     StrAdd(filetype,RealF(tmpstr,(float) (long) bmh[0].bmh_Depth,0));

    if( quiet==0 ) {

    	if( (char) (long) bmh[0].bmh_Depth==(char) 1 ) {

    	temp_ARRAY_OF_CHAR = (char*) " ";

    	} else { 

    	temp_ARRAY_OF_CHAR = (char*) "s";

    	}

    	WriteF((char*) "Width:  %ld\nHeight: %ld\nDepth:  %ld bit%s ", (long) bmh[0].bmh_Width, (long) bmh[0].bmh_Height, (long) bmh[0].bmh_Depth, (long) temp_ARRAY_OF_CHAR );

    }

     StrAdd(filetype,(char*) " bit (");
     StrAdd(filetype,RealF(tmpstr,(float) cols[0],0));
     StrAdd(filetype,(char*) " colours)");

// extract info

StrCopy(objauth,(char*) "");
StrCopy(objcopy,(char*) "");
StrCopy(objanno,(char*) "");



GetDTAttrsA((Object*) o ,(struct TagItem*) tempGlobal_ILIST6  );



//    StrAdd(filetype,'\nName: ')
//    StrAdd(filetype,objname)

if( GetDTAttrsA((Object*) o ,(struct TagItem*) tempGlobal_ILIST7  )) {
if( (long) objauth2!=(long) NULL) {
    StrAdd(filetype,(char*) "\nAuthor: ");
    StrAdd(filetype,objauth2);
    StrCopy(objauth,objauth2);
}
}

if( GetDTAttrsA((Object*) o ,(struct TagItem*) tempGlobal_ILIST8  )) {
if( (long) objcopy2!=(long) NULL) {
    StrAdd(filetype,(char*) "\nCopyright: ");
    StrAdd(filetype,objcopy2);
StrCopy(objcopy,objcopy2);
}
}


if( GetDTAttrsA((Object*) o ,(struct TagItem*) tempGlobal_ILIST9  )) {
if( (long) objanno2!=(long) NULL) {
    StrAdd(filetype,(char*) "\nAnnotation: ");
    StrAdd(filetype,objanno2);
StrCopy(objanno,objanno2);
}
}

/*
IF GetDTAttrsA(o,[DTA_OBJVERSION,objver2,NIL])
    StrAdd(filetype,'\nVersion: ')
    StrAdd(filetype,objver)
ENDIF
*/


    // *** end

    if( quiet==0 ) { WriteF((char*) "(%ld colours)\n", cols[0]);}

    if( scaleornot) {
        StrCopy(wintit,(char*) "Scaling to Spectrum screen size...") ;// 256x192
          if( (void*) wbmessage!=NULL ) { SetWindowTitles(curwin ,wintit ,(char*) -1 );}
        if( quiet==0 ) { WriteF((char*) "%s\n",(long) wintit);}

		DoMethodA((Object*) o , (Msg)(long) tempGlobal_ARRAY_OF_pdtscale  );

    } else {
        if( (short) (long) bmh[0].bmh_Width>256) {
            i2=256 ; if( quiet==0 ) { WriteF((char*) "Picture has been truncated horizontally\n");}
        } else {
            i2=(long) bmh[0].bmh_Width;
        }
        if( (short) (long) bmh[0].bmh_Height>192) {
            j2=192 ; if( quiet==0 ) { WriteF((char*) "Picture has been truncated vertically\n");}
        } else {
            j2=(long) bmh[0].bmh_Height;
        }

    }
    
    setprogresswin(20);

    /* Finally, convert the BitMap to chunky data */

//    rgbs:=ppmmem

	DoMethodA((Object*) o , (Msg)(long) tempGlobal_ARRAY_OF_pdtblitpixelarray  );

    setprogresswin(40);

allok=1;
} catch(...) {}
	DisposeString(tmpstr2 );
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return allok;

} 

void freeup() {
    
    if( o) {
      DisposeDTObject((Object*) o );
      o=(long) NULL;
    }

    if( dbm) {
      FreeBitMap(dbm );
      dbm=(struct BitMap*) NULL;
//      bm:=NIL -> *** temp ***
    }

    if( rbm) {
      FreeBitMap(rbm );
      rbm=(struct BitMap*) NULL;
    }
    
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

//    Flush(ppmfh)
//    Close(ppmfh)

if( acontrast ) { autocontrast();}
	return ;

}

void autocontrast() {
long n, highval=0;

lowval=255;

/* auto-contrast detection stuff */
for( n=0 ; n<=(long) 49151               ; n++) {// No. of pixels in a 256x192 piccie
if( ppmmem[n]>highval ) { highval=ppmmem[n];}
if( ppmmem[n]<lowval ) { lowval=ppmmem[n];}
}
range=highval-lowval;
	return ;

}

// ppmmem

void readtooltypes(long fromcli) {
long olddir=-1, tmplock;

if( fromcli) {
    if( (long) (IconBase=OpenLibrary("icon.library" ,(ULONG) 36 ))==(long) 0 ) { Raise(ERR_ICNN);}
    dobj=GetDiskObjectNew("PROGDIR:PDHFIC" );
} else {
    wbenchMsg=wbmessage;
    wbarg=wbenchMsg->sm_ArgList;
    if( (long) wbarg->wa_Lock  & (long) - ((long) wbarg->wa_Name!=(long) 0)  ) { olddir=(long) CurrentDir(wbarg->wa_Lock );}
    dobj=GetDiskObject((char*) wbarg->wa_Name );
    quiet=1;
}

if( dobj) {

toolarray=(long) dobj->do_ToolTypes;

if( (tt=(long) FindToolType((char**) toolarray ,"COLOURSENSE" ))) { thr=Val((char*) tt);}

if( (tt=(long) FindToolType((char**) toolarray ,"BRIGHTSENSE" ))) { brthr=Val((char*) tt);}

if( (tt=(long) FindToolType((char**) toolarray ,"WHITESENSE" ))) { wtthr=Val((char*) tt);}

if( (tt=(long) FindToolType((char**) toolarray ,"GREENMAGENTA" ))) { grnmag=Val((char*) tt);}

if( (tt=(long) FindToolType((char**) toolarray ,"BLUECYAN" ))) { blucyn=Val((char*) tt);}

if( (tt=(long) FindToolType((char**) toolarray ,"REDYELLOW" ))) { redyel=Val((char*) tt);}

if( (tt=(long) FindToolType((char**) toolarray ,"FLASHRED" ))) { flashred=Val((char*) tt);}

if( (tt=(long) FindToolType((char**) toolarray ,"FLASHGRN" ))) { flashgrn=Val((char*) tt);}

if( (tt=(long) FindToolType((char**) toolarray ,"FLASHBLU" ))) { flashblu=Val((char*) tt);}

if( (tt=(long) FindToolType((char**) toolarray ,"CLIPUNIT" ))) { clipunit=Val((char*) tt);}

if( FindToolType((char**) toolarray ,"GREYSCALE" )) { thr=5000;}

if( FindToolType((char**) toolarray ,"NOBRIGHT" )) { brthr=5000;}

if( FindToolType((char**) toolarray ,"NOSMOOTH" )) { smooth=1;}

if( FindToolType((char**) toolarray ,"NOSCALE" )) { scale=0;}

if( FindToolType((char**) toolarray ,"ROMREMAP" )) { os3=1;}

if( FindToolType((char**) toolarray ,"SIMPLEMAP" )) { simpleremap=1;}

if( FindToolType((char**) toolarray ,"ALTPALETTE" )) { rompal=1;}

if( FindToolType((char**) toolarray ,"QUIET" )) { quiet=1;}

if( (tt=(long) FindToolType((char**) toolarray ,"SAVEFORMAT" ))) {
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "SCR" )) { saveformat=0;}
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "ZX82" )) { saveformat=1;}
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "BYTES" )) { saveformat=2;}
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "TAP" )) { saveformat=3;}
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "TZX" )) { saveformat=4;}
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "PLUS3" )) { saveformat=5;}
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "GIF" )) {
        saveformat=0;
        gifsave=1;
    }
}

if( FindToolType((char**) toolarray ,"ADVANCED" )) { advopts=1;}

if( FindToolType((char**) toolarray ,"SCREEN" )) { mvscreen=1;}

if( FindToolType((char**) toolarray ,"WARNOVERWRITE" )) { askover=1;}

if( (tt=(long) FindToolType((char**) toolarray ,"TZXBLOCKS" ))) {
    tzxarray[0]=0;
    tzxarray[1]=0;
    tzxarray[2]=0;
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "INFO" )) { tzxarray[0]=1;}
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "CUSTOM" )) { tzxarray[2]=1 ;} else { tzxarray[1]=1;}

}

if( (tt=(long) FindToolType((char**) toolarray ,"ODDCOLOURS" ))) {
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "INK" )) { allelseink=1;}
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "PAPER" )) { allelseink=0;}
}

if( FindToolType((char**) toolarray ,"NOHEADER" )) { writeheader=0;}
if( FindToolType((char**) toolarray ,"NOFILTER" )) { nofilter=1;}
if( forcenofilter ) { nofilter=1;}
if( FindToolType((char**) toolarray ,"NOPROGRESSBAR" )) { noprogress=1;}

autonaming=1;
if( FindToolType((char**) toolarray ,"NOAUTONAMING" )) { autonaming=0;}

if( FindToolType((char**) toolarray ,"NOATTRIBUTES" )) { saveattr=0 ;} else { saveattr=1;}
if( FindToolType((char**) toolarray ,"AUTOCONTRAST" )) { acontrast=1 ;} else { acontrast=0;}
if( FindToolType((char**) toolarray ,"REMAPAFTERDITHER" )) { remapafterdither=1 ;} else { remapafterdither=0;}

//IF FindToolType(toolarray,'TESTMODE') THEN testmode:=1

if( FindToolType((char**) toolarray ,"DITHER" )) { dither=1;}

dtype=0;
if( (tt=(long) FindToolType((char**) toolarray ,"DITHERTYPE" ))) {
    if( -MatchToolValue((UBYTE*) tt ,(UBYTE*) "RANDOM" )) { dtype=1;}
}

if( (tt=(long) FindToolType((char**) toolarray ,"BORDER" ))) { tzxborder=Val((char*) tt);}

if( FindToolType((char**) toolarray ,"STRANGEMODE" )) {

	tempGlobal_ARRAY_OF_easystruct8 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);

	EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct8  ,(ULONG*) 0 ,(APTR) (long) NULL );

}

if( (tt=(long) FindToolType((char**) toolarray ,"WINX" ))) { winx=Val((char*) tt);}
if( (tt=(long) FindToolType((char**) toolarray ,"WINY" ))) { winy=Val((char*) tt);}
if( (tt=(long) FindToolType((char**) toolarray ,"ASLX" ))) { aslx=Val((char*) tt);}
if( (tt=(long) FindToolType((char**) toolarray ,"ASLY" ))) { asly=Val((char*) tt);}

FreeDiskObject(dobj );
dobj=(struct DiskObject*) NULL;
}

if( fromcli==0 ) { CurrentDir((BPTR) olddir );}
	return ;

}

void gadgets() {
long gad, type;

  mysc=LockPubScreen((char*) NULL );
//  tbar:=mysc.font.ysize+mysc.wbortop+1 /* "real" tbar */

tbar=(long) mysc->Font->ta_YSize+(long) mysc->WBorTop-(long) 1;

//  winheight:=tbar+10 /* + progress bar height */
//wptr=nil

/*
type[0]:='SCR'
type[1]:='ZX82'
type[2]:='.bytes'
type[3]:='TAP'
*/

  vi=(long) GetVisualInfoA(mysc ,(struct TagItem*) tempGlobal_ILIST10  );
  // GadTools gadgets require this step to be taken
  gad=(long) CreateContext(glist );

  // Create a button gadget centered below the window title
// gadg 1
  tempGlobal_ARRAY_OF_newgadget [0].ng_TopEdge = pixsep+tbar;
  tempGlobal_ARRAY_OF_newgadget [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);
  tempGlobal_ARRAY_OF_newgadget [0].ng_TextAttr = mysc->Font;
  tempGlobal_ARRAY_OF_newgadget [0].ng_VisualInfo = (APTR) vi;
  gads[GADG_LOADSTR]=(long) CreateGadgetA((ULONG) TEXT_KIND ,(struct Gadget*) gad ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget  ,tempGlobal_ARRAY_OF_tagitem  );
                    // GTTX_JUSTIFICATION,GTJ_RIGHT,NIL])

// ['topaz.font', 8, 0, 0]:textattr

  tempGlobal_ARRAY_OF_newgadget2 [0].ng_TopEdge = pixsep+tbar;

  tempGlobal_ARRAY_OF_newgadget2 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget2 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget2 [0].ng_VisualInfo = (APTR) vi;

  gads[GADG_LOAD]=(long) CreateGadgetA((ULONG) BUTTON_KIND ,(struct Gadget*) gads[GADG_LOADSTR] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget2  ,(struct TagItem*) tempGlobal_ILIST11  );

// STRING_
  tempGlobal_ARRAY_OF_newgadget3 [0].ng_TopEdge = pixsep*(long) 2+(long) mysc->Font->ta_YSize+tbar;
  tempGlobal_ARRAY_OF_newgadget3 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);
  tempGlobal_ARRAY_OF_newgadget3 [0].ng_TextAttr = mysc->Font;
  tempGlobal_ARRAY_OF_newgadget3 [0].ng_VisualInfo = (APTR) vi;
  gads[GADG_SAVESTR]=(long) CreateGadgetA((ULONG) TEXT_KIND ,(struct Gadget*) gads[GADG_LOAD] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget3  ,(struct TagItem*) tempGlobal_ILIST12  );
                    // GTTX_JUSTIFICATION,GTJ_RIGHT,NIL])

  tempGlobal_ARRAY_OF_newgadget4 [0].ng_TopEdge = pixsep*(long) 2+(long) mysc->Font->ta_YSize+tbar;

  tempGlobal_ARRAY_OF_newgadget4 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget4 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget4 [0].ng_VisualInfo = (APTR) vi;

  gads[GADG_SAVE]=(long) CreateGadgetA((ULONG) BUTTON_KIND ,(struct Gadget*) gads[GADG_SAVESTR] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget4  ,(struct TagItem*) tempGlobal_ILIST13  );

  tempGlobal_ARRAY_OF_newgadget5 [0].ng_TopEdge = pixsep*(long) 5+(long) ((long) mysc->Font->ta_YSize*4)+tbar;

  tempGlobal_ARRAY_OF_newgadget5 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget5 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget5 [0].ng_VisualInfo = (APTR) vi;

  gads[GADG_START]=(long) CreateGadgetA((ULONG) BUTTON_KIND ,(struct Gadget*) gads[GADG_SAVE] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget5  ,(struct TagItem*) tempGlobal_ILIST14  );

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

if( gifavailable) {
    type=(long) tempGlobal_ILIST15 ;
    if( gifsave ) { saveformat=5;}
} else {
    type=(long) tempGlobal_ILIST16 ;
}

// MX_


  tempGlobal_ARRAY_OF_newgadget6 [0].ng_TopEdge = pixsep*(long) 5+(long) ((long) mysc->Font->ta_YSize*4)+tbar;


  tempGlobal_ARRAY_OF_newgadget6 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);


  tempGlobal_ARRAY_OF_newgadget6 [0].ng_TextAttr = mysc->Font;


  tempGlobal_ARRAY_OF_newgadget6 [0].ng_VisualInfo = (APTR) vi;


  tempGlobal_ILIST17 [1]= type;


  tempGlobal_ILIST17 [3]= saveformat;


  gads[GADG_TYPE]=(long) CreateGadgetA((ULONG) CYCLE_KIND ,(struct Gadget*) gads[GADG_START] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget6  ,(struct TagItem*) tempGlobal_ILIST17  );

/*
                    GTMX_TITLEPLACE,PLACETEXT_ABOVE,
                    NIL])
*/

if( saveformat==5 ) { saveformat=0;}

  tempGlobal_ARRAY_OF_newgadget7 [0].ng_TopEdge = pixsep*(long) 4+(long) ((long) mysc->Font->ta_YSize*3)+tbar;

  tempGlobal_ARRAY_OF_newgadget7 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget7 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget7 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST18 [1]= os3;

  gads[GADG_ROMREMAP]=(long) CreateGadgetA((ULONG) CHECKBOX_KIND ,(struct Gadget*) gads[GADG_TYPE] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget7  ,(struct TagItem*) tempGlobal_ILIST18  );

  tempGlobal_ARRAY_OF_newgadget8 [0].ng_TopEdge = pixsep*(long) 3+(long) ((long) mysc->Font->ta_YSize*2)+tbar;

  tempGlobal_ARRAY_OF_newgadget8 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget8 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget8 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST19 [1]= scale;

  gads[GADG_SCALE]=(long) CreateGadgetA((ULONG) CHECKBOX_KIND ,(struct Gadget*) gads[GADG_ROMREMAP] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget8  ,(struct TagItem*) tempGlobal_ILIST19  );

  tempGlobal_ARRAY_OF_newgadget9 [0].ng_TopEdge = pixsep*(long) 3+(long) ((long) mysc->Font->ta_YSize*2)+tbar;

  tempGlobal_ARRAY_OF_newgadget9 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget9 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget9 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST20 [1]= smooth;

  gads[ADV_SMOOTH]=(long) CreateGadgetA((ULONG) CHECKBOX_KIND ,(struct Gadget*) gads[GADG_SCALE] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget9  ,(struct TagItem*) tempGlobal_ILIST20  );

  tempGlobal_ARRAY_OF_newgadget10 [0].ng_TopEdge = pixsep*(long) 4+(long) ((long) mysc->Font->ta_YSize*3)+tbar;

  tempGlobal_ARRAY_OF_newgadget10 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget10 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget10 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST21 [1]= dither;

  gads[ADV_ZXDTPAL]=(long) CreateGadgetA((ULONG) CHECKBOX_KIND ,(struct Gadget*) gads[ADV_SMOOTH] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget10  ,(struct TagItem*) tempGlobal_ILIST21  );
	return ;

}

void menus() {
long bored=0, advmen, scrmen, askovermen, tzx0men, tzx1men, tzx2men, headermen;
long bordermen[8]; long z2, filtermen, progressmen;
long oddcolsmen0, oddcolsmen1, oddcolsmen2, extractmen, snapwinmen, saveattrmen;
long simplemapmen, zxdtmen, dtypemen0, dtypemen1, radmen, alsbase=NULL;

if( alsbase==(long) NULL ) { bored=NM_ITEMDISABLED;}
if( advopts ) { advmen=(short) (CHECKIT | MENUTOGGLE )| CHECKED  ;} else { advmen=CHECKIT | MENUTOGGLE;}
if( winy!=-1) {
    snapwinmen=(short) (CHECKIT | MENUTOGGLE )| CHECKED;
    snapshotwindows=1;
} else {
    snapwinmen=CHECKIT | MENUTOGGLE;
    snapshotwindows=0;
}
if( mvscreen ) { scrmen=(short) (CHECKIT | MENUTOGGLE )| CHECKED  ;} else { scrmen=CHECKIT | MENUTOGGLE;}
if( askover ) { askovermen=(short) (CHECKIT | MENUTOGGLE )| CHECKED  ;} else { askovermen=CHECKIT | MENUTOGGLE;}
if( writeheader ) { headermen=(short) (CHECKIT | MENUTOGGLE )| CHECKED  ;} else { headermen=CHECKIT | MENUTOGGLE;}
if( tzxarray[0] ) { tzx0men=(short) (CHECKIT | MENUTOGGLE )| CHECKED  ;} else { tzx0men=CHECKIT | MENUTOGGLE;}
if( tzxarray[1] ) { tzx1men=(short) CHECKIT | CHECKED  ;} else { tzx1men=CHECKIT;}
if( tzxarray[2] ) { tzx2men=(short) CHECKIT | CHECKED  ;} else { tzx2men=CHECKIT;}
if( nofilter ) { filtermen=CHECKIT | MENUTOGGLE  ;} else { filtermen=(short) (CHECKIT | MENUTOGGLE )| CHECKED;}
if( forcenofilter ) { filtermen=CHECKIT | MENUTOGGLE | NM_ITEMDISABLED;}

if( noprogress ) { progressmen=CHECKIT | MENUTOGGLE  ;} else { progressmen=(short) (CHECKIT | MENUTOGGLE )| CHECKED;}
if( autonaming ) { extractmen=(short) (CHECKIT | MENUTOGGLE )| CHECKED  ;} else { extractmen=CHECKIT | MENUTOGGLE;}
if( saveattr ) { saveattrmen=(short) (CHECKIT | MENUTOGGLE )| CHECKED  ;} else { saveattrmen=CHECKIT | MENUTOGGLE;}
if( simpleremap ) { simplemapmen=(short) (CHECKIT | MENUTOGGLE )| CHECKED  ;} else { simplemapmen=CHECKIT | MENUTOGGLE;}
if( rompal ) { zxdtmen=CHECKIT | MENUTOGGLE  ;} else { zxdtmen=(short) (CHECKIT | MENUTOGGLE )| CHECKED;}
if( remapafterdither ) { radmen=(short) (CHECKIT | MENUTOGGLE )| CHECKED  ;} else { radmen=CHECKIT | MENUTOGGLE;}


oddcolsmen0=CHECKIT | NM_ITEMDISABLED;
if( allelseink) {
    oddcolsmen1=(short) CHECKIT | CHECKED;
    oddcolsmen2=CHECKIT;
} else {
    oddcolsmen1=CHECKIT;
    oddcolsmen2=(short) CHECKIT | CHECKED;
}

if( dtype==0) {
    dtypemen0=(short) CHECKIT | CHECKED;
    dtypemen1=CHECKIT;
}

if( dtype==1) {
    dtypemen1=(short) CHECKIT | CHECKED;
    dtypemen0=CHECKIT;
}

for( z2=0 ; z2<=(long) 8; z2++) {
    bordermen[z2]=CHECKIT;
}

bordermen[tzxborder]=(short) CHECKIT | CHECKED;

tempGlobal_ARRAY_OF_newmenu [14].nm_Flags = (UWORD) advmen;

tempGlobal_ARRAY_OF_newmenu [15].nm_Flags = (UWORD) scrmen;

tempGlobal_ARRAY_OF_newmenu [16].nm_Flags = (UWORD) askovermen;

tempGlobal_ARRAY_OF_newmenu [17].nm_Flags = (UWORD) filtermen;

tempGlobal_ARRAY_OF_newmenu [18].nm_Flags = (UWORD) extractmen;

tempGlobal_ARRAY_OF_newmenu [19].nm_Flags = (UWORD) progressmen;

tempGlobal_ARRAY_OF_newmenu [22].nm_Flags = (UWORD) oddcolsmen0;

tempGlobal_ARRAY_OF_newmenu [23].nm_Flags = (UWORD) oddcolsmen1;

tempGlobal_ARRAY_OF_newmenu [24].nm_Flags = (UWORD) oddcolsmen2;

tempGlobal_ARRAY_OF_newmenu [25].nm_Flags = (UWORD) saveattrmen;

tempGlobal_ARRAY_OF_newmenu [26].nm_Flags = (UWORD) zxdtmen;

tempGlobal_ARRAY_OF_newmenu [28].nm_Flags = (UWORD) dtypemen0;

tempGlobal_ARRAY_OF_newmenu [29].nm_Flags = (UWORD) dtypemen1;

tempGlobal_ARRAY_OF_newmenu [30].nm_Flags = (UWORD) simplemapmen;

tempGlobal_ARRAY_OF_newmenu [31].nm_Flags = (UWORD) radmen;

tempGlobal_ARRAY_OF_newmenu [34].nm_Flags = (UWORD) tzx0men;

tempGlobal_ARRAY_OF_newmenu [36].nm_Flags = (UWORD) tzx1men;

tempGlobal_ARRAY_OF_newmenu [37].nm_Flags = (UWORD) tzx2men;

tempGlobal_ARRAY_OF_newmenu [39].nm_Flags = (UWORD) bordermen[0];

tempGlobal_ARRAY_OF_newmenu [40].nm_Flags = (UWORD) bordermen[1];

tempGlobal_ARRAY_OF_newmenu [41].nm_Flags = (UWORD) bordermen[2];

tempGlobal_ARRAY_OF_newmenu [42].nm_Flags = (UWORD) bordermen[3];

tempGlobal_ARRAY_OF_newmenu [43].nm_Flags = (UWORD) bordermen[4];

tempGlobal_ARRAY_OF_newmenu [44].nm_Flags = (UWORD) bordermen[5];

tempGlobal_ARRAY_OF_newmenu [45].nm_Flags = (UWORD) bordermen[6];

tempGlobal_ARRAY_OF_newmenu [46].nm_Flags = (UWORD) bordermen[7];

tempGlobal_ARRAY_OF_newmenu [47].nm_Flags = (UWORD) headermen;

tempGlobal_ARRAY_OF_newmenu [49].nm_Flags = (UWORD) snapwinmen;

tempGlobal_ARRAY_OF_newmenu [53].nm_Flags = (UWORD) bored;

menustrip=(long) CreateMenusA((struct NewMenu*) tempGlobal_ARRAY_OF_newmenu  ,(struct TagItem*) tempGlobal_ILIST22  );
	return ;

}

void openwin() {
short zoompos[5]; long tmplock;

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

if( (tmplock=(long) Lock("SCR2GIF" ,ACCESS_READ ))) {
    gifavailable=1;
    (BOOL)(0!=Close((BPTR) tmplock ));
}

gadgets();
advgadgets();

zoompos[0]=-1;
zoompos[1]=-1;
zoompos[2]=270;
zoompos[3]=tbar+(long) 2;
zoompos[4]=(short) (long) NULL;

if( winy!=-1) {

tempGlobal_ARRAY_OF_tagitem2 [1].ti_Data = (STACKIPTR) winy;

tempGlobal_ARRAY_OF_tagitem2 [2].ti_Data = (STACKIPTR) winx;

tempGlobal_ARRAY_OF_tagitem2 [3].ti_Data = (STACKIPTR) (long) glist[0];

tempGlobal_ARRAY_OF_tagitem2 [6].ti_Data = (STACKIPTR) (pixsep*(long) 6+(long) ((long) mysc->Font->ta_YSize*5)+tbar+(long) 10);

tempGlobal_ARRAY_OF_tagitem2 [14].ti_Data = (STACKIPTR) (long) zoompos;

wptr=OpenWindowTagList((struct NewWindow*) NULL ,tempGlobal_ARRAY_OF_tagitem2  );
} else {
tempGlobal_ARRAY_OF_tagitem3 [1].ti_Data = (STACKIPTR) (long) glist[0];
tempGlobal_ARRAY_OF_tagitem3 [4].ti_Data = (STACKIPTR) (pixsep*(long) 6+(long) ((long) mysc->Font->ta_YSize*5)+tbar+(long) 10);
tempGlobal_ARRAY_OF_tagitem3 [12].ti_Data = (STACKIPTR) (long) zoompos;
wptr=OpenWindowTagList((struct NewWindow*) NULL ,tempGlobal_ARRAY_OF_tagitem3  );
}

  GT_RefreshWindow(wptr ,(struct Requester*) NULL );

menus();

if( -LayoutMenusA((struct Menu*) menustrip ,(APTR) vi ,(struct TagItem*) tempGlobal_ILIST23  )) {
  -SetMenuStrip(wptr ,(struct Menu*) menustrip );
}

rp=wptr->RPort;

if( advopts) {
  advopts=0;
  advancedoptions();
}

handle_gadgets();
	return ;

}

void askload() {
char* tmp2=NULL; char* tmp=NULL; struct Hook myhookfunc[1];
long temp_QUAD;
try {
	tmp2 = NewString(1024);
	temp_QUAD = exception ;
	exception = 0 ;

clipboard=0;
tmp=(char*) NULL;

myhookfunc[0].h_Entry = (IPTR	  (*)()) (long) (void*) &aslhookfunc ;
myhookfunc[0].h_SubEntry=(IPTR	  (*)()) (long) NULL;
myhookfunc[0].h_Data=(APTR) (long) NULL;

if( filename) {
	StrCopy(tmp2,filename);
	tmp=FilePart(tmp2 );
}

if( nofilter) {

    if( aslx  & asly) {

tempGlobal_ILIST24 [5]= (long) loaddrw;

tempGlobal_ILIST24 [7]= (long) tmp;

tempGlobal_ILIST24 [9]= aslx;

tempGlobal_ILIST24 [11]= asly;

filereq=(struct FileRequester*) AllocAslRequest((ULONG) ASL_FileRequest ,(struct TagItem*) tempGlobal_ILIST24  );

    } else {

tempGlobal_ILIST25 [5]= (long) loaddrw;

tempGlobal_ILIST25 [7]= (long) tmp;

filereq=(struct FileRequester*) AllocAslRequest((ULONG) ASL_FileRequest ,(struct TagItem*) tempGlobal_ILIST25  );
    }

} else {

//myhookfunc:=eCodeASLHook({aslhookfunc})

    if( aslx  & asly) {

tempGlobal_ILIST26 [5]= (long) loaddrw;

tempGlobal_ILIST26 [7]= (long) tmp;

tempGlobal_ILIST26 [9]= aslx;

tempGlobal_ILIST26 [11]= asly;

tempGlobal_ILIST26 [15]= (long) myhookfunc;

filereq=(struct FileRequester*) AllocAslRequest((ULONG) ASL_FileRequest ,(struct TagItem*) tempGlobal_ILIST26  );

    } else {
tempGlobal_ILIST27 [5]= (long) loaddrw;
tempGlobal_ILIST27 [7]= (long) tmp;
tempGlobal_ILIST27 [11]= (long) myhookfunc;
filereq=(struct FileRequester*) AllocAslRequest((ULONG) ASL_FileRequest ,(struct TagItem*) tempGlobal_ILIST27  );

    }

}

if( filereq) {

okay=-AslRequest((APTR) (long) filereq ,(struct TagItem*) NULL );

if( okay==FALSE ) { Raise(0);}

StrCopy(filename,filereq->fr_Drawer);

StrCopy(loaddrw,filereq->fr_Drawer);
(BOOL)(0!=AddPart(filename ,filereq->fr_File ,(ULONG) (long) 1024 ));

StrCopy(fname,filename);

/*
StrCopy(drw,filename)
fname:=FilePart(fname)
drw:=PathPart(fname)
*/

FreeAslRequest((APTR) (long) filereq );
filereq=(struct FileRequester*) NULL;
}

if( quiet==0 ) { WriteF((char*) "Input file: %s\n",(long) filename);}
} catch(...) {}
	DisposeString(tmp2 );
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;

}

void askoutput() {
if( EstrLen(filename)==(long) NULL) {
StrCopy(msg,(char*) "No file selected");
} else {

if( clipboard==0) {
    StrCopy(msg,(char*) "File: ");
    StrAdd(msg,filename);
    StrAdd(msg,(char*) "\nType: ");
} else {
    StrCopy(msg,(char*) "Using Clipboard");
}

    StrAdd(msg,filetype);

if( chg==1 ) { StrAdd(msg,(char*) "\n\nPicture was scaled");}
if( chg==2 ) { StrAdd(msg,(char*) "\n\nPicture was truncated");}

// IF os3 THEN StrAdd(msg,'\n*** OS3 REMAP MODE! ***')

// StrAdd(msg,'\n\nPlease select output format')

}

tempGlobal_ARRAY_OF_easystruct9 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);

tempGlobal_ARRAY_OF_easystruct9 [0].es_TextFormat = (CONST_STRPTR) (long) msg;

EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct9  ,(ULONG*) 0 ,(APTR) (long) NULL );
	return ;

// 'ZX82|BYTES|TAP|SCR'


}

void autoname(long updategadgets, long extonly) {
char* tmpfn=NULL;
long temp_QUAD;
try {
	tmpfn = NewString(1024);
	temp_QUAD = exception ;
	exception = 0 ;

if( autonaming==0 ) { Raise(0);}
if( EstrLen(filename)==0 ) { Raise(0);}

StrCopy(fname,filename);

StrCopy(tmpfn,FilePart(fname ));

if( EstrLen(scrfile)==0) {
    MidStr(drw,fname,0,EstrLen(fname)-EstrLen(tmpfn));
    extonly=0;
}

if( extonly ) { StrCopy(tmpfn,FilePart(scrfile ));}

dot=InStr(tmpfn,(char*) ".");

if( dot!=-1) {
    StrCopy(fname,tmpfn,dot);
} else {
    StrCopy(fname,tmpfn);
}

if( saveformat==0) {
    if( gifsave) {
        StrAdd(fname,(char*) ".gif");
    } else {
        StrAdd(fname,(char*) ".scr");
    }
}
if( saveformat==1 ) { StrAdd(fname,(char*) ".ZX82");}
if( saveformat==2 ) { StrAdd(fname,(char*) ".bytes");}
if( saveformat==3 ) { StrAdd(fname,(char*) ".tap");}
if( saveformat==4 ) { StrAdd(fname,(char*) ".tzx");}
/* IF saveformat=5 THEN StrAdd(fname,'.plus3')
     sod this, .ZXT is the best I can do, and that's not identical */

StrCopy(scrfile,drw);
(BOOL)(0!=AddPart(scrfile ,fname ,(ULONG) (long) 1024 ));

if( updategadgets) {
    tempGlobal_ILIST28 [1]= (long) FilePart(scrfile );
    GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_SAVESTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST28  );
//    IF (EstrLen(filename)<>NIL AND EstrLen(scrfile)<>NIL) THEN Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,FALSE,NIL]) ELSE Gt_SetGadgetAttrsA(gads[GADG_START],wptr,NIL,[GA_DISABLED,TRUE,NIL])
}
} catch(...) {}
	DisposeString(tmpfn );
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;

}

void asksave() {
char* tmpfname=NULL;
long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;

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

if( aslx  & asly) {

tempGlobal_ILIST29 [5]= (long) drw;

tempGlobal_ILIST29 [7]= (long) fname;

tempGlobal_ILIST29 [11]= aslx;

tempGlobal_ILIST29 [13]= asly;

savereq=(struct FileRequester*) AllocAslRequest((ULONG) ASL_FileRequest ,(struct TagItem*) tempGlobal_ILIST29  );

} else {
tempGlobal_ILIST30 [5]= (long) drw;
tempGlobal_ILIST30 [7]= (long) fname;
savereq=(struct FileRequester*) AllocAslRequest((ULONG) ASL_FileRequest ,(struct TagItem*) tempGlobal_ILIST30  );
}



okay=-AslRequest((APTR) (long) savereq ,(struct TagItem*) NULL );

if( okay==FALSE ) { Raise(0);}

StrCopy(scrfile,savereq->fr_Drawer);

if( strlen(savereq->fr_File )==0) {
    tmpfname=FilePart(filename );
    dot=InStr(tmpfname,(char*) ".");

    if( dot!=-1) {
        StrCopy(fname,tmpfname,dot);
    } else {
        StrCopy(fname,tmpfname);
    }

    if( saveformat==0) {
        if( gifsave) {
            StrAdd(fname,(char*) ".gif");
        } else {
            StrAdd(fname,(char*) ".scr");
        }
    }
    if( saveformat==1 ) { StrAdd(fname,(char*) ".ZX82");}
    if( saveformat==2 ) { StrAdd(fname,(char*) ".bytes");}
    if( saveformat==3 ) { StrAdd(fname,(char*) ".tap");}
    if( saveformat==4 ) { StrAdd(fname,(char*) ".tzx");}

    (BOOL)(0!=AddPart(scrfile ,fname ,(ULONG) (long) 1024 ));
} else {
    (BOOL)(0!=AddPart(scrfile ,savereq->fr_File ,(ULONG) (long) 1024 ));
}

StrCopy(fname,savereq->fr_File);
StrCopy(drw,savereq->fr_Drawer);

if( savereq ) { FreeAslRequest((APTR) (long) savereq );}
savereq=(struct FileRequester*) NULL;
} catch(...) {}
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;

// ENDIF




}

void handle_gadgets(long noarexx) {
  struct IntuiMessage* imsg=NULL; struct Gadget* gad=NULL; long terminated=FALSE, class2;
  long menunumber, menunum, itemnum, subnum, signal=NULL, winsig, appwinsig;
  struct WBArg* argptr=NULL; struct MenuItem* item=NULL; long arexxsig, rexxcmd; struct RexxMsg* arexxmsg=NULL;
  long rc; char* result=NULL; long quote, tzxsize, stop=0;
  char* temp_ARRAY_OF_CHAR=NULL; long temp_QUAD;
try {
	result = NewString(128);
	temp_QUAD = exception ;
	exception = 0 ;

// noarexx:=1 -> temp for portablE port


if( noarexx==0) {
    arexxport=rx_OpenPort((char*) "PDHFIC");
    arexxsig= ret2 ;
} else {
    arexxport=(struct MsgPort*) NULL;
    arexxsig=(long) NULL;
     tempGlobal_ARRAY_OF_easystruct10 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
     EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct10  ,(ULONG*) 0 ,(APTR) (long) NULL );
}


curwin=wptr;
vp=& mysc->ViewPort;
cmap=vp->ColorMap;
// stdrast:=rp

flashcol=ObtainPen(cmap ,(ULONG) -1 ,(ULONG) (flashred << 24) ,(ULONG) (flashgrn << 24) ,(ULONG) (flashblu << 24) ,(ULONG) (PEN_EXCLUSIVE | PEN_NO_SETCOLOR) );

if( flashcol!=-1) {
        if( - (flashred!=-1)  & - (flashgrn!=-1)  & - (flashblu!=-1)) {
            SetColour(mysc,(UBYTE) flashcol,(UBYTE) flashred,(UBYTE) flashgrn,(UBYTE) flashblu);
        }
    SetAPen(rp ,(ULONG) flashcol );
} else {
    if( (flashcol=ObtainBestPenA(cmap ,(ULONG) (flashred << 24) ,(ULONG) (flashgrn << 24) ,(ULONG) (flashblu << 24) ,(struct TagItem*) tempGlobal_ILIST31  ))) {
        obtainbest=1;
        SetAPen(rp ,(ULONG) flashcol );
    } else {
        SetAPen(rp ,(ULONG) 1 );
    }
}

appwinport=CreateMsgPort();
appwin=(long) AddAppWindowA((ULONG) 1 ,(ULONG) 0 ,wptr ,appwinport ,(struct TagItem*) NULL );


winsig=1 << (long) wptr->UserPort->mp_SigBit;
appwinsig=1 << (long) appwinport->mp_SigBit;
// arexxsig:=Shl(1,arexxport.sigbit)

  do {
    signal=(long) Wait((ULONG) (winsig | appwinsig | arexxsig) );

if( signal & winsig) {

    // Use Gt_GetIMsg() and Gt_ReplyIMsg() for handling IntuiMessages
    // with GadTools gadgets.
    while( (struct IntuiMessage*) (- (terminated==FALSE)  & (long) (imsg=GT_GetIMsg(wptr->UserPort )))) {
      // Gt_ReplyIMsg() at end of loop
      class2=(long) imsg->Class;
      switch( class2) {
      case IDCMP_GADGETUP  :// Buttons only report GADGETUP
        gad=(struct Gadget*) imsg->IAddress;
        if( (short) (long) gad->GadgetID==(short) GADG_LOAD) {
        SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST32  );
          askload();
          identifyfile();
          tempGlobal_ILIST33 [1]= (long) FilePart(filename );
          GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_LOADSTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST33  );
        autoname(1) ;// autonaming
        if( ((long) - (EstrLen(filename)!=(long) NULL )& EstrLen(scrfile))!=(long) NULL) { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST34  );} else { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST35  );}
        SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST36  );
        }
        if( (short) (long) gad->GadgetID==(short) GADG_SAVE) {
        SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST37  );
        asksave();
// GTST_STRING
          tempGlobal_ILIST38 [1]= (long) FilePart(scrfile );
          GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_SAVESTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST38  );
        if( ((long) - (EstrLen(filename)!=(long) NULL )& EstrLen(scrfile))!=(long) NULL) { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST39  );} else { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST40  );}
        SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST41  );
        }
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
        if( (short) (long) gad->GadgetID==(short) GADG_START) {
        if( askover) {
            if( (lock=(long) Lock(scrfile ,ACCESS_READ ))) {
                stop=0;
                tempGlobal_ARRAY_OF_easystruct11 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
                if( EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct11  ,(ULONG*) 0 ,(APTR) (long) NULL )==0 ) { stop=1;}
                (BOOL)(0!=Close((BPTR) lock ));
                lock=(long) NULL;
            }
        }
        if( stop==0) {
            SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST42  );
            openprogresswin();
            identifyfile() ;// TEMP???!
               if( dt2ppm(scale) ) { ppmtoscr();}
              closeprogresswin();
            SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST43  );
        }
        stop=0;
        }
        if( (short) (long) gad->GadgetID==(short) GADG_TYPE) {
        if( (short) (long) imsg->Code==(short) 6) {
            saveformat=0;
            gifsave=1;
        } else {
            saveformat=(long) imsg->Code;
            gifsave=0;
        }
        autoname(1,1);
        }
        if( (short) (long) gad->GadgetID==(short) GADG_ROMREMAP) {
          os3=(long) imsg->Code;
             // Gt_SetGadgetAttrsA(gads[ADV_ZXDTPAL],wptr,NIL,[GA_DISABLED,(os3-1),NIL])

            if( advopts) {
                tempGlobal_ILIST44 [1]= os3;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_BRIGHT] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST44  );
                tempGlobal_ILIST45 [1]= os3;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_COLOUR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST45  );
                tempGlobal_ILIST46 [1]= os3;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_WHITE] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST46  );
                tempGlobal_ILIST47 [1]= os3;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_BLUCYN] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST47  );
                tempGlobal_ILIST48 [1]= os3;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_GRNMAG] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST48  );
                tempGlobal_ILIST49 [1]= os3;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_REDYEL] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST49  );
            }
        }

        if( (short) (long) gad->GadgetID==(short) GADG_SCALE ) { scale=(long) imsg->Code;}
        if( (short) (long) gad->GadgetID==(short) ADV_ZXDTPAL) {
            dither=(long) imsg->Code;
        }
      
        if( (short) (long) gad->GadgetID==(short) ADV_SMOOTH) {
            smooth=(long) imsg->Code;
        }
      
      /* Advanced Options */
      
        if( (short) (long) gad->GadgetID==(short) ADV_BRIGHT) {
           brthr=(long) imsg->Code;
          tempGlobal_ILIST50 [1]= (long) imsg->Code;
          GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_BRIGHTNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST50  );
        }
        
        if( (short) (long) gad->GadgetID==(short) ADV_COLOUR) {
            thr=(long) imsg->Code;
          tempGlobal_ILIST51 [1]= (long) imsg->Code;
          GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_COLOURNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST51  );
        }
        
        if( (short) (long) gad->GadgetID==(short) ADV_WHITE) {
            wtthr=(long) imsg->Code;
          tempGlobal_ILIST52 [1]= (long) imsg->Code;
          GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_WHITENUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST52  );
        }

        if( (short) (long) gad->GadgetID==(short) ADV_BLUCYN) {
            blucyn=(long) imsg->Code;
          tempGlobal_ILIST53 [1]= (long) imsg->Code;
          GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_BLUCYNNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST53  );
        }
        
        if( (short) (long) gad->GadgetID==(short) ADV_GRNMAG) {
            grnmag=(long) imsg->Code;
          tempGlobal_ILIST54 [1]= (long) imsg->Code;
          GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_GRNMAGNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST54  );
        }
        
        if( (short) (long) gad->GadgetID==(short) ADV_REDYEL) {
            redyel=(long) imsg->Code;
          tempGlobal_ILIST55 [1]= (long) imsg->Code;
          GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_REDYELNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST55  );
        }

        if( (short) (long) gad->GadgetID==(short) ADV_FLASHRED) {
            flashred=(long) imsg->Code;
          tempGlobal_ILIST56 [1]= (long) imsg->Code;
          GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_FLASHREDNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST56  );
//        SetRGB32(vp,flashcol,(flashred!/255),(flashgrn!/255),(flashblu!/255))
drawflashcolour();
        }

        if( (short) (long) gad->GadgetID==(short) ADV_FLASHGRN) {
            flashgrn=(long) imsg->Code;
          tempGlobal_ILIST57 [1]= (long) imsg->Code;
          GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_FLASHGRNNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST57  );
//        SetRGB32(vp,flashcol,(flashred/255)!,(flashgrn/255)!,(flashblu/255)!)
drawflashcolour();
        }

        if( (short) (long) gad->GadgetID==(short) ADV_FLASHBLU) {
            flashblu=(long) imsg->Code;
          tempGlobal_ILIST58 [1]= (long) imsg->Code;
          GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_FLASHBLUNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST58  );
//        SetRGB32(vp,flashcol,(flashred/255)!,(flashgrn/255)!,(flashblu/255)!)
drawflashcolour();
        }

      /**/
      
      	break;
      case IDCMP_CLOSEWINDOW:
/*
        IF EasyRequestArgs(0,[SIZEOF easystruct,0,'WHAT???!','Do you REALLY want to quit?','Yes|No']:easystruct,0,NIL) THEN 
*/
terminated=TRUE;
      	break;
      case IDCMP_REFRESHWINDOW:
        // This handling is REQUIRED with GadTools.
        if( advopts) {
            tempGlobal_ILIST59 [5]= vi;
            DrawBevelBoxA(rp ,6 ,(pixsep*(long) 6+(long) ((long) mysc->Font->ta_YSize*5)+tbar+(long) 1) ,258 ,(pixsep*(long) 7+(long) ((long) mysc->Font->ta_YSize*6)+(long) 2) ,(struct TagItem*) tempGlobal_ILIST59  );
             
            tempGlobal_ILIST60 [3]= vi;
            DrawBevelBoxA(rp ,227 ,(pixsep*(long) 14+(long) ((long) mysc->Font->ta_YSize*11)+tbar) ,35 ,(pixsep*(long) 2+(long) ((long) mysc->Font->ta_YSize*3)+(long) 4) ,(struct TagItem*) tempGlobal_ILIST60  );

            drawflashcolour();
            
            }
            
        GT_BeginRefresh(wptr );
        GT_EndRefresh(wptr , -TRUE );
      	break;

      case IDCMP_MENUPICK:
        SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST61  );
        menunumber=(short) (long) imsg->Code  & (long) 0xFFFF;
        while(menunumber!=MENUNULL) {
            item=ItemAddress((struct Menu*) menustrip ,(UWORD) menunumber );
            menunum=menunumber  & (long) 0x1F;
            itemnum=(menunumber) >> 5& (long) 0x3F;
            subnum=(menunumber) >> 11& (long) 0x1F;
            
//            WriteF('\d\n\d\n',menunum,itemnum)
            
            if( menunum==0 ) {// Project
              if( itemnum==0 ) { about();}
              if( itemnum==2) {
/*                IF EasyRequestArgs(0,[SIZEOF easystruct,0,'WHAT???!','Do you REALLY want to quit?','Yes|No'],0,NIL) THEN */
                terminated=TRUE;
              }
            }
            if( menunum==1 ) {// Picture
                if( itemnum==0) {
                  askload();
                  identifyfile();
                  tempGlobal_ILIST62 [1]= (long) FilePart(filename );
                  GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_LOADSTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST62  );
                  autoname(1);
                if( ((long) - (EstrLen(filename)!=(long) NULL )& EstrLen(scrfile))!=(long) NULL) { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST63  );} else { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST64  );}
                }

            if( itemnum==1) {
                     clipboard=1;
                     if( EstrLen(filename)==0 ) { StrCopy(filename,(char*) "Clipboard");}
/*                     ELSE
                         MidStr(tmpstr,filename,0,EstrLen(filename)-EstrLen(FilePart(filename)))
                         StrCopy(filename,tmpstr)
                     ENDIF
*/
                     StrCopy(fname,(char*) "Clipboard");

                     StrCopy(filetype,(char*) "\nUnit: ");
                     StrAdd(filetype,RealF(tmpstr,(float) clipunit,0));
                     chg=0;
                  GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_LOADSTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST65  );
//                  autoname(1)
            }

            if( itemnum==2) {
                if( EstrLen(filename)!=(long) NULL) {
                    StrCopy(tmpstr,(char*) "Run >NIL: SYS:Utilities/Multiview ");
                    if( clipboard==0) {
                        StrAdd(tmpstr,(char*) "\"");
                        StrAdd(tmpstr,filename);
                        StrAdd(tmpstr,(char*) "\"");
                    } else {
                        StrAdd(tmpstr,(char*) "CLIPBOARD");
                    }
                    if( mvscreen ) { StrAdd(tmpstr,(char*) " SCREEN");}
                    SystemTagList(tmpstr ,(struct TagItem*) NULL );
//                    Execute(tmpstr,0,0)
                }
            }

            if( itemnum==4) {
            asksave();
              tempGlobal_ILIST66 [1]= (long) FilePart(scrfile );
              GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_SAVESTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST66  );
            if( ((long) - (EstrLen(filename)!=(long) NULL )& EstrLen(scrfile))!=(long) NULL) { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST67  );} else { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST68  );}
            }

            if( itemnum==5) {
                if( EstrLen(scrfile)!=(long) NULL) {
                    StrCopy(tmpstr,(char*) "Run >NIL: SYS:Utilities/Multiview ");
                    if( clipboard==0) {
                        StrAdd(tmpstr,(char*) "\"");
                        StrAdd(tmpstr,scrfile);
                        StrAdd(tmpstr,(char*) "\"");
                    } else {
                        StrAdd(tmpstr,(char*) "CLIPBOARD");
                    }
                    if( mvscreen ) { StrAdd(tmpstr,(char*) " SCREEN");}
                    SystemTagList(tmpstr ,(struct TagItem*) NULL );
//                    Execute(tmpstr,0,0)
                }
            }

              if( itemnum==7 ) { askoutput();}

            }
            
                if( menunum==2 ) {// Prefs
                      if( itemnum==0) {
                          if( (short) (long) item->Flags & CHECKED) {
                              if( advopts==0 ) { advancedoptions();}
                          } else {
                              if( advopts ) { advancedoptions();}
                          }
                      }
                      if( itemnum==1) {
                          if( (short) (long) item->Flags & CHECKED) { mvscreen=1 ;} else { mvscreen=0;}
                      }
                      if( itemnum==2) {
                          if( (short) (long) item->Flags & CHECKED) { askover=1 ;} else { askover=0;}
                      }
                      
                      if( itemnum==3) {
                          if( (short) (long) item->Flags & CHECKED) { nofilter=0 ;} else { nofilter=1;}
                      }
                      
                if( itemnum==4) {
                          if( (short) (long) item->Flags & CHECKED) { autonaming=1 ;} else { autonaming=0;}
                      }

                      if( itemnum==5) {
                          if( (short) (long) item->Flags & CHECKED) { noprogress=0 ;} else { noprogress=1;}
                      }
                      
                      if( itemnum==7) {
                          if( subnum==1) {
                              if( (short) (long) item->Flags & CHECKED) { allelseink=1 ;} else { allelseink=0;}
                          }
                          if( subnum==2) {
                              if( (short) (long) item->Flags & CHECKED) { allelseink=0 ;} else { allelseink=1;}
                          }
                      }
                      
                if( itemnum==8) {
                          if( (short) (long) item->Flags & CHECKED) { saveattr=1 ;} else { saveattr=0;}
                }
                
                if( itemnum==11) {
                          if( (short) (long) item->Flags & CHECKED) { simpleremap=1 ;} else { simpleremap=0;}
                }

                      if( itemnum==10) {
                          if( subnum==0) {
                              if( (short) (long) item->Flags & CHECKED) { dtype=0 ;} else { dtype=1;}
                          }
                          if( subnum==1) {
                              if( (short) (long) item->Flags & CHECKED) { dtype=1 ;} else { dtype=0;}
                          }
                      }


                if( itemnum==9) {
                          if( (short) (long) item->Flags & CHECKED) { rompal=0 ;} else { rompal=1;}
                           os3setup();
                }

                if( itemnum==12) {
                          if( (short) (long) item->Flags & CHECKED) { remapafterdither=1 ;} else { remapafterdither=0;}
                }


                      if( itemnum==14) {
                          if( subnum==0) {
                        if( (short) (long) item->Flags & CHECKED) { tzxarray[0]=1 ;} else { tzxarray[0]=0;}
                    }
                          if( subnum==2) {
                        if( (short) (long) item->Flags & CHECKED) {
                            tzxarray[1]=1;
                            tzxarray[2]=0;
                        } else {
                            tzxarray[1]=0;
                            tzxarray[2]=1;
                        }
                    }
                          if( subnum==3) {
                        if( (short) (long) item->Flags & CHECKED) {
                            tzxarray[2]=1;
                            tzxarray[1]=0;
                        } else {
                            tzxarray[2]=0;
                            tzxarray[1]=1;
                        }
                    }
                      }
                      
                      if( itemnum==15) {
                          if( (short) (long) item->Flags & CHECKED) { tzxborder=subnum;}
                      }
                      
                      if( itemnum==16) {
                    if( (short) (long) item->Flags & CHECKED) { writeheader=1 ;} else { writeheader=0;}
                }
                      
                      if( itemnum==18) {
                    if( (short) (long) item->Flags & CHECKED) { snapshotwindows=1 ;} else { snapshotwindows=0;}
                }
                      
                if( itemnum==19 ) { saveconfig();}
            }
            
            if( menunum==3 ) {// Help
        
              if( itemnum==0) {
                if( saveformat==0) {
                    if( gifsave) {
                        tempGlobal_ARRAY_OF_easystruct12 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
                        EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct12  ,(ULONG*) 0 ,(APTR) (long) NULL );
                    } else {
                        tempGlobal_ARRAY_OF_easystruct13 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
                        EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct13  ,(ULONG*) 0 ,(APTR) (long) NULL );
                    }
                }
                  if( saveformat==1 ) {
                  	tempGlobal_ARRAY_OF_easystruct14 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
                  	EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct14  ,(ULONG*) 0 ,(APTR) (long) NULL );
                  }
                  if( saveformat==2 ) {
                  	tempGlobal_ARRAY_OF_easystruct15 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
                  	EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct15  ,(ULONG*) 0 ,(APTR) (long) NULL );
                  }
                     if( saveformat==3 ) {
                     	tempGlobal_ARRAY_OF_easystruct16 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
                     	EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct16  ,(ULONG*) 0 ,(APTR) (long) NULL );
                     }
                  if( saveformat==5 ) {
                  	tempGlobal_ARRAY_OF_easystruct17 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
                  	EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct17  ,(ULONG*) 0 ,(APTR) (long) NULL );
                  }
                  if( saveformat==4) {
                      StrCopy(tmpstr,(char*) "TZX (ZX Tape)\n\nThis is a \"digital tape\" format, which\nsupports several different types of\nblock including turbo loaders.\n\nSize: ");
                      if( tzxarray[1] ) { StrAdd(tmpstr,(char*) "6953");}
                      if( tzxarray[2] ) { StrAdd(tmpstr,(char*) "6976");}
                      if( tzxarray[0] ) { StrAdd(tmpstr,(char*) " + archive info (varies)");}
                      StrAdd(tmpstr,(char*) "\n\nUsed by: TZX -> TAP convertors :-)\n\nMore information available from WOS at\nhttp://www.void.demon.nl/spectrum.html");
                      tempGlobal_ARRAY_OF_easystruct18 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
                      tempGlobal_ARRAY_OF_easystruct18 [0].es_TextFormat = (CONST_STRPTR) (long) tmpstr;
                      EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct18  ,(ULONG*) 0 ,(APTR) (long) NULL );
                  }
            }
        
              if( itemnum==1) {
/*
                IF alsbase
                    Als(0)
                ELSE
                    EasyRequestArgs(0,[SIZEOF easystruct,0,'PDHFIC','Could not open als.library 6+','OK']:easystruct,0,NIL)
                ENDIF
*/
              }
        }

            if( menunum==4 ) {// ARexx
                if( itemnum==0 ) { arexxecute();}
            }

            menunumber=(short) (long) item->NextSelect  & (long) 0xFFFF;
            }
        SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST69  );

      	break;
      }
      // Use the toolkit message-replying function here...
      GT_ReplyIMsg(imsg );
    }
    
}

if( signal & appwinsig) {
    while( appwinmsg=(struct AppMessage*) GetMsg(appwinport )) {
        argptr=appwinmsg->am_ArgList;
        /* only interested in first msg */
//        WriteF('\s\n',argptr.name)
        SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST70  );
        (BOOL)(0!=NameFromLock(argptr->wa_Lock ,(char*) filebuf ,1024 ));
        StrCopy(filename,(char*) filebuf);
        StrCopy(loaddrw,filename);
          (BOOL)(0!=AddPart(filename ,(char*) argptr->wa_Name ,(ULONG) (long) 1024 ));
          StrCopy(fname,(char*) argptr->wa_Name);
        ReplyMsg((struct Message*) appwinmsg );
          identifyfile();
          tempGlobal_ILIST71 [1]= (long) FilePart(filename );
          GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_LOADSTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST71  );
        clipboard=0;
          autoname(1);
        if( ((long) - (EstrLen(filename)!=(long) NULL )& EstrLen(scrfile))!=(long) NULL) { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST72  );} else { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST73  );}
        SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST74  );
    }
}


if( arexxsig!=(long) NULL) {
    if( signal & arexxsig) {
        do {
            SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST75  );
            arexxmsg=rx_GetMsg(arexxport);
            rexxcmd= ret2 ;
            rc=0;
            StrCopy(result,(char*) "");
            if( (void*) arexxmsg!=NULL) {
                
                if( (char*) rexxcmd) {
                temp_ARRAY_OF_CHAR = UpperStr((char*) rexxcmd) ;
                } else { 
                temp_ARRAY_OF_CHAR = (char*) rexxcmd;
                }
                temp_ARRAY_OF_CHAR ;
                if( StrCmp((char*) rexxcmd,(char*) "HELLO")) {
                    tempGlobal_ARRAY_OF_easystruct19 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
                    EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct19  ,(ULONG*) 0 ,(APTR) (long) NULL );
                    StrCopy(result,(char*) "\251 1998-2001, 2008-2009 Unsatisfactory Software");
                }

                if( StrCmp((char*) rexxcmd,(char*) "QUICKCONVERT",12)) {
                clipboard=0;
                dot=InStr((char*) rexxcmd,(char*) "\"",1)+(long) 1;
                quote=InStr((char*) rexxcmd,(char*) "\"",dot+(long) 1);
                MidStr(filename,(char*) rexxcmd,dot,quote-dot);
                dot=InStr((char*) rexxcmd,(char*) "\"",quote+(long) 1)+(long) 1;
                quote=InStr((char*) rexxcmd,(char*) "\"",dot+(long) 1);
                MidStr(scrfile,(char*) rexxcmd,dot,quote-dot);
                  tempGlobal_ILIST76 [1]= (long) FilePart(filename );
                  GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_LOADSTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST76  );
                  tempGlobal_ILIST77 [1]= (long) FilePart(scrfile );
                  GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_SAVESTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST77  );
                if( ((long) - (EstrLen(filename)!=(long) NULL )& EstrLen(scrfile))!=(long) NULL) { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST78  );} else { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST79  );}
                rc=1;
                if( identifyfile()) {
                    rc=2;
                       if( dt2ppm(scale)) {
                           rc=0;
                           ppmtoscr();
                       }
                   }
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "SELECTSOURCE",12)) {
                dot=InStr((char*) rexxcmd,(char*) "\"",1)+(long) 1;
                quote=InStr((char*) rexxcmd,(char*) "\"",dot+(long) 1);
                if( quote==-1) {
                    askload();
                } else {
                    clipboard=0;
                    MidStr(filename,(char*) rexxcmd,dot,quote-dot);
                }
                
                identifyfile();
                  tempGlobal_ILIST80 [1]= (long) FilePart(filename );
                  GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_LOADSTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST80  );
                  autoname(1);
                if( ((long) - (EstrLen(filename)!=(long) NULL )& EstrLen(scrfile))!=(long) NULL) { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST81  );} else { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST82  );}
                StrCopy(result,filename);
                if( EstrLen(filename)==0 ) { rc=1;}
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "SELECTDEST",10)) {
                dot=InStr((char*) rexxcmd,(char*) "\"",1)+(long) 1;
                quote=InStr((char*) rexxcmd,(char*) "\"",dot+(long) 1);
                if( dot!=0) {
                    MidStr(filename,(char*) rexxcmd,dot,quote-dot);
                } else {
                    asksave();
                }
                  tempGlobal_ILIST83 [1]= (long) FilePart(filename );
                  GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_LOADSTR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST83  );
                if( ((long) - (EstrLen(filename)!=(long) NULL )& EstrLen(scrfile))!=(long) NULL) { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST84  );} else { GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_START] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST85  );}
                StrCopy(result,scrfile);
                if( EstrLen(scrfile)==0 ) { rc=1;}
            }

            if( StrCmp((char*) rexxcmd,(char*) "CONVERT")) {
                rc=1;
                if( ((long) - (EstrLen(filename)!=(long) NULL )& EstrLen(scrfile))!=(long) NULL) {
                    rc=2;
                    identifyfile();
                       if( dt2ppm(scale)) {
                           rc=0;
                           ppmtoscr();
                       }
                   }
               }
               
               if( StrCmp((char*) rexxcmd,(char*) "SCALE")) {
                   scale=1;
                GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_SCALE] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST86  );
               }

               if( StrCmp((char*) rexxcmd,(char*) "NOSCALE")) {
                   scale=0;
                GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_SCALE] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST87  );
               }

               if( StrCmp((char*) rexxcmd,(char*) "ROMREMAP")) {
                   os3=1;
                GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_ROMREMAP] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST88  );
               }

               if( StrCmp((char*) rexxcmd,(char*) "STDREMAP")) {
                   os3=0;
                GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_ROMREMAP] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST89  );
               }
               
                  if( StrCmp((char*) rexxcmd,(char*) "ALTPALETTE")) {
                   rompal=1;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_ZXDTPAL] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST90  );
                os3setup();
               }

               if( StrCmp((char*) rexxcmd,(char*) "ZXDTPALETTE")) {
                   rompal=0;
//                Gt_SetGadgetAttrsA(gads[ADV_ZXDTPAL],wptr,NIL,[GTCB_CHECKED,TRUE])
               }

               if( StrCmp((char*) rexxcmd,(char*) "DITHER")) {
                   dither=1;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_ZXDTPAL] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST91  );
               }

               if( StrCmp((char*) rexxcmd,(char*) "SMOOTH")) {
                   smooth=1;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_SMOOTH] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST92  );
               }

               if( StrCmp((char*) rexxcmd,(char*) "NOSMOOTH")) {
                   smooth=0;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_SMOOTH] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST93  );
               }

            if( StrCmp((char*) rexxcmd,(char*) "BRIGHTSENSE",11)) {
                MidStr(tmpstr,(char*) rexxcmd,12,ALL);
                brthr=Val(tmpstr);
                tempGlobal_ILIST94 [1]= brthr;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_BRIGHT] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST94  );
                tempGlobal_ILIST95 [1]= brthr;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_BRIGHTNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST95  );
            }

            if( StrCmp((char*) rexxcmd,(char*) "COLOURSENSE",11)) {
                MidStr(tmpstr,(char*) rexxcmd,12,ALL);
                thr=Val(tmpstr);
                tempGlobal_ILIST96 [1]= thr;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_COLOUR] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST96  );
                tempGlobal_ILIST97 [1]= thr;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_COLOURNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST97  );
            }

            if( StrCmp((char*) rexxcmd,(char*) "WHITESENSE",10)) {
                MidStr(tmpstr,(char*) rexxcmd,11,ALL);
                wtthr=Val(tmpstr);
                tempGlobal_ILIST98 [1]= wtthr;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_WHITE] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST98  );
                tempGlobal_ILIST99 [1]= wtthr;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_WHITENUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST99  );
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "REDYELLOW",9)) {
                MidStr(tmpstr,(char*) rexxcmd,10,ALL);
                redyel=Val(tmpstr);
                tempGlobal_ILIST100 [1]= redyel;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_REDYEL] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST100  );
                tempGlobal_ILIST101 [1]= redyel;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_REDYELNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST101  );
            }

            if( StrCmp((char*) rexxcmd,(char*) "BLUECYAN",8)) {
                MidStr(tmpstr,(char*) rexxcmd,9,ALL);
                blucyn=Val(tmpstr);
                tempGlobal_ILIST102 [1]= blucyn;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_BLUCYN] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST102  );
                tempGlobal_ILIST103 [1]= blucyn;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_BLUCYNNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST103  );
            }

            if( StrCmp((char*) rexxcmd,(char*) "GREENMAGENTA",12)) {
                MidStr(tmpstr,(char*) rexxcmd,13,ALL);
                grnmag=Val(tmpstr);
                tempGlobal_ILIST104 [1]= grnmag;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_GRNMAG] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST104  );
                tempGlobal_ILIST105 [1]= grnmag;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_GRNMAGNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST105  );
            }

            if( StrCmp((char*) rexxcmd,(char*) "FLASHRED",8)) {
                MidStr(tmpstr,(char*) rexxcmd,9,ALL);
                flashred=Val(tmpstr);
                tempGlobal_ILIST106 [1]= flashred;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_FLASHRED] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST106  );
                tempGlobal_ILIST107 [1]= flashred;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_FLASHREDNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST107  );
drawflashcolour();
            }

            if( StrCmp((char*) rexxcmd,(char*) "FLASHGREEN",10)) {
                MidStr(tmpstr,(char*) rexxcmd,11,ALL);
                flashgrn=Val(tmpstr);
                tempGlobal_ILIST108 [1]= flashgrn;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_FLASHGRN] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST108  );
                tempGlobal_ILIST109 [1]= flashgrn;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_FLASHGRNNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST109  );
drawflashcolour();
            }

            if( StrCmp((char*) rexxcmd,(char*) "FLASHBLUE",9)) {
                MidStr(tmpstr,(char*) rexxcmd,10,ALL);
                flashblu=Val(tmpstr);
                tempGlobal_ILIST110 [1]= flashblu;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_FLASHBLU] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST110  );
                tempGlobal_ILIST111 [1]= flashblu;
                GT_SetGadgetAttrsA((struct Gadget*) gads[ADV_FLASHBLUNUM] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST111  );
drawflashcolour();
            }

            if( StrCmp((char*) rexxcmd,(char*) "VERSION")) {
                StrCopy(result,(char*) "3.3");
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "SAVEFORMAT",10)) {
                MidStr(tmpstr,(char*) rexxcmd,11,ALL);
                saveformat=Val(tmpstr);
                tempGlobal_ILIST112 [1]= saveformat;
                GT_SetGadgetAttrsA((struct Gadget*) gads[GADG_TYPE] ,wptr ,(struct Requester*) NULL ,(struct TagItem*) tempGlobal_ILIST112  );
                if( saveformat==6) {
                    saveformat=0;
                    gifsave=1;
                } else {
                    gifsave=0;
                }
            }

            if( StrCmp((char*) rexxcmd,(char*) "GETSAVEFORMAT")) {
                if( saveformat==0) {
                    if( gifsave) {
                        StrCopy(result,(char*) "GIF");
                    } else {
                        StrCopy(result,(char*) "SCR");
                    }
                }
                if( saveformat==1 ) { StrCopy(result,(char*) "ZX82");}
                if( saveformat==2 ) { StrCopy(result,(char*) "BYTES");}
                if( saveformat==3 ) { StrCopy(result,(char*) "TAP");}
                if( saveformat==4 ) { StrCopy(result,(char*) "TZX");}
                if( saveformat==5 ) { StrCopy(result,(char*) "+3DOS");}
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETSOURCE")) {
                StrCopy(result,filename);
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETDEST")) {
                StrCopy(result,scrfile);
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETBRIGHT")) {
                StrCopy(result,RealF(tmpstr,(float) brthr,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETCOLOUR")) {
                StrCopy(result,RealF(tmpstr,(float) thr,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETWHITE")) {
                StrCopy(result,RealF(tmpstr,(float) wtthr,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETREDYEL")) {
                StrCopy(result,RealF(tmpstr,(float) redyel,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETBLUCYN")) {
                StrCopy(result,RealF(tmpstr,(float) blucyn,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETGRNMAG")) {
                StrCopy(result,RealF(tmpstr,(float) grnmag,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETFLASHRED")) {
                StrCopy(result,RealF(tmpstr,(float) flashred,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETFLASHGRN")) {
                StrCopy(result,RealF(tmpstr,(float) flashgrn,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETFLASHBLU")) {
                StrCopy(result,RealF(tmpstr,(float) flashblu,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETSCALE")) {
                StrCopy(result,RealF(tmpstr,(float) scale,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETSMOOTH")) {
                StrCopy(result,RealF(tmpstr,(float) smooth,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETDITHER")) {
                StrCopy(result,RealF(tmpstr,(float) dither,0));
            }

            if( StrCmp((char*) rexxcmd,(char*) "GETALTPAL")) {
                StrCopy(result,RealF(tmpstr,(float) rompal,0));
            }
            
            if( StrCmp((char*) rexxcmd,(char*) "GETROMREMAP")) {
                StrCopy(result,RealF(tmpstr,(float) os3,0));
            }
            
               if( StrCmp((char*) rexxcmd,(char*) "QUIT")) {
                   terminated=TRUE;
               }

            // WriteF('ARexx: \s\n',rexxcmd)
            rx_ReplyMsg(arexxmsg,rc,result);
        }
        SetWindowPointerA(wptr ,(struct TagItem*) tempGlobal_ILIST113  );
    } while(( (void*) arexxmsg==NULL)==0);
}
}

  } while(( terminated)==0);
} catch(...) {}
	DisposeString(result );
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;

}

void advancedoptions() {
if( wptr->Height==tbar+(long) 2  ) { ZipWindow(wptr );}

if( advopts) {
/*    SizeWindow(wptr,0,-((pixsep*8)+(mysc.font.ysize*6))) *//* 240,0 */
    ChangeWindowBox(wptr ,wptr->LeftEdge ,wptr->TopEdge ,270 ,(pixsep*(long) 6+(long) ((long) mysc->Font->ta_YSize*5)+tbar+(long) 10) );
    RemoveGList(wptr ,(struct Gadget*) gads[ADV_BRIGHTNUM] ,-1 );
    advopts=0;
} else {
/*
    IF mysc.height-(wptr.topedge+wptr.height) < ((pixsep*8)+(mysc.font.ysize*6))
        MoveWindow(wptr,0,-((pixsep*8)+(mysc.font.ysize*6)-(mysc.height-(wptr.topedge+wptr.height))))
    ENDIF

    SizeWindow(wptr,0,((pixsep*8)+(mysc.font.ysize*6))) ** 240,0 **
*/

//    ChangeWindowBox(wptr,wptr.leftedge,wptr.topedge,270,((pixsep*14)+(mysc.font.ysize*11)+tbar+2))
    ChangeWindowBox(wptr ,wptr->LeftEdge ,wptr->TopEdge ,270 ,(pixsep*(long) 17+(long) ((long) mysc->Font->ta_YSize*14)+tbar+(long) 4) );
advgadgets();
    AddGList(wptr ,(struct Gadget*) gads[ADV_BRIGHTNUM] ,(ULONG) -1 ,-1 ,(struct Requester*) NULL );
    RefreshGList((struct Gadget*) gads[ADV_BRIGHTNUM] ,wptr ,(struct Requester*) NULL ,-1 );
    GT_RefreshWindow(wptr ,(struct Requester*) NULL );
    advopts=1;
}
	return ;

}

void advgadgets() {
long gad, top;

top=pixsep*(long) 6+(long) ((long) mysc->Font->ta_YSize*5);

//  mysc:=LockPubScreen(NIL)
//  tbar:=mysc.font.ysize+mysc.wbortop-1 /* true value is +2 */

//  vi:=GetVisualInfoA(mysc, [NIL])
  // GadTools gadgets require this step to be taken
  gad=(long) CreateContext(advglist );

  tempGlobal_ARRAY_OF_newgadget11 [0].ng_TopEdge = pixsep+tbar+top;

  tempGlobal_ARRAY_OF_newgadget11 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget11 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget11 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST114 [1]= brthr;

  gads[ADV_BRIGHTNUM]=(long) CreateGadgetA((ULONG) NUMBER_KIND ,(struct Gadget*) gad ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget11  ,(struct TagItem*) tempGlobal_ILIST114  );

  tempGlobal_ARRAY_OF_newgadget12 [0].ng_TopEdge = pixsep*(long) 2+(long) mysc->Font->ta_YSize+tbar+top;

  tempGlobal_ARRAY_OF_newgadget12 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget12 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget12 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST115 [1]= thr;

  gads[ADV_COLOURNUM]=(long) CreateGadgetA((ULONG) NUMBER_KIND ,(struct Gadget*) gads[ADV_BRIGHTNUM] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget12  ,(struct TagItem*) tempGlobal_ILIST115  );

  tempGlobal_ARRAY_OF_newgadget13 [0].ng_TopEdge = pixsep*(long) 3+(long) ((long) mysc->Font->ta_YSize*2)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget13 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget13 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget13 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST116 [1]= wtthr;

  gads[ADV_WHITENUM]=(long) CreateGadgetA((ULONG) NUMBER_KIND ,(struct Gadget*) gads[ADV_COLOURNUM] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget13  ,(struct TagItem*) tempGlobal_ILIST116  );

  tempGlobal_ARRAY_OF_newgadget14 [0].ng_TopEdge = pixsep+tbar+top;

  tempGlobal_ARRAY_OF_newgadget14 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget14 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget14 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST117 [1]= os3;

  tempGlobal_ILIST117 [9]= brthr;

  gads[ADV_BRIGHT]=(long) CreateGadgetA((ULONG) SLIDER_KIND ,(struct Gadget*) gads[ADV_WHITENUM] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget14  ,(struct TagItem*) tempGlobal_ILIST117  );

  tempGlobal_ARRAY_OF_newgadget15 [0].ng_TopEdge = pixsep*(long) 2+(long) mysc->Font->ta_YSize+tbar+top;

  tempGlobal_ARRAY_OF_newgadget15 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget15 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget15 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST118 [1]= os3;

  tempGlobal_ILIST118 [9]= thr;

  gads[ADV_COLOUR]=(long) CreateGadgetA((ULONG) SLIDER_KIND ,(struct Gadget*) gads[ADV_BRIGHT] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget15  ,(struct TagItem*) tempGlobal_ILIST118  );

  tempGlobal_ARRAY_OF_newgadget16 [0].ng_TopEdge = pixsep*(long) 3+(long) ((long) mysc->Font->ta_YSize*2)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget16 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget16 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget16 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST119 [1]= os3;

  tempGlobal_ILIST119 [9]= wtthr;

  gads[ADV_WHITE]=(long) CreateGadgetA((ULONG) SLIDER_KIND ,(struct Gadget*) gads[ADV_COLOUR] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget16  ,(struct TagItem*) tempGlobal_ILIST119  );

  tempGlobal_ARRAY_OF_newgadget17 [0].ng_TopEdge = pixsep*(long) 4+(long) ((long) mysc->Font->ta_YSize*3)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget17 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget17 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget17 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST120 [1]= os3;

  tempGlobal_ILIST120 [9]= redyel;

  gads[ADV_REDYEL]=(long) CreateGadgetA((ULONG) SLIDER_KIND ,(struct Gadget*) gads[ADV_WHITE] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget17  ,(struct TagItem*) tempGlobal_ILIST120  );

  tempGlobal_ARRAY_OF_newgadget18 [0].ng_TopEdge = pixsep*(long) 5+(long) ((long) mysc->Font->ta_YSize*4)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget18 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget18 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget18 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST121 [1]= os3;

  tempGlobal_ILIST121 [9]= blucyn;

  gads[ADV_BLUCYN]=(long) CreateGadgetA((ULONG) SLIDER_KIND ,(struct Gadget*) gads[ADV_REDYEL] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget18  ,(struct TagItem*) tempGlobal_ILIST121  );

  tempGlobal_ARRAY_OF_newgadget19 [0].ng_TopEdge = pixsep*(long) 6+(long) ((long) mysc->Font->ta_YSize*5)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget19 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget19 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget19 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST122 [1]= os3;

  tempGlobal_ILIST122 [9]= grnmag;

  gads[ADV_GRNMAG]=(long) CreateGadgetA((ULONG) SLIDER_KIND ,(struct Gadget*) gads[ADV_BLUCYN] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget19  ,(struct TagItem*) tempGlobal_ILIST122  );

  tempGlobal_ARRAY_OF_newgadget20 [0].ng_TopEdge = pixsep*(long) 4+(long) ((long) mysc->Font->ta_YSize*3)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget20 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget20 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget20 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST123 [1]= redyel;

  gads[ADV_REDYELNUM]=(long) CreateGadgetA((ULONG) NUMBER_KIND ,(struct Gadget*) gads[ADV_GRNMAG] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget20  ,(struct TagItem*) tempGlobal_ILIST123  );

  tempGlobal_ARRAY_OF_newgadget21 [0].ng_TopEdge = pixsep*(long) 5+(long) ((long) mysc->Font->ta_YSize*4)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget21 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget21 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget21 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST124 [1]= blucyn;

  gads[ADV_BLUCYNNUM]=(long) CreateGadgetA((ULONG) NUMBER_KIND ,(struct Gadget*) gads[ADV_REDYELNUM] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget21  ,(struct TagItem*) tempGlobal_ILIST124  );

  tempGlobal_ARRAY_OF_newgadget22 [0].ng_TopEdge = pixsep*(long) 6+(long) ((long) mysc->Font->ta_YSize*5)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget22 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget22 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget22 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST125 [1]= grnmag;

  gads[ADV_GRNMAGNUM]=(long) CreateGadgetA((ULONG) NUMBER_KIND ,(struct Gadget*) gads[ADV_BLUCYNNUM] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget22  ,(struct TagItem*) tempGlobal_ILIST125  );

// IF mysc THEN UnlockPubScreen(mysc,NIL)

  tempGlobal_ARRAY_OF_newgadget23 [0].ng_TopEdge = pixsep*(long) 8+(long) ((long) mysc->Font->ta_YSize*6)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget23 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget23 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget23 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST126 [7]= flashred;

  gads[ADV_FLASHRED]=(long) CreateGadgetA((ULONG) SLIDER_KIND ,(struct Gadget*) gads[ADV_GRNMAGNUM] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget23  ,(struct TagItem*) tempGlobal_ILIST126  );

  tempGlobal_ARRAY_OF_newgadget24 [0].ng_TopEdge = pixsep*(long) 9+(long) ((long) mysc->Font->ta_YSize*7)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget24 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget24 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget24 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST127 [7]= flashgrn;

  gads[ADV_FLASHGRN]=(long) CreateGadgetA((ULONG) SLIDER_KIND ,(struct Gadget*) gads[ADV_FLASHRED] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget24  ,(struct TagItem*) tempGlobal_ILIST127  );

  tempGlobal_ARRAY_OF_newgadget25 [0].ng_TopEdge = pixsep*(long) 10+(long) ((long) mysc->Font->ta_YSize*8)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget25 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget25 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget25 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST128 [7]= flashblu;

  gads[ADV_FLASHBLU]=(long) CreateGadgetA((ULONG) SLIDER_KIND ,(struct Gadget*) gads[ADV_FLASHGRN] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget25  ,(struct TagItem*) tempGlobal_ILIST128  );

  tempGlobal_ARRAY_OF_newgadget26 [0].ng_TopEdge = pixsep*(long) 8+(long) ((long) mysc->Font->ta_YSize*6)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget26 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget26 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget26 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST129 [1]= flashred;

  gads[ADV_FLASHREDNUM]=(long) CreateGadgetA((ULONG) NUMBER_KIND ,(struct Gadget*) gads[ADV_FLASHBLU] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget26  ,(struct TagItem*) tempGlobal_ILIST129  );

  tempGlobal_ARRAY_OF_newgadget27 [0].ng_TopEdge = pixsep*(long) 9+(long) ((long) mysc->Font->ta_YSize*7)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget27 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget27 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget27 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST130 [1]= flashgrn;

  gads[ADV_FLASHGRNNUM]=(long) CreateGadgetA((ULONG) NUMBER_KIND ,(struct Gadget*) gads[ADV_FLASHREDNUM] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget27  ,(struct TagItem*) tempGlobal_ILIST130  );

  tempGlobal_ARRAY_OF_newgadget28 [0].ng_TopEdge = pixsep*(long) 10+(long) ((long) mysc->Font->ta_YSize*8)+tbar+top;

  tempGlobal_ARRAY_OF_newgadget28 [0].ng_Height = (short) (long) ((long) mysc->Font->ta_YSize+4);

  tempGlobal_ARRAY_OF_newgadget28 [0].ng_TextAttr = mysc->Font;

  tempGlobal_ARRAY_OF_newgadget28 [0].ng_VisualInfo = (APTR) vi;

  tempGlobal_ILIST131 [1]= flashblu;

  gads[ADV_FLASHBLUNUM]=(long) CreateGadgetA((ULONG) NUMBER_KIND ,(struct Gadget*) gads[ADV_FLASHGRNNUM] ,(struct NewGadget*) tempGlobal_ARRAY_OF_newgadget28  ,(struct TagItem*) tempGlobal_ILIST131  );
	return ;

/*
  gads[ADV_FLASHCOL]:=CreateGadgetA(PALETTE_KIND, gads[ADV_FLASHBLUNUM],
                    [220, ((pixsep*7)+(mysc.font.ysize*6)+tbar+top),
                     30, ((mysc.font.ysize+4)*3)+(pixsep*2),
                     '', mysc.font,
                     ADV_FLASHBLUNUM, 0,
                     vi, NIL]:newgadget,
                    [GTPA_COLOR,flashcol,GTPA_COLOROFFSET,flashcol,GTPA_NUMCOLORS,1,NIL])
*/

}

void saveconfig() {
long olddir=-1, oldtooltypes; long newtooltypes[40]; char* tmpstr2=NULL;
char* ttstr1=NULL;
char* ttstr2=NULL;
char* ttstr3=NULL;
char* ttstr4=NULL;
char* ttstr5=NULL;
char* ttstr6=NULL;
char* ttstr7=NULL;
char* ttstr8=NULL;
char* ttstr9=NULL;
char* ttstr10=NULL;
char* ttstr11=NULL;
char* ttstr12=NULL;
char* ttstr13=NULL;
char* ttstr14=NULL;
char* ttstr15=NULL;
char* ttstr16=NULL;
char* ttstr17=NULL;
char* ttstr18=NULL;
char* ttstr19=NULL;
char* ttstr20=NULL;
char* ttstr21=NULL;
char* ttstr22=NULL;
char* ttstr23=NULL;
char* ttstr24=NULL;
char* ttstr25=NULL;
char* ttstr26=NULL;
char* ttstr27=NULL;
char* ttstr28=NULL;
long temp_QUAD;
try {
	tmpstr2 = NewString(100);
	ttstr1 = NewString(20);
	ttstr2 = NewString(20);
	ttstr3 = NewString(20);
	ttstr4 = NewString(20);
	ttstr5 = NewString(20);
	ttstr6 = NewString(20);
	ttstr7 = NewString(20);
	ttstr8 = NewString(20);
	ttstr9 = NewString(20);
	ttstr10 = NewString(20);
	ttstr11 = NewString(30);
	ttstr12 = NewString(30);
	ttstr13 = NewString(30);
	ttstr14 = NewString(20);
	ttstr15 = NewString(20);
	ttstr16 = NewString(20);
	ttstr17 = NewString(20);
	ttstr18 = NewString(20);
	ttstr19 = NewString(20);
	ttstr20 = NewString(20);
	ttstr21 = NewString(20);
	ttstr22 = NewString(20);
	ttstr23 = NewString(20);
	ttstr24 = NewString(20);
	ttstr25 = NewString(20);
	ttstr26 = NewString(20);
	ttstr27 = NewString(20);
	ttstr28 = NewString(20);
	temp_QUAD = exception ;
	exception = 0 ;

// wbenchMsg:=wbmessage
// wbarg:=wbenchMsg.arglist

if( (long) wbarg->wa_Lock  & (long) - ((long) wbarg->wa_Name!=(long) 0)  ) { olddir=(long) CurrentDir(wbarg->wa_Lock );}

if( (dobj=GetDiskObject((char*) wbarg->wa_Name ))) {

oldtooltypes=(long) dobj->do_ToolTypes;

StringF(ttstr16,(char*) "COLOURSENSE=%ld",thr);
newtooltypes[0]=(long) ttstr16;
StringF(ttstr17,(char*) "BRIGHTSENSE=%ld",brthr);
newtooltypes[1]=(long) ttstr17;
StringF(ttstr18,(char*) "WHITESENSE=%ld",wtthr);
newtooltypes[2]=(long) ttstr18;
StringF(ttstr19,(char*) "GREENMAGENTA=%ld",grnmag);
newtooltypes[3]=(long) ttstr19;
StringF(ttstr20,(char*) "BLUECYAN=%ld",blucyn);
newtooltypes[4]=(long) ttstr20;
StringF(ttstr21,(char*) "REDYELLOW=%ld",redyel);
newtooltypes[5]=(long) ttstr21;
StringF(ttstr22,(char*) "FLASHRED=%ld",flashred);
newtooltypes[6]=(long) ttstr22;
StringF(ttstr23,(char*) "FLASHGRN=%ld",flashgrn);
newtooltypes[7]=(long) ttstr23;
StringF(ttstr24,(char*) "FLASHBLU=%ld",flashblu);
newtooltypes[8]=(long) ttstr24;

/* greyscale, nobright not needed */
if( smooth==0 ) { newtooltypes[9]=(long) "NOSMOOTH";} else { newtooltypes[9]=(long) "(NOSMOOTH)";}
if( scale==0 ) { newtooltypes[10]=(long) "NOSCALE";} else { newtooltypes[10]=(long) "(NOSCALE)";}
if( os3 ) { newtooltypes[11]=(long) "ROMREMAP";} else { newtooltypes[11]=(long) "(ROMREMAP)";}
if( rompal==1 ) { newtooltypes[12]=(long) "ALTPALETTE";} else { newtooltypes[12]=(long) "(ALTPALETTE)";}
if( saveformat==0) {
    if( gifsave) {
        newtooltypes[13]=(long) "SAVEFORMAT=GIF";
    } else {
        newtooltypes[13]=(long) "SAVEFORMAT=SCR";
    }
}
if( saveformat==1 ) { newtooltypes[13]=(long) "SAVEFORMAT=ZX82";}
if( saveformat==2 ) { newtooltypes[13]=(long) "SAVEFORMAT=BYTES";}
if( saveformat==3 ) { newtooltypes[13]=(long) "SAVEFORMAT=TAP";}
if( saveformat==4 ) { newtooltypes[13]=(long) "SAVEFORMAT=TZX";}
if( saveformat==5 ) { newtooltypes[13]=(long) "SAVEFORMAT=PLUS3";}
if( mvscreen ) { newtooltypes[14]=(long) "SCREEN";} else { newtooltypes[14]=(long) "(SCREEN)";}
if( advopts ) { newtooltypes[15]=(long) "ADVANCED";} else { newtooltypes[15]=(long) "(ADVANCED)";}
StringF(ttstr25,(char*) "CLIPUNIT=%ld",clipunit);
newtooltypes[16]=(long) ttstr25;
if( askover ) { newtooltypes[17]=(long) "WARNOVERWRITE";} else { newtooltypes[17]=(long) "(WARNOVERWRITE)";}

StrCopy(ttstr11,(char*) "TZXBLOCKS=");
if( tzxarray[0] ) { StrAdd(ttstr11,(char*) "INFO|");}
if( tzxarray[1] ) { StrAdd(ttstr11,(char*) "STANDARD") ;} else { StrAdd(ttstr11,(char*) "CUSTOM");}
newtooltypes[18]=(long) ttstr11;

if( writeheader ) { newtooltypes[19]=(long) "(NOHEADER)";} else { newtooltypes[19]=(long) "NOHEADER";}

StringF(ttstr26,(char*) "BORDER=%ld",tzxborder);
newtooltypes[20]=(long) ttstr26;

if( nofilter ) { newtooltypes[21]=(long) "NOFILTER";} else { newtooltypes[21]=(long) "(NOFILTER)";}
if( noprogress ) { newtooltypes[22]=(long) "NOPROGRESSBAR";} else { newtooltypes[22]=(long) "(NOPROGRESSBAR)";}

StrCopy(ttstr13,(char*) "ODDCOLOURS=");
if( allelseink ) { StrAdd(ttstr13,(char*) "INK") ;} else { StrAdd(ttstr13,(char*) "PAPER");}
newtooltypes[23]=(long) ttstr13;

if( autonaming ) { newtooltypes[24]=(long) "(NOAUTONAMING)";} else { newtooltypes[24]=(long) "NOAUTONAMING";}

if( testmode ) { newtooltypes[26]=(long) "TESTMODE";} else { newtooltypes[26]=(long) "(TESTMODE)";}

if( saveattr ) { newtooltypes[25]=(long) "(NOATTRIBUTES)";} else { newtooltypes[25]=(long) "NOATTRIBUTES";}

if( dither ) { newtooltypes[27]=(long) "DITHER";} else { newtooltypes[27]=(long) "(DITHER)";}
if( simpleremap ) { newtooltypes[28]=(long) "SIMPLEMAP";} else { newtooltypes[28]=(long) "(SIMPLEMAP)";}

if( dtype==1 ) { newtooltypes[29]=(long) "DITHERTYPE=RANDOM";} else { newtooltypes[29]=(long) "DITHERTYPE=ORDERED";}

if( remapafterdither ) { newtooltypes[30]=(long) "REMAPAFTERDITHER";} else { newtooltypes[30]=(long) "(REMAPAFTERDITHER)";}

if( snapshotwindows) {
    StringF(ttstr27,(char*) "WINX=%ld",wptr->LeftEdge);
    StringF(ttstr28,(char*) "WINY=%ld",wptr->TopEdge);
newtooltypes[31]=(long) ttstr27;
newtooltypes[32]=(long) ttstr28;

    newtooltypes[33]=(long) NULL;
} else {
    newtooltypes[31]=(long) NULL;
}

dobj->do_ToolTypes=(char**) newtooltypes;

-PutDiskObject((char*) wbarg->wa_Name ,dobj );

dobj->do_ToolTypes=(char**) oldtooltypes;
FreeDiskObject(dobj );
}
if( olddir!=-1 ) { CurrentDir((BPTR) olddir );}
} catch(...) {}
	DisposeString(ttstr28 );
	DisposeString(ttstr27 );
	DisposeString(ttstr26 );
	DisposeString(ttstr25 );
	DisposeString(ttstr24 );
	DisposeString(ttstr23 );
	DisposeString(ttstr22 );
	DisposeString(ttstr21 );
	DisposeString(ttstr20 );
	DisposeString(ttstr19 );
	DisposeString(ttstr18 );
	DisposeString(ttstr17 );
	DisposeString(ttstr16 );
	DisposeString(ttstr15 );
	DisposeString(ttstr14 );
	DisposeString(ttstr13 );
	DisposeString(ttstr12 );
	DisposeString(ttstr11 );
	DisposeString(ttstr10 );
	DisposeString(ttstr9 );
	DisposeString(ttstr8 );
	DisposeString(ttstr7 );
	DisposeString(ttstr6 );
	DisposeString(ttstr5 );
	DisposeString(ttstr4 );
	DisposeString(ttstr3 );
	DisposeString(ttstr2 );
	DisposeString(ttstr1 );
	DisposeString(tmpstr2 );
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;

}

/* 1348 */

BOOLEAN aslhookfunc(long hook, struct FileRequester* fr, struct AnchorPath* frobj) {
struct FileInfoBlock* finfo=NULL; long gid; struct AChain* ac=NULL; char* name=NULL; long ppm2=NULL;
BOOLEAN temp_BOOL; long temp_QUAD;
try {
	name = NewString(1024);
	temp_QUAD = exception ;
	exception = 0 ;

finfo=& frobj->ap_Info;
ac=frobj->ap_Last;
// finfo:=ac.info

if( finfo->fib_DirEntryType>0 ) {

	temp_BOOL = -1;

	Raise(0);

}
//IF finfo.size=0 THEN RETURN -1

// NameFromLock(ac.lock,filebuf,1024)  -> **** hit **** // achain problem?
StrCopy(name,fr->fr_Drawer);
(BOOL)(0!=AddPart(name ,(char*) finfo->fib_FileName ,(ULONG) (long) 1024 ));

if( (lock=(long) Lock(name ,ACCESS_READ ))) {
     
    deetee=ObtainDataTypeA((ULONG) DTST_FILE ,(APTR) lock ,(struct TagItem*) NULL );
    gid=(long) deetee->dtn_Header->dth_GroupID;

     ReleaseDataType(deetee );

     (BOOL)(0!=Close((BPTR) lock ));
	lock=(long) NULL;

    if( gid==GID_PICTURE) {
        temp_BOOL = -1;
        Raise(0);
    } else { /* check if PPM */
        if( finfo->fib_Size==147471 ) {// Correct size for 256x192
            if( (fhid=(long) Open(name ,OLDFILE ))) {
                okay=Read((BPTR) fhid ,(APTR) fileid ,2 );
                (BOOL)(0!=Close((BPTR) fhid ));
                if( StrCmp((char*) "P6",(char*) fileid,2) ) { ppm2=TRUE;}
            }
            if( ppm2==TRUE ) {
            	temp_BOOL = -1 ;// ELSE RETURN -1
            	Raise(0);
            }
        }
    }
}
	temp_BOOL = 0;
} catch(...) {}
	DisposeString(name );
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return temp_BOOL ;
} 

void about() {
tempGlobal_ARRAY_OF_easystruct20 [0].es_StructSize = (ULONG) sizeof( struct EasyStruct);
EasyRequestArgs((struct Window*) 0 ,(struct EasyStruct*) tempGlobal_ARRAY_OF_easystruct20  ,(ULONG*) 0 ,(APTR) (long) NULL );
	return ;
}

void openprogresswin() {
long pwtop, pwleft;

if( noprogress==0) {

pwtbar=(long) mysc->Font->ta_YSize+(long) mysc->WBorTop+(long) 1;
pwtop=(long) wptr->TopEdge+(long) (wptr->Height/(short) 2)-((long) ((long) mysc->Font->ta_YSize*2)+pwtbar+(long) 1)/(long) 2;
pwleft=(long) wptr->LeftEdge+(long) (wptr->Width/(short) 2)-(long) 135;

tempGlobal_ARRAY_OF_tagitem4 [1].ti_Data = (STACKIPTR) pwtop;

tempGlobal_ARRAY_OF_tagitem4 [2].ti_Data = (STACKIPTR) pwleft;

tempGlobal_ARRAY_OF_tagitem4 [4].ti_Data = (STACKIPTR) (pwtbar+(long) 23);

pwptr=OpenWindowTagList((struct NewWindow*) NULL ,tempGlobal_ARRAY_OF_tagitem4  );

pwrp=pwptr->RPort;
curwin=pwptr;

            tempGlobal_ILIST132 [3]= vi;

            DrawBevelBoxA(pwrp ,6 ,pwtbar+(long) 2 ,258 ,(short) (long) ((long) mysc->Font->ta_YSize+4) ,(struct TagItem*) tempGlobal_ILIST132  );

SetAPen(pwrp ,(ULONG) 3 );
} else {
    if( wbmessage ) { curwin=wptr;}
}
	return ;
}

void setprogresswin(long percent) {
long barsize;

barsize=(long) ((float) percent*2.58)  ;// 258 -> ((percent/254.0)*100.0)

if( pwptr ) { RectFill(pwrp ,8 ,pwtbar+(long) 3 ,barsize ,pwtbar+(long) mysc->Font->ta_YSize+(long) 4 );}
	return ;

}

void closeprogresswin() {
if( pwptr) {
    CloseWindow(pwptr );
    pwptr=(struct Window*) NULL;
}

if( wbmessage ) { curwin=wptr;}
	return ;
}

void arexxecute() {
char* tmpstr2=NULL;
long temp_QUAD;
try {
	tmpstr2 = NewString(1024);
	temp_QUAD = exception ;
	exception = 0 ;

if( arexxreq) {

okay=-AslRequest((APTR) (long) arexxreq ,(struct TagItem*) NULL );

if( okay==FALSE ) { Raise(0);}

StrCopy(tmpstr2,arexxreq->fr_Drawer);

(BOOL)(0!=AddPart(tmpstr2 ,arexxreq->fr_File ,(ULONG) (long) 1024 ));

StrCopy(tmpstr,(char*) "c:run c:rx \"");
StrAdd(tmpstr,tmpstr2);
StrAdd(tmpstr,(char*) "\"");

WriteF((char*) "%s\n",(long) tmpstr);

(BOOL)(0!=Execute(tmpstr ,(BPTR) (long) NULL ,(BPTR) (long) NULL ));

}
} catch(...) {}
	DisposeString(tmpstr2 );
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;

}


void drawcross() {
                Move(rp ,229 ,(pixsep*(long) 14+(long) ((long) mysc->Font->ta_YSize*11)+tbar+(long) 1) );
                Draw(rp ,259 ,(pixsep*(long) 16+(long) ((long) mysc->Font->ta_YSize*14)+tbar+(long) 2) );
                Move(rp ,259 ,(pixsep*(long) 14+(long) ((long) mysc->Font->ta_YSize*11)+tbar+(long) 1) );
                Draw(rp ,229 ,(pixsep*(long) 16+(long) ((long) mysc->Font->ta_YSize*14)+tbar+(long) 2) );
	return ;

}

void drawflashcolour() {
	long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;

//        IF (flashred<>-1) AND (flashgrn<>-1) AND (flashblu<>-1)

            if( obtainbest==0) {
                if( flashcol==-1) {
                    SetAPen(rp ,(ULONG) 0 );
                    drawcross();
                    Raise(0);
                }
            } else {
                if( flashcol!=-1 ) { ReleasePen(cmap ,(ULONG) flashcol );}
                flashcol=-1;
            }


                if( - (flashred==-1)  | - (flashgrn==-1)  | - (flashblu==-1)) {
                    SetAPen(rp ,(ULONG) 0 );
                } else {
                    if( obtainbest) {
                        if( (flashcol=ObtainBestPenA(cmap ,(ULONG) (flashred << 24) ,(ULONG) (flashgrn << 24) ,(ULONG) (flashblu << 24) ,(struct TagItem*) tempGlobal_ILIST133  ))==-1 ) { SetAPen(rp ,(ULONG) 1 );}
                    } else {
                        SetColour(mysc,(UBYTE) flashcol,(UBYTE) flashred,(UBYTE) flashgrn,(UBYTE) flashblu);
                    }
                    SetAPen(rp ,(ULONG) flashcol );
                }
                 RectFill(rp ,229 ,(pixsep*(long) 14+(long) ((long) mysc->Font->ta_YSize*11)+tbar+(long) 1) ,259 ,(pixsep*(long) 16+(long) ((long) mysc->Font->ta_YSize*14)+tbar+(long) 2) );
                if( - (flashred==-1)  | - (flashgrn==-1)  | - (flashblu==-1)) {
                 SetAPen(rp ,(ULONG) 1 );
                     if( flashred==-1 ) { drawcross();}
                     if( flashgrn==-1 ) { drawcross();}
                     if( flashblu==-1 ) { drawcross();}
                }
} catch(...) {}
	if (exception!=0) {throw eException;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;




}
void new_dt2scr2_asl_aros()  {
	scrfile = NewString(1024);
	headerfile = NewString(1024);
	filetype = NewString(512);
	msg = NewString(1024);
	drw = NewString(1024);
	fname = NewString(1024);
	verstring = NewString(30);
	endmsg = NewString(100);
	wintit = NewString(50);
	filetypeerror = NewString(1024);
	filename = NewString(1024);
	tmpstr = NewString(300);
	loaddrw = NewString(512);
	objname = NewString(30);
	objauth = NewString(30);
	objver = NewString(30);
	objanno = NewString(30);
	objcopy = NewString(30);
	gifname = NewString(1024);
	tempGlobal_ILIST = NewList_elist(22);
	tempGlobal_ILIST [0]= 0;
	tempGlobal_ILIST [1]= 0;
	tempGlobal_ILIST [2]= 0;
	tempGlobal_ILIST [3]= 0;
	tempGlobal_ILIST [4]= 0;
	tempGlobal_ILIST [5]= 0;
	tempGlobal_ILIST [6]= 0;
	tempGlobal_ILIST [7]= 0;
	tempGlobal_ILIST [8]= 0;
	tempGlobal_ILIST [9]= 0;
	tempGlobal_ILIST [10]= 0;
	tempGlobal_ILIST [11]= 0;
	tempGlobal_ILIST [12]= 0;
	tempGlobal_ILIST [13]= 0;
	tempGlobal_ILIST [14]= 0;
	tempGlobal_ILIST [15]= 0;
	tempGlobal_ILIST [16]= 0;
	tempGlobal_ILIST [17]= 0;
	tempGlobal_ILIST [18]= 0;
	tempGlobal_ILIST [19]= 0;
	tempGlobal_ILIST [20]= 0;
	tempGlobal_ILIST [21]= 0;
	SetList(tempGlobal_ILIST ,22);
	tempGlobal_ILIST2 = NewList_elist(11);
	tempGlobal_ILIST2 [0]= ASLFR_TitleText;
	tempGlobal_ILIST2 [1]= (long) "Execute ARexx script...";
	tempGlobal_ILIST2 [2]= ASLFR_RejectIcons;
	tempGlobal_ILIST2 [3]= TRUE;
	tempGlobal_ILIST2 [4]= ASLFR_InitialDrawer;
	tempGlobal_ILIST2 [5]= (long) "REXX:";
	tempGlobal_ILIST2 [6]= ASLFR_DoPatterns;
	tempGlobal_ILIST2 [7]= TRUE;
	tempGlobal_ILIST2 [8]= ASLFR_InitialPattern;
	tempGlobal_ILIST2 [9]= (long) "#?.rexx";
	tempGlobal_ILIST2 [10]= (long) NULL;
	SetList(tempGlobal_ILIST2 ,11);
	tempGlobal_ARRAY_OF_easystruct = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ARRAY_OF_easystruct2 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct2 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct2 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct2 [0].es_TextFormat = (CONST_STRPTR) (long) "PDHFIC wants to write a\ntemporary file, but it\nalready exists!";
	tempGlobal_ARRAY_OF_easystruct2 [0].es_GadgetFormat = (CONST_STRPTR) (long) "Overwrite|Cancel";
	tempGlobal_ARRAY_OF_easystruct3 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct3 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct3 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct3 [0].es_TextFormat = (CONST_STRPTR) (long) "Warning!\n\nA .header for this\nfile already exists!";
	tempGlobal_ARRAY_OF_easystruct3 [0].es_GadgetFormat = (CONST_STRPTR) (long) "Overwrite|Cancel";
	tempGlobal_ARRAY_OF_easystruct4 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct4 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct4 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct4 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ARRAY_OF_easystruct5 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct5 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct5 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct5 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ILIST3 = NewList_elist(11);
	tempGlobal_ILIST3 [0]= DTA_SourceType;
	tempGlobal_ILIST3 [1]= DTST_FILE;
	tempGlobal_ILIST3 [2]= PDTA_Remap;
	tempGlobal_ILIST3 [3]= TRUE;
	tempGlobal_ILIST3 [4]= PDTA_DestMode;
	tempGlobal_ILIST3 [5]= PMODE_V43;
	tempGlobal_ILIST3 [6]= PDTA_ScaleQuality;
	tempGlobal_ILIST3 [7]= 1;
	tempGlobal_ILIST3 [8]= PDTA_Screen;
	tempGlobal_ILIST3 [10]= TAG_DONE;
	SetList(tempGlobal_ILIST3 ,11);
	tempGlobal_ARRAY_OF_easystruct6 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct6 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct6 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct6 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ILIST4 = NewList_elist(11);
	tempGlobal_ILIST4 [0]= DTA_SourceType;
	tempGlobal_ILIST4 [1]= DTST_CLIPBOARD;
	tempGlobal_ILIST4 [2]= PDTA_Remap;
	tempGlobal_ILIST4 [3]= TRUE;
	tempGlobal_ILIST4 [4]= PDTA_DestMode;
	tempGlobal_ILIST4 [5]= PMODE_V43;
	tempGlobal_ILIST4 [6]= PDTA_ScaleQuality;
	tempGlobal_ILIST4 [7]= 1;
	tempGlobal_ILIST4 [8]= DTA_GroupID;
	tempGlobal_ILIST4 [9]= GID_PICTURE;
	tempGlobal_ILIST4 [10]= TAG_DONE;
	SetList(tempGlobal_ILIST4 ,11);
	tempGlobal_ARRAY_OF_easystruct7 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct7 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct7 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct7 [0].es_TextFormat = (CONST_STRPTR) (long) "Clipboard data not ILBM";
	tempGlobal_ARRAY_OF_easystruct7 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ILIST5 = NewList_elist(5);
	tempGlobal_ILIST5 [0]= PDTA_BitMapHeader;
	tempGlobal_ILIST5 [1]= (long) bmh;
	tempGlobal_ILIST5 [2]= PDTA_NumColors;
	tempGlobal_ILIST5 [4]= TAG_DONE;
	SetList(tempGlobal_ILIST5 ,5);
	tempGlobal_ILIST6 = NewList_elist(3);
	tempGlobal_ILIST6 [0]= DTA_ObjName;
	tempGlobal_ILIST6 [1]= (long) objname2;
	tempGlobal_ILIST6 [2]= (long) NULL;
	SetList(tempGlobal_ILIST6 ,3);
	tempGlobal_ILIST7 = NewList_elist(3);
	tempGlobal_ILIST7 [0]= DTA_ObjAuthor;
	tempGlobal_ILIST7 [1]= (long) objauth2;
	tempGlobal_ILIST7 [2]= (long) NULL;
	SetList(tempGlobal_ILIST7 ,3);
	tempGlobal_ILIST8 = NewList_elist(3);
	tempGlobal_ILIST8 [0]= DTA_ObjCopyright;
	tempGlobal_ILIST8 [1]= (long) objcopy2;
	tempGlobal_ILIST8 [2]= (long) NULL;
	SetList(tempGlobal_ILIST8 ,3);
	tempGlobal_ILIST9 = NewList_elist(3);
	tempGlobal_ILIST9 [0]= DTA_ObjAnnotation;
	tempGlobal_ILIST9 [1]= (long) objanno2;
	tempGlobal_ILIST9 [2]= (long) NULL;
	SetList(tempGlobal_ILIST9 ,3);
	tempGlobal_ARRAY_OF_pdtscale = (struct pdtScale*) calloc(1 ,sizeof( struct pdtScale) );
	tempGlobal_ARRAY_OF_pdtscale [0].MethodID = (ULONG) (long) PDTM_SCALE;
	tempGlobal_ARRAY_OF_pdtscale [0].ps_NewWidth = (ULONG) (long) 256;
	tempGlobal_ARRAY_OF_pdtscale [0].ps_NewHeight = (ULONG) (long) 192;
	tempGlobal_ARRAY_OF_pdtscale [0].ps_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_pdtblitpixelarray = (struct pdtBlitPixelArray*) calloc(1 ,sizeof( struct pdtBlitPixelArray) );
	tempGlobal_ARRAY_OF_pdtblitpixelarray [0].MethodID = (ULONG) (long) PDTM_READPIXELARRAY;
	tempGlobal_ARRAY_OF_pdtblitpixelarray [0].pbpa_PixelData = (APTR) (long) ppmmem;
	tempGlobal_ARRAY_OF_pdtblitpixelarray [0].pbpa_PixelFormat = (ULONG) PBPAFMT_RGB;
	tempGlobal_ARRAY_OF_pdtblitpixelarray [0].pbpa_PixelArrayMod = (ULONG) (long) (256*(short) 3);
	tempGlobal_ARRAY_OF_pdtblitpixelarray [0].pbpa_Left = (ULONG) 0;
	tempGlobal_ARRAY_OF_pdtblitpixelarray [0].pbpa_Top = (ULONG) 0;
	tempGlobal_ARRAY_OF_pdtblitpixelarray [0].pbpa_Width = (ULONG) (long) 256;
	tempGlobal_ARRAY_OF_pdtblitpixelarray [0].pbpa_Height = (ULONG) (long) 192;
	tempGlobal_ARRAY_OF_easystruct8 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct8 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct8 [0].es_Title = (CONST_STRPTR) (long) "Welcome...";
	tempGlobal_ARRAY_OF_easystruct8 [0].es_TextFormat = (CONST_STRPTR) (long) "Hello.  I\'m Nigel Blenkinsopp,\nthe long lost explorer of the\nfar east.";
	tempGlobal_ARRAY_OF_easystruct8 [0].es_GadgetFormat = (CONST_STRPTR) (long) "Pleased to meet you";
	tempGlobal_ILIST10 = NewList_elist(1);
	tempGlobal_ILIST10 [0]= (long) NULL;
	SetList(tempGlobal_ILIST10 ,1);
	tempGlobal_ARRAY_OF_newgadget = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget [0].ng_LeftEdge = 70;
	tempGlobal_ARRAY_OF_newgadget [0].ng_Width = 128;
	tempGlobal_ARRAY_OF_newgadget [0].ng_GadgetText = (CONST_STRPTR) (long) "Input:";
	tempGlobal_ARRAY_OF_newgadget [0].ng_GadgetID = (UWORD) GADG_LOADSTR;
	tempGlobal_ARRAY_OF_newgadget [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ARRAY_OF_tagitem = (struct TagItem*) calloc(2 ,sizeof( struct TagItem) );
	tempGlobal_ARRAY_OF_tagitem [0].ti_Tag = (Tag) GTTX_Border;
	tempGlobal_ARRAY_OF_tagitem [0].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem [1].ti_Tag = (Tag) (long) NULL;
	tempGlobal_ARRAY_OF_tagitem [1].ti_Data = (STACKIPTR) 0;
	tempGlobal_ARRAY_OF_newgadget2 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget2 [0].ng_LeftEdge = 200;
	tempGlobal_ARRAY_OF_newgadget2 [0].ng_Width = 60;
	tempGlobal_ARRAY_OF_newgadget2 [0].ng_GadgetText = (CONST_STRPTR) (long) "Select";
	tempGlobal_ARRAY_OF_newgadget2 [0].ng_GadgetID = (UWORD) GADG_LOAD;
	tempGlobal_ARRAY_OF_newgadget2 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget2 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST11 = NewList_elist(1);
	tempGlobal_ILIST11 [0]= (long) NULL;
	SetList(tempGlobal_ILIST11 ,1);
	tempGlobal_ARRAY_OF_newgadget3 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget3 [0].ng_LeftEdge = 70;
	tempGlobal_ARRAY_OF_newgadget3 [0].ng_Width = 128;
	tempGlobal_ARRAY_OF_newgadget3 [0].ng_GadgetText = (CONST_STRPTR) (long) "Output:";
	tempGlobal_ARRAY_OF_newgadget3 [0].ng_GadgetID = (UWORD) GADG_SAVESTR;
	tempGlobal_ARRAY_OF_newgadget3 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget3 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST12 = NewList_elist(3);
	tempGlobal_ILIST12 [0]= GTTX_Border;
	tempGlobal_ILIST12 [1]= TRUE;
	tempGlobal_ILIST12 [2]= (long) NULL;
	SetList(tempGlobal_ILIST12 ,3);
	tempGlobal_ARRAY_OF_newgadget4 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget4 [0].ng_LeftEdge = 200;
	tempGlobal_ARRAY_OF_newgadget4 [0].ng_Width = 60;
	tempGlobal_ARRAY_OF_newgadget4 [0].ng_GadgetText = (CONST_STRPTR) (long) "Select";
	tempGlobal_ARRAY_OF_newgadget4 [0].ng_GadgetID = (UWORD) GADG_SAVE;
	tempGlobal_ARRAY_OF_newgadget4 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget4 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST13 = NewList_elist(1);
	tempGlobal_ILIST13 [0]= (long) NULL;
	SetList(tempGlobal_ILIST13 ,1);
	tempGlobal_ARRAY_OF_newgadget5 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget5 [0].ng_LeftEdge = 160;
	tempGlobal_ARRAY_OF_newgadget5 [0].ng_Width = 100;
	tempGlobal_ARRAY_OF_newgadget5 [0].ng_GadgetText = (CONST_STRPTR) (long) "Convert!";
	tempGlobal_ARRAY_OF_newgadget5 [0].ng_GadgetID = (UWORD) GADG_START;
	tempGlobal_ARRAY_OF_newgadget5 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget5 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST14 = NewList_elist(3);
	tempGlobal_ILIST14 [0]= GA_Disabled;
	tempGlobal_ILIST14 [1]= TRUE;
	tempGlobal_ILIST14 [2]= (long) NULL;
	SetList(tempGlobal_ILIST14 ,3);
	tempGlobal_ILIST15 = NewList_elist(8);
	tempGlobal_ILIST15 [0]= (long) "SCR";
	tempGlobal_ILIST15 [1]= (long) "ZX82";
	tempGlobal_ILIST15 [2]= (long) ".bytes";
	tempGlobal_ILIST15 [3]= (long) "TAP";
	tempGlobal_ILIST15 [4]= (long) "TZX";
	tempGlobal_ILIST15 [5]= (long) "+3DOS";
	tempGlobal_ILIST15 [6]= (long) "GIF";
	tempGlobal_ILIST15 [7]= (long) NULL;
	SetList(tempGlobal_ILIST15 ,8);
	tempGlobal_ILIST16 = NewList_elist(7);
	tempGlobal_ILIST16 [0]= (long) "SCR";
	tempGlobal_ILIST16 [1]= (long) "ZX82";
	tempGlobal_ILIST16 [2]= (long) ".bytes";
	tempGlobal_ILIST16 [3]= (long) "TAP";
	tempGlobal_ILIST16 [4]= (long) "TZX";
	tempGlobal_ILIST16 [5]= (long) "+3DOS";
	tempGlobal_ILIST16 [6]= (long) NULL;
	SetList(tempGlobal_ILIST16 ,7);
	tempGlobal_ARRAY_OF_newgadget6 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget6 [0].ng_LeftEdge = 70;
	tempGlobal_ARRAY_OF_newgadget6 [0].ng_Width = 75;
	tempGlobal_ARRAY_OF_newgadget6 [0].ng_GadgetText = (CONST_STRPTR) (long) "Save as";
	tempGlobal_ARRAY_OF_newgadget6 [0].ng_GadgetID = (UWORD) GADG_TYPE;
	tempGlobal_ARRAY_OF_newgadget6 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget6 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST17 = NewList_elist(5);
	tempGlobal_ILIST17 [0]= GTCY_Labels;
	tempGlobal_ILIST17 [2]= GTCY_Active;
	tempGlobal_ILIST17 [4]= (long) NULL;
	SetList(tempGlobal_ILIST17 ,5);
	tempGlobal_ARRAY_OF_newgadget7 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget7 [0].ng_LeftEdge = 95;
	tempGlobal_ARRAY_OF_newgadget7 [0].ng_Width = 27;
	tempGlobal_ARRAY_OF_newgadget7 [0].ng_GadgetText = (CONST_STRPTR) (long) "ROM Remap";
	tempGlobal_ARRAY_OF_newgadget7 [0].ng_GadgetID = (UWORD) GADG_ROMREMAP;
	tempGlobal_ARRAY_OF_newgadget7 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget7 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST18 = NewList_elist(5);
	tempGlobal_ILIST18 [0]= GTCB_Checked;
	tempGlobal_ILIST18 [2]= GTCB_Scaled;
	tempGlobal_ILIST18 [3]= TRUE;
	tempGlobal_ILIST18 [4]= (long) NULL;
	SetList(tempGlobal_ILIST18 ,5);
	tempGlobal_ARRAY_OF_newgadget8 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget8 [0].ng_LeftEdge = 230;
	tempGlobal_ARRAY_OF_newgadget8 [0].ng_Width = 27;
	tempGlobal_ARRAY_OF_newgadget8 [0].ng_GadgetText = (CONST_STRPTR) (long) "Scale Pic";
	tempGlobal_ARRAY_OF_newgadget8 [0].ng_GadgetID = (UWORD) GADG_SCALE;
	tempGlobal_ARRAY_OF_newgadget8 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget8 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST19 = NewList_elist(5);
	tempGlobal_ILIST19 [0]= GTCB_Checked;
	tempGlobal_ILIST19 [2]= GTCB_Scaled;
	tempGlobal_ILIST19 [3]= TRUE;
	tempGlobal_ILIST19 [4]= (long) NULL;
	SetList(tempGlobal_ILIST19 ,5);
	tempGlobal_ARRAY_OF_newgadget9 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget9 [0].ng_LeftEdge = 95;
	tempGlobal_ARRAY_OF_newgadget9 [0].ng_Width = 27;
	tempGlobal_ARRAY_OF_newgadget9 [0].ng_GadgetText = (CONST_STRPTR) (long) "Smooth";
	tempGlobal_ARRAY_OF_newgadget9 [0].ng_GadgetID = (UWORD) ADV_SMOOTH;
	tempGlobal_ARRAY_OF_newgadget9 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget9 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST20 = NewList_elist(5);
	tempGlobal_ILIST20 [0]= GTCB_Checked;
	tempGlobal_ILIST20 [2]= GTCB_Scaled;
	tempGlobal_ILIST20 [3]= TRUE;
	tempGlobal_ILIST20 [4]= (long) NULL;
	SetList(tempGlobal_ILIST20 ,5);
	tempGlobal_ARRAY_OF_newgadget10 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget10 [0].ng_LeftEdge = 230;
	tempGlobal_ARRAY_OF_newgadget10 [0].ng_Width = 27;
	tempGlobal_ARRAY_OF_newgadget10 [0].ng_GadgetText = (CONST_STRPTR) (long) "Dither";
	tempGlobal_ARRAY_OF_newgadget10 [0].ng_GadgetID = (UWORD) ADV_ZXDTPAL;
	tempGlobal_ARRAY_OF_newgadget10 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget10 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST21 = NewList_elist(5);
	tempGlobal_ILIST21 [0]= GTCB_Checked;
	tempGlobal_ILIST21 [2]= GTCB_Scaled;
	tempGlobal_ILIST21 [3]= TRUE;
	tempGlobal_ILIST21 [4]= (long) NULL;
	SetList(tempGlobal_ILIST21 ,5);
	tempGlobal_ARRAY_OF_newmenu = (struct NewMenu*) calloc(57 ,sizeof( struct NewMenu) );
	tempGlobal_ARRAY_OF_newmenu [0].nm_Type = (UBYTE) NM_TITLE;
	tempGlobal_ARRAY_OF_newmenu [0].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [0].nm_Label = (CONST_STRPTR) (long) "Project";
	tempGlobal_ARRAY_OF_newmenu [0].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [0].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [0].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [0].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [1].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [1].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [1].nm_Label = (CONST_STRPTR) (long) "About...";
	tempGlobal_ARRAY_OF_newmenu [1].nm_CommKey = (CONST_STRPTR) (long) "A";
	tempGlobal_ARRAY_OF_newmenu [1].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [1].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [1].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [2].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [2].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [2].nm_Label = (CONST_STRPTR) (long) NM_BARLABEL;
	tempGlobal_ARRAY_OF_newmenu [2].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [2].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [2].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [2].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [3].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [3].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [3].nm_Label = (CONST_STRPTR) (long) "Quit...";
	tempGlobal_ARRAY_OF_newmenu [3].nm_CommKey = (CONST_STRPTR) (long) "Q";
	tempGlobal_ARRAY_OF_newmenu [3].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [3].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [3].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [4].nm_Type = (UBYTE) NM_TITLE;
	tempGlobal_ARRAY_OF_newmenu [4].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [4].nm_Label = (CONST_STRPTR) (long) "Picture";
	tempGlobal_ARRAY_OF_newmenu [4].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [4].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [4].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [4].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [5].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [5].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [5].nm_Label = (CONST_STRPTR) (long) "Select Source...";
	tempGlobal_ARRAY_OF_newmenu [5].nm_CommKey = (CONST_STRPTR) (long) "S";
	tempGlobal_ARRAY_OF_newmenu [5].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [5].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [5].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [6].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [6].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [6].nm_Label = (CONST_STRPTR) (long) "From Clipboard";
	tempGlobal_ARRAY_OF_newmenu [6].nm_CommKey = (CONST_STRPTR) (long) "V";
	tempGlobal_ARRAY_OF_newmenu [6].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [6].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [6].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [7].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [7].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [7].nm_Label = (CONST_STRPTR) (long) "Display...";
	tempGlobal_ARRAY_OF_newmenu [7].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [7].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [7].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [7].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [8].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [8].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [8].nm_Label = (CONST_STRPTR) (long) NM_BARLABEL;
	tempGlobal_ARRAY_OF_newmenu [8].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [8].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [8].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [8].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [9].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [9].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [9].nm_Label = (CONST_STRPTR) (long) "Select Destination...";
	tempGlobal_ARRAY_OF_newmenu [9].nm_CommKey = (CONST_STRPTR) (long) "D";
	tempGlobal_ARRAY_OF_newmenu [9].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [9].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [9].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [10].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [10].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [10].nm_Label = (CONST_STRPTR) (long) "Display...";
	tempGlobal_ARRAY_OF_newmenu [10].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [10].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [10].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [10].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [11].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [11].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [11].nm_Label = (CONST_STRPTR) (long) NM_BARLABEL;
	tempGlobal_ARRAY_OF_newmenu [11].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [11].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [11].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [11].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [12].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [12].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [12].nm_Label = (CONST_STRPTR) (long) "Information...";
	tempGlobal_ARRAY_OF_newmenu [12].nm_CommKey = (CONST_STRPTR) (long) "I";
	tempGlobal_ARRAY_OF_newmenu [12].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [12].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [12].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [13].nm_Type = (UBYTE) NM_TITLE;
	tempGlobal_ARRAY_OF_newmenu [13].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [13].nm_Label = (CONST_STRPTR) (long) "Settings";
	tempGlobal_ARRAY_OF_newmenu [13].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [13].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [13].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [13].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [14].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [14].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [14].nm_Label = (CONST_STRPTR) (long) "Advanced Options";
	tempGlobal_ARRAY_OF_newmenu [14].nm_CommKey = (CONST_STRPTR) (long) "O";
	tempGlobal_ARRAY_OF_newmenu [14].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [14].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [15].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [15].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [15].nm_Label = (CONST_STRPTR) (long) "Use Screen";
	tempGlobal_ARRAY_OF_newmenu [15].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [15].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [15].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [16].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [16].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [16].nm_Label = (CONST_STRPTR) (long) "Warn Overwrite";
	tempGlobal_ARRAY_OF_newmenu [16].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [16].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [16].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [17].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [17].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [17].nm_Label = (CONST_STRPTR) (long) "Intelligent Filtering";
	tempGlobal_ARRAY_OF_newmenu [17].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [17].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [17].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [18].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [18].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [18].nm_Label = (CONST_STRPTR) (long) "Automatic Naming";
	tempGlobal_ARRAY_OF_newmenu [18].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [18].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [18].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [19].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [19].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [19].nm_Label = (CONST_STRPTR) (long) "Progress Window";
	tempGlobal_ARRAY_OF_newmenu [19].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [19].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [19].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [20].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [20].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [20].nm_Label = (CONST_STRPTR) (long) NM_BARLABEL;
	tempGlobal_ARRAY_OF_newmenu [20].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [20].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [20].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [20].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [21].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [21].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [21].nm_Label = (CONST_STRPTR) (long) "Map Extra Colours To";
	tempGlobal_ARRAY_OF_newmenu [21].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [21].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [21].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [21].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [22].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [22].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [22].nm_Label = (CONST_STRPTR) (long) "Auto";
	tempGlobal_ARRAY_OF_newmenu [22].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [22].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [22].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [23].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [23].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [23].nm_Label = (CONST_STRPTR) (long) "Ink";
	tempGlobal_ARRAY_OF_newmenu [23].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [23].nm_MutualExclude = 4;
	tempGlobal_ARRAY_OF_newmenu [23].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [24].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [24].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [24].nm_Label = (CONST_STRPTR) (long) "Paper";
	tempGlobal_ARRAY_OF_newmenu [24].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [24].nm_MutualExclude = 2;
	tempGlobal_ARRAY_OF_newmenu [24].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [25].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [25].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [25].nm_Label = (CONST_STRPTR) (long) "Attributes";
	tempGlobal_ARRAY_OF_newmenu [25].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [25].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [25].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [26].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [26].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [26].nm_Label = (CONST_STRPTR) (long) "ZXDT Palette";
	tempGlobal_ARRAY_OF_newmenu [26].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [26].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [26].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [27].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [27].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [27].nm_Label = (CONST_STRPTR) (long) "Dithering Method";
	tempGlobal_ARRAY_OF_newmenu [27].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [27].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [27].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [27].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [28].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [28].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [28].nm_Label = (CONST_STRPTR) (long) "Ordered";
	tempGlobal_ARRAY_OF_newmenu [28].nm_CommKey = (CONST_STRPTR) (long) "C";
	tempGlobal_ARRAY_OF_newmenu [28].nm_MutualExclude = 2;
	tempGlobal_ARRAY_OF_newmenu [28].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [29].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [29].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [29].nm_Label = (CONST_STRPTR) (long) "Random";
	tempGlobal_ARRAY_OF_newmenu [29].nm_CommKey = (CONST_STRPTR) (long) "R";
	tempGlobal_ARRAY_OF_newmenu [29].nm_MutualExclude = 1;
	tempGlobal_ARRAY_OF_newmenu [29].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [30].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [30].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [30].nm_Label = (CONST_STRPTR) (long) "Dither SimpleMap";
	tempGlobal_ARRAY_OF_newmenu [30].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [30].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [30].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [31].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [31].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [31].nm_Label = (CONST_STRPTR) (long) "Remap After Dither";
	tempGlobal_ARRAY_OF_newmenu [31].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [31].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [31].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [32].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [32].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [32].nm_Label = (CONST_STRPTR) (long) NM_BARLABEL;
	tempGlobal_ARRAY_OF_newmenu [32].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [32].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [32].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [32].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [33].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [33].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [33].nm_Label = (CONST_STRPTR) (long) "TZX Blocks";
	tempGlobal_ARRAY_OF_newmenu [33].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [33].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [33].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [33].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [34].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [34].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [34].nm_Label = (CONST_STRPTR) (long) "Archive Info";
	tempGlobal_ARRAY_OF_newmenu [34].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [34].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [34].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [35].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [35].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [35].nm_Label = (CONST_STRPTR) (long) NM_BARLABEL;
	tempGlobal_ARRAY_OF_newmenu [35].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [35].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [35].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [35].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [36].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [36].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [36].nm_Label = (CONST_STRPTR) (long) "Standard";
	tempGlobal_ARRAY_OF_newmenu [36].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [36].nm_MutualExclude = 8;
	tempGlobal_ARRAY_OF_newmenu [36].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [37].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [37].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [37].nm_Label = (CONST_STRPTR) (long) "Custom";
	tempGlobal_ARRAY_OF_newmenu [37].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [37].nm_MutualExclude = 4;
	tempGlobal_ARRAY_OF_newmenu [37].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [38].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [38].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [38].nm_Label = (CONST_STRPTR) (long) "TZX Custom Block Border";
	tempGlobal_ARRAY_OF_newmenu [38].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [38].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [38].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [38].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [39].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [39].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [39].nm_Label = (CONST_STRPTR) (long) "Black";
	tempGlobal_ARRAY_OF_newmenu [39].nm_CommKey = (CONST_STRPTR) (long) "0";
	tempGlobal_ARRAY_OF_newmenu [39].nm_MutualExclude = 254;
	tempGlobal_ARRAY_OF_newmenu [39].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [40].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [40].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [40].nm_Label = (CONST_STRPTR) (long) "Blue";
	tempGlobal_ARRAY_OF_newmenu [40].nm_CommKey = (CONST_STRPTR) (long) "1";
	tempGlobal_ARRAY_OF_newmenu [40].nm_MutualExclude = 253;
	tempGlobal_ARRAY_OF_newmenu [40].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [41].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [41].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [41].nm_Label = (CONST_STRPTR) (long) "Red";
	tempGlobal_ARRAY_OF_newmenu [41].nm_CommKey = (CONST_STRPTR) (long) "2";
	tempGlobal_ARRAY_OF_newmenu [41].nm_MutualExclude = 251;
	tempGlobal_ARRAY_OF_newmenu [41].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [42].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [42].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [42].nm_Label = (CONST_STRPTR) (long) "Cyan";
	tempGlobal_ARRAY_OF_newmenu [42].nm_CommKey = (CONST_STRPTR) (long) "3";
	tempGlobal_ARRAY_OF_newmenu [42].nm_MutualExclude = 247;
	tempGlobal_ARRAY_OF_newmenu [42].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [43].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [43].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [43].nm_Label = (CONST_STRPTR) (long) "Green";
	tempGlobal_ARRAY_OF_newmenu [43].nm_CommKey = (CONST_STRPTR) (long) "4";
	tempGlobal_ARRAY_OF_newmenu [43].nm_MutualExclude = 239;
	tempGlobal_ARRAY_OF_newmenu [43].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [44].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [44].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [44].nm_Label = (CONST_STRPTR) (long) "Magenta";
	tempGlobal_ARRAY_OF_newmenu [44].nm_CommKey = (CONST_STRPTR) (long) "5";
	tempGlobal_ARRAY_OF_newmenu [44].nm_MutualExclude = 223;
	tempGlobal_ARRAY_OF_newmenu [44].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [45].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [45].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [45].nm_Label = (CONST_STRPTR) (long) "Yellow";
	tempGlobal_ARRAY_OF_newmenu [45].nm_CommKey = (CONST_STRPTR) (long) "6";
	tempGlobal_ARRAY_OF_newmenu [45].nm_MutualExclude = 191;
	tempGlobal_ARRAY_OF_newmenu [45].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [46].nm_Type = (UBYTE) NM_SUB;
	tempGlobal_ARRAY_OF_newmenu [46].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [46].nm_Label = (CONST_STRPTR) (long) "White";
	tempGlobal_ARRAY_OF_newmenu [46].nm_CommKey = (CONST_STRPTR) (long) "7";
	tempGlobal_ARRAY_OF_newmenu [46].nm_MutualExclude = 127;
	tempGlobal_ARRAY_OF_newmenu [46].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [47].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [47].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [47].nm_Label = (CONST_STRPTR) (long) "Write .header";
	tempGlobal_ARRAY_OF_newmenu [47].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [47].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [47].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [48].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [48].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [48].nm_Label = (CONST_STRPTR) (long) NM_BARLABEL;
	tempGlobal_ARRAY_OF_newmenu [48].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [48].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [48].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [48].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [49].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [49].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [49].nm_Label = (CONST_STRPTR) (long) "Snapshot Window";
	tempGlobal_ARRAY_OF_newmenu [49].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [49].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [49].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [50].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [50].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [50].nm_Label = (CONST_STRPTR) (long) "Save Configuration";
	tempGlobal_ARRAY_OF_newmenu [50].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [50].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [50].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [50].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [51].nm_Type = (UBYTE) NM_TITLE;
	tempGlobal_ARRAY_OF_newmenu [51].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [51].nm_Label = (CONST_STRPTR) (long) "Help";
	tempGlobal_ARRAY_OF_newmenu [51].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [51].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [51].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [51].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [52].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [52].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [52].nm_Label = (CONST_STRPTR) (long) "Selected Output Format...";
	tempGlobal_ARRAY_OF_newmenu [52].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [52].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [52].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [52].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [53].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [53].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [53].nm_Label = (CONST_STRPTR) (long) "Relieving Boredom...";
	tempGlobal_ARRAY_OF_newmenu [53].nm_CommKey = (CONST_STRPTR) (long) "B";
	tempGlobal_ARRAY_OF_newmenu [53].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [53].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [54].nm_Type = (UBYTE) NM_TITLE;
	tempGlobal_ARRAY_OF_newmenu [54].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [54].nm_Label = (CONST_STRPTR) (long) "ARexx";
	tempGlobal_ARRAY_OF_newmenu [54].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [54].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [54].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [54].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [55].nm_Type = (UBYTE) NM_ITEM;
	tempGlobal_ARRAY_OF_newmenu [55].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [55].nm_Label = (CONST_STRPTR) (long) "Execute Script...";
	tempGlobal_ARRAY_OF_newmenu [55].nm_CommKey = (CONST_STRPTR) (long) "E";
	tempGlobal_ARRAY_OF_newmenu [55].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [55].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [55].nm_UserData = (APTR) 0;
	tempGlobal_ARRAY_OF_newmenu [56].nm_Type = (UBYTE) NM_END;
	tempGlobal_ARRAY_OF_newmenu [56].nm_Label  = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [56].nm_Label = (CONST_STRPTR) (long) NULL;
	tempGlobal_ARRAY_OF_newmenu [56].nm_CommKey = (CONST_STRPTR) 0;
	tempGlobal_ARRAY_OF_newmenu [56].nm_Flags = (UWORD) 0;
	tempGlobal_ARRAY_OF_newmenu [56].nm_MutualExclude = 0;
	tempGlobal_ARRAY_OF_newmenu [56].nm_UserData = (APTR) 0;
	tempGlobal_ILIST22 = NewList_elist(5);
	tempGlobal_ILIST22 [0]= GTMN_FullMenu;
	tempGlobal_ILIST22 [1]= TRUE;
	tempGlobal_ILIST22 [2]= GTMN_FrontPen;
	tempGlobal_ILIST22 [3]= 1;
	tempGlobal_ILIST22 [4]= (long) NULL;
	SetList(tempGlobal_ILIST22 ,5);
	tempGlobal_ARRAY_OF_tagitem2 = (struct TagItem*) calloc(16 ,sizeof( struct TagItem) );
	tempGlobal_ARRAY_OF_tagitem2 [0].ti_Tag = (Tag) WA_Title;
	tempGlobal_ARRAY_OF_tagitem2 [0].ti_Data = (STACKIPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_tagitem2 [1].ti_Tag = (Tag) WA_Top;
	tempGlobal_ARRAY_OF_tagitem2 [2].ti_Tag = (Tag) WA_Left;
	tempGlobal_ARRAY_OF_tagitem2 [3].ti_Tag = (Tag) WA_Gadgets;
	tempGlobal_ARRAY_OF_tagitem2 [4].ti_Tag = (Tag) WA_AutoAdjust;
	tempGlobal_ARRAY_OF_tagitem2 [4].ti_Data = (STACKIPTR) FALSE;
	tempGlobal_ARRAY_OF_tagitem2 [5].ti_Tag = (Tag) WA_Width;
	tempGlobal_ARRAY_OF_tagitem2 [5].ti_Data = (STACKIPTR) (long) 270;
	tempGlobal_ARRAY_OF_tagitem2 [6].ti_Tag = (Tag) WA_Height;
	tempGlobal_ARRAY_OF_tagitem2 [7].ti_Tag = (Tag) WA_DragBar;
	tempGlobal_ARRAY_OF_tagitem2 [7].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem2 [8].ti_Tag = (Tag) WA_DepthGadget;
	tempGlobal_ARRAY_OF_tagitem2 [8].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem2 [9].ti_Tag = (Tag) WA_Activate;
	tempGlobal_ARRAY_OF_tagitem2 [9].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem2 [10].ti_Tag = (Tag) WA_CloseGadget;
	tempGlobal_ARRAY_OF_tagitem2 [10].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem2 [11].ti_Tag = (Tag) WA_NewLookMenus;
	tempGlobal_ARRAY_OF_tagitem2 [11].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem2 [12].ti_Tag = (Tag) WA_ScreenTitle;
	tempGlobal_ARRAY_OF_tagitem2 [12].ti_Data = (STACKIPTR) (long) "PDHFIC 3.3  \251 1998-2001,2008-2009 Unsatisfactory Software";
	tempGlobal_ARRAY_OF_tagitem2 [13].ti_Tag = (Tag) WA_IDCMP;
	tempGlobal_ARRAY_OF_tagitem2 [13].ti_Data = (STACKIPTR) (long) (IDCMP_CLOSEWINDOW | (short) IDCMP_REFRESHWINDOW | (short) BUTTONIDCMP | IDCMP_MENUPICK);
	tempGlobal_ARRAY_OF_tagitem2 [14].ti_Tag = (Tag) WA_Zoom;
	tempGlobal_ARRAY_OF_tagitem2 [15].ti_Tag = (Tag) (long) NULL;
	tempGlobal_ARRAY_OF_tagitem2 [15].ti_Data = (STACKIPTR) 0;
	tempGlobal_ARRAY_OF_tagitem3 = (struct TagItem*) calloc(14 ,sizeof( struct TagItem) );
	tempGlobal_ARRAY_OF_tagitem3 [0].ti_Tag = (Tag) WA_Title;
	tempGlobal_ARRAY_OF_tagitem3 [0].ti_Data = (STACKIPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_tagitem3 [1].ti_Tag = (Tag) WA_Gadgets;
	tempGlobal_ARRAY_OF_tagitem3 [2].ti_Tag = (Tag) WA_AutoAdjust;
	tempGlobal_ARRAY_OF_tagitem3 [2].ti_Data = (STACKIPTR) FALSE;
	tempGlobal_ARRAY_OF_tagitem3 [3].ti_Tag = (Tag) WA_Width;
	tempGlobal_ARRAY_OF_tagitem3 [3].ti_Data = (STACKIPTR) (long) 270;
	tempGlobal_ARRAY_OF_tagitem3 [4].ti_Tag = (Tag) WA_Height;
	tempGlobal_ARRAY_OF_tagitem3 [5].ti_Tag = (Tag) WA_DragBar;
	tempGlobal_ARRAY_OF_tagitem3 [5].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem3 [6].ti_Tag = (Tag) WA_DepthGadget;
	tempGlobal_ARRAY_OF_tagitem3 [6].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem3 [7].ti_Tag = (Tag) WA_Activate;
	tempGlobal_ARRAY_OF_tagitem3 [7].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem3 [8].ti_Tag = (Tag) WA_CloseGadget;
	tempGlobal_ARRAY_OF_tagitem3 [8].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem3 [9].ti_Tag = (Tag) WA_NewLookMenus;
	tempGlobal_ARRAY_OF_tagitem3 [9].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem3 [10].ti_Tag = (Tag) WA_ScreenTitle;
	tempGlobal_ARRAY_OF_tagitem3 [10].ti_Data = (STACKIPTR) (long) "PDHFIC 3.3  \251 1998-2001,2008-2009 Unsatisfactory Software";
	tempGlobal_ARRAY_OF_tagitem3 [11].ti_Tag = (Tag) WA_IDCMP;
	tempGlobal_ARRAY_OF_tagitem3 [11].ti_Data = (STACKIPTR) (long) (IDCMP_CLOSEWINDOW | (short) IDCMP_REFRESHWINDOW | (short) BUTTONIDCMP | IDCMP_MENUPICK);
	tempGlobal_ARRAY_OF_tagitem3 [12].ti_Tag = (Tag) WA_Zoom;
	tempGlobal_ARRAY_OF_tagitem3 [13].ti_Tag = (Tag) (long) NULL;
	tempGlobal_ARRAY_OF_tagitem3 [13].ti_Data = (STACKIPTR) 0;
	tempGlobal_ILIST23 = NewList_elist(1);
	tempGlobal_ILIST23 [0]= (long) NULL;
	SetList(tempGlobal_ILIST23 ,1);
	tempGlobal_ILIST24 = NewList_elist(13);
	tempGlobal_ILIST24 [0]= ASLFR_TitleText;
	tempGlobal_ILIST24 [1]= (long) "Select image to convert...";
	tempGlobal_ILIST24 [2]= ASLFR_RejectIcons;
	tempGlobal_ILIST24 [3]= TRUE;
	tempGlobal_ILIST24 [4]= ASLFR_InitialDrawer;
	tempGlobal_ILIST24 [6]= ASLFR_InitialFile;
	tempGlobal_ILIST24 [8]= ASLFR_InitialWidth;
	tempGlobal_ILIST24 [10]= ASLFR_InitialHeight;
	tempGlobal_ILIST24 [12]= (long) NULL;
	SetList(tempGlobal_ILIST24 ,13);
	tempGlobal_ILIST25 = NewList_elist(9);
	tempGlobal_ILIST25 [0]= ASLFR_TitleText;
	tempGlobal_ILIST25 [1]= (long) "Select image to convert...";
	tempGlobal_ILIST25 [2]= ASLFR_RejectIcons;
	tempGlobal_ILIST25 [3]= TRUE;
	tempGlobal_ILIST25 [4]= ASLFR_InitialDrawer;
	tempGlobal_ILIST25 [6]= ASLFR_InitialFile;
	tempGlobal_ILIST25 [8]= (long) NULL;
	SetList(tempGlobal_ILIST25 ,9);
	tempGlobal_ILIST26 = NewList_elist(17);
	tempGlobal_ILIST26 [0]= ASLFR_TitleText;
	tempGlobal_ILIST26 [1]= (long) "Select image to convert...";
	tempGlobal_ILIST26 [2]= ASLFR_RejectIcons;
	tempGlobal_ILIST26 [3]= TRUE;
	tempGlobal_ILIST26 [4]= ASLFR_InitialDrawer;
	tempGlobal_ILIST26 [6]= ASLFR_InitialFile;
	tempGlobal_ILIST26 [8]= ASLFR_InitialWidth;
	tempGlobal_ILIST26 [10]= ASLFR_InitialHeight;
	tempGlobal_ILIST26 [12]= ASLFR_Flags1;
	tempGlobal_ILIST26 [13]= FRF_FILTERFUNC;
	tempGlobal_ILIST26 [14]= ASLFR_FilterFunc;
	tempGlobal_ILIST26 [16]= (long) NULL;
	SetList(tempGlobal_ILIST26 ,17);
	tempGlobal_ILIST27 = NewList_elist(13);
	tempGlobal_ILIST27 [0]= ASLFR_TitleText;
	tempGlobal_ILIST27 [1]= (long) "Select image to convert...";
	tempGlobal_ILIST27 [2]= ASLFR_RejectIcons;
	tempGlobal_ILIST27 [3]= TRUE;
	tempGlobal_ILIST27 [4]= ASLFR_InitialDrawer;
	tempGlobal_ILIST27 [6]= ASLFR_InitialFile;
	tempGlobal_ILIST27 [8]= ASLFR_Flags1;
	tempGlobal_ILIST27 [9]= FRF_FILTERFUNC;
	tempGlobal_ILIST27 [10]= ASLFR_FilterFunc;
	tempGlobal_ILIST27 [12]= (long) NULL;
	SetList(tempGlobal_ILIST27 ,13);
	tempGlobal_ARRAY_OF_easystruct9 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct9 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct9 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct9 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ILIST28 = NewList_elist(3);
	tempGlobal_ILIST28 [0]= GTTX_Text;
	tempGlobal_ILIST28 [2]= (long) NULL;
	SetList(tempGlobal_ILIST28 ,3);
	tempGlobal_ILIST29 = NewList_elist(15);
	tempGlobal_ILIST29 [0]= ASLFR_TitleText;
	tempGlobal_ILIST29 [1]= (long) "Enter a filename to save Spectrum screen as...";
	tempGlobal_ILIST29 [2]= ASLFR_RejectIcons;
	tempGlobal_ILIST29 [3]= TRUE;
	tempGlobal_ILIST29 [4]= ASLFR_InitialDrawer;
	tempGlobal_ILIST29 [6]= ASLFR_InitialFile;
	tempGlobal_ILIST29 [8]= ASLFR_DoSaveMode;
	tempGlobal_ILIST29 [9]= TRUE;
	tempGlobal_ILIST29 [10]= ASLFR_InitialWidth;
	tempGlobal_ILIST29 [12]= ASLFR_InitialHeight;
	tempGlobal_ILIST29 [14]= (long) NULL;
	SetList(tempGlobal_ILIST29 ,15);
	tempGlobal_ILIST30 = NewList_elist(11);
	tempGlobal_ILIST30 [0]= ASLFR_TitleText;
	tempGlobal_ILIST30 [1]= (long) "Enter a filename to save Spectrum screen as...";
	tempGlobal_ILIST30 [2]= ASLFR_RejectIcons;
	tempGlobal_ILIST30 [3]= TRUE;
	tempGlobal_ILIST30 [4]= ASLFR_InitialDrawer;
	tempGlobal_ILIST30 [6]= ASLFR_InitialFile;
	tempGlobal_ILIST30 [8]= ASLFR_DoSaveMode;
	tempGlobal_ILIST30 [9]= TRUE;
	tempGlobal_ILIST30 [10]= (long) NULL;
	SetList(tempGlobal_ILIST30 ,11);
	tempGlobal_ARRAY_OF_easystruct10 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct10 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct10 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct10 [0].es_TextFormat = (CONST_STRPTR) (long) "ARexx port could\nnot be allocated";
	tempGlobal_ARRAY_OF_easystruct10 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ILIST31 = NewList_elist(3);
	tempGlobal_ILIST31 [0]= OBP_Precision;
	tempGlobal_ILIST31 [1]= PRECISION_GUI;
	tempGlobal_ILIST31 [2]= (long) NULL;
	SetList(tempGlobal_ILIST31 ,3);
	tempGlobal_ILIST32 = NewList_elist(5);
	tempGlobal_ILIST32 [0]= WA_BusyPointer;
	tempGlobal_ILIST32 [1]= TRUE;
	tempGlobal_ILIST32 [2]= WA_PointerDelay;
	tempGlobal_ILIST32 [3]= TRUE;
	tempGlobal_ILIST32 [4]= (long) NULL;
	SetList(tempGlobal_ILIST32 ,5);
	tempGlobal_ILIST33 = NewList_elist(3);
	tempGlobal_ILIST33 [0]= GTTX_Text;
	tempGlobal_ILIST33 [2]= (long) NULL;
	SetList(tempGlobal_ILIST33 ,3);
	tempGlobal_ILIST34 = NewList_elist(3);
	tempGlobal_ILIST34 [0]= GA_Disabled;
	tempGlobal_ILIST34 [1]= FALSE;
	tempGlobal_ILIST34 [2]= (long) NULL;
	SetList(tempGlobal_ILIST34 ,3);
	tempGlobal_ILIST35 = NewList_elist(3);
	tempGlobal_ILIST35 [0]= GA_Disabled;
	tempGlobal_ILIST35 [1]= TRUE;
	tempGlobal_ILIST35 [2]= (long) NULL;
	SetList(tempGlobal_ILIST35 ,3);
	tempGlobal_ILIST36 = NewList_elist(1);
	tempGlobal_ILIST36 [0]= (long) NULL;
	SetList(tempGlobal_ILIST36 ,1);
	tempGlobal_ILIST37 = NewList_elist(5);
	tempGlobal_ILIST37 [0]= WA_BusyPointer;
	tempGlobal_ILIST37 [1]= TRUE;
	tempGlobal_ILIST37 [2]= WA_PointerDelay;
	tempGlobal_ILIST37 [3]= TRUE;
	tempGlobal_ILIST37 [4]= (long) NULL;
	SetList(tempGlobal_ILIST37 ,5);
	tempGlobal_ILIST38 = NewList_elist(3);
	tempGlobal_ILIST38 [0]= GTTX_Text;
	tempGlobal_ILIST38 [2]= (long) NULL;
	SetList(tempGlobal_ILIST38 ,3);
	tempGlobal_ILIST39 = NewList_elist(3);
	tempGlobal_ILIST39 [0]= GA_Disabled;
	tempGlobal_ILIST39 [1]= FALSE;
	tempGlobal_ILIST39 [2]= (long) NULL;
	SetList(tempGlobal_ILIST39 ,3);
	tempGlobal_ILIST40 = NewList_elist(3);
	tempGlobal_ILIST40 [0]= GA_Disabled;
	tempGlobal_ILIST40 [1]= TRUE;
	tempGlobal_ILIST40 [2]= (long) NULL;
	SetList(tempGlobal_ILIST40 ,3);
	tempGlobal_ILIST41 = NewList_elist(1);
	tempGlobal_ILIST41 [0]= (long) NULL;
	SetList(tempGlobal_ILIST41 ,1);
	tempGlobal_ARRAY_OF_easystruct11 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct11 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct11 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct11 [0].es_TextFormat = (CONST_STRPTR) (long) "Warning!\n\nThat file already exists,\nand will be overwritten!";
	tempGlobal_ARRAY_OF_easystruct11 [0].es_GadgetFormat = (CONST_STRPTR) (long) "Overwrite|Cancel";
	tempGlobal_ILIST42 = NewList_elist(5);
	tempGlobal_ILIST42 [0]= WA_BusyPointer;
	tempGlobal_ILIST42 [1]= TRUE;
	tempGlobal_ILIST42 [2]= WA_PointerDelay;
	tempGlobal_ILIST42 [3]= TRUE;
	tempGlobal_ILIST42 [4]= (long) NULL;
	SetList(tempGlobal_ILIST42 ,5);
	tempGlobal_ILIST43 = NewList_elist(1);
	tempGlobal_ILIST43 [0]= (long) NULL;
	SetList(tempGlobal_ILIST43 ,1);
	tempGlobal_ILIST44 = NewList_elist(3);
	tempGlobal_ILIST44 [0]= GA_Disabled;
	tempGlobal_ILIST44 [2]= (long) NULL;
	SetList(tempGlobal_ILIST44 ,3);
	tempGlobal_ILIST45 = NewList_elist(3);
	tempGlobal_ILIST45 [0]= GA_Disabled;
	tempGlobal_ILIST45 [2]= (long) NULL;
	SetList(tempGlobal_ILIST45 ,3);
	tempGlobal_ILIST46 = NewList_elist(3);
	tempGlobal_ILIST46 [0]= GA_Disabled;
	tempGlobal_ILIST46 [2]= (long) NULL;
	SetList(tempGlobal_ILIST46 ,3);
	tempGlobal_ILIST47 = NewList_elist(3);
	tempGlobal_ILIST47 [0]= GA_Disabled;
	tempGlobal_ILIST47 [2]= (long) NULL;
	SetList(tempGlobal_ILIST47 ,3);
	tempGlobal_ILIST48 = NewList_elist(3);
	tempGlobal_ILIST48 [0]= GA_Disabled;
	tempGlobal_ILIST48 [2]= (long) NULL;
	SetList(tempGlobal_ILIST48 ,3);
	tempGlobal_ILIST49 = NewList_elist(3);
	tempGlobal_ILIST49 [0]= GA_Disabled;
	tempGlobal_ILIST49 [2]= (long) NULL;
	SetList(tempGlobal_ILIST49 ,3);
	tempGlobal_ILIST50 = NewList_elist(3);
	tempGlobal_ILIST50 [0]= GTNM_Number;
	tempGlobal_ILIST50 [2]= (long) NULL;
	SetList(tempGlobal_ILIST50 ,3);
	tempGlobal_ILIST51 = NewList_elist(3);
	tempGlobal_ILIST51 [0]= GTNM_Number;
	tempGlobal_ILIST51 [2]= (long) NULL;
	SetList(tempGlobal_ILIST51 ,3);
	tempGlobal_ILIST52 = NewList_elist(3);
	tempGlobal_ILIST52 [0]= GTNM_Number;
	tempGlobal_ILIST52 [2]= (long) NULL;
	SetList(tempGlobal_ILIST52 ,3);
	tempGlobal_ILIST53 = NewList_elist(3);
	tempGlobal_ILIST53 [0]= GTNM_Number;
	tempGlobal_ILIST53 [2]= (long) NULL;
	SetList(tempGlobal_ILIST53 ,3);
	tempGlobal_ILIST54 = NewList_elist(3);
	tempGlobal_ILIST54 [0]= GTNM_Number;
	tempGlobal_ILIST54 [2]= (long) NULL;
	SetList(tempGlobal_ILIST54 ,3);
	tempGlobal_ILIST55 = NewList_elist(3);
	tempGlobal_ILIST55 [0]= GTNM_Number;
	tempGlobal_ILIST55 [2]= (long) NULL;
	SetList(tempGlobal_ILIST55 ,3);
	tempGlobal_ILIST56 = NewList_elist(3);
	tempGlobal_ILIST56 [0]= GTNM_Number;
	tempGlobal_ILIST56 [2]= (long) NULL;
	SetList(tempGlobal_ILIST56 ,3);
	tempGlobal_ILIST57 = NewList_elist(3);
	tempGlobal_ILIST57 [0]= GTNM_Number;
	tempGlobal_ILIST57 [2]= (long) NULL;
	SetList(tempGlobal_ILIST57 ,3);
	tempGlobal_ILIST58 = NewList_elist(3);
	tempGlobal_ILIST58 [0]= GTNM_Number;
	tempGlobal_ILIST58 [2]= (long) NULL;
	SetList(tempGlobal_ILIST58 ,3);
	tempGlobal_ILIST59 = NewList_elist(7);
	tempGlobal_ILIST59 [0]= GTBB_Recessed;
	tempGlobal_ILIST59 [1]= TRUE;
	tempGlobal_ILIST59 [2]= GTBB_FrameType;
	tempGlobal_ILIST59 [3]= BBFT_RIDGE;
	tempGlobal_ILIST59 [4]= GT_VisualInfo;
	tempGlobal_ILIST59 [6]= (long) NULL;
	SetList(tempGlobal_ILIST59 ,7);
	tempGlobal_ILIST60 = NewList_elist(5);
	tempGlobal_ILIST60 [0]= GTBB_Recessed;
	tempGlobal_ILIST60 [1]= TRUE;
	tempGlobal_ILIST60 [2]= GT_VisualInfo;
	tempGlobal_ILIST60 [4]= (long) NULL;
	SetList(tempGlobal_ILIST60 ,5);
	tempGlobal_ILIST61 = NewList_elist(5);
	tempGlobal_ILIST61 [0]= WA_BusyPointer;
	tempGlobal_ILIST61 [1]= TRUE;
	tempGlobal_ILIST61 [2]= WA_PointerDelay;
	tempGlobal_ILIST61 [3]= TRUE;
	tempGlobal_ILIST61 [4]= (long) NULL;
	SetList(tempGlobal_ILIST61 ,5);
	tempGlobal_ILIST62 = NewList_elist(3);
	tempGlobal_ILIST62 [0]= GTTX_Text;
	tempGlobal_ILIST62 [2]= (long) NULL;
	SetList(tempGlobal_ILIST62 ,3);
	tempGlobal_ILIST63 = NewList_elist(3);
	tempGlobal_ILIST63 [0]= GA_Disabled;
	tempGlobal_ILIST63 [1]= FALSE;
	tempGlobal_ILIST63 [2]= (long) NULL;
	SetList(tempGlobal_ILIST63 ,3);
	tempGlobal_ILIST64 = NewList_elist(3);
	tempGlobal_ILIST64 [0]= GA_Disabled;
	tempGlobal_ILIST64 [1]= TRUE;
	tempGlobal_ILIST64 [2]= (long) NULL;
	SetList(tempGlobal_ILIST64 ,3);
	tempGlobal_ILIST65 = NewList_elist(3);
	tempGlobal_ILIST65 [0]= GTTX_Text;
	tempGlobal_ILIST65 [1]= (long) "CLIPBOARD";
	tempGlobal_ILIST65 [2]= (long) NULL;
	SetList(tempGlobal_ILIST65 ,3);
	tempGlobal_ILIST66 = NewList_elist(3);
	tempGlobal_ILIST66 [0]= GTTX_Text;
	tempGlobal_ILIST66 [2]= (long) NULL;
	SetList(tempGlobal_ILIST66 ,3);
	tempGlobal_ILIST67 = NewList_elist(3);
	tempGlobal_ILIST67 [0]= GA_Disabled;
	tempGlobal_ILIST67 [1]= FALSE;
	tempGlobal_ILIST67 [2]= (long) NULL;
	SetList(tempGlobal_ILIST67 ,3);
	tempGlobal_ILIST68 = NewList_elist(3);
	tempGlobal_ILIST68 [0]= GA_Disabled;
	tempGlobal_ILIST68 [1]= TRUE;
	tempGlobal_ILIST68 [2]= (long) NULL;
	SetList(tempGlobal_ILIST68 ,3);
	tempGlobal_ARRAY_OF_easystruct12 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct12 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct12 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct12 [0].es_TextFormat = (CONST_STRPTR) (long) "GIF\n\nThis saver is a bit special: it\nuses SCR2GIF to output a Spectrum\ntype image, but suitable for\nweb pages.\n\nSize: <6912\n\nUsed by: WWW and a decreasing\namount of other programs.";
	tempGlobal_ARRAY_OF_easystruct12 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ARRAY_OF_easystruct13 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct13 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct13 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct13 [0].es_TextFormat = (CONST_STRPTR) (long) "SCR\n\nThis is simply the raw data taken\nfrom the Spectrum\'s memory at\naddress 16384.\n\nSize: 6912\n\nUsed by: ZX Datatype, ZXAM";
	tempGlobal_ARRAY_OF_easystruct13 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ARRAY_OF_easystruct14 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct14 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct14 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct14 [0].es_TextFormat = (CONST_STRPTR) (long) "ZX82\n\nThis format is really a disk-based\nformat, as it does not contain\nan original Spectrum tape header.\n\nSize: 6924\n\nUsed by: Speculator, xfs, ZXMIT\n\nMore information available from\nSpeculator documentation";
	tempGlobal_ARRAY_OF_easystruct14 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ARRAY_OF_easystruct15 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct15 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct15 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct15 [0].es_TextFormat = (CONST_STRPTR) (long) "BYTES (.header/.bytes)\n\nThese are nothing other than\nindividual blocks of data as a\nSpectrum would save to tape.\n\nSize: 19 (.header), 6914 (.bytes)\n\nUsed by: Peter McGavin\'s Spectrum, ZXAM";
	tempGlobal_ARRAY_OF_easystruct15 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ARRAY_OF_easystruct16 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct16 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct16 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct16 [0].es_TextFormat = (CONST_STRPTR) (long) "TAP (Z80 Tape Image)\n\nTAPs are tape files which can store\nseveral data blocks in one file.\n\nSize: 6937\n\nUsed by: ZXAM, ASp";
	tempGlobal_ARRAY_OF_easystruct16 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ARRAY_OF_easystruct17 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct17 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct17 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct17 [0].es_TextFormat = (CONST_STRPTR) (long) "+3DOS\n\nThis is a +3DOS file, intended for\nuse on +3 disks\n\nSize: 7040\n\nUsed by: +3";
	tempGlobal_ARRAY_OF_easystruct17 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ARRAY_OF_easystruct18 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct18 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct18 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct18 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ILIST69 = NewList_elist(1);
	tempGlobal_ILIST69 [0]= (long) NULL;
	SetList(tempGlobal_ILIST69 ,1);
	tempGlobal_ILIST70 = NewList_elist(5);
	tempGlobal_ILIST70 [0]= WA_BusyPointer;
	tempGlobal_ILIST70 [1]= TRUE;
	tempGlobal_ILIST70 [2]= WA_PointerDelay;
	tempGlobal_ILIST70 [3]= TRUE;
	tempGlobal_ILIST70 [4]= (long) NULL;
	SetList(tempGlobal_ILIST70 ,5);
	tempGlobal_ILIST71 = NewList_elist(3);
	tempGlobal_ILIST71 [0]= GTTX_Text;
	tempGlobal_ILIST71 [2]= (long) NULL;
	SetList(tempGlobal_ILIST71 ,3);
	tempGlobal_ILIST72 = NewList_elist(3);
	tempGlobal_ILIST72 [0]= GA_Disabled;
	tempGlobal_ILIST72 [1]= FALSE;
	tempGlobal_ILIST72 [2]= (long) NULL;
	SetList(tempGlobal_ILIST72 ,3);
	tempGlobal_ILIST73 = NewList_elist(3);
	tempGlobal_ILIST73 [0]= GA_Disabled;
	tempGlobal_ILIST73 [1]= TRUE;
	tempGlobal_ILIST73 [2]= (long) NULL;
	SetList(tempGlobal_ILIST73 ,3);
	tempGlobal_ILIST74 = NewList_elist(1);
	tempGlobal_ILIST74 [0]= (long) NULL;
	SetList(tempGlobal_ILIST74 ,1);
	tempGlobal_ILIST75 = NewList_elist(5);
	tempGlobal_ILIST75 [0]= WA_BusyPointer;
	tempGlobal_ILIST75 [1]= TRUE;
	tempGlobal_ILIST75 [2]= WA_PointerDelay;
	tempGlobal_ILIST75 [3]= TRUE;
	tempGlobal_ILIST75 [4]= (long) NULL;
	SetList(tempGlobal_ILIST75 ,5);
	tempGlobal_ARRAY_OF_easystruct19 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct19 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct19 [0].es_Title = (CONST_STRPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_easystruct19 [0].es_TextFormat = (CONST_STRPTR) (long) "Hello!";
	tempGlobal_ARRAY_OF_easystruct19 [0].es_GadgetFormat = (CONST_STRPTR) (long) "OK";
	tempGlobal_ILIST76 = NewList_elist(3);
	tempGlobal_ILIST76 [0]= GTTX_Text;
	tempGlobal_ILIST76 [2]= (long) NULL;
	SetList(tempGlobal_ILIST76 ,3);
	tempGlobal_ILIST77 = NewList_elist(3);
	tempGlobal_ILIST77 [0]= GTTX_Text;
	tempGlobal_ILIST77 [2]= (long) NULL;
	SetList(tempGlobal_ILIST77 ,3);
	tempGlobal_ILIST78 = NewList_elist(3);
	tempGlobal_ILIST78 [0]= GA_Disabled;
	tempGlobal_ILIST78 [1]= FALSE;
	tempGlobal_ILIST78 [2]= (long) NULL;
	SetList(tempGlobal_ILIST78 ,3);
	tempGlobal_ILIST79 = NewList_elist(3);
	tempGlobal_ILIST79 [0]= GA_Disabled;
	tempGlobal_ILIST79 [1]= TRUE;
	tempGlobal_ILIST79 [2]= (long) NULL;
	SetList(tempGlobal_ILIST79 ,3);
	tempGlobal_ILIST80 = NewList_elist(3);
	tempGlobal_ILIST80 [0]= GTTX_Text;
	tempGlobal_ILIST80 [2]= (long) NULL;
	SetList(tempGlobal_ILIST80 ,3);
	tempGlobal_ILIST81 = NewList_elist(3);
	tempGlobal_ILIST81 [0]= GA_Disabled;
	tempGlobal_ILIST81 [1]= FALSE;
	tempGlobal_ILIST81 [2]= (long) NULL;
	SetList(tempGlobal_ILIST81 ,3);
	tempGlobal_ILIST82 = NewList_elist(3);
	tempGlobal_ILIST82 [0]= GA_Disabled;
	tempGlobal_ILIST82 [1]= TRUE;
	tempGlobal_ILIST82 [2]= (long) NULL;
	SetList(tempGlobal_ILIST82 ,3);
	tempGlobal_ILIST83 = NewList_elist(3);
	tempGlobal_ILIST83 [0]= GTTX_Text;
	tempGlobal_ILIST83 [2]= (long) NULL;
	SetList(tempGlobal_ILIST83 ,3);
	tempGlobal_ILIST84 = NewList_elist(3);
	tempGlobal_ILIST84 [0]= GA_Disabled;
	tempGlobal_ILIST84 [1]= FALSE;
	tempGlobal_ILIST84 [2]= (long) NULL;
	SetList(tempGlobal_ILIST84 ,3);
	tempGlobal_ILIST85 = NewList_elist(3);
	tempGlobal_ILIST85 [0]= GA_Disabled;
	tempGlobal_ILIST85 [1]= TRUE;
	tempGlobal_ILIST85 [2]= (long) NULL;
	SetList(tempGlobal_ILIST85 ,3);
	tempGlobal_ILIST86 = NewList_elist(2);
	tempGlobal_ILIST86 [0]= GTCB_Checked;
	tempGlobal_ILIST86 [1]= TRUE;
	SetList(tempGlobal_ILIST86 ,2);
	tempGlobal_ILIST87 = NewList_elist(2);
	tempGlobal_ILIST87 [0]= GTCB_Checked;
	tempGlobal_ILIST87 [1]= FALSE;
	SetList(tempGlobal_ILIST87 ,2);
	tempGlobal_ILIST88 = NewList_elist(2);
	tempGlobal_ILIST88 [0]= GTCB_Checked;
	tempGlobal_ILIST88 [1]= TRUE;
	SetList(tempGlobal_ILIST88 ,2);
	tempGlobal_ILIST89 = NewList_elist(2);
	tempGlobal_ILIST89 [0]= GTCB_Checked;
	tempGlobal_ILIST89 [1]= FALSE;
	SetList(tempGlobal_ILIST89 ,2);
	tempGlobal_ILIST90 = NewList_elist(2);
	tempGlobal_ILIST90 [0]= GTCB_Checked;
	tempGlobal_ILIST90 [1]= FALSE;
	SetList(tempGlobal_ILIST90 ,2);
	tempGlobal_ILIST91 = NewList_elist(2);
	tempGlobal_ILIST91 [0]= GTCB_Checked;
	tempGlobal_ILIST91 [1]= TRUE;
	SetList(tempGlobal_ILIST91 ,2);
	tempGlobal_ILIST92 = NewList_elist(2);
	tempGlobal_ILIST92 [0]= GTCB_Checked;
	tempGlobal_ILIST92 [1]= TRUE;
	SetList(tempGlobal_ILIST92 ,2);
	tempGlobal_ILIST93 = NewList_elist(2);
	tempGlobal_ILIST93 [0]= GTCB_Checked;
	tempGlobal_ILIST93 [1]= FALSE;
	SetList(tempGlobal_ILIST93 ,2);
	tempGlobal_ILIST94 = NewList_elist(2);
	tempGlobal_ILIST94 [0]= GTSL_Level;
	SetList(tempGlobal_ILIST94 ,2);
	tempGlobal_ILIST95 = NewList_elist(3);
	tempGlobal_ILIST95 [0]= GTNM_Number;
	tempGlobal_ILIST95 [2]= (long) NULL;
	SetList(tempGlobal_ILIST95 ,3);
	tempGlobal_ILIST96 = NewList_elist(2);
	tempGlobal_ILIST96 [0]= GTSL_Level;
	SetList(tempGlobal_ILIST96 ,2);
	tempGlobal_ILIST97 = NewList_elist(3);
	tempGlobal_ILIST97 [0]= GTNM_Number;
	tempGlobal_ILIST97 [2]= (long) NULL;
	SetList(tempGlobal_ILIST97 ,3);
	tempGlobal_ILIST98 = NewList_elist(2);
	tempGlobal_ILIST98 [0]= GTSL_Level;
	SetList(tempGlobal_ILIST98 ,2);
	tempGlobal_ILIST99 = NewList_elist(3);
	tempGlobal_ILIST99 [0]= GTNM_Number;
	tempGlobal_ILIST99 [2]= (long) NULL;
	SetList(tempGlobal_ILIST99 ,3);
	tempGlobal_ILIST100 = NewList_elist(2);
	tempGlobal_ILIST100 [0]= GTSL_Level;
	SetList(tempGlobal_ILIST100 ,2);
	tempGlobal_ILIST101 = NewList_elist(3);
	tempGlobal_ILIST101 [0]= GTNM_Number;
	tempGlobal_ILIST101 [2]= (long) NULL;
	SetList(tempGlobal_ILIST101 ,3);
	tempGlobal_ILIST102 = NewList_elist(2);
	tempGlobal_ILIST102 [0]= GTSL_Level;
	SetList(tempGlobal_ILIST102 ,2);
	tempGlobal_ILIST103 = NewList_elist(3);
	tempGlobal_ILIST103 [0]= GTNM_Number;
	tempGlobal_ILIST103 [2]= (long) NULL;
	SetList(tempGlobal_ILIST103 ,3);
	tempGlobal_ILIST104 = NewList_elist(2);
	tempGlobal_ILIST104 [0]= GTSL_Level;
	SetList(tempGlobal_ILIST104 ,2);
	tempGlobal_ILIST105 = NewList_elist(3);
	tempGlobal_ILIST105 [0]= GTNM_Number;
	tempGlobal_ILIST105 [2]= (long) NULL;
	SetList(tempGlobal_ILIST105 ,3);
	tempGlobal_ILIST106 = NewList_elist(2);
	tempGlobal_ILIST106 [0]= GTSL_Level;
	SetList(tempGlobal_ILIST106 ,2);
	tempGlobal_ILIST107 = NewList_elist(3);
	tempGlobal_ILIST107 [0]= GTNM_Number;
	tempGlobal_ILIST107 [2]= (long) NULL;
	SetList(tempGlobal_ILIST107 ,3);
	tempGlobal_ILIST108 = NewList_elist(2);
	tempGlobal_ILIST108 [0]= GTSL_Level;
	SetList(tempGlobal_ILIST108 ,2);
	tempGlobal_ILIST109 = NewList_elist(3);
	tempGlobal_ILIST109 [0]= GTNM_Number;
	tempGlobal_ILIST109 [2]= (long) NULL;
	SetList(tempGlobal_ILIST109 ,3);
	tempGlobal_ILIST110 = NewList_elist(2);
	tempGlobal_ILIST110 [0]= GTSL_Level;
	SetList(tempGlobal_ILIST110 ,2);
	tempGlobal_ILIST111 = NewList_elist(3);
	tempGlobal_ILIST111 [0]= GTNM_Number;
	tempGlobal_ILIST111 [2]= (long) NULL;
	SetList(tempGlobal_ILIST111 ,3);
	tempGlobal_ILIST112 = NewList_elist(2);
	tempGlobal_ILIST112 [0]= GTCY_Active;
	SetList(tempGlobal_ILIST112 ,2);
	tempGlobal_ILIST113 = NewList_elist(1);
	tempGlobal_ILIST113 [0]= (long) NULL;
	SetList(tempGlobal_ILIST113 ,1);
	tempGlobal_ARRAY_OF_newgadget11 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget11 [0].ng_LeftEdge = 215;
	tempGlobal_ARRAY_OF_newgadget11 [0].ng_Width = 40;
	tempGlobal_ARRAY_OF_newgadget11 [0].ng_GadgetText = (CONST_STRPTR) (long) "";
	tempGlobal_ARRAY_OF_newgadget11 [0].ng_GadgetID = (UWORD) ADV_BRIGHTNUM;
	tempGlobal_ARRAY_OF_newgadget11 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget11 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST114 = NewList_elist(5);
	tempGlobal_ILIST114 [0]= GTNM_Number;
	tempGlobal_ILIST114 [2]= GTNM_Border;
	tempGlobal_ILIST114 [3]= TRUE;
	tempGlobal_ILIST114 [4]= (long) NULL;
	SetList(tempGlobal_ILIST114 ,5);
	tempGlobal_ARRAY_OF_newgadget12 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget12 [0].ng_LeftEdge = 215;
	tempGlobal_ARRAY_OF_newgadget12 [0].ng_Width = 40;
	tempGlobal_ARRAY_OF_newgadget12 [0].ng_GadgetText = (CONST_STRPTR) (long) "";
	tempGlobal_ARRAY_OF_newgadget12 [0].ng_GadgetID = (UWORD) ADV_COLOURNUM;
	tempGlobal_ARRAY_OF_newgadget12 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget12 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST115 = NewList_elist(5);
	tempGlobal_ILIST115 [0]= GTNM_Number;
	tempGlobal_ILIST115 [2]= GTNM_Border;
	tempGlobal_ILIST115 [3]= TRUE;
	tempGlobal_ILIST115 [4]= (long) NULL;
	SetList(tempGlobal_ILIST115 ,5);
	tempGlobal_ARRAY_OF_newgadget13 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget13 [0].ng_LeftEdge = 215;
	tempGlobal_ARRAY_OF_newgadget13 [0].ng_Width = 40;
	tempGlobal_ARRAY_OF_newgadget13 [0].ng_GadgetText = (CONST_STRPTR) (long) "";
	tempGlobal_ARRAY_OF_newgadget13 [0].ng_GadgetID = (UWORD) ADV_WHITENUM;
	tempGlobal_ARRAY_OF_newgadget13 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget13 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST116 = NewList_elist(5);
	tempGlobal_ILIST116 [0]= GTNM_Number;
	tempGlobal_ILIST116 [2]= GTNM_Border;
	tempGlobal_ILIST116 [3]= TRUE;
	tempGlobal_ILIST116 [4]= (long) NULL;
	SetList(tempGlobal_ILIST116 ,5);
	tempGlobal_ARRAY_OF_newgadget14 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget14 [0].ng_LeftEdge = 85;
	tempGlobal_ARRAY_OF_newgadget14 [0].ng_Width = 130;
	tempGlobal_ARRAY_OF_newgadget14 [0].ng_GadgetText = (CONST_STRPTR) (long) "Bright:";
	tempGlobal_ARRAY_OF_newgadget14 [0].ng_GadgetID = (UWORD) ADV_BRIGHT;
	tempGlobal_ARRAY_OF_newgadget14 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget14 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST117 = NewList_elist(15);
	tempGlobal_ILIST117 [0]= GA_Disabled;
	tempGlobal_ILIST117 [2]= GA_RelVerify;
	tempGlobal_ILIST117 [3]= TRUE;
	tempGlobal_ILIST117 [4]= GTSL_Min;
	tempGlobal_ILIST117 [5]= 0;
	tempGlobal_ILIST117 [6]= GTSL_Max;
	tempGlobal_ILIST117 [7]= 765;
	tempGlobal_ILIST117 [8]= GTSL_Level;
	tempGlobal_ILIST117 [10]= GTSL_MaxLevelLen;
	tempGlobal_ILIST117 [11]= 3;
	tempGlobal_ILIST117 [12]= GTSL_LevelPlace;
	tempGlobal_ILIST117 [13]= PLACETEXT_RIGHT;
	tempGlobal_ILIST117 [14]= (long) NULL;
	SetList(tempGlobal_ILIST117 ,15);
	tempGlobal_ARRAY_OF_newgadget15 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget15 [0].ng_LeftEdge = 85;
	tempGlobal_ARRAY_OF_newgadget15 [0].ng_Width = 130;
	tempGlobal_ARRAY_OF_newgadget15 [0].ng_GadgetText = (CONST_STRPTR) (long) "Colour:";
	tempGlobal_ARRAY_OF_newgadget15 [0].ng_GadgetID = (UWORD) ADV_COLOUR;
	tempGlobal_ARRAY_OF_newgadget15 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget15 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST118 = NewList_elist(15);
	tempGlobal_ILIST118 [0]= GA_Disabled;
	tempGlobal_ILIST118 [2]= GA_RelVerify;
	tempGlobal_ILIST118 [3]= TRUE;
	tempGlobal_ILIST118 [4]= GTSL_Min;
	tempGlobal_ILIST118 [5]= 0;
	tempGlobal_ILIST118 [6]= GTSL_Max;
	tempGlobal_ILIST118 [7]= 255;
	tempGlobal_ILIST118 [8]= GTSL_Level;
	tempGlobal_ILIST118 [10]= GTSL_MaxLevelLen;
	tempGlobal_ILIST118 [11]= 3;
	tempGlobal_ILIST118 [12]= GTSL_LevelPlace;
	tempGlobal_ILIST118 [13]= PLACETEXT_RIGHT;
	tempGlobal_ILIST118 [14]= (long) NULL;
	SetList(tempGlobal_ILIST118 ,15);
	tempGlobal_ARRAY_OF_newgadget16 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget16 [0].ng_LeftEdge = 85;
	tempGlobal_ARRAY_OF_newgadget16 [0].ng_Width = 130;
	tempGlobal_ARRAY_OF_newgadget16 [0].ng_GadgetText = (CONST_STRPTR) (long) "White:";
	tempGlobal_ARRAY_OF_newgadget16 [0].ng_GadgetID = (UWORD) ADV_WHITE;
	tempGlobal_ARRAY_OF_newgadget16 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget16 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST119 = NewList_elist(15);
	tempGlobal_ILIST119 [0]= GA_Disabled;
	tempGlobal_ILIST119 [2]= GA_RelVerify;
	tempGlobal_ILIST119 [3]= TRUE;
	tempGlobal_ILIST119 [4]= GTSL_Min;
	tempGlobal_ILIST119 [5]= 0;
	tempGlobal_ILIST119 [6]= GTSL_Max;
	tempGlobal_ILIST119 [7]= 765;
	tempGlobal_ILIST119 [8]= GTSL_Level;
	tempGlobal_ILIST119 [10]= GTSL_MaxLevelLen;
	tempGlobal_ILIST119 [11]= 3;
	tempGlobal_ILIST119 [12]= GTSL_LevelPlace;
	tempGlobal_ILIST119 [13]= PLACETEXT_RIGHT;
	tempGlobal_ILIST119 [14]= (long) NULL;
	SetList(tempGlobal_ILIST119 ,15);
	tempGlobal_ARRAY_OF_newgadget17 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget17 [0].ng_LeftEdge = 85;
	tempGlobal_ARRAY_OF_newgadget17 [0].ng_Width = 130;
	tempGlobal_ARRAY_OF_newgadget17 [0].ng_GadgetText = (CONST_STRPTR) (long) "Red/Yel:";
	tempGlobal_ARRAY_OF_newgadget17 [0].ng_GadgetID = (UWORD) ADV_REDYEL;
	tempGlobal_ARRAY_OF_newgadget17 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget17 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST120 = NewList_elist(15);
	tempGlobal_ILIST120 [0]= GA_Disabled;
	tempGlobal_ILIST120 [2]= GA_RelVerify;
	tempGlobal_ILIST120 [3]= TRUE;
	tempGlobal_ILIST120 [4]= GTSL_Min;
	tempGlobal_ILIST120 [5]= 0;
	tempGlobal_ILIST120 [6]= GTSL_Max;
	tempGlobal_ILIST120 [7]= 255;
	tempGlobal_ILIST120 [8]= GTSL_Level;
	tempGlobal_ILIST120 [10]= GTSL_MaxLevelLen;
	tempGlobal_ILIST120 [11]= 3;
	tempGlobal_ILIST120 [12]= GTSL_LevelPlace;
	tempGlobal_ILIST120 [13]= PLACETEXT_RIGHT;
	tempGlobal_ILIST120 [14]= (long) NULL;
	SetList(tempGlobal_ILIST120 ,15);
	tempGlobal_ARRAY_OF_newgadget18 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget18 [0].ng_LeftEdge = 85;
	tempGlobal_ARRAY_OF_newgadget18 [0].ng_Width = 130;
	tempGlobal_ARRAY_OF_newgadget18 [0].ng_GadgetText = (CONST_STRPTR) (long) "Blu/Cyn:";
	tempGlobal_ARRAY_OF_newgadget18 [0].ng_GadgetID = (UWORD) ADV_BLUCYN;
	tempGlobal_ARRAY_OF_newgadget18 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget18 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST121 = NewList_elist(15);
	tempGlobal_ILIST121 [0]= GA_Disabled;
	tempGlobal_ILIST121 [2]= GA_RelVerify;
	tempGlobal_ILIST121 [3]= TRUE;
	tempGlobal_ILIST121 [4]= GTSL_Min;
	tempGlobal_ILIST121 [5]= 0;
	tempGlobal_ILIST121 [6]= GTSL_Max;
	tempGlobal_ILIST121 [7]= 255;
	tempGlobal_ILIST121 [8]= GTSL_Level;
	tempGlobal_ILIST121 [10]= GTSL_MaxLevelLen;
	tempGlobal_ILIST121 [11]= 3;
	tempGlobal_ILIST121 [12]= GTSL_LevelPlace;
	tempGlobal_ILIST121 [13]= PLACETEXT_RIGHT;
	tempGlobal_ILIST121 [14]= (long) NULL;
	SetList(tempGlobal_ILIST121 ,15);
	tempGlobal_ARRAY_OF_newgadget19 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget19 [0].ng_LeftEdge = 85;
	tempGlobal_ARRAY_OF_newgadget19 [0].ng_Width = 130;
	tempGlobal_ARRAY_OF_newgadget19 [0].ng_GadgetText = (CONST_STRPTR) (long) "Grn/Mag:";
	tempGlobal_ARRAY_OF_newgadget19 [0].ng_GadgetID = (UWORD) ADV_GRNMAG;
	tempGlobal_ARRAY_OF_newgadget19 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget19 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST122 = NewList_elist(15);
	tempGlobal_ILIST122 [0]= GA_Disabled;
	tempGlobal_ILIST122 [2]= GA_RelVerify;
	tempGlobal_ILIST122 [3]= TRUE;
	tempGlobal_ILIST122 [4]= GTSL_Min;
	tempGlobal_ILIST122 [5]= 0;
	tempGlobal_ILIST122 [6]= GTSL_Max;
	tempGlobal_ILIST122 [7]= 255;
	tempGlobal_ILIST122 [8]= GTSL_Level;
	tempGlobal_ILIST122 [10]= GTSL_MaxLevelLen;
	tempGlobal_ILIST122 [11]= 3;
	tempGlobal_ILIST122 [12]= GTSL_LevelPlace;
	tempGlobal_ILIST122 [13]= PLACETEXT_RIGHT;
	tempGlobal_ILIST122 [14]= (long) NULL;
	SetList(tempGlobal_ILIST122 ,15);
	tempGlobal_ARRAY_OF_newgadget20 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget20 [0].ng_LeftEdge = 215;
	tempGlobal_ARRAY_OF_newgadget20 [0].ng_Width = 40;
	tempGlobal_ARRAY_OF_newgadget20 [0].ng_GadgetText = (CONST_STRPTR) (long) "";
	tempGlobal_ARRAY_OF_newgadget20 [0].ng_GadgetID = (UWORD) ADV_REDYELNUM;
	tempGlobal_ARRAY_OF_newgadget20 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget20 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST123 = NewList_elist(5);
	tempGlobal_ILIST123 [0]= GTNM_Number;
	tempGlobal_ILIST123 [2]= GTNM_Border;
	tempGlobal_ILIST123 [3]= TRUE;
	tempGlobal_ILIST123 [4]= (long) NULL;
	SetList(tempGlobal_ILIST123 ,5);
	tempGlobal_ARRAY_OF_newgadget21 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget21 [0].ng_LeftEdge = 215;
	tempGlobal_ARRAY_OF_newgadget21 [0].ng_Width = 40;
	tempGlobal_ARRAY_OF_newgadget21 [0].ng_GadgetText = (CONST_STRPTR) (long) "";
	tempGlobal_ARRAY_OF_newgadget21 [0].ng_GadgetID = (UWORD) ADV_BLUCYNNUM;
	tempGlobal_ARRAY_OF_newgadget21 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget21 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST124 = NewList_elist(5);
	tempGlobal_ILIST124 [0]= GTNM_Number;
	tempGlobal_ILIST124 [2]= GTNM_Border;
	tempGlobal_ILIST124 [3]= TRUE;
	tempGlobal_ILIST124 [4]= (long) NULL;
	SetList(tempGlobal_ILIST124 ,5);
	tempGlobal_ARRAY_OF_newgadget22 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget22 [0].ng_LeftEdge = 215;
	tempGlobal_ARRAY_OF_newgadget22 [0].ng_Width = 40;
	tempGlobal_ARRAY_OF_newgadget22 [0].ng_GadgetText = (CONST_STRPTR) (long) "";
	tempGlobal_ARRAY_OF_newgadget22 [0].ng_GadgetID = (UWORD) ADV_GRNMAGNUM;
	tempGlobal_ARRAY_OF_newgadget22 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget22 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST125 = NewList_elist(5);
	tempGlobal_ILIST125 [0]= GTNM_Number;
	tempGlobal_ILIST125 [2]= GTNM_Border;
	tempGlobal_ILIST125 [3]= TRUE;
	tempGlobal_ILIST125 [4]= (long) NULL;
	SetList(tempGlobal_ILIST125 ,5);
	tempGlobal_ARRAY_OF_newgadget23 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget23 [0].ng_LeftEdge = 85;
	tempGlobal_ARRAY_OF_newgadget23 [0].ng_Width = 100;
	tempGlobal_ARRAY_OF_newgadget23 [0].ng_GadgetText = (CONST_STRPTR) (long) "Flash: Red";
	tempGlobal_ARRAY_OF_newgadget23 [0].ng_GadgetID = (UWORD) ADV_FLASHRED;
	tempGlobal_ARRAY_OF_newgadget23 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget23 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST126 = NewList_elist(13);
	tempGlobal_ILIST126 [0]= GA_RelVerify;
	tempGlobal_ILIST126 [1]= TRUE;
	tempGlobal_ILIST126 [2]= GTSL_Min;
	tempGlobal_ILIST126 [3]= -1;
	tempGlobal_ILIST126 [4]= GTSL_Max;
	tempGlobal_ILIST126 [5]= 255;
	tempGlobal_ILIST126 [6]= GTSL_Level;
	tempGlobal_ILIST126 [8]= GTSL_MaxLevelLen;
	tempGlobal_ILIST126 [9]= 3;
	tempGlobal_ILIST126 [10]= GTSL_LevelPlace;
	tempGlobal_ILIST126 [11]= PLACETEXT_RIGHT;
	tempGlobal_ILIST126 [12]= (long) NULL;
	SetList(tempGlobal_ILIST126 ,13);
	tempGlobal_ARRAY_OF_newgadget24 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget24 [0].ng_LeftEdge = 85;
	tempGlobal_ARRAY_OF_newgadget24 [0].ng_Width = 100;
	tempGlobal_ARRAY_OF_newgadget24 [0].ng_GadgetText = (CONST_STRPTR) (long) "Green";
	tempGlobal_ARRAY_OF_newgadget24 [0].ng_GadgetID = (UWORD) ADV_FLASHGRN;
	tempGlobal_ARRAY_OF_newgadget24 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget24 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST127 = NewList_elist(13);
	tempGlobal_ILIST127 [0]= GA_RelVerify;
	tempGlobal_ILIST127 [1]= TRUE;
	tempGlobal_ILIST127 [2]= GTSL_Min;
	tempGlobal_ILIST127 [3]= -1;
	tempGlobal_ILIST127 [4]= GTSL_Max;
	tempGlobal_ILIST127 [5]= 255;
	tempGlobal_ILIST127 [6]= GTSL_Level;
	tempGlobal_ILIST127 [8]= GTSL_MaxLevelLen;
	tempGlobal_ILIST127 [9]= 3;
	tempGlobal_ILIST127 [10]= GTSL_LevelPlace;
	tempGlobal_ILIST127 [11]= PLACETEXT_RIGHT;
	tempGlobal_ILIST127 [12]= (long) NULL;
	SetList(tempGlobal_ILIST127 ,13);
	tempGlobal_ARRAY_OF_newgadget25 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget25 [0].ng_LeftEdge = 85;
	tempGlobal_ARRAY_OF_newgadget25 [0].ng_Width = 100;
	tempGlobal_ARRAY_OF_newgadget25 [0].ng_GadgetText = (CONST_STRPTR) (long) "Blue";
	tempGlobal_ARRAY_OF_newgadget25 [0].ng_GadgetID = (UWORD) ADV_FLASHBLU;
	tempGlobal_ARRAY_OF_newgadget25 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget25 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST128 = NewList_elist(13);
	tempGlobal_ILIST128 [0]= GA_RelVerify;
	tempGlobal_ILIST128 [1]= TRUE;
	tempGlobal_ILIST128 [2]= GTSL_Min;
	tempGlobal_ILIST128 [3]= -1;
	tempGlobal_ILIST128 [4]= GTSL_Max;
	tempGlobal_ILIST128 [5]= 255;
	tempGlobal_ILIST128 [6]= GTSL_Level;
	tempGlobal_ILIST128 [8]= GTSL_MaxLevelLen;
	tempGlobal_ILIST128 [9]= 3;
	tempGlobal_ILIST128 [10]= GTSL_LevelPlace;
	tempGlobal_ILIST128 [11]= PLACETEXT_RIGHT;
	tempGlobal_ILIST128 [12]= (long) NULL;
	SetList(tempGlobal_ILIST128 ,13);
	tempGlobal_ARRAY_OF_newgadget26 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget26 [0].ng_LeftEdge = 185;
	tempGlobal_ARRAY_OF_newgadget26 [0].ng_Width = 40;
	tempGlobal_ARRAY_OF_newgadget26 [0].ng_GadgetText = (CONST_STRPTR) (long) "";
	tempGlobal_ARRAY_OF_newgadget26 [0].ng_GadgetID = (UWORD) ADV_FLASHREDNUM;
	tempGlobal_ARRAY_OF_newgadget26 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget26 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST129 = NewList_elist(5);
	tempGlobal_ILIST129 [0]= GTNM_Number;
	tempGlobal_ILIST129 [2]= GTNM_Border;
	tempGlobal_ILIST129 [3]= TRUE;
	tempGlobal_ILIST129 [4]= (long) NULL;
	SetList(tempGlobal_ILIST129 ,5);
	tempGlobal_ARRAY_OF_newgadget27 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget27 [0].ng_LeftEdge = 185;
	tempGlobal_ARRAY_OF_newgadget27 [0].ng_Width = 40;
	tempGlobal_ARRAY_OF_newgadget27 [0].ng_GadgetText = (CONST_STRPTR) (long) "";
	tempGlobal_ARRAY_OF_newgadget27 [0].ng_GadgetID = (UWORD) ADV_FLASHGRNNUM;
	tempGlobal_ARRAY_OF_newgadget27 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget27 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST130 = NewList_elist(5);
	tempGlobal_ILIST130 [0]= GTNM_Number;
	tempGlobal_ILIST130 [2]= GTNM_Border;
	tempGlobal_ILIST130 [3]= TRUE;
	tempGlobal_ILIST130 [4]= (long) NULL;
	SetList(tempGlobal_ILIST130 ,5);
	tempGlobal_ARRAY_OF_newgadget28 = (struct NewGadget*) calloc(1 ,sizeof( struct NewGadget) );
	tempGlobal_ARRAY_OF_newgadget28 [0].ng_LeftEdge = 185;
	tempGlobal_ARRAY_OF_newgadget28 [0].ng_Width = 40;
	tempGlobal_ARRAY_OF_newgadget28 [0].ng_GadgetText = (CONST_STRPTR) (long) "";
	tempGlobal_ARRAY_OF_newgadget28 [0].ng_GadgetID = (UWORD) ADV_FLASHBLUNUM;
	tempGlobal_ARRAY_OF_newgadget28 [0].ng_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_newgadget28 [0].ng_UserData = (APTR) (long) NULL;
	tempGlobal_ILIST131 = NewList_elist(5);
	tempGlobal_ILIST131 [0]= GTNM_Number;
	tempGlobal_ILIST131 [2]= GTNM_Border;
	tempGlobal_ILIST131 [3]= TRUE;
	tempGlobal_ILIST131 [4]= (long) NULL;
	SetList(tempGlobal_ILIST131 ,5);
	tempGlobal_ARRAY_OF_easystruct20 = (struct EasyStruct*) calloc(1 ,sizeof( struct EasyStruct) );
	tempGlobal_ARRAY_OF_easystruct20 [0].es_Flags = (ULONG) 0;
	tempGlobal_ARRAY_OF_easystruct20 [0].es_Title = (CONST_STRPTR) (long) "About PDHFIC...";
	tempGlobal_ARRAY_OF_easystruct20 [0].es_TextFormat = (CONST_STRPTR) (long) "PDHFIC version 3.3 by Chris Young\nDithering based on code by LCD\nOriginal Datatypes code by Joe Mackay\nOS4 port made possible with PortablE by Chris Handley\n\n\251 1998-2001, 2008-2009 Unsatisfactory Software\nemail: chris@unsatisfactorysoftware.co.uk\nhttp://www.unsatisfactorysoftware.co.uk\n\nThe Amiga -> Spectrum image conversion utility!\n\nLoads: Datatypes, PPM\nSaves: SCR, TAP, TZX, ZX82, .header/.bytes, GIF, +3DOS";
	tempGlobal_ARRAY_OF_easystruct20 [0].es_GadgetFormat = (CONST_STRPTR) (long) "Oh, great";
	tempGlobal_ARRAY_OF_tagitem4 = (struct TagItem*) calloc(13 ,sizeof( struct TagItem) );
	tempGlobal_ARRAY_OF_tagitem4 [0].ti_Tag = (Tag) WA_Title;
	tempGlobal_ARRAY_OF_tagitem4 [0].ti_Data = (STACKIPTR) (long) "PDHFIC";
	tempGlobal_ARRAY_OF_tagitem4 [1].ti_Tag = (Tag) WA_Top;
	tempGlobal_ARRAY_OF_tagitem4 [2].ti_Tag = (Tag) WA_Left;
	tempGlobal_ARRAY_OF_tagitem4 [3].ti_Tag = (Tag) WA_Width;
	tempGlobal_ARRAY_OF_tagitem4 [3].ti_Data = (STACKIPTR) (long) 270;
	tempGlobal_ARRAY_OF_tagitem4 [4].ti_Tag = (Tag) WA_Height;
	tempGlobal_ARRAY_OF_tagitem4 [5].ti_Tag = (Tag) WA_AutoAdjust;
	tempGlobal_ARRAY_OF_tagitem4 [5].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem4 [6].ti_Tag = (Tag) WA_DragBar;
	tempGlobal_ARRAY_OF_tagitem4 [6].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem4 [7].ti_Tag = (Tag) WA_DepthGadget;
	tempGlobal_ARRAY_OF_tagitem4 [7].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem4 [8].ti_Tag = (Tag) WA_Activate;
	tempGlobal_ARRAY_OF_tagitem4 [8].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem4 [9].ti_Tag = (Tag) WA_CloseGadget;
	tempGlobal_ARRAY_OF_tagitem4 [9].ti_Data = (STACKIPTR) FALSE;
	tempGlobal_ARRAY_OF_tagitem4 [10].ti_Tag = (Tag) WA_BusyPointer;
	tempGlobal_ARRAY_OF_tagitem4 [10].ti_Data = (STACKIPTR) TRUE;
	tempGlobal_ARRAY_OF_tagitem4 [11].ti_Tag = (Tag) WA_ScreenTitle;
	tempGlobal_ARRAY_OF_tagitem4 [11].ti_Data = (STACKIPTR) (long) "PDHFIC 3.3  \251 1998-2001,2008-2009 Unsatisfactory Software";
	tempGlobal_ARRAY_OF_tagitem4 [12].ti_Tag = (Tag) (long) NULL;
	tempGlobal_ARRAY_OF_tagitem4 [12].ti_Data = (STACKIPTR) 0;
	tempGlobal_ILIST132 = NewList_elist(5);
	tempGlobal_ILIST132 [0]= GTBB_Recessed;
	tempGlobal_ILIST132 [1]= TRUE;
	tempGlobal_ILIST132 [2]= GT_VisualInfo;
	tempGlobal_ILIST132 [4]= (long) NULL;
	SetList(tempGlobal_ILIST132 ,5);
	tempGlobal_ILIST133 = NewList_elist(3);
	tempGlobal_ILIST133 [0]= OBP_Precision;
	tempGlobal_ILIST133 [1]= PRECISION_GUI;
	tempGlobal_ILIST133 [2]= (long) NULL;
	SetList(tempGlobal_ILIST133 ,3);
	return ;
}
void end_dt2scr2_asl_aros()  {
	DisposeList(tempGlobal_ILIST133 );
	DisposeList(tempGlobal_ILIST132 );
	free(tempGlobal_ARRAY_OF_tagitem4 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct20 );
	NULLA;
	DisposeList(tempGlobal_ILIST131 );
	free(tempGlobal_ARRAY_OF_newgadget28 );
	NULLA;
	DisposeList(tempGlobal_ILIST130 );
	free(tempGlobal_ARRAY_OF_newgadget27 );
	NULLA;
	DisposeList(tempGlobal_ILIST129 );
	free(tempGlobal_ARRAY_OF_newgadget26 );
	NULLA;
	DisposeList(tempGlobal_ILIST128 );
	free(tempGlobal_ARRAY_OF_newgadget25 );
	NULLA;
	DisposeList(tempGlobal_ILIST127 );
	free(tempGlobal_ARRAY_OF_newgadget24 );
	NULLA;
	DisposeList(tempGlobal_ILIST126 );
	free(tempGlobal_ARRAY_OF_newgadget23 );
	NULLA;
	DisposeList(tempGlobal_ILIST125 );
	free(tempGlobal_ARRAY_OF_newgadget22 );
	NULLA;
	DisposeList(tempGlobal_ILIST124 );
	free(tempGlobal_ARRAY_OF_newgadget21 );
	NULLA;
	DisposeList(tempGlobal_ILIST123 );
	free(tempGlobal_ARRAY_OF_newgadget20 );
	NULLA;
	DisposeList(tempGlobal_ILIST122 );
	free(tempGlobal_ARRAY_OF_newgadget19 );
	NULLA;
	DisposeList(tempGlobal_ILIST121 );
	free(tempGlobal_ARRAY_OF_newgadget18 );
	NULLA;
	DisposeList(tempGlobal_ILIST120 );
	free(tempGlobal_ARRAY_OF_newgadget17 );
	NULLA;
	DisposeList(tempGlobal_ILIST119 );
	free(tempGlobal_ARRAY_OF_newgadget16 );
	NULLA;
	DisposeList(tempGlobal_ILIST118 );
	free(tempGlobal_ARRAY_OF_newgadget15 );
	NULLA;
	DisposeList(tempGlobal_ILIST117 );
	free(tempGlobal_ARRAY_OF_newgadget14 );
	NULLA;
	DisposeList(tempGlobal_ILIST116 );
	free(tempGlobal_ARRAY_OF_newgadget13 );
	NULLA;
	DisposeList(tempGlobal_ILIST115 );
	free(tempGlobal_ARRAY_OF_newgadget12 );
	NULLA;
	DisposeList(tempGlobal_ILIST114 );
	free(tempGlobal_ARRAY_OF_newgadget11 );
	NULLA;
	DisposeList(tempGlobal_ILIST113 );
	DisposeList(tempGlobal_ILIST112 );
	DisposeList(tempGlobal_ILIST111 );
	DisposeList(tempGlobal_ILIST110 );
	DisposeList(tempGlobal_ILIST109 );
	DisposeList(tempGlobal_ILIST108 );
	DisposeList(tempGlobal_ILIST107 );
	DisposeList(tempGlobal_ILIST106 );
	DisposeList(tempGlobal_ILIST105 );
	DisposeList(tempGlobal_ILIST104 );
	DisposeList(tempGlobal_ILIST103 );
	DisposeList(tempGlobal_ILIST102 );
	DisposeList(tempGlobal_ILIST101 );
	DisposeList(tempGlobal_ILIST100 );
	DisposeList(tempGlobal_ILIST99 );
	DisposeList(tempGlobal_ILIST98 );
	DisposeList(tempGlobal_ILIST97 );
	DisposeList(tempGlobal_ILIST96 );
	DisposeList(tempGlobal_ILIST95 );
	DisposeList(tempGlobal_ILIST94 );
	DisposeList(tempGlobal_ILIST93 );
	DisposeList(tempGlobal_ILIST92 );
	DisposeList(tempGlobal_ILIST91 );
	DisposeList(tempGlobal_ILIST90 );
	DisposeList(tempGlobal_ILIST89 );
	DisposeList(tempGlobal_ILIST88 );
	DisposeList(tempGlobal_ILIST87 );
	DisposeList(tempGlobal_ILIST86 );
	DisposeList(tempGlobal_ILIST85 );
	DisposeList(tempGlobal_ILIST84 );
	DisposeList(tempGlobal_ILIST83 );
	DisposeList(tempGlobal_ILIST82 );
	DisposeList(tempGlobal_ILIST81 );
	DisposeList(tempGlobal_ILIST80 );
	DisposeList(tempGlobal_ILIST79 );
	DisposeList(tempGlobal_ILIST78 );
	DisposeList(tempGlobal_ILIST77 );
	DisposeList(tempGlobal_ILIST76 );
	free(tempGlobal_ARRAY_OF_easystruct19 );
	NULLA;
	DisposeList(tempGlobal_ILIST75 );
	DisposeList(tempGlobal_ILIST74 );
	DisposeList(tempGlobal_ILIST73 );
	DisposeList(tempGlobal_ILIST72 );
	DisposeList(tempGlobal_ILIST71 );
	DisposeList(tempGlobal_ILIST70 );
	DisposeList(tempGlobal_ILIST69 );
	free(tempGlobal_ARRAY_OF_easystruct18 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct17 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct16 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct15 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct14 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct13 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct12 );
	NULLA;
	DisposeList(tempGlobal_ILIST68 );
	DisposeList(tempGlobal_ILIST67 );
	DisposeList(tempGlobal_ILIST66 );
	DisposeList(tempGlobal_ILIST65 );
	DisposeList(tempGlobal_ILIST64 );
	DisposeList(tempGlobal_ILIST63 );
	DisposeList(tempGlobal_ILIST62 );
	DisposeList(tempGlobal_ILIST61 );
	DisposeList(tempGlobal_ILIST60 );
	DisposeList(tempGlobal_ILIST59 );
	DisposeList(tempGlobal_ILIST58 );
	DisposeList(tempGlobal_ILIST57 );
	DisposeList(tempGlobal_ILIST56 );
	DisposeList(tempGlobal_ILIST55 );
	DisposeList(tempGlobal_ILIST54 );
	DisposeList(tempGlobal_ILIST53 );
	DisposeList(tempGlobal_ILIST52 );
	DisposeList(tempGlobal_ILIST51 );
	DisposeList(tempGlobal_ILIST50 );
	DisposeList(tempGlobal_ILIST49 );
	DisposeList(tempGlobal_ILIST48 );
	DisposeList(tempGlobal_ILIST47 );
	DisposeList(tempGlobal_ILIST46 );
	DisposeList(tempGlobal_ILIST45 );
	DisposeList(tempGlobal_ILIST44 );
	DisposeList(tempGlobal_ILIST43 );
	DisposeList(tempGlobal_ILIST42 );
	free(tempGlobal_ARRAY_OF_easystruct11 );
	NULLA;
	DisposeList(tempGlobal_ILIST41 );
	DisposeList(tempGlobal_ILIST40 );
	DisposeList(tempGlobal_ILIST39 );
	DisposeList(tempGlobal_ILIST38 );
	DisposeList(tempGlobal_ILIST37 );
	DisposeList(tempGlobal_ILIST36 );
	DisposeList(tempGlobal_ILIST35 );
	DisposeList(tempGlobal_ILIST34 );
	DisposeList(tempGlobal_ILIST33 );
	DisposeList(tempGlobal_ILIST32 );
	DisposeList(tempGlobal_ILIST31 );
	free(tempGlobal_ARRAY_OF_easystruct10 );
	NULLA;
	DisposeList(tempGlobal_ILIST30 );
	DisposeList(tempGlobal_ILIST29 );
	DisposeList(tempGlobal_ILIST28 );
	free(tempGlobal_ARRAY_OF_easystruct9 );
	NULLA;
	DisposeList(tempGlobal_ILIST27 );
	DisposeList(tempGlobal_ILIST26 );
	DisposeList(tempGlobal_ILIST25 );
	DisposeList(tempGlobal_ILIST24 );
	DisposeList(tempGlobal_ILIST23 );
	free(tempGlobal_ARRAY_OF_tagitem3 );
	NULLA;
	free(tempGlobal_ARRAY_OF_tagitem2 );
	NULLA;
	DisposeList(tempGlobal_ILIST22 );
	free(tempGlobal_ARRAY_OF_newmenu );
	NULLA;
	DisposeList(tempGlobal_ILIST21 );
	free(tempGlobal_ARRAY_OF_newgadget10 );
	NULLA;
	DisposeList(tempGlobal_ILIST20 );
	free(tempGlobal_ARRAY_OF_newgadget9 );
	NULLA;
	DisposeList(tempGlobal_ILIST19 );
	free(tempGlobal_ARRAY_OF_newgadget8 );
	NULLA;
	DisposeList(tempGlobal_ILIST18 );
	free(tempGlobal_ARRAY_OF_newgadget7 );
	NULLA;
	DisposeList(tempGlobal_ILIST17 );
	free(tempGlobal_ARRAY_OF_newgadget6 );
	NULLA;
	DisposeList(tempGlobal_ILIST16 );
	DisposeList(tempGlobal_ILIST15 );
	DisposeList(tempGlobal_ILIST14 );
	free(tempGlobal_ARRAY_OF_newgadget5 );
	NULLA;
	DisposeList(tempGlobal_ILIST13 );
	free(tempGlobal_ARRAY_OF_newgadget4 );
	NULLA;
	DisposeList(tempGlobal_ILIST12 );
	free(tempGlobal_ARRAY_OF_newgadget3 );
	NULLA;
	DisposeList(tempGlobal_ILIST11 );
	free(tempGlobal_ARRAY_OF_newgadget2 );
	NULLA;
	free(tempGlobal_ARRAY_OF_tagitem );
	NULLA;
	free(tempGlobal_ARRAY_OF_newgadget );
	NULLA;
	DisposeList(tempGlobal_ILIST10 );
	free(tempGlobal_ARRAY_OF_easystruct8 );
	NULLA;
	free(tempGlobal_ARRAY_OF_pdtblitpixelarray );
	NULLA;
	free(tempGlobal_ARRAY_OF_pdtscale );
	NULLA;
	DisposeList(tempGlobal_ILIST9 );
	DisposeList(tempGlobal_ILIST8 );
	DisposeList(tempGlobal_ILIST7 );
	DisposeList(tempGlobal_ILIST6 );
	DisposeList(tempGlobal_ILIST5 );
	free(tempGlobal_ARRAY_OF_easystruct7 );
	NULLA;
	DisposeList(tempGlobal_ILIST4 );
	free(tempGlobal_ARRAY_OF_easystruct6 );
	NULLA;
	DisposeList(tempGlobal_ILIST3 );
	free(tempGlobal_ARRAY_OF_easystruct5 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct4 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct3 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct2 );
	NULLA;
	free(tempGlobal_ARRAY_OF_easystruct );
	NULLA;
	DisposeList(tempGlobal_ILIST2 );
	DisposeList(tempGlobal_ILIST );
	DisposeString(gifname );
	DisposeString(objcopy );
	DisposeString(objanno );
	DisposeString(objver );
	DisposeString(objauth );
	DisposeString(objname );
	DisposeString(loaddrw );
	DisposeString(tmpstr );
	DisposeString(filename );
	DisposeString(filetypeerror );
	DisposeString(wintit );
	DisposeString(endmsg );
	DisposeString(verstring );
	DisposeString(fname );
	DisposeString(drw );
	DisposeString(msg );
	DisposeString(filetype );
	DisposeString(headerfile );
	DisposeString(scrfile );
	return ;
}
