
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

#include <typeinfo>

#define NULLA NULL
#define NULLS NULL
#define NULLL NULL
#define EMPTY (void)0
#define TRUE -1
#define FALSE 0
#define QuadChara(a, b, c, d) ((a << 24) | (b << 16) | (c << 8) | d)
typedef signed char BOOLEAN;	//enum BOOLEAN {FALSE=0, TRUE=-1};
typedef void* pointer;
typedef void array;
class Exception {} Exception;

void* FastNew(long size, BOOLEAN noClear);
void* FastDispose(void* mem, long size);

int    main_argc;
char** main_argv;

const long ALL=(long) -1;
class object;
class class_base;

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
	
	friend class class_base;
	friend short Inp(void* fileHandle);
	friend long FileLength(char* path);
	friend BOOLEAN StrCmp(char* first, char* second, long len);
	friend long Val(char* string, long* addrRead);
	friend long InStr(char* haystack, char* needle, long startPos);
	friend char* UpperStr(char* string);
	friend char* LowerStr(char* string);
	friend void AstrCopy(void* destination, char* source, long destSize);
	friend void* NewR(long size, BOOLEAN noClear);
	friend void CleanUp(long returnValue);
	friend long Rnd(long max);
	friend long Mod(long a2, long b2);
	friend long Pow(long a2, long b2);
	friend float RealVal(char* string);
	friend void Throw(long a2, char* b2);
	friend void Raise(long a2);
	friend void new_base() ;
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
	
	friend class object;
	friend short Inp(void* fileHandle);
	friend long FileLength(char* path);
	friend BOOLEAN StrCmp(char* first, char* second, long len);
	friend long Val(char* string, long* addrRead);
	friend long InStr(char* haystack, char* needle, long startPos);
	friend char* UpperStr(char* string);
	friend char* LowerStr(char* string);
	friend void AstrCopy(void* destination, char* source, long destSize);
	friend void* NewR(long size, BOOLEAN noClear);
	friend void CleanUp(long returnValue);
	friend long Rnd(long max);
	friend long Mod(long a2, long b2);
	friend long Pow(long a2, long b2);
	friend float RealVal(char* string);
	friend void Throw(long a2, char* b2);
	friend void Raise(long a2);
	friend void new_base() ;
};
const long TYPEOF_class_base = (long) "class_base";

char* pe_TargetLanguage;

/* system globals */

long exception;
char* exceptionInfo;
float retFloat2; long ret2; float retFloat3; long ret3; float retFloat4; long ret4; float retFloat5; long ret5;
short Inp(void* fileHandle);
long FileLength(char* path);
BOOLEAN StrCmp(char* first, char* second, long len=ALL);
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

BOOLEAN StrCmp(char* first, char* second, long len) {
//IS 0 = (IF len=ALL THEN NATIVE {strcmp(} first {,} second {)} ENDNATIVE) ELSE (0 = NATIVE {strncmp(} first {,} second {,} len {)} ENDNATIVE))
	BOOLEAN match;
	if( len == ALL) {
		match = - (strcmp(first ,second )== 0) 	;//!!BYTE
	} else {
		match = - (strncmp(first ,second ,len )== 0) 	;//!!BYTE
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
	if( isNegative = - (str[0] == '-')  ) { str++;}
	
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
	if (exception!=0) {throw Exception;} else {EMPTY;};
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
	mem = ( noClear)? malloc(size): memset(malloc(size), 0,size);
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
	if (exception!=0) {throw Exception;} else {EMPTY;};
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
	if (exception!=0) {throw Exception;} else {EMPTY;};
	exception = temp_QUAD ;
	return value;
} 

//Does not work in StormC: IS (exception := a) BUT (exceptionInfo := b) BUT {throw new Exception()}
void Throw(long a2, char* b2) {
	exception     = a2;
	exceptionInfo = b2;
	throw Exception;
	return ;
}
void Raise(long a2) {
	exception = a2;
	throw Exception;
	return ;
}
void new_base()  {
	pe_TargetLanguage = "CPP";
	return ;
}
void class_base::end_class()  {
	EMPTY;
}
long class_base::InfoClassType()  {
	return TYPEOF_class_base;
}
BOOLEAN class_base::IsSameClassTypeAs(long type)  {
	return - (type== this->InfoClassType());
}
BOOLEAN class_base::IsOfClassType(long parent)  {
	return - (parent== TYPEOF_class_base);
}

/*** target/PE/base ***/
/* PortablE target module for C++ AmigaOS */ 

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
	pe_TargetOS = "AmigaOS3";
	return ;
}

/*** PE/CPP/FastMem_emulated ***/
/* PE/CPP/FastMem_emulated.e 02-04-08
   A re-implementation of AmigaE's fast memory functions,
   based upon Tomasz Wiszkowski's description of how AmigaE implemented it.
   
   By Christopher S Handley.
   Completed on 06-01-08, and put in the Public Domain.
   Replaced "NEW" by "MEM" on 02-04-08.
*/ 


void* freeListArray[256/4+1];
void* chopMem=NULLA; long chopLeft;
void* FastNew(long size, BOOLEAN noClear=FALSE);
void* FastDispose(void* mem, long size);


/*NATIVE {FastNew}*/ 
void* FastNew(long size, BOOLEAN noClear) {
	void* mem=NULL; signed char index;
	
	if( size <= 0 ) { Raise(QuadChara(0,'M','E','M'));}
	
	size = size + (long) sizeof( signed char);
	if( size > 256) {
		mem = NewR(size, noClear);
		index = 0;
	} else {
		size = size + (long) 3  & (long) ~ 3		;//round-up to a multiple of 4
		index = (signed char) ((size)>>(2));
		
		mem = freeListArray[index];
		if( mem != NULLA) {
			freeListArray[index] = (*(void**) mem);
		} else {
			if( - (chopMem == NULLA)  | - (chopLeft < size)) {
				chopLeft = 65536;
				chopMem  = NewR(chopLeft, /*noClear=*/ TRUE)		;//ensures memory will be freed automagically upon exit
			}
			
			mem = chopMem;
			chopMem  = (void*) ((char*) chopMem  + size);
			chopLeft = chopLeft - size;
		}
		
		if( noClear == FALSE ) { memset(mem , 0,size );}
	}
	(*(signed char*) mem=index);
	mem = (void*) ((char*) mem + sizeof( signed char));
	return mem;
} 

/*NATIVE {FastDispose}*/ 
void* FastDispose(void* mem, long size) {
	signed char index;
	
	if( mem) {
		if( - (size <= 0)  & - (size != -999)  ) { Throw(QuadChara(0,'M','E','M'), "FastDispose(); size<=0");}
		
		mem   = (void*) ((char*) mem - sizeof( signed char));
		index = (*(signed char*) mem);
		
		if( size != -999) {
			size = size + (long) sizeof( signed char);
			if( size > 256) {
				if( index != 0 ) { Throw(QuadChara(0,'M','E','M'), "FastDispose(); wrong size supplied or memory header corrupted");}
			} else {
				size = size + (long) 3  & (long) ~ 3;
				if( index != ((size)>>(2))) { Throw(QuadChara(0,'M','E','M'), "FastDispose(); wrong size supplied or memory header corrupted");}
			}
		}
		
		if( index == 0) {
			free(mem);
			NULLA;
		} else {
			(*(void**) mem=freeListArray[index]);
			freeListArray[index] = mem;
		}
	}
	return NULLA;
} 

/*** PE/EndianBig ***/
/* PE/EndianBig.e 31-03-08
   Does NOT swaps endianness, for use on big-endian machines.
*/ 

/*** PE/Char ***/
/* PE/Char.e 06-06-08
*/ 

/*** PE/OstrCmp ***/


//Replacement for AmigaE's OstrCmp(), which (possibly erratically) seems to incorrectly think these two characters are the same:
// "A" = $41 = 065 = %01000001
// "«" = $AB = 171 = %10101011

signed char OstrCmp(char* string1, char* string2, long max=ALL);

signed char OstrCmp(char* string1, char* string2, long max) {
	signed char sign;
	short order; char char1; long index;
	
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

/*** PE/EString_partial ***/
/* PE/EString.e 27-10-07
   A re-implementation of AmigaE's E-string functions.
   
   By Christopher S Handley:
   Mostly completed 19-03-02, started 10-03-02.
   Ported to PortablE from 11-07-06 to 14-07-06.
   Put in the Public Domain on 14-07-06.
   Updated to use the STRING type on 27-07-06.
   Fixed Next() & Link() bugs on 27-10-07.
*/ /* Emulated procedures:
NewString(maxLen) RETURNS eString:STRING
DisposeString(eString:STRING) RETURNS NILS
StrCopy( eString:STRING, string:ARRAY OF CHAR, len=ALL, pos=0) RETURNS eString:STRING
StrAdd(  eString:STRING, string:ARRAY OF CHAR, len=ALL, pos=0) RETURNS eString:STRING
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
	
	friend char* NewString(long maxLen);
	friend char* DisposeString(char* eString);
	friend char* StrCopy(char* eString, char* string, long len, long pos);
	friend char* StrAdd(char* eString, char* string, long len, long pos);
	friend long EstrLen(char* eString);
	friend long StrMax(char* eString);
	friend char* RightStr(char* eString, char* eString2, long n);
	friend char* MidStr(char* eString, char* string, long pos, long len);
	friend void SetStr(char* eString, long newLen);
	friend char* Link(char* complex, char* tail);
	friend char* Next(char* complex);
	friend char* Forward(char* complex, long num);
};
char* NewString(long maxLen);
char* DisposeString(char* eString);
char* StrCopy(char* eString, char* string, long len=ALL, long pos=0);
char* StrAdd(char* eString, char* string, long len=ALL, long pos=0);
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
	if( maxLen < 0 ) { Throw(QuadChara(0,'E','P','U'), "EString; NewString(); maxLen<0");}
	
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
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), "EString; StrCopy(); eString=NILS");}
	if(  (void*) string == NULLA ) { Throw(QuadChara(0,'E','P','U'), "EString; StrCopy(); string=NILA");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), "EString; StrCopy(); len<0");}
	if( pos < 0) { Throw(QuadChara(0,'E','P','U'), "EString; StrCopy(); pos<0");}
	
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
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), "EString; StrAdd(); eString=NILS");}
	if(  (void*) string == NULLA ) { Throw(QuadChara(0,'E','P','U'), "EString; StrAdd(); string=NILA");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), "EString; StrAdd(); len<0");}
	if( pos < 0) { Throw(QuadChara(0,'E','P','U'), "EString; StrAdd(); pos<0");}
	
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

long EstrLen(char* eString) {
	long len;
	pEString* pEString2=NULL;
	
	//use check
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), "EString; EstrLen(); eString=NILS");}
	
	//retrieve string header
	pEString2 = (pEString*) (eString - sizeof( pEString ));
	len = pEString2->length;
	return len;
} 

long StrMax(char* eString) {
	long max;
	pEString* pEString2=NULL;
	
	//use check
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), "EString; StrMax(); eString=NILS");}
	
	//retrieve string header
	pEString2 = (pEString*) (eString - sizeof( pEString ));
	max = pEString2->size - (long) 1;
	return max;
} 

char* RightStr(char* eString, char* eString2, long n) {
	pEString* pEString2=NULL;
	char* readString=NULL;
	
	//use check
	if( eString  == NULLS ) { Throw(QuadChara(0,'E','P','U'), "EString; RightStr(); eString=NILS");}
	if( eString2 == NULLS ) { Throw(QuadChara(0,'E','P','U'), "EString; RightStr(); eString2=NILS");}
	if( n < 0           ) { Throw(QuadChara(0,'E','P','U'), "EString; RightStr(); n<0");}
	
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
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), "EString; MidStr(); eString=NILS");}
	if(  (void*) string == NULLA ) { Throw(QuadChara(0,'E','P','U'), "EString; MidStr(); string=NILA");}
	if( pos < 0        ) { Throw(QuadChara(0,'E','P','U'), "EString; MidStr(); pos<0");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), "EString; MidStr(); len<0");}
	
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
	if( eString == NULLS ) { Throw(QuadChara(0,'E','P','U'), "EString; SetStr(); eString=NILS");}
	if( newLen < 0     ) { Throw(QuadChara(0,'E','P','U'), "EString; SetStr(); newLen<0");}
	
	//retrieve string header
	pEString2 = (pEString*) (eString - sizeof( pEString ));
	
	//additional use check
	if( newLen >= pEString2->size ) { Throw(QuadChara(0,'E','P','U'), "EString; SetStr(); newLen exceeds string size");}
	
	//set length
	pEString2->length = newLen;
	eString[newLen] = '\000';
	return ;
}

char* Link(char* complex, char* tail) {
	pEString* pEString2=NULL;
	
	//use check
	if( complex == NULLS ) { Throw(QuadChara(0,'E','P','U'), "EString; Link(); complex=NILS");}
	
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
	if (exception!=0) {throw Exception;} else {EMPTY;};
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
	if( num < 0        ) { Throw(QuadChara(0,'E','P','U'), "EString; Forward(); num<0");}
	
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
	if (exception!=0) {throw Exception;} else {EMPTY;};
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
	
	friend long* NewList(long maxLen);
	friend long* DisposeList(long* list);
	friend long* ListCopy(long* list, long* other, long len);
	friend long* ListAdd(long* list, long* other, long len);
	friend BOOLEAN ListCmp(long* list, long* other, long len);
	friend long ListMax(long* list);
	friend long ListLen(long* list);
	friend long ListItem(long* list, long index);
	friend void SetList(long* list, long newLen);
};
long* NewList(long maxLen);
long* DisposeList(long* list);
long* ListCopy(long* list, long* other, long len=ALL);
long* ListAdd(long* list, long* other, long len=ALL);
BOOLEAN ListCmp(long* list, long* other, long len=ALL);
long ListMax(long* list);
long ListLen(long* list);
long ListItem(long* list, long index);
void SetList(long* list, long newLen);


long* NewList(long maxLen) {
	long* list=NULL;
	pEList* pEList2=NULL;	
	long sizeOfEList;
	
	//use check
	if( maxLen < 0 ) { Throw(QuadChara(0,'E','P','U'), "EList; NewList(); maxLen<0");}
	
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
	if(  list == NULLL ) { Throw(QuadChara(0,'E','P','U'), "EList; ListCopy(); list=NILL");}
	if( other == NULLL ) { Throw(QuadChara(0,'E','P','U'), "EList; ListCopy(); other=NILL");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), "EList; ListCopy(); len<0");}
	
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
	if(  list == NULLL ) { Throw(QuadChara(0,'E','P','U'), "EList; ListAdd(); list=NILL");}
	if( other == NULLL ) { Throw(QuadChara(0,'E','P','U'), "EList; ListAdd(); other=NILL");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), "EList; ListAdd(); len<0");}
	
	//retrieve list header
	pEList2  =  (pEList*) ((char*) list - sizeof( pEList ));
	pEOther = (pEList*) ((char*) other - sizeof( pEList ));
	
	//calc end of list reading from & writing to
	maxReadIndex  = ( len==ALL )? pEOther->size : ( len<pEOther->size)? len: pEOther->size;
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
	if(  list == NULLL ) { Throw(QuadChara(0,'E','P','U'), "EList; ListCmp(); list=NILL");}
	if( other == NULLL ) { Throw(QuadChara(0,'E','P','U'), "EList; ListCmp(); other=NILL");}
	if( - (len < 0)  & - (len != ALL)  ) { Throw(QuadChara(0,'E','P','U'), "EList; ListCmp(); len<0");}
	
	//retrieve list header
	pEList2  =  (pEList*) ((char*) list - sizeof( pEList ));
	pEOther = (pEList*) ((char*) other - sizeof( pEList ));
	
	if( pEList2->length != pEOther->length) {
		match = FALSE;
	} else {
		//calc where should stop comparison
		maxIndex = ( pEList2->size<pEOther->size)? pEList2->size: pEOther->size;
		if( len != ALL ) { maxIndex = ( len<maxIndex)? len: maxIndex;}
		
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
	if( list == NULLL ) { Throw(QuadChara(0,'E','P','U'), "EList; ListMax(); list=NILL");}
	
	//retrieve list header
	pEList2 = (pEList*) ((char*) list - sizeof( pEList ));
	max = pEList2->size;
	return max;
} 

long ListLen(long* list) {
	long len;
	pEList* pEList2=NULL;
	
	//use check
	if( list == NULLL ) { Throw(QuadChara(0,'E','P','U'), "EList; ListLen(); list=NILL");}
	
	//retrieve list header
	pEList2 = (pEList*) ((char*) list - sizeof( pEList ));
	len = pEList2->length;
	return len;
} 

long ListItem(long* list, long index) {
	long value;
	pEList* pEList2=NULL;
	
	//use check
	if( list == NULLL ) { Throw(QuadChara(0,'E','P','U'), "EList; ListLen(); list=NILL");}
	
	//retrieve list header
	pEList2 = (pEList*) ((char*) list - sizeof( pEList ));
	
	//additional use check
	if( - (index < 0)  | - (index >= pEList2->length)  ) { Throw(QuadChara(0,'E','P','U'), "EList; ListLen(); index exceeds list bounds");}
	
	value = list[index];
	return value;
} 

void SetList(long* list, long newLen) {
	pEList* pEList2=NULL;
	
	//use check
	if( list == NULLL ) { Throw(QuadChara(0,'E','P','U'), "EList; SetList(); list=NILL");}
	if( newLen < 0  ) { Throw(QuadChara(0,'E','P','U'), "EList; SetList(); newLen<0");}
	
	//retrieve list header
	pEList2 = (pEList*) ((char*) list - sizeof( pEList ));
	
	//additional use check
	if( newLen > pEList2->size ) { Throw(QuadChara(0,'E','P','U'), "EList; SetList(); newLen exceeds list size");}
	
	//set length
	pEList2->length = newLen;
	return ;
}

/*** PE/CPP/EString ***/
/* PortablE target module that completes EStrings */ /* missing E-string functions */
char* StringF(char* eString, char* fmtString, long arg1=0, long arg2=0, long arg3=0, long arg4=0, long arg5=0, long arg6=0, long arg7=0, long arg8=0);
BOOLEAN ReadStr(void* fileHandle, char* eString);
char* RealF(char* eString, float value, signed char decimalPlaces=8);
char* appendDecimal(char* eString, long value);

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
	if (exception!=0) {throw Exception;} else {EMPTY;};
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
	SetStr(eString, strlen(eString));
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
		temp_ARRAY_OF_CHAR = "";
	} else { 
		temp_ARRAY_OF_CHAR = "-";
	}
	StrCopy(eString, temp_ARRAY_OF_CHAR );
	value = fabs(value);
	
	integer = floor(value);
	appendDecimal(eString, (long) integer);
	
	if( decimalPlaces > 0) {
		StrAdd(eString, ".");
		
		value = value - integer;
		do {
			nextDecimalPlaces = (signed char) (( decimalPlaces<9)? (long) decimalPlaces: (long) 9);
			decimalPlaces = (signed char) ((short) decimalPlaces - (short) nextDecimalPlaces );
			
			value = value * (float) Pow(10, nextDecimalPlaces);
			integer = floor(value);
			appendDecimal(eString, (long) integer);
			value = value - integer;
		} while(( decimalPlaces <= 0)==0);
	}
	return eString;
} 


char* appendDecimal(char* eString, long value) {
	BOOLEAN isNegative; char temp[12]; signed char pos; long digit;
	
	//use check
	if( (void*) eString == NULLA ) { Throw(QuadChara(0,'E','P','U'), "PE/CPP/EString; appendDecimal(); eString=NILA");}
	
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
	} else {
		do {
			//extract right-most digit then remove from value
			digit= Mod(value, 10);
			value = ret2 ;
			
			//write digit as character
			pos--;
			temp[pos] = (char) ((long) '0' + digit );
		} while(( value == 0)==0);
		
		//prepend minus sign if was negative
		if( isNegative) {
			pos--;
			temp[pos] = '-';
		}
	}
	
	//now append string representation to target E-string
	StrAdd(eString, temp, ALL, pos);
	return eString;
} 

/*** target/graphics/rpattr ***/
/* $VER: rpattr.h 39.2 (31.5.1993) */ 

#include <graphics/rpattr.h>

/*** target/graphics/display ***/
/* $VER: display.h 39.0 (21.8.1991) */ 

#include <graphics/display.h>

/*** target/graphics/collide ***/
/* $VER: collide.h 37.0 (7.1.1991) */ 

#include <graphics/collide.h>

/*** target/graphics/coerce ***/
/* $VER: coerce.h 39.3 (15.2.1993) */ 

#include <graphics/coerce.h>

/*** target/exec/strings ***/


const signed char NL=10;
      const signed char DEL=0x7F;
      const signed char BS=8;
      const signed char BELL=7;
      const signed char CR=13;
      const signed char LF=10;
      const BOOLEAN EOS=0;

/*** target/exec/initializers ***/
/* $VER: initializers.h 39.0 (15.10.1991) */ 

#include <exec/initializers.h>

/*** target/exec/errors ***/
/* $VER: errors.h 39.0 (15.10.1991) */ 

#include <exec/errors.h>

const BOOLEAN ERR_OPENDEVICE=-1	;//IOERR_OPENFAIL

/*** target/exec/alerts ***/
/* $VER: alerts.h 39.3 (12.5.1992) */ 

#include <exec/alerts.h>

/*** target/dos/dos_lib ***/


const signed char RESERVE=4;
      const signed char VSIZE=6;

/*** target/exec/types ***/
/* $Id: types.h,v 45.2 2001/03/12 17:51:53 heinz Exp $ */ 

#include <exec/types.h>

/*** target/datatypes/pictureclass ***/
/* placeholder module */ 

#include <datatypes/pictureclass.h>

/*** target/hardware/dmabits ***/
/* placeholder module */ 

#include <hardware/dmabits.h>

/*** target/hardware/custom ***/
/* placeholder module */ 

#include <hardware/custom.h>

/*** target/hardware/blit ***/
/* placeholder module */ 

#include <hardware/blit.h>

/*** target/graphics/sprite ***/
/* $VER: sprite.h 39.6 (16.6.1992) */ 

#include <graphics/sprite.h>

/*** target/graphics/gels ***/
/* $VER: gels.h 39.0 (21.8.1991) */ 

#include <graphics/gels.h>

/*** target/exec/avl ***/
/* $VER: avl.h 45.4 (27.2.2001) */ 

#include <exec/avl.h>

/*** target/exec/resident ***/
/* $VER: resident.h 39.0 (15.10.1991) */ 

#include <exec/resident.h>

/*** target/utility/tagitem ***/
/* $VER: tagitem.h 40.1 (19.7.1993) */ 

#include <utility/tagitem.h>

/*** target/exec/nodes ***/
/* $VER: nodes.h 39.0 (15.10.1991) */ 

#include <exec/nodes.h>

/*** target/graphics/gfx ***/
/* $VER: gfx.h 39.5 (19.3.1992) */ 

#include <graphics/gfx.h>

/*** target/intuition/icclass ***/
/* $VER: icclass.h 38.1 (11.11.1991) */ 

#include <intuition/icclass.h>

/*** target/graphics/videocontrol ***/
/* $VER: videocontrol.h 39.8 (31.5.1993) */ 

#include <graphics/videocontrol.h>

/*** target/utility/hooks ***/
/* $VER: hooks.h 39.2 (16.6.1993) */ 

#include <utility/hooks.h>

/*** target/graphics/graphint ***/
/* $VER: graphint.h 39.0 (23.9.1991) */ 

#include <graphics/graphint.h>

/*** target/exec/libraries ***/
/* $VER: libraries.h 39.2 (10.4.1992) */ 

#include <exec/libraries.h>

const signed char LIBF_EXP0CNT=0x10;

/*** target/exec/lists ***/
/* $VER: lists.h 39.0 (15.10.1991) */ 

#include <exec/lists.h>

/*** target/dos/var ***/
/* $VER: var.h 36.11 (2.6.1992) */ 

#include <dos/var.h>

/*** target/dos/rdargs ***/
/* $VER: rdargs.h 36.6 (12.7.1990) */ 

#include <dos/rdargs.h>

/*** target/exec/memory ***/
/* $VER: memory.h 39.3 (21.5.1992) */ 

#include <exec/memory.h>

/*** target/dos/datetime ***/
/* $VER: datetime.h 45.1 (17.12.2001) */ 

#include <dos/datetime.h>

/*** target/dos/dos ***/
/* $VER: dos.h 36.27 (5.4.1992) */ 

#include <dos/dos.h>
char* dosname;
void new_dos() ;
void new_dos()  {
	dosname = "dos.library";
	return ;
}

/*** target/graphics/scale ***/
/* $VER: scale.h 39.0 (21.8.1991) */ 

#include <graphics/scale.h>

/*** target/graphics/regions ***/
/* $VER: regions.h 39.0 (21.8.1991) */ 

#include <graphics/regions.h>

/*** target/dos/exall ***/
/* $VER: exall.h 36.6 (5.4.1992) */ 

#include <dos/exall.h>

/*** target/devices/keymap ***/
/* $VER: keymap.h 36.3 (13.4.1990) */ 

#include <devices/keymap.h>

/*** target/exec/interrupts ***/
/* $VER: interrupts.h 39.1 (18.9.1992) */ 

#include <exec/interrupts.h>

const long SF_SAR=0x8000;
const signed char SIH_QUEUES=5;
const short SF_SINT=0x2000;
const short SF_TQE=0x4000;

/*** target/exec/ports ***/
/* $VER: ports.h 39.0 (15.10.1991) */ 

#include <exec/ports.h>

const signed char MP_SOFTINT=16;

/*** target/dos/record ***/
/* $VER: record.h 36.5 (12.7.1990) */ 

#include <dos/record.h>

/*** target/dos/dosasl ***/
/* $VER: dosasl.h 36.16 (2.5.1991) */ 

#include <dos/dosasl.h>

/*** target/workbench/startup ***/
/* $VER: startup.h 36.3 (11.7.1990) */ 

#include <workbench/startup.h>

/*** target/exec/devices ***/
/* $VER: devices.h 39.0 (15.10.1991) */ 

#include <exec/devices.h>

/*** target/exec/tasks ***/
/* $VER: tasks.h 39.3 (18.9.1992) */ 

#include <exec/tasks.h>
class etask;

const signed char CHILD_NOTNEW=1;
const signed char CHILD_NOTFOUND=2;
const signed char CHILD_EXITED=3;
const signed char CHILD_ACTIVE=4;

const long SYS_TRAPALLOC=0x8000;
const long SYS_SIGALLOC=0xFFFF;

//no such object!
class etask: public object {
public:
	struct Message mn;
	struct Task* parent;
	long uniqueid;
	struct MinList children;
	short trapalloc;
	short trapable;
	long result1;
	long result2;
	struct MsgPort taskmsgport;
	
};

/*** target/exec/io ***/
/* $VER: io.h 39.0 (15.10.1991) */ 

#include <exec/io.h>

/*** target/exec/execbase ***/
/* $VER: execbase.h 39.6 (18.1.1993) */ 

#include <exec/execbase.h>

/*** target/exec/semaphores ***/
/* $VER: semaphores.h 39.1 (7.2.1992) */ 

#include <exec/semaphores.h>

const signed char SM_LOCKMSG=16;

/*** target/dos/notify ***/
/* $VER: notify.h 36.8 (29.8.1990) */ 

#include <dos/notify.h>

/*** target/devices/timer ***/
/* $VER: timer.h 36.16 (25.1.1991) */ 

#include <devices/timer.h>
char* timername;
void new_timer() ;
void new_timer()  {
	timername = "timer.device";
	return ;
}

/*** target/exec ***/
/* C++ module, for $VER: exec_protos.h 45.2 (6.6.1998) */ 


#include <proto/exec.h>

struct ExecBase* SysBase = NULL;


/*** target/intuition/preferences ***/
/* $VER: preferences.h 38.2 (16.9.1992) */ 

#include <intuition/preferences.h>

/*** target/dos/dosextens ***/
/* $VER: dosextens.h 36.41 (14.5.1992) */ 

#include <dos/dosextens.h>

/*** target/graphics/gfxnodes ***/
/* $VER: gfxnodes.h 39.0 (21.8.1991) */ 

#include <graphics/gfxnodes.h>

/*** target/graphics/copper ***/
/* $VER: copper.h 39.10 (31.5.1993) */ 

#include <graphics/copper.h>

/*** target/graphics/text ***/
/* $VER: text.h 39.0 (21.8.1991) */ 

#include <graphics/text.h>

/*** target/graphics/layers ***/
/* $VER: layers.h 39.4 (14.4.1992) */ 

#include <graphics/layers.h>

/*** target/graphics/clip ***/
/* $VER: clip.h 39.0 (2.12.1991) */ 

#include <graphics/clip.h>

/*** target/graphics/rastport ***/
/* $VER: rastport.h 39.0 (21.8.1991) */ 

#include <graphics/rastport.h>

/*** PE/exec ***/




void* NewM(long size, long flags);

void* NewM(long size, long flags) {
	void* mem=NULL;
	
	if( flags & (long) ((short) (MEMF_CHIP | MEMF_FAST | MEMF_PUBLIC )| MEMF_LOCAL | MEMF_24BITDMA)) {
		printf("NewM() emulation was passed an unsupported flag\n",0,0,0,0,0,0,0,0);
		Raise(QuadChara(0,'M','E','M'));
	}
	
	mem = memset(malloc(size), 0,size);
	if( mem == NULL ) { Raise(QuadChara(0,'M','E','M'));}
	return mem;
} 

/*** target/dos ***/
/* C++ module, for $VER: dos_protos.h 40.2 (6.6.1998) */ 


#include <proto/dos.h>

struct DosLibrary* DOSBase = NULL;

void new_dos2();
void end_dos();

//automatic opening of dos library
void new_dos2() {
	DOSBase = (struct DosLibrary*) (long) OpenLibrary("dos.library",(ULONG) 39);
	if( (void*) DOSBase==NULL ) { CleanUp(RETURN_ERROR);}
	return ;
}

//automatic closing of dos library
void end_dos() {
	CloseLibrary((struct Library*) DOSBase);
	return ;
}

/*** target/graphics/monitor ***/
/* $VER: monitor.h 39.7 (9.6.1992) */ 

#include <graphics/monitor.h>
char* default_monitor_name;
char* ntsc_monitor_name;
char* pal_monitor_name;
char* vga_monitor_name;
char* vga70_monitor_name;
void new_monitor() ;
void new_monitor()  {
	default_monitor_name = "default.monitor";
	ntsc_monitor_name = "ntsc.monitor";
	pal_monitor_name = "pal.monitor";
	vga_monitor_name = "vga.monitor";
	vga70_monitor_name = "vga70.monitor";
	return ;
}

/*** PE/CPP/exec ***/





long FreeStack() ;
long StackSize();

long FreeStack()  {
	long bytes;
	struct Task* task=NULL; long size;
	task = FindTask((char*) NULLA);
	size = (long) ((char*) (void*) task->tc_SPUpper - (long) task->tc_SPLower);
	
	bytes = (long) ((char*) (&bytes )- (long) task->tc_SPLower);
	if( - (bytes < 0)  | - (bytes > size)) {
		bytes = (long) ((char*) (void*) task->tc_SPReg - (long) task->tc_SPLower);
		if( - (bytes < 0)  | - (bytes > size)) {
			bytes = size;
		}
	}
	return bytes;
} 

long StackSize() {
	long bytes;
	struct Task* task=NULL;
	task = FindTask((char*) NULLA);
	bytes = (long) ((char*) (void*) task->tc_SPUpper - (long) task->tc_SPLower);
	return bytes;
} 

/*** PE/CPP/dos ***/




char* arg;


char* argString=NULLS;
void new_dos3();
void end_dos2();


void new_dos3() {
	char* args=NULL; long len;
	
	if( args = GetArgStr()) {
		len = strlen(args);
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

/*** target/graphics/displayinfo ***/
/* $VER: displayinfo.h 39.13 (31.5.1993) */ 

#include <graphics/displayinfo.h>

/*** target/graphics/view ***/
/* $VER: view.h 39.34 (31.5.1993) */ 

#include <graphics/view.h>

/*** target/graphics/modeid ***/
/* $VER: modeid.h 39.9 (27.5.1993) */ 

#include <graphics/modeid.h>

/*** target/graphics/gfxbase ***/
/* $VER: gfxbase.h 39.21 (21.4.1993) */ 

#include <graphics/gfxbase.h>
char* graphicsname;
void new_gfxbase() ;
void new_gfxbase()  {
	graphicsname = "graphics.library";
	return ;
}

/*** target/intuition/screens ***/
/* $VER: screens.h 38.25 (15.2.1993) */ 

#include <intuition/screens.h>

/*** target/graphics ***/
/* $VER: graphics_protos.h 40.2 (6.6.1998) */ 


#include <proto/graphics.h>

struct GfxBase* GfxBase = NULL;

void new_graphics();
void end_graphics();

//automatic opening of gfx library
void new_graphics() {
	GfxBase = (struct GfxBase*) (long) OpenLibrary("graphics.library",(ULONG) 39);
	if( (void*) GfxBase==NULL ) { CleanUp(RETURN_ERROR);}
	return ;
}

//automatic closing of gfx library
void end_graphics() {
	CloseLibrary((struct Library*) GfxBase);
	return ;
}

/*** target/devices/inputevent ***/
/* $VER: inputevent.h 36.10 (26.6.1992) */ 

#include <devices/inputevent.h>

/*** PE/CPP/graphics ***/





struct RastPort* stdrast=NULL;
void Plot(long x2, long y2, long colour=1);
void Line(long x1, long y1, long x2, long y2, long colour=1);
void Box(long x1, long y1, long x2, long y2, long colour=1);
void Colour(long foreground, long background=0);
long TextF(long x2, long y2, char* fmtString, long arg1=0, long arg2=0, long arg3=0, long arg4=0, long arg5=0, long arg6=0, long arg7=0, long arg8=0);
struct RastPort* SetStdRast(struct RastPort* rast);
void SetTopaz(short size=8);

void Plot(long x2, long y2, long colour) {
	if( stdrast) {
		SetAPen(stdrast,(ULONG) colour);
		WritePixel(stdrast,x2,y2);
	}
	return ;
}

void Line(long x1, long y1, long x2, long y2, long colour) {
	if( stdrast) {
		SetAPen(stdrast,(ULONG) colour);
		Move(stdrast,(short) x1 ,(short) y1 );
		Draw(stdrast,x2,y2);
	}
	return ;
}

void Box(long x1, long y1, long x2, long y2, long colour) {
	long xmin, ymin, xmax, ymax;
	if( stdrast) {
		SetAPen(stdrast,(ULONG) colour);
		xmin = ( x1<x2)? x1: x2;
		xmax = ( x1>x2)? x1: x2;
		ymin = ( y1<y2)? y1: y2;
		ymax = ( y1>y2)? y1: y2;
		RectFill(stdrast,xmin,ymin,xmax,ymax);
	}
	return ;
}

void Colour(long foreground, long background) {
	if( stdrast) {
		SetAPen(stdrast,(ULONG) foreground);
		SetBPen(stdrast,(ULONG) background);
	}
	return ;
}

long TextF(long x2, long y2, char* fmtString, long arg1, long arg2, long arg3, long arg4, long arg5, long arg6, long arg7, long arg8) {
	long length;
	char* string=NULL;
	
	if( stdrast) {
		string= NewString(strlen(fmtString)*(long) 2 + (long) 100 );
		StringF(string, fmtString, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
		
		length = EstrLen(string);
		Move(stdrast,(short) x2 ,(short) y2 );
		Text(stdrast,string,(ULONG) length);
		
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
	temp_ARRAY_OF_textattr = (struct TextAttr*) calloc(1,sizeof( struct TextAttr));
	temp_ARRAY_OF_textattr [0].ta_Name = "topaz.font";
	temp_ARRAY_OF_textattr [0].ta_Style = (UBYTE) FS_NORMAL;
	temp_ARRAY_OF_textattr [0].ta_Flags = (UBYTE) (FPF_PROPORTIONAL | FPF_DESIGNED);
	temp_QUAD = exception ;
	exception = 0 ;
	
	if( stdrast) {
		temp_ARRAY_OF_textattr [0].ta_YSize = (UWORD) (long) size;
		font = OpenFont((struct TextAttr*) temp_ARRAY_OF_textattr );
		SetFont(stdrast,font);
		//CloseFont(font)
	}
} catch(...) {}
	free(temp_ARRAY_OF_textattr );
	NULLA;
	if (exception!=0) {throw Exception;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;
}

/*** target/graphics/gfxmacros ***/
/* $VER: gfxmacros.h 39.3 (31.5.1993) */ 

#include <graphics/gfxmacros.h>

/*** target/intuition/intuition ***/
/* $VER: intuition.h 38.26 (15.2.1993) */ 

#include <intuition/intuition.h>

/*** target/workbench/workbench ***/
/* $VER: workbench.h 45.6 (23.11.2000) */ 

#include <workbench/workbench.h>
char* workbench_name;
void new_workbench() ;
void new_workbench()  {
	workbench_name = "workbench.library";
	return ;
}

/*** target/intuition/pointerclass ***/
/* $VER: pointerclass.h 39.6 (15.2.1993) */ 

#include <intuition/pointerclass.h>

/*** target/intuition/iobsolete ***/
/* $VER: iobsolete.h 38.1 (22.1.1992) */ 

#include <intuition/iobsolete.h>

/*** target/intuition/intuitionbase ***/
/* $VER: intuitionbase.h 38.0 (12.6.1991) */ 

#include <intuition/intuitionbase.h>

/*** target/intuition/cghooks ***/
/* $VER: cghooks.h 38.1 (11.11.1991) */ 

#include <intuition/cghooks.h>

/*** target/intuition/sghooks ***/
/* $VER: sghooks.h 38.1 (11.11.1991) */ 

#include <intuition/sghooks.h>

/*** target/intuition/imageclass ***/
/* $VER: imageclass.h 44.1 (19.10.1999) */ 

#include <intuition/imageclass.h>
class impdrawframe;
class imperaseframe;
class imphitframe;


class impdrawframe: public impDraw {
	
	friend class imperaseframe;
	friend class imphitframe;
};
class imperaseframe: public impErase {
	
	friend class impdrawframe;
	friend class imphitframe;
};
class imphitframe: public impHitTest {
	
	friend class impdrawframe;
	friend class imperaseframe;
};

/*** target/intuition/gadgetclass ***/
/* $VER: gadgetclass.h 44.1 (19.10.1999) */ 

#include <intuition/gadgetclass.h>
class gpgoactive;
class gphelptest;


class gpgoactive: public gpInput {
	
	friend class gphelptest;
};
class gphelptest: public gpHitTest {
	
	friend class gpgoactive;
};

/*** target/intuition/classusr ***/
/* $VER: classusr.h 38.2 (14.4.1992) */ 

#include <intuition/classusr.h>
class opnew;
class opnotify;
class opaddmember;
class opremmember;


class opnew: public opSet {
	
	friend class opnotify;
	friend class opaddmember;
	friend class opremmember;
	friend void new_classusr() ;
};
class opnotify: public opUpdate {
	
	friend class opnew;
	friend class opaddmember;
	friend class opremmember;
	friend void new_classusr() ;
};
class opaddmember: public opMember {
	
	friend class opnew;
	friend class opnotify;
	friend class opremmember;
	friend void new_classusr() ;
};
class opremmember: public opMember {
	
	friend class opnew;
	friend class opnotify;
	friend class opaddmember;
	friend void new_classusr() ;
};
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
void new_classusr() ;
void new_classusr()  {
	rootclass = "rootclass";
	imageclass = "imageclass";
	frameiclass = "frameiclass";
	sysiclass = "sysiclass";
	fillrectclass = "fillrectclass";
	gadgetclass = "gadgetclass";
	propgclass = "propgclass";
	strgclass = "strgclass";
	buttongclass = "buttongclass";
	frbuttonclass = "frbuttonclass";
	groupgclass = "groupgclass";
	icclass = "icclass";
	modelclass = "modelclass";
	itexticlass = "itexticlass";
	pointerclass = "pointerclass";
	return ;
}

/*** target/workbench/icon ***/
/* $VER: icon.h 45.1 (2.8.2001) */ 

#include <workbench/icon.h>
char* iconname;
void new_icon() ;
void new_icon()  {
	iconname = "icon.library";
	return ;
}

/*** target/intuition/classes ***/
/* $VER: classes.h 40.0 (15.2.1994) */ 

#include <intuition/classes.h>

/*** target/wb ***/
/* $VER: wb_protos.h 44.5 (21.6.1999) */ 


#include <proto/wb.h>

struct Library* WorkbenchBase = NULL;


/*** target/intuition ***/
/* $VER: intuition_protos.h 40.1 (17.5.1996) */ 


#include <proto/intuition.h>

struct IntuitionBase* IntuitionBase = NULL;

void new_intuition();
void end_intuition();

//automatic opening of intuition library
void new_intuition() {
	IntuitionBase = (struct IntuitionBase*) (long) OpenLibrary("intuition.library",(ULONG) 39);
	if( (void*) IntuitionBase==NULL ) { CleanUp(RETURN_ERROR);}
	return ;
}

//automatic closing of intuition library
void end_intuition() {
	CloseLibrary((struct Library*) IntuitionBase);
	return ;
}

/*** PE/CPP/wb ***/





struct WBStartup* wbmessage=NULL;
void new_wb();


void new_wb() {
	if( main_argc == 0) {
		wbmessage = (struct WBStartup*) main_argv ;
	}
	return ;
}

/*** PE/CPP/intuition ***/





const signed char GADGETSIZE=120;



long code; long qual; APTR iaddr;
struct TextAttr* tempGlobal_ARRAY_OF_textattr; struct TextAttr* tempGlobal_ARRAY_OF_textattr2;
void SetColour(struct Screen* screen, UBYTE colourreg, UBYTE r, UBYTE g, UBYTE b2);
struct Window* OpenW(short x2, short y2, short width, short height, long idcmp, long wflags, char* title, struct Screen* screen, short sflags, struct Gadget* gadgets, struct TagItem* taglist=NULLA);
void CloseW(struct Window* wptr);
struct Screen* OpenS(short width, short height, short depth, short sflags, char* title, struct TagItem* taglist=NULLA);
void CloseS(struct Screen* sptr);
void* Gadget_intuition(void* buffer, void* glist, short id, long flags, short x2, short y2, short width, char* string);
long Mouse();
BOOLEAN LeftMouse(struct Window* win);
void WaitLeftMouse(struct Window* win);
long WaitIMessage(struct Window* win);
void new_intuition2() ;
void end_intuition2() ;

void SetColour(struct Screen* screen, UBYTE colourreg, UBYTE r, UBYTE g, UBYTE b2) {
	SetRGB32(& screen->ViewPort,(ULONG) (long) colourreg,(ULONG) (((long) r)<<(32-8)),(ULONG) (((long) g)<<(32-8)),(ULONG) (((long) b2)<<(32-8)));
	return ;
	//or SetRGB4(screen.viewport, colourreg, r, g, b)
}


struct Window* OpenW(short x2, short y2, short width, short height, long idcmp, long wflags, char* title, struct Screen* screen, short sflags, struct Gadget* gadgets, struct TagItem* taglist) {
	struct Window* wptr=NULL;
	struct NewWindow* temp_ARRAY_OF_nw=NULL; long temp_QUAD;
try {
	temp_ARRAY_OF_nw = (struct NewWindow*) calloc(1,sizeof( struct NewWindow));
	temp_ARRAY_OF_nw [0].DetailPen = (UBYTE) (long) 0xFF ;
	temp_ARRAY_OF_nw [0].BlockPen = (UBYTE) (long) 0xFF ;
	temp_ARRAY_OF_nw [0].CheckMark = (struct Image*) NULL;
	temp_ARRAY_OF_nw [0].BitMap = (struct BitMap*) NULL;
	temp_ARRAY_OF_nw [0].MinWidth = 0;
	temp_ARRAY_OF_nw [0].MinHeight = 0;
	temp_ARRAY_OF_nw [0].MaxWidth = (UWORD) 0;
	temp_ARRAY_OF_nw [0].MaxHeight = (UWORD) 0;
	temp_QUAD = exception ;
	exception = 0 ;
	temp_ARRAY_OF_nw [0].LeftEdge = x2;
	temp_ARRAY_OF_nw [0].TopEdge = y2;
	temp_ARRAY_OF_nw [0].Width = width;
	temp_ARRAY_OF_nw [0].Height = height;
	temp_ARRAY_OF_nw [0].IDCMPFlags = (ULONG) idcmp;
	temp_ARRAY_OF_nw [0].Flags = (ULONG) wflags;
	temp_ARRAY_OF_nw [0].FirstGadget = gadgets;
	temp_ARRAY_OF_nw [0].Title = (UBYTE*) title;
	temp_ARRAY_OF_nw [0].Screen = screen;
	temp_ARRAY_OF_nw [0].Type = (UWORD) (long) sflags;
	wptr = OpenWindowTagList((struct NewWindow*) temp_ARRAY_OF_nw ,(struct TagItem*) taglist);
	stdrast = wptr->RPort;
} catch(...) {}
	free(temp_ARRAY_OF_nw );
	NULLA;
	if (exception!=0) {throw Exception;} else {EMPTY;};
	exception = temp_QUAD ;
	return wptr;
} 

void CloseW(struct Window* wptr) {
	if( wptr) {
		if( wptr->RPort == stdrast ) { stdrast = (struct RastPort*) NULL;}
		CloseWindow(wptr);
	}
	return ;
}

struct Screen* OpenS(short width, short height, short depth, short sflags, char* title, struct TagItem* taglist) {
	struct Screen* sptr=NULL;
	struct NewScreen* temp_ARRAY_OF_ns=NULL; long temp_QUAD;
try {
	temp_ARRAY_OF_ns = (struct NewScreen*) calloc(1,sizeof( struct NewScreen));
	temp_ARRAY_OF_ns [0].LeftEdge = 0;
	temp_ARRAY_OF_ns [0].TopEdge = 0;
	temp_ARRAY_OF_ns [0].DetailPen = (UBYTE) DETAILPEN;
	temp_ARRAY_OF_ns [0].BlockPen = (UBYTE) BLOCKPEN;
	temp_ARRAY_OF_ns [0].Type = (UWORD) (CUSTOMSCREEN | SHOWTITLE);
	temp_ARRAY_OF_ns [0].Gadgets = (struct Gadget*) NULL;
	temp_ARRAY_OF_ns [0].CustomBitMap = (struct BitMap*) NULL;
	temp_QUAD = exception ;
	exception = 0 ;
	temp_ARRAY_OF_ns [0].Width = width;
	temp_ARRAY_OF_ns [0].Height = height;
	temp_ARRAY_OF_ns [0].Depth = depth;
	temp_ARRAY_OF_ns [0].ViewModes = (UWORD) (long) sflags;
	temp_ARRAY_OF_ns [0].Font = (struct TextAttr*) tempGlobal_ARRAY_OF_textattr ;
	temp_ARRAY_OF_ns [0].DefaultTitle = (UBYTE*) title;
	sptr = OpenScreenTagList((struct NewScreen*) temp_ARRAY_OF_ns ,(struct TagItem*) taglist);
	stdrast = & sptr->RastPort;
} catch(...) {}
	free(temp_ARRAY_OF_ns );
	NULLA;
	if (exception!=0) {throw Exception;} else {EMPTY;};
	exception = temp_QUAD ;
	return sptr;
} 

void CloseS(struct Screen* sptr) {
	if( sptr) {
		if( & sptr->RastPort == stdrast ) { stdrast = (struct RastPort*) NULL;}
		-CloseScreen(sptr);
	}
	return ;
}

void* Gadget_intuition(void* buffer, void* glist, short id, long flags, short x2, short y2, short width, char* string) {
	void* nextbuffer=NULL;
	struct Gadget* gadget=NULL; struct Border* border=NULL; struct IntuiText* intuitext=NULL; short* borderxy=NULL;
	short height; struct Gadget* firstgadget=NULL;
	signed char* zero=NULL; long i2;
	
	nextbuffer = buffer;
	height = 12;
	
	//clear memory (probably not necessary, but does no harm)
	zero = (signed char*) nextbuffer;
	for( i2 = 0 ; i2 <=sizeof( struct Gadget )+ sizeof( struct Border )+ sizeof( struct IntuiText )- (long) 1; i2 ++) {
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
	intuitext->LeftEdge = (short) (((long) width - (long) 8 * strlen(string)) / (long) 2 );
	intuitext->TopEdge  = 2;
	intuitext->ITextFont= (struct TextAttr*) tempGlobal_ARRAY_OF_textattr2 ;
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
	if( glist) {
		firstgadget = (struct Gadget*) glist;
		gadget->NextGadget      = firstgadget->NextGadget;
		firstgadget->NextGadget = gadget;
	}
	
	if( (long) ((char*) nextbuffer - (long) buffer) > (long) GADGETSIZE ) { Throw(QuadChara(0,'B','U','G'), "Gadget(); GADGETSIZE is too small");}
	nextbuffer = (void*) ((char*) buffer + GADGETSIZE);
	return nextbuffer;
} 

long Mouse() {
	long code2;
	code2 = 0;
	if( (*(signed char*) 0xBFE001 )&   64 ) { code2 = code2 | (long) 1;}
	if( (*(short*) 0xDFF016 )& 1024 ) { code2 = code2 | (long) 2;}
	if( (*(short*) 0xDFF016 )&  256 ) { code2 = code2 | (long) 4;}
	return code2;
} 

BOOLEAN LeftMouse(struct Window* win) {
	BOOLEAN button;
	struct IntuiMessage* msg=NULL;
	
	-ModifyIDCMP(win,(ULONG) IDCMP_MOUSEBUTTONS);
	msg = (struct IntuiMessage*) GetMsg(win->UserPort);
	
	if( msg) {
		button = - ((long) msg->Class == (long) IDCMP_MOUSEBUTTONS)  & - ((short) (long) msg->Code == (short) SELECTDOWN);
		ReplyMsg(& msg->ExecMessage);
	} else {
		button = FALSE;
	}
	return button;
} 

void WaitLeftMouse(struct Window* win) {
	struct IntuiMessage* msg=NULL; long class2, code2;
	
	-ModifyIDCMP(win,(ULONG) IDCMP_MOUSEBUTTONS);
	msg = (struct IntuiMessage*) GetMsg(win->UserPort);
	do {
		if( (void*) msg == NULL) {
			WaitPort(win->UserPort);
			msg = (struct IntuiMessage*) GetMsg(win->UserPort);
		}
		
		class2 = (long) msg->Class;
		code2  = (long) msg->Code;
		ReplyMsg(& msg->ExecMessage);
		msg = (struct IntuiMessage*) NULL;
	} while(( - (class2 == IDCMP_MOUSEBUTTONS)  & - (code2 == SELECTDOWN))==0);
	return ;
}


long WaitIMessage(struct Window* win) {
	long class2;
	struct MsgPort* port=NULL; struct IntuiMessage* mes=NULL;
	
	port = win->UserPort;
	while( (void*) (mes = (struct IntuiMessage*) GetMsg(port)) == NULL) {
		WaitPort(port);
	}
	
	class2 = (long) mes->Class;
	code  = (long) mes->Code;
	qual  = (long) mes->Qualifier;
	iaddr = mes->IAddress;
	ReplyMsg(& mes->ExecMessage);
	return class2;
} 
void new_intuition2()  {
	tempGlobal_ARRAY_OF_textattr = (struct TextAttr*) calloc(1,sizeof( struct TextAttr));
	tempGlobal_ARRAY_OF_textattr [0].ta_Name = "topaz.font";
	tempGlobal_ARRAY_OF_textattr [0].ta_YSize = (UWORD) 8;
	tempGlobal_ARRAY_OF_textattr [0].ta_Style = (UBYTE) FS_NORMAL;
	tempGlobal_ARRAY_OF_textattr [0].ta_Flags = (UBYTE) (FPF_PROPORTIONAL | FPF_DESIGNED);
	tempGlobal_ARRAY_OF_textattr2 = (struct TextAttr*) calloc(1,sizeof( struct TextAttr));
	tempGlobal_ARRAY_OF_textattr2 [0].ta_Name = "topaz.font";
	tempGlobal_ARRAY_OF_textattr2 [0].ta_YSize = (UWORD) 8;
	tempGlobal_ARRAY_OF_textattr2 [0].ta_Style = (UBYTE) FS_NORMAL;
	tempGlobal_ARRAY_OF_textattr2 [0].ta_Flags = (UBYTE) (FPF_PROPORTIONAL | FPF_DESIGNED);
	return ;
}
void end_intuition2()  {
	free(tempGlobal_ARRAY_OF_textattr2 );
	NULLA;
	free(tempGlobal_ARRAY_OF_textattr );
	NULLA;
	return ;
}

/*** PE/compatibility ***/
/* PE/compatibility.e 28-06-08
   The module is used by OPT AMIGAE.
*/ 
void WriteF(char* fmtString, long arg1=0, long arg2=0, long arg3=0, long arg4=0, long arg5=0, long arg6=0, long arg7=0, long arg8=0);

void WriteF(char* fmtString, long arg1, long arg2, long arg3, long arg4, long arg5, long arg6, long arg7, long arg8) {
	printf(fmtString,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
	fflush(stdout);
	return ;
}

/*** Data:/Mine/Doc/Comp/Code/PE/PortablE/TestECode/ChrisYoung/ppmtoscr ***/
/* 3.3 changes: Added "NOHEADER" switch */ 

const BOOLEAN ERR_NONE=0 ; const signed char ERR_FILE=1 ; const signed char ERR_DTYP=2 ; const signed char ERR_NPIC=3 ; const signed char ERR_MEM=4 ; const signed char ERR_BTMP=5 ; const signed char ERR_LIB=6 ; const signed char ERR_ABOR=7 ; const signed char ERR_ASL=8 ; const signed char ERR_ICON=9 ; const signed char ERR_ARGS=10 ; const signed char ERR_GTLS=11 ; const signed char ERR_PPM=12 ;

long fhhead; long fhand; long fhandin; long fhid; char* scrfile; long okay; char* headerfile;
long ftypeid;  /* :STRING[6144] */  /* [147456]:STRING */
long cm; long os3=FALSE;
long h; long z; long x; long y; long b; long l; long k; long j; long i; long m; long col; long bin; long blk; long a; long attr; long red; long grn; long blu; long dot;
short ink[769]; short paper[769]; short best[9];
char* fname;
char* verstring; long saveformat=0; long checksum; char* ppmmem;
char headerbytes[23]; char zx82header[12]; long headcheck;
long thr=20; long smooth=1; char* endmsg; long rompal=0;
long templ; long rdargs; long* rargs=NULL;
long bright; short brightattr[769]; long availpix; long brthr=450; long wtthr=200;
long quiet=0;
char* wintit;
long flashred=-1; long flashgrn=-1; long flashblu=-1; short flash[769];
long grnmag=0; long blucyn=20; long redyel=40; long thr3; long thr4; char* filetypeerror;
char* filename; long writeheader=1;
long* tempGlobal_ILIST;
int main(int argc, char** argv) ;
void ppmtoscr();
long mapcol(long offset, long binary);
void stdchoosecol() /*(red,grn,blu)*/;
void os3setup();
void choosecol();
void new_ppmtoscr() ;
void end_ppmtoscr() ;

int main(int argc, char** argv)  {
	char* temp_ARRAY_OF_CHAR=NULL;

// IF ppmmem THEN FreeMem(ppmmem,147471)

main_argc = argc;

// IF ppmmem THEN FreeMem(ppmmem,147471)

main_argv = argv;

// IF ppmmem THEN FreeMem(ppmmem,147471)

try {
	new_base();
	new_base2();
	new_dos();
	new_timer();
	new_dos2();
	new_monitor();
	new_dos3();
	new_gfxbase();
	new_graphics();
	new_workbench();
	new_classusr();
	new_icon();
	new_intuition();
	new_wb();
	new_intuition2();
	new_ppmtoscr();

StrCopy(verstring,"$VER: ppmtoscr 3.5 (28.06.2008)");

rargs=tempGlobal_ILIST ;
templ=(long) "PPMFILE/A,SCRFILE/A,FORM=SAVEFORMAT/K,COL=COLOURSENSE/K/N,BRT=BRIGHTSENSE/K/N,WHT=WHITESENSE/K/N,GRNMAG=GREENMAGENTA/K/N,BLUCYN=BLUECYAN/K/N,REDYEL=REDYELLOW/K/N,RED=FLASHRED/K/N,GRN=FLASHGREEN/K/N,BLU=FLASHBLUE/K/N,GREYSCALE/S,NOBRIGHT/S,NOSMOOTH/S,OS=ROMREMAP/S,ALTPAL=ALTROMPALETTE/S,NOHEADER/S,QUIET/S";
rdargs=(long) ReadArgs((char*) templ,rargs,(struct RDArgs*) NULL);

// WriteF('\s\n',rargs)

	if( (ppmmem=(char*) AllocMem((ULONG) 147471,(ULONG) MEMF_CLEAR))==NULL ) { Raise(ERR_MEM);}
	/* Note: 147456 = 256*192*3 bytes (enuff room for PPM sans header) */

 if( rdargs) {

if( rargs[0] ) { StrCopy(filename,(char*) rargs[0]);}
if( rargs[1] ) { StrCopy(scrfile,(char*) rargs[1]);}
if( rargs[2]) {
	if( (char*) rargs[2]) {
	temp_ARRAY_OF_CHAR = UpperStr((char*) rargs[2]) ;
	} else { 
	temp_ARRAY_OF_CHAR = (char*) rargs[2];
	}
	temp_ARRAY_OF_CHAR ;
	if( StrCmp((char*) rargs[2],"SCR") ) { saveformat=0;}
	if( StrCmp((char*) rargs[2],"ZX82") ) { saveformat=1;}
	if( StrCmp((char*) rargs[2],"BYTES") ) { saveformat=2;}
	if( StrCmp((char*) rargs[2],"TAP") ) { saveformat=3;}
	if( StrCmp((char*) rargs[2],"TZX") ) { saveformat=4;}
}
if( rargs[3] ) { thr=(*(long*) rargs[3]);}
if( rargs[4] ) { brthr=(*(long*) rargs[4]);}
if( rargs[5] ) { wtthr=(*(long*) rargs[5]);}

if( rargs[6] ) { grnmag=(*(long*) rargs[6]);}
if( rargs[7] ) { blucyn=(*(long*) rargs[7]);}
if( rargs[8] ) { redyel=(*(long*) rargs[8]);}

if( rargs[9] ) { flashred=(*(long*) rargs[9]);}
if( rargs[10] ) { flashgrn=(*(long*) rargs[10]);}
if( rargs[11] ) { flashblu=(*(long*) rargs[11]);}
if( rargs[12]==TRUE ) { thr=5000;}
if( rargs[13]==TRUE ) { brthr=5000;}
if( rargs[14]==TRUE ) { smooth=0;}
if( rargs[15]==TRUE ) { os3=TRUE;}
if( rargs[16]==TRUE ) { rompal=TRUE;}
if( rargs[17]==TRUE ) { writeheader=0;}
if( rargs[18]==TRUE ) { quiet=TRUE;}
FreeArgs((struct RDArgs*) rdargs);

} else {
  Raise(ERR_ARGS);
}

if( quiet==0) {
  WriteF("ppmtoscr 3.5\nby Chris Young <chris@unsatisfactorysoftware.co.uk>\n\251 1998-9 Unsatisfactory Software\n\n");
if( os3!=FALSE ) { WriteF("You are set to run in OS3 mode...\n\n");}
}

os3setup();
ppmtoscr();

// IF ppmmem THEN FreeMem(ppmmem,147471)

} catch(...) {}

	if( exception) {
//	  IF quiet=0 THEN WriteF('*** ERROR ***\n')
	
	switch( exception) {
	case ERR_FILE : StrCopy(endmsg,"Couldn\'t open file!");
		break;
	case ERR_DTYP : StrCopy(endmsg,"Datatypes error!");
		break;
	case ERR_NPIC : StrCopy(endmsg,filetypeerror);
		break;
	case ERR_MEM  : StrCopy(endmsg,"Not enough memory!");
		break;
	case ERR_BTMP : StrCopy(endmsg,"Could not allocate BitMap!");
		break;
	case ERR_LIB  : StrCopy(endmsg,"Could not open datatypes.library v39+");
		break;
     case ERR_ABOR : StrCopy(endmsg,"Aborted by user!");
     	break;
     case ERR_ASL  : StrCopy(endmsg,"Could not open asl.library 37+");
     	break;
	case ERR_ICON : StrCopy(endmsg,"Could not open icon.library 33+");
		break;
	case ERR_GTLS : StrCopy(endmsg,"Could not open gadtools.library 37+");
		break;
	case ERR_ARGS : StrCopy(endmsg,"required argument missing");
		break;
	case ERR_PPM  : StrCopy(endmsg,"Filetype not supported!  (Not a P6 PPM)");
		break;
	}

}
	if( ppmmem ) { FreeMem((APTR) (long) ppmmem,(ULONG) 147471);}

       if( quiet==0 ) { WriteF("%s\n",(long) endmsg);}
	end_ppmtoscr();
	end_intuition2();
	end_intuition();
	end_graphics();
	end_dos2();
	end_dos();
	if (exception==-1) return (int) exceptionInfo; //finish the CleanUp() call

}

void ppmtoscr() {
	char* fname2=NULL;

if( (ftypeid=(long) AllocMem((ULONG) 10,(ULONG) MEMF_CLEAR))==(long) NULL ) { Raise(ERR_MEM);}

if( fhid=(long) Open(filename,OLDFILE)) {
if( quiet==0 ) { WriteF("Reading input file...\n");}

okay=Read((BPTR) fhid,(APTR) ftypeid,4);

if( okay) {
 if( quiet==0 ) { WriteF("Read header OK\n");}
}

-Close((BPTR) fhid);
}

if( StrCmp((char*) ftypeid,"P6",2)) {
StrCopy(wintit,"Format: P6 (PPM RAWBITS)");
 if( quiet==0 ) { WriteF("%s\n\n",(long) wintit);}
} else {
 Raise(ERR_PPM);
}

if( ftypeid ) { FreeMem((APTR) ftypeid,(ULONG) 10);}

if( fhandin=(long) Open(filename,OLDFILE)) {

if( quiet==0 ) { WriteF("Reading PPM file...\n");}

okay=Read((BPTR) fhandin,(APTR) (long) ppmmem,147471);

if( okay) {
  if( quiet==0 ) { WriteF("Read data OK\n");}
}

-Close((BPTR) fhandin);
}

StrCopy(fname,scrfile);
fname2=FilePart(fname);
dot=InStr(fname2,".");
if( dot!=-1 ) { StrCopy(fname,fname2,dot);}
StrAdd(fname,"          ");

if( saveformat==0) {
 if( quiet==0 ) { WriteF("Save Format: Raw SCR\n");}
}

if( saveformat==2) {
   if( quiet==0 ) { WriteF("Save Format: .header/.bytes\n");}
   dot=InStr(scrfile,".");
if( dot==(long) NULL ) { StrCopy(headerfile,scrfile) ;} else { StrCopy(headerfile,scrfile,dot);}
   StrAdd(headerfile,".header");
//   dot:=InStr(savereq.file,'.')
//   StrCopy(specname,savereq.file,dot)
}

if( saveformat==3) {
  if( quiet==0 ) { WriteF("Save Format: Z80 Tape Image\n");}
}

if( saveformat==1) {
  if( quiet==0 ) { WriteF("Save Format: ZX82\n");}
}

if( saveformat==4) {
  if( quiet==0 ) { WriteF("Save Format: TZX\n");}
}

headcheck=0;

headerbytes[0]=0x13  /* .tap only */;
headerbytes[1]=0x00  /* .tap only */;
headerbytes[2]=0x00;
headerbytes[3]=0x03;

// headcheck:=$F8
headerbytes[4]=fname[0];
headerbytes[5]=fname[1];
headerbytes[6]=fname[2];
headerbytes[7]=fname[3];
headerbytes[8]=fname[4];
headerbytes[9]=fname[5];
headerbytes[10]=fname[6];
headerbytes[11]=fname[7];
headerbytes[12]=fname[8];
headerbytes[13]=fname[9];

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

headerbytes[14]=0x00;
headerbytes[15]=0x1B;
headerbytes[16]=0x00;
headerbytes[17]=0x40;
headerbytes[18]=0x20;
headerbytes[19]=0x80;
headerbytes[20]=0xEC /* eor of values 2 to 19 */;
headerbytes[21]=0x02 /* .tap only */;
headerbytes[22]=0x1B /* .tap only */;






if( saveformat==2) {
 if( writeheader) {
  if( fhhead=(long) Open(headerfile,MODE_NEWFILE)) {
     if( quiet==0 ) { WriteF("Writing %s file...\n",(long) headerfile);}
     for( h=2 ; h<=(long) 19 /* 20 */; h++) {
     headcheck=((headcheck)^(headerbytes[h]));
     FPutC((BPTR) fhhead,headerbytes[h]);
     }
     FPutC((BPTR) fhhead,headcheck);
//     FFlush(fhhead)
     -Close((BPTR) fhhead);
  }
 }
}

if( fhand=(long) Open(scrfile,MODE_NEWFILE)) {

if( saveformat==3) {
  if( quiet==0 ) { WriteF("Writing TAP header...\n");}
     for( h=0 ; h<=(long) 1; h++) {
     FPutC((BPTR) fhand,headerbytes[h]);
     }
     for( h=2 ; h<=(long) 19; h++) {
     FPutC((BPTR) fhand,headerbytes[h]);
     headcheck=((headcheck)^(headerbytes[h]));
     }
     FPutC((BPTR) fhand,headcheck);
     for( h=21 ; h<=(long) 22; h++) {
     FPutC((BPTR) fhand,headerbytes[h]);
     }
}

if( saveformat==4) {
  if( quiet==0 ) { WriteF("Writing TZX header...\n");}
	FPuts((BPTR) fhand,"ZXTape!");
	FPutC((BPTR) fhand,0x1A);
	FPutC((BPTR) fhand,01);
	FPutC((BPTR) fhand,11);
	
	/* archive info */
	
	FPutC((BPTR) fhand,0x32);
	FPutC((BPTR) fhand,28);
	FPutC((BPTR) fhand,00);
	FPutC((BPTR) fhand,01);
	FPutC((BPTR) fhand,0xFF);
	FPutC((BPTR) fhand,25);
	FPuts((BPTR) fhand,"Created with ppmtoscr 3.4");
	
	/*** start of ID/data etc ***/
	FPutC((BPTR) fhand,0x10);
	FPutC((BPTR) fhand,00);
	FPutC((BPTR) fhand,10);
	
	for( h=0 ; h<=(long) 1; h++) {
	     FPutC((BPTR) fhand,headerbytes[h]);
     }
     for( h=2 ; h<=(long) 19; h++) {
     	FPutC((BPTR) fhand,headerbytes[h]);
	     headcheck=((headcheck)^(headerbytes[h]));
     }
     FPutC((BPTR) fhand,headcheck);
     
    	FPutC((BPTR) fhand,0x10);
	FPutC((BPTR) fhand,00);
	FPutC((BPTR) fhand,10);

     for( h=21 ; h<=(long) 22; h++) {
	     FPutC((BPTR) fhand,headerbytes[h]);
     }
}

if( saveformat==1) {
  if( quiet==0 ) { WriteF("Writing ZX82 header...\n");}
zx82header[0]=0x5A;
zx82header[1]=0x58;
zx82header[2]=0x38;
zx82header[3]=0x32;
zx82header[4]=0x03;
zx82header[5]=0x00;
zx82header[6]=0x1B;
zx82header[7]=0x00;
zx82header[8]=0x40;
zx82header[9]=0x00;
zx82header[10]=0x80;
zx82header[11]=0x00;

     for( h=0 ; h<=(long) 11; h++) {
     FPutC((BPTR) fhand,zx82header[h]);
     }
}

if( saveformat>1) {
  FPutC((BPTR) fhand,0xFF);
}

checksum=0xFF;

      StrCopy(wintit,"Mapping colours & writing image data...");
if( quiet==0 ) { WriteF("%s\n",(long) wintit);}

/* temp stuff *********************
Fputs(fhand,header)
Fputs(fhand,data)
Flush(fhand)
Close(fhand)
JUMP finish
********************************/

/*
do i=1 to 147456   ** 71 **

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

a=15 /*-172*/;
x=0;

for( l=1 ; l<=(long) 3; l++) {

for( k=1 ; k<=(long) 8; k++) {

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


FPutC((BPTR) fhand,bin);

checksum=((checksum)^(bin));

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

      StrCopy(wintit,"Writing colour attributes...");
      if( quiet==0 ) { WriteF("%s\n",(long) wintit);}


for( m=1 ; m<=(long) 768; m++) {

/* check attr() command! */

/* ink=0
paper=7 */

attr=(long) ((short) 1*ink[m])+(long) ((short) 8*paper[m])+(long) brightattr[m]+(long) flash[m];

// col=d2c(attr)

FPutC((BPTR) fhand,attr);

checksum=((checksum)^(attr));

}

if( saveformat>1) {
  FPutC((BPTR) fhand,checksum);
}

// Flush(fhand)

-Close((BPTR) fhand);

StrCopy(endmsg,"Conversion done - all OK!");

}
	return ;

}


long mapcol(long offset, long binary) {

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
  
//  col,bright:=choosecol(red,grn,blu)
  
choosecol();
  
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

availpix=(long) ((short) 64-best[0])/(long) 2;

if( best[8]>availpix ) { brightattr[blk]=64 /* ELSE brightattr[blk]:=0 */;}

}

red=ppmmem[a+offset];
grn=ppmmem[a+offset+(long) 1];
blu=ppmmem[a+offset+(long) 2];

// col:=choosecol(red,grn,blu)

choosecol();

if( flashred==red) {
  if( flashgrn==grn) {
    if( flashblu==blu) {
      flash[blk]=128;
    }
  }
}

// WriteF('\d \d \d\n',red,grn,blu)


// IF red=grn AND grn=blu
// ENDIF


if( paper[blk]!=col) {
  if( smooth==1) {
    if( labs((long) paper[blk]-col)>labs((long) ink[blk]-col)) { bin=bin+binary;}
// v old ink[blk] := col
  } else {
  bin=bin+binary;
  }

}
	return col;

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
if( cm==(long) NULL ) { cm=(long) GetColorMap(15);}

if( rompal==0 /* ZXDT */) {
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 15,(ULONG) 0x00000000,(ULONG) 0x00000000,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 0,(ULONG) 0x00000000,(ULONG) 0x00000000,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 1,(ULONG) 0x00000000,(ULONG) 0x00000000,(ULONG) ((153)<<(24)));
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 2,(ULONG) ((153)<<(24)),(ULONG) 0x00000000,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 3,(ULONG) ((153)<<(24)),(ULONG) 0x00000000,(ULONG) ((153)<<(24)));
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 4,(ULONG) 0x00000000,(ULONG) ((153)<<(24)),(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 5,(ULONG) 0x00000000,(ULONG) ((153)<<(24)),(ULONG) ((153)<<(24)));
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 6,(ULONG) ((153)<<(24)),(ULONG) ((153)<<(24)),(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 7,(ULONG) ((153)<<(24)),(ULONG) ((153)<<(24)),(ULONG) ((153)<<(24)));
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 8,(ULONG) 0x00000000,(ULONG) 0x00000000,(ULONG) ((170)<<(24)));
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 9,(ULONG) ((187)<<(24)),(ULONG) 0x00000000,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 10,(ULONG) ((204)<<(24)),(ULONG) 0x00000000,(ULONG) ((204)<<(24)));
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 11,(ULONG) 0x00000000,(ULONG) ((204)<<(24)),(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 12,(ULONG) 0x00000000,(ULONG) 0xDDDDDDDD,(ULONG) 0xDDDDDDDD);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 13,(ULONG) ((238)<<(24)),(ULONG) ((238)<<(24)),(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 14,(ULONG) 0xFFFFFFFF,(ULONG) 0xFFFFFFFF,(ULONG) 0xFFFFFFFF);

} else {

  SetRGB32CM((struct ColorMap*) cm,(ULONG) 15,(ULONG) 0x00000000,(ULONG) 0x00000000,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 0,(ULONG) 0x00000000,(ULONG) 0x00000000,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 8,(ULONG) 0x00000000,(ULONG) 0x00000000,(ULONG) 0xFFFFFFFF);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 9,(ULONG) 0xFFFFFFFF,(ULONG) 0x00000000,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 10,(ULONG) 0xFFFFFFFF,(ULONG) 0x00000000,(ULONG) 0xFFFFFFFF);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 11,(ULONG) 0x00000000,(ULONG) 0xFFFFFFFF,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 12,(ULONG) 0x00000000,(ULONG) 0xFFFFFFFF,(ULONG) 0xFFFFFFFF);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 13,(ULONG) 0xFFFFFFFF,(ULONG) 0xFFFFFFFF,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 14,(ULONG) 0xFFFFFFFF,(ULONG) 0xFFFFFFFF,(ULONG) 0xFFFFFFFF);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 1,(ULONG) 0x00000000,(ULONG) 0x00000000,(ULONG) 0xDDDDDDDD);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 2,(ULONG) 0xDDDDDDDD,(ULONG) 0x00000000,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 3,(ULONG) 0xDDDDDDDD,(ULONG) 0x00000000,(ULONG) 0xDDDDDDDD);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 4,(ULONG) 0x00000000,(ULONG) 0xDDDDDDDD,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 5,(ULONG) 0x00000000,(ULONG) 0xDDDDDDDD,(ULONG) 0xDDDDDDDD);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 6,(ULONG) 0xDDDDDDDD,(ULONG) 0xDDDDDDDD,(ULONG) 0x00000000);
  SetRGB32CM((struct ColorMap*) cm,(ULONG) 7,(ULONG) 0xDDDDDDDD,(ULONG) 0xDDDDDDDD,(ULONG) 0xDDDDDDDD);
}
	return ;

}

void choosecol() {
	long temp_QUAD;
try {
	temp_QUAD = exception ;
	exception = 0 ;
if( os3==FALSE) {
   stdchoosecol();
   Raise(0);
}

bright=0;
red=((red)<<(24));
grn=((grn)<<(24));
blu=((blu)<<(24));

col=FindColor((struct ColorMap*) cm,(ULONG) red,(ULONG) grn,(ULONG) blu,-1);
if( col==15 ) { col=0;}
if( col>7) {
  bright=1;
  col=col-(long) 7;
}
} catch(...) {}
	if (exception!=0) {throw Exception;} else {EMPTY;};
	exception = temp_QUAD ;
	return ;

}
void new_ppmtoscr()  {
	scrfile = NewString(512);
	headerfile = NewString(512);
	fname = NewString(50);
	verstring = NewString(30);
	endmsg = NewString(100);
	wintit = NewString(50);
	filetypeerror = NewString(100);
	filename = NewString(512);
	tempGlobal_ILIST = NewList(19);
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
	SetList(tempGlobal_ILIST ,19);
	return ;
}
void end_ppmtoscr()  {
	DisposeList(tempGlobal_ILIST );
	DisposeString(filename );
	DisposeString(filetypeerror );
	DisposeString(wintit );
	DisposeString(endmsg );
	DisposeString(verstring );
	DisposeString(fname );
	DisposeString(headerfile );
	DisposeString(scrfile );
	return ;
}
