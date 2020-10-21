/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/tab_taxd.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( TAB_TAXD );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( FCOUNT );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( FIELDNAME );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( DBGOBOTTOM );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( F_TAXD );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( __XSAVESCREEN );
HB_FUNC_EXTERN( SETCURSOR );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( __XRESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TAB_TAXD )
{ "TAB_TAXD", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( TAB_TAXD )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCOUNT )}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "ECAMPOS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EAUX", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FIELDNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDNAME )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "TIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBGOBOTTOM", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOBOTTOM )}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "F_TAXD", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_TAXD )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "__XSAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XSAVESCREEN )}, NULL },
{ "SETCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCURSOR )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "ROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROW )}, NULL },
{ "COL", {HB_FS_PUBLIC}, {HB_FUNCNAME( COL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "DATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "__XRESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XRESTSCREEN )}, NULL },
{ "FLD_NAME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FLD_PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TAB_TAXD, "C:/sis/SAN/xhb/tab_taxd.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TAB_TAXD
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TAB_TAXD )
   #include "hbiniseg.h"
#endif

HB_FUNC( TAB_TAXD )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,14,0,176,5,
		0,106,39,65,84,85,65,76,73,90,65,67,65,79,
		32,68,65,32,84,65,66,69,76,65,32,68,69,32,
		84,65,88,65,83,32,68,73,65,82,73,65,83,0,
		20,1,36,15,0,176,6,0,106,2,50,0,20,1,
		36,16,0,176,7,0,9,100,106,8,73,78,68,73,
		67,69,83,0,100,100,9,20,6,176,8,0,106,8,
		73,78,68,73,67,69,83,0,20,1,36,17,0,176,
		9,0,20,0,36,18,0,176,6,0,106,2,49,0,
		20,1,36,19,0,176,7,0,9,100,106,9,84,65,
		88,65,95,68,73,65,0,100,100,9,20,6,176,8,
		0,106,9,84,65,88,65,95,68,73,65,0,20,1,
		36,20,0,176,9,0,20,0,36,21,0,176,10,0,
		20,0,36,22,0,176,11,0,106,9,87,47,78,44,
		43,87,47,82,0,20,1,36,23,0,176,13,0,12,
		0,3,1,0,176,13,0,12,0,3,1,0,176,13,
		0,12,0,3,1,0,176,13,0,12,0,3,1,0,
		176,17,0,108,12,108,14,108,15,108,16,20,4,81,
		16,0,81,15,0,81,14,0,81,12,0,36,24,0,
		176,18,0,109,12,0,120,109,16,0,20,3,36,26,
		0,106,9,32,32,47,32,32,47,32,32,0,98,14,
		0,122,2,36,27,0,106,7,32,32,68,65,84,65,
		0,98,15,0,122,2,36,28,0,121,83,19,0,36,
		29,0,176,13,0,12,0,83,19,0,36,30,0,92,
		2,83,20,0,36,31,0,109,20,0,109,19,0,34,
		29,230,0,36,32,0,176,21,0,109,20,0,12,1,
		98,15,0,109,20,0,2,36,33,0,176,6,0,106,
		2,50,0,20,1,36,34,0,176,22,0,98,15,0,
		109,20,0,1,100,100,20,3,36,36,0,109,23,0,
		106,5,84,65,88,65,0,5,28,30,36,37,0,106,
		14,64,82,32,57,57,57,46,57,57,57,57,57,57,
		0,98,14,0,109,20,0,2,25,94,36,38,0,109,
		23,0,106,6,86,65,76,79,82,0,5,28,34,36,
		39,0,106,18,64,82,32,57,44,57,57,57,44,57,
		57,57,46,57,57,57,57,0,98,14,0,109,20,0,
		2,25,45,36,40,0,109,23,0,106,6,70,65,84,
		79,82,0,5,28,28,36,41,0,106,14,64,82,32,
		57,46,57,57,57,57,57,57,57,57,0,98,14,0,
		109,20,0,2,36,43,0,109,20,0,122,72,83,20,
		0,36,44,0,176,6,0,106,2,49,0,20,1,36,
		45,0,176,24,0,20,0,36,46,0,176,25,0,92,
		247,20,1,26,19,255,36,48,0,176,26,0,92,5,
		121,92,21,92,79,92,2,20,5,36,49,0,176,27,
		0,106,3,48,49,0,20,1,36,50,0,176,11,0,
		106,1,0,20,1,36,51,0,176,11,0,106,6,87,
		47,78,43,42,0,20,1,36,52,0,176,28,0,92,
		23,121,20,2,176,29,0,106,8,91,69,78,84,69,
		82,93,0,20,1,36,53,0,176,28,0,92,23,92,
		21,20,2,176,29,0,106,6,91,69,83,67,93,0,
		20,1,36,54,0,176,28,0,92,23,92,37,20,2,
		176,29,0,106,9,91,69,83,80,65,128,79,93,0,
		20,1,36,55,0,176,28,0,92,23,92,59,20,2,
		176,29,0,106,7,91,26,27,24,25,93,0,20,1,
		36,56,0,176,11,0,106,1,0,20,1,36,57,0,
		176,11,0,106,9,87,47,78,44,43,87,47,82,0,
		20,1,36,58,0,176,28,0,92,23,92,7,20,2,
		176,29,0,106,13,80,97,114,97,32,65,108,116,101,
		114,97,114,0,20,1,36,59,0,176,28,0,92,23,
		92,26,20,2,176,29,0,106,10,80,97,114,97,32,
		83,97,105,114,0,20,1,36,60,0,176,28,0,92,
		23,92,45,20,2,176,29,0,106,13,80,97,114,97,
		32,73,110,99,108,117,105,114,0,20,1,36,61,0,
		176,28,0,92,23,92,65,20,2,176,29,0,106,13,
		77,111,118,105,109,101,110,116,97,135,131,111,0,20,
		1,36,62,0,176,30,0,92,6,122,92,21,92,78,
		109,12,0,106,7,70,95,84,65,88,68,0,109,14,
		0,109,15,0,106,2,196,0,106,2,179,0,106,2,
		205,0,20,11,36,63,0,176,31,0,20,0,176,6,
		0,106,2,49,0,20,1,176,32,0,100,20,1,36,
		64,0,176,33,0,121,121,176,2,0,12,0,176,3,
		0,12,0,109,4,0,20,5,36,65,0,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_TAXD )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,35,0,1,71,36,0,2,36,70,0,
		176,17,0,108,37,108,38,20,2,36,71,0,176,39,
		0,12,0,83,38,0,36,72,0,122,83,37,0,36,
		73,0,109,35,0,92,4,8,29,117,2,36,75,0,
		109,38,0,92,32,8,29,155,1,36,76,0,176,40,
		0,20,0,36,77,0,176,41,0,122,20,1,36,78,
		0,176,27,0,106,3,48,49,0,20,1,36,79,0,
		176,42,0,106,27,67,111,110,102,105,114,109,97,32,
		73,110,99,108,117,115,97,111,32,40,83,47,78,41,
		32,63,32,0,20,1,36,80,0,109,43,0,106,2,
		83,0,5,29,54,1,36,81,0,134,0,0,0,0,
		83,44,0,36,83,0,176,27,0,106,3,48,49,0,
		20,1,36,84,0,176,28,0,92,23,122,20,2,176,
		29,0,106,18,73,110,102,111,114,109,101,32,97,32,
		68,97,116,97,32,58,32,0,20,1,176,45,0,176,
		46,0,12,0,176,47,0,12,0,122,72,20,2,176,
		48,0,109,49,0,176,50,0,100,106,6,101,68,65,
		84,65,0,100,100,100,12,5,20,2,48,51,0,176,
		52,0,109,49,0,12,1,112,0,73,36,85,0,176,
		53,0,109,49,0,100,100,100,100,100,100,20,7,4,
		0,0,83,49,0,109,49,0,73,36,86,0,109,44,
		0,134,0,0,0,0,5,28,34,36,87,0,176,54,
		0,106,14,68,97,116,97,32,73,110,118,97,108,105,
		100,97,0,92,3,20,2,36,88,0,26,81,255,36,
		90,0,176,9,0,20,0,36,91,0,176,22,0,109,
		44,0,100,100,20,3,36,92,0,176,55,0,12,0,
		28,39,36,93,0,176,54,0,106,19,68,97,116,97,
		32,106,160,32,99,97,100,97,115,116,114,97,100,97,
		0,92,3,20,2,36,94,0,26,13,255,36,96,0,
		176,6,0,106,2,49,0,20,1,36,97,0,176,56,
		0,20,0,36,98,0,176,57,0,12,0,28,22,36,
		99,0,109,44,0,78,58,0,36,100,0,176,59,0,
		20,0,36,102,0,36,107,0,176,41,0,121,20,1,
		36,108,0,176,60,0,20,0,26,209,0,36,109,0,
		109,38,0,92,13,8,29,179,0,36,110,0,109,36,
		0,122,15,29,186,0,36,111,0,176,57,0,12,0,
		29,175,0,36,112,0,176,40,0,20,0,36,113,0,
		176,41,0,122,20,1,36,114,0,98,12,0,109,36,
		0,1,83,61,0,36,115,0,98,14,0,109,36,0,
		1,83,62,0,36,116,0,176,45,0,176,46,0,12,
		0,176,47,0,12,0,20,2,176,48,0,109,49,0,
		176,50,0,100,109,61,0,109,62,0,100,100,12,5,
		20,2,48,51,0,176,52,0,109,49,0,12,1,112,
		0,73,36,117,0,176,53,0,109,49,0,100,100,100,
		100,100,100,20,7,4,0,0,83,49,0,109,49,0,
		73,36,118,0,176,41,0,121,20,1,36,119,0,176,
		59,0,20,0,36,120,0,176,60,0,20,0,36,122,
		0,25,20,36,123,0,109,38,0,92,27,8,28,9,
		36,124,0,121,83,37,0,36,127,0,109,37,0,110,
		7
	};

	hb_vmExecute( pcode, symbols );
}

