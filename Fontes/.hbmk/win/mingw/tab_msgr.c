/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/tab_msgr.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( TAB_MSGR );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( F_MSGR );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( __XSAVESCREEN );
HB_FUNC_EXTERN( SETCURSOR );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( __XRESTSCREEN );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( DBAPPEND );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TAB_MSGR )
{ "TAB_MSGR", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( TAB_MSGR )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "F_MSGR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_MSGR )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "__XSAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XSAVESCREEN )}, NULL },
{ "SETCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCURSOR )}, NULL },
{ "FLD_NAME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FLD_PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "ROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROW )}, NULL },
{ "COL", {HB_FS_PUBLIC}, {HB_FUNCNAME( COL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "__XRESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XRESTSCREEN )}, NULL },
{ "ECOD_MSG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "COD_MSG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TAB_MSGR, "C:/sis/SAN/xhb/tab_msgr.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TAB_MSGR
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TAB_MSGR )
   #include "hbiniseg.h"
#endif

HB_FUNC( TAB_MSGR )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,14,0,176,5,
		0,106,21,67,79,78,83,85,76,84,65,32,65,32,
		77,69,78,83,65,71,69,78,83,0,20,1,36,15,
		0,176,6,0,106,3,48,49,0,20,1,36,16,0,
		176,7,0,106,18,67,111,110,102,105,114,109,97,32,
		40,83,47,78,41,32,63,32,0,20,1,36,17,0,
		109,8,0,106,2,83,0,5,29,138,2,36,18,0,
		176,9,0,106,2,49,0,20,1,36,19,0,176,10,
		0,9,100,106,9,77,83,71,95,66,79,76,69,0,
		100,100,9,20,6,176,11,0,106,9,77,83,71,95,
		66,79,76,69,0,20,1,176,11,0,106,9,77,83,
		71,95,80,82,79,68,0,20,1,36,20,0,176,12,
		0,20,0,36,21,0,176,13,0,20,0,36,22,0,
		176,14,0,106,9,87,47,78,44,43,87,47,82,0,
		20,1,36,23,0,92,7,3,1,0,92,7,3,1,
		0,92,7,3,1,0,92,7,3,1,0,176,19,0,
		108,15,108,16,108,17,108,18,20,4,81,18,0,81,
		17,0,81,16,0,81,15,0,36,24,0,176,20,0,
		109,15,0,120,109,18,0,20,3,36,25,0,106,9,
		64,82,32,57,57,57,57,57,0,98,16,0,122,2,
		36,26,0,106,9,64,82,32,57,57,57,57,57,0,
		98,16,0,92,2,2,36,27,0,106,5,64,83,53,
		48,0,98,16,0,92,3,2,36,28,0,106,5,64,
		82,53,48,0,98,16,0,92,4,2,36,29,0,106,
		5,64,82,53,48,0,98,16,0,92,5,2,36,30,
		0,106,5,64,82,53,48,0,98,16,0,92,6,2,
		36,31,0,106,5,64,82,53,48,0,98,16,0,92,
		7,2,36,32,0,176,21,0,92,5,121,92,21,92,
		79,92,2,20,5,36,33,0,176,6,0,106,3,48,
		49,0,20,1,36,34,0,176,14,0,106,1,0,20,
		1,36,35,0,176,14,0,106,6,87,47,78,43,42,
		0,20,1,36,36,0,176,22,0,92,23,121,20,2,
		176,23,0,106,8,91,69,78,84,69,82,93,0,20,
		1,36,37,0,176,22,0,92,23,92,21,20,2,176,
		23,0,106,6,91,69,83,67,93,0,20,1,36,38,
		0,176,22,0,92,23,92,37,20,2,176,23,0,106,
		9,91,69,83,80,65,128,79,93,0,20,1,36,39,
		0,176,22,0,92,23,92,59,20,2,176,23,0,106,
		7,91,26,27,24,25,93,0,20,1,36,40,0,176,
		14,0,106,1,0,20,1,36,41,0,176,14,0,106,
		9,87,47,78,44,43,87,47,82,0,20,1,36,42,
		0,176,22,0,92,23,92,7,20,2,176,23,0,106,
		13,80,97,114,97,32,65,108,116,101,114,97,114,0,
		20,1,36,43,0,176,22,0,92,23,92,26,20,2,
		176,23,0,106,10,80,97,114,97,32,83,97,105,114,
		0,20,1,36,44,0,176,22,0,92,23,92,45,20,
		2,176,23,0,106,13,80,97,114,97,32,73,110,99,
		108,117,105,114,0,20,1,36,45,0,176,22,0,92,
		23,92,65,20,2,176,23,0,106,13,77,111,118,105,
		109,101,110,116,97,135,131,111,0,20,1,36,46,0,
		176,24,0,92,6,122,92,21,92,78,109,15,0,106,
		7,70,95,77,83,71,82,0,109,16,0,109,17,0,
		106,2,196,0,106,2,179,0,106,2,205,0,20,11,
		36,48,0,176,25,0,20,0,176,9,0,106,2,49,
		0,20,1,176,26,0,100,20,1,36,49,0,176,27,
		0,121,121,176,2,0,12,0,176,3,0,12,0,109,
		4,0,20,5,36,50,0,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_MSGR )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,29,0,1,71,30,0,2,36,55,0,
		176,19,0,108,31,108,32,20,2,36,56,0,176,33,
		0,12,0,83,32,0,36,57,0,122,83,31,0,36,
		58,0,109,29,0,92,4,8,29,84,2,36,60,0,
		109,32,0,92,13,8,29,180,0,36,61,0,109,30,
		0,122,15,29,61,2,36,62,0,176,34,0,12,0,
		29,50,2,36,63,0,176,35,0,20,0,36,64,0,
		176,36,0,122,20,1,36,65,0,98,15,0,109,30,
		0,1,83,37,0,36,66,0,98,16,0,109,30,0,
		1,83,38,0,36,67,0,176,39,0,176,40,0,12,
		0,176,41,0,12,0,20,2,176,42,0,109,43,0,
		176,44,0,100,109,37,0,109,38,0,100,100,12,5,
		20,2,48,45,0,176,46,0,109,43,0,12,1,112,
		0,73,36,68,0,176,47,0,109,43,0,100,100,100,
		100,100,100,20,7,4,0,0,83,43,0,109,43,0,
		73,36,69,0,176,36,0,121,20,1,36,70,0,176,
		48,0,20,0,36,71,0,176,49,0,20,0,36,73,
		0,26,151,1,36,74,0,109,32,0,92,32,8,29,
		121,1,36,75,0,176,35,0,20,0,36,76,0,176,
		36,0,122,20,1,36,77,0,176,6,0,106,3,48,
		49,0,20,1,36,78,0,176,7,0,106,27,67,111,
		110,102,105,114,109,97,32,73,110,99,108,117,115,97,
		111,32,40,83,47,78,41,32,63,32,0,20,1,36,
		79,0,109,8,0,106,2,83,0,5,29,21,1,36,
		80,0,121,83,50,0,36,82,0,176,6,0,106,3,
		48,49,0,20,1,36,83,0,176,22,0,92,23,122,
		20,2,176,23,0,106,22,67,111,100,105,103,111,32,
		100,97,32,77,101,110,115,97,103,101,109,32,58,32,
		0,20,1,176,39,0,176,40,0,12,0,176,41,0,
		12,0,122,72,20,2,176,42,0,109,43,0,176,44,
		0,100,106,9,101,67,79,68,95,77,83,71,0,106,
		9,64,82,32,57,57,57,57,57,0,100,100,12,5,
		20,2,48,45,0,176,46,0,109,43,0,12,1,112,
		0,73,36,84,0,176,47,0,109,43,0,100,100,100,
		100,100,100,20,7,4,0,0,83,43,0,109,43,0,
		73,36,85,0,109,50,0,121,5,31,110,36,88,0,
		176,12,0,20,0,36,89,0,176,51,0,109,50,0,
		100,100,20,3,36,90,0,176,52,0,12,0,28,41,
		36,91,0,176,53,0,106,21,67,111,100,105,103,111,
		32,106,97,32,67,97,100,97,115,116,114,97,100,111,
		0,92,3,20,2,36,92,0,26,30,255,36,94,0,
		176,54,0,20,0,36,95,0,176,34,0,12,0,28,
		22,36,96,0,109,50,0,78,55,0,36,97,0,176,
		48,0,20,0,36,99,0,36,104,0,176,36,0,121,
		20,1,36,105,0,176,49,0,20,0,25,20,36,106,
		0,109,32,0,92,27,8,28,9,36,107,0,121,83,
		31,0,36,110,0,109,31,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

