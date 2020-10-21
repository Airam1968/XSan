/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/tab_0100.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( TAB_0100 );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( FCOUNT );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( F_P0100 );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( __XSAVESCREEN );
HB_FUNC_EXTERN( F_BLOQR );
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


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TAB_0100 )
{ "TAB_0100", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( TAB_0100 )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA08", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "CAMPO01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCOUNT )}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "F_P0100", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_P0100 )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "__XSAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XSAVESCREEN )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
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
{ "__XRESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XRESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TAB_0100, "C:/sis/SAN/xhb/tab_0100.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TAB_0100
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TAB_0100 )
   #include "hbiniseg.h"
#endif

HB_FUNC( TAB_0100 )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,14,0,176,5,
		0,106,36,77,65,78,85,84,69,78,128,199,79,32,
		82,69,71,73,83,84,82,79,32,48,49,48,48,32,
		80,73,83,32,67,79,70,73,78,83,0,20,1,36,
		15,0,176,6,0,106,2,49,0,20,1,36,16,0,
		176,7,0,9,100,106,9,80,73,83,95,48,49,48,
		48,0,100,100,9,20,6,36,17,0,176,8,0,20,
		0,36,18,0,176,9,0,12,0,28,31,36,19,0,
		176,10,0,20,0,36,20,0,106,5,48,49,48,48,
		0,78,11,0,36,21,0,176,8,0,20,0,36,23,
		0,176,12,0,20,0,36,24,0,176,13,0,106,9,
		87,47,78,44,43,87,47,82,0,20,1,36,25,0,
		176,15,0,12,0,3,1,0,176,15,0,12,0,3,
		1,0,176,15,0,12,0,3,1,0,176,15,0,12,
		0,3,1,0,176,19,0,108,14,108,16,108,17,108,
		18,20,4,81,18,0,81,17,0,81,16,0,81,14,
		0,36,26,0,176,20,0,109,14,0,120,109,18,0,
		20,3,36,27,0,106,9,82,69,71,73,83,84,82,
		79,0,98,17,0,122,2,36,28,0,106,18,78,79,
		77,69,32,67,79,78,84,65,66,73,76,73,83,84,
		65,0,98,17,0,92,2,2,36,29,0,106,4,67,
		80,70,0,98,17,0,92,3,2,36,30,0,106,4,
		67,82,67,0,98,17,0,92,4,2,36,31,0,106,
		16,67,78,80,74,32,69,83,67,82,73,84,224,82,
		73,79,0,98,17,0,92,5,2,36,32,0,106,4,
		67,69,80,0,98,17,0,92,6,2,36,33,0,106,
		9,69,78,68,69,82,69,128,79,0,98,17,0,92,
		7,2,36,34,0,106,7,78,233,77,69,82,79,0,
		98,17,0,92,8,2,36,35,0,106,12,67,79,77,
		80,76,69,77,69,78,84,79,0,98,17,0,92,9,
		2,36,36,0,106,7,66,65,73,82,82,79,0,98,
		17,0,92,10,2,36,37,0,106,9,84,69,76,69,
		70,79,78,69,0,98,17,0,92,11,2,36,38,0,
		106,4,70,65,88,0,98,17,0,92,12,2,36,39,
		0,106,7,69,45,77,65,73,76,0,98,17,0,92,
		13,2,36,40,0,106,22,67,79,68,73,71,79,32,
		73,66,71,69,32,77,85,78,73,67,73,80,73,79,
		0,98,17,0,92,14,2,36,41,0,176,21,0,92,
		5,121,92,21,92,79,92,2,20,5,36,42,0,176,
		22,0,106,3,48,49,0,20,1,36,43,0,176,13,
		0,106,1,0,20,1,36,44,0,176,13,0,106,6,
		87,47,78,43,42,0,20,1,36,45,0,176,23,0,
		92,23,121,20,2,176,24,0,106,8,91,69,78,84,
		69,82,93,0,20,1,36,46,0,176,23,0,92,23,
		92,21,20,2,176,24,0,106,6,91,69,83,67,93,
		0,20,1,36,47,0,176,23,0,92,23,92,59,20,
		2,176,24,0,106,7,91,26,27,24,25,93,0,20,
		1,36,48,0,176,13,0,106,1,0,20,1,36,49,
		0,176,13,0,106,9,87,47,78,44,43,87,47,82,
		0,20,1,36,50,0,176,23,0,92,23,92,7,20,
		2,176,24,0,106,13,80,97,114,97,32,65,108,116,
		101,114,97,114,0,20,1,36,51,0,176,23,0,92,
		23,92,26,20,2,176,24,0,106,10,80,97,114,97,
		32,83,97,105,114,0,20,1,36,52,0,176,23,0,
		92,23,92,65,20,2,176,24,0,106,13,77,111,118,
		105,109,101,110,116,97,135,131,111,0,20,1,36,53,
		0,176,25,0,92,6,122,92,21,92,78,109,14,0,
		106,8,70,95,80,48,49,48,48,0,109,16,0,109,
		17,0,106,2,196,0,106,2,179,0,106,2,205,0,
		20,11,36,54,0,176,26,0,20,0,176,6,0,106,
		2,49,0,20,1,176,27,0,100,20,1,36,55,0,
		176,28,0,121,121,176,2,0,12,0,176,3,0,12,
		0,109,4,0,20,5,36,56,0,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_P0100 )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,30,0,1,71,31,0,2,36,61,0,
		176,19,0,108,32,108,33,20,2,36,62,0,176,34,
		0,12,0,83,33,0,36,63,0,122,83,32,0,36,
		64,0,109,30,0,92,4,8,29,222,0,36,66,0,
		109,33,0,92,13,8,29,192,0,36,67,0,176,35,
		0,20,0,36,68,0,109,31,0,122,15,29,163,0,
		36,69,0,176,36,0,12,0,29,152,0,36,70,0,
		176,35,0,20,0,36,71,0,176,37,0,122,20,1,
		36,72,0,98,14,0,109,31,0,1,83,38,0,36,
		73,0,98,16,0,109,31,0,1,83,39,0,36,74,
		0,176,40,0,176,41,0,12,0,176,42,0,12,0,
		20,2,176,43,0,109,44,0,176,45,0,100,109,38,
		0,109,39,0,100,100,12,5,20,2,48,46,0,176,
		47,0,109,44,0,12,1,112,0,73,36,75,0,176,
		48,0,109,44,0,100,100,100,100,100,100,20,7,4,
		0,0,83,44,0,109,44,0,73,36,76,0,176,37,
		0,121,20,1,36,77,0,176,49,0,20,0,36,78,
		0,176,50,0,20,0,36,81,0,176,50,0,20,0,
		25,20,36,82,0,109,33,0,92,27,8,28,9,36,
		83,0,121,83,32,0,36,86,0,109,32,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}
