/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/tab_corr.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( TAB_CORR );
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
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( F_CORR );
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
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( __XRESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TAB_CORR )
{ "TAB_CORR", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( TAB_CORR )}, NULL },
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
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "F_CORR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_CORR )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "__XSAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XSAVESCREEN )}, NULL },
{ "SETCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCURSOR )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "__XRESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XRESTSCREEN )}, NULL },
{ "FLD_NAME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FLD_PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TAB_CORR, "C:/sis/SAN/xhb/tab_corr.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TAB_CORR
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TAB_CORR )
   #include "hbiniseg.h"
#endif

HB_FUNC( TAB_CORR )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,14,0,176,5,
		0,106,36,65,84,85,65,76,73,90,65,67,65,79,
		32,68,65,32,84,65,66,69,76,65,32,68,69,32,
		67,79,82,82,69,84,79,82,69,83,0,20,1,36,
		15,0,176,6,0,106,2,49,0,20,1,36,16,0,
		176,7,0,9,100,106,9,67,79,82,82,69,84,79,
		82,0,100,100,9,20,6,176,8,0,106,9,67,79,
		82,82,69,84,79,82,0,20,1,36,17,0,176,9,
		0,20,0,36,18,0,176,10,0,20,0,36,19,0,
		176,11,0,106,9,87,47,78,44,43,87,47,82,0,
		20,1,36,20,0,176,13,0,12,0,3,1,0,176,
		13,0,12,0,3,1,0,176,13,0,12,0,3,1,
		0,176,13,0,12,0,3,1,0,176,17,0,108,12,
		108,14,108,15,108,16,20,4,81,16,0,81,15,0,
		81,14,0,81,12,0,36,21,0,176,18,0,109,12,
		0,120,109,16,0,20,3,36,22,0,106,9,64,82,
		32,57,57,57,57,57,0,98,14,0,122,2,36,23,
		0,106,5,64,83,49,50,0,98,14,0,92,2,2,
		36,24,0,106,5,64,83,52,48,0,98,14,0,92,
		3,2,36,25,0,106,7,67,224,68,73,71,79,0,
		98,15,0,122,2,36,26,0,106,8,65,80,69,76,
		73,68,79,0,98,15,0,92,2,2,36,27,0,106,
		14,78,79,77,69,32,67,79,77,80,76,69,84,79,
		0,98,15,0,92,3,2,36,28,0,176,19,0,92,
		5,121,92,21,92,79,92,2,20,5,36,29,0,176,
		20,0,106,3,48,49,0,20,1,36,30,0,176,11,
		0,106,1,0,20,1,36,31,0,176,11,0,106,6,
		87,47,78,43,42,0,20,1,36,32,0,176,21,0,
		92,23,121,20,2,176,22,0,106,8,91,69,78,84,
		69,82,93,0,20,1,36,33,0,176,21,0,92,23,
		92,21,20,2,176,22,0,106,6,91,69,83,67,93,
		0,20,1,36,34,0,176,21,0,92,23,92,37,20,
		2,176,22,0,106,9,91,69,83,80,65,128,79,93,
		0,20,1,36,35,0,176,21,0,92,23,92,59,20,
		2,176,22,0,106,7,91,26,27,24,25,93,0,20,
		1,36,36,0,176,11,0,106,1,0,20,1,36,37,
		0,176,11,0,106,9,87,47,78,44,43,87,47,82,
		0,20,1,36,38,0,176,21,0,92,23,92,7,20,
		2,176,22,0,106,13,80,97,114,97,32,65,108,116,
		101,114,97,114,0,20,1,36,39,0,176,21,0,92,
		23,92,26,20,2,176,22,0,106,10,80,97,114,97,
		32,83,97,105,114,0,20,1,36,40,0,176,21,0,
		92,23,92,45,20,2,176,22,0,106,13,80,97,114,
		97,32,73,110,99,108,117,105,114,0,20,1,36,41,
		0,176,21,0,92,23,92,65,20,2,176,22,0,106,
		13,77,111,118,105,109,101,110,116,97,135,131,111,0,
		20,1,36,42,0,176,23,0,92,6,122,92,21,92,
		78,109,12,0,106,7,70,95,67,79,82,82,0,109,
		14,0,109,15,0,106,2,196,0,106,2,179,0,106,
		2,205,0,20,11,36,43,0,176,24,0,20,0,176,
		6,0,106,2,49,0,20,1,176,25,0,100,20,1,
		36,44,0,176,26,0,121,121,176,2,0,12,0,176,
		3,0,12,0,109,4,0,20,5,36,45,0,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_CORR )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,28,0,1,71,29,0,2,36,50,0,
		176,17,0,108,30,108,31,20,2,36,51,0,176,32,
		0,12,0,83,31,0,36,52,0,122,83,30,0,36,
		53,0,109,28,0,92,4,8,29,138,2,36,55,0,
		109,31,0,92,32,8,29,163,1,36,56,0,176,33,
		0,20,0,36,57,0,176,34,0,122,20,1,36,58,
		0,176,20,0,106,3,48,49,0,20,1,36,59,0,
		176,35,0,106,27,67,111,110,102,105,114,109,97,32,
		73,110,99,108,117,115,97,111,32,40,83,47,78,41,
		32,63,32,0,20,1,36,60,0,109,36,0,106,2,
		83,0,5,29,62,1,36,61,0,121,83,37,0,36,
		63,0,176,20,0,106,3,48,49,0,20,1,36,64,
		0,176,21,0,92,23,122,20,2,176,22,0,106,18,
		67,162,100,105,103,111,32,67,111,114,114,101,116,111,
		114,58,32,0,20,1,176,38,0,176,39,0,12,0,
		176,40,0,12,0,122,72,20,2,176,41,0,109,42,
		0,176,43,0,100,106,8,101,67,79,68,73,71,79,
		0,106,9,64,82,32,57,57,57,57,57,0,100,100,
		12,5,20,2,48,44,0,176,45,0,109,42,0,12,
		1,112,0,73,36,65,0,176,46,0,109,42,0,100,
		100,100,100,100,100,20,7,4,0,0,83,42,0,109,
		42,0,73,36,66,0,109,37,0,121,5,28,36,36,
		67,0,176,47,0,106,16,67,162,100,105,103,111,32,
		73,110,118,97,108,105,100,111,0,92,3,20,2,36,
		68,0,26,71,255,36,70,0,176,9,0,20,0,36,
		71,0,176,48,0,109,37,0,100,100,20,3,36,72,
		0,176,49,0,12,0,28,41,36,73,0,176,47,0,
		106,21,67,162,100,105,103,111,32,106,160,32,99,97,
		100,97,115,116,114,97,100,111,0,92,3,20,2,36,
		74,0,26,1,255,36,76,0,176,6,0,106,2,49,
		0,20,1,36,77,0,176,50,0,20,0,36,78,0,
		176,51,0,12,0,28,22,36,79,0,109,37,0,78,
		52,0,36,80,0,176,53,0,20,0,36,82,0,36,
		87,0,176,34,0,121,20,1,36,88,0,176,54,0,
		20,0,26,222,0,36,89,0,109,31,0,92,13,8,
		29,192,0,36,90,0,176,33,0,20,0,36,91,0,
		109,29,0,122,15,29,163,0,36,92,0,176,51,0,
		12,0,29,152,0,36,93,0,176,33,0,20,0,36,
		94,0,176,34,0,122,20,1,36,95,0,98,12,0,
		109,29,0,1,83,55,0,36,96,0,98,14,0,109,
		29,0,1,83,56,0,36,97,0,176,38,0,176,39,
		0,12,0,176,40,0,12,0,20,2,176,41,0,109,
		42,0,176,43,0,100,109,55,0,109,56,0,100,100,
		12,5,20,2,48,44,0,176,45,0,109,42,0,12,
		1,112,0,73,36,98,0,176,46,0,109,42,0,100,
		100,100,100,100,100,20,7,4,0,0,83,42,0,109,
		42,0,73,36,99,0,176,34,0,121,20,1,36,100,
		0,176,53,0,20,0,36,101,0,176,54,0,20,0,
		36,104,0,176,54,0,20,0,25,20,36,105,0,109,
		31,0,92,27,8,28,9,36,106,0,121,83,30,0,
		36,109,0,109,30,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

