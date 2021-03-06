/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_scot.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_SCOT );
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
HB_FUNC( F_CON_SCOT );
HB_FUNC_EXTERN( LASTKEY );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_SCOT )
{ "CON_SCOT", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_SCOT )}, NULL },
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
{ "F_CON_SCOT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_CON_SCOT )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_SCOT, "C:/sis/SAN/xhb/con_scot.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_SCOT
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_SCOT )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_SCOT )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,32,67,79,78,83,85,76,84,65,32,65,32,
		83,73,84,85,65,67,65,79,32,68,69,32,67,79,
		84,65,67,79,69,83,0,20,1,36,16,0,176,6,
		0,106,3,48,49,0,20,1,36,17,0,176,7,0,
		106,18,67,111,110,102,105,114,109,97,32,40,83,47,
		78,41,32,63,32,0,20,1,36,18,0,109,8,0,
		106,2,83,0,5,29,84,1,36,19,0,176,9,0,
		106,2,50,0,20,1,36,20,0,176,10,0,9,100,
		106,8,67,79,84,65,67,65,79,0,100,100,9,20,
		6,176,11,0,106,8,67,79,84,65,67,65,79,0,
		20,1,36,21,0,176,12,0,20,0,36,22,0,176,
		13,0,20,0,36,23,0,176,14,0,106,9,87,47,
		78,44,43,87,47,82,0,20,1,36,24,0,92,10,
		3,1,0,92,10,3,1,0,92,10,3,1,0,92,
		10,3,1,0,176,19,0,108,15,108,16,108,17,108,
		18,20,4,81,18,0,81,17,0,81,16,0,81,15,
		0,36,25,0,176,20,0,109,15,0,120,109,18,0,
		20,3,36,26,0,176,21,0,92,5,121,92,21,92,
		79,92,2,20,5,36,27,0,176,6,0,106,3,48,
		49,0,20,1,36,28,0,176,14,0,106,1,0,20,
		1,36,29,0,176,14,0,106,6,87,47,78,43,42,
		0,20,1,36,30,0,176,22,0,92,23,121,20,2,
		176,23,0,106,6,91,69,83,67,93,0,20,1,36,
		31,0,176,14,0,106,1,0,20,1,36,32,0,176,
		14,0,106,9,87,47,78,44,43,87,47,82,0,20,
		1,36,33,0,176,22,0,92,23,92,7,20,2,176,
		23,0,106,10,80,97,114,97,32,83,97,105,114,0,
		20,1,36,34,0,176,24,0,92,6,122,92,21,92,
		78,109,15,0,106,11,70,95,67,79,78,95,83,67,
		79,84,0,109,16,0,109,17,0,106,2,196,0,106,
		2,179,0,106,2,205,0,20,11,36,36,0,176,25,
		0,20,0,176,9,0,106,2,49,0,20,1,176,26,
		0,100,20,1,36,37,0,176,27,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,4,0,20,5,36,
		38,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_CON_SCOT )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,29,0,1,71,30,0,2,36,43,0,
		176,19,0,108,31,108,32,20,2,36,44,0,176,33,
		0,12,0,83,32,0,36,45,0,122,83,31,0,36,
		46,0,109,29,0,92,4,8,28,31,36,48,0,109,
		32,0,92,13,8,31,20,36,50,0,109,32,0,92,
		27,8,28,9,36,51,0,121,83,31,0,36,54,0,
		109,31,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

