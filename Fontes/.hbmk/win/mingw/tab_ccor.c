/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/tab_ccor.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( TAB_CCOR );
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
HB_FUNC( F_CCOR );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( __XSAVESCREEN );
HB_FUNC_EXTERN( SETCURSOR );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( PCENTRA );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( __DBCOPYSTRUCT );
HB_FUNC_EXTERN( DBCREATEINDEX );
HB_FUNC_EXTERN( __XRESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TAB_CCOR )
{ "TAB_CCOR", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( TAB_CCOR )}, NULL },
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
{ "F_CCOR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_CCOR )}, NULL },
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
{ "ECOD_BAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ECOD_AGE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECOD_CCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENOME_AG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EESTADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ELIMITE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONTABIL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "PCENTRA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCENTRA )}, NULL },
{ "ROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROW )}, NULL },
{ "COL", {HB_FS_PUBLIC}, {HB_FUNCNAME( COL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "NOME_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_AGENCI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_CC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NOME_AGENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESTADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LIMITE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CONTABIL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "ECONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EZERO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "EARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__DBCOPYSTRUCT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBCOPYSTRUCT )}, NULL },
{ "DBCREATEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCREATEINDEX )}, NULL },
{ "DATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__XRESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XRESTSCREEN )}, NULL },
{ "FLD_NAME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FLD_PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TAB_CCOR, "C:/sis/SAN/xhb/tab_ccor.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TAB_CCOR
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TAB_CCOR )
   #include "hbiniseg.h"
#endif

HB_FUNC( TAB_CCOR )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,27,84,65,66,69,76,65,32,68,69,32,67,
		79,78,84,65,83,32,67,79,82,82,69,78,84,69,
		83,0,20,1,36,16,0,176,6,0,106,2,49,0,
		20,1,36,17,0,176,7,0,9,100,106,7,66,65,
		78,67,79,83,0,100,100,9,20,6,176,8,0,106,
		7,66,65,78,67,79,83,0,20,1,36,18,0,176,
		9,0,20,0,36,19,0,176,6,0,106,2,50,0,
		20,1,36,20,0,176,7,0,9,100,106,7,67,79,
		78,84,65,83,0,100,100,9,20,6,176,8,0,106,
		7,67,79,78,84,65,83,0,20,1,36,21,0,176,
		9,0,20,0,36,22,0,176,6,0,106,2,51,0,
		20,1,36,23,0,176,7,0,9,100,106,9,77,79,
		68,95,67,65,73,88,0,100,100,9,20,6,36,24,
		0,176,9,0,20,0,36,25,0,176,6,0,106,2,
		50,0,20,1,36,26,0,176,9,0,20,0,36,27,
		0,176,10,0,20,0,36,28,0,176,11,0,106,9,
		87,47,78,44,43,87,47,82,0,20,1,36,29,0,
		92,15,3,1,0,92,15,3,1,0,92,15,3,1,
		0,92,15,3,1,0,176,16,0,108,12,108,13,108,
		14,108,15,20,4,81,15,0,81,14,0,81,13,0,
		81,12,0,36,30,0,176,17,0,109,12,0,120,109,
		15,0,20,3,36,31,0,106,9,64,82,32,57,57,
		57,57,57,0,98,13,0,122,2,36,32,0,106,8,
		64,82,32,57,57,57,57,0,98,13,0,92,2,2,
		36,33,0,106,5,64,83,49,48,0,98,13,0,92,
		3,2,36,34,0,106,5,64,83,50,48,0,98,13,
		0,92,4,2,36,35,0,106,5,64,83,51,48,0,
		98,13,0,92,5,2,36,36,0,106,5,64,83,51,
		48,0,98,13,0,92,6,2,36,37,0,106,4,64,
		83,50,0,98,13,0,92,7,2,36,38,0,106,18,
		64,82,32,57,57,57,44,57,57,57,44,57,57,57,
		46,57,57,0,98,13,0,92,8,2,36,39,0,106,
		9,64,82,32,57,57,57,57,57,0,98,13,0,92,
		9,2,36,40,0,106,9,64,82,32,57,57,57,57,
		57,0,98,13,0,92,10,2,36,41,0,106,9,64,
		82,32,57,57,57,57,57,0,98,13,0,92,11,2,
		36,42,0,106,9,64,82,32,57,57,57,57,57,0,
		98,13,0,92,12,2,36,43,0,106,10,64,82,32,
		57,57,46,57,57,57,0,98,13,0,92,13,2,36,
		44,0,106,9,57,57,47,57,57,47,57,57,0,98,
		13,0,92,14,2,36,45,0,106,5,64,83,50,48,
		0,98,13,0,92,15,2,36,46,0,106,7,67,162,
		100,105,103,111,0,98,14,0,122,2,36,47,0,106,
		6,66,97,110,99,111,0,98,14,0,92,2,2,36,
		48,0,106,8,65,103,135,110,99,105,97,0,98,14,
		0,92,3,2,36,49,0,106,15,67,111,110,116,97,
		32,67,111,114,114,101,110,116,101,0,98,14,0,92,
		4,2,36,50,0,106,13,78,111,109,101,32,65,103,
		135,110,99,105,97,0,98,14,0,92,5,2,36,51,
		0,106,7,67,105,100,97,100,101,0,98,14,0,92,
		6,2,36,52,0,106,7,69,115,116,97,100,111,0,
		98,14,0,92,7,2,36,53,0,106,15,76,105,109,
		105,116,101,32,67,114,135,100,105,116,111,0,98,14,
		0,92,8,2,36,54,0,106,15,67,111,110,116,97,
		32,67,111,110,116,97,98,105,108,0,98,14,0,92,
		9,2,36,55,0,106,13,68,101,115,112,101,115,97,
		32,67,80,77,70,0,98,14,0,92,10,2,36,56,
		0,106,16,70,111,114,110,101,99,101,100,111,114,32,
		67,80,77,70,0,98,14,0,92,11,2,36,57,0,
		106,13,80,114,111,100,117,116,111,32,67,80,77,70,
		0,98,14,0,92,12,2,36,58,0,106,7,37,32,
		67,80,77,70,0,98,14,0,92,13,2,36,59,0,
		106,10,68,97,116,97,32,67,80,77,70,0,98,14,
		0,92,14,2,36,60,0,106,12,67,111,109,112,108,
		101,109,101,110,116,111,0,98,14,0,92,15,2,36,
		61,0,176,18,0,92,6,121,92,19,92,79,92,2,
		20,5,36,62,0,176,19,0,106,3,48,49,0,20,
		1,36,63,0,176,11,0,106,1,0,20,1,36,64,
		0,176,11,0,106,6,87,47,78,43,42,0,20,1,
		36,65,0,176,20,0,92,23,121,20,2,176,21,0,
		106,8,91,69,78,84,69,82,93,0,20,1,36,66,
		0,176,20,0,92,23,92,21,20,2,176,21,0,106,
		6,91,69,83,67,93,0,20,1,36,67,0,176,20,
		0,92,23,92,37,20,2,176,21,0,106,9,91,69,
		83,80,65,135,79,93,0,20,1,36,68,0,176,20,
		0,92,23,92,59,20,2,176,21,0,106,7,91,26,
		27,24,25,93,0,20,1,36,69,0,176,11,0,106,
		1,0,20,1,36,70,0,176,11,0,106,9,87,47,
		78,44,43,87,47,82,0,20,1,36,71,0,176,20,
		0,92,23,92,7,20,2,176,21,0,106,13,80,97,
		114,97,32,65,108,116,101,114,97,114,0,20,1,36,
		72,0,176,20,0,92,23,92,26,20,2,176,21,0,
		106,10,80,97,114,97,32,83,97,105,114,0,20,1,
		36,73,0,176,20,0,92,23,92,45,20,2,176,21,
		0,106,13,80,97,114,97,32,73,110,99,108,117,105,
		114,0,20,1,36,74,0,176,20,0,92,23,92,65,
		20,2,176,21,0,106,13,77,111,118,105,109,101,110,
		116,97,135,198,111,0,20,1,36,75,0,176,22,0,
		92,7,122,92,19,92,78,109,12,0,106,7,70,95,
		67,67,79,82,0,109,13,0,109,14,0,106,2,196,
		0,106,2,179,0,106,2,205,0,20,11,36,76,0,
		176,23,0,20,0,176,6,0,106,2,49,0,20,1,
		176,24,0,100,20,1,36,77,0,176,25,0,121,121,
		176,2,0,12,0,176,3,0,12,0,109,4,0,20,
		5,36,78,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_CCOR )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,27,0,1,71,28,0,2,36,83,0,
		176,16,0,108,29,108,30,20,2,36,84,0,176,31,
		0,12,0,83,30,0,36,85,0,122,83,29,0,36,
		86,0,109,27,0,92,4,8,29,106,8,36,88,0,
		109,30,0,92,32,8,29,158,7,36,89,0,176,32,
		0,20,0,36,90,0,176,33,0,122,20,1,36,91,
		0,176,19,0,106,3,48,49,0,20,1,36,92,0,
		176,34,0,106,27,67,111,110,102,105,114,109,97,32,
		73,110,99,108,117,115,135,111,32,40,83,47,78,41,
		32,63,32,0,20,1,36,93,0,109,35,0,106,2,
		83,0,5,29,57,7,36,94,0,176,1,0,121,121,
		176,2,0,12,0,176,3,0,12,0,12,4,83,4,
		0,36,96,0,121,83,36,0,36,97,0,121,83,37,
		0,36,98,0,176,38,0,92,10,12,1,83,39,0,
		36,99,0,176,38,0,92,20,12,1,83,40,0,36,
		100,0,176,38,0,92,30,12,1,83,41,0,36,101,
		0,176,38,0,92,30,12,1,83,42,0,36,102,0,
		176,38,0,92,2,12,1,83,43,0,36,103,0,121,
		83,44,0,36,104,0,121,83,45,0,36,105,0,176,
		46,0,92,10,92,5,92,20,92,75,20,4,176,47,
		0,92,10,92,5,20,2,36,106,0,176,18,0,92,
		10,92,5,92,20,92,75,92,2,20,5,36,107,0,
		176,11,0,106,9,87,47,78,44,43,87,47,82,0,
		20,1,36,108,0,176,48,0,106,18,67,65,68,65,
		83,84,82,79,32,68,65,32,67,79,78,84,65,0,
		92,11,20,2,36,110,0,176,20,0,92,13,92,7,
		20,2,176,21,0,106,17,67,162,100,105,103,111,32,
		32,32,32,32,32,32,32,58,32,0,20,1,176,47,
		0,176,49,0,12,0,176,50,0,12,0,122,72,20,
		2,176,51,0,109,52,0,176,53,0,100,106,8,101,
		67,79,68,73,71,79,0,106,6,64,82,32,57,57,
		0,100,100,12,5,20,2,48,54,0,176,55,0,109,
		52,0,12,1,112,0,73,36,111,0,176,56,0,109,
		52,0,100,100,100,100,100,100,20,7,4,0,0,83,
		52,0,109,52,0,73,36,112,0,109,36,0,121,69,
		29,120,255,36,113,0,176,57,0,109,36,0,100,100,
		20,3,36,114,0,176,58,0,12,0,28,63,36,115,
		0,176,19,0,106,3,48,49,0,20,1,36,116,0,
		176,59,0,106,30,67,162,100,105,103,111,32,100,101,
		32,67,111,110,116,97,32,106,135,32,67,97,100,97,
		115,116,114,97,100,111,0,92,3,20,2,36,117,0,
		26,36,255,36,124,0,176,20,0,92,12,92,7,20,
		2,176,21,0,106,17,67,162,100,105,103,111,32,66,
		97,110,99,111,32,32,58,32,0,20,1,176,47,0,
		176,49,0,12,0,176,50,0,12,0,122,72,20,2,
		176,51,0,109,52,0,176,53,0,100,106,9,101,67,
		79,68,95,66,65,78,0,106,8,64,82,32,57,57,
		57,57,0,100,100,12,5,20,2,48,54,0,176,55,
		0,109,52,0,12,1,112,0,73,36,125,0,176,56,
		0,109,52,0,100,100,100,100,100,100,20,7,4,0,
		0,83,52,0,109,52,0,73,36,126,0,109,37,0,
		121,69,29,117,255,36,127,0,176,6,0,106,2,49,
		0,20,1,36,128,0,176,57,0,109,37,0,100,100,
		20,3,36,129,0,176,58,0,12,0,31,64,36,130,
		0,176,19,0,106,3,48,49,0,20,1,36,131,0,
		176,59,0,106,31,67,162,100,105,103,111,32,100,101,
		32,66,97,110,99,111,32,78,198,111,32,67,97,100,
		97,115,116,114,97,100,111,0,92,3,20,2,36,132,
		0,26,20,255,36,134,0,176,20,0,92,12,92,30,
		20,2,176,21,0,109,60,0,20,1,36,135,0,176,
		6,0,106,2,50,0,20,1,36,140,0,176,20,0,
		92,13,92,7,20,2,176,21,0,106,17,67,162,100,
		105,103,111,32,65,103,135,110,99,105,97,58,32,0,
		20,1,176,47,0,176,49,0,12,0,176,50,0,12,
		0,122,72,20,2,176,51,0,109,52,0,176,53,0,
		100,106,9,101,67,79,68,95,65,71,69,0,100,100,
		100,12,5,20,2,48,54,0,176,55,0,109,52,0,
		12,1,112,0,73,36,141,0,176,20,0,92,14,92,
		7,20,2,176,21,0,106,17,67,162,100,105,103,111,
		32,67,47,67,32,32,32,32,58,32,0,20,1,176,
		47,0,176,49,0,12,0,176,50,0,12,0,122,72,
		20,2,176,51,0,109,52,0,176,53,0,100,106,9,
		101,67,79,68,95,67,67,79,0,100,100,100,12,5,
		20,2,48,54,0,176,55,0,109,52,0,12,1,112,
		0,73,36,142,0,176,20,0,92,15,92,7,20,2,
		176,21,0,106,17,78,111,109,101,32,65,103,135,110,
		99,105,97,32,32,58,32,0,20,1,176,47,0,176,
		49,0,12,0,176,50,0,12,0,122,72,20,2,176,
		51,0,109,52,0,176,53,0,100,106,9,101,78,79,
		77,69,95,65,71,0,100,100,100,12,5,20,2,48,
		54,0,176,55,0,109,52,0,12,1,112,0,73,36,
		143,0,176,20,0,92,16,92,7,20,2,176,21,0,
		106,17,67,105,100,97,100,101,32,32,32,32,32,32,
		32,32,58,32,0,20,1,176,47,0,176,49,0,12,
		0,176,50,0,12,0,122,72,20,2,176,51,0,109,
		52,0,176,53,0,100,106,8,101,67,73,68,65,68,
		69,0,100,100,100,12,5,20,2,48,54,0,176,55,
		0,109,52,0,12,1,112,0,73,36,144,0,176,20,
		0,92,17,92,7,20,2,176,21,0,106,17,69,115,
		116,97,100,111,32,32,32,32,32,32,32,32,58,32,
		0,20,1,176,47,0,176,49,0,12,0,176,50,0,
		12,0,122,72,20,2,176,51,0,109,52,0,176,53,
		0,100,106,8,101,69,83,84,65,68,79,0,100,100,
		100,12,5,20,2,48,54,0,176,55,0,109,52,0,
		12,1,112,0,73,36,145,0,176,20,0,92,18,92,
		7,20,2,176,21,0,106,17,76,105,109,105,116,101,
		32,67,114,135,100,105,116,111,58,32,0,20,1,176,
		47,0,176,49,0,12,0,176,50,0,12,0,122,72,
		20,2,176,51,0,109,52,0,176,53,0,100,106,8,
		101,76,73,77,73,84,69,0,106,18,64,82,32,57,
		57,57,44,57,57,57,44,57,57,57,46,57,57,0,
		100,100,12,5,20,2,48,54,0,176,55,0,109,52,
		0,12,1,112,0,73,36,146,0,176,20,0,92,19,
		92,7,20,2,176,21,0,106,17,67,111,110,116,97,
		32,67,111,110,116,97,98,105,108,58,32,0,20,1,
		176,47,0,176,49,0,12,0,176,50,0,12,0,122,
		72,20,2,176,51,0,109,52,0,176,53,0,100,106,
		10,101,67,79,78,84,65,66,73,76,0,106,9,64,
		82,32,57,57,57,57,57,0,100,100,12,5,20,2,
		48,54,0,176,55,0,109,52,0,12,1,112,0,73,
		36,147,0,176,56,0,109,52,0,100,100,100,100,100,
		100,20,7,4,0,0,83,52,0,109,52,0,73,36,
		148,0,176,19,0,106,3,48,49,0,20,1,36,149,
		0,176,34,0,106,18,67,111,110,102,105,114,109,97,
		32,40,83,47,78,41,32,63,32,0,20,1,36,150,
		0,109,35,0,106,2,83,0,5,29,94,1,36,151,
		0,176,61,0,106,22,71,82,65,86,65,78,68,79,
		44,32,65,71,85,65,82,68,69,32,46,46,46,0,
		20,1,36,152,0,176,6,0,106,2,50,0,20,1,
		36,153,0,176,62,0,20,0,36,154,0,176,63,0,
		12,0,28,73,36,157,0,109,36,0,78,64,0,109,
		37,0,78,65,0,109,39,0,78,66,0,36,160,0,
		109,40,0,78,67,0,109,41,0,78,68,0,109,42,
		0,78,69,0,36,163,0,109,43,0,78,70,0,109,
		44,0,78,71,0,109,45,0,78,72,0,36,164,0,
		176,73,0,20,0,36,166,0,176,6,0,106,2,51,
		0,20,1,36,167,0,176,74,0,109,36,0,92,5,
		12,2,83,75,0,36,168,0,106,3,48,48,0,83,
		76,0,36,169,0,106,7,67,79,78,84,65,95,0,
		176,77,0,109,76,0,122,92,2,176,78,0,176,79,
		0,109,75,0,12,1,12,1,49,12,3,72,176,79,
		0,109,75,0,12,1,72,83,80,0,36,170,0,106,
		7,67,79,78,84,65,95,0,176,77,0,109,76,0,
		122,92,2,176,78,0,176,79,0,109,75,0,12,1,
		12,1,49,12,3,72,176,79,0,109,75,0,12,1,
		72,83,81,0,36,171,0,176,82,0,109,80,0,4,
		0,0,20,2,36,172,0,176,7,0,9,100,109,80,
		0,100,100,9,20,6,36,173,0,176,83,0,109,81,
		0,106,5,68,65,84,65,0,90,6,109,84,0,6,
		100,20,4,36,174,0,176,9,0,20,0,36,175,0,
		176,6,0,106,2,50,0,20,1,36,181,0,176,25,
		0,121,121,176,2,0,12,0,176,3,0,12,0,109,
		4,0,20,5,36,183,0,176,33,0,121,20,1,36,
		184,0,176,85,0,20,0,26,195,0,36,185,0,109,
		30,0,92,13,8,29,165,0,36,186,0,176,32,0,
		20,0,36,187,0,176,63,0,12,0,29,136,0,36,
		188,0,176,33,0,122,20,1,36,189,0,98,12,0,
		109,28,0,1,83,86,0,36,190,0,98,13,0,109,
		28,0,1,83,87,0,36,191,0,176,47,0,176,49,
		0,12,0,176,50,0,12,0,20,2,176,51,0,109,
		52,0,176,53,0,100,109,86,0,109,87,0,100,100,
		12,5,20,2,48,54,0,176,55,0,109,52,0,12,
		1,112,0,73,36,192,0,176,56,0,109,52,0,100,
		100,100,100,100,100,20,7,4,0,0,83,52,0,109,
		52,0,73,36,193,0,176,33,0,121,20,1,36,194,
		0,176,73,0,20,0,36,196,0,176,85,0,20,0,
		25,20,36,197,0,109,30,0,92,27,8,28,9,36,
		198,0,121,83,29,0,36,201,0,109,29,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}
