/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/tab_forn.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( TAB_FORN );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( F_FORN );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( SETCURSOR );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( __XSAVESCREEN );
HB_FUNC_EXTERN( __XRESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TAB_FORN )
{ "TAB_FORN", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( TAB_FORN )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "ROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROW )}, NULL },
{ "COL", {HB_FS_PUBLIC}, {HB_FUNCNAME( COL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "F_FORN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_FORN )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "TELA05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SETCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCURSOR )}, NULL },
{ "TELA04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "FORN_CPG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ECLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NOME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "__XSAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XSAVESCREEN )}, NULL },
{ "FLD_NAME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FLD_PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__XRESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XRESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TAB_FORN, "C:/sis/SAN/xhb/tab_forn.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TAB_FORN
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TAB_FORN )
   #include "hbiniseg.h"
#endif

HB_FUNC( TAB_FORN )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,14,0,176,5,
		0,106,35,84,65,66,69,76,65,32,70,79,82,78,
		69,67,69,68,79,82,69,83,32,67,79,78,84,65,
		83,32,65,32,80,65,71,65,82,0,20,1,36,15,
		0,176,6,0,106,2,51,0,20,1,36,16,0,176,
		7,0,9,100,106,9,67,76,65,83,95,70,79,82,
		0,100,100,9,20,6,176,8,0,106,9,67,76,65,
		83,95,70,79,82,0,20,1,36,17,0,176,9,0,
		20,0,36,18,0,176,6,0,106,2,50,0,20,1,
		36,19,0,176,7,0,9,100,106,7,83,84,65,84,
		85,83,0,100,100,9,20,6,36,20,0,176,9,0,
		20,0,36,21,0,176,10,0,106,3,48,49,0,20,
		1,36,22,0,176,11,0,106,46,68,101,115,101,106,
		97,32,67,111,110,115,117,108,116,97,32,112,111,114,
		32,79,114,100,101,109,32,65,108,102,97,98,130,116,
		105,99,97,32,40,83,47,78,41,32,63,32,0,20,
		1,36,23,0,109,12,0,106,2,78,0,5,28,80,
		36,24,0,176,6,0,106,2,49,0,20,1,36,25,
		0,176,7,0,9,100,106,9,70,79,82,78,69,67,
		69,68,0,100,100,9,20,6,176,8,0,106,9,70,
		79,82,78,69,67,69,68,0,20,1,176,8,0,106,
		9,84,69,77,80,95,70,79,82,0,20,1,36,26,
		0,176,9,0,20,0,25,78,36,28,0,176,6,0,
		106,2,49,0,20,1,36,29,0,176,7,0,9,100,
		106,9,70,79,82,78,69,67,69,68,0,100,100,9,
		20,6,176,8,0,106,9,84,69,77,80,95,70,79,
		82,0,20,1,176,8,0,106,9,70,79,82,78,69,
		67,69,68,0,20,1,36,30,0,176,9,0,20,0,
		36,32,0,176,10,0,106,3,48,49,0,20,1,36,
		33,0,121,83,13,0,36,34,0,176,14,0,92,23,
		121,20,2,176,15,0,106,52,73,110,102,111,114,109,
		101,32,97,32,67,108,97,115,115,101,32,100,101,32,
		70,111,114,110,101,99,101,100,111,114,32,111,117,32,
		40,48,41,32,112,97,114,97,32,84,111,100,97,115,
		32,58,32,0,20,1,176,16,0,176,17,0,12,0,
		176,18,0,12,0,122,72,20,2,176,19,0,109,20,
		0,176,21,0,100,106,8,101,67,76,65,83,83,69,
		0,106,7,64,82,32,57,57,57,0,100,100,12,5,
		20,2,48,22,0,176,23,0,109,20,0,12,1,112,
		0,73,36,35,0,176,24,0,109,20,0,100,100,100,
		100,100,100,20,7,4,0,0,83,20,0,109,20,0,
		73,36,36,0,109,13,0,121,69,28,59,36,37,0,
		176,6,0,106,2,49,0,20,1,36,38,0,176,25,
		0,90,10,109,26,0,109,13,0,5,6,106,17,67,
		76,65,83,83,69,32,61,32,101,67,76,65,83,83,
		69,0,20,2,36,39,0,176,9,0,20,0,36,41,
		0,176,27,0,20,0,36,42,0,176,28,0,106,9,
		87,47,78,44,43,87,47,82,0,20,1,36,43,0,
		92,24,3,1,0,92,24,3,1,0,92,24,3,1,
		0,92,24,3,1,0,176,33,0,108,29,108,30,108,
		31,108,32,20,4,81,32,0,81,31,0,81,30,0,
		81,29,0,36,44,0,176,34,0,109,29,0,120,109,
		32,0,20,3,36,45,0,106,12,64,82,32,57,57,
		57,57,57,57,57,57,0,98,30,0,122,2,36,46,
		0,106,5,64,83,53,48,0,98,30,0,92,2,2,
		36,47,0,106,7,64,57,57,57,57,57,0,98,30,
		0,92,3,2,36,48,0,106,5,64,83,49,48,0,
		98,30,0,92,4,2,36,49,0,106,5,64,83,50,
		48,0,98,30,0,92,5,2,36,50,0,106,5,64,
		83,49,52,0,98,30,0,92,6,2,36,51,0,106,
		5,64,83,49,57,0,98,30,0,92,7,2,36,52,
		0,106,5,64,83,49,50,0,98,30,0,92,8,2,
		36,53,0,106,4,64,83,50,0,98,30,0,92,9,
		2,36,54,0,106,9,32,32,47,32,32,47,32,32,
		0,98,30,0,92,10,2,36,55,0,106,5,64,83,
		49,48,0,98,30,0,92,11,2,36,56,0,106,4,
		64,83,49,0,98,30,0,92,12,2,36,57,0,106,
		9,32,32,47,32,32,47,32,32,0,98,30,0,92,
		13,2,36,58,0,106,5,64,83,53,48,0,98,30,
		0,92,14,2,36,59,0,106,5,64,83,50,48,0,
		98,30,0,92,15,2,36,60,0,106,5,64,83,50,
		48,0,98,30,0,92,16,2,36,61,0,106,4,64,
		83,50,0,98,30,0,92,17,2,36,62,0,106,13,
		64,82,32,88,88,88,88,88,45,88,88,88,0,98,
		30,0,92,18,2,36,63,0,106,20,64,82,32,40,
		88,88,88,88,41,32,88,88,88,88,45,88,88,88,
		88,0,98,30,0,92,19,2,36,64,0,106,20,64,
		82,32,40,88,88,88,88,41,32,88,88,88,88,45,
		88,88,88,88,0,98,30,0,92,20,2,36,65,0,
		106,5,64,83,56,48,0,98,30,0,92,21,2,36,
		66,0,106,5,64,83,56,48,0,98,30,0,92,22,
		2,36,67,0,106,5,64,83,56,48,0,98,30,0,
		92,23,2,36,68,0,106,4,64,83,49,0,98,30,
		0,92,24,2,36,69,0,106,7,67,162,100,105,103,
		111,0,98,31,0,122,2,36,70,0,106,5,78,111,
		109,101,0,98,31,0,92,2,2,36,71,0,106,8,
		65,112,101,108,105,100,111,0,98,31,0,92,3,2,
		36,72,0,106,7,67,108,97,115,115,101,0,98,31,
		0,92,4,2,36,73,0,106,8,67,111,110,116,97,
		116,111,0,98,31,0,92,5,2,36,74,0,106,8,
		67,80,70,47,67,71,67,0,98,31,0,92,6,2,
		36,75,0,106,10,73,110,115,99,114,105,135,198,111,
		0,98,31,0,92,7,2,36,76,0,106,3,82,71,
		0,98,31,0,92,8,2,36,77,0,106,6,85,70,
		32,82,71,0,98,31,0,92,9,2,36,78,0,106,
		8,69,109,105,115,115,198,111,0,98,31,0,92,10,
		2,36,79,0,106,6,79,114,103,198,111,0,98,31,
		0,92,11,2,36,80,0,106,5,83,101,120,111,0,
		98,31,0,92,12,2,36,81,0,106,11,78,97,115,
		99,105,109,101,110,116,111,0,98,31,0,92,13,2,
		36,82,0,106,9,69,110,100,101,114,101,135,111,0,
		98,31,0,92,14,2,36,83,0,106,7,66,97,105,
		114,114,111,0,98,31,0,92,15,2,36,84,0,106,
		7,67,105,100,97,100,101,0,98,31,0,92,16,2,
		36,85,0,106,7,69,115,116,97,100,111,0,98,31,
		0,92,17,2,36,86,0,106,4,67,69,80,0,98,
		31,0,92,18,2,36,87,0,106,9,84,101,108,101,
		102,111,110,101,0,98,31,0,92,19,2,36,88,0,
		106,10,70,97,120,83,161,109,105,108,101,0,98,31,
		0,92,20,2,36,89,0,106,12,79,98,115,101,114,
		118,97,135,228,101,115,0,98,31,0,92,21,2,36,
		90,0,106,12,79,98,115,101,114,118,97,135,228,101,
		115,0,98,31,0,92,22,2,36,91,0,106,12,79,
		98,115,101,114,118,97,135,228,101,115,0,98,31,0,
		92,23,2,36,92,0,106,5,84,105,112,111,0,98,
		31,0,92,24,2,36,93,0,176,35,0,92,6,121,
		92,19,92,79,92,2,20,5,36,94,0,176,10,0,
		106,3,48,49,0,20,1,36,95,0,176,28,0,106,
		1,0,20,1,36,96,0,176,28,0,106,6,87,47,
		78,43,42,0,20,1,36,97,0,176,14,0,92,23,
		121,20,2,176,15,0,106,8,91,69,78,84,69,82,
		93,0,20,1,36,98,0,176,14,0,92,23,92,21,
		20,2,176,15,0,106,6,91,69,83,67,93,0,20,
		1,36,99,0,176,14,0,92,23,92,37,20,2,176,
		15,0,106,9,91,69,83,80,65,128,79,93,0,20,
		1,36,100,0,176,14,0,92,23,92,59,20,2,176,
		15,0,106,7,91,26,27,24,25,93,0,20,1,36,
		101,0,176,28,0,106,1,0,20,1,36,102,0,176,
		28,0,106,9,87,47,78,44,43,87,47,82,0,20,
		1,36,103,0,176,14,0,92,23,92,7,20,2,176,
		15,0,106,13,80,97,114,97,32,65,108,116,101,114,
		97,114,0,20,1,36,104,0,176,14,0,92,23,92,
		26,20,2,176,15,0,106,10,80,97,114,97,32,83,
		97,105,114,0,20,1,36,105,0,176,14,0,92,23,
		92,45,20,2,176,15,0,106,13,80,97,114,97,32,
		73,110,99,108,117,105,114,0,20,1,36,106,0,176,
		14,0,92,23,92,65,20,2,176,15,0,106,13,77,
		111,118,105,109,101,110,116,97,135,131,111,0,20,1,
		36,107,0,176,36,0,92,7,122,92,19,92,78,109,
		29,0,106,7,70,95,70,79,82,78,0,109,30,0,
		109,31,0,106,2,196,0,106,2,179,0,106,2,205,
		0,20,11,36,108,0,176,37,0,20,0,176,6,0,
		106,2,49,0,20,1,176,38,0,100,20,1,36,109,
		0,176,39,0,121,121,176,2,0,12,0,176,3,0,
		12,0,109,4,0,20,5,36,110,0,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_FORN )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,41,0,1,71,42,0,2,36,115,0,
		176,33,0,108,43,108,44,20,2,36,116,0,176,45,
		0,12,0,83,44,0,36,117,0,122,83,43,0,36,
		118,0,109,41,0,92,4,8,29,78,4,36,120,0,
		109,44,0,92,32,8,29,127,3,36,121,0,176,1,
		0,121,121,176,2,0,12,0,176,3,0,12,0,12,
		4,83,46,0,36,122,0,176,47,0,122,20,1,36,
		123,0,176,10,0,106,3,48,49,0,20,1,36,124,
		0,176,11,0,106,27,67,111,110,102,105,114,109,97,
		32,73,110,99,108,117,115,198,111,32,40,83,47,78,
		41,32,63,32,0,20,1,36,125,0,109,12,0,106,
		2,83,0,5,29,252,2,36,126,0,176,1,0,121,
		121,176,2,0,12,0,176,3,0,12,0,12,4,83,
		48,0,36,128,0,176,49,0,92,5,92,5,92,12,
		92,75,20,4,176,16,0,92,5,92,5,20,2,36,
		129,0,176,35,0,92,5,92,5,92,12,92,75,92,
		2,20,5,36,130,0,176,28,0,106,9,87,47,78,
		44,43,87,47,82,0,20,1,36,131,0,176,6,0,
		106,2,50,0,20,1,36,132,0,109,50,0,83,51,
		0,36,133,0,176,14,0,92,6,92,7,20,2,176,
		15,0,106,15,67,162,100,105,103,111,32,32,32,32,
		32,32,58,32,0,20,1,176,16,0,176,17,0,12,
		0,176,18,0,12,0,122,72,20,2,176,19,0,109,
		20,0,176,21,0,100,106,8,101,67,79,68,73,71,
		79,0,100,100,100,12,5,20,2,48,22,0,176,23,
		0,109,20,0,12,1,112,0,73,36,134,0,176,52,
		0,120,20,1,4,0,0,83,20,0,109,20,0,73,
		36,135,0,176,53,0,92,50,12,1,83,54,0,36,
		136,0,176,14,0,92,8,92,7,20,2,176,15,0,
		106,17,67,108,105,101,110,116,101,32,32,32,32,32,
		32,32,58,32,0,20,1,176,16,0,176,17,0,12,
		0,176,18,0,12,0,122,72,20,2,176,19,0,109,
		20,0,176,21,0,100,106,9,101,67,76,73,69,78,
		84,69,0,100,100,100,12,5,20,2,48,22,0,176,
		23,0,109,20,0,12,1,112,0,73,36,137,0,176,
		24,0,109,20,0,100,100,100,100,100,100,20,7,4,
		0,0,83,20,0,109,20,0,73,36,138,0,176,6,
		0,106,2,51,0,20,1,36,139,0,121,83,13,0,
		36,140,0,176,14,0,92,10,92,7,20,2,176,15,
		0,106,17,67,108,97,115,115,101,32,32,32,32,32,
		32,32,32,58,32,0,20,1,176,16,0,176,17,0,
		12,0,176,18,0,12,0,122,72,20,2,176,19,0,
		109,20,0,176,21,0,100,106,8,101,67,76,65,83,
		83,69,0,106,7,64,82,32,57,57,57,0,100,100,
		12,5,20,2,48,22,0,176,23,0,109,20,0,12,
		1,112,0,73,36,141,0,176,24,0,109,20,0,100,
		100,100,100,100,100,20,7,4,0,0,83,20,0,109,
		20,0,73,36,142,0,176,10,0,106,3,48,49,0,
		20,1,36,143,0,176,11,0,106,18,67,111,110,102,
		105,114,109,97,32,40,83,47,78,41,32,63,32,0,
		20,1,36,144,0,109,12,0,106,2,83,0,5,29,
		186,0,36,145,0,176,55,0,106,22,71,82,65,86,
		65,78,68,79,44,32,65,71,85,65,82,68,69,32,
		46,46,46,0,20,1,36,146,0,176,6,0,106,2,
		49,0,20,1,36,147,0,176,56,0,20,0,36,148,
		0,176,57,0,12,0,28,31,36,151,0,109,51,0,
		78,58,0,109,54,0,78,59,0,109,13,0,78,26,
		0,36,152,0,176,60,0,20,0,36,154,0,176,6,
		0,106,2,50,0,20,1,36,155,0,176,7,0,9,
		100,106,7,83,84,65,84,85,83,0,100,100,9,20,
		6,36,156,0,176,9,0,20,0,36,157,0,109,50,
		0,122,72,83,51,0,36,158,0,176,57,0,12,0,
		28,19,36,159,0,109,51,0,78,50,0,36,160,0,
		176,60,0,20,0,36,162,0,176,6,0,106,2,49,
		0,20,1,36,168,0,176,39,0,121,121,176,2,0,
		12,0,176,3,0,12,0,109,48,0,20,5,36,170,
		0,176,47,0,121,20,1,36,171,0,176,39,0,121,
		121,176,2,0,12,0,176,3,0,12,0,109,46,0,
		20,5,26,198,0,36,172,0,109,44,0,92,13,8,
		29,168,0,36,173,0,176,57,0,12,0,29,175,0,
		36,174,0,176,61,0,20,0,36,175,0,176,47,0,
		122,20,1,36,176,0,98,29,0,109,42,0,1,83,
		62,0,36,177,0,98,30,0,109,42,0,1,83,63,
		0,36,178,0,176,16,0,176,17,0,12,0,176,18,
		0,12,0,20,2,176,19,0,109,20,0,176,21,0,
		100,109,62,0,109,63,0,100,100,12,5,20,2,48,
		22,0,176,23,0,109,20,0,12,1,112,0,73,36,
		179,0,176,24,0,109,20,0,100,100,100,100,100,100,
		20,7,4,0,0,83,20,0,109,20,0,73,36,180,
		0,176,47,0,121,20,1,36,181,0,176,60,0,20,
		0,36,182,0,176,64,0,20,0,36,183,0,25,20,
		36,184,0,109,44,0,92,27,8,28,9,36,185,0,
		121,83,43,0,36,188,0,109,43,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

