/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_ctpg.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_CTPG );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( SETPOS );
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
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( F_CONCTP );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( PCENTRA );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( DBCLOSEAREA );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( __XSAVESCREEN );
HB_FUNC_EXTERN( SETCURSOR );
HB_FUNC_EXTERN( __XRESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_CTPG )
{ "CON_CTPG", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_CTPG )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EOPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "ECHEQUE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "EDATA_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "DATA_PREV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "ETOT_PREV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_TITU", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PREVISTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REALIZADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "F_CONCTP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_CONCTP )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "T_PROP1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE_D", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLASSE_D", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "PCENTRA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCENTRA )}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "DBCLOSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEAREA )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "EMPREENDIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLASSE_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CODIGO_FOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECOD_FORN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__XSAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XSAVESCREEN )}, NULL },
{ "SETCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCURSOR )}, NULL },
{ "FLD_NAME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FLD_PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__XRESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XRESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_CTPG, "C:/sis/SAN/xhb/con_ctpg.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_CTPG
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_CTPG )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_CTPG )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,32,67,79,78,83,85,76,84,65,32,67,79,
		78,84,65,83,32,65,32,80,65,71,65,82,32,45,
		32,71,69,82,65,76,0,20,1,36,16,0,176,6,
		0,106,2,51,0,20,1,36,17,0,176,7,0,9,
		100,106,9,67,76,65,83,83,69,95,68,0,100,100,
		9,20,6,176,8,0,106,9,67,76,65,83,83,69,
		95,68,0,20,1,36,18,0,176,9,0,20,0,36,
		19,0,176,6,0,106,2,50,0,20,1,36,20,0,
		176,7,0,9,100,106,7,67,79,78,84,65,83,0,
		100,100,9,20,6,176,8,0,106,7,67,79,78,84,
		65,83,0,20,1,36,21,0,176,9,0,20,0,36,
		24,0,176,9,0,20,0,36,25,0,176,10,0,106,
		3,48,49,0,20,1,36,26,0,176,11,0,122,12,
		1,83,12,0,36,27,0,176,13,0,92,23,121,20,
		2,176,14,0,106,66,67,111,110,115,117,108,116,97,
		32,112,101,108,97,32,68,97,116,97,32,40,80,41,
		114,101,118,105,115,116,97,44,32,40,82,41,101,97,
		108,32,111,117,32,40,78,41,111,116,97,32,70,105,
		115,99,97,108,32,40,80,47,82,47,78,41,32,63,
		32,0,20,1,176,15,0,176,16,0,12,0,176,17,
		0,12,0,122,72,20,2,176,18,0,109,19,0,176,
		20,0,100,106,7,101,79,80,67,65,79,0,106,4,
		64,33,88,0,90,13,109,12,0,106,4,80,82,78,
		0,24,6,100,12,5,20,2,48,21,0,176,22,0,
		109,19,0,12,1,112,0,73,36,28,0,176,23,0,
		109,19,0,100,100,100,100,100,100,20,7,4,0,0,
		83,19,0,109,19,0,73,36,29,0,176,10,0,106,
		3,48,49,0,20,1,36,30,0,176,11,0,122,12,
		1,83,24,0,36,31,0,176,13,0,92,23,121,20,
		2,176,14,0,106,52,68,101,115,101,106,97,32,82,
		101,108,97,116,135,114,105,111,32,112,111,114,32,111,
		114,100,101,109,32,100,101,32,40,68,41,97,116,97,
		32,111,117,32,40,67,41,104,101,113,117,101,32,63,
		32,0,20,1,176,15,0,176,16,0,12,0,176,17,
		0,12,0,122,72,20,2,176,18,0,109,19,0,176,
		20,0,100,106,8,101,67,72,69,81,85,69,0,106,
		4,64,33,88,0,90,12,109,24,0,106,3,68,67,
		0,24,6,100,12,5,20,2,48,21,0,176,22,0,
		109,19,0,12,1,112,0,73,36,32,0,176,23,0,
		109,19,0,100,100,100,100,100,100,20,7,4,0,0,
		83,19,0,109,19,0,73,36,34,0,121,83,25,0,
		36,35,0,176,10,0,106,3,48,49,0,20,1,36,
		36,0,176,13,0,92,23,121,20,2,176,14,0,106,
		53,73,110,102,111,114,109,101,32,111,32,78,111,46,
		32,100,97,32,67,111,110,116,97,32,67,111,114,114,
		101,110,116,101,32,111,117,32,40,48,41,32,112,97,
		114,97,32,84,111,100,97,115,32,63,32,0,20,1,
		176,15,0,176,16,0,12,0,176,17,0,12,0,122,
		72,20,2,176,18,0,109,19,0,176,20,0,100,106,
		7,101,67,79,78,84,65,0,106,7,64,82,32,57,
		57,57,0,100,100,12,5,20,2,48,21,0,176,22,
		0,109,19,0,12,1,112,0,73,36,37,0,176,23,
		0,109,19,0,100,100,100,100,100,100,20,7,4,0,
		0,83,19,0,109,19,0,73,36,38,0,176,6,0,
		106,2,50,0,20,1,36,39,0,176,26,0,109,25,
		0,100,100,20,3,36,40,0,176,27,0,12,0,31,
		90,36,43,0,109,25,0,121,5,31,80,36,46,0,
		176,10,0,106,3,48,49,0,20,1,36,47,0,176,
		28,0,106,10,65,116,101,110,135,198,111,32,33,0,
		92,3,20,2,36,48,0,176,29,0,106,19,67,111,
		110,116,97,32,78,198,111,32,69,120,105,115,116,101,
		32,33,0,20,1,36,49,0,176,30,0,20,0,36,
		50,0,26,209,254,36,54,0,134,0,0,0,0,83,
		31,0,36,55,0,134,0,0,0,0,83,32,0,36,
		57,0,176,10,0,106,3,48,49,0,20,1,36,58,
		0,176,13,0,92,23,92,2,20,2,176,14,0,106,
		11,80,101,114,105,111,100,111,32,58,32,0,20,1,
		176,15,0,176,16,0,12,0,176,17,0,12,0,122,
		72,20,2,176,18,0,109,19,0,176,20,0,100,106,
		10,101,68,65,84,65,95,73,78,73,0,100,100,100,
		12,5,20,2,48,21,0,176,22,0,109,19,0,12,
		1,112,0,73,36,59,0,176,13,0,92,23,92,25,
		20,2,176,14,0,106,7,65,116,101,32,58,32,0,
		20,1,176,15,0,176,16,0,12,0,176,17,0,12,
		0,122,72,20,2,176,18,0,109,19,0,176,20,0,
		100,106,10,101,68,65,84,65,95,70,73,77,0,100,
		100,100,12,5,20,2,48,21,0,176,22,0,109,19,
		0,12,1,112,0,73,36,60,0,176,23,0,109,19,
		0,100,100,100,100,100,100,20,7,4,0,0,83,19,
		0,109,19,0,73,36,61,0,109,31,0,134,0,0,
		0,0,5,31,13,109,32,0,134,0,0,0,0,5,
		28,51,36,62,0,176,10,0,106,3,48,49,0,20,
		1,36,63,0,176,28,0,106,18,68,97,116,97,115,
		32,73,110,118,160,108,105,100,97,115,32,33,0,92,
		3,20,2,36,64,0,26,225,254,36,66,0,109,31,
		0,109,32,0,15,28,59,36,67,0,176,10,0,106,
		3,48,49,0,20,1,36,68,0,176,28,0,106,26,
		68,97,116,97,32,73,110,105,99,105,111,32,62,32,
		81,117,101,32,70,105,110,97,108,32,33,0,92,3,
		20,2,36,69,0,26,156,254,36,76,0,109,24,0,
		106,2,68,0,5,29,178,5,36,78,0,109,12,0,
		106,2,78,0,5,29,218,1,36,80,0,109,25,0,
		121,5,29,208,0,36,81,0,176,6,0,106,2,49,
		0,20,1,36,82,0,176,7,0,9,100,106,7,67,
		80,65,71,65,82,0,100,100,9,20,6,176,8,0,
		106,8,68,65,84,65,95,78,70,0,20,1,176,8,
		0,106,9,68,65,84,65,95,67,80,86,0,20,1,
		176,8,0,106,7,67,80,65,71,65,82,0,20,1,
		176,8,0,106,9,68,65,84,65,95,67,80,71,0,
		20,1,176,8,0,106,9,78,85,77,95,67,72,69,
		81,0,20,1,36,83,0,176,33,0,90,21,109,34,
		0,109,31,0,16,21,28,10,73,109,34,0,109,32,
		0,34,6,106,52,68,65,84,65,95,80,82,69,86,
		32,62,61,32,101,68,65,84,65,95,73,78,73,32,
		46,65,78,68,46,32,68,65,84,65,95,80,82,69,
		86,32,60,61,32,101,68,65,84,65,95,70,73,77,
		0,20,2,36,84,0,176,9,0,20,0,26,158,8,
		36,85,0,109,25,0,121,69,29,147,8,36,86,0,
		176,6,0,106,2,49,0,20,1,36,87,0,176,7,
		0,9,100,106,7,67,80,65,71,65,82,0,100,100,
		9,20,6,176,8,0,106,8,68,65,84,65,95,78,
		70,0,20,1,176,8,0,106,9,68,65,84,65,95,
		67,80,86,0,20,1,176,8,0,106,7,67,80,65,
		71,65,82,0,20,1,176,8,0,106,9,68,65,84,
		65,95,67,80,71,0,20,1,176,8,0,106,9,78,
		85,77,95,67,72,69,81,0,20,1,36,88,0,176,
		33,0,90,32,109,34,0,109,31,0,16,21,28,21,
		73,109,34,0,109,32,0,34,21,28,10,73,109,35,
		0,109,25,0,5,6,106,77,68,65,84,65,95,80,
		82,69,86,32,62,61,32,101,68,65,84,65,95,73,
		78,73,32,46,65,78,68,46,32,68,65,84,65,95,
		80,82,69,86,32,60,61,32,101,68,65,84,65,95,
		70,73,77,32,46,65,78,68,46,32,67,79,68,95,
		67,79,78,84,65,32,61,32,101,67,79,78,84,65,
		0,20,2,36,89,0,176,9,0,20,0,36,90,0,
		26,159,7,36,91,0,109,12,0,106,2,80,0,5,
		29,218,1,36,93,0,109,25,0,121,5,29,208,0,
		36,94,0,176,6,0,106,2,49,0,20,1,36,95,
		0,176,7,0,9,100,106,7,67,80,65,71,65,82,
		0,100,100,9,20,6,176,8,0,106,9,68,65,84,
		65,95,67,80,86,0,20,1,176,8,0,106,7,67,
		80,65,71,65,82,0,20,1,176,8,0,106,9,68,
		65,84,65,95,67,80,71,0,20,1,176,8,0,106,
		9,78,85,77,95,67,72,69,81,0,20,1,176,8,
		0,106,8,68,65,84,65,95,78,70,0,20,1,36,
		96,0,176,33,0,90,21,109,34,0,109,31,0,16,
		21,28,10,73,109,34,0,109,32,0,34,6,106,52,
		68,65,84,65,95,80,82,69,86,32,62,61,32,101,
		68,65,84,65,95,73,78,73,32,46,65,78,68,46,
		32,68,65,84,65,95,80,82,69,86,32,60,61,32,
		101,68,65,84,65,95,70,73,77,0,20,2,36,97,
		0,176,9,0,20,0,26,185,6,36,98,0,109,25,
		0,121,69,29,174,6,36,99,0,176,6,0,106,2,
		49,0,20,1,36,100,0,176,7,0,9,100,106,7,
		67,80,65,71,65,82,0,100,100,9,20,6,176,8,
		0,106,9,68,65,84,65,95,67,80,86,0,20,1,
		176,8,0,106,7,67,80,65,71,65,82,0,20,1,
		176,8,0,106,9,68,65,84,65,95,67,80,71,0,
		20,1,176,8,0,106,9,78,85,77,95,67,72,69,
		81,0,20,1,176,8,0,106,8,68,65,84,65,95,
		78,70,0,20,1,36,101,0,176,33,0,90,32,109,
		34,0,109,31,0,16,21,28,21,73,109,34,0,109,
		32,0,34,21,28,10,73,109,35,0,109,25,0,5,
		6,106,77,68,65,84,65,95,80,82,69,86,32,62,
		61,32,101,68,65,84,65,95,73,78,73,32,46,65,
		78,68,46,32,68,65,84,65,95,80,82,69,86,32,
		60,61,32,101,68,65,84,65,95,70,73,77,32,46,
		65,78,68,46,32,67,79,68,95,67,79,78,84,65,
		32,61,32,101,67,79,78,84,65,0,20,2,36,102,
		0,176,9,0,20,0,36,103,0,26,186,5,36,104,
		0,109,12,0,106,2,82,0,5,29,172,5,36,106,
		0,109,25,0,121,5,29,208,0,36,107,0,176,6,
		0,106,2,49,0,20,1,36,108,0,176,7,0,9,
		100,106,7,67,80,65,71,65,82,0,100,100,9,20,
		6,176,8,0,106,9,68,65,84,65,95,67,80,71,
		0,20,1,176,8,0,106,7,67,80,65,71,65,82,
		0,20,1,176,8,0,106,9,68,65,84,65,95,67,
		80,86,0,20,1,176,8,0,106,9,78,85,77,95,
		67,72,69,81,0,20,1,176,8,0,106,8,68,65,
		84,65,95,78,70,0,20,1,36,109,0,176,33,0,
		90,21,109,36,0,109,31,0,16,21,28,10,73,109,
		36,0,109,32,0,34,6,106,52,68,65,84,65,95,
		82,69,65,76,32,62,61,32,101,68,65,84,65,95,
		73,78,73,32,46,65,78,68,46,32,68,65,84,65,
		95,82,69,65,76,32,60,61,32,101,68,65,84,65,
		95,70,73,77,0,20,2,36,110,0,176,9,0,20,
		0,26,212,4,36,111,0,109,25,0,121,69,29,201,
		4,36,112,0,176,6,0,106,2,49,0,20,1,36,
		113,0,176,7,0,9,100,106,7,67,80,65,71,65,
		82,0,100,100,9,20,6,176,8,0,106,9,68,65,
		84,65,95,67,80,71,0,20,1,176,8,0,106,7,
		67,80,65,71,65,82,0,20,1,176,8,0,106,9,
		68,65,84,65,95,67,80,86,0,20,1,176,8,0,
		106,9,78,85,77,95,67,72,69,81,0,20,1,176,
		8,0,106,8,68,65,84,65,95,78,70,0,20,1,
		36,114,0,176,33,0,90,32,109,36,0,109,31,0,
		16,21,28,21,73,109,36,0,109,32,0,34,21,28,
		10,73,109,35,0,109,25,0,5,6,106,77,68,65,
		84,65,95,82,69,65,76,32,62,61,32,101,68,65,
		84,65,95,73,78,73,32,46,65,78,68,46,32,68,
		65,84,65,95,82,69,65,76,32,60,61,32,101,68,
		65,84,65,95,70,73,77,32,46,65,78,68,46,32,
		67,79,68,95,67,79,78,84,65,32,61,32,101,67,
		79,78,84,65,0,20,2,36,115,0,176,9,0,20,
		0,36,117,0,26,213,3,36,118,0,109,24,0,106,
		2,67,0,5,29,199,3,36,120,0,109,12,0,106,
		2,80,0,5,29,218,1,36,122,0,109,25,0,121,
		5,29,208,0,36,123,0,176,6,0,106,2,49,0,
		20,1,36,124,0,176,7,0,9,100,106,7,67,80,
		65,71,65,82,0,100,100,9,20,6,176,8,0,106,
		9,78,85,77,95,67,72,69,81,0,20,1,176,8,
		0,106,9,68,65,84,65,95,67,80,86,0,20,1,
		176,8,0,106,7,67,80,65,71,65,82,0,20,1,
		176,8,0,106,9,68,65,84,65,95,67,80,71,0,
		20,1,176,8,0,106,8,68,65,84,65,95,78,70,
		0,20,1,36,125,0,176,33,0,90,21,109,34,0,
		109,31,0,16,21,28,10,73,109,34,0,109,32,0,
		34,6,106,52,68,65,84,65,95,80,82,69,86,32,
		62,61,32,101,68,65,84,65,95,73,78,73,32,46,
		65,78,68,46,32,68,65,84,65,95,80,82,69,86,
		32,60,61,32,101,68,65,84,65,95,70,73,77,0,
		20,2,36,126,0,176,9,0,20,0,26,225,2,36,
		127,0,109,25,0,121,69,29,214,2,36,128,0,176,
		6,0,106,2,49,0,20,1,36,129,0,176,7,0,
		9,100,106,7,67,80,65,71,65,82,0,100,100,9,
		20,6,176,8,0,106,9,78,85,77,95,67,72,69,
		81,0,20,1,176,8,0,106,9,68,65,84,65,95,
		67,80,86,0,20,1,176,8,0,106,7,67,80,65,
		71,65,82,0,20,1,176,8,0,106,9,68,65,84,
		65,95,67,80,71,0,20,1,176,8,0,106,8,68,
		65,84,65,95,78,70,0,20,1,36,130,0,176,33,
		0,90,32,109,34,0,109,31,0,16,21,28,21,73,
		109,34,0,109,32,0,34,21,28,10,73,109,35,0,
		109,25,0,5,6,106,77,68,65,84,65,95,80,82,
		69,86,32,62,61,32,101,68,65,84,65,95,73,78,
		73,32,46,65,78,68,46,32,68,65,84,65,95,80,
		82,69,86,32,60,61,32,101,68,65,84,65,95,70,
		73,77,32,46,65,78,68,46,32,67,79,68,95,67,
		79,78,84,65,32,61,32,101,67,79,78,84,65,0,
		20,2,36,131,0,176,9,0,20,0,36,132,0,26,
		226,1,36,133,0,109,12,0,106,2,82,0,5,29,
		212,1,36,135,0,109,25,0,121,5,29,208,0,36,
		136,0,176,6,0,106,2,49,0,20,1,36,137,0,
		176,7,0,9,100,106,7,67,80,65,71,65,82,0,
		100,100,9,20,6,176,8,0,106,9,78,85,77,95,
		67,72,69,81,0,20,1,176,8,0,106,9,68,65,
		84,65,95,67,80,71,0,20,1,176,8,0,106,7,
		67,80,65,71,65,82,0,20,1,176,8,0,106,9,
		68,65,84,65,95,67,80,86,0,20,1,176,8,0,
		106,8,68,65,84,65,95,78,70,0,20,1,36,138,
		0,176,33,0,90,21,109,36,0,109,31,0,16,21,
		28,10,73,109,36,0,109,32,0,34,6,106,52,68,
		65,84,65,95,82,69,65,76,32,62,61,32,101,68,
		65,84,65,95,73,78,73,32,46,65,78,68,46,32,
		68,65,84,65,95,82,69,65,76,32,60,61,32,101,
		68,65,84,65,95,70,73,77,0,20,2,36,139,0,
		176,9,0,20,0,26,252,0,36,140,0,109,25,0,
		121,69,29,241,0,36,141,0,176,6,0,106,2,49,
		0,20,1,36,142,0,176,7,0,9,100,106,7,67,
		80,65,71,65,82,0,100,100,9,20,6,176,8,0,
		106,9,78,85,77,95,67,72,69,81,0,20,1,176,
		8,0,106,9,68,65,84,65,95,67,80,71,0,20,
		1,176,8,0,106,7,67,80,65,71,65,82,0,20,
		1,176,8,0,106,9,68,65,84,65,95,67,80,86,
		0,20,1,176,8,0,106,8,68,65,84,65,95,78,
		70,0,20,1,36,143,0,176,33,0,90,32,109,36,
		0,109,31,0,16,21,28,21,73,109,36,0,109,32,
		0,34,21,28,10,73,109,35,0,109,25,0,5,6,
		106,77,68,65,84,65,95,82,69,65,76,32,62,61,
		32,101,68,65,84,65,95,73,78,73,32,46,65,78,
		68,46,32,68,65,84,65,95,82,69,65,76,32,60,
		61,32,101,68,65,84,65,95,70,73,77,32,46,65,
		78,68,46,32,67,79,68,95,67,79,78,84,65,32,
		61,32,101,67,79,78,84,65,0,20,2,36,144,0,
		176,9,0,20,0,36,148,0,176,6,0,106,2,49,
		0,20,1,36,149,0,176,9,0,20,0,36,150,0,
		176,37,0,20,0,36,151,0,176,38,0,106,9,87,
		47,78,44,43,87,47,82,0,20,1,36,152,0,92,
		17,3,1,0,92,17,3,1,0,92,17,3,1,0,
		92,17,3,1,0,176,43,0,108,39,108,40,108,41,
		108,42,20,4,81,42,0,81,41,0,81,40,0,81,
		39,0,36,153,0,176,44,0,109,39,0,120,109,42,
		0,20,3,36,154,0,106,12,64,82,32,57,57,57,
		57,57,57,57,57,0,98,40,0,122,2,36,155,0,
		106,12,64,82,32,57,57,57,57,57,57,57,57,0,
		98,40,0,92,2,2,36,156,0,106,12,64,82,32,
		57,57,57,57,57,57,57,57,0,98,40,0,92,3,
		2,36,157,0,106,12,64,82,32,57,57,57,57,57,
		57,57,57,0,98,40,0,92,4,2,36,158,0,106,
		5,64,83,53,48,0,98,40,0,92,5,2,36,159,
		0,106,3,64,68,0,98,40,0,92,6,2,36,160,
		0,106,22,64,82,32,57,57,57,44,57,57,57,44,
		57,57,57,44,57,57,57,46,57,57,0,98,40,0,
		92,7,2,36,161,0,106,3,64,68,0,98,40,0,
		92,8,2,36,162,0,106,22,64,82,32,57,57,57,
		44,57,57,57,44,57,57,57,44,57,57,57,46,57,
		57,0,98,40,0,92,9,2,36,163,0,106,10,64,
		82,32,57,57,57,57,57,57,0,98,40,0,92,10,
		2,36,164,0,106,14,64,82,32,57,57,57,57,57,
		57,57,57,57,57,0,98,40,0,92,11,2,36,165,
		0,106,3,64,68,0,98,40,0,92,12,2,36,166,
		0,106,5,64,83,50,48,0,98,40,0,92,13,2,
		36,167,0,106,3,64,68,0,98,40,0,92,14,2,
		36,168,0,106,4,64,83,51,0,98,40,0,92,15,
		2,36,169,0,106,4,64,33,88,0,98,40,0,92,
		16,2,36,170,0,106,2,57,0,98,40,0,92,17,
		2,36,171,0,106,11,76,97,110,135,97,109,101,110,
		116,111,0,98,41,0,122,2,36,172,0,106,13,67,
		108,97,115,115,101,32,68,101,115,112,46,0,98,41,
		0,92,2,2,36,173,0,106,15,32,32,32,32,80,
		114,111,100,117,116,111,32,32,32,0,98,41,0,92,
		3,2,36,174,0,106,11,70,111,114,110,101,99,101,
		100,111,114,0,98,41,0,92,4,2,36,175,0,106,
		10,68,101,115,99,114,105,135,198,111,0,98,41,0,
		92,5,2,36,176,0,106,11,68,97,116,97,32,80,
		114,101,118,46,0,98,41,0,92,6,2,36,177,0,
		106,14,86,108,114,46,32,80,114,101,118,105,115,116,
		111,0,98,41,0,92,7,2,36,178,0,106,11,68,
		97,116,97,32,80,103,116,111,46,0,98,41,0,92,
		8,2,36,179,0,106,10,86,108,114,46,32,80,97,
		103,111,0,98,41,0,92,9,2,36,180,0,106,11,
		67,111,100,46,32,67,111,110,116,97,0,98,41,0,
		92,10,2,36,181,0,106,12,78,117,109,46,32,67,
		104,101,113,117,101,0,98,41,0,92,11,2,36,182,
		0,106,12,68,97,116,97,32,67,111,109,112,101,46,
		0,98,41,0,92,12,2,36,183,0,106,12,78,117,
		109,46,32,68,111,99,117,109,46,0,98,41,0,92,
		13,2,36,184,0,106,8,68,97,116,97,32,78,70,
		0,98,41,0,92,14,2,36,185,0,106,2,32,0,
		98,41,0,92,15,2,36,186,0,106,14,78,111,109,
		105,110,97,108,32,40,83,47,78,41,0,98,41,0,
		92,16,2,36,187,0,106,13,71,114,117,112,111,32,
		67,104,101,113,117,101,0,98,41,0,92,17,2,36,
		188,0,176,45,0,92,6,121,92,20,92,79,92,2,
		20,5,36,189,0,176,10,0,106,3,48,49,0,20,
		1,36,190,0,176,38,0,106,1,0,20,1,36,191,
		0,176,38,0,106,6,87,47,78,43,42,0,20,1,
		36,192,0,176,13,0,92,23,121,20,2,176,14,0,
		106,6,91,69,83,67,93,0,20,1,36,193,0,176,
		13,0,92,23,92,20,20,2,176,14,0,106,7,91,
		26,27,24,25,93,0,20,1,36,194,0,176,38,0,
		106,1,0,20,1,36,195,0,176,38,0,106,9,87,
		47,78,44,43,87,47,82,0,20,1,36,196,0,176,
		13,0,92,23,92,5,20,2,176,14,0,106,13,80,
		97,114,97,32,65,108,116,101,114,97,114,0,20,1,
		36,197,0,176,13,0,92,23,92,26,20,2,176,14,
		0,106,13,77,111,118,105,109,101,110,116,97,135,198,
		111,0,20,1,36,198,0,176,46,0,92,7,122,92,
		20,92,78,109,39,0,106,9,70,95,67,79,78,67,
		84,80,0,109,40,0,109,41,0,106,2,196,0,106,
		2,179,0,106,2,205,0,20,11,36,199,0,121,83,
		47,0,36,200,0,121,83,48,0,36,201,0,121,83,
		49,0,36,202,0,176,9,0,20,0,36,203,0,176,
		50,0,12,0,31,50,36,204,0,109,47,0,109,51,
		0,72,83,47,0,36,205,0,109,48,0,109,52,0,
		72,83,48,0,36,206,0,109,49,0,122,72,83,49,
		0,36,207,0,176,53,0,122,20,1,25,200,36,209,
		0,176,37,0,20,0,36,210,0,176,13,0,92,10,
		92,10,20,2,176,14,0,106,21,84,111,116,97,108,
		32,80,114,101,118,105,115,116,111,32,32,32,32,58,
		32,0,20,1,176,15,0,176,16,0,12,0,176,17,
		0,12,0,122,72,20,2,176,18,0,109,19,0,176,
		20,0,100,106,10,101,84,79,84,95,80,82,69,86,
		0,106,18,64,82,32,57,57,57,44,57,57,57,44,
		57,57,57,46,57,57,0,100,100,12,5,20,2,48,
		21,0,176,22,0,109,19,0,12,1,112,0,73,36,
		211,0,176,13,0,92,12,92,10,20,2,176,14,0,
		106,21,84,111,116,97,108,32,82,101,97,108,105,122,
		97,100,111,32,32,32,58,32,0,20,1,176,15,0,
		176,16,0,12,0,176,17,0,12,0,122,72,20,2,
		176,18,0,109,19,0,176,20,0,100,106,10,101,84,
		79,84,95,82,69,65,76,0,106,18,64,82,32,57,
		57,57,44,57,57,57,44,57,57,57,46,57,57,0,
		100,100,12,5,20,2,48,21,0,176,22,0,109,19,
		0,12,1,112,0,73,36,212,0,176,13,0,92,14,
		92,10,20,2,176,14,0,106,21,84,111,116,97,108,
		32,84,105,116,117,108,111,115,32,32,32,32,32,58,
		32,0,20,1,176,15,0,176,16,0,12,0,176,17,
		0,12,0,122,72,20,2,176,18,0,109,19,0,176,
		20,0,100,106,10,101,84,79,84,95,84,73,84,85,
		0,106,18,64,82,32,57,57,57,44,57,57,57,44,
		57,57,57,46,57,57,0,100,100,12,5,20,2,48,
		21,0,176,22,0,109,19,0,12,1,112,0,73,36,
		213,0,176,10,0,106,3,48,49,0,20,1,36,214,
		0,176,29,0,106,44,80,114,101,115,115,105,111,110,
		101,32,81,117,97,108,113,117,101,114,32,84,101,99,
		108,97,32,112,97,114,97,32,70,105,110,97,108,105,
		122,97,114,32,46,46,46,0,20,1,36,215,0,176,
		30,0,20,0,36,216,0,176,54,0,20,0,176,6,
		0,106,2,49,0,20,1,176,55,0,100,20,1,36,
		217,0,176,56,0,121,121,176,2,0,12,0,176,3,
		0,12,0,109,4,0,20,5,36,218,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_CONCTP )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,58,0,1,71,59,0,2,36,223,0,
		176,43,0,108,60,108,61,20,2,36,224,0,176,62,
		0,12,0,83,61,0,36,225,0,122,83,60,0,36,
		226,0,109,58,0,92,4,8,29,216,4,36,228,0,
		109,61,0,92,13,8,29,186,4,36,229,0,109,59,
		0,92,2,5,29,81,1,36,230,0,176,63,0,12,
		0,29,70,1,36,231,0,176,1,0,121,121,176,2,
		0,12,0,176,3,0,12,0,12,4,83,64,0,36,
		232,0,109,65,0,83,66,0,36,233,0,176,6,0,
		106,2,51,0,20,1,36,234,0,176,7,0,9,100,
		106,9,67,76,65,83,83,69,95,68,0,100,100,9,
		20,6,176,8,0,106,9,67,76,65,83,83,69,95,
		68,0,20,1,36,235,0,176,9,0,20,0,36,236,
		0,176,26,0,109,66,0,100,100,20,3,36,237,0,
		176,67,0,92,12,92,13,92,14,92,67,20,4,176,
		15,0,92,12,92,13,20,2,36,238,0,176,45,0,
		92,12,92,13,92,14,92,67,92,2,20,5,36,239,
		0,176,68,0,106,18,67,108,97,115,115,101,32,100,
		101,32,68,101,115,112,101,115,97,0,92,12,20,2,
		36,240,0,176,15,0,92,13,92,15,20,2,176,18,
		0,109,19,0,176,20,0,100,106,10,68,69,83,67,
		82,73,67,65,79,0,100,100,100,12,5,20,2,48,
		21,0,176,22,0,109,19,0,12,1,112,0,73,36,
		241,0,176,69,0,120,20,1,4,0,0,83,19,0,
		109,19,0,73,36,242,0,176,6,0,106,2,57,0,
		20,1,36,243,0,176,70,0,20,0,36,244,0,176,
		6,0,106,2,49,0,20,1,36,245,0,176,30,0,
		20,0,36,246,0,176,56,0,121,121,176,2,0,12,
		0,176,3,0,12,0,109,64,0,20,5,36,247,0,
		176,71,0,20,0,36,250,0,109,59,0,92,3,5,
		29,75,1,36,251,0,176,63,0,12,0,29,64,1,
		36,252,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,64,0,36,253,0,109,72,
		0,83,73,0,36,254,0,176,6,0,106,2,57,0,
		20,1,36,255,0,176,7,0,9,100,106,9,67,76,
		65,83,95,80,82,79,0,100,100,9,20,6,176,8,
		0,106,9,67,76,65,83,95,80,82,79,0,20,1,
		36,0,1,176,9,0,20,0,36,1,1,176,26,0,
		109,73,0,100,100,20,3,36,2,1,176,67,0,92,
		12,92,13,92,14,92,67,20,4,176,15,0,92,12,
		92,13,20,2,36,3,1,176,45,0,92,12,92,13,
		92,14,92,67,92,2,20,5,36,4,1,176,68,0,
		106,12,9,80,114,111,100,117,116,111,32,32,32,0,
		92,12,20,2,36,5,1,176,15,0,92,13,92,15,
		20,2,176,18,0,109,19,0,176,20,0,100,106,10,
		68,69,83,67,82,73,67,65,79,0,100,100,100,12,
		5,20,2,48,21,0,176,22,0,109,19,0,12,1,
		112,0,73,36,6,1,176,69,0,120,20,1,4,0,
		0,83,19,0,109,19,0,73,36,7,1,176,6,0,
		106,2,57,0,20,1,36,8,1,176,70,0,20,0,
		36,9,1,176,6,0,106,2,49,0,20,1,36,10,
		1,176,30,0,20,0,36,11,1,176,56,0,121,121,
		176,2,0,12,0,176,3,0,12,0,109,64,0,20,
		5,36,12,1,176,71,0,20,0,36,15,1,109,59,
		0,92,4,5,29,69,1,36,16,1,176,63,0,12,
		0,29,58,1,36,17,1,176,1,0,121,121,176,2,
		0,12,0,176,3,0,12,0,12,4,83,64,0,36,
		18,1,109,74,0,83,75,0,36,19,1,176,6,0,
		106,2,57,0,20,1,36,20,1,176,7,0,9,100,
		106,9,70,79,82,78,69,67,69,68,0,100,100,9,
		20,6,176,8,0,106,9,70,79,82,78,69,67,69,
		68,0,20,1,36,21,1,176,9,0,20,0,36,22,
		1,176,26,0,109,75,0,100,100,20,3,36,23,1,
		176,67,0,92,12,92,13,92,14,92,67,20,4,176,
		15,0,92,12,92,13,20,2,36,24,1,176,45,0,
		92,12,92,13,92,14,92,67,92,2,20,5,36,25,
		1,176,68,0,106,11,70,111,114,110,101,99,101,100,
		111,114,0,92,12,20,2,36,26,1,176,15,0,92,
		13,92,15,20,2,176,18,0,109,19,0,176,20,0,
		100,106,5,78,79,77,69,0,100,100,100,12,5,20,
		2,48,21,0,176,22,0,109,19,0,12,1,112,0,
		73,36,27,1,176,69,0,120,20,1,4,0,0,83,
		19,0,109,19,0,73,36,28,1,176,6,0,106,2,
		57,0,20,1,36,29,1,176,70,0,20,0,36,30,
		1,176,6,0,106,2,49,0,20,1,36,31,1,176,
		30,0,20,0,36,32,1,176,56,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,64,0,20,5,36,
		33,1,176,71,0,20,0,36,36,1,109,59,0,92,
		8,16,29,196,0,36,37,1,176,76,0,20,0,36,
		38,1,176,63,0,12,0,29,146,0,36,39,1,176,
		77,0,122,20,1,36,40,1,98,39,0,109,59,0,
		1,83,78,0,36,41,1,98,40,0,109,59,0,1,
		83,79,0,36,42,1,176,63,0,12,0,28,91,36,
		43,1,176,15,0,176,16,0,12,0,176,17,0,12,
		0,20,2,176,18,0,109,19,0,176,20,0,100,109,
		78,0,109,79,0,100,100,12,5,20,2,48,21,0,
		176,22,0,109,19,0,12,1,112,0,73,36,44,1,
		176,23,0,109,19,0,100,100,100,100,100,100,20,7,
		4,0,0,83,19,0,109,19,0,73,36,45,1,176,
		71,0,20,0,36,47,1,176,77,0,121,20,1,36,
		49,1,176,80,0,20,0,36,50,1,25,20,36,51,
		1,109,61,0,92,27,8,28,9,36,52,1,121,83,
		60,0,36,55,1,109,60,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}
