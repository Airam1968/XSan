/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_reca.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_RECA );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( F_BLOQA );
HB_FUNC_EXTERN( DBEVAL );
HB_FUNC_EXTERN( DBDELETE );
HB_FUNC_EXTERN( __DBPACK );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC( F_CAIXA );
HB_FUNC_EXTERN( LASTKEY );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_RECA )
{ "CON_RECA", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_RECA )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "F_BLOQA", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQA )}, NULL },
{ "DBEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEVAL )}, NULL },
{ "DBDELETE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBDELETE )}, NULL },
{ "__DBPACK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBPACK )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "EDATA_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "ESUB_SALD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_APLD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_APLB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_TOTG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_TOTG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EZERO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "EARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "DATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "SALDO_F", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "APLICACAOD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "APLICACAOB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EAPLICD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EAPLICB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESALDO_F", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "SALDOF", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TOTALGERAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
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
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "F_CAIXA", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_CAIXA )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_RECA, "C:/sis/SAN/xhb/con_reca.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_RECA
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_RECA )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_RECA )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,29,67,79,78,83,85,76,84,65,32,68,69,
		32,82,69,83,85,77,79,32,68,69,32,67,79,78,
		84,65,83,0,20,1,36,16,0,176,6,0,106,3,
		48,49,0,20,1,36,17,0,176,7,0,106,18,67,
		111,110,102,105,114,109,97,32,40,83,47,78,41,32,
		63,32,0,20,1,36,18,0,109,8,0,106,2,78,
		0,5,28,31,36,19,0,176,9,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,4,0,20,5,36,
		20,0,100,110,7,36,22,0,176,10,0,106,2,49,
		0,20,1,36,23,0,176,11,0,92,8,106,3,79,
		78,0,20,2,36,24,0,176,12,0,9,100,106,9,
		82,69,83,85,77,79,67,67,0,100,100,9,20,6,
		36,25,0,176,13,0,20,0,36,26,0,176,14,0,
		12,0,28,31,36,27,0,176,15,0,90,8,176,16,
		0,12,0,6,100,100,100,100,9,20,6,36,28,0,
		176,17,0,20,0,36,30,0,176,11,0,92,8,106,
		4,79,70,70,0,20,2,36,31,0,176,13,0,20,
		0,36,32,0,176,10,0,106,2,55,0,20,1,36,
		33,0,176,12,0,9,100,106,7,66,65,78,67,79,
		83,0,100,100,9,20,6,176,18,0,106,7,66,65,
		78,67,79,83,0,20,1,36,34,0,176,13,0,20,
		0,36,35,0,176,10,0,106,2,52,0,20,1,36,
		36,0,176,12,0,9,100,106,9,67,76,65,83,83,
		69,95,68,0,100,100,9,20,6,176,18,0,106,9,
		67,76,65,83,83,69,95,68,0,20,1,36,37,0,
		176,13,0,20,0,36,38,0,176,10,0,106,2,53,
		0,20,1,36,39,0,176,12,0,9,100,106,9,67,
		76,65,83,83,69,95,82,0,100,100,9,20,6,176,
		18,0,106,9,67,76,65,83,83,69,95,82,0,20,
		1,36,40,0,176,13,0,20,0,36,41,0,176,10,
		0,106,2,50,0,20,1,36,42,0,176,12,0,9,
		100,106,7,67,79,78,84,65,83,0,100,100,9,20,
		6,176,18,0,106,7,67,79,78,84,65,83,0,20,
		1,36,43,0,176,13,0,20,0,36,44,0,134,0,
		0,0,0,83,19,0,36,45,0,134,0,0,0,0,
		83,20,0,36,47,0,176,6,0,106,3,48,49,0,
		20,1,36,48,0,176,21,0,92,23,92,2,20,2,
		176,22,0,106,15,68,97,116,97,32,82,101,115,117,
		109,111,32,58,32,0,20,1,176,23,0,176,24,0,
		12,0,176,25,0,12,0,122,72,20,2,176,26,0,
		109,27,0,176,28,0,100,106,10,101,68,65,84,65,
		95,73,78,73,0,100,100,100,12,5,20,2,48,29,
		0,176,30,0,109,27,0,12,1,112,0,73,36,49,
		0,176,31,0,109,27,0,100,100,100,100,100,100,20,
		7,4,0,0,83,27,0,109,27,0,73,36,50,0,
		109,19,0,134,0,0,0,0,5,28,49,36,51,0,
		176,6,0,106,3,48,49,0,20,1,36,52,0,176,
		32,0,106,16,68,97,116,97,32,73,110,118,97,108,
		105,100,97,32,33,0,92,3,20,2,36,53,0,26,
		64,255,36,58,0,176,10,0,106,2,50,0,20,1,
		36,59,0,176,13,0,20,0,36,60,0,121,83,33,
		0,36,61,0,121,83,34,0,36,62,0,121,83,35,
		0,36,63,0,121,83,36,0,36,64,0,121,83,37,
		0,36,65,0,176,38,0,12,0,32,201,2,36,66,
		0,176,10,0,106,2,50,0,20,1,36,67,0,176,
		39,0,109,40,0,92,5,12,2,83,41,0,36,68,
		0,106,3,48,48,0,83,42,0,36,69,0,106,7,
		67,79,78,84,65,95,0,176,43,0,109,42,0,122,
		92,2,176,44,0,176,45,0,109,41,0,12,1,12,
		1,49,12,3,72,176,45,0,109,41,0,12,1,72,
		83,46,0,36,70,0,106,7,67,79,78,84,65,95,
		0,176,43,0,109,42,0,122,92,2,176,44,0,176,
		45,0,109,41,0,12,1,12,1,49,12,3,72,176,
		45,0,109,41,0,12,1,72,83,47,0,36,71,0,
		176,10,0,106,2,54,0,20,1,36,72,0,176,12,
		0,9,100,109,46,0,100,100,9,20,6,176,18,0,
		109,47,0,20,1,36,73,0,176,13,0,20,0,36,
		74,0,176,48,0,109,19,0,100,100,20,3,36,75,
		0,176,49,0,12,0,29,3,1,36,76,0,109,50,
		0,109,19,0,5,31,9,176,38,0,12,0,31,13,
		36,77,0,176,51,0,122,20,1,25,228,36,79,0,
		176,51,0,92,255,20,1,36,80,0,121,83,36,0,
		36,81,0,109,52,0,109,53,0,72,109,54,0,72,
		83,36,0,36,82,0,109,33,0,109,52,0,72,83,
		33,0,36,83,0,109,34,0,109,53,0,72,83,34,
		0,36,84,0,109,35,0,109,54,0,72,83,35,0,
		36,85,0,109,37,0,109,36,0,72,83,37,0,36,
		86,0,109,50,0,83,55,0,36,87,0,109,56,0,
		83,57,0,36,88,0,109,53,0,83,58,0,36,89,
		0,109,54,0,83,59,0,36,90,0,109,52,0,83,
		60,0,36,91,0,176,10,0,106,2,49,0,20,1,
		36,92,0,176,61,0,20,0,36,93,0,176,62,0,
		12,0,28,52,36,96,0,109,55,0,78,50,0,109,
		57,0,78,56,0,109,60,0,78,63,0,36,99,0,
		109,58,0,78,53,0,109,59,0,78,54,0,109,36,
		0,78,64,0,36,100,0,176,65,0,20,0,36,102,
		0,176,10,0,106,2,54,0,20,1,26,226,0,36,
		104,0,176,51,0,92,255,20,1,36,105,0,121,83,
		36,0,36,106,0,109,52,0,109,53,0,72,109,54,
		0,72,83,36,0,36,107,0,109,33,0,109,52,0,
		72,83,33,0,36,108,0,109,34,0,109,53,0,72,
		83,34,0,36,109,0,109,35,0,109,54,0,72,83,
		35,0,36,110,0,109,37,0,109,36,0,72,83,37,
		0,36,111,0,109,50,0,83,55,0,36,112,0,109,
		56,0,83,57,0,36,113,0,109,53,0,83,58,0,
		36,114,0,109,54,0,83,59,0,36,115,0,109,52,
		0,83,60,0,36,116,0,176,10,0,106,2,49,0,
		20,1,36,117,0,176,61,0,20,0,36,118,0,176,
		62,0,12,0,28,52,36,121,0,109,55,0,78,50,
		0,109,57,0,78,56,0,109,60,0,78,63,0,36,
		124,0,109,58,0,78,53,0,109,59,0,78,54,0,
		109,36,0,78,64,0,36,125,0,176,65,0,20,0,
		36,127,0,176,10,0,106,2,54,0,20,1,36,129,
		0,176,10,0,106,2,50,0,20,1,36,130,0,176,
		51,0,122,20,1,26,50,253,36,133,0,176,10,0,
		106,2,49,0,20,1,36,134,0,176,13,0,20,0,
		36,135,0,176,66,0,20,0,36,136,0,176,67,0,
		106,9,87,47,78,44,43,87,47,82,0,20,1,36,
		137,0,92,6,3,1,0,92,6,3,1,0,92,6,
		3,1,0,92,6,3,1,0,176,72,0,108,68,108,
		69,108,70,108,71,20,4,81,71,0,81,70,0,81,
		69,0,81,68,0,36,138,0,176,73,0,109,68,0,
		120,109,71,0,20,3,36,139,0,106,3,64,68,0,
		98,69,0,122,2,36,140,0,106,7,64,82,32,57,
		57,57,0,98,69,0,92,2,2,36,141,0,106,18,
		64,82,32,57,57,57,44,57,57,57,44,57,57,57,
		46,57,57,0,98,69,0,92,3,2,36,142,0,106,
		18,64,82,32,57,57,57,44,57,57,57,44,57,57,
		57,46,57,57,0,98,69,0,92,4,2,36,143,0,
		106,18,64,82,32,57,57,57,44,57,57,57,44,57,
		57,57,46,57,57,0,98,69,0,92,5,2,36,144,
		0,106,18,64,82,32,57,57,57,44,57,57,57,44,
		57,57,57,46,57,57,0,98,69,0,92,6,2,36,
		145,0,106,5,68,97,116,97,0,98,70,0,122,2,
		36,146,0,106,6,67,111,110,116,97,0,98,70,0,
		92,2,2,36,147,0,106,12,83,97,108,100,111,32,
		70,105,110,97,108,0,98,70,0,92,3,2,36,148,
		0,106,13,65,112,108,105,99,46,68,101,115,98,108,
		46,0,98,70,0,92,4,2,36,149,0,106,12,65,
		112,108,105,99,46,66,108,111,113,46,0,98,70,0,
		92,5,2,36,150,0,106,12,84,111,116,97,108,32,
		71,101,114,97,108,0,98,70,0,92,6,2,36,151,
		0,176,74,0,92,6,122,92,19,92,79,92,2,20,
		5,36,152,0,176,6,0,106,3,48,49,0,20,1,
		36,153,0,176,67,0,106,1,0,20,1,36,154,0,
		176,67,0,106,6,87,47,78,43,42,0,20,1,36,
		155,0,176,21,0,92,23,121,20,2,176,22,0,106,
		6,91,69,83,67,93,0,20,1,36,156,0,176,67,
		0,106,1,0,20,1,36,157,0,176,67,0,106,9,
		87,47,78,44,43,87,47,82,0,20,1,36,158,0,
		176,21,0,92,23,92,8,20,2,176,22,0,106,10,
		80,97,114,97,32,83,97,105,114,0,20,1,36,159,
		0,176,75,0,92,7,92,2,92,19,92,78,109,68,
		0,106,8,70,95,67,65,73,88,65,0,109,69,0,
		109,70,0,106,2,196,0,106,2,179,0,106,2,205,
		0,20,11,36,160,0,176,21,0,92,21,92,20,20,
		2,176,76,0,109,33,0,106,16,64,82,32,57,44,
		57,57,57,44,57,57,57,46,57,57,0,20,2,36,
		161,0,176,21,0,92,21,92,35,20,2,176,76,0,
		109,34,0,106,16,64,82,32,57,44,57,57,57,44,
		57,57,57,46,57,57,0,20,2,36,162,0,176,21,
		0,92,21,92,50,20,2,176,76,0,109,35,0,106,
		16,64,82,32,57,44,57,57,57,44,57,57,57,46,
		57,57,0,20,2,36,163,0,176,21,0,92,21,92,
		65,20,2,176,76,0,109,37,0,106,16,64,82,32,
		57,44,57,57,57,44,57,57,57,46,57,57,0,20,
		2,36,164,0,176,77,0,20,0,36,165,0,176,78,
		0,20,0,176,10,0,106,2,49,0,20,1,176,79,
		0,100,20,1,36,166,0,176,9,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,4,0,20,5,36,
		167,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_CAIXA )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,81,0,1,71,82,0,2,36,172,0,
		176,72,0,108,83,108,84,20,2,36,173,0,176,85,
		0,12,0,83,84,0,36,174,0,122,83,83,0,36,
		175,0,109,81,0,92,4,8,28,20,36,177,0,109,
		84,0,92,27,8,28,9,36,178,0,121,83,83,0,
		36,181,0,109,83,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

