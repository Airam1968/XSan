/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_ctrc.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_CTRC );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
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
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DBCLOSEAREA );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_CTRC )
{ "CON_CTRC", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_CTRC )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "ORDE_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EORDE_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ORDE_CLAR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EORDE_CLAR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ORDE_CLAS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EORDE_CLAS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "ECONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EOPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLASSE_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "PESQUISA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLASSE_RESC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEAREA )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "DATA_PREV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "ETOT_PREV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_TITU", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PREVISTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REALIZADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_CTRC, "C:/sis/SAN/xhb/con_ctrc.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_CTRC
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_CTRC )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_CTRC )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,99,116,114,99,46,112,114,103,
		58,67,79,78,95,67,84,82,67,0,36,14,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,15,0,176,5,0,106,39,67,
		79,78,83,85,76,84,65,32,67,79,78,84,65,83,
		32,65,32,82,69,67,69,66,69,82,32,45,32,80,
		79,82,32,67,76,65,83,83,69,0,20,1,36,16,
		0,176,6,0,106,3,48,49,0,20,1,36,17,0,
		176,7,0,106,2,51,0,20,1,36,18,0,176,8,
		0,9,100,106,7,83,84,65,84,85,83,0,100,100,
		9,20,6,36,19,0,176,9,0,20,0,36,20,0,
		176,10,0,12,0,28,37,36,21,0,109,11,0,83,
		12,0,36,22,0,109,13,0,83,14,0,36,23,0,
		109,15,0,83,16,0,36,24,0,176,17,0,20,0,
		36,26,0,176,7,0,106,2,50,0,20,1,36,27,
		0,176,8,0,9,100,106,7,67,79,78,84,65,83,
		0,100,100,9,20,6,176,18,0,106,7,67,79,78,
		84,65,83,0,20,1,36,28,0,176,9,0,20,0,
		36,29,0,176,7,0,106,2,51,0,20,1,36,30,
		0,176,8,0,9,100,106,9,67,76,65,83,83,69,
		95,82,0,100,100,9,20,6,176,18,0,106,9,67,
		76,65,83,83,69,95,82,0,20,1,36,31,0,176,
		9,0,20,0,36,32,0,176,7,0,106,2,52,0,
		20,1,36,33,0,176,8,0,9,100,106,9,67,76,
		73,69,78,84,69,83,0,100,100,9,20,6,176,18,
		0,106,9,67,76,73,69,78,84,69,83,0,20,1,
		36,34,0,176,9,0,20,0,36,35,0,36,36,0,
		121,83,19,0,36,37,0,176,6,0,106,3,48,49,
		0,20,1,36,38,0,176,20,0,92,23,121,20,2,
		176,21,0,106,53,73,110,102,111,114,109,101,32,111,
		32,78,111,46,32,100,97,32,67,111,110,116,97,32,
		67,111,114,114,101,110,116,101,32,111,117,32,40,48,
		41,32,112,97,114,97,32,84,111,100,97,115,32,63,
		32,0,20,1,176,22,0,176,23,0,12,0,176,24,
		0,12,0,122,72,20,2,176,25,0,109,26,0,176,
		27,0,100,106,7,101,67,79,78,84,65,0,106,7,
		64,82,32,57,57,57,0,100,100,12,5,20,2,48,
		28,0,176,29,0,109,26,0,12,1,112,0,73,36,
		39,0,176,30,0,109,26,0,100,100,100,100,100,100,
		20,7,4,0,0,83,26,0,109,26,0,73,36,40,
		0,176,7,0,106,2,50,0,20,1,36,41,0,176,
		31,0,109,19,0,100,100,20,3,36,42,0,176,32,
		0,12,0,31,90,36,45,0,109,19,0,121,5,31,
		80,36,48,0,176,6,0,106,3,48,49,0,20,1,
		36,49,0,176,33,0,106,10,65,116,101,110,135,198,
		111,32,33,0,92,3,20,2,36,50,0,176,34,0,
		106,19,67,111,110,116,97,32,78,198,111,32,69,120,
		105,115,116,101,32,33,0,20,1,36,51,0,176,35,
		0,20,0,36,52,0,26,206,254,36,56,0,176,6,
		0,106,3,48,49,0,20,1,36,57,0,176,36,0,
		122,12,1,83,37,0,36,58,0,176,20,0,92,23,
		121,20,2,176,21,0,106,66,67,111,110,115,117,108,
		116,97,32,112,101,108,97,32,68,97,116,97,32,40,
		80,41,114,101,118,105,115,116,97,44,32,40,82,41,
		101,97,108,32,111,117,32,40,78,41,111,116,97,32,
		70,105,115,99,97,108,32,40,80,47,82,47,78,41,
		32,63,32,0,20,1,176,22,0,176,23,0,12,0,
		176,24,0,12,0,122,72,20,2,176,25,0,109,26,
		0,176,27,0,100,106,7,101,79,80,67,65,79,0,
		106,4,64,33,88,0,90,51,51,67,58,47,115,105,
		115,47,83,65,78,47,120,104,98,47,99,111,110,95,
		99,116,114,99,46,112,114,103,58,67,79,78,95,67,
		84,82,67,0,109,37,0,106,4,80,82,78,0,24,
		6,100,12,5,20,2,36,58,0,48,28,0,176,29,
		0,109,26,0,12,1,112,0,73,36,59,0,176,30,
		0,109,26,0,100,100,100,100,100,100,20,7,4,0,
		0,83,26,0,109,26,0,73,36,60,0,176,6,0,
		106,3,48,49,0,20,1,36,61,0,134,0,0,0,
		0,83,38,0,36,62,0,134,0,0,0,0,83,39,
		0,36,63,0,36,64,0,176,20,0,92,23,92,2,
		20,2,176,21,0,106,11,80,101,114,105,111,100,111,
		32,58,32,0,20,1,176,22,0,176,23,0,12,0,
		176,24,0,12,0,122,72,20,2,176,25,0,109,26,
		0,176,27,0,100,106,10,101,68,65,84,65,95,73,
		78,73,0,100,100,100,12,5,20,2,48,28,0,176,
		29,0,109,26,0,12,1,112,0,73,36,65,0,176,
		20,0,92,23,92,25,20,2,176,21,0,106,7,65,
		116,101,32,58,32,0,20,1,176,22,0,176,23,0,
		12,0,176,24,0,12,0,122,72,20,2,176,25,0,
		109,26,0,176,27,0,100,106,10,101,68,65,84,65,
		95,70,73,77,0,100,100,100,12,5,20,2,48,28,
		0,176,29,0,109,26,0,12,1,112,0,73,36,66,
		0,176,30,0,109,26,0,100,100,100,100,100,100,20,
		7,4,0,0,83,26,0,109,26,0,73,36,67,0,
		109,38,0,134,0,0,0,0,5,31,13,109,39,0,
		134,0,0,0,0,5,28,51,36,68,0,176,6,0,
		106,3,48,49,0,20,1,36,69,0,176,33,0,106,
		18,68,97,116,97,115,32,73,110,118,160,108,105,100,
		97,115,32,33,0,92,3,20,2,36,70,0,26,235,
		254,36,72,0,109,38,0,109,39,0,15,28,59,36,
		73,0,176,6,0,106,3,48,49,0,20,1,36,74,
		0,176,33,0,106,26,68,97,116,97,32,73,110,105,
		99,105,111,32,62,32,81,117,101,32,70,105,110,97,
		108,32,33,0,92,3,20,2,36,75,0,26,166,254,
		36,82,0,176,7,0,106,2,51,0,20,1,36,83,
		0,176,8,0,9,100,106,9,67,76,65,83,83,69,
		95,82,0,100,100,9,20,6,176,18,0,106,9,67,
		76,65,83,83,69,95,82,0,20,1,36,84,0,176,
		9,0,20,0,36,85,0,109,14,0,32,139,2,36,
		86,0,121,83,40,0,36,87,0,36,88,0,176,6,
		0,106,3,48,49,0,20,1,36,89,0,176,20,0,
		92,23,121,20,2,176,21,0,106,17,67,111,100,46,
		32,82,101,99,101,105,116,97,32,32,58,32,0,20,
		1,176,22,0,176,23,0,12,0,176,24,0,12,0,
		122,72,20,2,176,25,0,109,26,0,176,27,0,100,
		106,10,101,67,76,65,83,83,69,95,82,0,106,12,
		64,82,32,57,57,57,57,57,57,57,57,0,100,100,
		12,5,20,2,48,28,0,176,29,0,109,26,0,12,
		1,112,0,73,36,90,0,176,30,0,109,26,0,100,
		100,100,100,100,100,20,7,4,0,0,83,26,0,109,
		26,0,73,36,91,0,176,41,0,106,9,87,47,78,
		44,43,87,47,82,0,20,1,36,92,0,176,6,0,
		106,3,48,49,0,20,1,36,93,0,176,20,0,92,
		23,121,20,2,176,21,0,106,17,67,111,100,46,32,
		82,101,99,101,105,116,97,32,32,58,32,0,20,1,
		36,94,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,42,0,36,95,0,176,7,
		0,106,2,51,0,20,1,36,96,0,176,9,0,20,
		0,36,97,0,109,40,0,121,69,28,15,36,98,0,
		176,31,0,109,40,0,100,100,20,3,36,100,0,92,
		2,3,1,0,92,2,3,1,0,92,2,3,1,0,
		92,2,3,1,0,176,47,0,108,43,108,44,108,45,
		108,46,20,4,81,46,0,81,45,0,81,44,0,81,
		43,0,36,101,0,176,48,0,109,43,0,120,109,46,
		0,20,3,36,102,0,106,8,64,82,57,57,57,57,
		57,0,98,44,0,122,2,36,103,0,106,5,64,83,
		52,48,0,98,44,0,92,2,2,36,104,0,106,7,
		67,162,100,105,103,111,0,98,45,0,122,2,36,105,
		0,106,21,68,101,115,99,114,105,135,198,111,32,100,
		97,32,82,101,99,101,105,116,97,0,98,45,0,92,
		2,2,36,106,0,176,49,0,92,7,92,19,92,18,
		92,79,92,2,20,5,36,107,0,176,50,0,92,8,
		92,20,92,18,92,78,109,43,0,106,10,70,67,79,
		78,95,67,84,80,71,0,109,44,0,109,45,0,106,
		2,196,0,106,2,179,0,106,2,205,0,20,11,36,
		108,0,109,51,0,83,40,0,36,109,0,176,52,0,
		121,121,176,2,0,12,0,176,3,0,12,0,109,42,
		0,20,5,36,110,0,176,6,0,106,3,48,49,0,
		20,1,36,111,0,176,20,0,92,23,121,20,2,176,
		21,0,106,17,67,111,100,46,32,82,101,99,101,105,
		116,97,32,32,58,32,0,20,1,36,112,0,176,20,
		0,92,23,92,21,20,2,176,21,0,176,53,0,109,
		51,0,92,5,12,2,106,4,32,45,32,0,72,176,
		54,0,109,55,0,12,1,72,20,1,36,113,0,176,
		7,0,106,2,49,0,20,1,36,114,0,36,115,0,
		26,167,2,36,117,0,176,36,0,92,30,12,1,83,
		56,0,36,118,0,36,119,0,176,6,0,106,3,48,
		49,0,20,1,36,120,0,176,20,0,92,23,121,20,
		2,176,21,0,106,17,67,111,100,46,32,82,101,99,
		101,105,116,97,32,32,58,32,0,20,1,176,22,0,
		176,23,0,12,0,176,24,0,12,0,122,72,20,2,
		176,25,0,109,26,0,176,27,0,100,106,13,101,67,
		76,65,83,83,69,95,82,69,83,67,0,106,5,64,
		83,51,48,0,100,100,12,5,20,2,48,28,0,176,
		29,0,109,26,0,12,1,112,0,73,36,121,0,176,
		30,0,109,26,0,100,100,100,100,100,100,20,7,4,
		0,0,83,26,0,109,26,0,73,36,122,0,176,54,
		0,109,56,0,12,1,83,56,0,36,123,0,176,7,
		0,106,2,51,0,20,1,36,124,0,176,9,0,20,
		0,36,125,0,109,56,0,176,36,0,92,30,12,1,
		69,28,15,36,126,0,176,31,0,109,56,0,100,100,
		20,3,36,128,0,176,41,0,106,9,87,47,78,44,
		43,87,47,82,0,20,1,36,129,0,176,6,0,106,
		3,48,49,0,20,1,36,130,0,176,20,0,92,23,
		121,20,2,176,21,0,106,17,67,111,100,46,32,82,
		101,99,101,105,116,97,32,32,58,32,0,20,1,36,
		131,0,176,1,0,121,121,176,2,0,12,0,176,3,
		0,12,0,12,4,83,42,0,36,132,0,92,2,3,
		1,0,92,2,3,1,0,92,2,3,1,0,92,2,
		3,1,0,176,47,0,108,43,108,44,108,45,108,46,
		20,4,81,46,0,81,45,0,81,44,0,81,43,0,
		36,133,0,176,48,0,109,43,0,120,109,46,0,20,
		3,36,134,0,106,8,64,82,57,57,57,57,57,0,
		98,44,0,122,2,36,135,0,106,5,64,83,52,48,
		0,98,44,0,92,2,2,36,136,0,106,7,67,162,
		100,105,103,111,0,98,45,0,122,2,36,137,0,106,
		21,68,101,115,99,114,105,135,198,111,32,100,97,32,
		82,101,99,101,105,116,97,0,98,45,0,92,2,2,
		36,138,0,176,49,0,92,7,92,19,92,18,92,79,
		92,2,20,5,36,139,0,176,50,0,92,8,92,20,
		92,18,92,78,109,43,0,106,10,70,67,79,78,95,
		67,84,80,71,0,109,44,0,109,45,0,106,2,196,
		0,106,2,179,0,106,2,205,0,20,11,36,140,0,
		109,51,0,83,40,0,36,141,0,109,55,0,83,56,
		0,36,142,0,176,52,0,121,121,176,2,0,12,0,
		176,3,0,12,0,109,42,0,20,5,36,143,0,176,
		6,0,106,3,48,49,0,20,1,36,144,0,176,20,
		0,92,23,121,20,2,176,21,0,106,17,67,111,100,
		46,32,82,101,99,101,105,116,97,32,32,58,32,0,
		20,1,36,145,0,176,20,0,92,23,92,21,20,2,
		176,21,0,176,53,0,109,51,0,92,5,12,2,106,
		4,32,45,32,0,72,176,54,0,109,55,0,12,1,
		72,20,1,36,146,0,176,7,0,106,2,49,0,20,
		1,36,147,0,36,150,0,176,7,0,106,2,51,0,
		20,1,36,151,0,176,57,0,20,0,36,153,0,109,
		37,0,106,2,78,0,5,29,79,2,36,154,0,109,
		19,0,121,5,29,16,1,36,155,0,176,7,0,106,
		2,49,0,20,1,36,156,0,176,8,0,9,100,106,
		9,67,82,69,67,69,66,69,82,0,100,100,9,20,
		6,176,18,0,106,8,68,65,84,65,95,78,70,0,
		20,1,176,18,0,106,9,68,65,84,65,95,67,82,
		86,0,20,1,176,18,0,106,9,67,82,69,67,69,
		66,69,82,0,20,1,176,18,0,106,9,68,65,84,
		65,95,67,82,69,0,20,1,36,157,0,176,58,0,
		90,70,51,67,58,47,115,105,115,47,83,65,78,47,
		120,104,98,47,99,111,110,95,99,116,114,99,46,112,
		114,103,58,67,79,78,95,67,84,82,67,0,109,59,
		0,109,38,0,16,21,28,21,73,109,59,0,109,39,
		0,34,21,28,10,73,109,60,0,109,40,0,5,6,
		106,79,68,65,84,65,95,80,82,69,86,32,62,61,
		32,101,68,65,84,65,95,73,78,73,32,46,65,78,
		68,46,32,68,65,84,65,95,80,82,69,86,32,60,
		61,32,101,68,65,84,65,95,70,73,77,32,46,65,
		78,68,46,32,67,76,65,83,83,69,95,82,32,61,
		32,101,67,76,65,83,83,69,95,82,0,20,2,36,
		158,0,176,9,0,20,0,26,232,5,36,160,0,176,
		7,0,106,2,49,0,20,1,36,161,0,176,8,0,
		9,100,106,9,67,82,69,67,69,66,69,82,0,100,
		100,9,20,6,176,18,0,106,8,68,65,84,65,95,
		78,70,0,20,1,176,18,0,106,9,68,65,84,65,
		95,67,82,86,0,20,1,176,18,0,106,9,67,82,
		69,67,69,66,69,82,0,20,1,176,18,0,106,9,
		68,65,84,65,95,67,82,69,0,20,1,36,162,0,
		176,58,0,90,81,51,67,58,47,115,105,115,47,83,
		65,78,47,120,104,98,47,99,111,110,95,99,116,114,
		99,46,112,114,103,58,67,79,78,95,67,84,82,67,
		0,109,59,0,109,38,0,16,21,28,32,73,109,59,
		0,109,39,0,34,21,28,21,73,109,60,0,109,40,
		0,5,21,28,10,73,109,61,0,109,19,0,5,6,
		106,104,68,65,84,65,95,80,82,69,86,32,62,61,
		32,101,68,65,84,65,95,73,78,73,32,46,65,78,
		68,46,32,68,65,84,65,95,80,82,69,86,32,60,
		61,32,101,68,65,84,65,95,70,73,77,32,46,65,
		78,68,46,32,67,76,65,83,83,69,95,82,32,61,
		32,101,67,76,65,83,83,69,95,82,32,46,65,78,
		68,46,32,67,79,68,95,67,79,78,84,65,32,61,
		32,101,67,79,78,84,65,0,20,2,36,163,0,176,
		9,0,20,0,36,164,0,26,180,4,36,165,0,109,
		37,0,106,2,80,0,5,29,79,2,36,166,0,109,
		19,0,121,5,29,16,1,36,167,0,176,7,0,106,
		2,49,0,20,1,36,168,0,176,8,0,9,100,106,
		9,67,82,69,67,69,66,69,82,0,100,100,9,20,
		6,176,18,0,106,9,68,65,84,65,95,67,82,86,
		0,20,1,176,18,0,106,9,67,82,69,67,69,66,
		69,82,0,20,1,176,18,0,106,9,68,65,84,65,
		95,67,82,69,0,20,1,176,18,0,106,8,68,65,
		84,65,95,78,70,0,20,1,36,169,0,176,58,0,
		90,70,51,67,58,47,115,105,115,47,83,65,78,47,
		120,104,98,47,99,111,110,95,99,116,114,99,46,112,
		114,103,58,67,79,78,95,67,84,82,67,0,109,59,
		0,109,38,0,16,21,28,21,73,109,59,0,109,39,
		0,34,21,28,10,73,109,60,0,109,40,0,5,6,
		106,79,68,65,84,65,95,80,82,69,86,32,62,61,
		32,101,68,65,84,65,95,73,78,73,32,46,65,78,
		68,46,32,68,65,84,65,95,80,82,69,86,32,60,
		61,32,101,68,65,84,65,95,70,73,77,32,46,65,
		78,68,46,32,67,76,65,83,83,69,95,82,32,61,
		32,101,67,76,65,83,83,69,95,82,0,20,2,36,
		170,0,176,9,0,20,0,26,142,3,36,172,0,176,
		7,0,106,2,49,0,20,1,36,173,0,176,8,0,
		9,100,106,9,67,82,69,67,69,66,69,82,0,100,
		100,9,20,6,176,18,0,106,9,68,65,84,65,95,
		67,82,86,0,20,1,176,18,0,106,9,67,82,69,
		67,69,66,69,82,0,20,1,176,18,0,106,9,68,
		65,84,65,95,67,82,69,0,20,1,176,18,0,106,
		8,68,65,84,65,95,78,70,0,20,1,36,174,0,
		176,58,0,90,81,51,67,58,47,115,105,115,47,83,
		65,78,47,120,104,98,47,99,111,110,95,99,116,114,
		99,46,112,114,103,58,67,79,78,95,67,84,82,67,
		0,109,59,0,109,38,0,16,21,28,32,73,109,59,
		0,109,39,0,34,21,28,21,73,109,60,0,109,40,
		0,5,21,28,10,73,109,61,0,109,19,0,5,6,
		106,104,68,65,84,65,95,80,82,69,86,32,62,61,
		32,101,68,65,84,65,95,73,78,73,32,46,65,78,
		68,46,32,68,65,84,65,95,80,82,69,86,32,60,
		61,32,101,68,65,84,65,95,70,73,77,32,46,65,
		78,68,46,32,67,76,65,83,83,69,95,82,32,61,
		32,101,67,76,65,83,83,69,95,82,32,46,65,78,
		68,46,32,67,79,68,95,67,79,78,84,65,32,61,
		32,101,67,79,78,84,65,0,20,2,36,175,0,176,
		9,0,20,0,36,176,0,26,90,2,36,177,0,109,
		37,0,106,2,82,0,5,29,76,2,36,178,0,109,
		19,0,121,5,29,16,1,36,179,0,176,7,0,106,
		2,49,0,20,1,36,180,0,176,8,0,9,100,106,
		9,67,82,69,67,69,66,69,82,0,100,100,9,20,
		6,176,18,0,106,9,68,65,84,65,95,67,82,69,
		0,20,1,176,18,0,106,9,67,82,69,67,69,66,
		69,82,0,20,1,176,18,0,106,9,68,65,84,65,
		95,67,82,86,0,20,1,176,18,0,106,8,68,65,
		84,65,95,78,70,0,20,1,36,181,0,176,58,0,
		90,70,51,67,58,47,115,105,115,47,83,65,78,47,
		120,104,98,47,99,111,110,95,99,116,114,99,46,112,
		114,103,58,67,79,78,95,67,84,82,67,0,109,62,
		0,109,38,0,16,21,28,21,73,109,62,0,109,39,
		0,34,21,28,10,73,109,60,0,109,40,0,5,6,
		106,79,68,65,84,65,95,82,69,65,76,32,62,61,
		32,101,68,65,84,65,95,73,78,73,32,46,65,78,
		68,46,32,68,65,84,65,95,82,69,65,76,32,60,
		61,32,101,68,65,84,65,95,70,73,77,32,46,65,
		78,68,46,32,67,76,65,83,83,69,95,82,32,61,
		32,101,67,76,65,83,83,69,95,82,0,20,2,36,
		182,0,176,9,0,20,0,26,52,1,36,184,0,176,
		7,0,106,2,49,0,20,1,36,185,0,176,8,0,
		9,100,106,9,67,82,69,67,69,66,69,82,0,100,
		100,9,20,6,176,18,0,106,9,68,65,84,65,95,
		67,82,69,0,20,1,176,18,0,106,9,67,82,69,
		67,69,66,69,82,0,20,1,176,18,0,106,9,68,
		65,84,65,95,67,82,86,0,20,1,176,18,0,106,
		8,68,65,84,65,95,78,70,0,20,1,36,186,0,
		176,58,0,90,81,51,67,58,47,115,105,115,47,83,
		65,78,47,120,104,98,47,99,111,110,95,99,116,114,
		99,46,112,114,103,58,67,79,78,95,67,84,82,67,
		0,109,62,0,109,38,0,16,21,28,32,73,109,62,
		0,109,39,0,34,21,28,21,73,109,60,0,109,40,
		0,5,21,28,10,73,109,61,0,109,19,0,5,6,
		106,104,68,65,84,65,95,82,69,65,76,32,62,61,
		32,101,68,65,84,65,95,73,78,73,32,46,65,78,
		68,46,32,68,65,84,65,95,82,69,65,76,32,60,
		61,32,101,68,65,84,65,95,70,73,77,32,46,65,
		78,68,46,32,67,76,65,83,83,69,95,82,32,61,
		32,101,67,76,65,83,83,69,95,82,32,46,65,78,
		68,46,32,67,79,68,95,67,79,78,84,65,32,61,
		32,101,67,79,78,84,65,0,20,2,36,187,0,176,
		9,0,20,0,36,188,0,36,190,0,176,7,0,106,
		2,49,0,20,1,36,191,0,176,9,0,20,0,36,
		192,0,176,63,0,20,0,36,193,0,176,41,0,106,
		9,87,47,78,44,43,87,47,82,0,20,1,36,194,
		0,92,13,3,1,0,92,13,3,1,0,92,13,3,
		1,0,92,13,3,1,0,176,47,0,108,43,108,44,
		108,45,108,46,20,4,81,46,0,81,45,0,81,44,
		0,81,43,0,36,195,0,176,48,0,109,43,0,120,
		109,46,0,20,3,36,196,0,106,12,64,82,32,57,
		57,57,57,57,57,57,57,0,98,44,0,122,2,36,
		197,0,106,12,64,82,32,57,57,57,57,57,57,57,
		57,0,98,44,0,92,2,2,36,198,0,106,12,64,
		82,32,57,57,57,57,57,57,57,57,0,98,44,0,
		92,3,2,36,199,0,106,12,64,82,32,57,57,57,
		57,57,57,57,57,0,98,44,0,92,4,2,36,200,
		0,106,5,64,83,53,48,0,98,44,0,92,5,2,
		36,201,0,106,3,64,68,0,98,44,0,92,6,2,
		36,202,0,106,22,64,82,32,57,57,57,44,57,57,
		57,44,57,57,57,44,57,57,57,46,57,57,0,98,
		44,0,92,7,2,36,203,0,106,3,64,68,0,98,
		44,0,92,8,2,36,204,0,106,22,64,82,32,57,
		57,57,44,57,57,57,44,57,57,57,44,57,57,57,
		46,57,57,0,98,44,0,92,9,2,36,205,0,106,
		10,64,82,32,57,57,57,57,57,57,0,98,44,0,
		92,10,2,36,206,0,106,3,64,68,0,98,44,0,
		92,11,2,36,207,0,106,5,64,83,50,48,0,98,
		44,0,92,12,2,36,208,0,106,3,64,68,0,98,
		44,0,92,13,2,36,209,0,106,11,76,97,110,135,
		97,109,101,110,116,111,0,98,45,0,122,2,36,210,
		0,106,12,67,108,97,115,115,101,32,82,101,99,46,
		0,98,45,0,92,2,2,36,211,0,106,15,32,32,
		32,80,114,111,100,117,116,111,32,32,32,32,0,98,
		45,0,92,3,2,36,212,0,106,8,67,108,105,101,
		110,116,101,0,98,45,0,92,4,2,36,213,0,106,
		10,68,101,115,99,114,105,135,198,111,0,98,45,0,
		92,5,2,36,214,0,106,11,68,97,116,97,32,80,
		114,101,118,46,0,98,45,0,92,6,2,36,215,0,
		106,14,86,108,114,46,32,80,114,101,118,105,115,116,
		111,0,98,45,0,92,7,2,36,216,0,106,11,68,
		97,116,97,32,80,103,116,111,46,0,98,45,0,92,
		8,2,36,217,0,106,10,86,108,114,46,32,80,97,
		103,111,0,98,45,0,92,9,2,36,218,0,106,11,
		67,111,100,46,32,67,111,110,116,97,0,98,45,0,
		92,10,2,36,219,0,106,12,68,97,116,97,32,67,
		111,109,112,101,46,0,98,45,0,92,11,2,36,220,
		0,106,12,78,117,109,46,32,68,111,99,117,109,46,
		0,98,45,0,92,12,2,36,221,0,106,8,68,97,
		116,97,32,78,70,0,98,45,0,92,13,2,36,222,
		0,176,49,0,92,6,121,92,20,92,79,92,2,20,
		5,36,223,0,176,6,0,106,3,48,49,0,20,1,
		36,224,0,176,41,0,106,1,0,20,1,36,225,0,
		176,41,0,106,6,87,47,78,43,42,0,20,1,36,
		226,0,176,20,0,92,23,121,20,2,176,21,0,106,
		6,91,69,83,67,93,0,20,1,36,227,0,176,20,
		0,92,23,92,20,20,2,176,21,0,106,7,91,26,
		27,24,25,93,0,20,1,36,228,0,176,41,0,106,
		1,0,20,1,36,229,0,176,41,0,106,9,87,47,
		78,44,43,87,47,82,0,20,1,36,230,0,176,20,
		0,92,23,92,5,20,2,176,21,0,106,13,80,97,
		114,97,32,65,108,116,101,114,97,114,0,20,1,36,
		231,0,176,20,0,92,23,92,26,20,2,176,21,0,
		106,13,77,111,118,105,109,101,110,116,97,135,198,111,
		0,20,1,36,232,0,176,50,0,92,7,122,92,20,
		92,78,109,43,0,106,9,70,95,67,79,78,67,84,
		82,0,109,44,0,109,45,0,106,2,196,0,106,2,
		179,0,106,2,205,0,20,11,36,233,0,121,83,64,
		0,36,234,0,121,83,65,0,36,235,0,121,83,66,
		0,36,236,0,176,9,0,20,0,36,237,0,176,67,
		0,12,0,31,50,36,238,0,109,64,0,109,68,0,
		72,83,64,0,36,239,0,109,65,0,109,69,0,72,
		83,65,0,36,240,0,109,66,0,122,72,83,66,0,
		36,241,0,176,70,0,122,20,1,25,200,36,243,0,
		176,63,0,20,0,36,244,0,176,20,0,92,10,92,
		10,20,2,176,21,0,106,21,84,111,116,97,108,32,
		80,114,101,118,105,115,116,111,32,32,32,32,58,32,
		0,20,1,176,22,0,176,23,0,12,0,176,24,0,
		12,0,122,72,20,2,176,25,0,109,26,0,176,27,
		0,100,106,10,101,84,79,84,95,80,82,69,86,0,
		106,18,64,82,32,57,57,57,44,57,57,57,44,57,
		57,57,46,57,57,0,100,100,12,5,20,2,48,28,
		0,176,29,0,109,26,0,12,1,112,0,73,36,245,
		0,176,20,0,92,12,92,10,20,2,176,21,0,106,
		21,84,111,116,97,108,32,82,101,97,108,105,122,97,
		100,111,32,32,32,58,32,0,20,1,176,22,0,176,
		23,0,12,0,176,24,0,12,0,122,72,20,2,176,
		25,0,109,26,0,176,27,0,100,106,10,101,84,79,
		84,95,82,69,65,76,0,106,18,64,82,32,57,57,
		57,44,57,57,57,44,57,57,57,46,57,57,0,100,
		100,12,5,20,2,48,28,0,176,29,0,109,26,0,
		12,1,112,0,73,36,246,0,176,20,0,92,14,92,
		10,20,2,176,21,0,106,21,84,111,116,97,108,32,
		84,105,116,117,108,111,115,32,32,32,32,32,58,32,
		0,20,1,176,22,0,176,23,0,12,0,176,24,0,
		12,0,122,72,20,2,176,25,0,109,26,0,176,27,
		0,100,106,10,101,84,79,84,95,84,73,84,85,0,
		106,18,64,82,32,57,57,57,44,57,57,57,44,57,
		57,57,46,57,57,0,100,100,12,5,20,2,48,28,
		0,176,29,0,109,26,0,12,1,112,0,73,36,247,
		0,176,6,0,106,3,48,49,0,20,1,36,248,0,
		176,34,0,106,44,80,114,101,115,115,105,111,110,101,
		32,81,117,97,108,113,117,101,114,32,84,101,99,108,
		97,32,112,97,114,97,32,70,105,110,97,108,105,122,
		97,114,32,46,46,46,0,20,1,36,249,0,176,35,
		0,20,0,36,250,0,176,71,0,20,0,176,7,0,
		106,2,49,0,20,1,176,72,0,100,20,1,36,251,
		0,176,52,0,121,121,176,2,0,12,0,176,3,0,
		12,0,109,4,0,20,5,36,252,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,99,116,114,99,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,32,192,255,253,255,39,31,
		255,127,15,252,255,247,255,239,127,255,255,207,126,255,
		247,127,223,255,255,255,255,255,255,251,31,0,4,3,
		0,4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

