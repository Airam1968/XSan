/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_chnc.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_CHNC );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLQUA );
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
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_CHNC )
{ "CON_CHNC", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_CHNC )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "ECONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
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
{ "EDATA_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "DATA_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REALIZADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_COMPE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "NUM_CHEQUE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PREVISTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_CHNC, "C:/sis/SAN/xhb/con_chnc.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_CHNC
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_CHNC )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_CHNC )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,99,104,110,99,46,112,114,103,
		58,67,79,78,95,67,72,78,67,0,36,14,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,15,0,176,5,0,106,33,67,
		79,78,83,85,76,84,65,32,67,72,69,81,85,69,
		83,32,78,65,79,32,67,79,77,80,69,78,83,65,
		68,79,83,0,20,1,36,16,0,176,6,0,106,2,
		50,0,20,1,36,17,0,176,7,0,9,100,106,7,
		67,79,78,84,65,83,0,100,100,9,20,6,176,8,
		0,106,7,67,79,78,84,65,83,0,20,1,36,18,
		0,176,9,0,20,0,36,19,0,36,20,0,121,83,
		10,0,36,21,0,176,11,0,106,3,48,49,0,20,
		1,36,22,0,176,12,0,92,23,121,20,2,176,13,
		0,106,35,73,110,102,111,114,109,101,32,111,32,78,
		111,46,32,100,97,32,67,111,110,116,97,32,67,111,
		114,114,101,110,116,101,32,63,32,0,20,1,176,14,
		0,176,15,0,12,0,176,16,0,12,0,122,72,20,
		2,176,17,0,109,18,0,176,19,0,100,106,7,101,
		67,79,78,84,65,0,106,7,64,82,32,57,57,57,
		0,100,100,12,5,20,2,48,20,0,176,21,0,109,
		18,0,12,1,112,0,73,36,23,0,176,22,0,109,
		18,0,100,100,100,100,100,100,20,7,4,0,0,83,
		18,0,109,18,0,73,36,24,0,176,6,0,106,2,
		50,0,20,1,36,25,0,176,23,0,109,10,0,100,
		100,20,3,36,26,0,176,24,0,12,0,31,91,36,
		29,0,109,10,0,121,69,29,45,255,36,30,0,176,
		11,0,106,3,48,49,0,20,1,36,31,0,176,25,
		0,106,10,65,116,101,110,135,198,111,32,33,0,92,
		3,20,2,36,32,0,176,26,0,106,19,67,111,110,
		116,97,32,78,198,111,32,69,120,105,115,116,101,32,
		33,0,20,1,36,33,0,176,27,0,20,0,36,34,
		0,26,223,254,36,38,0,134,0,0,0,0,83,28,
		0,36,39,0,134,0,0,0,0,83,29,0,36,40,
		0,36,41,0,176,11,0,106,3,48,49,0,20,1,
		36,42,0,176,12,0,92,23,92,2,20,2,176,13,
		0,106,11,80,101,114,105,111,100,111,32,58,32,0,
		20,1,176,14,0,176,15,0,12,0,176,16,0,12,
		0,122,72,20,2,176,17,0,109,18,0,176,19,0,
		100,106,10,101,68,65,84,65,95,73,78,73,0,100,
		100,100,12,5,20,2,48,20,0,176,21,0,109,18,
		0,12,1,112,0,73,36,43,0,176,12,0,92,23,
		92,25,20,2,176,13,0,106,7,65,116,101,32,58,
		32,0,20,1,176,14,0,176,15,0,12,0,176,16,
		0,12,0,122,72,20,2,176,17,0,109,18,0,176,
		19,0,100,106,10,101,68,65,84,65,95,70,73,77,
		0,100,100,100,12,5,20,2,48,20,0,176,21,0,
		109,18,0,12,1,112,0,73,36,44,0,176,22,0,
		109,18,0,100,100,100,100,100,100,20,7,4,0,0,
		83,18,0,109,18,0,73,36,45,0,109,28,0,134,
		0,0,0,0,5,31,13,109,29,0,134,0,0,0,
		0,5,28,51,36,46,0,176,11,0,106,3,48,49,
		0,20,1,36,47,0,176,25,0,106,18,68,97,116,
		97,115,32,73,110,118,160,108,105,100,97,115,32,33,
		0,92,3,20,2,36,48,0,26,222,254,36,50,0,
		109,28,0,109,29,0,15,28,59,36,51,0,176,11,
		0,106,3,48,49,0,20,1,36,52,0,176,25,0,
		106,26,68,97,116,97,32,73,110,105,99,105,111,32,
		62,32,81,117,101,32,70,105,110,97,108,32,33,0,
		92,3,20,2,36,53,0,26,153,254,36,59,0,176,
		6,0,106,2,49,0,20,1,36,60,0,176,7,0,
		9,100,106,7,67,80,65,71,65,82,0,100,100,9,
		20,6,176,8,0,106,9,68,65,84,65,95,67,80,
		86,0,20,1,176,8,0,106,7,67,80,65,71,65,
		82,0,20,1,176,8,0,106,9,68,65,84,65,95,
		67,80,71,0,20,1,36,61,0,176,9,0,20,0,
		36,62,0,176,30,0,90,92,51,67,58,47,115,105,
		115,47,83,65,78,47,120,104,98,47,99,111,110,95,
		99,104,110,99,46,112,114,103,58,67,79,78,95,67,
		72,78,67,0,109,31,0,109,28,0,16,21,28,43,
		73,109,31,0,109,29,0,34,21,28,32,73,109,32,
		0,121,69,21,28,23,73,109,33,0,109,10,0,5,
		21,28,12,73,109,34,0,134,0,0,0,0,5,6,
		106,126,68,65,84,65,95,82,69,65,76,32,62,61,
		32,101,68,65,84,65,95,73,78,73,32,46,65,78,
		68,46,32,68,65,84,65,95,82,69,65,76,32,60,
		61,32,101,68,65,84,65,95,70,73,77,32,46,65,
		78,68,46,32,82,69,65,76,73,90,65,68,79,32,
		60,62,32,48,32,46,65,78,68,46,32,67,79,68,
		95,67,79,78,84,65,32,61,32,101,67,79,78,84,
		65,32,46,65,78,68,46,32,68,65,84,65,95,67,
		79,77,80,69,32,61,32,67,84,79,68,40,34,34,
		41,0,20,2,36,63,0,176,9,0,20,0,36,64,
		0,176,35,0,20,0,36,65,0,176,36,0,106,9,
		87,47,78,44,43,87,47,82,0,20,1,36,66,0,
		92,12,3,1,0,92,12,3,1,0,92,12,3,1,
		0,92,12,3,1,0,176,41,0,108,37,108,38,108,
		39,108,40,20,4,81,40,0,81,39,0,81,38,0,
		81,37,0,36,67,0,176,42,0,109,37,0,120,109,
		40,0,20,3,36,68,0,106,12,64,82,32,57,57,
		57,57,57,57,57,57,0,98,38,0,122,2,36,69,
		0,106,12,64,82,32,57,57,57,57,57,57,57,57,
		0,98,38,0,92,2,2,36,70,0,106,12,64,82,
		32,57,57,57,57,57,57,57,57,0,98,38,0,92,
		3,2,36,71,0,106,12,64,82,32,57,57,57,57,
		57,57,57,57,0,98,38,0,92,4,2,36,72,0,
		106,5,64,83,53,48,0,98,38,0,92,5,2,36,
		73,0,106,11,32,32,47,32,32,47,32,32,32,32,
		0,98,38,0,92,6,2,36,74,0,106,22,64,82,
		32,57,57,57,44,57,57,57,44,57,57,57,44,57,
		57,57,46,57,57,0,98,38,0,92,7,2,36,75,
		0,106,11,32,32,47,32,32,47,32,32,32,32,0,
		98,38,0,92,8,2,36,76,0,106,22,64,82,32,
		57,57,57,44,57,57,57,44,57,57,57,44,57,57,
		57,46,57,57,0,98,38,0,92,9,2,36,77,0,
		106,10,64,82,32,57,57,57,57,57,57,0,98,38,
		0,92,10,2,36,78,0,106,14,64,82,32,57,57,
		57,57,57,57,57,57,57,57,0,98,38,0,92,11,
		2,36,79,0,106,11,32,32,47,32,32,47,32,32,
		32,32,0,98,38,0,92,12,2,36,80,0,106,11,
		76,97,110,135,97,109,101,110,116,111,0,98,39,0,
		122,2,36,81,0,106,13,67,108,97,115,115,101,32,
		68,101,115,112,46,0,98,39,0,92,2,2,36,82,
		0,106,15,32,32,32,80,114,111,100,117,116,111,32,
		32,32,32,0,98,39,0,92,3,2,36,83,0,106,
		11,70,111,114,110,101,99,101,100,111,114,0,98,39,
		0,92,4,2,36,84,0,106,10,68,101,115,99,114,
		105,135,198,111,0,98,39,0,92,5,2,36,85,0,
		106,11,68,97,116,97,32,80,114,101,118,46,0,98,
		39,0,92,6,2,36,86,0,106,14,86,108,114,46,
		32,80,114,101,118,105,115,116,111,0,98,39,0,92,
		7,2,36,87,0,106,11,68,97,116,97,32,80,103,
		116,111,46,0,98,39,0,92,8,2,36,88,0,106,
		10,86,108,114,46,32,80,97,103,111,0,98,39,0,
		92,9,2,36,89,0,106,11,67,111,100,46,32,67,
		111,110,116,97,0,98,39,0,92,10,2,36,90,0,
		106,12,78,117,109,46,32,67,104,101,113,117,101,0,
		98,39,0,92,11,2,36,91,0,106,17,68,97,116,
		97,32,67,111,109,112,101,110,115,97,99,97,111,0,
		98,39,0,92,12,2,36,92,0,176,43,0,92,6,
		121,92,20,92,79,92,2,20,5,36,93,0,176,11,
		0,106,3,48,49,0,20,1,36,94,0,176,36,0,
		106,1,0,20,1,36,95,0,176,36,0,106,6,87,
		47,78,43,42,0,20,1,36,96,0,176,12,0,92,
		23,121,20,2,176,13,0,106,6,91,69,83,67,93,
		0,20,1,36,97,0,176,12,0,92,23,92,20,20,
		2,176,13,0,106,7,91,26,27,24,25,93,0,20,
		1,36,98,0,176,36,0,106,1,0,20,1,36,99,
		0,176,36,0,106,9,87,47,78,44,43,87,47,82,
		0,20,1,36,100,0,176,12,0,92,23,92,5,20,
		2,176,13,0,106,13,80,97,114,97,32,65,108,116,
		101,114,97,114,0,20,1,36,101,0,176,12,0,92,
		23,92,26,20,2,176,13,0,106,13,77,111,118,105,
		109,101,110,116,97,135,198,111,0,20,1,36,102,0,
		176,44,0,92,7,122,92,20,92,78,109,37,0,106,
		10,70,95,67,79,78,67,72,69,50,0,109,38,0,
		109,39,0,106,2,196,0,106,2,179,0,106,2,205,
		0,20,11,36,103,0,121,83,45,0,36,104,0,121,
		83,46,0,36,105,0,121,83,47,0,36,106,0,176,
		30,0,90,101,51,67,58,47,115,105,115,47,83,65,
		78,47,120,104,98,47,99,111,110,95,99,104,110,99,
		46,112,114,103,58,67,79,78,95,67,72,78,67,0,
		109,31,0,109,28,0,16,21,28,52,73,109,31,0,
		109,29,0,34,21,28,41,73,109,32,0,121,69,21,
		28,32,73,109,33,0,109,10,0,5,21,28,21,73,
		109,34,0,134,0,0,0,0,5,21,28,8,73,109,
		48,0,121,69,6,106,148,68,65,84,65,95,82,69,
		65,76,32,62,61,32,101,68,65,84,65,95,73,78,
		73,32,46,65,78,68,46,32,68,65,84,65,95,82,
		69,65,76,32,60,61,32,101,68,65,84,65,95,70,
		73,77,32,46,65,78,68,46,32,82,69,65,76,73,
		90,65,68,79,32,60,62,32,48,32,46,65,78,68,
		46,32,67,79,68,95,67,79,78,84,65,32,61,32,
		101,67,79,78,84,65,32,46,65,78,68,46,32,68,
		65,84,65,95,67,79,77,80,69,32,61,32,67,84,
		79,68,40,34,34,41,32,46,65,78,68,46,32,78,
		85,77,95,67,72,69,81,85,69,32,60,62,32,48,
		0,20,2,36,107,0,176,9,0,20,0,36,108,0,
		176,49,0,12,0,31,50,36,109,0,109,45,0,109,
		50,0,72,83,45,0,36,110,0,109,46,0,109,32,
		0,72,83,46,0,36,111,0,109,47,0,122,72,83,
		47,0,36,112,0,176,51,0,122,20,1,25,200,36,
		114,0,176,35,0,20,0,36,115,0,176,12,0,92,
		10,92,10,20,2,176,13,0,106,21,84,111,116,97,
		108,32,80,114,101,118,105,115,116,111,32,32,32,32,
		58,32,0,20,1,176,14,0,176,15,0,12,0,176,
		16,0,12,0,122,72,20,2,176,17,0,109,18,0,
		176,19,0,100,106,10,101,84,79,84,95,80,82,69,
		86,0,106,18,64,82,32,57,57,57,44,57,57,57,
		44,57,57,57,46,57,57,0,100,100,12,5,20,2,
		48,20,0,176,21,0,109,18,0,12,1,112,0,73,
		36,116,0,176,12,0,92,12,92,10,20,2,176,13,
		0,106,21,84,111,116,97,108,32,82,101,97,108,105,
		122,97,100,111,32,32,32,58,32,0,20,1,176,14,
		0,176,15,0,12,0,176,16,0,12,0,122,72,20,
		2,176,17,0,109,18,0,176,19,0,100,106,10,101,
		84,79,84,95,82,69,65,76,0,106,18,64,82,32,
		57,57,57,44,57,57,57,44,57,57,57,46,57,57,
		0,100,100,12,5,20,2,48,20,0,176,21,0,109,
		18,0,12,1,112,0,73,36,117,0,176,12,0,92,
		14,92,10,20,2,176,13,0,106,21,84,111,116,97,
		108,32,84,105,116,117,108,111,115,32,32,32,32,32,
		58,32,0,20,1,176,14,0,176,15,0,12,0,176,
		16,0,12,0,122,72,20,2,176,17,0,109,18,0,
		176,19,0,100,106,10,101,84,79,84,95,84,73,84,
		85,0,106,18,64,82,32,57,57,57,44,57,57,57,
		44,57,57,57,46,57,57,0,100,100,12,5,20,2,
		48,20,0,176,21,0,109,18,0,12,1,112,0,73,
		36,118,0,176,11,0,106,3,48,49,0,20,1,36,
		119,0,176,26,0,106,44,80,114,101,115,115,105,111,
		110,101,32,81,117,97,108,113,117,101,114,32,84,101,
		99,108,97,32,112,97,114,97,32,70,105,110,97,108,
		105,122,97,114,32,46,46,46,0,20,1,36,120,0,
		176,27,0,20,0,36,121,0,176,52,0,20,0,176,
		6,0,106,2,49,0,20,1,176,53,0,100,20,1,
		36,122,0,176,54,0,121,121,176,2,0,12,0,176,
		3,0,12,0,109,4,0,20,5,36,123,0,100,110,
		7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,99,104,110,99,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,16,192,255,231,199,255,61,
		248,255,255,255,255,255,255,253,15,0,4,3,0,4,
		1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

