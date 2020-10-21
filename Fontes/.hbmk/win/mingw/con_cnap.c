/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_cnap.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_CNAP );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( __ATPROMPT );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( __MENUTO );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_CNAP )
{ "CON_CNAP", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_CNAP )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "ECOD_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "COD_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "NOME_ARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENOME_ARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "__ATPROMPT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __ATPROMPT )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISOP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MENUTO", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MENUTO )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "ECOD_PROT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "CDOC_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_CNAP, "C:/sis/SAN/xhb/con_cnap.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_CNAP
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_CNAP )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_CNAP )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,2,50,0,20,1,36,16,0,176,6,0,9,
		100,106,7,66,65,78,67,79,83,0,100,100,9,20,
		6,176,7,0,106,7,66,65,78,67,79,83,0,20,
		1,36,17,0,176,8,0,20,0,36,18,0,176,9,
		0,20,0,36,19,0,176,10,0,106,9,87,47,78,
		44,43,87,47,82,0,20,1,36,20,0,121,83,11,
		0,36,21,0,176,12,0,92,5,92,2,20,2,176,
		13,0,106,11,66,97,110,99,111,32,32,32,58,32,
		0,20,1,36,22,0,176,8,0,20,0,36,23,0,
		92,2,3,1,0,92,2,3,1,0,92,2,3,1,
		0,92,2,3,1,0,176,18,0,108,14,108,15,108,
		16,108,17,20,4,81,17,0,81,16,0,81,15,0,
		81,14,0,36,24,0,176,19,0,109,14,0,120,109,
		17,0,20,3,36,25,0,106,7,64,82,57,57,57,
		57,0,98,15,0,122,2,36,26,0,106,5,64,83,
		53,48,0,98,15,0,92,2,2,36,27,0,106,5,
		32,32,32,32,0,98,16,0,122,2,36,28,0,106,
		5,32,32,32,32,0,98,16,0,92,2,2,36,29,
		0,176,20,0,92,5,92,15,92,5,92,75,109,14,
		0,106,11,70,95,67,79,78,70,95,67,79,66,0,
		109,15,0,109,16,0,106,2,32,0,106,4,32,45,
		32,0,106,2,32,0,20,11,36,30,0,109,21,0,
		83,11,0,36,31,0,176,22,0,109,23,0,12,1,
		83,24,0,36,32,0,176,25,0,92,8,106,3,79,
		78,0,20,2,36,34,0,176,10,0,106,10,87,47,
		78,44,43,87,47,66,71,0,20,1,36,35,0,176,
		26,0,92,15,92,60,92,20,92,79,20,4,176,27,
		0,92,15,92,60,20,2,36,36,0,176,28,0,92,
		15,92,60,92,20,92,79,122,20,5,36,37,0,176,
		29,0,92,16,92,61,106,19,32,49,32,32,76,105,
		113,117,105,100,97,135,198,111,32,32,32,32,0,106,
		53,32,84,135,116,117,108,111,115,32,76,105,113,117,
		105,100,97,100,111,115,32,101,109,32,67,97,114,116,
		135,114,105,111,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,0,20,4,
		36,38,0,176,29,0,92,17,92,61,106,19,32,50,
		32,66,97,105,120,97,32,112,111,114,32,80,114,111,
		116,46,0,106,53,32,84,135,116,117,108,111,32,69,
		102,101,116,105,118,97,109,101,110,116,101,32,80,114,
		111,116,101,115,116,97,100,111,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,0,20,4,36,39,0,176,29,0,92,18,92,61,
		106,19,32,51,32,84,135,116,117,108,111,32,80,114,
		111,116,101,115,116,46,0,106,53,32,84,135,116,117,
		108,111,32,69,110,99,97,109,105,110,104,97,100,111,
		32,97,111,32,67,97,114,116,135,114,105,111,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,0,20,4,36,40,0,176,29,0,
		92,19,92,61,106,19,32,52,32,83,117,115,116,97,
		135,198,111,32,80,114,111,116,46,32,0,106,53,32,
		73,110,115,116,114,117,135,198,111,32,100,101,32,80,
		114,111,116,101,115,116,111,32,83,117,115,116,97,100,
		97,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,0,20,4,36,41,
		0,176,30,0,109,31,0,100,100,100,100,100,100,20,
		7,4,0,0,83,31,0,109,31,0,73,36,42,0,
		122,83,32,0,36,43,0,176,33,0,89,28,0,1,
		0,0,0,176,34,0,12,0,121,8,28,7,109,32,
		0,25,8,95,1,165,83,32,0,6,106,6,115,105,
		115,79,80,0,12,2,83,32,0,36,45,0,109,32,
		0,122,5,29,33,1,36,47,0,109,11,0,122,5,
		28,16,36,48,0,106,3,49,53,0,83,35,0,26,
		123,4,36,49,0,109,11,0,92,8,5,28,16,36,
		50,0,106,3,49,53,0,83,35,0,26,98,4,36,
		51,0,109,11,0,92,104,5,28,16,36,52,0,106,
		3,50,50,0,83,35,0,26,73,4,36,53,0,109,
		11,0,93,215,0,5,28,16,36,54,0,106,3,48,
		56,0,83,35,0,26,47,4,36,55,0,109,11,0,
		93,230,0,5,28,16,36,56,0,106,3,48,56,0,
		83,35,0,26,21,4,36,57,0,109,11,0,93,237,
		0,5,28,16,36,58,0,106,3,48,56,0,83,35,
		0,26,251,3,36,59,0,109,11,0,93,19,1,5,
		28,16,36,60,0,106,3,48,56,0,83,35,0,26,
		225,3,36,61,0,109,11,0,93,35,1,5,28,16,
		36,62,0,106,3,48,56,0,83,35,0,26,199,3,
		36,63,0,109,11,0,93,85,1,5,28,16,36,64,
		0,106,3,48,56,0,83,35,0,26,173,3,36,65,
		0,109,11,0,93,153,1,5,28,16,36,66,0,106,
		3,57,57,0,83,35,0,26,147,3,36,67,0,109,
		11,0,93,129,2,5,29,134,3,36,68,0,106,3,
		49,53,0,83,35,0,36,70,0,26,117,3,36,71,
		0,109,32,0,92,2,5,29,33,1,36,73,0,109,
		11,0,122,5,28,16,36,74,0,106,3,48,55,0,
		83,35,0,26,81,3,36,75,0,109,11,0,92,8,
		5,28,16,36,76,0,106,3,51,51,0,83,35,0,
		26,56,3,36,77,0,109,11,0,92,104,5,28,16,
		36,78,0,106,3,50,53,0,83,35,0,26,31,3,
		36,79,0,109,11,0,93,215,0,5,28,16,36,80,
		0,106,3,48,56,0,83,35,0,26,5,3,36,81,
		0,109,11,0,93,230,0,5,28,16,36,82,0,106,
		3,48,56,0,83,35,0,26,235,2,36,83,0,109,
		11,0,93,237,0,5,28,16,36,84,0,106,3,48,
		56,0,83,35,0,26,209,2,36,85,0,109,11,0,
		93,19,1,5,28,16,36,86,0,106,3,48,56,0,
		83,35,0,26,183,2,36,87,0,109,11,0,93,35,
		1,5,28,16,36,88,0,106,3,48,56,0,83,35,
		0,26,157,2,36,89,0,109,11,0,93,85,1,5,
		28,16,36,90,0,106,3,51,50,0,83,35,0,26,
		131,2,36,91,0,109,11,0,93,153,1,5,28,16,
		36,92,0,106,3,52,49,0,83,35,0,26,105,2,
		36,93,0,109,11,0,93,129,2,5,29,92,2,36,
		94,0,106,3,53,48,0,83,35,0,36,96,0,26,
		75,2,36,97,0,109,32,0,92,3,5,29,33,1,
		36,99,0,109,11,0,122,5,28,16,36,100,0,106,
		3,50,51,0,83,35,0,26,39,2,36,101,0,109,
		11,0,92,8,5,28,16,36,102,0,106,3,51,51,
		0,83,35,0,26,14,2,36,103,0,109,11,0,92,
		104,5,28,16,36,104,0,106,3,50,54,0,83,35,
		0,26,245,1,36,105,0,109,11,0,93,215,0,5,
		28,16,36,106,0,106,3,49,53,0,83,35,0,26,
		219,1,36,107,0,109,11,0,93,230,0,5,28,16,
		36,108,0,106,3,49,53,0,83,35,0,26,193,1,
		36,109,0,109,11,0,93,237,0,5,28,16,36,110,
		0,106,3,49,53,0,83,35,0,26,167,1,36,111,
		0,109,11,0,93,19,1,5,28,16,36,112,0,106,
		3,49,53,0,83,35,0,26,141,1,36,113,0,109,
		11,0,93,35,1,5,28,16,36,114,0,106,3,49,
		53,0,83,35,0,26,115,1,36,115,0,109,11,0,
		93,85,1,5,28,16,36,116,0,106,3,51,50,0,
		83,35,0,26,89,1,36,117,0,109,11,0,93,153,
		1,5,28,16,36,118,0,106,3,52,49,0,83,35,
		0,26,63,1,36,119,0,109,11,0,93,129,2,5,
		29,50,1,36,120,0,106,3,49,48,0,83,35,0,
		36,122,0,26,33,1,36,123,0,109,32,0,92,4,
		5,29,139,250,36,125,0,109,11,0,122,5,28,16,
		36,126,0,106,3,50,52,0,83,35,0,26,253,0,
		36,127,0,109,11,0,92,8,5,28,16,36,128,0,
		106,3,50,54,0,83,35,0,26,228,0,36,129,0,
		109,11,0,92,104,5,28,16,36,130,0,106,3,50,
		55,0,83,35,0,26,203,0,36,131,0,109,11,0,
		93,215,0,5,28,16,36,132,0,106,3,50,48,0,
		83,35,0,26,177,0,36,133,0,109,11,0,93,230,
		0,5,28,16,36,134,0,106,3,50,48,0,83,35,
		0,26,151,0,36,135,0,109,11,0,93,237,0,5,
		28,15,36,136,0,106,3,50,48,0,83,35,0,25,
		125,36,137,0,109,11,0,93,19,1,5,28,15,36,
		138,0,106,3,50,48,0,83,35,0,25,100,36,139,
		0,109,11,0,93,35,1,5,28,15,36,140,0,106,
		3,50,48,0,83,35,0,25,75,36,141,0,109,11,
		0,93,85,1,5,28,15,36,142,0,106,3,50,48,
		0,83,35,0,25,50,36,143,0,109,11,0,93,153,
		1,5,28,15,36,144,0,106,3,51,49,0,83,35,
		0,25,25,36,145,0,109,11,0,93,129,2,5,28,
		13,36,146,0,106,3,50,48,0,83,35,0,36,152,
		0,109,11,0,122,5,28,100,36,153,0,176,5,0,
		106,2,49,0,20,1,36,154,0,176,6,0,9,100,
		106,9,84,82,65,78,83,48,48,49,0,100,100,9,
		20,6,36,155,0,176,8,0,20,0,36,156,0,176,
		36,0,90,10,109,37,0,109,35,0,5,6,106,23,
		67,68,79,67,95,84,82,78,48,49,32,61,32,101,
		67,79,68,95,80,82,79,84,0,20,2,36,157,0,
		176,8,0,20,0,26,74,5,36,158,0,109,11,0,
		92,8,5,28,100,36,159,0,176,5,0,106,2,49,
		0,20,1,36,160,0,176,6,0,9,100,106,9,84,
		82,65,78,83,48,48,56,0,100,100,9,20,6,36,
		161,0,176,8,0,20,0,36,162,0,176,36,0,90,
		10,109,37,0,109,35,0,5,6,106,23,67,68,79,
		67,95,84,82,78,48,49,32,61,32,101,67,79,68,
		95,80,82,79,84,0,20,2,36,163,0,176,8,0,
		20,0,26,221,4,36,164,0,109,11,0,92,33,5,
		28,97,36,165,0,176,5,0,106,2,49,0,20,1,
		36,166,0,176,6,0,9,100,106,9,84,82,65,78,
		83,48,51,51,0,100,100,9,20,6,36,167,0,176,
		8,0,20,0,36,168,0,176,36,0,90,12,109,37,
		0,106,3,32,32,0,5,6,106,18,67,68,79,67,
		95,84,82,78,48,49,32,61,32,34,32,32,34,0,
		20,2,36,169,0,176,8,0,20,0,26,115,4,36,
		170,0,109,11,0,92,104,5,28,100,36,171,0,176,
		5,0,106,2,49,0,20,1,36,172,0,176,6,0,
		9,100,106,9,84,82,65,78,83,49,48,52,0,100,
		100,9,20,6,36,173,0,176,8,0,20,0,36,174,
		0,176,36,0,90,10,109,37,0,109,35,0,5,6,
		106,23,67,68,79,67,95,84,82,78,48,49,32,61,
		32,101,67,79,68,95,80,82,79,84,0,20,2,36,
		175,0,176,8,0,20,0,26,6,4,36,176,0,109,
		11,0,93,215,0,5,28,100,36,177,0,176,5,0,
		106,2,49,0,20,1,36,178,0,176,6,0,9,100,
		106,9,84,82,65,78,83,50,49,53,0,100,100,9,
		20,6,36,179,0,176,8,0,20,0,36,180,0,176,
		36,0,90,10,109,37,0,109,35,0,5,6,106,23,
		67,68,79,67,95,84,82,78,48,49,32,61,32,101,
		67,79,68,95,80,82,79,84,0,20,2,36,181,0,
		176,8,0,20,0,26,152,3,36,182,0,109,11,0,
		93,230,0,5,28,100,36,183,0,176,5,0,106,2,
		49,0,20,1,36,184,0,176,6,0,9,100,106,9,
		84,82,65,78,83,50,51,48,0,100,100,9,20,6,
		36,185,0,176,8,0,20,0,36,186,0,176,36,0,
		90,10,109,37,0,109,35,0,5,6,106,23,67,68,
		79,67,95,84,82,78,48,49,32,61,32,101,67,79,
		68,95,80,82,79,84,0,20,2,36,187,0,176,8,
		0,20,0,26,42,3,36,188,0,109,11,0,93,237,
		0,5,28,100,36,189,0,176,5,0,106,2,49,0,
		20,1,36,190,0,176,6,0,9,100,106,9,84,82,
		65,78,83,50,57,49,0,100,100,9,20,6,36,191,
		0,176,8,0,20,0,36,192,0,176,36,0,90,10,
		109,37,0,109,35,0,5,6,106,23,67,68,79,67,
		95,84,82,78,48,49,32,61,32,101,67,79,68,95,
		80,82,79,84,0,20,2,36,193,0,176,8,0,20,
		0,26,188,2,36,194,0,109,11,0,93,19,1,5,
		28,100,36,195,0,176,5,0,106,2,49,0,20,1,
		36,196,0,176,6,0,9,100,106,9,84,82,65,78,
		83,50,55,53,0,100,100,9,20,6,36,197,0,176,
		8,0,20,0,36,198,0,176,36,0,90,10,109,37,
		0,109,35,0,5,6,106,23,67,68,79,67,95,84,
		82,78,48,49,32,61,32,101,67,79,68,95,80,82,
		79,84,0,20,2,36,199,0,176,8,0,20,0,26,
		78,2,36,200,0,109,11,0,93,35,1,5,28,100,
		36,201,0,176,5,0,106,2,49,0,20,1,36,202,
		0,176,6,0,9,100,106,9,84,82,65,78,83,50,
		57,49,0,100,100,9,20,6,36,203,0,176,8,0,
		20,0,36,204,0,176,36,0,90,10,109,37,0,109,
		35,0,5,6,106,23,67,68,79,67,95,84,82,78,
		48,49,32,61,32,101,67,79,68,95,80,82,79,84,
		0,20,2,36,205,0,176,8,0,20,0,26,224,1,
		36,206,0,109,11,0,93,85,1,5,28,100,36,207,
		0,176,5,0,106,2,49,0,20,1,36,208,0,176,
		6,0,9,100,106,9,84,82,65,78,83,51,52,49,
		0,100,100,9,20,6,36,209,0,176,8,0,20,0,
		36,210,0,176,36,0,90,10,109,37,0,109,35,0,
		5,6,106,23,67,68,79,67,95,84,82,78,48,49,
		32,61,32,101,67,79,68,95,80,82,79,84,0,20,
		2,36,211,0,176,8,0,20,0,26,114,1,36,212,
		0,109,11,0,93,153,1,5,28,100,36,213,0,176,
		5,0,106,2,49,0,20,1,36,214,0,176,6,0,
		9,100,106,9,84,82,65,78,83,52,48,57,0,100,
		100,9,20,6,36,215,0,176,8,0,20,0,36,216,
		0,176,36,0,90,10,109,37,0,109,35,0,5,6,
		106,23,67,68,79,67,95,84,82,78,48,49,32,61,
		32,101,67,79,68,95,80,82,79,84,0,20,2,36,
		217,0,176,8,0,20,0,26,4,1,36,218,0,109,
		11,0,93,168,1,5,28,97,36,219,0,176,5,0,
		106,2,49,0,20,1,36,220,0,176,6,0,9,100,
		106,9,84,82,65,78,83,52,50,52,0,100,100,9,
		20,6,36,221,0,176,8,0,20,0,36,222,0,176,
		36,0,90,12,109,37,0,106,3,32,32,0,5,6,
		106,18,67,68,79,67,95,84,82,78,48,49,32,61,
		32,34,32,32,34,0,20,2,36,223,0,176,8,0,
		20,0,26,153,0,36,224,0,109,11,0,93,129,2,
		5,28,96,36,225,0,176,5,0,106,2,49,0,20,
		1,36,226,0,176,6,0,9,100,106,9,84,82,65,
		78,83,54,52,49,0,100,100,9,20,6,36,227,0,
		176,8,0,20,0,36,228,0,176,36,0,90,12,109,
		37,0,106,3,32,32,0,5,6,106,18,67,68,79,
		67,95,84,82,78,48,49,32,61,32,34,32,32,34,
		0,20,2,36,229,0,176,8,0,20,0,25,46,36,
		231,0,176,5,0,106,2,49,0,20,1,36,232,0,
		176,6,0,9,100,106,9,84,82,65,78,83,84,88,
		84,0,100,100,9,20,6,36,233,0,176,8,0,20,
		0,36,235,0,176,25,0,92,8,106,4,79,70,70,
		0,20,2,36,236,0,176,38,0,106,43,67,79,78,
		83,85,76,84,65,32,68,69,32,68,65,68,79,83,
		32,45,32,84,73,84,85,76,79,83,32,80,82,79,
		84,69,83,84,65,68,79,83,32,45,32,0,109,24,
		0,72,20,1,36,237,0,176,39,0,106,3,49,49,
		0,20,1,36,238,0,176,28,0,92,5,121,92,21,
		92,79,92,2,20,5,36,239,0,92,40,3,1,0,
		92,40,3,1,0,92,40,3,1,0,92,40,3,1,
		0,176,18,0,108,14,108,15,108,16,108,17,20,4,
		81,17,0,81,16,0,81,15,0,81,14,0,36,240,
		0,176,19,0,109,14,0,120,109,17,0,20,3,36,
		241,0,176,20,0,92,6,122,92,21,92,78,109,14,
		0,106,7,70,95,67,78,65,66,0,109,15,0,109,
		16,0,106,2,196,0,106,2,179,0,106,2,205,0,
		20,11,36,242,0,176,40,0,20,0,36,243,0,176,
		41,0,121,121,176,2,0,12,0,176,3,0,12,0,
		109,4,0,20,5,36,244,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}
