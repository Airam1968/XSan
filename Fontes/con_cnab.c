/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_cnab.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_CNAB );
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
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_CNAB )
{ "CON_CNAB", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_CNAB )}, NULL },
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
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "CDOC_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_CNAB, "C:/sis/SAN/xhb/con_cnab.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_CNAB
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_CNAB )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_CNAB )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,99,110,97,98,46,112,114,103,
		58,67,79,78,95,67,78,65,66,0,36,14,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,15,0,176,5,0,106,2,50,
		0,20,1,36,16,0,176,6,0,9,100,106,7,66,
		65,78,67,79,83,0,100,100,9,20,6,176,7,0,
		106,7,66,65,78,67,79,83,0,20,1,36,17,0,
		176,8,0,20,0,36,18,0,176,9,0,20,0,36,
		19,0,176,10,0,106,9,87,47,78,44,43,87,47,
		82,0,20,1,36,20,0,121,83,11,0,36,21,0,
		176,12,0,92,5,92,2,20,2,176,13,0,106,11,
		66,97,110,99,111,32,32,32,58,32,0,20,1,36,
		22,0,176,8,0,20,0,36,23,0,92,2,3,1,
		0,92,2,3,1,0,92,2,3,1,0,92,2,3,
		1,0,176,18,0,108,14,108,15,108,16,108,17,20,
		4,81,17,0,81,16,0,81,15,0,81,14,0,36,
		24,0,176,19,0,109,14,0,120,109,17,0,20,3,
		36,25,0,106,7,64,82,57,57,57,57,0,98,15,
		0,122,2,36,26,0,106,5,64,83,53,48,0,98,
		15,0,92,2,2,36,27,0,106,5,32,32,32,32,
		0,98,16,0,122,2,36,28,0,106,5,32,32,32,
		32,0,98,16,0,92,2,2,36,29,0,176,20,0,
		92,5,92,15,92,5,92,75,109,14,0,106,11,70,
		95,67,79,78,70,95,67,79,66,0,109,15,0,109,
		16,0,106,2,32,0,106,4,32,45,32,0,106,2,
		32,0,20,11,36,30,0,109,21,0,83,11,0,36,
		31,0,176,22,0,109,23,0,12,1,83,24,0,36,
		32,0,176,25,0,92,8,106,3,79,78,0,20,2,
		36,34,0,109,11,0,122,5,29,136,0,36,35,0,
		176,5,0,106,2,49,0,20,1,36,36,0,176,6,
		0,9,100,106,9,84,82,65,78,83,48,48,49,0,
		100,100,9,20,6,36,37,0,176,8,0,20,0,36,
		38,0,176,26,0,90,50,51,67,58,47,115,105,115,
		47,83,65,78,47,120,104,98,47,99,111,110,95,99,
		110,97,98,46,112,114,103,58,67,79,78,95,67,78,
		65,66,0,109,27,0,106,3,48,54,0,5,6,106,
		18,67,68,79,67,95,84,82,78,48,49,32,61,32,
		34,48,54,34,0,20,2,36,39,0,176,8,0,20,
		0,26,248,9,36,40,0,109,11,0,92,8,5,29,
		136,0,36,41,0,176,5,0,106,2,49,0,20,1,
		36,42,0,176,6,0,9,100,106,9,84,82,65,78,
		83,48,48,56,0,100,100,9,20,6,36,43,0,176,
		8,0,20,0,36,44,0,176,26,0,90,50,51,67,
		58,47,115,105,115,47,83,65,78,47,120,104,98,47,
		99,111,110,95,99,110,97,98,46,112,114,103,58,67,
		79,78,95,67,78,65,66,0,109,27,0,106,3,48,
		54,0,5,6,106,18,67,68,79,67,95,84,82,78,
		48,49,32,61,32,34,48,54,34,0,20,2,36,45,
		0,176,8,0,20,0,26,103,9,36,46,0,109,11,
		0,92,33,5,29,171,0,36,47,0,176,5,0,106,
		2,49,0,20,1,36,48,0,176,6,0,9,100,106,
		9,84,82,65,78,83,48,51,51,0,100,100,9,20,
		6,36,49,0,176,8,0,20,0,36,50,0,176,26,
		0,90,63,51,67,58,47,115,105,115,47,83,65,78,
		47,120,104,98,47,99,111,110,95,99,110,97,98,46,
		112,114,103,58,67,79,78,95,67,78,65,66,0,109,
		27,0,106,3,48,54,0,5,21,31,12,73,109,27,
		0,106,3,49,53,0,5,6,106,40,67,68,79,67,
		95,84,82,78,48,49,32,61,32,34,48,54,34,46,
		79,82,46,32,67,68,79,67,95,84,82,78,48,49,
		32,61,32,34,49,53,34,0,20,2,36,51,0,176,
		8,0,20,0,26,179,8,36,52,0,109,11,0,92,
		104,5,29,172,0,36,53,0,176,5,0,106,2,49,
		0,20,1,36,54,0,176,6,0,9,100,106,9,84,
		82,65,78,83,49,48,52,0,100,100,9,20,6,36,
		55,0,176,8,0,20,0,36,56,0,176,26,0,90,
		63,51,67,58,47,115,105,115,47,83,65,78,47,120,
		104,98,47,99,111,110,95,99,110,97,98,46,112,114,
		103,58,67,79,78,95,67,78,65,66,0,109,27,0,
		106,3,50,49,0,5,21,31,12,73,109,27,0,106,
		3,50,50,0,5,6,106,41,67,68,79,67,95,84,
		82,78,48,49,32,61,32,34,50,49,34,32,46,79,
		82,46,32,67,68,79,67,95,84,82,78,48,49,32,
		61,32,34,50,50,34,0,20,2,36,57,0,176,8,
		0,20,0,26,254,7,36,58,0,109,11,0,93,215,
		0,5,29,172,0,36,59,0,176,5,0,106,2,49,
		0,20,1,36,60,0,176,6,0,9,100,106,9,84,
		82,65,78,83,50,49,53,0,100,100,9,20,6,36,
		61,0,176,8,0,20,0,36,62,0,176,26,0,90,
		63,51,67,58,47,115,105,115,47,83,65,78,47,120,
		104,98,47,99,111,110,95,99,110,97,98,46,112,114,
		103,58,67,79,78,95,67,78,65,66,0,109,27,0,
		106,3,48,54,0,5,21,31,12,73,109,27,0,106,
		3,48,56,0,5,6,106,41,67,68,79,67,95,84,
		82,78,48,49,32,61,32,34,48,54,34,32,46,79,
		82,46,32,67,68,79,67,95,84,82,78,48,49,32,
		61,32,34,48,56,34,0,20,2,36,63,0,176,8,
		0,20,0,26,72,7,36,64,0,109,11,0,93,230,
		0,5,29,172,0,36,65,0,176,5,0,106,2,49,
		0,20,1,36,66,0,176,6,0,9,100,106,9,84,
		82,65,78,83,50,51,48,0,100,100,9,20,6,36,
		67,0,176,8,0,20,0,36,68,0,176,26,0,90,
		63,51,67,58,47,115,105,115,47,83,65,78,47,120,
		104,98,47,99,111,110,95,99,110,97,98,46,112,114,
		103,58,67,79,78,95,67,78,65,66,0,109,27,0,
		106,3,48,54,0,5,21,31,12,73,109,27,0,106,
		3,48,56,0,5,6,106,41,67,68,79,67,95,84,
		82,78,48,49,32,61,32,34,48,54,34,32,46,79,
		82,46,32,67,68,79,67,95,84,82,78,48,49,32,
		61,32,34,48,56,34,0,20,2,36,69,0,176,8,
		0,20,0,26,146,6,36,70,0,109,11,0,93,237,
		0,5,29,172,0,36,71,0,176,5,0,106,2,49,
		0,20,1,36,72,0,176,6,0,9,100,106,9,84,
		82,65,78,83,50,51,55,0,100,100,9,20,6,36,
		73,0,176,8,0,20,0,36,74,0,176,26,0,90,
		63,51,67,58,47,115,105,115,47,83,65,78,47,120,
		104,98,47,99,111,110,95,99,110,97,98,46,112,114,
		103,58,67,79,78,95,67,78,65,66,0,109,27,0,
		106,3,48,54,0,5,21,31,12,73,109,27,0,106,
		3,48,56,0,5,6,106,41,67,68,79,67,95,84,
		82,78,48,49,32,61,32,34,48,54,34,32,46,79,
		82,46,32,67,68,79,67,95,84,82,78,48,49,32,
		61,32,34,48,56,34,0,20,2,36,75,0,176,8,
		0,20,0,26,220,5,36,76,0,109,11,0,93,19,
		1,5,29,172,0,36,77,0,176,5,0,106,2,49,
		0,20,1,36,78,0,176,6,0,9,100,106,9,84,
		82,65,78,83,50,55,53,0,100,100,9,20,6,36,
		79,0,176,8,0,20,0,36,80,0,176,26,0,90,
		63,51,67,58,47,115,105,115,47,83,65,78,47,120,
		104,98,47,99,111,110,95,99,110,97,98,46,112,114,
		103,58,67,79,78,95,67,78,65,66,0,109,27,0,
		106,3,48,54,0,5,21,31,12,73,109,27,0,106,
		3,48,56,0,5,6,106,41,67,68,79,67,95,84,
		82,78,48,49,32,61,32,34,48,54,34,32,46,79,
		82,46,32,67,68,79,67,95,84,82,78,48,49,32,
		61,32,34,48,56,34,0,20,2,36,81,0,176,8,
		0,20,0,26,38,5,36,82,0,109,11,0,93,35,
		1,5,29,172,0,36,83,0,176,5,0,106,2,49,
		0,20,1,36,84,0,176,6,0,9,100,106,9,84,
		82,65,78,83,50,57,49,0,100,100,9,20,6,36,
		85,0,176,8,0,20,0,36,86,0,176,26,0,90,
		63,51,67,58,47,115,105,115,47,83,65,78,47,120,
		104,98,47,99,111,110,95,99,110,97,98,46,112,114,
		103,58,67,79,78,95,67,78,65,66,0,109,27,0,
		106,3,48,54,0,5,21,31,12,73,109,27,0,106,
		3,48,56,0,5,6,106,41,67,68,79,67,95,84,
		82,78,48,49,32,61,32,34,48,54,34,32,46,79,
		82,46,32,67,68,79,67,95,84,82,78,48,49,32,
		61,32,34,48,56,34,0,20,2,36,87,0,176,8,
		0,20,0,26,112,4,36,88,0,109,11,0,93,85,
		1,5,29,208,0,36,89,0,176,5,0,106,2,49,
		0,20,1,36,90,0,176,6,0,9,100,106,9,84,
		82,65,78,83,51,52,49,0,100,100,9,20,6,36,
		91,0,176,8,0,20,0,36,92,0,176,26,0,90,
		76,51,67,58,47,115,105,115,47,83,65,78,47,120,
		104,98,47,99,111,110,95,99,110,97,98,46,112,114,
		103,58,67,79,78,95,67,78,65,66,0,109,27,0,
		106,3,48,54,0,5,21,31,25,73,109,27,0,106,
		3,48,55,0,5,21,31,12,73,109,27,0,106,3,
		48,56,0,5,6,106,64,67,68,79,67,95,84,82,
		78,48,49,32,61,32,34,48,54,34,32,46,79,82,
		46,32,67,68,79,67,95,84,82,78,48,49,32,61,
		32,34,48,55,34,32,46,79,82,46,32,67,68,79,
		67,95,84,82,78,48,49,32,61,32,34,48,56,34,
		0,20,2,36,93,0,176,8,0,20,0,26,150,3,
		36,94,0,109,11,0,93,153,1,5,29,208,0,36,
		95,0,176,5,0,106,2,49,0,20,1,36,96,0,
		176,6,0,9,100,106,9,84,82,65,78,83,52,48,
		57,0,100,100,9,20,6,36,97,0,176,8,0,20,
		0,36,98,0,176,26,0,90,76,51,67,58,47,115,
		105,115,47,83,65,78,47,120,104,98,47,99,111,110,
		95,99,110,97,98,46,112,114,103,58,67,79,78,95,
		67,78,65,66,0,109,27,0,106,3,48,54,0,5,
		21,31,25,73,109,27,0,106,3,57,56,0,5,21,
		31,12,73,109,27,0,106,3,57,57,0,5,6,106,
		64,67,68,79,67,95,84,82,78,48,49,32,61,32,
		34,48,54,34,32,46,79,82,46,32,67,68,79,67,
		95,84,82,78,48,49,32,61,32,34,57,56,34,32,
		46,79,82,46,32,67,68,79,67,95,84,82,78,48,
		49,32,61,32,34,57,57,34,0,20,2,36,99,0,
		176,8,0,20,0,26,188,2,36,100,0,109,11,0,
		93,166,1,5,29,208,0,36,101,0,176,5,0,106,
		2,49,0,20,1,36,102,0,176,6,0,9,100,106,
		9,84,82,65,78,83,52,50,50,0,100,100,9,20,
		6,36,103,0,176,8,0,20,0,36,104,0,176,26,
		0,90,76,51,67,58,47,115,105,115,47,83,65,78,
		47,120,104,98,47,99,111,110,95,99,110,97,98,46,
		112,114,103,58,67,79,78,95,67,78,65,66,0,109,
		27,0,106,3,48,54,0,5,21,31,25,73,109,27,
		0,106,3,48,55,0,5,21,31,12,73,109,27,0,
		106,3,49,48,0,5,6,106,64,67,68,79,67,95,
		84,82,78,48,49,32,61,32,34,48,54,34,32,46,
		79,82,46,32,67,68,79,67,95,84,82,78,48,49,
		32,61,32,34,48,55,34,32,46,79,82,46,32,67,
		68,79,67,95,84,82,78,48,49,32,61,32,34,49,
		48,34,0,20,2,36,105,0,176,8,0,20,0,26,
		226,1,36,106,0,109,11,0,93,168,1,5,29,172,
		0,36,107,0,176,5,0,106,2,49,0,20,1,36,
		108,0,176,6,0,9,100,106,9,84,82,65,78,83,
		52,50,52,0,100,100,9,20,6,36,109,0,176,8,
		0,20,0,36,110,0,176,26,0,90,63,51,67,58,
		47,115,105,115,47,83,65,78,47,120,104,98,47,99,
		111,110,95,99,110,97,98,46,112,114,103,58,67,79,
		78,95,67,78,65,66,0,109,27,0,106,3,48,54,
		0,5,21,31,12,73,109,27,0,106,3,49,53,0,
		5,6,106,41,67,68,79,67,95,84,82,78,48,49,
		32,61,32,34,48,54,34,32,46,79,82,46,32,67,
		68,79,67,95,84,82,78,48,49,32,61,32,34,49,
		53,34,0,20,2,36,111,0,176,8,0,20,0,26,
		44,1,36,112,0,109,11,0,93,129,2,5,29,243,
		0,36,113,0,176,5,0,106,2,49,0,20,1,36,
		114,0,176,6,0,9,100,106,9,84,82,65,78,83,
		54,52,49,0,100,100,9,20,6,36,115,0,176,8,
		0,20,0,36,116,0,176,26,0,90,89,51,67,58,
		47,115,105,115,47,83,65,78,47,120,104,98,47,99,
		111,110,95,99,110,97,98,46,112,114,103,58,67,79,
		78,95,67,78,65,66,0,109,27,0,106,3,48,54,
		0,5,21,31,38,73,109,27,0,106,3,49,53,0,
		5,21,31,25,73,109,27,0,106,3,49,54,0,5,
		21,31,12,73,109,27,0,106,3,49,55,0,5,6,
		106,87,67,68,79,67,95,84,82,78,48,49,32,61,
		32,34,48,54,34,32,46,79,82,46,32,67,68,79,
		67,95,84,82,78,48,49,32,61,32,34,49,53,34,
		32,46,79,82,46,32,67,68,79,67,95,84,82,78,
		48,49,32,61,32,34,49,54,34,32,46,79,82,46,
		32,67,68,79,67,95,84,82,78,48,49,32,61,32,
		34,49,55,34,0,20,2,36,117,0,176,8,0,20,
		0,25,46,36,119,0,176,5,0,106,2,49,0,20,
		1,36,120,0,176,6,0,9,100,106,9,84,82,65,
		78,83,84,88,84,0,100,100,9,20,6,36,121,0,
		176,8,0,20,0,36,123,0,176,25,0,92,8,106,
		4,79,70,70,0,20,2,36,124,0,176,28,0,106,
		30,67,79,78,83,85,76,84,65,32,68,69,32,68,
		65,68,79,83,32,45,32,66,65,73,88,65,83,32,
		45,32,0,109,24,0,72,20,1,36,125,0,176,29,
		0,106,3,49,49,0,20,1,36,126,0,176,5,0,
		106,2,49,0,20,1,36,127,0,176,8,0,20,0,
		36,128,0,176,30,0,92,5,121,92,21,92,79,92,
		2,20,5,36,129,0,92,40,3,1,0,92,40,3,
		1,0,92,40,3,1,0,92,40,3,1,0,176,18,
		0,108,14,108,15,108,16,108,17,20,4,81,17,0,
		81,16,0,81,15,0,81,14,0,36,130,0,176,19,
		0,109,14,0,120,109,17,0,20,3,36,131,0,176,
		20,0,92,6,122,92,21,92,78,109,14,0,106,7,
		70,95,67,78,65,66,0,109,15,0,109,16,0,106,
		2,196,0,106,2,179,0,106,2,205,0,20,11,36,
		132,0,176,31,0,20,0,36,133,0,176,32,0,121,
		121,176,2,0,12,0,176,3,0,12,0,109,4,0,
		20,5,36,134,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,99,110,97,98,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,17,192,255,255,253,255,255,
		255,255,255,255,255,255,255,191,251,127,0,4,3,0,
		4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

