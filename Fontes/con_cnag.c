/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_cnag.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_CNAG );
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
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( F_CNAB );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( SETCURSOR );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_CNAG )
{ "CON_CNAG", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_CNAG )}, NULL },
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
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "F_CNAB", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_CNAB )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "SETCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCURSOR )}, NULL },
{ "FLD_NAME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FLD_PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "ROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROW )}, NULL },
{ "COL", {HB_FS_PUBLIC}, {HB_FUNCNAME( COL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_CNAG, "C:/sis/SAN/xhb/con_cnag.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_CNAG
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_CNAG )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_CNAG )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,99,110,97,103,46,112,114,103,
		58,67,79,78,95,67,78,65,71,0,36,14,0,176,
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
		36,34,0,109,11,0,122,5,28,49,36,35,0,176,
		5,0,106,2,49,0,20,1,36,36,0,176,6,0,
		9,100,106,9,84,82,65,78,83,48,48,49,0,100,
		100,9,20,6,36,37,0,176,8,0,20,0,26,41,
		3,36,38,0,109,11,0,92,8,5,28,49,36,39,
		0,176,5,0,106,2,49,0,20,1,36,40,0,176,
		6,0,9,100,106,9,84,82,65,78,83,48,48,56,
		0,100,100,9,20,6,36,41,0,176,8,0,20,0,
		26,239,2,36,42,0,109,11,0,92,33,5,28,49,
		36,43,0,176,5,0,106,2,49,0,20,1,36,44,
		0,176,6,0,9,100,106,9,84,82,65,78,83,48,
		51,51,0,100,100,9,20,6,36,45,0,176,8,0,
		20,0,26,181,2,36,46,0,109,11,0,92,104,5,
		28,49,36,47,0,176,5,0,106,2,49,0,20,1,
		36,48,0,176,6,0,9,100,106,9,84,82,65,78,
		83,49,48,52,0,100,100,9,20,6,36,49,0,176,
		8,0,20,0,26,123,2,36,50,0,109,11,0,93,
		215,0,5,28,49,36,51,0,176,5,0,106,2,49,
		0,20,1,36,52,0,176,6,0,9,100,106,9,84,
		82,65,78,83,50,49,53,0,100,100,9,20,6,36,
		53,0,176,8,0,20,0,26,64,2,36,54,0,109,
		11,0,93,230,0,5,28,49,36,55,0,176,5,0,
		106,2,49,0,20,1,36,56,0,176,6,0,9,100,
		106,9,84,82,65,78,83,50,51,48,0,100,100,9,
		20,6,36,57,0,176,8,0,20,0,26,5,2,36,
		58,0,109,11,0,93,237,0,5,28,49,36,59,0,
		176,5,0,106,2,49,0,20,1,36,60,0,176,6,
		0,9,100,106,9,84,82,65,78,83,50,51,55,0,
		100,100,9,20,6,36,61,0,176,8,0,20,0,26,
		202,1,36,62,0,109,11,0,93,19,1,5,28,49,
		36,63,0,176,5,0,106,2,49,0,20,1,36,64,
		0,176,6,0,9,100,106,9,84,82,65,78,83,50,
		55,53,0,100,100,9,20,6,36,65,0,176,8,0,
		20,0,26,143,1,36,66,0,109,11,0,93,35,1,
		5,28,49,36,67,0,176,5,0,106,2,49,0,20,
		1,36,68,0,176,6,0,9,100,106,9,84,82,65,
		78,83,50,57,49,0,100,100,9,20,6,36,69,0,
		176,8,0,20,0,26,84,1,36,70,0,109,11,0,
		93,85,1,5,28,49,36,71,0,176,5,0,106,2,
		49,0,20,1,36,72,0,176,6,0,9,100,106,9,
		84,82,65,78,83,51,52,49,0,100,100,9,20,6,
		36,73,0,176,8,0,20,0,26,25,1,36,74,0,
		109,11,0,93,153,1,5,28,49,36,75,0,176,5,
		0,106,2,49,0,20,1,36,76,0,176,6,0,9,
		100,106,9,84,82,65,78,83,52,48,57,0,100,100,
		9,20,6,36,77,0,176,8,0,20,0,26,222,0,
		36,78,0,109,11,0,93,166,1,5,28,49,36,79,
		0,176,5,0,106,2,49,0,20,1,36,80,0,176,
		6,0,9,100,106,9,84,82,65,78,83,52,50,50,
		0,100,100,9,20,6,36,81,0,176,8,0,20,0,
		26,163,0,36,82,0,109,11,0,93,168,1,5,28,
		48,36,83,0,176,5,0,106,2,49,0,20,1,36,
		84,0,176,6,0,9,100,106,9,84,82,65,78,83,
		52,50,52,0,100,100,9,20,6,36,85,0,176,8,
		0,20,0,25,104,36,86,0,109,11,0,93,129,2,
		5,28,48,36,87,0,176,5,0,106,2,49,0,20,
		1,36,88,0,176,6,0,9,100,106,9,84,82,65,
		78,83,54,52,49,0,100,100,9,20,6,36,89,0,
		176,8,0,20,0,25,46,36,91,0,176,5,0,106,
		2,49,0,20,1,36,92,0,176,6,0,9,100,106,
		9,84,82,65,78,83,84,88,84,0,100,100,9,20,
		6,36,93,0,176,8,0,20,0,36,95,0,176,25,
		0,92,8,106,4,79,70,70,0,20,2,36,96,0,
		176,26,0,106,45,67,79,78,83,85,76,84,65,32,
		68,69,32,68,65,68,79,83,32,45,32,65,82,81,
		85,73,86,79,32,82,69,84,79,82,78,79,32,71,
		69,82,65,76,32,45,32,0,109,24,0,72,20,1,
		36,97,0,176,27,0,106,3,49,49,0,20,1,36,
		98,0,176,5,0,106,2,49,0,20,1,36,99,0,
		176,8,0,20,0,36,100,0,176,28,0,92,5,121,
		92,21,92,79,92,2,20,5,36,101,0,92,40,3,
		1,0,92,40,3,1,0,92,40,3,1,0,92,40,
		3,1,0,176,18,0,108,14,108,15,108,16,108,17,
		20,4,81,17,0,81,16,0,81,15,0,81,14,0,
		36,102,0,176,19,0,109,14,0,120,109,17,0,20,
		3,36,103,0,176,20,0,92,6,122,92,21,92,78,
		109,14,0,106,7,70,95,67,78,65,66,0,109,15,
		0,109,16,0,106,2,196,0,106,2,179,0,106,2,
		205,0,20,11,36,104,0,176,29,0,20,0,36,105,
		0,176,30,0,121,121,176,2,0,12,0,176,3,0,
		12,0,109,4,0,20,5,36,106,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_CNAB )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,51,67,58,47,115,105,115,47,83,65,78,
		47,120,104,98,47,99,111,110,95,99,110,97,103,46,
		112,114,103,58,70,95,67,78,65,66,0,71,32,0,
		1,71,33,0,2,36,111,0,176,18,0,108,34,108,
		35,20,2,36,112,0,176,36,0,12,0,83,35,0,
		36,113,0,122,83,34,0,36,114,0,109,32,0,92,
		4,8,29,199,0,36,116,0,109,35,0,92,13,8,
		29,166,0,36,117,0,109,33,0,122,15,29,173,0,
		36,118,0,176,37,0,12,0,29,165,0,36,119,0,
		176,38,0,122,20,1,36,120,0,98,14,0,109,33,
		0,1,83,39,0,36,121,0,98,15,0,109,33,0,
		1,83,40,0,36,122,0,176,41,0,176,42,0,12,
		0,176,43,0,12,0,20,2,176,44,0,109,45,0,
		176,46,0,100,109,39,0,109,40,0,100,100,12,5,
		20,2,48,47,0,176,48,0,109,45,0,12,1,112,
		0,73,36,123,0,176,49,0,109,45,0,100,100,100,
		100,100,100,20,7,4,0,0,83,45,0,109,45,0,
		73,36,124,0,176,38,0,121,20,1,36,125,0,176,
		50,0,20,0,36,126,0,36,127,0,25,23,36,128,
		0,109,35,0,92,27,8,28,12,36,129,0,121,83,
		34,0,36,130,0,36,132,0,109,34,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,99,110,97,103,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,17,192,255,255,253,255,255,
		255,255,255,255,187,255,135,247,255,23,0,4,3,0,
		4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

