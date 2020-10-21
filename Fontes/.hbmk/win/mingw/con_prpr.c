/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_prpr.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_PRPR );
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
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( FUNC_PRPR );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( PCENTRA );
HB_FUNC_EXTERN( PPARA );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_PRPR )
{ "CON_PRPR", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_PRPR )}, NULL },
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
{ "ENUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EPESQUISA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "NUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "PROPRIET_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "FUNC_PRPR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( FUNC_PRPR )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "T_PROP1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROPRIET_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "PCENTRA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCENTRA )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "T_PROP2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROPRIET_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_PRPR, "C:/sis/SAN/xhb/con_prpr.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_PRPR
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_PRPR )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_PRPR )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,38,67,79,78,83,85,76,84,65,32,68,69,
		32,85,78,73,68,65,68,69,83,32,80,79,82,32,
		80,82,79,80,82,73,69,84,65,82,73,79,0,20,
		1,36,16,0,176,6,0,106,2,49,0,20,1,36,
		17,0,176,7,0,9,100,106,9,85,78,73,95,73,
		77,79,66,0,100,100,9,20,6,176,8,0,106,9,
		85,78,73,95,73,77,79,66,0,20,1,36,18,0,
		176,9,0,20,0,36,19,0,176,6,0,106,2,50,
		0,20,1,36,20,0,176,7,0,9,100,106,9,67,
		76,65,83,95,80,82,79,0,100,100,9,20,6,176,
		8,0,106,9,67,76,65,83,95,80,82,79,0,20,
		1,36,21,0,176,9,0,20,0,36,22,0,176,6,
		0,106,2,51,0,20,1,36,23,0,176,7,0,9,
		100,106,9,67,65,68,95,81,85,65,68,0,100,100,
		9,20,6,176,8,0,106,9,67,65,68,95,81,85,
		65,68,0,20,1,36,24,0,176,9,0,20,0,36,
		25,0,176,6,0,106,2,52,0,20,1,36,26,0,
		176,7,0,9,100,106,9,67,65,68,95,65,80,65,
		82,0,100,100,9,20,6,176,8,0,106,9,67,65,
		68,95,65,80,65,82,0,20,1,36,27,0,176,9,
		0,20,0,36,28,0,176,6,0,106,2,53,0,20,
		1,36,29,0,176,7,0,9,100,106,8,67,79,78,
		84,65,84,79,0,100,100,9,20,6,176,8,0,106,
		8,67,79,78,84,65,84,79,0,20,1,176,8,0,
		106,9,78,85,77,95,67,76,73,69,0,20,1,36,
		30,0,176,9,0,20,0,36,31,0,176,10,0,20,
		0,36,32,0,176,11,0,106,9,87,47,78,44,43,
		87,47,82,0,20,1,36,33,0,121,83,12,0,36,
		34,0,176,13,0,92,49,12,1,83,14,0,36,35,
		0,176,15,0,92,5,92,2,20,2,176,16,0,106,
		7,78,111,109,101,32,58,0,20,1,176,17,0,176,
		18,0,12,0,176,19,0,12,0,122,72,20,2,176,
		20,0,109,21,0,176,22,0,100,106,10,101,80,69,
		83,81,85,73,83,65,0,106,53,64,33,32,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,0,100,100,12,5,20,2,48,23,
		0,176,24,0,109,21,0,12,1,112,0,73,36,36,
		0,176,25,0,109,21,0,100,100,100,100,100,100,20,
		7,4,0,0,83,21,0,109,21,0,73,36,37,0,
		176,26,0,109,14,0,12,1,83,14,0,36,38,0,
		176,27,0,109,14,0,100,100,20,3,36,39,0,92,
		2,3,1,0,92,2,3,1,0,92,2,3,1,0,
		92,2,3,1,0,176,32,0,108,28,108,29,108,30,
		108,31,20,4,81,31,0,81,30,0,81,29,0,81,
		28,0,36,40,0,176,33,0,109,28,0,120,109,31,
		0,20,3,36,41,0,106,8,67,80,70,95,67,71,
		67,0,98,28,0,92,2,2,36,42,0,92,14,98,
		31,0,92,2,2,36,43,0,106,5,64,83,53,48,
		0,98,29,0,122,2,36,44,0,106,5,64,83,49,
		52,0,98,29,0,92,2,2,36,45,0,106,7,32,
		32,78,79,77,69,0,98,30,0,122,2,36,46,0,
		106,8,67,71,67,47,67,80,70,0,98,30,0,92,
		2,2,36,47,0,176,34,0,92,5,92,5,92,5,
		92,79,109,28,0,106,7,75,69,89,83,95,52,0,
		109,29,0,109,30,0,106,2,32,0,106,2,32,0,
		106,2,32,0,20,11,36,48,0,109,35,0,83,12,
		0,36,49,0,176,15,0,92,6,92,2,20,2,176,
		16,0,106,14,67,111,100,105,103,111,32,32,32,32,
		32,32,58,0,20,1,176,17,0,176,18,0,12,0,
		176,19,0,12,0,122,72,20,2,176,20,0,109,21,
		0,176,22,0,100,106,10,101,78,85,77,95,67,76,
		73,69,0,106,6,57,57,57,57,57,0,100,100,12,
		5,20,2,48,23,0,176,24,0,109,21,0,12,1,
		112,0,73,36,50,0,176,36,0,120,20,1,4,0,
		0,83,21,0,109,21,0,73,36,51,0,176,6,0,
		106,2,49,0,20,1,36,52,0,176,37,0,90,21,
		109,38,0,109,12,0,5,21,31,10,73,109,39,0,
		109,12,0,5,6,106,51,80,82,79,80,82,73,69,
		84,95,49,32,61,32,101,78,85,77,95,67,76,73,
		69,32,46,79,82,46,32,80,82,79,80,82,73,69,
		84,95,50,32,61,32,101,78,85,77,95,67,76,73,
		69,0,20,2,36,53,0,176,9,0,20,0,36,54,
		0,92,8,3,1,0,92,8,3,1,0,92,8,3,
		1,0,92,8,3,1,0,176,32,0,108,28,108,29,
		108,30,108,31,20,4,81,31,0,81,30,0,81,29,
		0,81,28,0,36,55,0,176,33,0,109,28,0,120,
		109,31,0,20,3,36,56,0,106,11,80,82,79,80,
		82,73,69,84,95,49,0,98,28,0,92,5,2,36,
		57,0,106,10,81,85,73,84,65,68,79,95,49,0,
		98,28,0,92,6,2,36,58,0,106,11,80,82,79,
		80,82,73,69,84,95,50,0,98,28,0,92,7,2,
		36,59,0,106,10,81,85,73,84,65,68,79,95,50,
		0,98,28,0,92,8,2,36,60,0,92,5,98,31,
		0,92,5,2,36,61,0,122,98,31,0,92,6,2,
		36,62,0,92,5,98,31,0,92,7,2,36,63,0,
		122,98,31,0,92,8,2,36,64,0,106,8,64,82,
		57,57,57,57,57,0,98,29,0,122,2,36,65,0,
		106,8,64,82,57,57,57,57,57,0,98,29,0,92,
		2,2,36,66,0,106,8,64,82,57,57,57,57,57,
		0,98,29,0,92,3,2,36,67,0,106,8,64,82,
		57,57,57,57,57,0,98,29,0,92,4,2,36,68,
		0,106,8,64,82,57,57,57,57,57,0,98,29,0,
		92,5,2,36,69,0,106,8,64,82,57,57,57,57,
		57,0,98,29,0,92,6,2,36,70,0,106,10,32,
		80,114,111,100,117,116,111,32,0,98,30,0,122,2,
		36,71,0,106,6,71,114,117,112,111,0,98,30,0,
		92,2,2,36,72,0,106,5,85,110,105,100,0,98,
		30,0,92,3,2,36,73,0,106,7,83,116,97,116,
		117,115,0,98,30,0,92,4,2,36,74,0,106,17,
		49,111,46,32,80,114,111,112,114,105,101,116,97,114,
		105,111,0,98,30,0,92,5,2,36,75,0,106,13,
		81,117,105,116,97,99,97,111,32,49,111,46,0,98,
		30,0,92,6,2,36,76,0,106,17,50,111,46,32,
		80,114,111,112,114,105,101,116,97,114,105,111,0,98,
		30,0,92,7,2,36,77,0,106,13,81,117,105,116,
		97,99,97,111,32,50,111,46,0,98,30,0,92,8,
		2,36,78,0,176,40,0,92,7,121,92,21,92,79,
		92,2,20,5,36,79,0,176,34,0,92,8,122,92,
		21,92,78,109,28,0,106,10,70,85,78,67,95,80,
		82,80,82,0,109,29,0,109,30,0,106,2,196,0,
		106,2,179,0,106,2,205,0,20,11,36,80,0,176,
		41,0,20,0,36,81,0,176,42,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,4,0,20,5,36,
		82,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( FUNC_PRPR )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,44,0,1,71,45,0,2,36,87,0,
		176,32,0,108,46,108,47,20,2,36,88,0,176,48,
		0,12,0,83,47,0,36,89,0,122,83,46,0,36,
		90,0,109,44,0,92,4,8,29,23,3,36,92,0,
		109,47,0,92,13,8,29,11,3,36,93,0,109,45,
		0,92,5,5,29,120,1,36,94,0,176,1,0,121,
		121,176,2,0,12,0,176,3,0,12,0,12,4,83,
		49,0,36,95,0,109,38,0,83,50,0,36,96,0,
		176,6,0,106,2,53,0,20,1,36,97,0,176,7,
		0,9,100,106,8,67,79,78,84,65,84,79,0,100,
		100,9,20,6,176,8,0,106,9,78,85,77,95,67,
		76,73,69,0,20,1,176,8,0,106,8,67,79,78,
		84,65,84,79,0,20,1,36,98,0,176,9,0,20,
		0,36,99,0,176,27,0,109,50,0,100,100,20,3,
		36,100,0,176,51,0,92,12,92,13,92,14,92,67,
		20,4,176,17,0,92,12,92,13,20,2,36,101,0,
		176,40,0,92,12,92,13,92,14,92,67,92,2,20,
		5,36,102,0,176,52,0,106,27,78,111,109,101,32,
		100,111,32,80,114,111,112,114,105,101,116,97,114,105,
		111,32,78,111,46,32,49,0,92,12,20,2,36,103,
		0,176,17,0,92,13,92,15,20,2,176,20,0,109,
		21,0,176,22,0,100,106,8,67,76,73,69,78,84,
		69,0,100,100,100,12,5,20,2,48,23,0,176,24,
		0,109,21,0,12,1,112,0,73,36,104,0,176,36,
		0,120,20,1,4,0,0,83,21,0,109,21,0,73,
		36,105,0,176,7,0,9,100,106,8,67,79,78,84,
		65,84,79,0,100,100,9,20,6,176,8,0,106,8,
		67,79,78,84,65,84,79,0,20,1,176,8,0,106,
		9,78,85,77,95,67,76,73,69,0,20,1,36,106,
		0,176,6,0,106,2,49,0,20,1,36,107,0,176,
		53,0,20,0,36,108,0,176,42,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,49,0,20,5,26,
		138,1,36,110,0,109,45,0,92,7,5,29,119,1,
		36,111,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,54,0,36,112,0,109,39,
		0,83,55,0,36,113,0,176,6,0,106,2,53,0,
		20,1,36,114,0,176,7,0,9,100,106,8,67,79,
		78,84,65,84,79,0,100,100,9,20,6,176,8,0,
		106,9,78,85,77,95,67,76,73,69,0,20,1,176,
		8,0,106,8,67,79,78,84,65,84,79,0,20,1,
		36,115,0,176,9,0,20,0,36,116,0,176,27,0,
		109,55,0,100,100,20,3,36,117,0,176,51,0,92,
		12,92,13,92,14,92,67,20,4,176,17,0,92,12,
		92,13,20,2,36,118,0,176,40,0,92,12,92,13,
		92,14,92,67,92,2,20,5,36,119,0,176,52,0,
		106,27,78,111,109,101,32,100,111,32,80,114,111,112,
		114,105,101,116,97,114,105,111,32,78,111,46,32,50,
		0,92,12,20,2,36,120,0,176,17,0,92,13,92,
		15,20,2,176,20,0,109,21,0,176,22,0,100,106,
		8,67,76,73,69,78,84,69,0,100,100,100,12,5,
		20,2,48,23,0,176,24,0,109,21,0,12,1,112,
		0,73,36,121,0,176,36,0,120,20,1,4,0,0,
		83,21,0,109,21,0,73,36,122,0,176,7,0,9,
		100,106,8,67,79,78,84,65,84,79,0,100,100,9,
		20,6,176,8,0,106,8,67,79,78,84,65,84,79,
		0,20,1,176,8,0,106,9,78,85,77,95,67,76,
		73,69,0,20,1,36,123,0,176,6,0,106,2,49,
		0,20,1,36,124,0,176,53,0,20,0,36,125,0,
		176,42,0,121,121,176,2,0,12,0,176,3,0,12,
		0,109,54,0,20,5,25,9,36,127,0,121,83,46,
		0,36,132,0,109,46,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

