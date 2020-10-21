/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_prov.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_PROV );
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
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( PDESENV );
HB_FUNC( FUNC_PROV );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( PCENTRA );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_PROV )
{ "CON_PROV", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_PROV )}, NULL },
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
{ "ECLAS_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETIPO_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "NUM_LOTEAM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "CADASTRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISUNID1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "EBLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISGRUPO2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISUNID2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PDESENV", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDESENV )}, NULL },
{ "FUNC_PROV", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( FUNC_PROV )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "T_PROP1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROPRIET_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "PCENTRA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCENTRA )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "T_PROP2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROPRIET_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_PROV, "C:/sis/SAN/xhb/con_prov.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_PROV
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_PROV )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_PROV )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,112,114,111,118,46,112,114,103,
		58,67,79,78,95,80,82,79,86,0,36,14,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,15,0,176,5,0,106,30,67,
		79,78,83,85,76,84,65,32,68,69,32,85,78,73,
		68,65,68,69,83,32,86,69,78,68,73,68,65,83,
		0,20,1,36,16,0,176,6,0,106,2,49,0,20,
		1,36,17,0,176,7,0,9,100,106,9,85,78,73,
		95,73,77,79,66,0,100,100,9,20,6,176,8,0,
		106,9,85,78,73,95,73,77,79,66,0,20,1,36,
		18,0,176,9,0,20,0,36,19,0,176,6,0,106,
		2,50,0,20,1,36,20,0,176,7,0,9,100,106,
		9,67,76,65,83,95,80,82,79,0,100,100,9,20,
		6,176,8,0,106,9,67,76,65,83,95,80,82,79,
		0,20,1,36,21,0,176,9,0,20,0,36,22,0,
		176,6,0,106,2,51,0,20,1,36,23,0,176,7,
		0,9,100,106,9,67,65,68,95,81,85,65,68,0,
		100,100,9,20,6,176,8,0,106,9,67,65,68,95,
		81,85,65,68,0,20,1,36,24,0,176,9,0,20,
		0,36,25,0,176,6,0,106,2,52,0,20,1,36,
		26,0,176,7,0,9,100,106,9,67,65,68,95,65,
		80,65,82,0,100,100,9,20,6,176,8,0,106,9,
		67,65,68,95,65,80,65,82,0,20,1,36,27,0,
		176,9,0,20,0,36,28,0,176,6,0,106,2,53,
		0,20,1,36,29,0,176,7,0,9,100,106,8,67,
		79,78,84,65,84,79,0,100,100,9,20,6,176,8,
		0,106,9,78,85,77,95,67,76,73,69,0,20,1,
		176,8,0,106,8,67,79,78,84,65,84,79,0,20,
		1,36,30,0,176,9,0,20,0,36,31,0,176,6,
		0,106,2,50,0,20,1,36,33,0,36,34,0,176,
		10,0,20,0,36,35,0,176,11,0,106,9,87,47,
		78,44,43,87,47,82,0,20,1,36,36,0,121,83,
		12,0,36,37,0,121,83,13,0,36,38,0,176,14,
		0,92,5,92,2,20,2,176,15,0,106,11,80,114,
		111,100,117,116,111,32,58,32,0,20,1,36,39,0,
		176,9,0,20,0,36,40,0,92,3,3,1,0,92,
		3,3,1,0,92,3,3,1,0,92,3,3,1,0,
		176,20,0,108,16,108,17,108,18,108,19,20,4,81,
		19,0,81,18,0,81,17,0,81,16,0,36,41,0,
		176,21,0,109,16,0,120,109,19,0,20,3,36,42,
		0,106,8,64,82,57,57,57,57,57,0,98,17,0,
		122,2,36,43,0,106,8,64,82,57,57,57,57,57,
		0,98,17,0,92,2,2,36,44,0,106,5,64,83,
		52,48,0,98,17,0,92,3,2,36,45,0,106,5,
		32,32,32,32,0,98,18,0,122,2,36,46,0,106,
		5,32,32,32,32,0,98,18,0,92,2,2,36,47,
		0,106,5,32,32,32,32,0,98,18,0,92,3,2,
		36,48,0,176,22,0,92,5,92,15,92,5,92,70,
		109,16,0,106,10,70,85,78,67,95,80,82,79,86,
		0,109,17,0,109,18,0,106,2,32,0,106,4,32,
		45,32,0,106,2,32,0,20,11,36,49,0,109,23,
		0,83,12,0,36,50,0,109,24,0,83,13,0,36,
		51,0,36,55,0,109,13,0,122,5,29,206,4,36,
		57,0,176,6,0,106,2,51,0,20,1,36,58,0,
		176,25,0,90,62,51,67,58,47,115,105,115,47,83,
		65,78,47,120,104,98,47,99,111,110,95,112,114,111,
		118,46,112,114,103,58,67,79,78,95,80,82,79,86,
		0,109,26,0,176,27,0,109,12,0,92,5,12,2,
		5,21,28,6,73,109,28,0,6,106,45,78,85,77,
		95,76,79,84,69,65,77,32,61,32,83,84,82,40,
		101,67,76,65,83,95,80,82,79,44,53,41,32,46,
		65,78,68,46,32,67,65,68,65,83,84,82,79,0,
		20,2,36,59,0,176,9,0,20,0,36,60,0,121,
		83,29,0,36,61,0,176,14,0,92,6,92,2,20,
		2,176,15,0,109,30,0,106,4,32,58,32,0,72,
		20,1,36,62,0,176,9,0,20,0,36,63,0,92,
		6,3,1,0,92,6,3,1,0,92,6,3,1,0,
		92,6,3,1,0,176,20,0,108,16,108,17,108,18,
		108,19,20,4,81,19,0,81,18,0,81,17,0,81,
		16,0,36,64,0,176,21,0,109,16,0,120,109,19,
		0,20,3,36,65,0,106,8,64,82,57,57,57,57,
		57,0,98,17,0,122,2,36,66,0,106,8,64,82,
		57,57,57,57,57,0,98,17,0,92,2,2,36,67,
		0,106,8,64,82,57,57,57,57,57,0,98,17,0,
		92,3,2,36,68,0,106,8,64,83,57,57,57,57,
		57,0,98,17,0,92,4,2,36,69,0,106,4,64,
		83,49,0,98,17,0,92,5,2,36,70,0,106,5,
		64,83,50,48,0,98,17,0,92,6,2,36,71,0,
		106,5,32,32,32,32,0,98,18,0,122,2,36,72,
		0,106,5,32,32,32,32,0,98,18,0,92,2,2,
		36,73,0,106,5,32,32,32,32,0,98,18,0,92,
		3,2,36,74,0,106,5,32,32,32,32,0,98,18,
		0,92,4,2,36,75,0,106,2,32,0,98,18,0,
		92,5,2,36,76,0,106,5,32,32,32,32,0,98,
		18,0,92,6,2,36,77,0,176,22,0,92,6,92,
		14,92,6,92,75,109,16,0,106,10,70,85,78,67,
		95,80,82,79,86,0,109,17,0,109,18,0,106,2,
		32,0,106,4,32,45,32,0,106,2,32,0,20,11,
		36,78,0,109,31,0,83,29,0,36,79,0,109,32,
		0,83,33,0,36,80,0,109,34,0,83,35,0,36,
		81,0,176,6,0,106,2,49,0,20,1,36,82,0,
		176,25,0,90,66,51,67,58,47,115,105,115,47,83,
		65,78,47,120,104,98,47,99,111,110,95,112,114,111,
		118,46,112,114,103,58,67,79,78,95,80,82,79,86,
		0,109,36,0,176,27,0,109,12,0,92,5,12,2,
		5,21,28,10,73,109,37,0,109,29,0,5,6,106,
		49,67,76,65,83,83,69,32,61,32,83,84,82,40,
		101,67,76,65,83,95,80,82,79,44,53,41,32,46,
		65,78,68,46,32,81,85,65,68,82,65,32,61,32,
		101,81,85,65,68,82,65,0,20,2,36,83,0,176,
		9,0,20,0,36,84,0,176,38,0,12,0,28,119,
		36,85,0,176,39,0,106,3,48,49,0,20,1,36,
		86,0,176,40,0,106,10,65,116,101,110,135,198,111,
		32,33,0,92,3,20,2,36,87,0,176,41,0,106,
		8,78,198,111,32,72,160,32,0,109,30,0,72,106,
		12,32,86,101,110,100,105,100,111,115,32,33,0,72,
		20,1,36,88,0,176,42,0,20,0,36,89,0,176,
		43,0,20,0,36,90,0,176,44,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,4,0,20,5,36,
		91,0,100,110,7,36,93,0,92,6,3,1,0,92,
		6,3,1,0,92,6,3,1,0,92,6,3,1,0,
		176,20,0,108,16,108,17,108,18,108,19,20,4,81,
		19,0,81,18,0,81,17,0,81,16,0,36,94,0,
		176,21,0,109,16,0,120,109,19,0,20,3,36,95,
		0,106,11,80,82,79,80,82,73,69,84,95,49,0,
		98,16,0,92,5,2,36,96,0,106,11,80,82,79,
		80,82,73,69,84,95,50,0,98,16,0,92,6,2,
		36,97,0,92,5,98,19,0,92,5,2,36,98,0,
		92,5,98,19,0,92,6,2,36,99,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,122,2,36,100,
		0,106,8,64,82,57,57,57,57,57,0,98,17,0,
		92,2,2,36,101,0,106,8,64,82,57,57,57,57,
		57,0,98,17,0,92,3,2,36,102,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,92,4,2,36,
		103,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,92,5,2,36,104,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,92,6,2,36,105,0,106,10,
		32,80,114,111,100,117,116,111,32,0,98,18,0,122,
		2,36,106,0,106,6,71,114,117,112,111,0,98,18,
		0,92,2,2,36,107,0,106,5,85,110,105,100,0,
		98,18,0,92,3,2,36,108,0,106,7,83,116,97,
		116,117,115,0,98,18,0,92,4,2,36,109,0,106,
		17,49,111,46,32,80,114,111,112,114,105,101,116,97,
		114,105,111,0,98,18,0,92,5,2,36,110,0,106,
		17,50,111,46,32,80,114,111,112,114,105,101,116,97,
		114,105,111,0,98,18,0,92,6,2,36,111,0,176,
		45,0,92,8,121,92,21,92,79,92,2,20,5,36,
		112,0,176,22,0,92,9,122,92,21,92,78,109,16,
		0,106,10,70,85,78,67,95,80,82,79,86,0,109,
		17,0,109,18,0,106,2,196,0,106,2,179,0,106,
		2,205,0,20,11,26,97,8,36,113,0,109,13,0,
		92,2,5,29,50,4,36,115,0,176,6,0,106,2,
		52,0,20,1,36,116,0,176,25,0,90,62,51,67,
		58,47,115,105,115,47,83,65,78,47,120,104,98,47,
		99,111,110,95,112,114,111,118,46,112,114,103,58,67,
		79,78,95,80,82,79,86,0,109,26,0,176,27,0,
		109,12,0,92,5,12,2,5,21,28,6,73,109,28,
		0,6,106,45,78,85,77,95,76,79,84,69,65,77,
		32,61,32,83,84,82,40,101,67,76,65,83,95,80,
		82,79,44,53,41,32,46,65,78,68,46,32,67,65,
		68,65,83,84,82,79,0,20,2,36,117,0,176,9,
		0,20,0,36,118,0,121,83,46,0,36,119,0,176,
		14,0,92,6,92,2,20,2,176,15,0,109,47,0,
		106,4,32,58,32,0,72,20,1,36,120,0,176,9,
		0,20,0,36,121,0,92,4,3,1,0,92,4,3,
		1,0,92,4,3,1,0,92,4,3,1,0,176,20,
		0,108,16,108,17,108,18,108,19,20,4,81,19,0,
		81,18,0,81,17,0,81,16,0,36,122,0,176,21,
		0,109,16,0,120,109,19,0,20,3,36,123,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,122,2,
		36,124,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,92,2,2,36,125,0,106,5,64,83,51,48,
		0,98,17,0,92,3,2,36,126,0,106,8,64,82,
		57,57,57,57,57,0,98,17,0,92,4,2,36,127,
		0,106,5,32,32,32,32,0,98,18,0,122,2,36,
		128,0,106,5,32,32,32,32,0,98,18,0,92,2,
		2,36,129,0,106,5,32,32,32,32,0,98,18,0,
		92,3,2,36,130,0,106,5,32,32,32,32,0,98,
		18,0,92,4,2,36,131,0,176,22,0,92,6,92,
		8,92,6,92,76,109,16,0,106,10,70,85,78,67,
		95,80,82,79,86,0,109,17,0,109,18,0,106,2,
		32,0,106,4,32,45,32,0,106,2,32,0,20,11,
		36,132,0,109,48,0,83,46,0,36,133,0,109,49,
		0,83,50,0,36,134,0,176,6,0,106,2,49,0,
		20,1,36,135,0,176,25,0,90,66,51,67,58,47,
		115,105,115,47,83,65,78,47,120,104,98,47,99,111,
		110,95,112,114,111,118,46,112,114,103,58,67,79,78,
		95,80,82,79,86,0,109,36,0,176,27,0,109,12,
		0,92,5,12,2,5,21,28,10,73,109,37,0,109,
		46,0,5,6,106,48,67,76,65,83,83,69,32,61,
		32,83,84,82,40,101,67,76,65,83,95,80,82,79,
		44,53,41,32,46,65,78,68,46,32,81,85,65,68,
		82,65,32,61,32,101,66,76,79,67,79,0,20,2,
		36,136,0,176,9,0,20,0,36,137,0,176,38,0,
		12,0,28,119,36,138,0,176,39,0,106,3,48,49,
		0,20,1,36,139,0,176,40,0,106,10,65,116,101,
		110,135,198,111,32,33,0,92,3,20,2,36,140,0,
		176,41,0,106,8,78,198,111,32,72,160,32,0,109,
		51,0,72,106,12,32,86,101,110,100,105,100,111,115,
		32,33,0,72,20,1,36,141,0,176,42,0,20,0,
		36,142,0,176,43,0,20,0,36,143,0,176,44,0,
		121,121,176,2,0,12,0,176,3,0,12,0,109,4,
		0,20,5,36,144,0,100,110,7,36,146,0,92,5,
		3,1,0,92,5,3,1,0,92,5,3,1,0,92,
		5,3,1,0,176,20,0,108,16,108,17,108,18,108,
		19,20,4,81,19,0,81,18,0,81,17,0,81,16,
		0,36,147,0,176,21,0,109,16,0,120,109,19,0,
		20,3,36,148,0,106,11,80,82,79,80,82,73,69,
		84,95,49,0,98,16,0,92,5,2,36,149,0,92,
		5,98,19,0,92,5,2,36,150,0,106,8,64,82,
		57,57,57,57,57,0,98,17,0,122,2,36,151,0,
		106,8,64,82,57,57,57,57,57,0,98,17,0,92,
		2,2,36,152,0,106,8,64,82,57,57,57,57,57,
		0,98,17,0,92,3,2,36,153,0,106,8,64,82,
		57,57,57,57,57,0,98,17,0,92,4,2,36,154,
		0,106,8,64,82,57,57,57,57,57,0,98,17,0,
		92,5,2,36,155,0,106,10,32,80,114,111,100,117,
		116,111,32,0,98,18,0,122,2,36,156,0,106,6,
		71,114,117,112,111,0,98,18,0,92,2,2,36,157,
		0,106,6,85,110,105,100,46,0,98,18,0,92,3,
		2,36,158,0,106,7,83,116,97,116,117,115,0,98,
		18,0,92,4,2,36,159,0,106,13,80,114,111,112,
		114,105,101,116,97,114,105,111,0,98,18,0,92,5,
		2,36,160,0,176,45,0,92,8,121,92,21,92,79,
		92,2,20,5,36,161,0,176,22,0,92,9,122,92,
		21,92,78,109,16,0,106,10,70,85,78,67,95,80,
		82,79,86,0,109,17,0,109,18,0,106,2,196,0,
		106,2,179,0,106,2,205,0,20,11,26,38,4,36,
		162,0,109,13,0,92,3,5,29,18,4,36,164,0,
		176,6,0,106,2,52,0,20,1,36,165,0,176,25,
		0,90,62,51,67,58,47,115,105,115,47,83,65,78,
		47,120,104,98,47,99,111,110,95,112,114,111,118,46,
		112,114,103,58,67,79,78,95,80,82,79,86,0,109,
		26,0,176,27,0,109,12,0,92,5,12,2,5,21,
		28,6,73,109,28,0,6,106,45,78,85,77,95,76,
		79,84,69,65,77,32,61,32,83,84,82,40,101,67,
		76,65,83,95,80,82,79,44,53,41,32,46,65,78,
		68,46,32,67,65,68,65,83,84,82,79,0,20,2,
		36,166,0,176,9,0,20,0,36,167,0,121,83,46,
		0,36,168,0,176,14,0,92,6,92,2,20,2,176,
		15,0,106,11,66,108,111,99,111,32,32,32,58,32,
		0,20,1,36,169,0,176,9,0,20,0,36,170,0,
		92,3,3,1,0,92,3,3,1,0,92,3,3,1,
		0,92,3,3,1,0,176,20,0,108,16,108,17,108,
		18,108,19,20,4,81,19,0,81,18,0,81,17,0,
		81,16,0,36,171,0,176,21,0,109,16,0,120,109,
		19,0,20,3,36,172,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,122,2,36,173,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,92,2,2,36,
		174,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,92,3,2,36,175,0,106,5,32,32,32,32,0,
		98,18,0,122,2,36,176,0,106,5,32,32,32,32,
		0,98,18,0,92,2,2,36,177,0,106,5,32,32,
		32,32,0,98,18,0,92,3,2,36,178,0,176,22,
		0,92,6,92,10,92,6,92,41,109,16,0,106,10,
		70,85,78,67,95,80,82,79,86,0,109,17,0,109,
		18,0,106,2,32,0,106,4,32,45,32,0,106,2,
		32,0,20,11,36,179,0,109,48,0,83,46,0,36,
		180,0,109,49,0,83,50,0,36,181,0,176,6,0,
		106,2,49,0,20,1,36,182,0,176,25,0,90,66,
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,112,114,111,118,46,112,114,103,
		58,67,79,78,95,80,82,79,86,0,109,36,0,176,
		27,0,109,12,0,92,5,12,2,5,21,28,10,73,
		109,37,0,109,46,0,5,6,106,48,67,76,65,83,
		83,69,32,61,32,83,84,82,40,101,67,76,65,83,
		95,80,82,79,44,53,41,32,46,65,78,68,46,32,
		81,85,65,68,82,65,32,61,32,101,66,76,79,67,
		79,0,20,2,36,183,0,176,9,0,20,0,36,184,
		0,176,38,0,12,0,28,118,36,185,0,176,39,0,
		106,3,48,49,0,20,1,36,186,0,176,40,0,106,
		10,65,116,101,110,135,198,111,32,33,0,92,3,20,
		2,36,187,0,176,41,0,106,26,78,198,111,32,72,
		160,32,73,109,111,118,101,105,115,32,86,101,110,100,
		105,100,111,115,32,33,0,20,1,36,188,0,176,42,
		0,20,0,36,189,0,176,43,0,20,0,36,190,0,
		176,44,0,121,121,176,2,0,12,0,176,3,0,12,
		0,109,4,0,20,5,36,191,0,100,110,7,36,193,
		0,92,5,3,1,0,92,5,3,1,0,92,5,3,
		1,0,92,5,3,1,0,176,20,0,108,16,108,17,
		108,18,108,19,20,4,81,19,0,81,18,0,81,17,
		0,81,16,0,36,194,0,176,21,0,109,16,0,120,
		109,19,0,20,3,36,195,0,106,11,80,82,79,80,
		82,73,69,84,95,49,0,98,16,0,92,5,2,36,
		196,0,92,5,98,19,0,92,5,2,36,197,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,122,2,
		36,198,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,92,2,2,36,199,0,106,8,64,82,57,57,
		57,57,57,0,98,17,0,92,3,2,36,200,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,92,4,
		2,36,201,0,106,8,64,82,57,57,57,57,57,0,
		98,17,0,92,5,2,36,202,0,106,10,69,109,112,
		114,101,101,110,100,46,0,98,18,0,122,2,36,203,
		0,106,6,66,108,111,99,111,0,98,18,0,92,2,
		2,36,204,0,106,5,83,97,108,97,0,98,18,0,
		92,3,2,36,205,0,106,7,83,116,97,116,117,115,
		0,98,18,0,92,4,2,36,206,0,106,13,80,114,
		111,112,114,105,101,116,97,114,105,111,0,98,18,0,
		92,5,2,36,207,0,176,45,0,92,8,121,92,21,
		92,79,92,2,20,5,36,208,0,176,22,0,92,9,
		122,92,21,92,78,109,16,0,106,10,70,85,78,67,
		95,80,82,79,86,0,109,17,0,109,18,0,106,2,
		196,0,106,2,179,0,106,2,205,0,20,11,25,10,
		36,211,0,176,52,0,20,0,36,213,0,176,43,0,
		20,0,36,214,0,176,44,0,121,121,176,2,0,12,
		0,176,3,0,12,0,109,4,0,20,5,36,215,0,
		100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( FUNC_PROV )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,51,67,58,47,115,105,115,47,83,65,78,
		47,120,104,98,47,99,111,110,95,112,114,111,118,46,
		112,114,103,58,70,85,78,67,95,80,82,79,86,0,
		71,54,0,1,71,55,0,2,36,220,0,176,20,0,
		108,56,108,57,20,2,36,221,0,176,58,0,12,0,
		83,57,0,36,222,0,122,83,56,0,36,223,0,109,
		54,0,92,4,8,29,53,2,36,225,0,109,57,0,
		92,13,8,29,41,2,36,226,0,109,55,0,92,5,
		5,29,4,1,36,227,0,176,1,0,121,121,176,2,
		0,12,0,176,3,0,12,0,12,4,83,59,0,36,
		228,0,109,60,0,83,61,0,36,229,0,176,6,0,
		106,2,53,0,20,1,36,230,0,176,62,0,109,61,
		0,100,100,20,3,36,231,0,176,63,0,92,12,92,
		13,92,14,92,67,20,4,176,64,0,92,12,92,13,
		20,2,36,232,0,176,45,0,92,12,92,13,92,14,
		92,67,92,2,20,5,36,233,0,176,65,0,106,27,
		78,111,109,101,32,100,111,32,80,114,111,112,114,105,
		101,116,97,114,105,111,32,78,111,46,32,49,0,92,
		12,20,2,36,234,0,176,64,0,92,13,92,15,20,
		2,176,66,0,109,67,0,176,68,0,100,106,8,67,
		76,73,69,78,84,69,0,100,100,100,12,5,20,2,
		48,69,0,176,70,0,109,67,0,12,1,112,0,73,
		36,235,0,176,71,0,120,20,1,4,0,0,83,67,
		0,109,67,0,73,36,236,0,176,6,0,106,2,49,
		0,20,1,36,237,0,176,42,0,20,0,36,238,0,
		176,44,0,121,121,176,2,0,12,0,176,3,0,12,
		0,109,59,0,20,5,26,25,1,36,240,0,109,55,
		0,92,6,5,29,3,1,36,241,0,176,1,0,121,
		121,176,2,0,12,0,176,3,0,12,0,12,4,83,
		72,0,36,242,0,109,73,0,83,74,0,36,243,0,
		176,6,0,106,2,53,0,20,1,36,244,0,176,62,
		0,109,74,0,100,100,20,3,36,245,0,176,63,0,
		92,12,92,13,92,14,92,67,20,4,176,64,0,92,
		12,92,13,20,2,36,246,0,176,45,0,92,12,92,
		13,92,14,92,67,92,2,20,5,36,247,0,176,65,
		0,106,27,78,111,109,101,32,100,111,32,80,114,111,
		112,114,105,101,116,97,114,105,111,32,78,111,46,32,
		50,0,92,12,20,2,36,248,0,176,64,0,92,13,
		92,15,20,2,176,66,0,109,67,0,176,68,0,100,
		106,8,67,76,73,69,78,84,69,0,100,100,100,12,
		5,20,2,48,69,0,176,70,0,109,67,0,12,1,
		112,0,73,36,249,0,176,71,0,120,20,1,4,0,
		0,83,67,0,109,67,0,73,36,250,0,176,6,0,
		106,2,49,0,20,1,36,251,0,176,42,0,20,0,
		36,252,0,176,44,0,121,121,176,2,0,12,0,176,
		3,0,12,0,109,72,0,20,5,25,12,36,254,0,
		121,83,56,0,36,0,1,36,1,1,36,3,1,109,
		56,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,112,114,111,118,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,33,192,255,255,254,255,143,
		254,255,255,255,239,255,255,251,255,255,255,253,255,247,
		255,255,255,254,255,233,240,254,127,255,95,11,0,4,
		3,0,4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}
