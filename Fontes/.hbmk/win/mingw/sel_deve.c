/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/sel_deve.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( SEL_DEVE );
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
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PDESENV );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( F_BLOQA );
HB_FUNC_EXTERN( DBEVAL );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_SEL_DEVE )
{ "SEL_DEVE", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( SEL_DEVE )}, NULL },
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
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDESC_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EQUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "NUM_LOTEAM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "CADASTRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PDESENV", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDESENV )}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
{ "EDATA_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "ETIPO_SEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "F_BLOQA", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQA )}, NULL },
{ "DBEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEVAL )}, NULL },
{ "SELECAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "EIND_CLAS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND_QUAD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EZERO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "EARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EATIVA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ATIVA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLAS_VEN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUAD_VEN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIMOV_VEN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPART_VEN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EVAZIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EINDENT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "NUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "IDENTIF", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_SEL_DEVE, "C:/sis/SAN/xhb/sel_deve.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_SEL_DEVE
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_SEL_DEVE )
   #include "hbiniseg.h"
#endif

HB_FUNC( SEL_DEVE )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,21,83,69,76,69,67,65,79,32,68,69,32,
		68,69,86,69,68,79,82,69,83,0,20,1,36,16,
		0,176,6,0,106,2,49,0,20,1,36,17,0,176,
		7,0,9,100,106,9,85,78,73,95,73,77,79,66,
		0,100,100,9,20,6,176,8,0,106,9,85,78,73,
		95,73,77,79,66,0,20,1,36,18,0,176,9,0,
		20,0,36,19,0,176,6,0,106,2,50,0,20,1,
		36,20,0,176,7,0,9,100,106,9,67,76,65,83,
		95,80,82,79,0,100,100,9,20,6,176,8,0,106,
		9,67,76,65,83,95,80,82,79,0,20,1,36,21,
		0,176,9,0,20,0,36,22,0,176,6,0,106,2,
		51,0,20,1,36,23,0,176,7,0,9,100,106,9,
		67,65,68,95,81,85,65,68,0,100,100,9,20,6,
		176,8,0,106,9,67,65,68,95,81,85,65,68,0,
		20,1,36,24,0,176,9,0,20,0,36,25,0,176,
		6,0,106,2,52,0,20,1,36,26,0,176,7,0,
		9,100,106,9,67,65,68,95,65,80,65,82,0,100,
		100,9,20,6,176,8,0,106,9,67,65,68,95,65,
		80,65,82,0,20,1,36,27,0,176,9,0,20,0,
		36,28,0,176,6,0,106,2,53,0,20,1,36,29,
		0,176,7,0,9,100,106,8,67,79,78,84,65,84,
		79,0,100,100,9,20,6,176,8,0,106,9,78,85,
		77,95,67,76,73,69,0,20,1,176,8,0,106,8,
		67,79,78,84,65,84,79,0,20,1,36,30,0,176,
		9,0,20,0,36,31,0,176,6,0,106,2,54,0,
		20,1,36,32,0,176,7,0,9,100,106,9,67,65,
		68,95,86,69,78,68,0,100,100,9,20,6,176,8,
		0,106,9,67,65,68,95,86,69,78,68,0,20,1,
		176,8,0,106,9,68,65,84,65,95,86,69,78,0,
		20,1,36,33,0,176,9,0,20,0,36,34,0,176,
		6,0,106,2,56,0,20,1,36,36,0,176,6,0,
		106,2,57,0,20,1,36,37,0,176,7,0,9,100,
		106,9,77,83,71,95,66,79,76,69,0,100,100,9,
		20,6,176,8,0,106,9,77,83,71,95,80,82,79,
		68,0,20,1,176,8,0,106,9,77,83,71,95,66,
		79,76,69,0,20,1,36,38,0,176,9,0,20,0,
		36,39,0,176,6,0,106,2,50,0,20,1,36,42,
		0,176,10,0,20,0,36,43,0,176,11,0,106,9,
		87,47,78,44,43,87,47,82,0,20,1,36,44,0,
		121,83,12,0,36,45,0,121,83,13,0,36,46,0,
		176,14,0,92,5,92,2,20,2,176,15,0,106,11,
		80,114,111,100,117,116,111,32,58,32,0,20,1,36,
		47,0,176,9,0,20,0,36,48,0,92,3,3,1,
		0,92,3,3,1,0,92,3,3,1,0,92,3,3,
		1,0,176,20,0,108,16,108,17,108,18,108,19,20,
		4,81,19,0,81,18,0,81,17,0,81,16,0,36,
		49,0,176,21,0,109,16,0,120,109,19,0,20,3,
		36,50,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,122,2,36,51,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,92,2,2,36,52,0,106,5,
		64,83,52,48,0,98,17,0,92,3,2,36,53,0,
		106,5,32,32,32,32,0,98,18,0,122,2,36,54,
		0,106,5,32,32,32,32,0,98,18,0,92,2,2,
		36,55,0,106,5,32,32,32,32,0,98,18,0,92,
		3,2,36,56,0,176,22,0,92,5,92,15,92,5,
		92,70,109,16,0,106,10,70,85,78,67,95,80,82,
		79,86,0,109,17,0,109,18,0,106,2,32,0,106,
		4,32,45,32,0,106,2,32,0,20,11,36,57,0,
		109,23,0,83,12,0,36,58,0,109,24,0,83,13,
		0,36,59,0,109,25,0,83,26,0,36,63,0,176,
		27,0,122,12,1,83,28,0,36,65,0,109,13,0,
		122,5,29,131,0,36,67,0,176,6,0,106,2,51,
		0,20,1,36,68,0,176,29,0,90,24,109,30,0,
		176,31,0,109,12,0,92,5,12,2,5,21,28,6,
		73,109,32,0,6,106,45,78,85,77,95,76,79,84,
		69,65,77,32,61,32,83,84,82,40,101,67,76,65,
		83,95,80,82,79,44,53,41,32,46,65,78,68,46,
		32,67,65,68,65,83,84,82,79,0,20,2,36,69,
		0,176,9,0,20,0,36,70,0,176,27,0,92,5,
		12,1,83,28,0,36,71,0,176,27,0,92,5,12,
		1,83,33,0,26,34,1,36,72,0,109,13,0,92,
		2,5,29,131,0,36,74,0,176,6,0,106,2,52,
		0,20,1,36,75,0,176,29,0,90,24,109,30,0,
		176,31,0,109,12,0,92,5,12,2,5,21,28,6,
		73,109,32,0,6,106,45,78,85,77,95,76,79,84,
		69,65,77,32,61,32,83,84,82,40,101,67,76,65,
		83,95,80,82,79,44,53,41,32,46,65,78,68,46,
		32,67,65,68,65,83,84,82,79,0,20,2,36,76,
		0,176,9,0,20,0,36,77,0,176,27,0,92,5,
		12,1,83,28,0,36,78,0,176,27,0,92,5,12,
		1,83,33,0,26,150,0,36,79,0,109,13,0,92,
		3,5,29,130,0,36,81,0,176,6,0,106,2,52,
		0,20,1,36,82,0,176,29,0,90,24,109,30,0,
		176,31,0,109,12,0,92,5,12,2,5,21,28,6,
		73,109,32,0,6,106,45,78,85,77,95,76,79,84,
		69,65,77,32,61,32,83,84,82,40,101,67,76,65,
		83,95,80,82,79,44,53,41,32,46,65,78,68,46,
		32,67,65,68,65,83,84,82,79,0,20,2,36,83,
		0,176,9,0,20,0,36,84,0,176,27,0,92,5,
		12,1,83,28,0,36,85,0,176,34,0,106,3,48,
		49,0,20,1,25,10,36,88,0,176,35,0,20,0,
		36,90,0,176,36,0,106,11,48,49,47,48,49,47,
		49,57,48,48,0,12,1,83,37,0,36,91,0,134,
		0,0,0,0,83,38,0,36,93,0,176,14,0,92,
		8,92,2,20,2,176,15,0,106,24,76,105,109,105,
		116,101,32,73,110,97,100,105,109,112,108,135,110,99,
		105,97,32,58,32,0,20,1,176,39,0,176,40,0,
		12,0,176,41,0,12,0,122,72,20,2,176,42,0,
		109,43,0,176,44,0,100,106,10,101,68,65,84,65,
		95,70,73,77,0,100,100,100,12,5,20,2,48,45,
		0,176,46,0,109,43,0,12,1,112,0,73,36,94,
		0,176,47,0,109,43,0,100,100,100,100,100,100,20,
		7,4,0,0,83,43,0,109,43,0,73,36,95,0,
		109,38,0,134,0,0,0,0,5,28,49,36,96,0,
		176,34,0,106,3,48,49,0,20,1,36,97,0,176,
		48,0,106,16,68,97,116,97,32,73,110,118,97,108,
		105,100,97,32,33,0,92,3,20,2,36,98,0,26,
		68,255,36,103,0,176,34,0,106,3,48,49,0,20,
		1,36,104,0,176,27,0,122,12,1,83,49,0,36,
		105,0,121,83,50,0,36,106,0,121,83,51,0,36,
		107,0,176,14,0,92,23,121,20,2,176,15,0,106,
		49,83,101,108,101,135,198,111,32,84,111,116,97,108,
		32,111,117,32,80,111,114,32,78,135,109,101,114,111,
		32,100,101,32,80,97,114,99,101,108,97,115,32,40,
		84,47,78,41,32,63,32,0,20,1,176,39,0,176,
		40,0,12,0,176,41,0,12,0,122,72,20,2,176,
		42,0,109,43,0,176,44,0,100,106,10,101,84,73,
		80,79,95,83,69,76,0,106,4,64,33,88,0,90,
		12,109,49,0,106,3,84,78,0,24,6,100,12,5,
		20,2,48,45,0,176,46,0,109,43,0,12,1,112,
		0,73,36,108,0,176,47,0,109,43,0,100,100,100,
		100,100,100,20,7,4,0,0,83,43,0,109,43,0,
		73,36,109,0,109,49,0,106,2,78,0,5,29,97,
		1,36,111,0,176,34,0,106,3,48,49,0,20,1,
		36,112,0,176,14,0,92,23,121,20,2,176,15,0,
		106,38,73,110,102,111,114,109,101,32,111,32,73,110,
		116,101,114,118,97,108,111,32,100,101,32,80,97,114,
		99,101,108,97,115,32,100,101,32,58,32,0,20,1,
		176,39,0,176,40,0,12,0,176,41,0,12,0,122,
		72,20,2,176,42,0,109,43,0,176,44,0,100,106,
		9,101,78,85,77,95,73,78,73,0,106,7,64,82,
		32,57,57,57,0,100,100,12,5,20,2,48,45,0,
		176,46,0,109,43,0,12,1,112,0,73,36,113,0,
		176,14,0,92,23,92,43,20,2,176,15,0,106,7,
		97,116,135,32,58,32,0,20,1,176,39,0,176,40,
		0,12,0,176,41,0,12,0,122,72,20,2,176,42,
		0,109,43,0,176,44,0,100,106,9,101,78,85,77,
		95,70,73,77,0,106,7,64,82,32,57,57,57,0,
		100,100,12,5,20,2,48,45,0,176,46,0,109,43,
		0,12,1,112,0,73,36,114,0,176,47,0,109,43,
		0,100,100,100,100,100,100,20,7,4,0,0,83,43,
		0,109,43,0,73,36,115,0,109,50,0,121,69,29,
		248,254,109,50,0,121,69,29,240,254,36,116,0,109,
		51,0,109,50,0,35,28,65,36,117,0,176,34,0,
		106,3,48,49,0,20,1,36,118,0,176,48,0,106,
		32,78,111,46,32,70,105,110,97,108,32,77,101,110,
		111,114,32,113,117,101,32,78,111,46,32,73,110,105,
		99,105,97,108,0,92,3,20,2,36,119,0,26,165,
		254,36,126,0,176,34,0,106,3,48,49,0,20,1,
		36,127,0,176,48,0,106,10,65,116,101,110,135,198,
		111,32,33,0,92,3,20,2,36,128,0,176,52,0,
		106,41,65,103,117,97,114,100,101,32,97,32,109,111,
		110,116,97,103,101,109,32,100,111,32,66,97,110,99,
		111,32,100,101,32,68,97,100,111,115,32,46,46,46,
		0,20,1,36,129,0,109,13,0,122,5,29,200,5,
		36,130,0,176,6,0,106,2,53,0,20,1,36,131,
		0,176,53,0,92,8,106,3,79,78,0,20,2,36,
		132,0,176,7,0,9,100,106,8,67,79,78,84,65,
		84,79,0,100,100,9,20,6,176,8,0,106,9,78,
		85,77,95,67,76,73,69,0,20,1,176,8,0,106,
		8,67,79,78,84,65,84,79,0,20,1,36,133,0,
		176,9,0,20,0,36,134,0,176,54,0,12,0,28,
		34,36,135,0,176,55,0,90,11,106,2,32,0,165,
		78,56,0,6,100,100,100,100,9,20,6,36,136,0,
		176,57,0,20,0,36,138,0,176,53,0,92,8,106,
		4,79,70,70,0,20,2,36,139,0,176,6,0,106,
		2,51,0,20,1,36,140,0,176,29,0,90,24,176,
		31,0,109,12,0,92,5,12,2,109,30,0,5,21,
		28,6,73,109,32,0,6,106,45,83,84,82,40,101,
		67,76,65,83,95,80,82,79,44,53,41,32,61,32,
		78,85,77,95,76,79,84,69,65,77,32,46,65,78,
		68,46,32,67,65,68,65,83,84,82,79,0,20,2,
		36,141,0,176,9,0,20,0,36,142,0,176,58,0,
		12,0,32,193,4,36,143,0,176,31,0,109,12,0,
		92,5,12,2,83,59,0,36,144,0,109,60,0,83,
		61,0,36,145,0,176,6,0,106,2,56,0,20,1,
		36,146,0,106,4,48,48,48,0,83,62,0,36,147,
		0,106,3,67,80,0,176,63,0,109,62,0,122,92,
		3,176,64,0,176,65,0,109,59,0,12,1,12,1,
		49,12,3,72,176,65,0,109,59,0,12,1,72,176,
		63,0,109,62,0,122,92,3,176,64,0,176,65,0,
		109,61,0,12,1,12,1,49,12,3,72,176,65,0,
		109,61,0,12,1,72,83,66,0,36,148,0,106,3,
		73,71,0,176,63,0,109,62,0,122,92,3,176,64,
		0,176,65,0,109,59,0,12,1,12,1,49,12,3,
		72,176,65,0,109,59,0,12,1,72,176,63,0,109,
		62,0,122,92,3,176,64,0,176,65,0,109,61,0,
		12,1,12,1,49,12,3,72,176,65,0,109,61,0,
		12,1,72,83,67,0,36,149,0,176,7,0,9,100,
		109,66,0,100,100,9,20,6,176,8,0,109,67,0,
		20,1,36,150,0,176,9,0,20,0,36,151,0,120,
		83,68,0,36,152,0,176,29,0,90,45,109,69,0,
		109,37,0,16,21,28,34,73,109,69,0,109,38,0,
		34,21,28,23,73,109,70,0,134,0,0,0,0,5,
		21,28,10,73,109,71,0,109,68,0,5,6,106,102,
		40,68,65,84,65,95,86,69,78,67,32,62,61,32,
		101,68,65,84,65,95,73,78,73,32,46,65,78,68,
		46,32,68,65,84,65,95,86,69,78,67,32,60,61,
		32,101,68,65,84,65,95,70,73,77,41,32,46,65,
		78,68,46,32,68,65,84,65,95,80,71,84,79,32,
		61,32,67,84,79,68,40,34,34,41,32,46,65,78,
		68,46,32,65,84,73,86,65,32,61,32,101,65,84,
		73,86,65,0,20,2,36,153,0,176,9,0,20,0,
		36,154,0,121,83,72,0,36,155,0,109,73,0,83,
		74,0,36,156,0,109,75,0,83,76,0,36,157,0,
		109,77,0,83,78,0,36,158,0,176,31,0,109,79,
		0,122,12,2,83,80,0,36,159,0,106,3,48,48,
		0,83,81,0,36,160,0,176,63,0,109,81,0,122,
		92,2,176,64,0,176,65,0,109,74,0,12,1,12,
		1,49,12,3,176,65,0,109,74,0,12,1,72,176,
		63,0,109,81,0,122,92,2,176,64,0,176,65,0,
		109,76,0,12,1,12,1,49,12,3,72,176,65,0,
		109,76,0,12,1,72,176,63,0,109,62,0,122,92,
		3,176,64,0,176,65,0,109,78,0,12,1,12,1,
		49,12,3,72,176,65,0,109,78,0,12,1,72,176,
		65,0,109,80,0,12,1,72,83,82,0,36,161,0,
		176,58,0,12,0,32,76,2,36,162,0,176,6,0,
		106,2,56,0,20,1,36,163,0,176,58,0,12,0,
		32,185,0,109,74,0,109,73,0,5,29,175,0,109,
		76,0,109,75,0,5,29,165,0,109,78,0,109,77,
		0,5,29,155,0,109,80,0,176,31,0,109,79,0,
		122,12,2,5,29,139,0,36,164,0,176,34,0,106,
		3,48,49,0,20,1,36,165,0,109,72,0,122,72,
		83,72,0,36,166,0,176,52,0,106,15,80,114,111,
		99,101,115,115,97,110,100,111,32,58,32,0,176,65,
		0,109,74,0,12,1,72,106,2,47,0,72,176,65,
		0,109,76,0,12,1,72,106,2,47,0,72,176,65,
		0,109,78,0,12,1,72,106,2,47,0,72,176,65,
		0,109,80,0,12,1,72,106,5,32,45,32,32,0,
		72,176,65,0,176,31,0,109,72,0,92,3,12,2,
		12,1,72,20,1,36,167,0,176,83,0,122,20,1,
		26,66,255,36,169,0,176,6,0,106,2,54,0,20,
		1,36,170,0,176,9,0,20,0,36,171,0,176,84,
		0,109,74,0,109,76,0,72,109,78,0,72,109,80,
		0,72,100,100,20,3,36,172,0,109,85,0,83,86,
		0,36,173,0,176,6,0,106,2,53,0,20,1,36,
		174,0,176,84,0,109,86,0,100,100,20,3,36,175,
		0,109,49,0,106,2,84,0,5,28,41,36,176,0,
		176,87,0,12,0,28,26,36,178,0,106,2,88,0,
		78,56,0,109,82,0,78,88,0,36,179,0,176,57,
		0,20,0,36,180,0,25,60,36,182,0,109,72,0,
		109,50,0,16,28,45,109,72,0,109,51,0,34,28,
		36,36,183,0,176,87,0,12,0,28,26,36,185,0,
		106,2,88,0,78,56,0,109,82,0,78,88,0,36,
		186,0,176,57,0,20,0,36,190,0,121,83,72,0,
		36,191,0,176,6,0,106,2,56,0,20,1,36,192,
		0,109,73,0,83,74,0,36,193,0,109,75,0,83,
		76,0,36,194,0,109,77,0,83,78,0,36,195,0,
		176,31,0,109,79,0,122,12,2,83,80,0,36,196,
		0,106,3,48,48,0,83,81,0,36,197,0,176,63,
		0,109,81,0,122,92,2,176,64,0,176,65,0,109,
		74,0,12,1,12,1,49,12,3,176,65,0,109,74,
		0,12,1,72,176,63,0,109,81,0,122,92,2,176,
		64,0,176,65,0,109,76,0,12,1,12,1,49,12,
		3,72,176,65,0,109,76,0,12,1,72,176,63,0,
		109,62,0,122,92,3,176,64,0,176,65,0,109,78,
		0,12,1,12,1,49,12,3,72,176,65,0,109,78,
		0,12,1,72,176,65,0,109,80,0,12,1,72,83,
		82,0,26,175,253,36,199,0,176,6,0,106,2,51,
		0,20,1,36,200,0,176,83,0,122,20,1,26,58,
		251,36,201,0,26,135,5,36,203,0,176,6,0,106,
		2,53,0,20,1,36,204,0,176,53,0,92,8,106,
		3,79,78,0,20,2,36,205,0,176,7,0,9,100,
		106,8,67,79,78,84,65,84,79,0,100,100,9,20,
		6,176,8,0,106,9,78,85,77,95,67,76,73,69,
		0,20,1,176,8,0,106,8,67,79,78,84,65,84,
		79,0,20,1,36,206,0,176,9,0,20,0,36,207,
		0,176,54,0,12,0,28,34,36,208,0,176,55,0,
		90,11,106,2,32,0,165,78,56,0,6,100,100,100,
		100,9,20,6,36,209,0,176,57,0,20,0,36,211,
		0,176,53,0,92,8,106,4,79,70,70,0,20,2,
		36,212,0,176,6,0,106,2,52,0,20,1,36,213,
		0,176,29,0,90,24,176,31,0,109,12,0,92,5,
		12,2,109,30,0,5,21,28,6,73,109,32,0,6,
		106,45,83,84,82,40,101,67,76,65,83,95,80,82,
		79,44,53,41,32,61,32,78,85,77,95,76,79,84,
		69,65,77,32,46,65,78,68,46,32,67,65,68,65,
		83,84,82,79,0,20,2,36,214,0,176,9,0,20,
		0,36,215,0,176,58,0,12,0,32,131,4,36,216,
		0,176,31,0,109,12,0,92,5,12,2,83,59,0,
		36,217,0,109,89,0,83,61,0,36,218,0,176,6,
		0,106,2,56,0,20,1,36,219,0,106,4,48,48,
		48,0,83,62,0,36,220,0,106,3,67,80,0,176,
		63,0,109,62,0,122,92,3,176,64,0,176,65,0,
		109,59,0,12,1,12,1,49,12,3,72,176,65,0,
		109,59,0,12,1,72,176,63,0,109,62,0,122,92,
		3,176,64,0,176,65,0,109,61,0,12,1,12,1,
		49,12,3,72,176,65,0,109,61,0,12,1,72,83,
		66,0,36,221,0,106,3,73,71,0,176,63,0,109,
		62,0,122,92,3,176,64,0,176,65,0,109,59,0,
		12,1,12,1,49,12,3,72,176,65,0,109,59,0,
		12,1,72,176,63,0,109,62,0,122,92,3,176,64,
		0,176,65,0,109,61,0,12,1,12,1,49,12,3,
		72,176,65,0,109,61,0,12,1,72,83,67,0,36,
		222,0,176,7,0,9,100,109,66,0,100,100,9,20,
		6,176,8,0,109,67,0,20,1,36,223,0,176,9,
		0,20,0,36,224,0,176,29,0,90,30,109,69,0,
		109,38,0,34,21,28,19,73,109,70,0,134,0,0,
		0,0,5,21,28,6,73,109,71,0,6,106,62,68,
		65,84,65,95,86,69,78,67,32,60,61,32,101,68,
		65,84,65,95,70,73,77,32,46,65,78,68,46,32,
		68,65,84,65,95,80,71,84,79,32,61,32,67,84,
		79,68,40,34,34,41,32,46,65,78,68,46,32,65,
		84,73,86,65,0,20,2,36,225,0,176,9,0,20,
		0,36,226,0,121,83,72,0,36,227,0,109,73,0,
		83,74,0,36,228,0,109,75,0,83,76,0,36,229,
		0,109,77,0,83,78,0,36,230,0,176,31,0,109,
		79,0,122,12,2,83,80,0,36,231,0,106,3,48,
		48,0,83,81,0,36,232,0,176,63,0,109,81,0,
		122,92,2,176,64,0,176,65,0,109,74,0,12,1,
		12,1,49,12,3,176,65,0,109,74,0,12,1,72,
		176,63,0,109,81,0,122,92,2,176,64,0,176,65,
		0,109,76,0,12,1,12,1,49,12,3,72,176,65,
		0,109,76,0,12,1,72,176,63,0,109,62,0,122,
		92,3,176,64,0,176,65,0,109,78,0,12,1,12,
		1,49,12,3,72,176,65,0,109,78,0,12,1,72,
		176,65,0,109,80,0,12,1,72,83,82,0,36,233,
		0,176,58,0,12,0,32,76,2,36,234,0,176,6,
		0,106,2,56,0,20,1,36,235,0,176,58,0,12,
		0,32,185,0,109,74,0,109,73,0,5,29,175,0,
		109,76,0,109,75,0,5,29,165,0,109,78,0,109,
		77,0,5,29,155,0,109,80,0,176,31,0,109,79,
		0,122,12,2,5,29,139,0,36,236,0,176,34,0,
		106,3,48,49,0,20,1,36,237,0,109,72,0,122,
		72,83,72,0,36,238,0,176,52,0,106,15,80,114,
		111,99,101,115,115,97,110,100,111,32,58,32,0,176,
		65,0,109,74,0,12,1,72,106,2,47,0,72,176,
		65,0,109,76,0,12,1,72,106,2,47,0,72,176,
		65,0,109,78,0,12,1,72,106,2,47,0,72,176,
		65,0,109,80,0,12,1,72,106,5,32,45,32,32,
		0,72,176,65,0,176,31,0,109,72,0,92,3,12,
		2,12,1,72,20,1,36,239,0,176,83,0,122,20,
		1,26,66,255,36,241,0,176,6,0,106,2,54,0,
		20,1,36,242,0,176,9,0,20,0,36,243,0,176,
		84,0,109,74,0,109,76,0,72,109,78,0,72,109,
		80,0,72,100,100,20,3,36,244,0,109,85,0,83,
		86,0,36,245,0,176,6,0,106,2,53,0,20,1,
		36,246,0,176,84,0,109,86,0,100,100,20,3,36,
		247,0,109,49,0,106,2,84,0,5,28,41,36,248,
		0,176,87,0,12,0,28,26,36,250,0,106,2,88,
		0,78,56,0,109,82,0,78,88,0,36,251,0,176,
		57,0,20,0,36,252,0,25,60,36,254,0,109,72,
		0,109,50,0,16,28,45,109,72,0,109,51,0,34,
		28,36,36,255,0,176,87,0,12,0,28,26,36,1,
		1,106,2,88,0,78,56,0,109,82,0,78,88,0,
		36,2,1,176,57,0,20,0,36,6,1,121,83,72,
		0,36,7,1,176,6,0,106,2,56,0,20,1,36,
		8,1,109,73,0,83,74,0,36,9,1,109,75,0,
		83,76,0,36,10,1,109,77,0,83,78,0,36,11,
		1,176,31,0,109,79,0,122,12,2,83,80,0,36,
		12,1,106,3,48,48,0,83,81,0,36,13,1,176,
		63,0,109,81,0,122,92,2,176,64,0,176,65,0,
		109,74,0,12,1,12,1,49,12,3,176,65,0,109,
		74,0,12,1,72,176,63,0,109,81,0,122,92,2,
		176,64,0,176,65,0,109,76,0,12,1,12,1,49,
		12,3,72,176,65,0,109,76,0,12,1,72,176,63,
		0,109,62,0,122,92,3,176,64,0,176,65,0,109,
		78,0,12,1,12,1,49,12,3,72,176,65,0,109,
		78,0,12,1,72,176,65,0,109,80,0,12,1,72,
		83,82,0,26,175,253,36,15,1,176,6,0,106,2,
		52,0,20,1,36,16,1,176,83,0,122,20,1,26,
		120,251,36,19,1,176,34,0,106,3,48,49,0,20,
		1,36,20,1,176,48,0,106,20,83,101,108,101,135,
		198,111,32,67,111,110,99,108,117,135,100,97,32,33,
		0,92,3,20,2,36,21,1,176,53,0,92,20,106,
		7,83,67,82,69,69,78,0,20,2,36,22,1,176,
		90,0,20,0,36,23,1,176,91,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,4,0,20,5,36,
		24,1,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

