/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_inad.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_INAD );
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
HB_FUNC_EXTERN( PPERG );
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
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( F_BLOQA );
HB_FUNC_EXTERN( __DBZAP );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( __DBAPP );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBCREATEINDEX );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( DBDELETE );
HB_FUNC_EXTERN( __DBPACK );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( AFILL );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_INAD )
{ "CON_INAD", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_INAD )}, NULL },
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
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISGRUPO1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISGRUPO2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "EINTER_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EINTER_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND_CLAS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND_QUAD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EZERO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "EARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ATIVA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "F_BLOQA", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQA )}, NULL },
{ "__DBZAP", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBZAP )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "__DBAPP", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBAPP )}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DBCREATEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCREATEINDEX )}, NULL },
{ "NUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "ECONTADOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "DBDELETE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBDELETE )}, NULL },
{ "__DBPACK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBPACK )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "EDATA_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VDEVEDOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VVLR_DEVE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "AFILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFILL )}, NULL },
{ "ETOT_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_NPAR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_ANTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VLR_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_CASO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EAUX", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_INAD, "C:/sis/SAN/xhb/con_inad.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_INAD
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_INAD )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_INAD )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,105,110,97,100,46,112,114,103,
		58,67,79,78,95,73,78,65,68,0,36,14,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,15,0,176,5,0,106,32,67,
		79,78,83,85,76,84,65,32,84,79,84,65,76,32,
		68,69,32,73,78,65,68,73,77,80,76,69,78,84,
		69,83,0,20,1,36,16,0,176,6,0,106,2,49,
		0,20,1,36,17,0,176,7,0,9,100,106,9,85,
		78,73,95,73,77,79,66,0,100,100,9,20,6,176,
		8,0,106,9,85,78,73,95,73,77,79,66,0,20,
		1,36,18,0,176,9,0,20,0,36,19,0,176,6,
		0,106,2,50,0,20,1,36,20,0,176,7,0,9,
		100,106,9,67,76,65,83,95,80,82,79,0,100,100,
		9,20,6,176,8,0,106,9,67,76,65,83,95,80,
		82,79,0,20,1,36,21,0,176,9,0,20,0,36,
		22,0,176,6,0,106,2,51,0,20,1,36,23,0,
		176,7,0,9,100,106,9,67,65,68,95,81,85,65,
		68,0,100,100,9,20,6,176,8,0,106,9,67,65,
		68,95,81,85,65,68,0,20,1,36,24,0,176,9,
		0,20,0,36,25,0,176,6,0,106,2,52,0,20,
		1,36,26,0,176,7,0,9,100,106,9,67,65,68,
		95,65,80,65,82,0,100,100,9,20,6,176,8,0,
		106,9,67,65,68,95,65,80,65,82,0,20,1,36,
		27,0,176,9,0,20,0,36,28,0,176,6,0,106,
		2,53,0,20,1,36,29,0,176,7,0,9,100,106,
		8,67,79,78,84,65,84,79,0,100,100,9,20,6,
		176,8,0,106,9,78,85,77,95,67,76,73,69,0,
		20,1,176,8,0,106,8,67,79,78,84,65,84,79,
		0,20,1,36,30,0,176,9,0,20,0,36,31,0,
		176,6,0,106,2,54,0,20,1,36,32,0,176,7,
		0,9,100,106,9,67,65,68,95,86,69,78,68,0,
		100,100,9,20,6,176,8,0,106,9,67,65,68,95,
		86,69,78,68,0,20,1,176,8,0,106,9,68,65,
		84,65,95,86,69,78,0,20,1,36,33,0,176,9,
		0,20,0,36,34,0,176,6,0,106,2,56,0,20,
		1,36,36,0,176,6,0,106,2,57,0,20,1,36,
		37,0,176,7,0,9,100,106,9,77,83,71,95,66,
		79,76,69,0,100,100,9,20,6,176,8,0,106,9,
		77,83,71,95,80,82,79,68,0,20,1,176,8,0,
		106,9,77,83,71,95,66,79,76,69,0,20,1,36,
		38,0,176,9,0,20,0,36,39,0,176,6,0,106,
		2,50,0,20,1,36,41,0,36,42,0,176,10,0,
		20,0,36,43,0,176,11,0,106,9,87,47,78,44,
		43,87,47,82,0,20,1,36,44,0,121,83,12,0,
		36,45,0,121,83,13,0,36,46,0,176,14,0,92,
		5,92,2,20,2,176,15,0,106,11,80,114,111,100,
		117,116,111,32,58,32,0,20,1,36,47,0,176,9,
		0,20,0,36,48,0,92,3,3,1,0,92,3,3,
		1,0,92,3,3,1,0,92,3,3,1,0,176,20,
		0,108,16,108,17,108,18,108,19,20,4,81,19,0,
		81,18,0,81,17,0,81,16,0,36,49,0,176,21,
		0,109,16,0,120,109,19,0,20,3,36,50,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,122,2,
		36,51,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,92,2,2,36,52,0,106,5,64,83,52,48,
		0,98,17,0,92,3,2,36,53,0,106,5,32,32,
		32,32,0,98,18,0,122,2,36,54,0,106,5,32,
		32,32,32,0,98,18,0,92,2,2,36,55,0,106,
		5,32,32,32,32,0,98,18,0,92,3,2,36,56,
		0,176,22,0,92,5,92,15,92,5,92,70,109,16,
		0,106,10,70,85,78,67,95,80,82,79,86,0,109,
		17,0,109,18,0,106,2,32,0,106,4,32,45,32,
		0,106,2,32,0,20,11,36,57,0,109,23,0,83,
		12,0,36,58,0,109,24,0,83,13,0,36,59,0,
		109,25,0,83,26,0,36,60,0,36,63,0,176,27,
		0,122,12,1,83,28,0,36,65,0,109,13,0,122,
		5,29,107,2,36,67,0,176,6,0,106,2,51,0,
		20,1,36,68,0,176,29,0,90,62,51,67,58,47,
		115,105,115,47,83,65,78,47,120,104,98,47,99,111,
		110,95,105,110,97,100,46,112,114,103,58,67,79,78,
		95,73,78,65,68,0,109,30,0,176,31,0,109,12,
		0,92,5,12,2,5,21,28,6,73,109,32,0,6,
		106,45,78,85,77,95,76,79,84,69,65,77,32,61,
		32,83,84,82,40,101,67,76,65,83,95,80,82,79,
		44,53,41,32,46,65,78,68,46,32,67,65,68,65,
		83,84,82,79,0,20,2,36,69,0,176,9,0,20,
		0,36,70,0,176,27,0,92,5,12,1,83,28,0,
		36,71,0,176,27,0,92,5,12,1,83,33,0,36,
		72,0,176,34,0,106,3,48,49,0,20,1,36,73,
		0,176,35,0,106,19,68,101,115,101,106,97,32,83,
		101,108,101,99,105,111,110,97,114,32,0,109,36,0,
		72,106,10,32,40,83,47,78,41,32,63,32,0,72,
		20,1,36,74,0,109,37,0,106,2,83,0,5,29,
		179,5,36,75,0,176,14,0,92,6,92,2,20,2,
		176,15,0,109,36,0,106,4,32,58,32,0,72,20,
		1,36,76,0,176,9,0,20,0,36,77,0,92,6,
		3,1,0,92,6,3,1,0,92,6,3,1,0,92,
		6,3,1,0,176,20,0,108,16,108,17,108,18,108,
		19,20,4,81,19,0,81,18,0,81,17,0,81,16,
		0,36,78,0,176,21,0,109,16,0,120,109,19,0,
		20,3,36,79,0,106,8,64,82,57,57,57,57,57,
		0,98,17,0,122,2,36,80,0,106,8,64,82,57,
		57,57,57,57,0,98,17,0,92,2,2,36,81,0,
		106,8,64,82,57,57,57,57,57,0,98,17,0,92,
		3,2,36,82,0,106,8,64,83,57,57,57,57,57,
		0,98,17,0,92,4,2,36,83,0,106,4,64,83,
		49,0,98,17,0,92,5,2,36,84,0,106,5,64,
		83,50,48,0,98,17,0,92,6,2,36,85,0,106,
		5,32,32,32,32,0,98,18,0,122,2,36,86,0,
		106,5,32,32,32,32,0,98,18,0,92,2,2,36,
		87,0,106,5,32,32,32,32,0,98,18,0,92,3,
		2,36,88,0,106,5,32,32,32,32,0,98,18,0,
		92,4,2,36,89,0,106,2,32,0,98,18,0,92,
		5,2,36,90,0,106,5,32,32,32,32,0,98,18,
		0,92,6,2,36,91,0,176,22,0,92,6,92,14,
		92,6,92,75,109,16,0,106,10,70,85,78,67,95,
		67,79,78,83,0,109,17,0,109,18,0,106,2,32,
		0,106,4,32,45,32,0,106,2,32,0,20,11,36,
		92,0,109,38,0,83,28,0,36,93,0,109,39,0,
		83,40,0,36,94,0,109,41,0,83,42,0,36,95,
		0,26,55,4,36,96,0,109,13,0,92,2,5,29,
		35,2,36,98,0,176,6,0,106,2,52,0,20,1,
		36,99,0,176,29,0,90,62,51,67,58,47,115,105,
		115,47,83,65,78,47,120,104,98,47,99,111,110,95,
		105,110,97,100,46,112,114,103,58,67,79,78,95,73,
		78,65,68,0,109,30,0,176,31,0,109,12,0,92,
		5,12,2,5,21,28,6,73,109,32,0,6,106,45,
		78,85,77,95,76,79,84,69,65,77,32,61,32,83,
		84,82,40,101,67,76,65,83,95,80,82,79,44,53,
		41,32,46,65,78,68,46,32,67,65,68,65,83,84,
		82,79,0,20,2,36,100,0,176,9,0,20,0,36,
		101,0,176,27,0,92,5,12,1,83,28,0,36,102,
		0,176,27,0,92,5,12,1,83,33,0,36,103,0,
		176,34,0,106,3,48,49,0,20,1,36,104,0,176,
		35,0,106,19,68,101,115,101,106,97,32,83,101,108,
		101,99,105,111,110,97,114,32,0,109,43,0,72,106,
		10,32,40,83,47,78,41,32,63,32,0,72,20,1,
		36,105,0,109,37,0,106,2,83,0,5,29,63,3,
		36,106,0,176,14,0,92,6,92,2,20,2,176,15,
		0,109,43,0,106,4,32,58,32,0,72,20,1,36,
		107,0,176,9,0,20,0,36,108,0,92,4,3,1,
		0,92,4,3,1,0,92,4,3,1,0,92,4,3,
		1,0,176,20,0,108,16,108,17,108,18,108,19,20,
		4,81,19,0,81,18,0,81,17,0,81,16,0,36,
		109,0,176,21,0,109,16,0,120,109,19,0,20,3,
		36,110,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,122,2,36,111,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,92,2,2,36,112,0,106,5,
		64,83,51,48,0,98,17,0,92,3,2,36,113,0,
		106,8,64,82,57,57,57,57,57,0,98,17,0,92,
		4,2,36,114,0,106,5,32,32,32,32,0,98,18,
		0,122,2,36,115,0,106,5,32,32,32,32,0,98,
		18,0,92,2,2,36,116,0,106,5,32,32,32,32,
		0,98,18,0,92,3,2,36,117,0,106,5,32,32,
		32,32,0,98,18,0,92,4,2,36,118,0,176,22,
		0,92,6,92,8,92,6,92,76,109,16,0,106,10,
		70,85,78,67,95,67,79,78,83,0,109,17,0,109,
		18,0,106,2,32,0,106,4,32,45,32,0,106,2,
		32,0,20,11,36,119,0,109,44,0,83,28,0,36,
		120,0,109,45,0,83,46,0,36,121,0,26,11,2,
		36,122,0,109,13,0,92,3,5,29,247,1,36,124,
		0,176,6,0,106,2,52,0,20,1,36,125,0,176,
		29,0,90,62,51,67,58,47,115,105,115,47,83,65,
		78,47,120,104,98,47,99,111,110,95,105,110,97,100,
		46,112,114,103,58,67,79,78,95,73,78,65,68,0,
		109,30,0,176,31,0,109,12,0,92,5,12,2,5,
		21,28,6,73,109,32,0,6,106,45,78,85,77,95,
		76,79,84,69,65,77,32,61,32,83,84,82,40,101,
		67,76,65,83,95,80,82,79,44,53,41,32,46,65,
		78,68,46,32,67,65,68,65,83,84,82,79,0,20,
		2,36,126,0,176,9,0,20,0,36,127,0,176,27,
		0,92,5,12,1,83,28,0,36,128,0,176,34,0,
		106,3,48,49,0,20,1,36,129,0,176,35,0,106,
		35,68,101,115,101,106,97,32,83,101,108,101,99,105,
		111,110,97,114,32,111,32,66,108,111,99,111,32,40,
		83,47,78,41,32,63,32,0,20,1,36,130,0,109,
		37,0,106,2,83,0,5,29,33,1,36,131,0,176,
		14,0,92,6,92,2,20,2,176,15,0,106,11,66,
		108,111,99,111,32,32,32,58,32,0,20,1,36,132,
		0,176,9,0,20,0,36,133,0,92,3,3,1,0,
		92,3,3,1,0,92,3,3,1,0,92,3,3,1,
		0,176,20,0,108,16,108,17,108,18,108,19,20,4,
		81,19,0,81,18,0,81,17,0,81,16,0,36,134,
		0,176,21,0,109,16,0,120,109,19,0,20,3,36,
		135,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,122,2,36,136,0,106,8,64,82,57,57,57,57,
		57,0,98,17,0,92,2,2,36,137,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,92,3,2,36,
		138,0,106,5,32,32,32,32,0,98,18,0,122,2,
		36,139,0,106,5,32,32,32,32,0,98,18,0,92,
		2,2,36,140,0,106,5,32,32,32,32,0,98,18,
		0,92,3,2,36,141,0,176,22,0,92,6,92,10,
		92,6,92,41,109,16,0,106,10,70,85,78,67,95,
		67,79,78,83,0,109,17,0,109,18,0,106,2,32,
		0,106,4,32,45,32,0,106,2,32,0,20,11,36,
		142,0,109,44,0,83,28,0,36,143,0,109,45,0,
		83,46,0,36,144,0,25,10,36,147,0,176,47,0,
		20,0,36,149,0,176,48,0,106,11,48,49,47,48,
		49,47,49,57,56,48,0,12,1,83,49,0,36,150,
		0,134,0,0,0,0,83,50,0,36,151,0,36,152,
		0,176,14,0,92,8,92,2,20,2,176,15,0,106,
		11,80,101,114,105,111,100,111,32,58,32,0,20,1,
		176,51,0,176,52,0,12,0,176,53,0,12,0,122,
		72,20,2,176,54,0,109,55,0,176,56,0,100,106,
		10,101,68,65,84,65,95,73,78,73,0,100,100,100,
		12,5,20,2,48,57,0,176,58,0,109,55,0,12,
		1,112,0,73,36,153,0,176,14,0,92,8,92,25,
		20,2,176,15,0,106,7,65,116,101,32,58,32,0,
		20,1,176,51,0,176,52,0,12,0,176,53,0,12,
		0,122,72,20,2,176,54,0,109,55,0,176,56,0,
		100,106,10,101,68,65,84,65,95,70,73,77,0,100,
		100,100,12,5,20,2,48,57,0,176,58,0,109,55,
		0,12,1,112,0,73,36,154,0,176,59,0,109,55,
		0,100,100,100,100,100,100,20,7,4,0,0,83,55,
		0,109,55,0,73,36,155,0,109,49,0,134,0,0,
		0,0,5,31,13,109,50,0,134,0,0,0,0,5,
		28,51,36,156,0,176,34,0,106,3,48,49,0,20,
		1,36,157,0,176,60,0,106,18,68,97,116,97,115,
		32,73,110,118,160,108,105,100,97,115,32,33,0,92,
		3,20,2,36,158,0,26,235,254,36,160,0,109,49,
		0,109,50,0,15,28,59,36,161,0,176,34,0,106,
		3,48,49,0,20,1,36,162,0,176,60,0,106,26,
		68,97,116,97,32,73,110,105,99,105,111,32,62,32,
		81,117,101,32,70,105,110,97,108,32,33,0,92,3,
		20,2,36,163,0,26,166,254,36,169,0,121,83,61,
		0,36,170,0,121,83,62,0,36,171,0,176,34,0,
		106,3,48,49,0,20,1,36,172,0,176,35,0,106,
		43,68,101,115,101,106,97,32,68,101,102,105,110,105,
		114,32,117,109,32,73,110,116,101,114,118,97,108,111,
		32,100,101,32,80,97,114,99,101,108,97,115,32,63,
		32,0,20,1,36,173,0,109,37,0,106,2,83,0,
		5,29,20,1,36,174,0,36,175,0,176,34,0,106,
		3,48,49,0,20,1,36,176,0,176,14,0,92,23,
		121,20,2,176,15,0,106,21,80,97,114,99,101,108,
		97,32,100,101,32,73,110,105,99,105,111,32,58,32,
		0,20,1,176,51,0,176,52,0,12,0,176,53,0,
		12,0,122,72,20,2,176,54,0,109,55,0,176,56,
		0,100,106,11,101,73,78,84,69,82,95,73,78,73,
		0,106,7,64,114,32,57,57,57,0,100,100,12,5,
		20,2,48,57,0,176,58,0,109,55,0,12,1,112,
		0,73,36,177,0,176,14,0,92,23,92,35,20,2,
		176,15,0,106,18,80,97,114,99,101,108,97,32,100,
		101,32,70,105,109,32,58,32,0,20,1,176,51,0,
		176,52,0,12,0,176,53,0,12,0,122,72,20,2,
		176,54,0,109,55,0,176,56,0,100,106,11,101,73,
		78,84,69,82,95,70,73,77,0,106,7,64,114,32,
		57,57,57,0,100,100,12,5,20,2,48,57,0,176,
		58,0,109,55,0,12,1,112,0,73,36,178,0,176,
		59,0,109,55,0,100,100,100,100,100,100,20,7,4,
		0,0,83,55,0,109,55,0,73,36,179,0,109,62,
		0,109,61,0,35,32,245,254,36,184,0,36,186,0,
		109,28,0,176,27,0,92,5,12,1,69,29,181,2,
		36,187,0,176,6,0,106,2,56,0,20,1,36,188,
		0,176,31,0,109,12,0,92,5,12,2,83,63,0,
		36,189,0,109,28,0,83,64,0,36,190,0,106,4,
		48,48,48,0,83,65,0,36,191,0,106,3,67,80,
		0,176,66,0,109,65,0,122,92,3,176,67,0,176,
		68,0,109,63,0,12,1,12,1,49,12,3,72,176,
		68,0,109,63,0,12,1,72,176,66,0,109,65,0,
		122,92,3,176,67,0,176,68,0,109,64,0,12,1,
		12,1,49,12,3,72,176,68,0,109,64,0,12,1,
		72,83,69,0,36,192,0,106,3,73,71,0,176,66,
		0,109,65,0,122,92,3,176,67,0,176,68,0,109,
		63,0,12,1,12,1,49,12,3,72,176,68,0,109,
		63,0,12,1,72,176,66,0,109,65,0,122,92,3,
		176,67,0,176,68,0,109,64,0,12,1,12,1,49,
		12,3,72,176,68,0,109,64,0,12,1,72,83,70,
		0,36,193,0,106,3,73,86,0,176,66,0,109,65,
		0,122,92,3,176,67,0,176,68,0,109,63,0,12,
		1,12,1,49,12,3,72,176,68,0,109,63,0,12,
		1,72,176,66,0,109,65,0,122,92,3,176,67,0,
		176,68,0,109,64,0,12,1,12,1,49,12,3,72,
		176,68,0,109,64,0,12,1,72,83,71,0,36,194,
		0,106,3,73,80,0,176,66,0,109,65,0,122,92,
		3,176,67,0,176,68,0,109,63,0,12,1,12,1,
		49,12,3,72,176,68,0,109,63,0,12,1,72,176,
		66,0,109,65,0,122,92,3,176,67,0,176,68,0,
		109,64,0,12,1,12,1,49,12,3,72,176,68,0,
		109,64,0,12,1,72,83,72,0,36,195,0,176,7,
		0,9,100,109,69,0,100,100,9,20,6,176,8,0,
		109,70,0,20,1,176,8,0,109,71,0,20,1,176,
		8,0,109,72,0,20,1,36,196,0,176,29,0,90,
		108,51,67,58,47,115,105,115,47,83,65,78,47,120,
		104,98,47,99,111,110,95,105,110,97,100,46,112,114,
		103,58,67,79,78,95,73,78,65,68,0,109,73,0,
		176,31,0,109,12,0,92,5,12,2,5,21,28,52,
		73,109,74,0,109,28,0,5,21,28,41,73,109,75,
		0,109,49,0,16,21,28,30,73,109,75,0,109,50,
		0,34,21,28,19,73,109,76,0,134,0,0,0,0,
		5,21,28,6,73,109,77,0,6,106,148,67,76,65,
		83,83,69,32,61,32,83,84,82,40,101,67,76,65,
		83,95,80,82,79,44,53,41,32,46,65,78,68,46,
		32,81,85,65,68,82,65,32,61,32,101,81,85,65,
		68,82,65,32,46,65,78,68,46,32,40,68,65,84,
		65,95,86,69,78,67,32,62,61,32,101,68,65,84,
		65,95,73,78,73,32,46,65,78,68,46,32,68,65,
		84,65,95,86,69,78,67,32,60,61,32,101,68,65,
		84,65,95,70,73,77,41,32,46,65,78,68,46,32,
		68,65,84,65,95,80,71,84,79,32,61,32,67,84,
		79,68,40,34,34,41,32,46,65,78,68,46,32,65,
		84,73,86,65,0,20,2,36,197,0,176,9,0,20,
		0,26,249,7,36,199,0,176,34,0,106,3,48,49,
		0,20,1,36,200,0,176,35,0,106,42,68,101,115,
		101,106,97,32,67,114,105,97,114,32,78,111,118,111,
		32,66,97,110,99,111,32,100,101,32,68,97,100,111,
		115,32,40,83,47,78,41,32,63,32,0,20,1,36,
		201,0,109,37,0,106,2,83,0,5,29,148,6,36,
		202,0,176,34,0,106,3,48,49,0,20,1,36,203,
		0,176,60,0,106,10,65,116,101,110,135,198,111,32,
		33,0,92,3,20,2,36,204,0,176,78,0,106,56,
		65,103,117,97,114,100,101,32,97,32,109,111,110,116,
		97,103,101,109,32,100,111,32,66,97,110,99,111,32,
		100,101,32,68,97,100,111,115,32,112,97,114,97,32,
		73,109,112,114,101,115,115,198,111,32,46,46,46,0,
		20,1,36,205,0,109,13,0,122,5,29,18,3,36,
		206,0,176,6,0,106,2,56,0,20,1,36,207,0,
		176,79,0,92,8,106,3,79,78,0,20,2,36,208,
		0,176,7,0,9,100,106,9,84,69,77,80,95,80,
		65,82,0,100,100,9,20,6,36,209,0,176,80,0,
		12,0,28,18,36,210,0,176,81,0,20,0,36,211,
		0,176,82,0,20,0,36,213,0,176,79,0,92,8,
		106,4,79,70,70,0,20,2,36,214,0,176,6,0,
		106,2,51,0,20,1,36,215,0,176,29,0,90,62,
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,105,110,97,100,46,112,114,103,
		58,67,79,78,95,73,78,65,68,0,109,30,0,176,
		31,0,109,12,0,92,5,12,2,5,21,28,6,73,
		109,32,0,6,106,45,78,85,77,95,76,79,84,69,
		65,77,32,61,32,83,84,82,40,101,67,76,65,83,
		95,80,82,79,44,53,41,32,46,65,78,68,46,32,
		67,65,68,65,83,84,82,79,0,20,2,36,216,0,
		176,9,0,20,0,36,217,0,176,83,0,12,0,32,
		41,1,36,218,0,176,31,0,109,12,0,92,5,12,
		2,83,63,0,36,219,0,109,38,0,83,64,0,36,
		220,0,176,6,0,106,2,56,0,20,1,36,221,0,
		176,7,0,9,100,106,9,84,69,77,80,95,80,65,
		82,0,100,100,9,20,6,36,222,0,176,9,0,20,
		0,36,223,0,106,4,48,48,48,0,83,65,0,36,
		224,0,106,3,67,80,0,176,66,0,109,65,0,122,
		92,3,176,67,0,176,68,0,109,63,0,12,1,12,
		1,49,12,3,72,176,68,0,109,63,0,12,1,72,
		176,66,0,109,65,0,122,92,3,176,67,0,176,68,
		0,109,64,0,12,1,12,1,49,12,3,72,176,68,
		0,109,64,0,12,1,72,83,69,0,36,225,0,176,
		84,0,109,69,0,4,0,0,90,79,51,67,58,47,
		115,105,115,47,83,65,78,47,120,104,98,47,99,111,
		110,95,105,110,97,100,46,112,114,103,58,67,79,78,
		95,73,78,65,68,0,109,75,0,109,49,0,16,21,
		28,30,73,109,75,0,109,50,0,34,21,28,19,73,
		109,76,0,134,0,0,0,0,5,21,28,6,73,109,
		77,0,6,100,100,100,9,100,100,100,20,10,36,226,
		0,176,9,0,20,0,36,227,0,176,6,0,106,2,
		51,0,20,1,36,228,0,176,85,0,122,20,1,26,
		210,254,36,230,0,176,79,0,92,8,106,3,79,78,
		0,20,2,36,231,0,176,6,0,106,2,56,0,20,
		1,36,232,0,176,7,0,9,100,106,9,84,69,77,
		80,95,80,65,82,0,100,100,9,20,6,36,233,0,
		176,86,0,106,9,84,69,77,80,95,80,65,82,0,
		106,38,67,76,65,83,83,69,43,81,85,65,68,82,
		65,43,78,85,77,95,73,77,79,86,69,76,43,83,
		84,82,40,80,65,82,84,69,44,49,41,0,90,62,
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,105,110,97,100,46,112,114,103,
		58,67,79,78,95,73,78,65,68,0,109,73,0,109,
		74,0,72,109,87,0,72,176,31,0,109,88,0,122,
		12,2,72,6,100,20,4,36,234,0,176,9,0,20,
		0,36,235,0,176,7,0,9,100,106,9,84,69,77,
		80,95,80,65,82,0,100,100,9,20,6,176,8,0,
		106,9,84,69,77,80,95,80,65,82,0,20,1,36,
		236,0,176,9,0,20,0,36,237,0,176,79,0,92,
		8,106,4,79,70,70,0,20,2,26,40,4,36,239,
		0,176,6,0,106,2,56,0,20,1,36,240,0,176,
		79,0,92,8,106,3,79,78,0,20,2,36,241,0,
		176,7,0,9,100,106,9,84,69,77,80,95,80,65,
		82,0,100,100,9,20,6,36,242,0,176,80,0,12,
		0,28,18,36,243,0,176,81,0,20,0,36,244,0,
		176,82,0,20,0,36,246,0,176,79,0,92,8,106,
		4,79,70,70,0,20,2,36,247,0,176,6,0,106,
		2,52,0,20,1,36,248,0,176,29,0,90,62,51,
		67,58,47,115,105,115,47,83,65,78,47,120,104,98,
		47,99,111,110,95,105,110,97,100,46,112,114,103,58,
		67,79,78,95,73,78,65,68,0,109,30,0,176,31,
		0,109,12,0,92,5,12,2,5,21,28,6,73,109,
		32,0,6,106,45,78,85,77,95,76,79,84,69,65,
		77,32,61,32,83,84,82,40,101,67,76,65,83,95,
		80,82,79,44,53,41,32,46,65,78,68,46,32,67,
		65,68,65,83,84,82,79,0,20,2,36,249,0,176,
		9,0,20,0,36,250,0,176,83,0,12,0,32,41,
		1,36,251,0,176,31,0,109,12,0,92,5,12,2,
		83,63,0,36,252,0,109,44,0,83,64,0,36,253,
		0,176,6,0,106,2,56,0,20,1,36,254,0,176,
		7,0,9,100,106,9,84,69,77,80,95,80,65,82,
		0,100,100,9,20,6,36,255,0,176,9,0,20,0,
		36,0,1,106,4,48,48,48,0,83,65,0,36,1,
		1,106,3,67,80,0,176,66,0,109,65,0,122,92,
		3,176,67,0,176,68,0,109,63,0,12,1,12,1,
		49,12,3,72,176,68,0,109,63,0,12,1,72,176,
		66,0,109,65,0,122,92,3,176,67,0,176,68,0,
		109,64,0,12,1,12,1,49,12,3,72,176,68,0,
		109,64,0,12,1,72,83,69,0,36,2,1,176,84,
		0,109,69,0,4,0,0,90,79,51,67,58,47,115,
		105,115,47,83,65,78,47,120,104,98,47,99,111,110,
		95,105,110,97,100,46,112,114,103,58,67,79,78,95,
		73,78,65,68,0,109,75,0,109,49,0,16,21,28,
		30,73,109,75,0,109,50,0,34,21,28,19,73,109,
		76,0,134,0,0,0,0,5,21,28,6,73,109,77,
		0,6,100,100,100,9,100,100,100,20,10,36,3,1,
		176,9,0,20,0,36,4,1,176,6,0,106,2,52,
		0,20,1,36,5,1,176,85,0,122,20,1,26,210,
		254,36,7,1,176,79,0,92,8,106,3,79,78,0,
		20,2,36,8,1,176,6,0,106,2,56,0,20,1,
		36,9,1,176,7,0,9,100,106,9,84,69,77,80,
		95,80,65,82,0,100,100,9,20,6,36,10,1,176,
		86,0,106,9,84,69,77,80,95,80,65,82,0,106,
		38,67,76,65,83,83,69,43,81,85,65,68,82,65,
		43,78,85,77,95,73,77,79,86,69,76,43,83,84,
		82,40,80,65,82,84,69,44,49,41,0,90,62,51,
		67,58,47,115,105,115,47,83,65,78,47,120,104,98,
		47,99,111,110,95,105,110,97,100,46,112,114,103,58,
		67,79,78,95,73,78,65,68,0,109,73,0,109,74,
		0,72,109,87,0,72,176,31,0,109,88,0,122,12,
		2,72,6,100,20,4,36,11,1,176,9,0,20,0,
		36,12,1,176,7,0,9,100,106,9,84,69,77,80,
		95,80,65,82,0,100,100,9,20,6,176,8,0,106,
		9,84,69,77,80,95,80,65,82,0,20,1,36,13,
		1,176,9,0,20,0,36,14,1,176,79,0,92,8,
		106,4,79,70,70,0,20,2,36,15,1,26,22,1,
		36,18,1,176,6,0,106,2,56,0,20,1,36,19,
		1,176,7,0,9,100,106,9,84,69,77,80,95,80,
		65,82,0,100,100,9,20,6,36,20,1,176,9,0,
		20,0,36,21,1,176,86,0,106,9,84,69,77,80,
		95,80,65,82,0,106,38,67,76,65,83,83,69,43,
		81,85,65,68,82,65,43,78,85,77,95,73,77,79,
		86,69,76,43,83,84,82,40,80,65,82,84,69,44,
		49,41,0,90,62,51,67,58,47,115,105,115,47,83,
		65,78,47,120,104,98,47,99,111,110,95,105,110,97,
		100,46,112,114,103,58,67,79,78,95,73,78,65,68,
		0,109,73,0,109,74,0,72,109,87,0,72,176,31,
		0,109,88,0,122,12,2,72,6,100,20,4,36,22,
		1,176,9,0,20,0,36,23,1,176,7,0,9,100,
		106,9,84,69,77,80,95,80,65,82,0,100,100,9,
		20,6,176,8,0,106,9,84,69,77,80,95,80,65,
		82,0,20,1,36,24,1,176,9,0,20,0,36,26,
		1,176,89,0,92,23,121,92,23,20,3,176,51,0,
		92,23,121,20,2,36,27,1,176,14,0,92,23,121,
		20,2,176,15,0,106,14,73,78,68,69,88,65,68,
		79,32,33,33,33,33,0,20,1,36,31,1,109,61,
		0,121,69,29,218,1,109,62,0,121,69,29,210,1,
		36,32,1,176,6,0,106,2,56,0,20,1,36,33,
		1,176,9,0,20,0,36,34,1,121,83,90,0,36,
		35,1,109,73,0,83,91,0,36,36,1,109,74,0,
		83,28,0,36,37,1,109,87,0,83,33,0,36,38,
		1,109,88,0,83,92,0,36,39,1,176,83,0,12,
		0,32,239,0,36,40,1,109,91,0,109,73,0,5,
		28,51,109,28,0,109,74,0,5,28,42,109,33,0,
		109,87,0,5,28,33,109,92,0,109,88,0,5,28,
		24,36,41,1,109,90,0,122,72,83,90,0,36,42,
		1,176,85,0,122,20,1,25,186,36,44,1,109,90,
		0,109,61,0,35,31,11,109,90,0,109,62,0,15,
		28,107,36,45,1,176,93,0,109,91,0,109,28,0,
		72,109,33,0,72,176,31,0,109,92,0,122,12,2,
		72,100,100,20,3,36,46,1,109,91,0,109,73,0,
		5,28,55,109,28,0,109,74,0,5,28,46,109,33,
		0,109,87,0,5,28,37,109,92,0,109,88,0,5,
		28,28,176,83,0,12,0,31,21,36,47,1,176,94,
		0,20,0,36,48,1,176,85,0,122,20,1,25,193,
		36,50,1,121,83,90,0,25,9,36,52,1,121,83,
		90,0,36,54,1,109,73,0,83,91,0,36,55,1,
		109,74,0,83,28,0,36,56,1,109,87,0,83,33,
		0,36,57,1,109,88,0,83,92,0,36,58,1,26,
		12,255,36,60,1,109,90,0,109,61,0,35,31,11,
		109,90,0,109,62,0,15,28,98,36,61,1,176,93,
		0,109,91,0,109,28,0,72,109,33,0,72,176,31,
		0,109,92,0,122,12,2,72,100,100,20,3,36,62,
		1,109,91,0,109,73,0,5,28,55,109,28,0,109,
		74,0,5,28,46,109,33,0,109,87,0,5,28,37,
		109,92,0,109,88,0,5,28,28,176,83,0,12,0,
		31,21,36,63,1,176,94,0,20,0,36,64,1,176,
		85,0,122,20,1,25,193,36,67,1,176,6,0,106,
		2,56,0,20,1,36,68,1,176,9,0,20,0,36,
		69,1,176,95,0,20,0,36,70,1,176,9,0,20,
		0,36,72,1,176,6,0,106,2,56,0,20,1,36,
		73,1,176,9,0,20,0,36,74,1,176,83,0,12,
		0,28,91,36,75,1,176,34,0,106,3,48,49,0,
		20,1,36,76,1,176,60,0,106,10,65,116,101,110,
		135,198,111,32,33,0,92,3,20,2,36,77,1,176,
		78,0,106,33,78,198,111,32,72,160,32,68,101,118,
		101,100,111,114,101,115,32,78,101,115,116,101,32,80,
		101,114,161,111,100,111,32,33,0,20,1,36,78,1,
		176,96,0,20,0,26,35,4,36,80,1,109,76,0,
		83,97,0,36,81,1,92,12,3,1,0,92,12,3,
		1,0,176,20,0,108,98,108,99,20,2,81,99,0,
		81,98,0,36,82,1,176,100,0,109,98,0,121,20,
		2,36,83,1,176,100,0,109,99,0,121,20,2,36,
		84,1,121,83,101,0,36,85,1,121,83,102,0,36,
		86,1,121,83,103,0,36,87,1,121,83,104,0,36,
		88,1,176,83,0,12,0,32,130,3,36,89,1,109,
		74,0,83,28,0,36,90,1,109,87,0,83,33,0,
		36,91,1,109,88,0,83,92,0,36,92,1,176,6,
		0,106,2,56,0,20,1,36,93,1,109,102,0,109,
		105,0,72,83,102,0,36,94,1,109,103,0,122,72,
		83,103,0,36,95,1,176,34,0,106,3,48,49,0,
		20,1,36,96,1,176,14,0,92,23,121,20,2,176,
		15,0,176,68,0,176,31,0,109,12,0,92,5,12,
		2,12,1,106,2,47,0,72,109,74,0,72,106,2,
		47,0,72,109,87,0,72,20,1,36,97,1,176,85,
		0,122,20,1,36,98,1,176,31,0,109,12,0,92,
		5,12,2,109,73,0,69,31,30,109,28,0,109,74,
		0,69,31,21,109,33,0,109,87,0,69,31,12,109,
		88,0,109,92,0,69,29,146,0,36,99,1,109,101,
		0,109,102,0,72,83,101,0,36,100,1,98,98,0,
		109,103,0,122,16,28,15,109,103,0,92,10,34,28,
		7,109,103,0,25,4,92,11,1,122,72,98,98,0,
		109,103,0,122,16,28,15,109,103,0,92,10,34,28,
		7,109,103,0,25,4,92,11,2,36,101,1,98,99,
		0,109,103,0,122,16,28,15,109,103,0,92,10,34,
		28,7,109,103,0,25,4,92,11,1,109,102,0,72,
		98,99,0,109,103,0,122,16,28,15,109,103,0,92,
		10,34,28,7,109,103,0,25,4,92,11,2,36,102,
		1,121,83,102,0,36,103,1,121,83,103,0,36,105,
		1,176,83,0,12,0,29,169,254,36,106,1,121,83,
		106,0,36,107,1,92,10,83,107,0,36,108,1,122,
		83,108,0,36,109,1,109,108,0,92,11,34,29,89,
		1,36,110,1,109,108,0,92,10,34,29,155,0,36,
		111,1,176,14,0,109,107,0,92,10,20,2,176,15,
		0,106,17,68,69,86,69,68,79,82,69,83,32,67,
		79,77,32,32,32,0,176,31,0,109,108,0,92,2,
		12,2,72,106,13,32,80,65,82,67,69,76,65,83,
		32,58,32,0,72,20,1,36,112,1,176,14,0,109,
		107,0,92,40,20,2,176,109,0,98,98,0,109,108,
		0,1,106,9,64,82,32,57,44,57,57,57,0,20,
		2,36,113,1,176,14,0,109,107,0,92,50,20,2,
		176,109,0,98,99,0,109,108,0,1,106,22,64,82,
		32,57,57,57,44,57,57,57,44,57,57,57,44,57,
		57,57,46,57,57,0,20,2,26,139,0,36,115,1,
		176,14,0,109,107,0,92,10,20,2,176,15,0,106,
		31,68,69,86,69,68,79,82,69,83,32,67,79,77,
		32,43,32,49,48,32,80,65,82,67,69,76,65,83,
		32,58,32,0,20,1,36,116,1,176,14,0,109,107,
		0,92,40,20,2,176,109,0,98,98,0,109,108,0,
		1,106,9,64,82,32,57,44,57,57,57,0,20,2,
		36,117,1,176,14,0,109,107,0,92,50,20,2,176,
		109,0,98,99,0,109,108,0,1,106,22,64,82,32,
		57,57,57,44,57,57,57,44,57,57,57,44,57,57,
		57,46,57,57,0,20,2,36,119,1,109,106,0,98,
		98,0,109,108,0,1,72,83,106,0,36,120,1,109,
		107,0,122,72,83,107,0,36,121,1,109,108,0,122,
		72,83,108,0,26,161,254,36,123,1,176,14,0,92,
		21,92,10,20,2,176,15,0,106,31,84,79,84,65,
		76,32,71,69,82,65,76,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,58,32,0,20,
		1,36,124,1,176,14,0,92,21,92,40,20,2,176,
		109,0,109,106,0,106,9,64,82,32,57,44,57,57,
		57,0,20,2,36,125,1,176,51,0,92,21,92,50,
		20,2,176,54,0,109,55,0,176,56,0,100,106,10,
		101,84,79,84,95,80,65,82,67,0,106,22,64,82,
		32,57,57,57,44,57,57,57,44,57,57,57,44,57,
		57,57,46,57,57,0,100,100,12,5,20,2,48,57,
		0,176,58,0,109,55,0,12,1,112,0,73,36,126,
		1,176,110,0,120,20,1,4,0,0,83,55,0,109,
		55,0,73,36,127,1,26,121,252,36,129,1,176,78,
		0,106,43,80,114,101,115,115,105,111,110,101,32,81,
		117,97,108,113,117,101,114,32,84,101,99,108,97,32,
		80,97,114,97,32,84,101,114,109,105,110,97,114,32,
		46,46,46,0,20,1,36,130,1,176,96,0,20,0,
		36,132,1,176,111,0,20,0,36,133,1,176,112,0,
		121,121,176,2,0,12,0,176,3,0,12,0,109,4,
		0,20,5,36,134,1,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,99,111,110,95,105,110,97,100,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,49,192,255,255,247,254,255,
		159,250,255,255,255,253,255,255,247,255,255,233,127,15,
		254,15,253,191,255,239,255,223,191,223,255,191,255,252,
		141,255,247,213,247,121,127,255,255,255,254,187,251,118,
		0,4,3,0,4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

