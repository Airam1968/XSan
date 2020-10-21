/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_devp.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_DEVP );
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
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PDESENV );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_DEVP )
{ "REL_DEVP", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_DEVP )}, NULL },
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
{ "SISGRUPO1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDESC_QUA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "SISGRUPO2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PDESENV", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDESENV )}, NULL },
{ "ET_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VLR_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TIPO_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "ETOT_TIT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "SISLETRAC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "ETIPO_PRD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "NUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VLR_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_DEVP, "C:/sis/SAN/xhb/rel_devp.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_DEVP
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_DEVP )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_DEVP )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,14,0,176,5,
		0,106,30,82,69,76,65,84,79,82,73,79,32,68,
		69,32,80,65,82,67,69,76,65,83,32,68,69,86,
		73,68,65,83,0,20,1,36,15,0,176,6,0,106,
		2,49,0,20,1,36,16,0,176,7,0,9,100,106,
		9,85,78,73,95,73,77,79,66,0,100,100,9,20,
		6,176,8,0,106,9,85,78,73,95,73,77,79,66,
		0,20,1,36,17,0,176,9,0,20,0,36,18,0,
		176,6,0,106,2,50,0,20,1,36,19,0,176,7,
		0,9,100,106,9,67,76,65,83,95,80,82,79,0,
		100,100,9,20,6,176,8,0,106,9,67,76,65,83,
		95,80,82,79,0,20,1,36,20,0,176,9,0,20,
		0,36,21,0,176,6,0,106,2,51,0,20,1,36,
		22,0,176,7,0,9,100,106,9,67,65,68,95,81,
		85,65,68,0,100,100,9,20,6,176,8,0,106,9,
		67,65,68,95,81,85,65,68,0,20,1,36,23,0,
		176,9,0,20,0,36,24,0,176,6,0,106,2,52,
		0,20,1,36,25,0,176,7,0,9,100,106,9,67,
		65,68,95,65,80,65,82,0,100,100,9,20,6,176,
		8,0,106,9,67,65,68,95,65,80,65,82,0,20,
		1,36,26,0,176,9,0,20,0,36,27,0,176,6,
		0,106,2,53,0,20,1,36,28,0,176,7,0,9,
		100,106,8,67,79,78,84,65,84,79,0,100,100,9,
		20,6,176,8,0,106,9,78,85,77,95,67,76,73,
		69,0,20,1,176,8,0,106,8,67,79,78,84,65,
		84,79,0,20,1,36,29,0,176,9,0,20,0,36,
		30,0,176,6,0,106,2,54,0,20,1,36,31,0,
		176,7,0,9,100,106,9,67,65,68,95,86,69,78,
		68,0,100,100,9,20,6,176,8,0,106,9,67,65,
		68,95,86,69,78,68,0,20,1,36,32,0,176,9,
		0,20,0,36,33,0,176,6,0,106,2,56,0,20,
		1,36,34,0,176,7,0,9,100,106,9,67,65,68,
		95,80,65,82,67,0,100,100,9,20,6,176,8,0,
		106,9,67,65,68,95,80,65,82,67,0,20,1,176,
		8,0,106,9,68,65,84,95,80,65,82,67,0,20,
		1,36,35,0,176,9,0,20,0,36,36,0,176,6,
		0,106,2,57,0,20,1,36,37,0,176,7,0,9,
		100,106,9,84,73,80,79,95,80,82,79,0,100,100,
		9,20,6,176,8,0,106,9,84,73,80,79,95,80,
		82,79,0,20,1,36,38,0,176,9,0,20,0,36,
		39,0,176,6,0,106,2,50,0,20,1,36,42,0,
		176,10,0,20,0,36,43,0,176,11,0,106,9,87,
		47,78,44,43,87,47,82,0,20,1,36,44,0,121,
		83,12,0,36,45,0,121,83,13,0,36,46,0,176,
		14,0,92,5,92,2,20,2,176,15,0,106,11,80,
		114,111,100,117,116,111,32,58,32,0,20,1,36,47,
		0,176,9,0,20,0,36,48,0,92,3,3,1,0,
		92,3,3,1,0,92,3,3,1,0,92,3,3,1,
		0,176,20,0,108,16,108,17,108,18,108,19,20,4,
		81,19,0,81,18,0,81,17,0,81,16,0,36,49,
		0,176,21,0,109,16,0,120,109,19,0,20,3,36,
		50,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,122,2,36,51,0,106,8,64,82,57,57,57,57,
		57,0,98,17,0,92,2,2,36,52,0,106,5,64,
		83,52,48,0,98,17,0,92,3,2,36,53,0,106,
		5,32,32,32,32,0,98,18,0,122,2,36,54,0,
		106,5,32,32,32,32,0,98,18,0,92,2,2,36,
		55,0,106,5,32,32,32,32,0,98,18,0,92,3,
		2,36,56,0,176,22,0,92,5,92,15,92,5,92,
		70,109,16,0,106,10,70,85,78,67,95,80,82,79,
		86,0,109,17,0,109,18,0,106,2,32,0,106,4,
		32,45,32,0,106,2,32,0,20,11,36,57,0,109,
		23,0,83,12,0,36,58,0,109,24,0,83,13,0,
		36,59,0,109,25,0,83,26,0,36,63,0,176,27,
		0,122,12,1,83,28,0,36,65,0,109,13,0,122,
		5,29,36,3,36,67,0,176,6,0,106,2,51,0,
		20,1,36,68,0,176,29,0,90,24,109,30,0,176,
		31,0,109,12,0,92,5,12,2,5,21,28,6,73,
		109,32,0,6,106,45,78,85,77,95,76,79,84,69,
		65,77,32,61,32,83,84,82,40,101,67,76,65,83,
		95,80,82,79,44,53,41,32,46,65,78,68,46,32,
		67,65,68,65,83,84,82,79,0,20,2,36,69,0,
		176,9,0,20,0,36,70,0,121,83,28,0,36,71,
		0,176,14,0,92,6,92,2,20,2,176,15,0,109,
		33,0,106,4,32,58,32,0,72,20,1,36,72,0,
		176,9,0,20,0,36,73,0,92,4,3,1,0,92,
		4,3,1,0,92,4,3,1,0,92,4,3,1,0,
		176,20,0,108,16,108,17,108,18,108,19,20,4,81,
		19,0,81,18,0,81,17,0,81,16,0,36,74,0,
		176,21,0,109,16,0,120,109,19,0,20,3,36,75,
		0,106,8,64,82,57,57,57,57,57,0,98,17,0,
		122,2,36,76,0,106,8,64,82,57,57,57,57,57,
		0,98,17,0,92,2,2,36,77,0,106,8,64,82,
		57,57,57,57,57,0,98,17,0,92,3,2,36,78,
		0,106,8,64,83,57,57,57,57,57,0,98,17,0,
		92,4,2,36,79,0,106,5,32,32,32,32,0,98,
		18,0,122,2,36,80,0,106,5,32,32,32,32,0,
		98,18,0,92,2,2,36,81,0,106,5,32,32,32,
		32,0,98,18,0,92,3,2,36,82,0,106,5,32,
		32,32,32,0,98,18,0,92,4,2,36,83,0,176,
		22,0,92,6,92,14,92,6,92,45,109,16,0,106,
		10,70,85,78,67,95,80,82,79,86,0,109,17,0,
		109,18,0,106,2,32,0,106,4,32,45,32,0,106,
		2,32,0,20,11,36,84,0,109,34,0,83,28,0,
		36,85,0,176,27,0,122,12,1,83,35,0,36,86,
		0,109,36,0,83,37,0,36,87,0,109,38,0,83,
		39,0,36,88,0,134,0,0,0,0,83,40,0,36,
		89,0,134,0,0,0,0,83,41,0,36,91,0,176,
		14,0,92,7,92,2,20,2,176,15,0,106,11,80,
		101,114,105,111,100,111,32,58,32,0,20,1,176,42,
		0,176,43,0,12,0,176,44,0,12,0,122,72,20,
		2,176,45,0,109,46,0,176,47,0,100,106,10,101,
		68,65,84,65,95,73,78,73,0,100,100,100,12,5,
		20,2,48,48,0,176,49,0,109,46,0,12,1,112,
		0,73,36,92,0,176,14,0,92,7,92,25,20,2,
		176,15,0,106,7,65,116,101,32,58,32,0,20,1,
		176,42,0,176,43,0,12,0,176,44,0,12,0,122,
		72,20,2,176,45,0,109,46,0,176,47,0,100,106,
		10,101,68,65,84,65,95,70,73,77,0,100,100,100,
		12,5,20,2,48,48,0,176,49,0,109,46,0,12,
		1,112,0,73,36,93,0,176,50,0,109,46,0,100,
		100,100,100,100,100,20,7,4,0,0,83,46,0,109,
		46,0,73,36,94,0,109,40,0,134,0,0,0,0,
		5,31,13,109,41,0,134,0,0,0,0,5,28,51,
		36,95,0,176,51,0,106,3,48,49,0,20,1,36,
		96,0,176,52,0,106,18,68,97,116,97,115,32,73,
		110,118,97,108,105,100,97,115,32,33,0,92,3,20,
		2,36,97,0,26,238,254,36,99,0,109,40,0,109,
		41,0,15,29,97,3,36,100,0,176,51,0,106,3,
		48,49,0,20,1,36,101,0,176,52,0,106,26,68,
		97,116,97,32,73,110,105,99,105,111,32,62,32,81,
		117,101,32,70,105,110,97,108,32,33,0,92,3,20,
		2,36,102,0,26,168,254,36,108,0,109,13,0,92,
		2,5,29,20,3,36,110,0,176,6,0,106,2,52,
		0,20,1,36,111,0,176,29,0,90,24,109,30,0,
		176,31,0,109,12,0,92,5,12,2,5,21,28,6,
		73,109,32,0,6,106,45,78,85,77,95,76,79,84,
		69,65,77,32,61,32,83,84,82,40,101,67,76,65,
		83,95,80,82,79,44,53,41,32,46,65,78,68,46,
		32,67,65,68,65,83,84,82,79,0,20,2,36,112,
		0,176,9,0,20,0,36,113,0,121,83,28,0,36,
		114,0,176,14,0,92,6,92,2,20,2,176,15,0,
		109,53,0,106,4,32,58,32,0,72,20,1,36,115,
		0,176,9,0,20,0,36,116,0,92,4,3,1,0,
		92,4,3,1,0,92,4,3,1,0,92,4,3,1,
		0,176,20,0,108,16,108,17,108,18,108,19,20,4,
		81,19,0,81,18,0,81,17,0,81,16,0,36,117,
		0,176,21,0,109,16,0,120,109,19,0,20,3,36,
		118,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,122,2,36,119,0,106,8,64,82,57,57,57,57,
		57,0,98,17,0,92,2,2,36,120,0,106,5,64,
		83,51,48,0,98,17,0,92,3,2,36,121,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,92,4,
		2,36,122,0,106,5,32,32,32,32,0,98,18,0,
		122,2,36,123,0,106,5,32,32,32,32,0,98,18,
		0,92,2,2,36,124,0,106,5,32,32,32,32,0,
		98,18,0,92,3,2,36,125,0,106,5,32,32,32,
		32,0,98,18,0,92,4,2,36,126,0,176,22,0,
		92,6,92,8,92,6,92,76,109,16,0,106,10,70,
		85,78,67,95,80,82,79,86,0,109,17,0,109,18,
		0,106,2,32,0,106,4,32,45,32,0,106,2,32,
		0,20,11,36,127,0,109,54,0,83,28,0,36,128,
		0,109,25,0,83,35,0,36,129,0,109,55,0,83,
		56,0,36,130,0,134,0,0,0,0,83,40,0,36,
		131,0,134,0,0,0,0,83,41,0,36,133,0,176,
		14,0,92,7,92,2,20,2,176,15,0,106,11,80,
		101,114,105,111,100,111,32,58,32,0,20,1,176,42,
		0,176,43,0,12,0,176,44,0,12,0,122,72,20,
		2,176,45,0,109,46,0,176,47,0,100,106,10,101,
		68,65,84,65,95,73,78,73,0,100,100,100,12,5,
		20,2,48,48,0,176,49,0,109,46,0,12,1,112,
		0,73,36,134,0,176,14,0,92,7,92,25,20,2,
		176,15,0,106,7,65,116,101,32,58,32,0,20,1,
		176,42,0,176,43,0,12,0,176,44,0,12,0,122,
		72,20,2,176,45,0,109,46,0,176,47,0,100,106,
		10,101,68,65,84,65,95,70,73,77,0,100,100,100,
		12,5,20,2,48,48,0,176,49,0,109,46,0,12,
		1,112,0,73,36,135,0,176,50,0,109,46,0,100,
		100,100,100,100,100,20,7,4,0,0,83,46,0,109,
		46,0,73,36,136,0,109,40,0,134,0,0,0,0,
		5,31,13,109,41,0,134,0,0,0,0,5,28,51,
		36,137,0,176,51,0,106,3,48,49,0,20,1,36,
		138,0,176,52,0,106,18,68,97,116,97,115,32,73,
		110,118,97,108,105,100,97,115,32,33,0,92,3,20,
		2,36,139,0,26,238,254,36,141,0,109,40,0,109,
		41,0,15,28,67,36,142,0,176,51,0,106,3,48,
		49,0,20,1,36,143,0,176,52,0,106,26,68,97,
		116,97,32,73,110,105,99,105,111,32,62,32,81,117,
		101,32,70,105,110,97,108,32,33,0,92,3,20,2,
		36,144,0,26,169,254,36,152,0,176,57,0,20,0,
		36,154,0,176,27,0,122,12,1,83,58,0,36,155,
		0,176,51,0,106,3,48,49,0,20,1,36,156,0,
		176,14,0,92,23,121,20,2,176,15,0,106,56,68,
		101,115,101,106,97,32,80,97,114,99,101,108,97,115,
		32,78,111,114,109,97,105,115,47,73,110,116,101,114,
		109,101,100,105,160,114,105,97,115,47,65,109,98,97,
		115,32,40,78,47,73,47,65,41,32,63,32,0,20,
		1,176,42,0,176,43,0,12,0,176,44,0,12,0,
		122,72,20,2,176,45,0,109,46,0,176,47,0,100,
		106,8,101,84,95,80,65,82,67,0,106,2,88,0,
		90,13,109,58,0,106,4,78,73,65,0,24,6,100,
		12,5,20,2,48,48,0,176,49,0,109,46,0,12,
		1,112,0,73,36,157,0,176,50,0,109,46,0,100,
		100,100,100,100,100,20,7,4,0,0,83,46,0,109,
		46,0,73,36,158,0,176,6,0,106,2,56,0,20,
		1,36,159,0,176,7,0,9,100,106,9,67,65,68,
		95,80,65,82,67,0,100,100,9,20,6,176,8,0,
		106,9,68,65,84,95,80,65,82,67,0,20,1,176,
		8,0,106,9,67,65,68,95,80,65,82,67,0,20,
		1,36,160,0,176,9,0,20,0,36,161,0,109,58,
		0,106,2,65,0,5,29,211,0,36,162,0,176,29,
		0,90,59,109,59,0,176,31,0,109,12,0,92,5,
		12,2,5,21,28,41,73,109,60,0,109,28,0,5,
		21,28,30,73,109,61,0,109,40,0,16,21,28,19,
		73,109,61,0,109,41,0,34,21,28,8,73,109,62,
		0,121,5,6,106,128,67,76,65,83,83,69,32,61,
		32,83,84,82,40,101,67,76,65,83,95,80,82,79,
		44,53,41,32,46,65,78,68,46,32,81,85,65,68,
		82,65,32,61,32,101,81,85,65,68,82,65,32,46,
		65,78,68,46,32,40,68,65,84,65,95,86,69,78,
		67,32,62,61,32,101,68,65,84,65,95,73,78,73,
		32,46,65,78,68,46,32,68,65,84,65,95,86,69,
		78,67,32,60,61,32,101,68,65,84,65,95,70,73,
		77,41,32,46,65,78,68,46,32,86,76,82,95,80,
		71,84,79,32,61,32,48,0,20,2,36,163,0,176,
		9,0,20,0,26,24,2,36,165,0,109,58,0,106,
		2,73,0,5,29,245,0,36,166,0,176,29,0,90,
		71,109,59,0,176,31,0,109,12,0,92,5,12,2,
		5,21,28,53,73,109,60,0,109,28,0,5,21,28,
		42,73,109,61,0,109,40,0,16,21,28,31,73,109,
		61,0,109,41,0,34,21,28,20,73,109,63,0,106,
		2,73,0,5,21,28,8,73,109,62,0,121,5,6,
		106,150,67,76,65,83,83,69,32,61,32,83,84,82,
		40,101,67,76,65,83,95,80,82,79,44,53,41,32,
		46,65,78,68,46,32,81,85,65,68,82,65,32,61,
		32,101,81,85,65,68,82,65,32,46,65,78,68,46,
		32,40,68,65,84,65,95,86,69,78,67,32,62,61,
		32,101,68,65,84,65,95,73,78,73,32,46,65,78,
		68,46,32,68,65,84,65,95,86,69,78,67,32,60,
		61,32,101,68,65,84,65,95,70,73,77,41,32,46,
		65,78,68,46,32,84,73,80,79,95,80,65,82,67,
		32,61,32,34,73,34,32,46,65,78,68,46,32,86,
		76,82,95,80,71,84,79,32,61,32,48,0,20,2,
		36,167,0,176,9,0,20,0,26,21,1,36,169,0,
		176,29,0,90,83,109,59,0,176,31,0,109,12,0,
		92,5,12,2,5,21,28,65,73,109,60,0,109,28,
		0,5,21,28,54,73,109,61,0,109,40,0,16,21,
		28,43,73,109,61,0,109,41,0,34,21,28,32,73,
		109,63,0,106,2,78,0,5,21,31,14,73,109,63,
		0,106,2,82,0,5,21,28,8,73,109,62,0,121,
		5,6,106,173,67,76,65,83,83,69,32,61,32,83,
		84,82,40,101,67,76,65,83,95,80,82,79,44,53,
		41,32,46,65,78,68,46,32,81,85,65,68,82,65,
		32,61,32,101,81,85,65,68,82,65,32,46,65,78,
		68,46,32,40,68,65,84,65,95,86,69,78,67,32,
		62,61,32,101,68,65,84,65,95,73,78,73,32,46,
		65,78,68,46,32,68,65,84,65,95,86,69,78,67,
		32,60,61,32,101,68,65,84,65,95,70,73,77,41,
		32,46,65,78,68,46,32,40,84,73,80,79,95,80,
		65,82,67,32,61,32,34,78,34,32,46,79,82,46,
		32,84,73,80,79,95,80,65,82,67,32,61,32,34,
		82,34,41,32,46,65,78,68,46,32,86,76,82,95,
		80,71,84,79,32,61,32,48,0,20,2,36,170,0,
		176,9,0,20,0,36,173,0,176,9,0,20,0,36,
		174,0,176,64,0,12,0,28,98,36,175,0,176,51,
		0,106,3,48,49,0,20,1,36,176,0,176,52,0,
		106,10,65,116,101,110,99,97,111,32,33,0,92,3,
		20,2,36,177,0,176,65,0,106,40,78,97,111,32,
		72,97,32,80,97,114,99,101,108,97,115,32,68,101,
		118,105,100,97,115,32,78,101,115,116,101,32,80,101,
		114,105,111,100,111,32,33,0,20,1,36,178,0,176,
		66,0,20,0,26,121,5,36,180,0,176,65,0,106,
		50,80,111,115,105,99,105,111,110,101,32,111,32,70,
		111,114,109,117,108,97,114,105,111,32,56,48,32,67,
		111,108,117,110,97,115,32,101,32,84,101,99,108,101,
		32,91,69,78,84,69,82,93,0,20,1,36,181,0,
		121,83,67,0,36,182,0,121,83,68,0,36,183,0,
		176,66,0,20,0,36,184,0,176,69,0,20,0,36,
		185,0,176,70,0,92,20,106,8,80,82,73,78,84,
		69,82,0,20,2,36,186,0,176,64,0,12,0,32,
		237,4,36,187,0,176,14,0,122,121,20,2,176,15,
		0,109,71,0,106,11,69,109,112,114,101,115,97,32,
		58,32,0,72,109,72,0,72,20,1,36,188,0,176,
		14,0,122,92,115,20,2,176,15,0,106,8,68,97,
		116,97,32,58,32,0,176,73,0,176,74,0,12,0,
		12,1,72,20,1,36,189,0,176,14,0,92,2,121,
		20,2,176,15,0,106,11,83,105,115,116,101,109,97,
		32,58,32,0,109,75,0,72,20,1,36,190,0,176,
		14,0,92,2,92,115,20,2,176,15,0,106,14,82,
		101,108,97,116,58,32,83,65,78,48,48,51,0,20,
		1,36,191,0,176,6,0,106,2,57,0,20,1,36,
		192,0,176,76,0,109,13,0,100,100,20,3,36,193,
		0,109,25,0,83,77,0,36,194,0,176,6,0,106,
		2,56,0,20,1,36,195,0,176,14,0,92,3,92,
		40,20,2,176,15,0,109,78,0,106,30,82,69,76,
		65,84,79,82,73,79,32,68,69,32,80,65,82,67,
		69,76,65,83,32,68,69,86,73,68,65,83,0,72,
		109,71,0,72,20,1,36,196,0,176,14,0,92,4,
		121,20,2,176,15,0,106,18,32,32,32,80,114,111,
		100,117,116,111,32,32,32,32,32,58,32,0,176,31,
		0,109,12,0,92,5,12,2,72,106,4,32,45,32,
		0,72,176,79,0,109,26,0,12,1,72,106,19,32,
		45,32,81,117,97,100,114,97,92,66,108,111,99,111,
		32,58,32,0,72,176,79,0,109,28,0,12,1,72,
		106,4,32,45,32,0,72,109,35,0,72,20,1,36,
		197,0,176,14,0,92,5,121,20,2,176,15,0,176,
		80,0,106,2,61,0,93,135,0,12,2,20,1,36,
		198,0,176,14,0,92,6,92,2,20,2,176,15,0,
		106,11,78,111,46,32,73,77,79,86,69,76,0,20,
		1,36,199,0,176,14,0,92,6,92,15,20,2,176,
		15,0,106,7,83,84,65,84,85,83,0,20,1,36,
		200,0,176,14,0,92,6,92,25,20,2,176,15,0,
		106,8,80,65,82,67,69,76,65,0,20,1,36,201,
		0,176,14,0,92,6,92,35,20,2,176,15,0,106,
		5,84,73,80,79,0,20,1,36,202,0,176,14,0,
		92,6,92,45,20,2,176,15,0,106,11,68,65,84,
		65,32,86,69,78,67,46,0,20,1,36,203,0,176,
		14,0,92,6,92,57,20,2,176,15,0,106,12,86,
		76,82,46,80,65,82,67,69,76,65,0,20,1,36,
		204,0,176,14,0,92,6,92,80,20,2,176,15,0,
		106,10,67,79,68,95,66,65,78,67,79,0,20,1,
		36,205,0,176,14,0,92,7,121,20,2,176,15,0,
		176,80,0,106,2,61,0,93,135,0,12,2,20,1,
		36,206,0,92,8,83,81,0,36,207,0,176,64,0,
		12,0,32,152,1,109,81,0,92,54,34,29,143,1,
		36,208,0,176,14,0,109,81,0,92,2,20,2,176,
		82,0,109,83,0,106,9,64,82,32,57,57,57,57,
		57,0,20,2,36,209,0,109,84,0,121,5,28,30,
		36,210,0,176,14,0,109,81,0,92,15,20,2,176,
		15,0,106,6,85,110,105,99,111,0,20,1,25,75,
		36,212,0,109,84,0,122,5,28,33,36,213,0,176,
		14,0,109,81,0,92,15,20,2,176,15,0,106,9,
		49,111,46,80,114,111,112,46,0,20,1,25,31,36,
		215,0,176,14,0,109,81,0,92,15,20,2,176,15,
		0,106,9,50,111,46,80,114,111,112,46,0,20,1,
		36,218,0,176,14,0,109,81,0,92,25,20,2,176,
		82,0,109,85,0,106,6,57,57,57,57,57,0,20,
		2,36,219,0,176,14,0,109,81,0,92,35,20,2,
		176,82,0,109,63,0,106,5,64,33,32,88,0,20,
		2,36,220,0,176,14,0,109,81,0,92,45,20,2,
		176,15,0,109,61,0,20,1,36,221,0,176,14,0,
		109,81,0,92,57,20,2,176,82,0,109,86,0,106,
		22,64,82,32,57,57,57,44,57,57,57,44,57,57,
		57,44,57,57,57,46,57,57,0,20,2,36,222,0,
		176,14,0,109,81,0,92,80,20,2,176,15,0,176,
		79,0,109,87,0,12,1,20,1,36,223,0,109,81,
		0,122,72,83,81,0,36,224,0,109,67,0,122,72,
		83,67,0,36,225,0,109,68,0,109,86,0,72,83,
		68,0,36,226,0,176,88,0,122,20,1,36,227,0,
		176,89,0,20,0,36,228,0,109,90,0,29,145,254,
		36,229,0,176,70,0,92,20,106,7,83,67,82,69,
		69,78,0,20,2,36,230,0,176,91,0,20,0,176,
		6,0,106,2,49,0,20,1,176,92,0,100,20,1,
		36,231,0,7,36,234,0,176,64,0,12,0,29,152,
		0,36,235,0,176,14,0,109,81,0,121,20,2,176,
		15,0,176,80,0,106,2,61,0,93,135,0,12,2,
		20,1,36,236,0,109,81,0,122,72,83,81,0,36,
		237,0,176,14,0,109,81,0,92,2,20,2,176,15,
		0,106,10,84,111,116,97,105,115,32,58,32,0,20,
		1,36,238,0,176,14,0,109,81,0,92,32,20,2,
		176,82,0,109,67,0,106,11,64,82,32,57,57,57,
		44,57,57,57,0,20,2,36,239,0,176,14,0,109,
		81,0,92,57,20,2,176,82,0,109,68,0,106,22,
		64,82,32,57,57,57,44,57,57,57,44,57,57,57,
		44,57,57,57,46,57,57,0,20,2,36,241,0,176,
		14,0,92,56,121,20,2,176,15,0,176,80,0,106,
		2,61,0,93,135,0,12,2,20,1,36,242,0,176,
		14,0,92,57,92,95,20,2,176,15,0,109,93,0,
		20,1,36,243,0,176,14,0,122,121,20,2,176,15,
		0,106,2,32,0,109,94,0,72,20,1,26,14,251,
		36,245,0,176,70,0,92,20,106,7,83,67,82,69,
		69,78,0,20,2,36,247,0,176,91,0,20,0,36,
		248,0,176,95,0,121,121,176,2,0,12,0,176,3,
		0,12,0,109,4,0,20,5,36,249,0,7
	};

	hb_vmExecute( pcode, symbols );
}

