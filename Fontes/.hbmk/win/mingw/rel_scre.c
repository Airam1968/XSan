/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_scre.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_SCRE );
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
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( F_BLOQA );
HB_FUNC_EXTERN( __DBZAP );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( __DBAPP );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBCREATEINDEX );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBCLEARFILTER );
HB_FUNC_EXTERN( __DBPACK );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_SCRE )
{ "REL_SCRE", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_SCRE )}, NULL },
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
{ "EDATA_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
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
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESAIDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "ECONVERT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOTAIS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "F_BLOQA", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQA )}, NULL },
{ "__DBZAP", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBZAP )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "NUM_LOTEAM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CADASTRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "EIND_CLAS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND_QUAD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EZERO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "EARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__DBAPP", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBAPP )}, NULL },
{ "DATA_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DBCREATEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCREATEINDEX )}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "VLR_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLEARFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLEARFILTER )}, NULL },
{ "__DBPACK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBPACK )}, NULL },
{ "ESUB_SD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_PA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_SD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_PA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONTRATO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "NUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VLR_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_SCRE, "C:/sis/SAN/xhb/rel_scre.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_SCRE
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_SCRE )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_SCRE )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,26,82,69,76,65,84,224,82,73,79,32,68,
		69,32,83,65,76,68,79,32,67,82,69,68,79,82,
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
		0,106,2,54,0,20,1,36,32,0,176,7,0,9,
		100,106,9,67,65,68,95,86,69,78,68,0,100,100,
		9,20,6,176,8,0,106,9,67,65,68,95,86,69,
		78,68,0,20,1,176,8,0,106,9,68,65,84,65,
		95,86,69,78,0,20,1,36,33,0,176,9,0,20,
		0,36,34,0,176,6,0,106,2,56,0,20,1,36,
		36,0,176,6,0,106,2,57,0,20,1,36,37,0,
		176,7,0,9,100,106,9,77,83,71,95,66,79,76,
		69,0,100,100,9,20,6,176,8,0,106,9,77,83,
		71,95,80,82,79,68,0,20,1,176,8,0,106,9,
		77,83,71,95,66,79,76,69,0,20,1,36,38,0,
		176,9,0,20,0,36,39,0,176,6,0,106,2,50,
		0,20,1,36,42,0,176,10,0,20,0,36,43,0,
		176,11,0,106,9,87,47,78,44,43,87,47,82,0,
		20,1,36,44,0,121,83,12,0,36,45,0,121,83,
		13,0,36,46,0,176,14,0,92,5,92,2,20,2,
		176,15,0,106,11,80,114,111,100,117,116,111,32,58,
		32,0,20,1,36,47,0,176,9,0,20,0,36,48,
		0,92,3,3,1,0,92,3,3,1,0,92,3,3,
		1,0,92,3,3,1,0,176,20,0,108,16,108,17,
		108,18,108,19,20,4,81,19,0,81,18,0,81,17,
		0,81,16,0,36,49,0,176,21,0,109,16,0,120,
		109,19,0,20,3,36,50,0,106,8,64,82,57,57,
		57,57,57,0,98,17,0,122,2,36,51,0,106,8,
		64,82,57,57,57,57,57,0,98,17,0,92,2,2,
		36,52,0,106,5,64,83,52,48,0,98,17,0,92,
		3,2,36,53,0,106,5,32,32,32,32,0,98,18,
		0,122,2,36,54,0,106,5,32,32,32,32,0,98,
		18,0,92,2,2,36,55,0,106,5,32,32,32,32,
		0,98,18,0,92,3,2,36,56,0,176,22,0,92,
		5,92,15,92,5,92,70,109,16,0,106,10,70,85,
		78,67,95,67,79,78,83,0,109,17,0,109,18,0,
		106,2,32,0,106,4,32,45,32,0,106,2,32,0,
		20,11,36,57,0,109,23,0,83,12,0,36,58,0,
		109,24,0,83,13,0,36,59,0,109,25,0,83,26,
		0,36,62,0,134,0,0,0,0,83,27,0,36,63,
		0,134,0,0,0,0,83,28,0,36,64,0,176,29,
		0,12,0,83,27,0,36,65,0,176,30,0,106,11,
		51,49,47,49,50,47,50,57,57,57,0,12,1,83,
		28,0,36,67,0,176,14,0,92,8,92,2,20,2,
		176,15,0,106,11,80,101,114,105,111,100,111,32,58,
		32,0,20,1,176,31,0,176,32,0,12,0,176,33,
		0,12,0,122,72,20,2,176,34,0,109,35,0,176,
		36,0,100,106,10,101,68,65,84,65,95,73,78,73,
		0,100,100,100,12,5,20,2,48,37,0,176,38,0,
		109,35,0,12,1,112,0,73,36,68,0,176,14,0,
		92,8,92,25,20,2,176,15,0,106,7,65,116,101,
		32,58,32,0,20,1,176,31,0,176,32,0,12,0,
		176,33,0,12,0,122,72,20,2,176,34,0,109,35,
		0,176,36,0,100,106,10,101,68,65,84,65,95,70,
		73,77,0,100,100,100,12,5,20,2,48,37,0,176,
		38,0,109,35,0,12,1,112,0,73,36,69,0,176,
		39,0,109,35,0,100,100,100,100,100,100,20,7,4,
		0,0,83,35,0,109,35,0,73,36,70,0,109,27,
		0,134,0,0,0,0,5,31,13,109,28,0,134,0,
		0,0,0,5,28,51,36,71,0,176,40,0,106,3,
		48,49,0,20,1,36,72,0,176,41,0,106,18,68,
		97,116,97,115,32,73,110,118,160,108,105,100,97,115,
		32,33,0,92,3,20,2,36,73,0,26,238,254,36,
		75,0,109,27,0,109,28,0,15,28,59,36,76,0,
		176,40,0,106,3,48,49,0,20,1,36,77,0,176,
		41,0,106,26,68,97,116,97,32,73,110,105,99,105,
		111,32,62,32,81,117,101,32,70,105,110,97,108,32,
		33,0,92,3,20,2,36,78,0,26,169,254,36,84,
		0,176,42,0,109,12,0,92,5,12,2,83,12,0,
		36,85,0,176,43,0,92,60,12,1,83,44,0,36,
		86,0,176,45,0,109,46,0,12,1,176,43,0,122,
		12,1,69,28,18,36,87,0,109,46,0,106,2,58,
		0,72,83,44,0,25,13,36,89,0,106,3,67,58,
		0,83,44,0,36,92,0,109,47,0,176,43,0,92,
		30,12,1,69,28,49,36,93,0,176,45,0,109,44,
		0,12,1,109,47,0,72,106,16,83,65,76,68,79,
		67,82,69,68,79,82,46,84,88,84,0,72,176,43,
		0,92,10,12,1,72,83,44,0,25,43,36,95,0,
		176,45,0,109,44,0,12,1,106,16,83,65,76,68,
		79,67,82,69,68,79,82,46,84,88,84,0,72,176,
		43,0,92,10,12,1,72,83,44,0,36,97,0,176,
		43,0,92,10,12,1,83,48,0,36,99,0,176,40,
		0,106,3,48,49,0,20,1,36,100,0,176,49,0,
		106,55,68,101,115,101,106,97,32,101,120,116,114,97,
		116,111,32,101,109,32,97,114,113,117,105,118,111,32,
		116,101,120,116,111,32,110,111,32,68,114,105,118,101,
		32,40,88,58,41,32,40,83,47,78,41,32,63,32,
		0,20,1,36,101,0,109,50,0,106,2,83,0,5,
		29,255,0,36,102,0,176,40,0,106,3,48,49,0,
		20,1,36,103,0,176,14,0,92,23,121,20,2,176,
		15,0,106,23,67,79,78,70,73,82,77,69,32,79,
		32,68,69,83,84,73,78,79,32,32,58,32,0,20,
		1,176,31,0,176,32,0,12,0,176,33,0,12,0,
		122,72,20,2,176,34,0,109,35,0,176,36,0,100,
		106,9,101,65,82,81,85,73,86,79,0,100,100,100,
		12,5,20,2,48,37,0,176,38,0,109,35,0,12,
		1,112,0,73,36,104,0,176,39,0,109,35,0,100,
		100,100,100,100,100,20,7,4,0,0,83,35,0,109,
		35,0,73,36,105,0,176,45,0,109,44,0,12,1,
		83,44,0,36,106,0,176,40,0,106,3,48,49,0,
		20,1,36,107,0,176,51,0,106,46,73,110,115,105,
		114,97,32,117,109,32,100,105,115,99,111,32,110,111,
		32,100,114,105,118,101,32,40,88,58,41,44,32,101,
		32,116,101,99,108,101,32,69,78,84,69,82,46,0,
		20,1,36,108,0,176,52,0,20,0,36,109,0,106,
		8,65,82,81,85,73,86,79,0,83,48,0,36,110,
		0,25,97,36,112,0,176,51,0,106,50,80,111,115,
		105,99,105,111,110,101,32,111,32,70,111,114,109,117,
		108,97,114,105,111,32,56,48,32,67,111,108,117,110,
		97,115,32,101,32,84,101,99,108,101,32,91,69,78,
		84,69,82,93,0,20,1,36,113,0,176,52,0,20,
		0,36,114,0,176,53,0,20,0,36,115,0,106,11,
		73,77,80,82,69,83,83,79,82,65,0,83,48,0,
		36,119,0,176,40,0,106,3,48,49,0,20,1,36,
		120,0,176,49,0,106,59,68,101,115,101,106,97,32,
		69,102,101,116,117,97,114,32,97,32,67,111,110,115,
		111,108,105,100,97,135,198,111,32,100,111,32,65,114,
		113,117,105,118,111,32,100,101,32,83,97,108,100,111,
		32,40,83,47,78,41,32,63,32,0,20,1,36,121,
		0,109,50,0,106,2,83,0,5,29,223,6,36,122,
		0,9,83,54,0,36,123,0,176,40,0,106,3,48,
		49,0,20,1,36,124,0,176,49,0,106,63,67,111,
		110,118,101,114,116,101,114,32,83,97,108,100,111,115,
		32,65,110,116,101,114,105,111,114,101,115,32,97,32,
		48,49,47,48,55,47,49,57,57,52,32,101,109,32,
		82,101,97,108,32,40,82,36,41,32,40,83,47,78,
		41,32,63,32,0,20,1,36,125,0,109,50,0,106,
		2,83,0,5,28,9,36,126,0,120,83,54,0,36,
		128,0,9,83,55,0,36,129,0,176,40,0,106,3,
		48,49,0,20,1,36,130,0,176,49,0,106,30,68,
		101,115,101,106,97,32,97,112,101,110,97,115,32,84,
		111,116,97,105,115,32,40,83,47,78,41,32,63,32,
		0,20,1,36,131,0,109,50,0,106,2,83,0,5,
		28,9,36,132,0,120,83,55,0,36,134,0,176,40,
		0,106,3,48,49,0,20,1,36,135,0,176,41,0,
		106,10,65,116,101,110,135,198,111,32,33,0,92,3,
		20,2,36,136,0,176,51,0,106,56,65,103,117,97,
		114,100,101,32,97,32,109,111,110,116,97,103,101,109,
		32,100,111,32,66,97,110,99,111,32,100,101,32,68,
		97,100,111,115,32,112,97,114,97,32,73,109,112,114,
		101,115,115,198,111,32,46,46,46,0,20,1,36,137,
		0,109,13,0,122,5,29,214,2,36,138,0,176,6,
		0,106,2,56,0,20,1,36,139,0,176,56,0,92,
		8,106,3,79,78,0,20,2,36,140,0,176,7,0,
		9,100,106,9,84,69,77,80,95,80,65,82,0,100,
		100,9,20,6,36,141,0,176,57,0,12,0,28,18,
		36,142,0,176,58,0,20,0,36,143,0,176,59,0,
		20,0,36,145,0,176,56,0,92,8,106,4,79,70,
		70,0,20,2,36,146,0,176,6,0,106,2,51,0,
		20,1,36,147,0,176,60,0,90,17,109,61,0,109,
		12,0,5,21,28,6,73,109,62,0,6,106,38,78,
		85,77,95,76,79,84,69,65,77,32,61,32,101,67,
		76,65,83,95,80,82,79,32,46,65,78,68,46,32,
		67,65,68,65,83,84,82,79,0,20,2,36,148,0,
		176,9,0,20,0,36,149,0,176,63,0,12,0,32,
		232,0,36,150,0,109,12,0,83,64,0,36,151,0,
		109,65,0,83,66,0,36,152,0,176,6,0,106,2,
		56,0,20,1,36,153,0,176,7,0,9,100,106,9,
		84,69,77,80,95,80,65,82,0,100,100,9,20,6,
		36,154,0,176,9,0,20,0,36,155,0,106,4,48,
		48,48,0,83,67,0,36,156,0,106,3,67,80,0,
		176,68,0,109,67,0,122,92,3,176,69,0,176,45,
		0,109,64,0,12,1,12,1,49,12,3,72,176,45,
		0,109,64,0,12,1,72,176,68,0,109,67,0,122,
		92,3,176,69,0,176,45,0,109,66,0,12,1,12,
		1,49,12,3,72,176,45,0,109,66,0,12,1,72,
		83,70,0,36,157,0,176,71,0,109,70,0,4,0,
		0,90,21,109,72,0,109,27,0,16,21,28,10,73,
		109,72,0,109,28,0,34,6,100,100,100,9,100,100,
		100,20,10,36,158,0,176,9,0,20,0,36,159,0,
		176,6,0,106,2,51,0,20,1,36,160,0,176,73,
		0,122,20,1,26,19,255,36,162,0,176,56,0,92,
		8,106,3,79,78,0,20,2,36,163,0,176,6,0,
		106,2,56,0,20,1,36,164,0,176,7,0,9,100,
		106,9,84,69,77,80,95,80,65,82,0,100,100,9,
		20,6,36,165,0,176,74,0,106,9,84,69,77,80,
		95,80,65,82,0,106,38,67,76,65,83,83,69,43,
		81,85,65,68,82,65,43,78,85,77,95,73,77,79,
		86,69,76,43,83,84,82,40,80,65,82,84,69,44,
		49,41,0,90,24,109,75,0,109,76,0,72,109,77,
		0,72,176,42,0,109,78,0,122,12,2,72,6,100,
		20,4,36,166,0,176,9,0,20,0,36,167,0,109,
		54,0,28,89,36,168,0,176,9,0,20,0,36,169,
		0,176,63,0,12,0,31,71,36,170,0,109,72,0,
		176,30,0,106,11,48,49,47,48,55,47,49,57,57,
		52,0,12,1,34,28,33,36,171,0,176,79,0,12,
		0,28,23,36,172,0,109,80,0,93,190,10,18,78,
		80,0,36,173,0,176,59,0,20,0,36,176,0,176,
		73,0,122,20,1,25,179,36,179,0,176,7,0,9,
		100,106,9,84,69,77,80,95,80,65,82,0,100,100,
		9,20,6,176,8,0,106,9,84,69,77,80,95,80,
		65,82,0,20,1,36,180,0,176,9,0,20,0,36,
		181,0,176,56,0,92,8,106,4,79,70,70,0,20,
		2,26,219,2,36,183,0,176,6,0,106,2,56,0,
		20,1,36,184,0,176,56,0,92,8,106,3,79,78,
		0,20,2,36,185,0,176,7,0,9,100,106,9,84,
		69,77,80,95,80,65,82,0,100,100,9,20,6,36,
		186,0,176,57,0,12,0,28,18,36,187,0,176,58,
		0,20,0,36,188,0,176,59,0,20,0,36,190,0,
		176,56,0,92,8,106,4,79,70,70,0,20,2,36,
		191,0,176,6,0,106,2,52,0,20,1,36,192,0,
		176,60,0,90,17,109,61,0,109,12,0,5,21,28,
		6,73,109,62,0,6,106,38,78,85,77,95,76,79,
		84,69,65,77,32,61,32,101,67,76,65,83,95,80,
		82,79,32,46,65,78,68,46,32,67,65,68,65,83,
		84,82,79,0,20,2,36,193,0,176,9,0,20,0,
		36,194,0,176,63,0,12,0,32,232,0,36,195,0,
		109,12,0,83,64,0,36,196,0,109,81,0,83,66,
		0,36,197,0,176,6,0,106,2,56,0,20,1,36,
		198,0,176,7,0,9,100,106,9,84,69,77,80,95,
		80,65,82,0,100,100,9,20,6,36,199,0,176,9,
		0,20,0,36,200,0,106,4,48,48,48,0,83,67,
		0,36,201,0,106,3,67,80,0,176,68,0,109,67,
		0,122,92,3,176,69,0,176,45,0,109,64,0,12,
		1,12,1,49,12,3,72,176,45,0,109,64,0,12,
		1,72,176,68,0,109,67,0,122,92,3,176,69,0,
		176,45,0,109,66,0,12,1,12,1,49,12,3,72,
		176,45,0,109,66,0,12,1,72,83,70,0,36,202,
		0,176,71,0,109,70,0,4,0,0,90,21,109,72,
		0,109,27,0,16,21,28,10,73,109,72,0,109,28,
		0,34,6,100,100,100,9,100,100,100,20,10,36,203,
		0,176,9,0,20,0,36,204,0,176,6,0,106,2,
		52,0,20,1,36,205,0,176,73,0,122,20,1,26,
		19,255,36,207,0,176,56,0,92,8,106,3,79,78,
		0,20,2,36,208,0,176,6,0,106,2,56,0,20,
		1,36,209,0,176,7,0,9,100,106,9,84,69,77,
		80,95,80,65,82,0,100,100,9,20,6,36,210,0,
		176,74,0,106,9,84,69,77,80,95,80,65,82,0,
		106,38,67,76,65,83,83,69,43,81,85,65,68,82,
		65,43,78,85,77,95,73,77,79,86,69,76,43,83,
		84,82,40,80,65,82,84,69,44,49,41,0,90,24,
		109,75,0,109,76,0,72,109,77,0,72,176,42,0,
		109,78,0,122,12,2,72,6,100,20,4,36,211,0,
		176,9,0,20,0,36,212,0,176,7,0,9,100,106,
		9,84,69,77,80,95,80,65,82,0,100,100,9,20,
		6,176,8,0,106,9,84,69,77,80,95,80,65,82,
		0,20,1,36,213,0,176,9,0,20,0,36,214,0,
		109,54,0,28,89,36,215,0,176,9,0,20,0,36,
		216,0,176,63,0,12,0,31,71,36,217,0,109,72,
		0,176,30,0,106,11,48,49,47,48,55,47,49,57,
		57,52,0,12,1,34,28,33,36,218,0,176,79,0,
		12,0,28,23,36,219,0,109,80,0,93,190,10,18,
		78,80,0,36,220,0,176,59,0,20,0,36,223,0,
		176,73,0,122,20,1,25,179,36,226,0,176,9,0,
		20,0,36,227,0,176,56,0,92,8,106,4,79,70,
		70,0,20,2,36,230,0,176,6,0,106,2,54,0,
		20,1,36,231,0,176,82,0,100,20,1,36,232,0,
		176,9,0,20,0,36,233,0,176,6,0,106,2,56,
		0,20,1,36,234,0,176,57,0,12,0,28,18,36,
		235,0,176,83,0,20,0,36,236,0,176,59,0,20,
		0,36,238,0,176,9,0,20,0,36,239,0,176,63,
		0,12,0,28,92,36,240,0,176,40,0,106,3,48,
		49,0,20,1,36,241,0,176,41,0,106,10,65,116,
		101,110,135,198,111,32,33,0,92,3,20,2,36,242,
		0,176,51,0,106,34,78,198,111,32,72,160,32,80,
		97,103,97,109,101,110,116,111,115,32,78,101,115,116,
		101,32,80,101,114,161,111,100,111,32,33,0,20,1,
		36,243,0,176,52,0,20,0,26,127,6,36,245,0,
		109,48,0,106,8,65,82,81,85,73,86,79,0,5,
		28,53,36,246,0,176,56,0,92,23,106,3,79,78,
		0,20,2,36,247,0,176,56,0,92,20,106,8,80,
		82,73,78,84,69,82,0,20,2,36,248,0,176,56,
		0,92,24,109,44,0,9,20,3,25,98,36,250,0,
		176,51,0,106,50,80,111,115,105,99,105,111,110,101,
		32,111,32,70,111,114,109,117,108,97,114,105,111,32,
		56,48,32,67,111,108,117,110,97,115,32,101,32,84,
		101,99,108,101,32,91,69,78,84,69,82,93,0,20,
		1,36,251,0,176,52,0,20,0,36,252,0,176,53,
		0,20,0,36,253,0,176,56,0,92,20,106,8,80,
		82,73,78,84,69,82,0,20,2,36,255,0,121,83,
		84,0,36,0,1,121,83,85,0,36,1,1,121,83,
		86,0,36,2,1,121,83,87,0,36,3,1,121,83,
		88,0,36,4,1,176,63,0,12,0,32,168,5,36,
		5,1,176,14,0,122,121,20,2,176,15,0,109,89,
		0,106,11,69,109,112,114,101,115,97,32,58,32,0,
		72,109,90,0,72,20,1,36,6,1,176,14,0,122,
		92,120,20,2,176,15,0,106,8,68,97,116,97,32,
		58,32,0,176,91,0,176,29,0,12,0,12,1,72,
		20,1,36,7,1,176,14,0,92,2,121,20,2,176,
		15,0,106,11,83,105,115,116,101,109,97,32,58,32,
		0,109,92,0,72,20,1,36,8,1,176,14,0,92,
		2,92,121,20,2,176,15,0,106,14,82,101,108,97,
		116,58,32,83,65,78,48,48,52,0,20,1,36,9,
		1,176,14,0,92,3,92,20,20,2,176,15,0,109,
		93,0,106,27,82,69,76,65,84,79,82,73,79,32,
		83,65,76,68,79,32,67,82,69,68,79,82,32,68,
		69,32,0,72,176,91,0,109,27,0,12,1,72,106,
		6,32,65,84,69,32,0,72,176,91,0,109,28,0,
		12,1,72,109,89,0,72,20,1,36,10,1,176,14,
		0,92,4,121,20,2,176,15,0,106,11,80,114,111,
		100,117,116,111,32,58,32,0,109,12,0,72,106,4,
		32,45,32,0,72,176,45,0,109,26,0,12,1,72,
		20,1,36,11,1,176,14,0,92,5,121,20,2,176,
		15,0,176,94,0,106,2,61,0,93,135,0,12,2,
		20,1,36,12,1,176,14,0,92,6,92,2,20,2,
		176,15,0,106,11,78,111,46,32,73,77,79,86,69,
		76,0,20,1,36,13,1,176,14,0,92,6,92,15,
		20,2,176,15,0,106,13,78,79,77,69,32,67,76,
		73,69,78,84,69,0,20,1,36,14,1,176,14,0,
		92,6,92,70,20,2,176,15,0,106,11,83,68,46,
		32,67,82,69,68,79,82,0,20,1,36,15,1,176,
		14,0,92,6,92,95,20,2,176,15,0,106,9,80,
		65,82,67,69,76,65,83,0,20,1,36,16,1,176,
		14,0,92,6,92,115,20,2,176,15,0,106,10,67,
		79,78,84,82,65,84,79,83,0,20,1,36,17,1,
		176,14,0,92,7,121,20,2,176,15,0,176,94,0,
		106,2,61,0,93,135,0,12,2,20,1,36,18,1,
		92,8,83,95,0,36,19,1,176,63,0,12,0,32,
		188,2,109,95,0,92,54,34,29,179,2,36,20,1,
		109,13,0,122,5,28,40,36,21,1,109,75,0,83,
		12,0,36,22,1,109,76,0,83,96,0,36,23,1,
		109,77,0,83,97,0,36,24,1,109,78,0,83,98,
		0,25,38,36,26,1,109,75,0,83,12,0,36,27,
		1,109,76,0,83,96,0,36,28,1,109,77,0,83,
		97,0,36,29,1,109,78,0,83,98,0,36,31,1,
		176,6,0,106,2,54,0,20,1,36,32,1,176,99,
		0,109,12,0,109,96,0,72,109,97,0,72,176,42,
		0,109,98,0,122,12,2,72,100,100,20,3,36,33,
		1,109,100,0,83,101,0,36,34,1,176,6,0,106,
		2,53,0,20,1,36,35,1,176,99,0,109,101,0,
		100,100,20,3,36,36,1,109,102,0,83,103,0,36,
		37,1,176,6,0,106,2,56,0,20,1,36,38,1,
		121,83,84,0,36,39,1,121,83,85,0,36,40,1,
		109,75,0,109,12,0,5,28,123,109,96,0,109,76,
		0,5,28,114,109,97,0,109,77,0,5,28,105,109,
		98,0,109,78,0,5,28,96,176,63,0,12,0,31,
		89,36,41,1,109,104,0,176,30,0,106,11,48,49,
		47,48,55,47,49,57,57,52,0,12,1,35,28,26,
		109,54,0,28,21,36,42,1,109,84,0,109,105,0,
		93,190,10,18,72,83,84,0,25,15,36,44,1,109,
		84,0,109,105,0,72,83,84,0,36,46,1,109,85,
		0,122,72,83,85,0,36,47,1,176,73,0,122,20,
		1,26,126,255,36,49,1,109,55,0,32,180,0,36,
		50,1,176,14,0,109,95,0,92,2,20,2,176,15,
		0,176,42,0,109,98,0,122,12,2,106,2,47,0,
		72,176,45,0,109,96,0,12,1,72,106,2,47,0,
		72,176,45,0,109,97,0,12,1,72,20,1,36,51,
		1,176,14,0,109,95,0,92,15,20,2,176,15,0,
		109,103,0,20,1,36,52,1,176,14,0,109,95,0,
		92,70,20,2,176,106,0,109,84,0,106,22,64,82,
		32,57,57,57,44,57,57,57,44,57,57,57,44,57,
		57,57,46,57,57,0,20,2,36,53,1,176,14,0,
		109,95,0,92,95,20,2,176,106,0,109,85,0,106,
		11,64,82,32,57,57,57,44,57,57,57,0,20,2,
		36,54,1,109,95,0,122,72,83,95,0,36,55,1,
		109,88,0,122,72,83,88,0,36,57,1,109,86,0,
		109,84,0,72,83,86,0,36,58,1,109,87,0,109,
		85,0,72,83,87,0,36,59,1,176,107,0,20,0,
		36,60,1,109,108,0,29,193,253,36,61,1,109,48,
		0,106,8,65,82,81,85,73,86,79,0,5,28,52,
		36,62,1,176,56,0,92,24,106,1,0,20,2,36,
		63,1,176,56,0,92,23,106,4,79,70,70,0,20,
		2,36,64,1,176,56,0,92,20,106,7,83,67,82,
		69,69,78,0,20,2,25,34,36,66,1,176,56,0,
		92,24,106,1,0,20,2,36,67,1,176,56,0,92,
		20,106,7,83,67,82,69,69,78,0,20,2,36,69,
		1,176,109,0,20,0,176,6,0,106,2,49,0,20,
		1,176,110,0,100,20,1,36,70,1,100,110,7,36,
		73,1,176,63,0,12,0,29,161,0,36,74,1,109,
		95,0,122,72,83,95,0,36,75,1,176,14,0,109,
		95,0,92,2,20,2,176,15,0,106,18,84,111,116,
		97,108,32,71,101,114,97,108,32,32,32,32,58,32,
		0,20,1,36,76,1,176,14,0,109,95,0,92,70,
		20,2,176,106,0,109,86,0,106,22,64,82,32,57,
		57,57,44,57,57,57,44,57,57,57,44,57,57,57,
		46,57,57,0,20,2,36,77,1,176,14,0,109,95,
		0,92,95,20,2,176,106,0,109,87,0,106,11,64,
		82,32,57,57,57,44,57,57,57,0,20,2,36,78,
		1,176,14,0,109,95,0,92,115,20,2,176,106,0,
		109,88,0,106,7,64,82,32,57,57,57,0,20,2,
		36,80,1,176,6,0,106,2,56,0,20,1,36,81,
		1,176,14,0,92,56,121,20,2,176,15,0,176,94,
		0,106,2,61,0,93,135,0,12,2,20,1,36,82,
		1,176,14,0,92,57,92,104,20,2,176,15,0,109,
		111,0,20,1,36,83,1,176,14,0,122,121,20,2,
		176,15,0,106,2,32,0,109,112,0,72,20,1,26,
		83,250,36,86,1,109,48,0,106,8,65,82,81,85,
		73,86,79,0,5,28,52,36,87,1,176,56,0,92,
		24,106,1,0,20,2,36,88,1,176,56,0,92,23,
		106,4,79,70,70,0,20,2,36,89,1,176,56,0,
		92,20,106,7,83,67,82,69,69,78,0,20,2,25,
		34,36,91,1,176,56,0,92,24,106,1,0,20,2,
		36,92,1,176,56,0,92,20,106,7,83,67,82,69,
		69,78,0,20,2,36,94,1,176,109,0,20,0,36,
		95,1,176,113,0,121,121,176,2,0,12,0,176,3,
		0,12,0,109,4,0,20,5,36,96,1,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

