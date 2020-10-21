/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/cor_imoa.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( COR_IMOA );
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
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( RANGECHECK );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( PDESENV );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_COR_IMOA )
{ "COR_IMOA", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( COR_IMOA )}, NULL },
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
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EQUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "NUM_LOTEAM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "CADASTRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "NUM_QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STATUS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "NUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESTATUS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROPR_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROPR_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "ROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROW )}, NULL },
{ "COL", {HB_FS_PUBLIC}, {HB_FUNCNAME( COL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "RANGECHECK", {HB_FS_PUBLIC}, {HB_FUNCNAME( RANGECHECK )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PDESENV", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDESENV )}, NULL },
{ "EDATA_COR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ULT_CORREC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_VENDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDT_VENDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "INDICE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EINDICE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PARCELAS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPARCELAS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "EDATA_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "ATIVA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "EQUANT_PAR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VLR_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EACUM_RES1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EACUM_TAXA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "EMES_LIMIT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EANO_LIMIT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VLR_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EVLR_ORIG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMES_PARCE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EANO_PARCE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMES_VENDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EANO_VENDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ANO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MES", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND_ACUM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EIND_RESI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VLR_ORIGIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_COR_IMOA, "C:/sis/SAN/xhb/cor_imoa.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_COR_IMOA
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_COR_IMOA )
   #include "hbiniseg.h"
#endif

HB_FUNC( COR_IMOA )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,14,0,176,5,
		0,106,44,67,79,82,82,69,67,65,79,32,77,79,
		78,69,84,181,82,73,65,32,68,69,32,70,73,78,
		65,78,67,73,65,77,69,78,84,79,32,45,32,65,
		78,85,65,76,0,20,1,36,15,0,176,6,0,106,
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
		68,95,86,69,78,68,0,20,1,176,8,0,106,9,
		68,65,84,65,95,86,69,78,0,20,1,36,32,0,
		176,9,0,20,0,36,33,0,176,6,0,106,2,55,
		0,20,1,36,34,0,176,7,0,9,100,106,9,84,
		65,88,65,95,77,69,83,0,100,100,9,20,6,176,
		8,0,106,9,84,65,88,65,95,77,69,83,0,20,
		1,36,35,0,176,9,0,20,0,36,36,0,176,6,
		0,106,2,56,0,20,1,36,38,0,176,6,0,106,
		2,57,0,20,1,36,39,0,176,7,0,9,100,106,
		8,73,78,68,73,67,69,83,0,100,100,9,20,6,
		176,8,0,106,8,73,78,68,73,67,69,83,0,20,
		1,36,40,0,176,9,0,20,0,36,41,0,176,6,
		0,106,2,50,0,20,1,36,44,0,176,10,0,20,
		0,36,45,0,176,11,0,106,9,87,47,78,44,43,
		87,47,82,0,20,1,36,46,0,121,83,12,0,36,
		47,0,121,83,13,0,36,48,0,176,14,0,92,5,
		92,2,20,2,176,15,0,106,11,80,114,111,100,117,
		116,111,32,58,32,0,20,1,36,49,0,176,9,0,
		20,0,36,50,0,92,3,3,1,0,92,3,3,1,
		0,92,3,3,1,0,92,3,3,1,0,176,20,0,
		108,16,108,17,108,18,108,19,20,4,81,19,0,81,
		18,0,81,17,0,81,16,0,36,51,0,176,21,0,
		109,16,0,120,109,19,0,20,3,36,52,0,106,8,
		64,82,57,57,57,57,57,0,98,17,0,122,2,36,
		53,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,92,2,2,36,54,0,106,5,64,83,52,48,0,
		98,17,0,92,3,2,36,55,0,106,5,32,32,32,
		32,0,98,18,0,122,2,36,56,0,106,5,32,32,
		32,32,0,98,18,0,92,2,2,36,57,0,106,5,
		32,32,32,32,0,98,18,0,92,3,2,36,58,0,
		176,22,0,92,5,92,15,92,5,92,70,109,16,0,
		106,10,70,85,78,67,95,80,82,79,86,0,109,17,
		0,109,18,0,106,2,32,0,106,4,32,45,32,0,
		106,2,32,0,20,11,36,59,0,109,23,0,83,12,
		0,36,60,0,109,24,0,83,13,0,36,64,0,176,
		25,0,122,12,1,83,26,0,36,65,0,176,25,0,
		122,12,1,83,26,0,36,67,0,109,13,0,122,5,
		29,63,6,36,69,0,176,6,0,106,2,51,0,20,
		1,36,70,0,176,27,0,90,24,109,28,0,176,29,
		0,109,12,0,92,5,12,2,5,21,28,6,73,109,
		30,0,6,106,45,78,85,77,95,76,79,84,69,65,
		77,32,61,32,83,84,82,40,101,67,76,65,83,95,
		80,82,79,44,53,41,32,46,65,78,68,46,32,67,
		65,68,65,83,84,82,79,0,20,2,36,71,0,176,
		9,0,20,0,36,72,0,176,25,0,92,5,12,1,
		83,26,0,36,73,0,176,25,0,92,5,12,1,83,
		31,0,36,74,0,176,32,0,106,3,48,49,0,20,
		1,36,75,0,176,14,0,92,6,92,2,20,2,176,
		15,0,106,11,81,117,97,100,114,97,32,32,58,32,
		0,20,1,36,76,0,176,9,0,20,0,36,77,0,
		92,4,3,1,0,92,4,3,1,0,92,4,3,1,
		0,92,4,3,1,0,176,20,0,108,16,108,17,108,
		18,108,19,20,4,81,19,0,81,18,0,81,17,0,
		81,16,0,36,78,0,176,21,0,109,16,0,120,109,
		19,0,20,3,36,79,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,122,2,36,80,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,92,2,2,36,
		81,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,92,3,2,36,82,0,106,8,64,83,57,57,57,
		57,57,0,98,17,0,92,4,2,36,83,0,106,5,
		32,32,32,32,0,98,18,0,122,2,36,84,0,106,
		5,32,32,32,32,0,98,18,0,92,2,2,36,85,
		0,106,5,32,32,32,32,0,98,18,0,92,3,2,
		36,86,0,106,5,32,32,32,32,0,98,18,0,92,
		4,2,36,87,0,176,22,0,92,6,92,14,92,6,
		92,45,109,16,0,106,10,70,85,78,67,95,80,82,
		79,86,0,109,17,0,109,18,0,106,2,32,0,106,
		4,32,45,32,0,106,2,32,0,20,11,36,88,0,
		109,33,0,83,26,0,36,89,0,109,34,0,83,35,
		0,36,90,0,109,36,0,83,37,0,36,91,0,176,
		25,0,92,5,12,1,83,31,0,36,92,0,176,32,
		0,106,3,48,49,0,20,1,36,93,0,176,38,0,
		106,34,68,101,115,101,106,97,32,83,101,108,101,99,
		105,111,110,97,114,32,97,32,76,111,116,101,32,40,
		83,47,78,41,32,63,32,0,20,1,36,94,0,109,
		39,0,106,2,83,0,5,29,144,14,36,95,0,176,
		14,0,92,7,92,2,20,2,176,15,0,106,11,76,
		111,116,101,32,32,32,32,58,32,0,20,1,36,96,
		0,176,6,0,106,2,49,0,20,1,36,97,0,176,
		27,0,90,60,109,40,0,176,29,0,109,12,0,92,
		5,12,2,5,21,28,42,73,109,41,0,109,26,0,
		5,21,28,31,73,109,42,0,121,5,28,9,109,43,
		0,122,69,25,16,109,43,0,122,69,21,31,8,73,
		109,44,0,122,69,6,106,128,67,76,65,83,83,69,
		32,61,32,83,84,82,40,101,67,76,65,83,95,80,
		82,79,44,53,41,32,46,65,78,68,46,32,81,85,
		65,68,82,65,32,61,32,101,81,85,65,68,82,65,
		32,46,65,78,68,46,32,73,73,70,40,83,84,65,
		84,85,83,32,61,32,48,44,40,80,82,79,80,82,
		73,69,84,95,49,32,60,62,32,49,41,44,40,80,
		82,79,80,82,73,69,84,95,49,32,60,62,32,49,
		32,46,79,82,46,32,80,82,79,80,82,73,69,84,
		95,50,32,60,62,32,49,41,41,0,20,2,36,98,
		0,176,9,0,20,0,36,99,0,176,45,0,12,0,
		28,119,36,100,0,176,32,0,106,3,48,49,0,20,
		1,36,101,0,176,46,0,106,10,65,116,101,110,135,
		198,111,32,33,0,92,3,20,2,36,102,0,176,47,
		0,106,29,78,198,111,32,72,160,32,73,109,111,118,
		101,105,115,32,68,105,115,112,111,110,161,118,101,105,
		115,32,33,0,20,1,36,103,0,176,48,0,20,0,
		36,104,0,176,49,0,20,0,36,105,0,176,50,0,
		121,121,176,2,0,12,0,176,3,0,12,0,109,4,
		0,20,5,36,106,0,7,36,108,0,92,6,3,1,
		0,92,6,3,1,0,92,6,3,1,0,92,6,3,
		1,0,176,20,0,108,16,108,17,108,18,108,19,20,
		4,81,19,0,81,18,0,81,17,0,81,16,0,36,
		109,0,176,21,0,109,16,0,120,109,19,0,20,3,
		36,110,0,106,11,80,82,79,80,82,73,69,84,95,
		49,0,98,16,0,92,5,2,36,111,0,106,11,80,
		82,79,80,82,73,69,84,95,50,0,98,16,0,92,
		6,2,36,112,0,92,5,98,19,0,92,5,2,36,
		113,0,92,5,98,19,0,92,6,2,36,114,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,122,2,
		36,115,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,92,2,2,36,116,0,106,8,64,82,57,57,
		57,57,57,0,98,17,0,92,3,2,36,117,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,92,4,
		2,36,118,0,106,8,64,82,57,57,57,57,57,0,
		98,17,0,92,5,2,36,119,0,106,8,64,82,57,
		57,57,57,57,0,98,17,0,92,6,2,36,120,0,
		106,6,32,32,32,32,32,0,98,18,0,122,2,36,
		121,0,106,6,32,32,32,32,32,0,98,18,0,92,
		2,2,36,122,0,106,6,32,32,32,32,32,0,98,
		18,0,92,3,2,36,123,0,106,6,32,32,32,32,
		32,0,98,18,0,92,4,2,36,124,0,106,6,32,
		32,32,32,32,0,98,18,0,92,5,2,36,125,0,
		106,6,32,32,32,32,32,0,98,18,0,92,6,2,
		36,126,0,176,22,0,92,7,92,15,92,7,92,69,
		109,16,0,106,10,70,85,78,67,95,80,82,79,86,
		0,109,17,0,109,18,0,106,2,32,0,106,4,32,
		45,32,0,106,2,32,0,20,11,36,127,0,109,51,
		0,83,31,0,36,128,0,109,42,0,83,52,0,36,
		129,0,109,43,0,83,53,0,36,130,0,109,44,0,
		83,54,0,36,131,0,109,42,0,83,55,0,36,132,
		0,109,55,0,121,69,29,87,11,36,133,0,121,83,
		56,0,36,134,0,176,32,0,106,3,48,49,0,20,
		1,36,135,0,176,14,0,92,23,121,20,2,176,15,
		0,106,52,73,110,102,111,114,109,101,32,67,111,114,
		114,101,135,198,111,32,80,97,114,97,32,111,32,40,
		49,41,111,46,32,111,117,32,40,50,41,111,46,32,
		77,101,105,111,32,76,111,116,101,32,63,32,0,20,
		1,176,57,0,176,58,0,12,0,176,59,0,12,0,
		122,72,20,2,176,60,0,109,61,0,176,62,0,100,
		106,7,101,79,80,67,65,79,0,106,6,57,57,57,
		57,57,0,89,19,0,1,0,0,0,176,63,0,95,
		1,100,122,92,2,12,4,6,100,12,5,20,2,48,
		64,0,176,65,0,109,61,0,12,1,112,0,73,36,
		136,0,176,66,0,109,61,0,100,100,100,100,100,100,
		20,7,4,0,0,83,61,0,109,61,0,73,36,137,
		0,109,56,0,122,5,28,11,36,138,0,122,83,55,
		0,25,10,36,140,0,92,2,83,55,0,36,143,0,
		26,111,10,36,144,0,109,13,0,92,2,5,29,64,
		5,36,146,0,176,6,0,106,2,52,0,20,1,36,
		147,0,176,27,0,90,24,109,28,0,176,29,0,109,
		12,0,92,5,12,2,5,21,28,6,73,109,30,0,
		6,106,45,78,85,77,95,76,79,84,69,65,77,32,
		61,32,83,84,82,40,101,67,76,65,83,95,80,82,
		79,44,53,41,32,46,65,78,68,46,32,67,65,68,
		65,83,84,82,79,0,20,2,36,148,0,176,9,0,
		20,0,36,149,0,176,25,0,92,5,12,1,83,26,
		0,36,150,0,176,25,0,92,5,12,1,83,31,0,
		36,151,0,176,14,0,92,6,92,2,20,2,176,15,
		0,106,11,66,108,111,99,111,32,32,32,58,32,0,
		20,1,36,152,0,176,9,0,20,0,36,153,0,92,
		4,3,1,0,92,4,3,1,0,92,4,3,1,0,
		92,4,3,1,0,176,20,0,108,16,108,17,108,18,
		108,19,20,4,81,19,0,81,18,0,81,17,0,81,
		16,0,36,154,0,176,21,0,109,16,0,120,109,19,
		0,20,3,36,155,0,106,8,64,82,57,57,57,57,
		57,0,98,17,0,122,2,36,156,0,106,8,64,82,
		57,57,57,57,57,0,98,17,0,92,2,2,36,157,
		0,106,5,64,83,51,48,0,98,17,0,92,3,2,
		36,158,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,92,4,2,36,159,0,106,5,32,32,32,32,
		0,98,18,0,122,2,36,160,0,106,5,32,32,32,
		32,0,98,18,0,92,2,2,36,161,0,106,5,32,
		32,32,32,0,98,18,0,92,3,2,36,162,0,106,
		5,32,32,32,32,0,98,18,0,92,4,2,36,163,
		0,176,22,0,92,6,92,8,92,6,92,76,109,16,
		0,106,10,70,85,78,67,95,80,82,79,86,0,109,
		17,0,109,18,0,106,2,32,0,106,4,32,45,32,
		0,106,2,32,0,20,11,36,164,0,109,67,0,83,
		26,0,36,165,0,109,68,0,83,69,0,36,166,0,
		176,25,0,92,5,12,1,83,31,0,36,167,0,176,
		32,0,106,3,48,49,0,20,1,36,168,0,176,38,
		0,106,41,68,101,115,101,106,97,32,83,101,108,101,
		99,105,111,110,97,114,32,111,32,65,112,97,114,116,
		97,109,101,110,116,111,32,40,83,47,78,41,32,63,
		32,0,20,1,36,169,0,109,39,0,106,2,83,0,
		5,29,90,8,36,170,0,176,14,0,92,7,92,2,
		20,2,176,15,0,106,11,65,112,116,111,46,32,32,
		32,58,32,0,20,1,36,171,0,176,6,0,106,2,
		49,0,20,1,36,172,0,176,27,0,90,60,109,40,
		0,176,29,0,109,12,0,92,5,12,2,5,21,28,
		42,73,109,41,0,109,26,0,5,21,28,31,73,109,
		42,0,121,5,28,9,109,43,0,122,69,25,16,109,
		43,0,122,69,21,31,8,73,109,44,0,122,69,6,
		106,128,67,76,65,83,83,69,32,61,32,83,84,82,
		40,101,67,76,65,83,95,80,82,79,44,53,41,32,
		46,65,78,68,46,32,81,85,65,68,82,65,32,61,
		32,101,81,85,65,68,82,65,32,46,65,78,68,46,
		32,73,73,70,40,83,84,65,84,85,83,32,61,32,
		48,44,40,80,82,79,80,82,73,69,84,95,49,32,
		60,62,32,49,41,44,40,80,82,79,80,82,73,69,
		84,95,49,32,60,62,32,49,32,46,79,82,46,32,
		80,82,79,80,82,73,69,84,95,50,32,60,62,32,
		49,41,41,0,20,2,36,173,0,176,9,0,20,0,
		36,174,0,176,45,0,12,0,28,119,36,175,0,176,
		32,0,106,3,48,49,0,20,1,36,176,0,176,46,
		0,106,10,65,116,101,110,135,198,111,32,33,0,92,
		3,20,2,36,177,0,176,47,0,106,29,78,198,111,
		32,72,160,32,73,109,111,118,101,105,115,32,68,105,
		115,112,111,110,161,118,101,105,115,32,33,0,20,1,
		36,178,0,176,48,0,20,0,36,179,0,176,49,0,
		20,0,36,180,0,176,50,0,121,121,176,2,0,12,
		0,176,3,0,12,0,109,4,0,20,5,36,181,0,
		7,36,183,0,92,6,3,1,0,92,6,3,1,0,
		92,6,3,1,0,92,6,3,1,0,176,20,0,108,
		16,108,17,108,18,108,19,20,4,81,19,0,81,18,
		0,81,17,0,81,16,0,36,184,0,176,21,0,109,
		16,0,120,109,19,0,20,3,36,185,0,106,11,80,
		82,79,80,82,73,69,84,95,49,0,98,16,0,92,
		5,2,36,186,0,106,11,80,82,79,80,82,73,69,
		84,95,50,0,98,16,0,92,6,2,36,187,0,92,
		5,98,19,0,92,5,2,36,188,0,92,5,98,19,
		0,92,6,2,36,189,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,122,2,36,190,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,92,2,2,36,
		191,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,92,3,2,36,192,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,92,4,2,36,193,0,106,8,
		64,82,57,57,57,57,57,0,98,17,0,92,5,2,
		36,194,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,92,6,2,36,195,0,106,6,32,32,32,32,
		32,0,98,18,0,122,2,36,196,0,106,6,32,32,
		32,32,32,0,98,18,0,92,2,2,36,197,0,106,
		6,32,32,32,32,32,0,98,18,0,92,3,2,36,
		198,0,106,6,32,32,32,32,32,0,98,18,0,92,
		4,2,36,199,0,106,6,32,32,32,32,32,0,98,
		18,0,92,5,2,36,200,0,106,6,32,32,32,32,
		32,0,98,18,0,92,6,2,36,201,0,176,22,0,
		92,7,92,15,92,7,92,69,109,16,0,106,10,70,
		85,78,67,95,80,82,79,86,0,109,17,0,109,18,
		0,106,2,32,0,106,4,32,45,32,0,106,2,32,
		0,20,11,36,202,0,109,51,0,83,31,0,36,203,
		0,109,42,0,83,52,0,36,204,0,109,43,0,83,
		53,0,36,205,0,109,44,0,83,54,0,36,206,0,
		109,42,0,83,55,0,36,207,0,26,38,5,36,208,
		0,109,13,0,92,3,5,29,18,5,36,210,0,176,
		6,0,106,2,52,0,20,1,36,211,0,176,27,0,
		90,24,109,28,0,176,29,0,109,12,0,92,5,12,
		2,5,21,28,6,73,109,30,0,6,106,45,78,85,
		77,95,76,79,84,69,65,77,32,61,32,83,84,82,
		40,101,67,76,65,83,95,80,82,79,44,53,41,32,
		46,65,78,68,46,32,67,65,68,65,83,84,82,79,
		0,20,2,36,212,0,176,9,0,20,0,36,213,0,
		176,25,0,92,5,12,1,83,26,0,36,214,0,176,
		14,0,92,6,92,2,20,2,176,15,0,106,11,66,
		108,111,99,111,32,32,32,58,32,0,20,1,36,215,
		0,176,9,0,20,0,36,216,0,92,3,3,1,0,
		92,3,3,1,0,92,3,3,1,0,92,3,3,1,
		0,176,20,0,108,16,108,17,108,18,108,19,20,4,
		81,19,0,81,18,0,81,17,0,81,16,0,36,217,
		0,176,21,0,109,16,0,120,109,19,0,20,3,36,
		218,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,122,2,36,219,0,106,8,64,82,57,57,57,57,
		57,0,98,17,0,92,2,2,36,220,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,92,3,2,36,
		221,0,106,5,32,32,32,32,0,98,18,0,122,2,
		36,222,0,106,5,32,32,32,32,0,98,18,0,92,
		2,2,36,223,0,106,5,32,32,32,32,0,98,18,
		0,92,3,2,36,224,0,176,22,0,92,6,92,10,
		92,6,92,41,109,16,0,106,10,70,85,78,67,95,
		80,82,79,86,0,109,17,0,109,18,0,106,2,32,
		0,106,4,32,45,32,0,106,2,32,0,20,11,36,
		225,0,109,67,0,83,26,0,36,226,0,109,68,0,
		83,69,0,36,227,0,176,25,0,92,5,12,1,83,
		31,0,36,228,0,176,32,0,106,3,48,49,0,20,
		1,36,229,0,176,38,0,106,41,68,101,115,101,106,
		97,32,83,101,108,101,99,105,111,110,97,114,32,111,
		32,65,112,97,114,116,97,109,101,110,116,111,32,40,
		83,47,78,41,32,63,32,0,20,1,36,230,0,109,
		39,0,106,2,83,0,5,29,62,3,36,231,0,176,
		14,0,92,7,92,2,20,2,176,15,0,106,11,65,
		112,116,111,46,32,32,32,58,32,0,20,1,36,232,
		0,176,6,0,106,2,49,0,20,1,36,233,0,176,
		27,0,90,60,109,40,0,176,29,0,109,12,0,92,
		5,12,2,5,21,28,42,73,109,41,0,109,26,0,
		5,21,28,31,73,109,42,0,121,5,28,9,109,43,
		0,122,69,25,16,109,43,0,122,69,21,31,8,73,
		109,44,0,122,69,6,106,128,67,76,65,83,83,69,
		32,61,32,83,84,82,40,101,67,76,65,83,95,80,
		82,79,44,53,41,32,46,65,78,68,46,32,81,85,
		65,68,82,65,32,61,32,101,81,85,65,68,82,65,
		32,46,65,78,68,46,32,73,73,70,40,83,84,65,
		84,85,83,32,61,32,48,44,40,80,82,79,80,82,
		73,69,84,95,49,32,60,62,32,49,41,44,40,80,
		82,79,80,82,73,69,84,95,49,32,60,62,32,49,
		32,46,79,82,46,32,80,82,79,80,82,73,69,84,
		95,50,32,60,62,32,49,41,41,0,20,2,36,234,
		0,176,9,0,20,0,36,235,0,176,45,0,12,0,
		28,119,36,236,0,176,32,0,106,3,48,49,0,20,
		1,36,237,0,176,46,0,106,10,65,116,101,110,135,
		198,111,32,33,0,92,3,20,2,36,238,0,176,47,
		0,106,29,78,198,111,32,72,160,32,73,109,111,118,
		101,105,115,32,68,105,115,112,111,110,161,118,101,105,
		115,32,33,0,20,1,36,239,0,176,48,0,20,0,
		36,240,0,176,49,0,20,0,36,241,0,176,50,0,
		121,121,176,2,0,12,0,176,3,0,12,0,109,4,
		0,20,5,36,242,0,7,36,244,0,92,6,3,1,
		0,92,6,3,1,0,92,6,3,1,0,92,6,3,
		1,0,176,20,0,108,16,108,17,108,18,108,19,20,
		4,81,19,0,81,18,0,81,17,0,81,16,0,36,
		245,0,176,21,0,109,16,0,120,109,19,0,20,3,
		36,246,0,106,11,80,82,79,80,82,73,69,84,95,
		49,0,98,16,0,92,5,2,36,247,0,106,11,80,
		82,79,80,82,73,69,84,95,50,0,98,16,0,92,
		6,2,36,248,0,92,5,98,19,0,92,5,2,36,
		249,0,92,5,98,19,0,92,6,2,36,250,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,122,2,
		36,251,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,92,2,2,36,252,0,106,8,64,82,57,57,
		57,57,57,0,98,17,0,92,3,2,36,253,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,92,4,
		2,36,254,0,106,8,64,82,57,57,57,57,57,0,
		98,17,0,92,5,2,36,255,0,106,8,64,82,57,
		57,57,57,57,0,98,17,0,92,6,2,36,0,1,
		106,6,32,32,32,32,32,0,98,18,0,122,2,36,
		1,1,106,6,32,32,32,32,32,0,98,18,0,92,
		2,2,36,2,1,106,6,32,32,32,32,32,0,98,
		18,0,92,3,2,36,3,1,106,6,32,32,32,32,
		32,0,98,18,0,92,4,2,36,4,1,106,6,32,
		32,32,32,32,0,98,18,0,92,5,2,36,5,1,
		106,6,32,32,32,32,32,0,98,18,0,92,6,2,
		36,6,1,176,22,0,92,7,92,15,92,7,92,69,
		109,16,0,106,10,70,85,78,67,95,80,82,79,86,
		0,109,17,0,109,18,0,106,2,32,0,106,4,32,
		45,32,0,106,2,32,0,20,11,36,7,1,109,51,
		0,83,31,0,36,8,1,109,42,0,83,52,0,36,
		9,1,109,43,0,83,53,0,36,10,1,109,44,0,
		83,54,0,36,11,1,109,42,0,83,55,0,36,12,
		1,25,10,36,15,1,176,70,0,20,0,36,17,1,
		134,0,0,0,0,83,71,0,36,19,1,176,32,0,
		106,3,48,49,0,20,1,36,20,1,176,14,0,92,
		23,121,20,2,176,15,0,106,28,68,97,116,97,32,
		73,110,105,99,105,97,108,32,100,97,32,67,111,114,
		114,101,135,198,111,32,58,32,0,20,1,176,57,0,
		176,58,0,12,0,176,59,0,12,0,122,72,20,2,
		176,60,0,109,61,0,176,62,0,100,106,10,101,68,
		65,84,65,95,67,79,82,0,100,100,100,12,5,20,
		2,48,64,0,176,65,0,109,61,0,12,1,112,0,
		73,36,21,1,176,66,0,109,61,0,100,100,100,100,
		100,100,20,7,4,0,0,83,61,0,109,61,0,73,
		36,22,1,109,71,0,134,0,0,0,0,5,28,49,
		36,23,1,176,32,0,106,3,48,49,0,20,1,36,
		24,1,176,46,0,106,16,68,97,116,97,32,73,110,
		118,97,108,105,100,97,32,33,0,92,3,20,2,36,
		25,1,26,52,255,36,30,1,109,31,0,176,25,0,
		92,5,12,1,5,29,153,0,36,31,1,176,6,0,
		106,2,54,0,20,1,36,32,1,176,27,0,90,39,
		109,40,0,176,29,0,109,12,0,92,5,12,2,5,
		21,28,21,73,109,41,0,109,26,0,5,21,28,10,
		73,109,72,0,109,71,0,35,6,106,78,67,76,65,
		83,83,69,32,61,32,83,84,82,40,101,67,76,65,
		83,95,80,82,79,44,53,41,32,46,65,78,68,46,
		32,81,85,65,68,82,65,32,61,32,101,81,85,65,
		68,82,65,32,46,65,78,68,46,32,85,76,84,95,
		67,79,82,82,69,67,32,60,32,101,68,65,84,65,
		95,67,79,82,0,20,2,36,33,1,176,9,0,20,
		0,26,224,0,36,35,1,176,6,0,106,2,54,0,
		20,1,36,36,1,176,27,0,90,61,109,40,0,176,
		29,0,109,12,0,92,5,12,2,5,21,28,43,73,
		109,41,0,109,26,0,5,21,28,32,73,109,51,0,
		109,31,0,5,21,28,21,73,109,73,0,109,55,0,
		5,21,28,10,73,109,72,0,109,71,0,35,6,106,
		130,67,76,65,83,83,69,32,61,32,83,84,82,40,
		101,67,76,65,83,95,80,82,79,44,53,41,32,46,
		65,78,68,46,32,81,85,65,68,82,65,32,61,32,
		101,81,85,65,68,82,65,32,46,65,78,68,46,32,
		78,85,77,95,73,77,79,86,69,76,32,61,32,101,
		78,85,77,95,73,77,79,86,69,76,32,46,65,78,
		68,46,32,80,65,82,84,69,32,61,32,101,80,65,
		82,84,69,32,46,65,78,68,46,32,85,76,84,95,
		67,79,82,82,69,67,32,60,32,101,68,65,84,65,
		95,67,79,82,0,20,2,36,37,1,176,9,0,20,
		0,36,39,1,109,51,0,83,31,0,36,40,1,109,
		41,0,83,26,0,36,41,1,109,73,0,83,55,0,
		36,42,1,109,74,0,83,75,0,36,43,1,109,76,
		0,83,77,0,36,44,1,109,78,0,83,79,0,36,
		45,1,176,80,0,92,7,121,92,7,20,3,176,57,
		0,92,7,121,20,2,36,46,1,176,14,0,92,7,
		92,2,20,2,176,15,0,106,11,76,111,116,101,32,
		32,32,32,58,32,0,20,1,176,57,0,176,58,0,
		12,0,176,59,0,12,0,122,72,20,2,176,60,0,
		109,61,0,176,62,0,100,106,12,101,78,85,77,95,
		73,77,79,86,69,76,0,100,100,100,12,5,20,2,
		48,64,0,176,65,0,109,61,0,12,1,112,0,73,
		36,47,1,109,71,0,83,81,0,36,48,1,134,0,
		0,0,0,83,82,0,36,50,1,176,14,0,92,8,
		92,2,20,2,176,15,0,106,15,67,111,114,114,101,
		135,198,111,32,100,101,32,58,32,0,20,1,176,57,
		0,176,58,0,12,0,176,59,0,12,0,122,72,20,
		2,176,60,0,109,61,0,176,62,0,100,106,10,101,
		68,65,84,65,95,73,78,73,0,100,100,100,12,5,
		20,2,48,64,0,176,65,0,109,61,0,12,1,112,
		0,73,36,51,1,176,66,0,109,61,0,100,100,100,
		100,100,100,20,7,4,0,0,83,61,0,109,61,0,
		73,36,52,1,109,81,0,93,109,1,72,83,82,0,
		36,53,1,176,14,0,92,9,92,2,20,2,176,15,
		0,106,15,67,111,114,114,101,135,198,111,32,97,116,
		130,58,32,0,20,1,176,57,0,176,58,0,12,0,
		176,59,0,12,0,122,72,20,2,176,60,0,109,61,
		0,176,62,0,100,106,10,101,68,65,84,65,95,70,
		73,77,0,100,100,100,12,5,20,2,48,64,0,176,
		65,0,109,61,0,12,1,112,0,73,36,54,1,176,
		66,0,109,61,0,100,100,100,100,100,100,20,7,4,
		0,0,83,61,0,109,61,0,73,36,55,1,109,81,
		0,134,0,0,0,0,5,31,13,109,82,0,134,0,
		0,0,0,5,28,51,36,56,1,176,32,0,106,3,
		48,49,0,20,1,36,57,1,176,46,0,106,18,68,
		97,116,97,115,32,73,110,118,97,108,105,100,97,115,
		32,33,0,92,3,20,2,36,58,1,26,186,254,36,
		63,1,176,32,0,106,3,48,49,0,20,1,36,64,
		1,176,38,0,106,39,67,111,110,102,105,114,109,97,
		32,97,32,67,111,114,114,101,135,198,111,32,77,111,
		110,101,116,160,114,105,97,32,40,83,47,78,41,32,
		63,32,0,20,1,36,65,1,109,39,0,106,2,78,
		0,5,28,37,36,66,1,176,49,0,20,0,36,67,
		1,176,50,0,121,121,176,2,0,12,0,176,3,0,
		12,0,109,4,0,20,5,36,68,1,7,36,70,1,
		109,51,0,83,31,0,36,71,1,109,41,0,83,26,
		0,36,72,1,109,73,0,83,55,0,36,73,1,109,
		74,0,83,75,0,36,74,1,109,76,0,83,77,0,
		36,75,1,109,78,0,83,79,0,36,76,1,176,6,
		0,106,2,54,0,20,1,36,77,1,176,9,0,20,
		0,36,78,1,176,45,0,12,0,32,193,8,36,79,
		1,176,6,0,106,2,56,0,20,1,36,80,1,176,
		29,0,109,12,0,92,5,12,2,83,83,0,36,81,
		1,109,26,0,83,84,0,36,82,1,106,4,48,48,
		48,0,83,85,0,36,83,1,106,3,67,80,0,176,
		86,0,109,85,0,122,92,3,176,87,0,176,88,0,
		109,83,0,12,1,12,1,49,12,3,72,176,88,0,
		109,83,0,12,1,72,176,86,0,109,85,0,122,92,
		3,176,87,0,176,88,0,109,84,0,12,1,12,1,
		49,12,3,72,176,88,0,109,84,0,12,1,72,83,
		89,0,36,84,1,106,3,73,71,0,176,86,0,109,
		85,0,122,92,3,176,87,0,176,88,0,109,83,0,
		12,1,12,1,49,12,3,72,176,88,0,109,83,0,
		12,1,72,176,86,0,109,85,0,122,92,3,176,87,
		0,176,88,0,109,84,0,12,1,12,1,49,12,3,
		72,176,88,0,109,84,0,12,1,72,83,90,0,36,
		85,1,106,3,73,86,0,176,86,0,109,85,0,122,
		92,3,176,87,0,176,88,0,109,83,0,12,1,12,
		1,49,12,3,72,176,88,0,109,83,0,12,1,72,
		176,86,0,109,85,0,122,92,3,176,87,0,176,88,
		0,109,84,0,12,1,12,1,49,12,3,72,176,88,
		0,109,84,0,12,1,72,83,91,0,36,86,1,106,
		3,73,80,0,176,86,0,109,85,0,122,92,3,176,
		87,0,176,88,0,109,83,0,12,1,12,1,49,12,
		3,72,176,88,0,109,83,0,12,1,72,176,86,0,
		109,85,0,122,92,3,176,87,0,176,88,0,109,84,
		0,12,1,12,1,49,12,3,72,176,88,0,109,84,
		0,12,1,72,83,92,0,36,87,1,176,7,0,9,
		100,109,89,0,100,100,9,20,6,176,8,0,109,90,
		0,20,1,176,8,0,109,91,0,20,1,176,8,0,
		109,92,0,20,1,36,88,1,176,9,0,20,0,36,
		90,1,176,27,0,90,57,109,40,0,176,29,0,109,
		12,0,92,5,12,2,5,21,28,39,73,109,41,0,
		109,26,0,5,21,28,28,73,109,51,0,109,31,0,
		5,21,28,17,73,109,73,0,109,55,0,5,21,28,
		6,73,109,93,0,6,106,113,67,76,65,83,83,69,
		32,61,32,83,84,82,40,101,67,76,65,83,95,80,
		82,79,44,53,41,32,46,65,78,68,46,32,81,85,
		65,68,82,65,32,61,32,101,81,85,65,68,82,65,
		32,46,65,78,68,46,32,78,85,77,95,73,77,79,
		86,69,76,32,61,32,101,78,85,77,95,73,77,79,
		86,69,76,32,46,65,78,68,46,32,80,65,82,84,
		69,32,61,32,101,80,65,82,84,69,32,46,65,78,
		68,46,32,65,84,73,86,65,0,20,2,36,91,1,
		176,9,0,20,0,36,92,1,176,45,0,12,0,29,
		35,1,36,93,1,176,32,0,106,3,48,49,0,20,
		1,36,94,1,176,46,0,106,10,65,116,101,110,99,
		97,111,32,33,0,92,3,20,2,36,95,1,176,47,
		0,106,29,78,97,111,32,104,97,32,112,97,114,99,
		101,108,97,115,32,133,32,99,111,114,114,105,103,105,
		114,32,33,0,20,1,36,96,1,176,6,0,106,2,
		54,0,20,1,36,97,1,176,94,0,122,20,1,36,
		98,1,109,51,0,83,31,0,36,99,1,109,41,0,
		83,26,0,36,100,1,109,73,0,83,55,0,36,101,
		1,109,74,0,83,75,0,36,102,1,109,76,0,83,
		77,0,36,103,1,109,78,0,83,79,0,36,104,1,
		176,80,0,92,7,121,92,7,20,3,176,57,0,92,
		7,121,20,2,36,105,1,176,14,0,92,7,92,2,
		20,2,176,15,0,106,12,78,111,46,73,109,111,118,
		101,108,58,32,0,20,1,176,57,0,176,58,0,12,
		0,176,59,0,12,0,122,72,20,2,176,60,0,109,
		61,0,176,62,0,100,106,12,101,78,85,77,95,73,
		77,79,86,69,76,0,100,100,100,12,5,20,2,48,
		64,0,176,65,0,109,61,0,12,1,112,0,73,36,
		106,1,176,95,0,120,20,1,4,0,0,83,61,0,
		109,61,0,73,36,107,1,26,108,252,36,109,1,176,
		6,0,106,2,56,0,20,1,36,110,1,176,9,0,
		20,0,36,111,1,121,83,96,0,36,112,1,176,45,
		0,12,0,31,43,36,113,1,109,97,0,109,82,0,
		15,28,20,109,98,0,121,5,28,13,36,114,1,109,
		96,0,122,72,83,96,0,36,116,1,176,94,0,122,
		20,1,25,207,36,118,1,176,6,0,106,2,56,0,
		20,1,36,119,1,176,9,0,20,0,36,120,1,121,
		83,99,0,36,121,1,122,83,100,0,36,122,1,176,
		45,0,12,0,32,204,3,36,123,1,176,32,0,106,
		3,48,49,0,20,1,36,124,1,176,47,0,106,44,
		69,102,101,116,117,97,110,100,111,32,67,111,114,114,
		101,135,198,111,32,100,97,115,32,80,97,114,99,101,
		108,97,115,32,100,111,32,73,109,162,118,101,108,32,
		40,0,176,88,0,176,29,0,109,12,0,92,5,12,
		2,12,1,72,106,2,47,0,72,176,88,0,109,26,
		0,12,1,72,106,2,47,0,72,176,88,0,109,31,
		0,12,1,72,106,2,47,0,72,176,29,0,109,55,
		0,122,12,2,72,106,15,41,32,80,97,114,99,101,
		108,97,32,78,111,46,32,0,72,109,101,0,72,20,
		1,36,125,1,176,86,0,176,102,0,109,82,0,12,
		1,92,4,92,2,12,3,83,103,0,36,126,1,176,
		86,0,176,102,0,109,82,0,12,1,92,7,92,4,
		12,3,83,104,0,36,127,1,109,105,0,83,106,0,
		36,128,1,109,97,0,83,107,0,36,129,1,176,86,
		0,176,102,0,109,107,0,12,1,92,4,92,2,12,
		3,83,108,0,36,130,1,176,86,0,176,102,0,109,
		107,0,12,1,92,7,92,4,12,3,83,109,0,36,
		131,1,176,86,0,176,102,0,109,81,0,12,1,92,
		4,92,2,12,3,83,110,0,36,132,1,176,86,0,
		176,102,0,109,81,0,12,1,92,7,92,4,12,3,
		83,111,0,36,133,1,176,6,0,106,2,55,0,20,
		1,36,134,1,176,9,0,20,0,36,135,1,176,27,
		0,90,56,109,112,0,109,113,0,72,109,111,0,109,
		110,0,72,16,21,28,37,73,109,112,0,109,113,0,
		72,109,109,0,109,108,0,72,34,21,28,18,73,109,
		112,0,109,113,0,72,109,104,0,109,103,0,72,34,
		6,106,119,40,40,65,78,79,43,77,69,83,32,62,
		61,32,101,65,78,79,95,86,69,78,68,65,43,101,
		77,69,83,95,86,69,78,68,65,41,32,46,65,78,
		68,46,32,40,65,78,79,43,77,69,83,32,60,61,
		32,101,65,78,79,95,80,65,82,67,69,43,101,77,
		69,83,95,80,65,82,67,69,41,41,32,46,65,78,
		68,46,32,40,65,78,79,43,77,69,83,32,60,61,
		32,101,65,78,79,95,76,73,77,73,84,43,101,77,
		69,83,95,76,73,77,73,84,41,0,20,2,36,136,
		1,176,9,0,20,0,36,137,1,122,83,114,0,36,
		138,1,109,112,0,109,113,0,72,109,109,0,109,108,
		0,72,35,28,40,176,45,0,12,0,31,33,36,139,
		1,109,77,0,40,11,92,100,18,122,72,109,114,0,
		65,83,114,0,36,140,1,176,94,0,122,20,1,25,
		200,36,142,1,176,6,0,106,2,56,0,20,1,36,
		143,1,109,97,0,109,81,0,16,29,16,1,109,97,
		0,109,82,0,34,29,6,1,36,144,1,176,6,0,
		106,2,55,0,20,1,36,145,1,176,9,0,20,0,
		36,146,1,176,27,0,90,37,109,112,0,109,113,0,
		72,109,109,0,109,108,0,72,16,21,28,18,73,109,
		112,0,109,113,0,72,109,104,0,109,103,0,72,34,
		6,106,78,40,40,65,78,79,43,77,69,83,32,62,
		61,32,101,65,78,79,95,80,65,82,67,69,43,101,
		77,69,83,95,80,65,82,67,69,41,32,46,65,78,
		68,46,32,40,65,78,79,43,77,69,83,32,60,61,
		32,101,65,78,79,95,76,73,77,73,84,43,101,77,
		69,83,95,76,73,77,73,84,41,41,0,20,2,36,
		147,1,176,9,0,20,0,36,148,1,122,83,115,0,
		36,149,1,109,112,0,109,113,0,72,109,104,0,109,
		103,0,72,35,28,40,176,45,0,12,0,31,33,36,
		150,1,109,77,0,40,11,92,100,18,122,72,109,115,
		0,65,83,115,0,36,151,1,176,94,0,122,20,1,
		25,200,36,153,1,176,6,0,106,2,56,0,20,1,
		36,154,1,109,99,0,109,116,0,109,114,0,65,109,
		116,0,49,109,96,0,18,109,115,0,65,72,83,99,
		0,36,156,1,176,6,0,106,2,56,0,20,1,36,
		157,1,109,97,0,109,82,0,16,28,47,36,158,1,
		176,117,0,12,0,28,37,36,159,1,109,98,0,121,
		5,28,19,36,160,1,109,116,0,109,114,0,65,109,
		99,0,72,78,105,0,36,162,1,176,118,0,20,0,
		36,165,1,176,6,0,106,2,56,0,20,1,36,166,
		1,176,94,0,122,20,1,26,47,252,36,169,1,176,
		6,0,106,2,54,0,20,1,36,170,1,176,117,0,
		12,0,28,19,36,171,1,109,82,0,78,72,0,36,
		172,1,176,118,0,20,0,36,174,1,176,94,0,122,
		20,1,36,175,1,109,51,0,83,31,0,36,176,1,
		109,41,0,83,26,0,36,177,1,109,73,0,83,55,
		0,36,178,1,109,74,0,83,75,0,36,179,1,109,
		76,0,83,77,0,36,180,1,109,78,0,83,79,0,
		36,181,1,176,80,0,92,7,121,92,7,20,3,176,
		57,0,92,7,121,20,2,36,182,1,176,14,0,92,
		7,92,2,20,2,176,15,0,106,12,78,111,46,73,
		109,111,118,101,108,58,32,0,20,1,176,57,0,176,
		58,0,12,0,176,59,0,12,0,122,72,20,2,176,
		60,0,109,61,0,176,62,0,100,106,12,101,78,85,
		77,95,73,77,79,86,69,76,0,100,100,100,12,5,
		20,2,48,64,0,176,65,0,109,61,0,12,1,112,
		0,73,36,183,1,176,95,0,120,20,1,4,0,0,
		83,61,0,109,61,0,73,26,58,247,36,185,1,176,
		32,0,106,3,48,49,0,20,1,36,186,1,176,46,
		0,106,26,80,114,111,99,101,115,115,97,109,101,110,
		116,111,32,67,111,110,99,108,117,161,100,111,32,33,
		0,92,3,20,2,36,187,1,176,49,0,20,0,36,
		188,1,176,50,0,121,121,176,2,0,12,0,176,3,
		0,12,0,109,4,0,20,5,36,189,1,7
	};

	hb_vmExecute( pcode, symbols );
}

