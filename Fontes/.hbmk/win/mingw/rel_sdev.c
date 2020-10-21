/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_sdev.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_SDEV );
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
HB_FUNC_EXTERN( STR );
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
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( F_BLOQA );
HB_FUNC_EXTERN( __DBZAP );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( __DBAPP );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBCREATEINDEX );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( __DBPACK );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_SDEV )
{ "REL_SDEV", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_SDEV )}, NULL },
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
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
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
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ETIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "ETIPOREL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "EARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__DBAPP", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBAPP )}, NULL },
{ "DATA_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ATIVA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CONTABIL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DBCREATEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCREATEINDEX )}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONVERT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOTAIS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESAIDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "__DBPACK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBPACK )}, NULL },
{ "ECONTRATO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_SD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_PA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_SD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_PA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "DATA_VENDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VLR_PARC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "METRAGEM_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "METRAGEM_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_SDEV, "C:/sis/SAN/xhb/rel_sdev.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_SDEV
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_SDEV )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_SDEV )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,27,82,69,76,65,84,224,82,73,79,32,68,
		69,32,83,65,76,68,79,32,68,69,86,69,68,79,
		82,0,20,1,36,16,0,176,6,0,106,2,49,0,
		20,1,36,17,0,176,7,0,9,100,106,9,85,78,
		73,95,73,77,79,66,0,100,100,9,20,6,176,8,
		0,106,9,85,78,73,95,73,77,79,66,0,20,1,
		36,18,0,176,9,0,20,0,36,19,0,176,6,0,
		106,2,50,0,20,1,36,20,0,176,7,0,9,100,
		106,9,67,76,65,83,95,80,82,79,0,100,100,9,
		20,6,176,8,0,106,9,67,76,65,83,95,80,82,
		79,0,20,1,36,21,0,176,9,0,20,0,36,22,
		0,176,6,0,106,2,51,0,20,1,36,23,0,176,
		7,0,9,100,106,9,67,65,68,95,81,85,65,68,
		0,100,100,9,20,6,176,8,0,106,9,67,65,68,
		95,81,85,65,68,0,20,1,36,24,0,176,9,0,
		20,0,36,25,0,176,6,0,106,2,52,0,20,1,
		36,26,0,176,7,0,9,100,106,9,67,65,68,95,
		65,80,65,82,0,100,100,9,20,6,176,8,0,106,
		9,67,65,68,95,65,80,65,82,0,20,1,36,27,
		0,176,9,0,20,0,36,28,0,176,6,0,106,2,
		53,0,20,1,36,29,0,176,7,0,9,100,106,8,
		67,79,78,84,65,84,79,0,100,100,9,20,6,176,
		8,0,106,9,78,85,77,95,67,76,73,69,0,20,
		1,176,8,0,106,8,67,79,78,84,65,84,79,0,
		20,1,36,30,0,176,9,0,20,0,36,31,0,176,
		6,0,106,2,54,0,20,1,36,32,0,176,7,0,
		9,100,106,9,67,65,68,95,86,69,78,68,0,100,
		100,9,20,6,176,8,0,106,9,67,65,68,95,86,
		69,78,68,0,20,1,176,8,0,106,9,68,65,84,
		65,95,86,69,78,0,20,1,36,33,0,176,9,0,
		20,0,36,34,0,176,6,0,106,2,56,0,20,1,
		36,36,0,176,6,0,106,2,57,0,20,1,36,37,
		0,176,7,0,9,100,106,9,77,83,71,95,66,79,
		76,69,0,100,100,9,20,6,176,8,0,106,9,77,
		83,71,95,80,82,79,68,0,20,1,176,8,0,106,
		9,77,83,71,95,66,79,76,69,0,20,1,36,38,
		0,176,9,0,20,0,36,39,0,176,6,0,106,2,
		50,0,20,1,36,42,0,176,10,0,20,0,36,43,
		0,176,11,0,106,9,87,47,78,44,43,87,47,82,
		0,20,1,36,44,0,121,83,12,0,36,45,0,121,
		83,13,0,36,46,0,176,14,0,92,5,92,2,20,
		2,176,15,0,106,11,80,114,111,100,117,116,111,32,
		58,32,0,20,1,36,47,0,176,9,0,20,0,36,
		48,0,92,3,3,1,0,92,3,3,1,0,92,3,
		3,1,0,92,3,3,1,0,176,20,0,108,16,108,
		17,108,18,108,19,20,4,81,19,0,81,18,0,81,
		17,0,81,16,0,36,49,0,176,21,0,109,16,0,
		120,109,19,0,20,3,36,50,0,106,8,64,82,57,
		57,57,57,57,0,98,17,0,122,2,36,51,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,92,2,
		2,36,52,0,106,5,64,83,52,48,0,98,17,0,
		92,3,2,36,53,0,106,5,32,32,32,32,0,98,
		18,0,122,2,36,54,0,106,5,32,32,32,32,0,
		98,18,0,92,2,2,36,55,0,106,5,32,32,32,
		32,0,98,18,0,92,3,2,36,56,0,176,22,0,
		92,5,92,15,92,5,92,70,109,16,0,106,10,70,
		85,78,67,95,67,79,78,83,0,109,17,0,109,18,
		0,106,2,32,0,106,4,32,45,32,0,106,2,32,
		0,20,11,36,57,0,109,23,0,83,12,0,36,58,
		0,109,24,0,83,13,0,36,59,0,109,25,0,83,
		26,0,36,62,0,176,27,0,109,12,0,92,5,12,
		2,83,12,0,36,63,0,134,0,0,0,0,83,28,
		0,36,64,0,134,0,0,0,0,83,29,0,36,65,
		0,176,30,0,12,0,83,28,0,36,66,0,176,31,
		0,106,11,51,49,47,49,50,47,50,57,57,57,0,
		12,1,83,29,0,36,68,0,176,14,0,92,8,92,
		2,20,2,176,15,0,106,11,80,101,114,105,111,100,
		111,32,58,32,0,20,1,176,32,0,176,33,0,12,
		0,176,34,0,12,0,122,72,20,2,176,35,0,109,
		36,0,176,37,0,100,106,10,101,68,65,84,65,95,
		73,78,73,0,100,100,100,12,5,20,2,48,38,0,
		176,39,0,109,36,0,12,1,112,0,73,36,69,0,
		176,14,0,92,8,92,25,20,2,176,15,0,106,7,
		65,116,101,32,58,32,0,20,1,176,32,0,176,33,
		0,12,0,176,34,0,12,0,122,72,20,2,176,35,
		0,109,36,0,176,37,0,100,106,10,101,68,65,84,
		65,95,70,73,77,0,100,100,100,12,5,20,2,48,
		38,0,176,39,0,109,36,0,12,1,112,0,73,36,
		70,0,176,40,0,109,36,0,100,100,100,100,100,100,
		20,7,4,0,0,83,36,0,109,36,0,73,36,71,
		0,109,28,0,134,0,0,0,0,5,31,13,109,29,
		0,134,0,0,0,0,5,28,51,36,72,0,176,41,
		0,106,3,48,49,0,20,1,36,73,0,176,42,0,
		106,18,68,97,116,97,115,32,73,110,118,160,108,105,
		100,97,115,32,33,0,92,3,20,2,36,74,0,26,
		238,254,36,76,0,109,28,0,109,29,0,15,28,59,
		36,77,0,176,41,0,106,3,48,49,0,20,1,36,
		78,0,176,42,0,106,26,68,97,116,97,32,73,110,
		105,99,105,111,32,62,32,81,117,101,32,70,105,110,
		97,108,32,33,0,92,3,20,2,36,79,0,26,169,
		254,36,85,0,176,41,0,106,3,48,49,0,20,1,
		36,86,0,176,43,0,106,59,68,101,115,101,106,97,
		32,69,102,101,116,117,97,114,32,97,32,67,111,110,
		115,111,108,105,100,97,135,198,111,32,100,111,32,65,
		114,113,117,105,118,111,32,100,101,32,83,97,108,100,
		111,32,40,83,47,78,41,32,63,32,0,20,1,36,
		87,0,109,44,0,106,2,83,0,5,29,101,9,36,
		88,0,176,45,0,122,12,1,83,46,0,36,89,0,
		176,41,0,106,3,48,49,0,20,1,36,90,0,176,
		14,0,92,23,121,20,2,176,15,0,106,51,82,101,
		108,97,116,135,114,105,111,32,40,71,41,101,114,97,
		108,32,111,117,32,97,112,101,110,97,115,32,40,80,
		41,101,114,105,111,100,111,32,73,110,102,111,114,109,
		97,100,111,32,63,32,0,20,1,176,32,0,176,33,
		0,12,0,176,34,0,12,0,122,72,20,2,176,35,
		0,109,36,0,176,37,0,100,106,6,101,84,73,80,
		79,0,106,4,64,33,88,0,90,12,109,46,0,106,
		3,71,80,0,24,6,100,12,5,20,2,48,38,0,
		176,39,0,109,36,0,12,1,112,0,73,36,91,0,
		176,40,0,109,36,0,100,100,100,100,100,100,20,7,
		4,0,0,83,36,0,109,36,0,73,36,92,0,176,
		41,0,106,3,48,49,0,20,1,36,93,0,176,42,
		0,106,10,65,116,101,110,135,198,111,32,33,0,92,
		3,20,2,36,94,0,176,47,0,106,56,65,103,117,
		97,114,100,101,32,97,32,109,111,110,116,97,103,101,
		109,32,100,111,32,66,97,110,99,111,32,100,101,32,
		68,97,100,111,115,32,112,97,114,97,32,73,109,112,
		114,101,115,115,198,111,32,46,46,46,0,20,1,36,
		95,0,109,13,0,122,5,29,73,5,36,96,0,176,
		45,0,122,12,1,83,48,0,36,97,0,176,41,0,
		106,3,48,49,0,20,1,36,98,0,176,14,0,92,
		23,121,20,2,176,15,0,106,50,82,101,108,97,116,
		135,114,105,111,32,40,84,41,111,100,111,115,44,32,
		40,73,41,110,116,101,105,114,111,115,32,111,117,32,
		40,77,41,101,105,111,115,32,76,111,116,101,115,32,
		63,32,0,20,1,176,32,0,176,33,0,12,0,176,
		34,0,12,0,122,72,20,2,176,35,0,109,36,0,
		176,37,0,100,106,9,101,84,73,80,79,82,69,76,
		0,106,4,64,33,88,0,90,13,109,48,0,106,4,
		84,73,77,0,24,6,100,12,5,20,2,48,38,0,
		176,39,0,109,36,0,12,1,112,0,73,36,99,0,
		176,40,0,109,36,0,100,100,100,100,100,100,20,7,
		4,0,0,83,36,0,109,36,0,73,36,100,0,176,
		6,0,106,2,56,0,20,1,36,101,0,176,49,0,
		92,8,106,3,79,78,0,20,2,36,102,0,176,7,
		0,9,100,106,9,84,69,77,80,95,80,65,82,0,
		100,100,9,20,6,36,103,0,176,50,0,12,0,28,
		18,36,104,0,176,51,0,20,0,36,105,0,176,52,
		0,20,0,36,107,0,176,49,0,92,8,106,4,79,
		70,70,0,20,2,36,108,0,176,6,0,106,2,51,
		0,20,1,36,109,0,176,53,0,90,17,109,54,0,
		109,12,0,5,21,28,6,73,109,55,0,6,106,38,
		78,85,77,95,76,79,84,69,65,77,32,61,32,101,
		67,76,65,83,95,80,82,79,32,46,65,78,68,46,
		32,67,65,68,65,83,84,82,79,0,20,2,36,110,
		0,176,9,0,20,0,36,111,0,176,56,0,12,0,
		32,246,2,36,112,0,109,12,0,83,57,0,36,113,
		0,109,58,0,83,59,0,36,114,0,176,6,0,106,
		2,56,0,20,1,36,115,0,176,7,0,9,100,106,
		9,84,69,77,80,95,80,65,82,0,100,100,9,20,
		6,36,116,0,176,9,0,20,0,36,117,0,106,4,
		48,48,48,0,83,60,0,36,118,0,106,3,67,80,
		0,176,61,0,109,60,0,122,92,3,176,62,0,176,
		63,0,109,57,0,12,1,12,1,49,12,3,72,176,
		63,0,109,57,0,12,1,72,176,61,0,109,60,0,
		122,92,3,176,62,0,176,63,0,109,59,0,12,1,
		12,1,49,12,3,72,176,63,0,109,59,0,12,1,
		72,83,64,0,36,119,0,109,48,0,106,2,84,0,
		5,29,173,0,36,120,0,109,46,0,106,2,71,0,
		5,28,73,36,121,0,176,65,0,109,64,0,4,0,
		0,90,48,109,66,0,109,28,0,16,21,28,15,73,
		109,67,0,134,0,0,0,0,5,21,31,13,73,109,
		67,0,109,29,0,15,21,28,13,73,109,68,0,21,
		28,6,73,109,69,0,6,100,100,100,9,100,100,100,
		20,10,25,82,36,123,0,176,65,0,109,64,0,4,
		0,0,90,59,109,66,0,109,28,0,16,21,28,15,
		73,109,67,0,134,0,0,0,0,5,21,31,13,73,
		109,67,0,109,29,0,15,21,28,24,73,109,68,0,
		21,28,17,73,109,69,0,21,28,10,73,109,66,0,
		109,29,0,34,6,100,100,100,9,100,100,100,20,10,
		36,124,0,26,134,1,36,126,0,109,48,0,106,2,
		73,0,5,29,191,0,36,127,0,109,46,0,106,2,
		71,0,5,28,82,36,128,0,176,65,0,109,64,0,
		4,0,0,90,57,109,66,0,109,28,0,16,21,28,
		15,73,109,67,0,134,0,0,0,0,5,21,31,13,
		73,109,67,0,109,29,0,15,21,28,22,73,109,68,
		0,21,28,15,73,109,69,0,21,28,8,73,109,70,
		0,121,5,6,100,100,100,9,100,100,100,20,10,25,
		91,36,130,0,176,65,0,109,64,0,4,0,0,90,
		68,109,66,0,109,28,0,16,21,28,15,73,109,67,
		0,134,0,0,0,0,5,21,31,13,73,109,67,0,
		109,29,0,15,21,28,33,73,109,68,0,21,28,26,
		73,109,69,0,21,28,19,73,109,66,0,109,29,0,
		34,21,28,8,73,109,70,0,121,5,6,100,100,100,
		9,100,100,100,20,10,36,131,0,26,188,0,36,133,
		0,109,46,0,106,2,71,0,5,28,82,36,134,0,
		176,65,0,109,64,0,4,0,0,90,57,109,66,0,
		109,28,0,16,21,28,15,73,109,67,0,134,0,0,
		0,0,5,21,31,13,73,109,67,0,109,29,0,15,
		21,28,22,73,109,68,0,21,28,15,73,109,69,0,
		21,28,8,73,109,70,0,121,69,6,100,100,100,9,
		100,100,100,20,10,25,91,36,136,0,176,65,0,109,
		64,0,4,0,0,90,68,109,66,0,109,28,0,16,
		21,28,15,73,109,67,0,134,0,0,0,0,5,21,
		31,13,73,109,67,0,109,29,0,15,21,28,33,73,
		109,68,0,21,28,26,73,109,69,0,21,28,19,73,
		109,66,0,109,29,0,34,21,28,8,73,109,70,0,
		121,69,6,100,100,100,9,100,100,100,20,10,36,140,
		0,176,9,0,20,0,36,141,0,176,6,0,106,2,
		51,0,20,1,36,142,0,176,71,0,122,20,1,26,
		5,253,36,144,0,176,49,0,92,8,106,3,79,78,
		0,20,2,36,145,0,176,6,0,106,2,56,0,20,
		1,36,146,0,176,7,0,9,100,106,9,84,69,77,
		80,95,80,65,82,0,100,100,9,20,6,36,147,0,
		176,72,0,106,9,84,69,77,80,95,80,65,82,0,
		106,38,67,76,65,83,83,69,43,81,85,65,68,82,
		65,43,78,85,77,95,73,77,79,86,69,76,43,83,
		84,82,40,80,65,82,84,69,44,49,41,0,90,24,
		109,73,0,109,74,0,72,109,75,0,72,176,27,0,
		109,70,0,122,12,2,72,6,100,20,4,36,148,0,
		176,9,0,20,0,36,149,0,176,7,0,9,100,106,
		9,84,69,77,80,95,80,65,82,0,100,100,9,20,
		6,176,8,0,106,9,84,69,77,80,95,80,65,82,
		0,20,1,36,150,0,176,9,0,20,0,36,151,0,
		176,49,0,92,8,106,4,79,70,70,0,20,2,26,
		238,2,36,153,0,176,6,0,106,2,56,0,20,1,
		36,154,0,176,49,0,92,8,106,3,79,78,0,20,
		2,36,155,0,176,7,0,9,100,106,9,84,69,77,
		80,95,80,65,82,0,100,100,9,20,6,36,156,0,
		176,50,0,12,0,28,18,36,157,0,176,51,0,20,
		0,36,158,0,176,52,0,20,0,36,160,0,176,49,
		0,92,8,106,4,79,70,70,0,20,2,36,161,0,
		176,6,0,106,2,52,0,20,1,36,162,0,176,53,
		0,90,17,109,54,0,109,12,0,5,21,28,6,73,
		109,55,0,6,106,38,78,85,77,95,76,79,84,69,
		65,77,32,61,32,101,67,76,65,83,95,80,82,79,
		32,46,65,78,68,46,32,67,65,68,65,83,84,82,
		79,0,20,2,36,163,0,176,9,0,20,0,36,164,
		0,176,56,0,12,0,32,98,1,36,165,0,109,12,
		0,83,57,0,36,166,0,109,76,0,83,59,0,36,
		167,0,176,6,0,106,2,56,0,20,1,36,168,0,
		176,7,0,9,100,106,9,84,69,77,80,95,80,65,
		82,0,100,100,9,20,6,36,169,0,176,9,0,20,
		0,36,170,0,106,4,48,48,48,0,83,60,0,36,
		171,0,106,3,67,80,0,176,61,0,109,60,0,122,
		92,3,176,62,0,176,63,0,109,57,0,12,1,12,
		1,49,12,3,72,176,63,0,109,57,0,12,1,72,
		176,61,0,109,60,0,122,92,3,176,62,0,176,63,
		0,109,59,0,12,1,12,1,49,12,3,72,176,63,
		0,109,59,0,12,1,72,83,64,0,36,172,0,109,
		46,0,106,2,71,0,5,28,73,36,173,0,176,65,
		0,109,64,0,4,0,0,90,48,109,66,0,109,28,
		0,16,21,28,15,73,109,67,0,134,0,0,0,0,
		5,21,31,13,73,109,67,0,109,29,0,15,21,28,
		13,73,109,68,0,21,28,6,73,109,69,0,6,100,
		100,100,9,100,100,100,20,10,25,82,36,175,0,176,
		65,0,109,64,0,4,0,0,90,59,109,66,0,109,
		28,0,16,21,28,15,73,109,67,0,134,0,0,0,
		0,5,21,31,13,73,109,67,0,109,29,0,15,21,
		28,24,73,109,68,0,21,28,17,73,109,69,0,21,
		28,10,73,109,66,0,109,29,0,34,6,100,100,100,
		9,100,100,100,20,10,36,177,0,176,9,0,20,0,
		36,178,0,176,6,0,106,2,52,0,20,1,36,179,
		0,176,71,0,122,20,1,26,153,254,36,181,0,176,
		49,0,92,8,106,3,79,78,0,20,2,36,182,0,
		176,6,0,106,2,56,0,20,1,36,183,0,176,7,
		0,9,100,106,9,84,69,77,80,95,80,65,82,0,
		100,100,9,20,6,36,184,0,176,72,0,106,9,84,
		69,77,80,95,80,65,82,0,106,38,67,76,65,83,
		83,69,43,81,85,65,68,82,65,43,78,85,77,95,
		73,77,79,86,69,76,43,83,84,82,40,80,65,82,
		84,69,44,49,41,0,90,24,109,73,0,109,74,0,
		72,109,75,0,72,176,27,0,109,70,0,122,12,2,
		72,6,100,20,4,36,185,0,176,9,0,20,0,36,
		186,0,176,7,0,9,100,106,9,84,69,77,80,95,
		80,65,82,0,100,100,9,20,6,176,8,0,106,9,
		84,69,77,80,95,80,65,82,0,20,1,36,187,0,
		176,9,0,20,0,36,188,0,176,49,0,92,8,106,
		4,79,70,70,0,20,2,36,191,0,9,83,77,0,
		36,192,0,176,41,0,106,3,48,49,0,20,1,36,
		193,0,176,43,0,106,63,67,111,110,118,101,114,116,
		101,114,32,83,97,108,100,111,115,32,65,110,116,101,
		114,105,111,114,101,115,32,97,32,48,49,47,48,55,
		47,49,57,57,52,32,101,109,32,82,101,97,108,32,
		40,82,36,41,32,40,83,47,78,41,32,63,32,0,
		20,1,36,194,0,109,44,0,106,2,83,0,5,28,
		9,36,195,0,120,83,77,0,36,197,0,9,83,78,
		0,36,198,0,176,41,0,106,3,48,49,0,20,1,
		36,199,0,176,43,0,106,44,68,101,115,101,106,97,
		32,82,101,108,97,116,111,114,105,111,32,97,112,101,
		110,97,115,32,99,111,109,32,84,111,116,97,105,115,
		32,40,83,47,78,41,32,63,32,0,20,1,36,200,
		0,109,44,0,106,2,83,0,5,28,9,36,201,0,
		120,83,78,0,36,203,0,176,61,0,109,12,0,92,
		5,12,2,83,12,0,36,204,0,176,45,0,92,60,
		12,1,83,79,0,36,205,0,176,63,0,109,80,0,
		12,1,176,45,0,122,12,1,69,28,18,36,206,0,
		109,80,0,106,2,58,0,72,83,79,0,25,13,36,
		208,0,106,3,67,58,0,83,79,0,36,211,0,109,
		81,0,176,45,0,92,30,12,1,69,28,50,36,212,
		0,176,63,0,109,79,0,12,1,109,81,0,72,106,
		17,83,65,76,68,79,68,69,86,69,68,79,82,46,
		84,88,84,0,72,176,45,0,92,10,12,1,72,83,
		79,0,25,44,36,214,0,176,63,0,109,79,0,12,
		1,106,17,83,65,76,68,79,68,69,86,69,68,79,
		82,46,84,88,84,0,72,176,45,0,92,10,12,1,
		72,83,79,0,36,216,0,176,45,0,92,10,12,1,
		83,82,0,36,218,0,176,41,0,106,3,48,49,0,
		20,1,36,219,0,176,43,0,106,55,68,101,115,101,
		106,97,32,101,120,116,114,97,116,111,32,101,109,32,
		97,114,113,117,105,118,111,32,116,101,120,116,111,32,
		110,111,32,68,114,105,118,101,32,40,88,58,41,32,
		40,83,47,78,41,32,63,32,0,20,1,36,220,0,
		109,44,0,106,2,83,0,5,29,255,0,36,221,0,
		176,41,0,106,3,48,49,0,20,1,36,222,0,176,
		14,0,92,23,121,20,2,176,15,0,106,23,67,79,
		78,70,73,82,77,69,32,79,32,68,69,83,84,73,
		78,79,32,32,58,32,0,20,1,176,32,0,176,33,
		0,12,0,176,34,0,12,0,122,72,20,2,176,35,
		0,109,36,0,176,37,0,100,106,9,101,65,82,81,
		85,73,86,79,0,100,100,100,12,5,20,2,48,38,
		0,176,39,0,109,36,0,12,1,112,0,73,36,223,
		0,176,40,0,109,36,0,100,100,100,100,100,100,20,
		7,4,0,0,83,36,0,109,36,0,73,36,224,0,
		176,63,0,109,79,0,12,1,83,79,0,36,225,0,
		176,41,0,106,3,48,49,0,20,1,36,226,0,176,
		47,0,106,46,73,110,115,105,114,97,32,117,109,32,
		100,105,115,99,111,32,110,111,32,100,114,105,118,101,
		32,40,88,58,41,44,32,101,32,116,101,99,108,101,
		32,69,78,84,69,82,46,0,20,1,36,227,0,176,
		83,0,20,0,36,228,0,106,8,65,82,81,85,73,
		86,79,0,83,82,0,36,229,0,25,97,36,231,0,
		176,47,0,106,50,80,111,115,105,99,105,111,110,101,
		32,111,32,70,111,114,109,117,108,97,114,105,111,32,
		56,48,32,67,111,108,117,110,97,115,32,101,32,84,
		101,99,108,101,32,91,69,78,84,69,82,93,0,20,
		1,36,232,0,176,83,0,20,0,36,233,0,176,84,
		0,20,0,36,234,0,106,11,73,77,80,82,69,83,
		83,79,82,65,0,83,82,0,36,238,0,176,6,0,
		106,2,56,0,20,1,36,239,0,176,50,0,12,0,
		28,18,36,240,0,176,85,0,20,0,36,241,0,176,
		52,0,20,0,36,243,0,176,9,0,20,0,36,244,
		0,176,56,0,12,0,28,91,36,245,0,176,41,0,
		106,3,48,49,0,20,1,36,246,0,176,42,0,106,
		10,65,116,101,110,135,198,111,32,33,0,92,3,20,
		2,36,247,0,176,47,0,106,33,78,198,111,32,72,
		160,32,68,101,118,101,100,111,114,101,115,32,78,101,
		115,116,101,32,80,101,114,161,111,100,111,32,33,0,
		20,1,36,248,0,176,83,0,20,0,26,54,8,36,
		250,0,121,83,86,0,36,251,0,109,82,0,106,8,
		65,82,81,85,73,86,79,0,5,28,53,36,252,0,
		176,49,0,92,23,106,3,79,78,0,20,2,36,253,
		0,176,49,0,92,20,106,8,80,82,73,78,84,69,
		82,0,20,2,36,254,0,176,49,0,92,24,109,79,
		0,9,20,3,25,98,36,0,1,176,47,0,106,50,
		80,111,115,105,99,105,111,110,101,32,111,32,70,111,
		114,109,117,108,97,114,105,111,32,56,48,32,67,111,
		108,117,110,97,115,32,101,32,84,101,99,108,101,32,
		91,69,78,84,69,82,93,0,20,1,36,1,1,176,
		83,0,20,0,36,2,1,176,84,0,20,0,36,3,
		1,176,49,0,92,20,106,8,80,82,73,78,84,69,
		82,0,20,2,36,5,1,121,83,87,0,36,6,1,
		121,83,88,0,36,7,1,121,83,89,0,36,8,1,
		121,83,90,0,36,9,1,176,56,0,12,0,32,95,
		7,36,10,1,176,14,0,122,121,20,2,176,15,0,
		109,91,0,106,11,69,109,112,114,101,115,97,32,58,
		32,0,72,109,92,0,72,20,1,36,11,1,176,14,
		0,122,92,120,20,2,176,15,0,106,8,68,97,116,
		97,32,58,32,0,176,93,0,176,30,0,12,0,12,
		1,72,20,1,36,12,1,176,14,0,92,2,121,20,
		2,176,15,0,106,11,83,105,115,116,101,109,97,32,
		58,32,0,109,94,0,72,20,1,36,13,1,176,14,
		0,92,2,92,121,20,2,176,15,0,106,14,82,101,
		108,97,116,58,32,83,65,78,48,48,52,0,20,1,
		36,14,1,176,14,0,92,3,92,20,20,2,176,15,
		0,109,95,0,106,21,83,65,76,68,79,83,32,68,
		69,86,69,68,79,82,69,83,32,68,69,32,0,72,
		176,93,0,109,28,0,12,1,72,106,6,32,65,84,
		69,32,0,72,176,93,0,109,29,0,12,1,72,109,
		91,0,72,20,1,36,15,1,176,14,0,92,4,121,
		20,2,176,15,0,106,11,80,114,111,100,117,116,111,
		32,58,32,0,109,12,0,72,106,4,32,45,32,0,
		72,176,63,0,109,26,0,12,1,72,20,1,36,16,
		1,176,14,0,92,4,92,100,20,2,176,15,0,106,
		8,84,105,112,111,32,58,32,0,109,46,0,106,2,
		71,0,5,28,12,106,6,71,101,114,97,108,0,25,
		12,106,8,80,101,114,105,111,100,111,0,72,20,1,
		36,17,1,176,14,0,92,5,121,20,2,176,15,0,
		176,96,0,106,2,61,0,93,135,0,12,2,20,1,
		36,18,1,176,14,0,92,6,92,2,20,2,176,15,
		0,106,11,78,111,46,32,73,77,79,86,69,76,0,
		20,1,36,19,1,176,14,0,92,6,92,15,20,2,
		176,15,0,106,13,78,79,77,69,32,67,76,73,69,
		78,84,69,0,20,1,36,20,1,176,14,0,92,6,
		92,70,20,2,176,15,0,106,12,83,68,46,32,68,
		69,86,69,68,79,82,0,20,1,36,21,1,176,14,
		0,92,6,92,95,20,2,176,15,0,106,9,80,65,
		82,67,69,76,65,83,0,20,1,36,22,1,176,14,
		0,92,6,92,115,20,2,176,15,0,106,3,77,50,
		0,20,1,36,23,1,176,14,0,92,6,92,122,20,
		2,176,15,0,106,10,67,79,78,84,82,65,84,79,
		83,0,20,1,36,24,1,176,14,0,92,7,121,20,
		2,176,15,0,176,96,0,106,2,61,0,93,135,0,
		12,2,20,1,36,25,1,92,8,83,97,0,36,26,
		1,176,56,0,12,0,32,36,4,109,97,0,92,56,
		34,29,27,4,36,27,1,176,6,0,106,2,56,0,
		20,1,36,28,1,109,13,0,122,5,28,40,36,29,
		1,109,73,0,83,12,0,36,30,1,109,74,0,83,
		98,0,36,31,1,109,75,0,83,99,0,36,32,1,
		109,70,0,83,100,0,25,38,36,34,1,109,73,0,
		83,12,0,36,35,1,109,74,0,83,98,0,36,36,
		1,109,75,0,83,99,0,36,37,1,109,70,0,83,
		100,0,36,39,1,176,6,0,106,2,54,0,20,1,
		36,40,1,176,101,0,109,12,0,109,98,0,72,109,
		99,0,72,176,27,0,109,100,0,122,12,2,72,100,
		100,20,3,36,42,1,109,102,0,109,29,0,16,28,
		76,36,43,1,176,6,0,106,2,56,0,20,1,36,
		44,1,109,73,0,109,12,0,5,29,63,255,109,98,
		0,109,74,0,5,29,53,255,109,99,0,109,75,0,
		5,29,43,255,109,100,0,109,70,0,5,29,33,255,
		176,56,0,12,0,32,25,255,36,45,1,176,71,0,
		122,20,1,25,196,36,49,1,109,103,0,83,104,0,
		36,50,1,176,6,0,106,2,53,0,20,1,36,51,
		1,176,101,0,109,104,0,100,100,20,3,36,52,1,
		176,6,0,106,2,56,0,20,1,36,53,1,121,83,
		87,0,36,54,1,121,83,88,0,36,55,1,109,73,
		0,109,12,0,5,29,196,0,109,98,0,109,74,0,
		5,29,186,0,109,99,0,109,75,0,5,29,176,0,
		109,100,0,109,70,0,5,29,166,0,176,56,0,12,
		0,32,158,0,36,56,1,109,68,0,29,137,0,36,
		57,1,109,66,0,176,31,0,106,11,48,49,47,48,
		55,47,49,57,57,52,0,12,1,35,28,57,109,77,
		0,28,52,36,58,1,109,67,0,134,0,0,0,0,
		5,31,16,109,67,0,109,29,0,15,28,24,109,68,
		0,28,19,36,59,1,109,87,0,109,105,0,93,190,
		10,18,72,83,87,0,36,60,1,25,46,36,62,1,
		109,67,0,134,0,0,0,0,5,31,16,109,67,0,
		109,29,0,15,28,20,109,68,0,28,15,36,63,1,
		109,87,0,109,105,0,72,83,87,0,36,66,1,109,
		88,0,122,72,83,88,0,36,68,1,176,71,0,122,
		20,1,26,53,255,36,70,1,109,78,0,32,110,1,
		36,71,1,176,14,0,109,97,0,92,2,20,2,176,
		15,0,176,27,0,109,100,0,122,12,2,106,2,47,
		0,72,176,63,0,109,98,0,12,1,72,106,2,47,
		0,72,176,63,0,109,99,0,12,1,72,20,1,36,
		72,1,176,6,0,106,2,53,0,20,1,36,73,1,
		176,14,0,109,97,0,92,15,20,2,176,15,0,109,
		106,0,20,1,36,74,1,176,14,0,109,97,0,92,
		70,20,2,176,107,0,109,87,0,106,22,64,82,32,
		57,57,57,44,57,57,57,44,57,57,57,44,57,57,
		57,46,57,57,0,20,2,36,75,1,176,14,0,109,
		97,0,92,95,20,2,176,107,0,109,88,0,106,11,
		64,82,32,57,57,57,44,57,57,57,0,20,2,36,
		76,1,109,86,0,122,72,83,86,0,36,77,1,176,
		6,0,106,2,49,0,20,1,36,78,1,176,9,0,
		20,0,36,79,1,176,101,0,109,12,0,109,98,0,
		72,109,99,0,72,100,100,20,3,36,80,1,176,108,
		0,12,0,28,96,36,81,1,109,100,0,121,5,31,
		9,109,100,0,122,5,28,39,36,82,1,176,14,0,
		109,97,0,92,110,20,2,176,107,0,109,109,0,106,
		12,64,82,32,57,44,57,57,57,46,57,57,0,20,
		2,25,71,36,84,1,176,14,0,109,97,0,92,110,
		20,2,176,107,0,109,110,0,106,12,64,82,32,57,
		44,57,57,57,46,57,57,0,20,2,36,85,1,25,
		31,36,87,1,176,14,0,109,97,0,92,110,20,2,
		176,15,0,106,9,60,60,69,82,82,79,62,62,0,
		20,1,36,89,1,109,97,0,122,72,83,97,0,36,
		91,1,176,6,0,106,2,56,0,20,1,36,92,1,
		109,89,0,109,87,0,72,83,89,0,36,93,1,109,
		90,0,109,88,0,72,83,90,0,36,94,1,176,111,
		0,20,0,36,95,1,109,112,0,29,89,252,36,96,
		1,109,82,0,106,8,65,82,81,85,73,86,79,0,
		5,28,52,36,97,1,176,49,0,92,24,106,1,0,
		20,2,36,98,1,176,49,0,92,23,106,4,79,70,
		70,0,20,2,36,99,1,176,49,0,92,20,106,7,
		83,67,82,69,69,78,0,20,2,25,34,36,101,1,
		176,49,0,92,24,106,1,0,20,2,36,102,1,176,
		49,0,92,20,106,7,83,67,82,69,69,78,0,20,
		2,36,104,1,176,113,0,20,0,176,6,0,106,2,
		49,0,20,1,176,114,0,100,20,1,36,105,1,100,
		110,7,36,108,1,176,56,0,12,0,29,165,0,36,
		109,1,109,97,0,122,72,83,97,0,36,110,1,176,
		14,0,109,97,0,92,2,20,2,176,15,0,106,18,
		84,111,116,97,108,32,71,101,114,97,108,32,32,32,
		32,58,32,0,20,1,36,111,1,176,14,0,109,97,
		0,92,70,20,2,176,107,0,109,89,0,106,22,64,
		82,32,57,57,57,44,57,57,57,44,57,57,57,44,
		57,57,57,46,57,57,0,20,2,36,112,1,176,14,
		0,109,97,0,92,95,20,2,176,107,0,109,90,0,
		106,11,64,82,32,57,57,57,44,57,57,57,0,20,
		2,36,113,1,176,14,0,109,97,0,92,122,20,2,
		176,107,0,109,86,0,106,11,64,82,32,57,57,57,
		44,57,57,57,0,20,2,36,115,1,176,6,0,106,
		2,56,0,20,1,36,116,1,176,14,0,92,58,121,
		20,2,176,15,0,176,96,0,106,2,61,0,93,135,
		0,12,2,20,1,36,117,1,176,14,0,92,59,92,
		104,20,2,176,15,0,109,115,0,20,1,36,118,1,
		176,14,0,122,121,20,2,176,15,0,106,2,32,0,
		109,116,0,72,20,1,26,156,248,36,121,1,109,82,
		0,106,8,65,82,81,85,73,86,79,0,5,28,52,
		36,122,1,176,49,0,92,24,106,1,0,20,2,36,
		123,1,176,49,0,92,23,106,4,79,70,70,0,20,
		2,36,124,1,176,49,0,92,20,106,7,83,67,82,
		69,69,78,0,20,2,25,34,36,126,1,176,49,0,
		92,24,106,1,0,20,2,36,127,1,176,49,0,92,
		20,106,7,83,67,82,69,69,78,0,20,2,36,129,
		1,176,113,0,20,0,36,130,1,176,117,0,121,121,
		176,2,0,12,0,176,3,0,12,0,109,4,0,20,
		5,36,131,1,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

