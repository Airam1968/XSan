/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_ctpb.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_CTPB );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_CTPB )
{ "REL_CTPB", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_CTPB )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "ESALDO_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ERELATORIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "EDATA_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "DATA_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REALIZADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "ESAIDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "EDATA_AUX", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_PRE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_PRE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "SISLETRAC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "LANCAMENTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMPREENDIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CODIGO_FOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE_D", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLASSE_D", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECODIGO_FOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NOME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_PREV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PREVISTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_DOCUME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "APELIDO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_CHEQUE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_CTPB, "C:/sis/SAN/xhb/rel_ctpb.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_CTPB
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_CTPB )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_CTPB )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,47,82,69,76,65,84,224,82,73,79,32,68,
		69,32,67,79,78,84,65,83,32,65,32,80,65,71,
		65,82,32,45,32,84,214,84,85,76,79,83,32,66,
		65,73,88,65,68,79,83,0,20,1,36,16,0,176,
		6,0,106,3,48,49,0,20,1,36,17,0,176,7,
		0,106,2,50,0,20,1,36,18,0,176,8,0,9,
		100,106,7,67,79,78,84,65,83,0,100,100,9,20,
		6,176,9,0,106,7,67,79,78,84,65,83,0,20,
		1,36,19,0,176,10,0,20,0,36,20,0,176,7,
		0,106,2,51,0,20,1,36,21,0,176,8,0,9,
		100,106,9,67,76,65,83,83,69,95,68,0,100,100,
		9,20,6,176,9,0,106,9,67,76,65,83,83,69,
		95,68,0,20,1,36,22,0,176,10,0,20,0,36,
		23,0,176,7,0,106,2,52,0,20,1,36,24,0,
		176,8,0,9,100,106,9,70,79,82,78,69,67,69,
		68,0,100,100,9,20,6,176,9,0,106,9,70,79,
		82,78,69,67,69,68,0,20,1,36,25,0,176,10,
		0,20,0,36,26,0,121,83,11,0,36,27,0,176,
		6,0,106,3,48,49,0,20,1,36,28,0,176,12,
		0,92,23,121,20,2,176,13,0,106,40,73,110,102,
		111,114,109,101,32,111,32,83,97,108,100,111,32,73,
		110,105,99,105,97,108,32,100,111,32,82,101,108,97,
		116,111,114,105,111,32,58,32,0,20,1,176,14,0,
		176,15,0,12,0,176,16,0,12,0,122,72,20,2,
		176,17,0,109,18,0,176,19,0,100,106,11,101,83,
		65,76,68,79,95,73,78,73,0,106,18,64,82,32,
		57,57,57,44,57,57,57,44,57,57,57,46,57,57,
		0,100,100,12,5,20,2,48,20,0,176,21,0,109,
		18,0,12,1,112,0,73,36,29,0,176,22,0,109,
		18,0,100,100,100,100,100,100,20,7,4,0,0,83,
		18,0,109,18,0,73,36,30,0,176,6,0,106,3,
		48,49,0,20,1,36,31,0,176,23,0,122,12,1,
		83,24,0,36,32,0,176,12,0,92,23,121,20,2,
		176,13,0,106,51,68,101,115,101,106,97,32,68,101,
		115,99,114,105,135,135,101,115,32,112,111,114,32,40,
		68,41,101,115,112,101,115,97,32,111,117,32,40,70,
		41,111,114,110,101,99,101,100,111,114,32,63,32,0,
		20,1,176,14,0,176,15,0,12,0,176,16,0,12,
		0,122,72,20,2,176,17,0,109,18,0,176,19,0,
		100,106,11,101,82,69,76,65,84,79,82,73,79,0,
		106,4,64,33,88,0,90,12,109,24,0,106,3,68,
		70,0,24,6,100,12,5,20,2,48,20,0,176,21,
		0,109,18,0,12,1,112,0,73,36,33,0,176,22,
		0,109,18,0,100,100,100,100,100,100,20,7,4,0,
		0,83,18,0,109,18,0,73,36,35,0,121,83,25,
		0,36,36,0,176,6,0,106,3,48,49,0,20,1,
		36,37,0,176,12,0,92,23,121,20,2,176,13,0,
		106,53,73,110,102,111,114,109,101,32,111,32,78,111,
		46,32,100,97,32,67,111,110,116,97,32,67,111,114,
		114,101,110,116,101,32,111,117,32,40,48,41,32,112,
		97,114,97,32,84,111,100,97,115,32,63,32,0,20,
		1,176,14,0,176,15,0,12,0,176,16,0,12,0,
		122,72,20,2,176,17,0,109,18,0,176,19,0,100,
		106,7,101,67,79,78,84,65,0,106,7,64,82,32,
		57,57,57,0,100,100,12,5,20,2,48,20,0,176,
		21,0,109,18,0,12,1,112,0,73,36,38,0,176,
		22,0,109,18,0,100,100,100,100,100,100,20,7,4,
		0,0,83,18,0,109,18,0,73,36,39,0,176,7,
		0,106,2,50,0,20,1,36,40,0,176,26,0,109,
		25,0,100,100,20,3,36,41,0,176,27,0,12,0,
		31,90,36,44,0,109,25,0,121,5,31,80,36,47,
		0,176,6,0,106,3,48,49,0,20,1,36,48,0,
		176,28,0,106,10,65,116,101,110,135,198,111,32,33,
		0,92,3,20,2,36,49,0,176,29,0,106,19,67,
		111,110,116,97,32,78,198,111,32,69,120,105,115,116,
		101,32,33,0,20,1,36,50,0,176,30,0,20,0,
		36,51,0,26,209,254,36,55,0,134,0,0,0,0,
		83,31,0,36,56,0,134,0,0,0,0,83,32,0,
		36,58,0,176,6,0,106,3,48,49,0,20,1,36,
		59,0,176,12,0,92,23,92,2,20,2,176,13,0,
		106,11,80,101,114,105,111,100,111,32,58,32,0,20,
		1,176,14,0,176,15,0,12,0,176,16,0,12,0,
		122,72,20,2,176,17,0,109,18,0,176,19,0,100,
		106,10,101,68,65,84,65,95,73,78,73,0,100,100,
		100,12,5,20,2,48,20,0,176,21,0,109,18,0,
		12,1,112,0,73,36,60,0,176,12,0,92,23,92,
		25,20,2,176,13,0,106,7,65,116,101,32,58,32,
		0,20,1,176,14,0,176,15,0,12,0,176,16,0,
		12,0,122,72,20,2,176,17,0,109,18,0,176,19,
		0,100,106,10,101,68,65,84,65,95,70,73,77,0,
		100,100,100,12,5,20,2,48,20,0,176,21,0,109,
		18,0,12,1,112,0,73,36,61,0,176,22,0,109,
		18,0,100,100,100,100,100,100,20,7,4,0,0,83,
		18,0,109,18,0,73,36,62,0,109,31,0,134,0,
		0,0,0,5,31,13,109,32,0,134,0,0,0,0,
		5,28,51,36,63,0,176,6,0,106,3,48,49,0,
		20,1,36,64,0,176,28,0,106,18,68,97,116,97,
		115,32,73,110,118,160,108,105,100,97,115,32,33,0,
		92,3,20,2,36,65,0,26,225,254,36,67,0,109,
		31,0,109,32,0,15,28,59,36,68,0,176,6,0,
		106,3,48,49,0,20,1,36,69,0,176,28,0,106,
		26,68,97,116,97,32,73,110,105,99,105,111,32,62,
		32,81,117,101,32,70,105,110,97,108,32,33,0,92,
		3,20,2,36,70,0,26,156,254,36,77,0,109,25,
		0,121,5,29,238,0,36,78,0,176,7,0,106,2,
		49,0,20,1,36,79,0,176,8,0,9,100,106,7,
		67,80,65,71,65,82,0,100,100,9,20,6,176,9,
		0,106,9,68,65,84,65,95,67,80,86,0,20,1,
		176,9,0,106,7,67,80,65,71,65,82,0,20,1,
		176,9,0,106,9,68,65,84,65,95,67,80,71,0,
		20,1,176,9,0,106,9,78,85,77,95,67,72,69,
		81,0,20,1,176,9,0,106,8,68,65,84,65,95,
		78,70,0,20,1,36,80,0,176,33,0,90,30,109,
		34,0,109,31,0,16,21,28,19,73,109,34,0,109,
		32,0,34,21,28,8,73,109,35,0,121,69,6,106,
		73,68,65,84,65,95,82,69,65,76,32,62,61,32,
		101,68,65,84,65,95,73,78,73,32,46,65,78,68,
		46,32,68,65,84,65,95,82,69,65,76,32,60,61,
		32,101,68,65,84,65,95,70,73,77,32,46,65,78,
		68,46,32,82,69,65,76,73,90,65,68,79,32,60,
		62,32,48,0,20,2,36,81,0,176,10,0,20,0,
		26,26,1,36,82,0,109,25,0,121,69,29,15,1,
		36,83,0,176,7,0,106,2,49,0,20,1,36,84,
		0,176,8,0,9,100,106,7,67,80,65,71,65,82,
		0,100,100,9,20,6,176,9,0,106,9,68,65,84,
		65,95,67,80,86,0,20,1,176,9,0,106,7,67,
		80,65,71,65,82,0,20,1,176,9,0,106,9,68,
		65,84,65,95,67,80,71,0,20,1,176,9,0,106,
		9,78,85,77,95,67,72,69,81,0,20,1,176,9,
		0,106,8,68,65,84,65,95,78,70,0,20,1,36,
		85,0,176,33,0,90,41,109,34,0,109,31,0,16,
		21,28,30,73,109,34,0,109,32,0,34,21,28,19,
		73,109,35,0,121,69,21,28,10,73,109,36,0,109,
		25,0,5,6,106,98,68,65,84,65,95,82,69,65,
		76,32,62,61,32,101,68,65,84,65,95,73,78,73,
		32,46,65,78,68,46,32,68,65,84,65,95,82,69,
		65,76,32,60,61,32,101,68,65,84,65,95,70,73,
		77,32,46,65,78,68,46,32,82,69,65,76,73,90,
		65,68,79,32,60,62,32,48,32,46,65,78,68,46,
		32,67,79,68,95,67,79,78,84,65,32,61,32,101,
		67,79,78,84,65,0,20,2,36,86,0,176,10,0,
		20,0,36,88,0,121,83,11,0,36,89,0,176,6,
		0,106,3,48,49,0,20,1,36,90,0,176,12,0,
		92,23,121,20,2,176,13,0,106,27,73,110,102,111,
		114,109,101,32,111,32,83,97,108,100,111,32,73,110,
		105,99,105,97,108,32,58,32,0,20,1,176,14,0,
		176,15,0,12,0,176,16,0,12,0,122,72,20,2,
		176,17,0,109,18,0,176,19,0,100,106,11,101,83,
		65,76,68,79,95,73,78,73,0,106,22,64,82,32,
		57,57,57,44,57,57,57,44,57,57,57,44,57,57,
		57,46,57,57,0,100,100,12,5,20,2,48,20,0,
		176,21,0,109,18,0,12,1,112,0,73,36,91,0,
		176,22,0,109,18,0,100,100,100,100,100,100,20,7,
		4,0,0,83,18,0,109,18,0,73,36,92,0,176,
		23,0,92,60,12,1,83,37,0,36,93,0,109,38,
		0,106,3,58,92,0,72,109,39,0,72,106,11,67,
		84,65,80,65,71,46,84,88,84,0,72,176,23,0,
		92,30,12,1,72,83,37,0,36,94,0,176,40,0,
		109,38,0,12,1,106,1,0,5,28,62,176,40,0,
		109,39,0,12,1,106,1,0,5,28,48,36,95,0,
		106,38,67,58,92,82,69,76,65,84,79,82,73,79,
		83,92,82,69,76,65,84,79,82,73,79,46,84,88,
		84,32,32,32,32,32,32,32,32,32,32,0,83,37,
		0,36,97,0,176,23,0,92,10,12,1,83,41,0,
		36,99,0,176,6,0,106,3,48,49,0,20,1,36,
		100,0,176,42,0,106,55,68,101,115,101,106,97,32,
		101,120,116,114,97,116,111,32,101,109,32,97,114,113,
		117,105,118,111,32,116,101,120,116,111,32,110,111,32,
		68,114,105,118,101,32,40,88,58,41,32,40,83,47,
		78,41,32,63,32,0,20,1,36,101,0,109,43,0,
		106,2,83,0,5,29,250,0,36,102,0,176,6,0,
		106,3,48,49,0,20,1,36,103,0,176,12,0,92,
		23,121,20,2,176,13,0,106,18,67,65,77,73,78,
		72,79,92,65,82,81,85,73,86,79,58,32,0,20,
		1,176,14,0,176,15,0,12,0,176,16,0,12,0,
		122,72,20,2,176,17,0,109,18,0,176,19,0,100,
		106,9,101,65,82,81,85,73,86,79,0,100,100,100,
		12,5,20,2,48,20,0,176,21,0,109,18,0,12,
		1,112,0,73,36,104,0,176,22,0,109,18,0,100,
		100,100,100,100,100,20,7,4,0,0,83,18,0,109,
		18,0,73,36,105,0,176,40,0,109,37,0,12,1,
		83,37,0,36,106,0,176,6,0,106,3,48,49,0,
		20,1,36,107,0,176,29,0,106,46,73,110,115,105,
		114,97,32,117,109,32,100,105,115,99,111,32,110,111,
		32,100,114,105,118,101,32,40,88,58,41,44,32,101,
		32,116,101,99,108,101,32,69,78,84,69,82,46,0,
		20,1,36,108,0,176,30,0,20,0,36,109,0,106,
		8,65,82,81,85,73,86,79,0,83,41,0,36,110,
		0,25,97,36,112,0,176,29,0,106,50,80,111,115,
		105,99,105,111,110,101,32,111,32,70,111,114,109,117,
		108,97,114,105,111,32,56,48,32,67,111,108,117,110,
		97,115,32,101,32,84,101,99,108,101,32,91,69,78,
		84,69,82,93,0,20,1,36,113,0,176,30,0,20,
		0,36,114,0,176,44,0,20,0,36,115,0,106,11,
		73,77,80,82,69,83,83,79,82,65,0,83,41,0,
		36,119,0,109,41,0,106,8,65,82,81,85,73,86,
		79,0,5,28,53,36,120,0,176,45,0,92,23,106,
		3,79,78,0,20,2,36,121,0,176,45,0,92,20,
		106,8,80,82,73,78,84,69,82,0,20,2,36,122,
		0,176,45,0,92,24,109,37,0,9,20,3,25,98,
		36,124,0,176,29,0,106,50,80,111,115,105,99,105,
		111,110,101,32,111,32,70,111,114,109,117,108,97,114,
		105,111,32,56,48,32,67,111,108,117,110,97,115,32,
		101,32,84,101,99,108,101,32,91,69,78,84,69,82,
		93,0,20,1,36,125,0,176,30,0,20,0,36,126,
		0,176,44,0,20,0,36,127,0,176,45,0,92,20,
		106,8,80,82,73,78,84,69,82,0,20,2,36,129,
		0,109,34,0,83,46,0,36,130,0,121,83,47,0,
		36,131,0,121,83,48,0,36,132,0,121,83,49,0,
		36,133,0,121,83,50,0,36,134,0,176,51,0,12,
		0,32,69,12,36,135,0,176,12,0,122,121,20,2,
		176,13,0,109,52,0,106,11,69,109,112,114,101,115,
		97,32,58,32,0,72,109,53,0,72,20,1,36,136,
		0,176,12,0,122,92,115,20,2,176,13,0,106,8,
		68,97,116,97,32,58,32,0,176,54,0,176,55,0,
		12,0,12,1,72,20,1,36,137,0,176,12,0,92,
		2,121,20,2,176,13,0,106,11,83,105,115,116,101,
		109,97,32,58,32,0,109,56,0,72,20,1,36,138,
		0,176,12,0,92,2,92,115,20,2,176,13,0,106,
		14,82,101,108,97,116,58,32,83,65,78,48,48,52,
		0,20,1,36,139,0,176,12,0,92,3,92,10,20,
		2,176,13,0,109,57,0,106,19,67,79,78,84,65,
		83,32,65,32,80,65,71,65,82,32,68,69,32,0,
		72,176,54,0,109,31,0,12,1,72,106,6,32,65,
		84,69,32,0,72,176,54,0,109,32,0,12,1,72,
		106,14,32,45,32,76,73,81,85,73,68,65,68,79,
		83,0,72,109,52,0,72,20,1,36,140,0,176,12,
		0,92,4,121,20,2,176,13,0,106,27,84,105,112,
		111,32,82,101,108,97,116,135,114,105,111,32,58,32,
		82,69,65,76,73,90,65,68,79,0,20,1,36,141,
		0,176,12,0,92,4,92,50,20,2,176,13,0,106,
		13,67,111,110,116,97,32,78,111,46,32,58,32,0,
		176,58,0,109,25,0,92,5,12,2,72,20,1,36,
		142,0,176,12,0,92,5,121,20,2,176,13,0,176,
		59,0,106,2,61,0,93,135,0,12,2,20,1,36,
		143,0,176,12,0,92,6,92,2,20,2,176,13,0,
		106,6,76,65,78,67,46,0,20,1,36,144,0,176,
		12,0,92,6,92,10,20,2,176,13,0,106,6,67,
		79,78,84,65,0,20,1,36,145,0,176,12,0,92,
		6,92,20,20,2,176,13,0,106,6,80,82,79,68,
		46,0,20,1,36,146,0,176,12,0,92,6,92,30,
		20,2,176,13,0,106,6,70,79,82,78,46,0,20,
		1,36,147,0,176,12,0,92,6,92,40,20,2,176,
		13,0,106,15,67,76,65,83,83,69,32,68,69,83,
		80,69,83,65,0,20,1,36,148,0,176,12,0,92,
		6,92,100,20,2,176,13,0,106,11,68,65,84,65,
		32,80,82,69,86,46,0,20,1,36,149,0,176,12,
		0,92,6,92,112,20,2,176,13,0,106,13,86,76,
		82,46,80,82,69,86,73,83,84,79,0,20,1,36,
		150,0,176,12,0,92,7,92,10,20,2,176,13,0,
		106,10,68,79,67,85,77,69,78,84,79,0,20,1,
		36,151,0,176,12,0,92,7,92,30,20,2,176,13,
		0,106,8,65,80,69,76,73,68,79,0,20,1,36,
		152,0,176,12,0,92,7,92,40,20,2,176,13,0,
		106,10,68,69,83,67,82,73,67,65,79,0,20,1,
		36,153,0,176,12,0,92,7,92,100,20,2,176,13,
		0,106,11,68,65,84,65,32,82,69,65,76,46,0,
		20,1,36,154,0,176,12,0,92,7,92,112,20,2,
		176,13,0,106,14,86,76,82,46,82,69,65,76,73,
		90,65,68,79,0,20,1,36,155,0,176,12,0,92,
		7,92,127,20,2,176,13,0,106,7,67,72,69,81,
		85,69,0,20,1,36,156,0,176,12,0,92,8,121,
		20,2,176,13,0,176,59,0,106,2,61,0,93,135,
		0,12,2,20,1,36,157,0,92,9,83,60,0,36,
		158,0,176,51,0,12,0,32,236,2,109,60,0,92,
		54,34,29,227,2,36,159,0,176,12,0,109,60,0,
		121,20,2,176,61,0,109,62,0,106,7,57,57,57,
		57,57,57,0,20,2,36,160,0,176,12,0,109,60,
		0,92,10,20,2,176,61,0,109,36,0,106,7,57,
		57,57,57,57,57,0,20,2,36,161,0,176,12,0,
		109,60,0,92,20,20,2,176,61,0,109,63,0,106,
		7,57,57,57,57,57,57,0,20,2,36,162,0,176,
		12,0,109,60,0,92,30,20,2,176,61,0,109,64,
		0,106,7,57,57,57,57,57,57,0,20,2,36,163,
		0,176,12,0,109,60,0,92,40,20,2,176,61,0,
		109,65,0,106,7,57,57,57,57,57,57,0,20,2,
		36,164,0,109,65,0,83,66,0,36,165,0,109,64,
		0,83,67,0,36,166,0,109,24,0,106,2,68,0,
		5,28,55,36,167,0,176,7,0,106,2,51,0,20,
		1,36,168,0,176,26,0,109,66,0,100,100,20,3,
		36,169,0,176,12,0,109,60,0,92,50,20,2,176,
		13,0,176,40,0,109,68,0,12,1,20,1,25,53,
		36,171,0,176,7,0,106,2,52,0,20,1,36,172,
		0,176,26,0,109,67,0,100,100,20,3,36,173,0,
		176,12,0,109,60,0,92,50,20,2,176,13,0,176,
		40,0,109,69,0,12,1,20,1,36,175,0,176,7,
		0,106,2,49,0,20,1,36,176,0,176,12,0,109,
		60,0,92,100,20,2,176,13,0,109,70,0,20,1,
		36,177,0,176,12,0,109,60,0,92,112,20,2,176,
		61,0,109,71,0,106,17,64,82,32,57,57,44,57,
		57,57,44,57,57,57,46,57,57,0,20,2,36,178,
		0,109,60,0,122,72,83,60,0,36,179,0,176,12,
		0,109,60,0,92,10,20,2,176,13,0,109,72,0,
		20,1,36,180,0,176,7,0,106,2,52,0,20,1,
		36,181,0,176,26,0,109,67,0,100,100,20,3,36,
		182,0,176,12,0,109,60,0,92,30,20,2,176,13,
		0,109,73,0,20,1,36,183,0,176,7,0,106,2,
		49,0,20,1,36,184,0,176,12,0,109,60,0,92,
		50,20,2,176,13,0,176,40,0,109,68,0,12,1,
		20,1,36,185,0,176,12,0,109,60,0,92,100,20,
		2,176,13,0,109,34,0,20,1,36,186,0,176,12,
		0,109,60,0,92,112,20,2,176,61,0,109,35,0,
		106,17,64,82,32,57,57,44,57,57,57,44,57,57,
		57,46,57,57,0,20,2,36,187,0,176,12,0,109,
		60,0,92,125,20,2,176,61,0,109,74,0,106,12,
		64,82,32,57,57,57,57,57,57,57,57,0,20,2,
		36,188,0,109,60,0,122,72,83,60,0,36,189,0,
		109,47,0,109,71,0,72,83,47,0,36,190,0,109,
		48,0,109,35,0,72,83,48,0,36,191,0,176,75,
		0,122,20,1,36,192,0,176,76,0,20,0,36,193,
		0,109,77,0,29,119,253,36,194,0,109,41,0,106,
		8,65,82,81,85,73,86,79,0,5,28,39,36,195,
		0,176,45,0,92,23,106,4,79,70,70,0,20,2,
		36,196,0,176,45,0,92,20,106,7,83,67,82,69,
		69,78,0,20,2,25,21,36,198,0,176,45,0,92,
		20,106,7,83,67,82,69,69,78,0,20,2,36,200,
		0,176,78,0,20,0,176,7,0,106,2,49,0,20,
		1,176,79,0,100,20,1,36,201,0,100,110,7,36,
		204,0,176,51,0,12,0,29,13,6,36,205,0,109,
		60,0,92,48,15,29,225,2,36,206,0,176,12,0,
		92,58,121,20,2,176,13,0,176,59,0,106,2,61,
		0,93,135,0,12,2,20,1,36,207,0,176,12,0,
		92,59,92,104,20,2,176,13,0,109,80,0,20,1,
		36,208,0,176,12,0,122,121,20,2,176,13,0,106,
		2,32,0,109,81,0,72,20,1,36,209,0,176,12,
		0,122,121,20,2,176,13,0,109,52,0,106,11,69,
		109,112,114,101,115,97,32,58,32,0,72,109,53,0,
		72,20,1,36,210,0,176,12,0,122,92,120,20,2,
		176,13,0,106,8,68,97,116,97,32,58,32,0,176,
		54,0,176,55,0,12,0,12,1,72,20,1,36,211,
		0,176,12,0,92,2,121,20,2,176,13,0,106,11,
		83,105,115,116,101,109,97,32,58,32,0,109,56,0,
		72,20,1,36,212,0,176,12,0,92,2,92,121,20,
		2,176,13,0,106,14,82,101,108,97,116,58,32,83,
		65,78,48,48,52,0,20,1,36,213,0,176,12,0,
		92,3,92,10,20,2,176,13,0,109,57,0,106,19,
		67,79,78,84,65,83,32,65,32,80,65,71,65,82,
		32,68,69,32,0,72,176,54,0,109,31,0,12,1,
		72,106,6,32,65,84,69,32,0,72,176,54,0,109,
		32,0,12,1,72,106,14,32,45,32,76,73,81,85,
		73,68,65,68,79,83,0,72,109,52,0,72,20,1,
		36,214,0,176,12,0,92,5,121,20,2,176,13,0,
		176,59,0,106,2,61,0,93,135,0,12,2,20,1,
		36,215,0,176,12,0,92,6,92,2,20,2,176,13,
		0,106,6,76,65,78,67,46,0,20,1,36,216,0,
		176,12,0,92,6,92,10,20,2,176,13,0,106,6,
		67,79,78,84,65,0,20,1,36,217,0,176,12,0,
		92,6,92,20,20,2,176,13,0,106,6,80,82,79,
		68,46,0,20,1,36,218,0,176,12,0,92,6,92,
		30,20,2,176,13,0,106,6,70,79,82,78,46,0,
		20,1,36,219,0,176,12,0,92,6,92,40,20,2,
		176,13,0,106,15,67,76,65,83,83,69,32,68,69,
		83,80,69,83,65,0,20,1,36,220,0,176,12,0,
		92,6,92,100,20,2,176,13,0,106,11,68,65,84,
		65,32,80,82,69,86,46,0,20,1,36,221,0,176,
		12,0,92,6,92,112,20,2,176,13,0,106,13,86,
		76,82,46,80,82,69,86,73,83,84,79,0,20,1,
		36,222,0,176,12,0,92,7,92,10,20,2,176,13,
		0,106,10,68,79,67,85,77,69,78,84,79,0,20,
		1,36,223,0,176,12,0,92,7,92,30,20,2,176,
		13,0,106,8,65,80,69,76,73,68,79,0,20,1,
		36,224,0,176,12,0,92,7,92,40,20,2,176,13,
		0,106,10,68,69,83,67,82,73,67,65,79,0,20,
		1,36,225,0,176,12,0,92,7,92,100,20,2,176,
		13,0,106,11,68,65,84,65,32,82,69,65,76,46,
		0,20,1,36,226,0,176,12,0,92,7,92,112,20,
		2,176,13,0,106,14,86,76,82,46,82,69,65,76,
		73,90,65,68,79,0,20,1,36,227,0,176,12,0,
		92,7,92,127,20,2,176,13,0,106,7,67,72,69,
		81,85,69,0,20,1,36,228,0,176,12,0,92,8,
		121,20,2,176,13,0,176,59,0,106,2,61,0,93,
		135,0,12,2,20,1,36,229,0,92,9,83,60,0,
		36,231,0,176,12,0,109,60,0,92,84,20,2,176,
		13,0,176,59,0,106,2,95,0,92,45,12,2,20,
		1,36,232,0,109,60,0,122,72,83,60,0,36,233,
		0,176,12,0,109,60,0,92,2,20,2,176,13,0,
		106,13,83,117,98,116,111,116,97,105,115,32,58,32,
		0,20,1,36,234,0,176,12,0,109,60,0,92,112,
		20,2,176,61,0,109,47,0,106,17,64,82,32,57,
		57,44,57,57,57,44,57,57,57,46,57,57,0,20,
		2,36,235,0,109,60,0,122,72,83,60,0,36,236,
		0,176,12,0,109,60,0,92,112,20,2,176,61,0,
		109,48,0,106,17,64,82,32,57,57,44,57,57,57,
		44,57,57,57,46,57,57,0,20,2,36,237,0,109,
		60,0,92,2,72,83,60,0,36,238,0,109,49,0,
		109,47,0,72,83,49,0,36,239,0,109,50,0,109,
		48,0,72,83,50,0,36,240,0,176,12,0,109,60,
		0,92,84,20,2,176,13,0,176,59,0,106,2,95,
		0,92,45,12,2,20,1,36,241,0,109,60,0,122,
		72,83,60,0,36,242,0,176,12,0,109,60,0,92,
		2,20,2,176,13,0,106,10,84,111,116,97,105,115,
		32,58,32,0,20,1,36,243,0,176,12,0,109,60,
		0,92,112,20,2,176,61,0,109,49,0,106,17,64,
		82,32,57,57,44,57,57,57,44,57,57,57,46,57,
		57,0,20,2,36,244,0,109,60,0,122,72,83,60,
		0,36,245,0,176,12,0,109,60,0,92,112,20,2,
		176,61,0,109,50,0,106,17,64,82,32,57,57,44,
		57,57,57,44,57,57,57,46,57,57,0,20,2,36,
		246,0,109,60,0,92,2,72,83,60,0,36,247,0,
		176,12,0,109,60,0,92,2,20,2,176,13,0,106,
		17,83,97,108,100,111,32,105,110,105,99,105,97,108,
		32,58,32,0,20,1,36,248,0,176,12,0,109,60,
		0,92,112,20,2,176,61,0,109,11,0,106,17,64,
		82,32,57,57,44,57,57,57,44,57,57,57,46,57,
		57,0,20,2,36,249,0,109,60,0,122,72,83,60,
		0,36,250,0,176,12,0,109,60,0,92,112,20,2,
		176,61,0,109,11,0,106,17,64,82,32,57,57,44,
		57,57,57,44,57,57,57,46,57,57,0,20,2,36,
		251,0,109,60,0,122,72,83,60,0,36,252,0,176,
		12,0,109,60,0,92,2,20,2,176,13,0,106,17,
		77,111,118,105,109,101,110,116,97,99,111,101,115,32,
		58,32,0,20,1,36,253,0,176,12,0,109,60,0,
		92,112,20,2,176,61,0,109,49,0,106,17,64,82,
		32,57,57,44,57,57,57,44,57,57,57,46,57,57,
		0,20,2,36,254,0,109,60,0,122,72,83,60,0,
		36,255,0,176,12,0,109,60,0,92,112,20,2,176,
		61,0,109,50,0,106,17,64,82,32,57,57,44,57,
		57,57,44,57,57,57,46,57,57,0,20,2,36,0,
		1,109,60,0,122,72,83,60,0,36,1,1,176,12,
		0,109,60,0,92,2,20,2,176,13,0,106,17,83,
		97,108,100,111,32,70,105,110,97,108,32,32,32,58,
		32,0,20,1,36,2,1,176,12,0,109,60,0,92,
		112,20,2,176,61,0,109,11,0,109,49,0,72,106,
		17,64,82,32,57,57,44,57,57,57,44,57,57,57,
		46,57,57,0,20,2,36,3,1,109,60,0,122,72,
		83,60,0,36,4,1,176,12,0,109,60,0,92,112,
		20,2,176,61,0,109,11,0,109,50,0,72,106,17,
		64,82,32,57,57,44,57,57,57,44,57,57,57,46,
		57,57,0,20,2,36,5,1,109,60,0,122,72,83,
		60,0,36,7,1,176,12,0,92,56,121,20,2,176,
		13,0,176,59,0,106,2,61,0,93,135,0,12,2,
		20,1,36,8,1,176,12,0,92,57,92,95,20,2,
		176,13,0,109,80,0,20,1,36,9,1,176,12,0,
		122,121,20,2,176,13,0,106,2,32,0,109,81,0,
		72,20,1,26,182,243,36,11,1,109,41,0,106,8,
		65,82,81,85,73,86,79,0,5,28,52,36,12,1,
		176,45,0,92,24,106,1,0,20,2,36,13,1,176,
		45,0,92,23,106,4,79,70,70,0,20,2,36,14,
		1,176,45,0,92,20,106,7,83,67,82,69,69,78,
		0,20,2,25,34,36,16,1,176,45,0,92,24,106,
		1,0,20,2,36,17,1,176,45,0,92,20,106,7,
		83,67,82,69,69,78,0,20,2,36,19,1,176,78,
		0,20,0,36,20,1,176,82,0,121,121,176,2,0,
		12,0,176,3,0,12,0,109,4,0,20,5,36,21,
		1,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}
