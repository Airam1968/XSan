/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_apag.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_APAG );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
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
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( PTECLA );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_APAG )
{ "REL_APAG", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_APAG )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "CIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EOPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "EDATA_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "ECOD_CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "COD_AGENCI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_CC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "COD_CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_PREV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REALIZADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_LANC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "LANCAMENTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CODIGO_FOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECODIGO_FOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NOME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORNECED", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "NUM_DOCUME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATA_NOTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "PREVISTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "EMPREENDIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE_D", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_APAG, "C:/sis/SAN/xhb/rel_apag.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_APAG
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_APAG )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_APAG )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,37,69,77,73,83,83,199,79,79,32,68,69,
		32,65,85,84,79,82,73,90,65,128,199,79,32,68,
		69,32,80,65,71,65,77,69,78,84,79,0,20,1,
		36,16,0,176,6,0,106,3,48,49,0,20,1,36,
		17,0,176,7,0,106,18,67,111,110,102,105,114,109,
		97,32,40,83,47,78,41,32,63,32,0,20,1,36,
		18,0,109,8,0,106,2,83,0,5,29,111,19,36,
		19,0,121,83,9,0,36,20,0,176,10,0,20,0,
		36,21,0,176,11,0,106,2,49,0,20,1,36,22,
		0,176,12,0,9,100,106,7,67,80,65,71,65,82,
		0,100,100,9,20,6,176,13,0,106,7,67,80,65,
		71,65,82,0,20,1,176,13,0,106,9,68,65,84,
		65,95,67,80,86,0,20,1,176,13,0,106,9,68,
		65,84,65,95,67,80,71,0,20,1,36,23,0,176,
		14,0,20,0,36,24,0,176,11,0,106,2,50,0,
		20,1,36,25,0,176,12,0,9,100,106,9,70,79,
		82,78,69,67,69,68,0,100,100,9,20,6,176,13,
		0,106,9,70,79,82,78,69,67,69,68,0,20,1,
		36,26,0,176,14,0,20,0,36,27,0,176,11,0,
		106,2,51,0,20,1,36,28,0,176,12,0,9,100,
		106,7,67,79,78,84,65,83,0,100,100,9,20,6,
		176,13,0,106,7,67,79,78,84,65,83,0,20,1,
		36,29,0,176,14,0,20,0,36,30,0,176,11,0,
		106,2,52,0,20,1,36,31,0,176,12,0,9,100,
		106,7,83,84,65,84,85,83,0,100,100,9,20,6,
		36,32,0,176,14,0,20,0,36,33,0,109,15,0,
		83,16,0,36,34,0,176,6,0,106,3,48,49,0,
		20,1,36,35,0,176,17,0,122,12,1,83,18,0,
		36,36,0,176,19,0,92,23,121,20,2,176,20,0,
		106,59,68,101,115,101,106,97,32,83,101,108,101,135,
		198,111,32,112,111,114,32,78,163,109,101,114,111,32,
		100,101,32,40,76,41,97,110,135,97,109,101,110,116,
		111,32,111,117,32,112,111,114,32,40,68,41,97,116,
		97,32,63,32,0,20,1,176,21,0,176,22,0,12,
		0,176,23,0,12,0,122,72,20,2,176,24,0,109,
		25,0,176,26,0,100,106,7,101,79,80,67,65,79,
		0,106,4,64,33,88,0,90,12,109,18,0,106,3,
		76,68,0,24,6,100,12,5,20,2,48,27,0,176,
		28,0,109,25,0,12,1,112,0,73,36,37,0,176,
		29,0,109,25,0,100,100,100,100,100,100,20,7,4,
		0,0,83,25,0,109,25,0,73,36,38,0,109,18,
		0,106,2,68,0,5,29,188,4,36,39,0,176,11,
		0,106,2,49,0,20,1,36,40,0,176,12,0,9,
		100,106,7,67,80,65,71,65,82,0,100,100,9,20,
		6,176,13,0,106,9,68,65,84,65,95,67,80,71,
		0,20,1,176,13,0,106,7,67,80,65,71,65,82,
		0,20,1,176,13,0,106,9,68,65,84,65,95,67,
		80,86,0,20,1,36,41,0,176,14,0,20,0,36,
		42,0,176,6,0,106,3,48,49,0,20,1,36,43,
		0,134,0,0,0,0,83,30,0,36,44,0,134,0,
		0,0,0,83,31,0,36,46,0,176,19,0,92,10,
		92,2,20,2,176,20,0,106,15,80,101,114,105,111,
		100,111,32,32,32,32,32,58,32,0,20,1,176,21,
		0,176,22,0,12,0,176,23,0,12,0,122,72,20,
		2,176,24,0,109,25,0,176,26,0,100,106,10,101,
		68,65,84,65,95,73,78,73,0,100,100,100,12,5,
		20,2,48,27,0,176,28,0,109,25,0,12,1,112,
		0,73,36,47,0,176,19,0,92,11,92,2,20,2,
		176,20,0,106,15,65,116,101,32,32,32,32,32,32,
		32,32,32,58,32,0,20,1,176,21,0,176,22,0,
		12,0,176,23,0,12,0,122,72,20,2,176,24,0,
		109,25,0,176,26,0,100,106,10,101,68,65,84,65,
		95,70,73,77,0,100,100,100,12,5,20,2,48,27,
		0,176,28,0,109,25,0,12,1,112,0,73,36,48,
		0,176,29,0,109,25,0,100,100,100,100,100,100,20,
		7,4,0,0,83,25,0,109,25,0,73,36,49,0,
		109,30,0,134,0,0,0,0,5,31,13,109,31,0,
		134,0,0,0,0,5,28,51,36,50,0,176,6,0,
		106,3,48,49,0,20,1,36,51,0,176,32,0,106,
		18,68,97,116,97,115,32,73,110,118,160,108,105,100,
		97,115,32,33,0,92,3,20,2,36,52,0,26,226,
		254,36,54,0,109,30,0,109,31,0,15,28,59,36,
		55,0,176,6,0,106,3,48,49,0,20,1,36,56,
		0,176,32,0,106,26,68,97,116,97,32,73,110,105,
		99,105,111,32,62,32,81,117,101,32,70,105,110,97,
		108,32,33,0,92,3,20,2,36,57,0,26,157,254,
		36,64,0,121,83,33,0,36,65,0,176,19,0,92,
		12,92,2,20,2,176,20,0,106,15,78,117,109,46,
		32,67,111,110,116,97,32,32,58,32,0,20,1,176,
		21,0,176,22,0,12,0,176,23,0,12,0,122,72,
		20,2,176,24,0,109,25,0,176,26,0,100,106,11,
		101,67,79,68,95,67,79,78,84,65,0,106,13,64,
		82,32,57,57,57,57,57,57,57,57,57,0,100,100,
		12,5,20,2,48,27,0,176,28,0,109,25,0,12,
		1,112,0,73,36,66,0,176,29,0,109,25,0,100,
		100,100,100,100,100,20,7,4,0,0,83,25,0,109,
		25,0,73,36,67,0,109,33,0,121,69,29,185,0,
		36,68,0,176,11,0,106,2,51,0,20,1,36,69,
		0,176,34,0,109,33,0,100,100,20,3,36,70,0,
		176,35,0,12,0,28,63,36,71,0,176,19,0,92,
		12,92,30,20,2,176,20,0,106,8,32,65,71,46,
		32,58,32,0,176,36,0,109,37,0,12,1,72,106,
		8,32,67,47,67,32,58,32,0,72,176,36,0,109,
		38,0,12,1,72,20,1,36,72,0,25,88,36,74,
		0,176,6,0,106,3,48,49,0,20,1,36,75,0,
		176,32,0,106,10,65,116,101,110,135,198,111,32,33,
		0,92,3,20,2,36,76,0,176,39,0,106,27,67,
		162,100,105,103,111,32,100,101,32,67,111,110,116,97,
		32,73,110,118,160,108,105,100,111,32,33,0,20,1,
		36,77,0,176,40,0,20,0,36,78,0,26,179,254,
		36,84,0,176,11,0,106,2,49,0,20,1,36,85,
		0,109,33,0,121,69,29,206,0,36,86,0,176,41,
		0,90,54,109,42,0,109,33,0,5,21,28,43,73,
		109,43,0,109,30,0,16,21,28,32,73,109,43,0,
		109,31,0,34,21,28,21,73,109,44,0,134,0,0,
		0,0,5,21,28,8,73,109,45,0,121,5,6,106,
		128,67,79,68,95,67,79,78,84,65,32,61,32,101,
		67,79,68,95,67,79,78,84,65,32,46,65,78,68,
		46,32,68,65,84,65,95,80,82,69,86,32,62,61,
		32,101,68,65,84,65,95,73,78,73,32,46,65,78,
		68,46,32,68,65,84,65,95,80,82,69,86,32,60,
		61,32,101,68,65,84,65,95,70,73,77,32,46,65,
		78,68,46,32,68,65,84,65,95,82,69,65,76,32,
		61,32,67,84,79,68,40,34,34,41,32,46,65,78,
		68,46,32,82,69,65,76,73,90,65,68,79,32,61,
		32,48,0,20,2,36,87,0,176,14,0,20,0,26,
		133,2,36,89,0,176,41,0,90,43,109,43,0,109,
		30,0,16,21,28,32,73,109,43,0,109,31,0,34,
		21,28,21,73,109,44,0,134,0,0,0,0,5,21,
		28,8,73,109,45,0,121,5,6,106,99,68,65,84,
		65,95,80,82,69,86,32,62,61,32,101,68,65,84,
		65,95,73,78,73,32,46,65,78,68,46,32,68,65,
		84,65,95,80,82,69,86,32,60,61,32,101,68,65,
		84,65,95,70,73,77,32,46,65,78,68,46,32,68,
		65,84,65,95,82,69,65,76,32,61,32,67,84,79,
		68,40,34,34,41,32,46,65,78,68,46,32,82,69,
		65,76,73,90,65,68,79,32,61,32,48,0,20,2,
		36,90,0,176,14,0,20,0,36,91,0,26,223,1,
		36,93,0,176,11,0,106,2,49,0,20,1,36,94,
		0,176,12,0,9,100,106,7,67,80,65,71,65,82,
		0,100,100,9,20,6,176,13,0,106,7,67,80,65,
		71,65,82,0,20,1,176,13,0,106,9,68,65,84,
		65,95,67,80,86,0,20,1,176,13,0,106,9,68,
		65,84,65,95,67,80,71,0,20,1,36,95,0,176,
		14,0,20,0,36,97,0,121,83,46,0,36,98,0,
		176,19,0,92,12,92,2,20,2,176,20,0,106,19,
		78,117,109,46,32,76,97,110,135,97,109,101,110,116,
		111,32,58,32,0,20,1,176,21,0,176,22,0,12,
		0,176,23,0,12,0,122,72,20,2,176,24,0,109,
		25,0,176,26,0,100,106,10,101,78,85,77,95,76,
		65,78,67,0,106,13,64,82,32,57,57,57,57,57,
		57,57,57,57,0,100,100,12,5,20,2,48,27,0,
		176,28,0,109,25,0,12,1,112,0,73,36,99,0,
		176,29,0,109,25,0,100,100,100,100,100,100,20,7,
		4,0,0,83,25,0,109,25,0,73,36,100,0,109,
		46,0,121,69,28,119,36,101,0,176,11,0,106,2,
		49,0,20,1,36,102,0,176,34,0,109,46,0,100,
		100,20,3,36,103,0,176,35,0,12,0,32,136,0,
		36,106,0,176,6,0,106,3,48,49,0,20,1,36,
		107,0,176,32,0,106,10,65,116,101,110,135,198,111,
		32,33,0,92,3,20,2,36,108,0,176,39,0,106,
		22,76,97,110,135,97,109,101,110,116,111,32,73,110,
		118,135,108,105,100,111,32,33,0,20,1,36,109,0,
		176,40,0,20,0,36,110,0,26,242,254,36,113,0,
		176,47,0,20,0,176,11,0,106,2,49,0,20,1,
		176,48,0,100,20,1,36,114,0,176,49,0,121,121,
		176,2,0,12,0,176,3,0,12,0,109,4,0,20,
		5,36,115,0,100,110,7,36,118,0,176,11,0,106,
		2,49,0,20,1,36,119,0,176,41,0,90,10,109,
		50,0,109,46,0,5,6,106,23,76,65,78,67,65,
		77,69,78,84,79,32,61,32,101,78,85,77,95,76,
		65,78,67,0,20,2,36,120,0,176,14,0,20,0,
		36,122,0,176,6,0,106,3,48,49,0,20,1,36,
		123,0,176,17,0,122,12,1,83,51,0,36,124,0,
		176,19,0,92,23,121,20,2,176,20,0,106,48,68,
		101,115,101,106,97,32,73,109,112,114,101,115,115,198,
		111,32,100,101,32,65,80,32,40,71,41,114,97,110,
		100,101,32,111,117,32,40,80,41,101,113,117,101,110,
		97,32,63,32,0,20,1,176,21,0,176,22,0,12,
		0,176,23,0,12,0,122,72,20,2,176,24,0,109,
		25,0,176,26,0,100,106,6,101,84,73,80,79,0,
		106,4,64,33,88,0,90,12,109,51,0,106,3,71,
		80,0,24,6,100,12,5,20,2,48,27,0,176,28,
		0,109,25,0,12,1,112,0,73,36,125,0,176,29,
		0,109,25,0,100,100,100,100,100,100,20,7,4,0,
		0,83,25,0,109,25,0,73,36,126,0,176,17,0,
		92,60,12,1,83,52,0,36,127,0,109,53,0,106,
		3,58,92,0,72,109,54,0,72,106,11,67,84,65,
		80,65,71,46,84,88,84,0,72,176,17,0,92,30,
		12,1,72,83,52,0,36,128,0,176,36,0,109,53,
		0,12,1,106,1,0,5,28,62,176,36,0,109,54,
		0,12,1,106,1,0,5,28,48,36,129,0,106,38,
		67,58,92,82,69,76,65,84,79,82,73,79,83,92,
		82,69,76,65,84,79,82,73,79,46,84,88,84,32,
		32,32,32,32,32,32,32,32,32,0,83,52,0,36,
		131,0,176,6,0,106,3,48,49,0,20,1,36,132,
		0,176,39,0,106,39,80,111,115,105,99,105,111,110,
		101,32,111,32,70,111,114,109,117,108,97,114,105,111,
		32,101,32,84,101,99,108,101,32,91,69,78,84,69,
		82,93,0,20,1,36,133,0,176,40,0,20,0,36,
		134,0,176,55,0,20,0,36,135,0,176,56,0,92,
		20,106,8,80,82,73,78,84,69,82,0,20,2,36,
		136,0,176,19,0,109,9,0,121,20,2,176,20,0,
		106,2,32,0,20,1,36,137,0,176,57,0,12,0,
		32,17,9,36,138,0,109,50,0,83,46,0,36,139,
		0,109,51,0,106,2,80,0,5,29,217,4,36,141,
		0,176,19,0,109,9,0,92,20,20,2,176,20,0,
		106,30,65,85,84,79,82,73,90,65,67,65,79,32,
		68,69,32,80,65,71,65,77,69,78,84,79,32,78,
		111,46,32,0,176,58,0,109,46,0,92,5,12,2,
		72,20,1,36,142,0,176,19,0,109,9,0,92,20,
		20,2,176,20,0,106,30,65,85,84,79,82,73,90,
		65,67,65,79,32,68,69,32,80,65,71,65,77,69,
		78,84,79,32,78,111,46,32,0,176,58,0,109,46,
		0,92,5,12,2,72,20,1,36,143,0,109,9,0,
		92,2,72,83,9,0,36,144,0,176,19,0,109,9,
		0,121,20,2,176,20,0,106,11,69,109,112,114,101,
		115,97,32,58,32,0,109,59,0,72,20,1,36,145,
		0,109,9,0,122,72,83,9,0,36,146,0,109,60,
		0,83,61,0,36,147,0,176,11,0,106,2,50,0,
		20,1,36,148,0,176,34,0,109,61,0,100,100,20,
		3,36,149,0,109,62,0,83,63,0,36,150,0,176,
		11,0,106,2,49,0,20,1,36,151,0,176,19,0,
		109,9,0,121,20,2,176,20,0,106,17,70,111,114,
		110,101,99,101,100,111,114,32,46,46,46,58,32,0,
		176,58,0,109,61,0,92,5,12,2,72,106,2,45,
		0,72,176,64,0,109,63,0,122,92,27,12,3,72,
		20,1,36,152,0,176,19,0,109,9,0,92,50,20,
		2,176,20,0,106,14,80,97,114,99,101,108,97,46,
		46,46,46,58,32,0,20,1,36,153,0,109,9,0,
		122,72,83,9,0,36,154,0,176,19,0,109,9,0,
		121,20,2,176,20,0,106,17,78,111,46,32,68,111,
		99,117,109,101,110,116,111,32,58,32,0,109,65,0,
		72,20,1,36,155,0,176,19,0,109,9,0,92,50,
		20,2,176,20,0,106,14,68,97,116,97,32,67,111,
		109,112,114,97,58,32,0,176,66,0,109,67,0,12,
		1,72,20,1,36,156,0,109,9,0,122,72,83,9,
		0,36,157,0,176,19,0,109,9,0,121,20,2,176,
		20,0,106,17,86,101,110,99,105,109,101,110,116,111,
		32,46,46,46,58,32,0,176,66,0,109,43,0,12,
		1,72,20,1,36,158,0,176,19,0,109,9,0,92,
		50,20,2,176,20,0,106,14,86,97,108,111,114,46,
		46,46,46,46,46,58,32,0,20,1,36,159,0,176,
		19,0,109,9,0,92,63,20,2,176,68,0,109,69,
		0,106,18,64,82,32,57,57,57,44,57,57,57,44,
		57,57,57,46,57,57,0,20,2,36,160,0,109,9,
		0,122,72,83,9,0,36,161,0,176,19,0,109,9,
		0,121,20,2,176,20,0,106,17,68,101,115,99,114,
		105,99,97,111,46,46,46,46,46,58,32,0,176,64,
		0,109,70,0,122,92,32,12,3,72,20,1,36,162,
		0,109,9,0,122,72,83,9,0,36,163,0,176,19,
		0,109,9,0,121,20,2,176,20,0,176,71,0,106,
		2,95,0,92,79,12,2,20,1,36,164,0,109,9,
		0,122,72,83,9,0,36,165,0,176,19,0,109,9,
		0,121,20,2,176,20,0,176,71,0,106,2,95,0,
		92,79,12,2,20,1,36,166,0,109,9,0,122,72,
		83,9,0,36,167,0,176,19,0,109,9,0,121,20,
		2,176,20,0,176,71,0,106,2,95,0,92,79,12,
		2,20,1,36,168,0,109,9,0,122,72,83,9,0,
		36,169,0,176,19,0,109,9,0,121,20,2,176,20,
		0,176,71,0,106,2,95,0,92,79,12,2,20,1,
		36,170,0,109,9,0,122,72,83,9,0,36,171,0,
		176,19,0,109,9,0,121,20,2,176,20,0,106,2,
		124,0,20,1,36,172,0,176,19,0,109,9,0,122,
		20,2,176,20,0,106,9,69,77,73,84,69,78,84,
		69,0,20,1,36,173,0,176,19,0,109,9,0,92,
		20,20,2,176,20,0,106,2,124,0,20,1,36,174,
		0,176,19,0,109,9,0,92,21,20,2,176,20,0,
		106,12,82,69,83,80,79,78,83,65,86,69,76,0,
		20,1,36,175,0,176,19,0,109,9,0,92,40,20,
		2,176,20,0,106,2,124,0,20,1,36,176,0,176,
		19,0,109,9,0,92,41,20,2,176,20,0,106,15,
		67,79,78,84,65,83,32,65,32,80,65,71,65,82,
		0,20,1,36,177,0,176,19,0,109,9,0,92,60,
		20,2,176,20,0,106,2,124,0,20,1,36,178,0,
		176,19,0,109,9,0,92,61,20,2,176,20,0,106,
		14,67,79,78,84,65,66,73,76,73,68,65,68,69,
		0,20,1,36,179,0,176,19,0,109,9,0,92,79,
		20,2,176,20,0,106,2,124,0,20,1,36,180,0,
		109,9,0,122,72,83,9,0,36,181,0,176,19,0,
		109,9,0,121,20,2,176,20,0,106,2,124,0,20,
		1,36,182,0,176,19,0,109,9,0,92,20,20,2,
		176,20,0,106,2,124,0,20,1,36,183,0,176,19,
		0,109,9,0,92,40,20,2,176,20,0,106,2,124,
		0,20,1,36,184,0,176,19,0,109,9,0,92,60,
		20,2,176,20,0,106,2,124,0,20,1,36,185,0,
		176,19,0,109,9,0,92,79,20,2,176,20,0,106,
		2,124,0,20,1,36,186,0,109,9,0,122,72,83,
		9,0,36,187,0,176,19,0,109,9,0,122,20,2,
		176,20,0,176,71,0,106,3,32,95,0,92,38,12,
		2,20,1,36,188,0,109,9,0,92,2,72,83,9,
		0,36,189,0,109,9,0,92,45,16,28,30,36,190,
		0,121,83,9,0,36,191,0,176,19,0,109,9,0,
		121,20,2,176,20,0,106,2,32,0,20,1,36,192,
		0,26,196,3,36,195,0,176,19,0,109,9,0,92,
		30,20,2,176,20,0,106,25,65,85,84,79,82,73,
		90,65,67,65,79,32,68,69,32,80,65,71,65,77,
		69,78,84,79,0,20,1,36,196,0,176,19,0,109,
		9,0,92,30,20,2,176,20,0,106,25,65,85,84,
		79,82,73,90,65,67,65,79,32,68,69,32,80,65,
		71,65,77,69,78,84,79,0,20,1,36,197,0,109,
		9,0,92,4,72,83,9,0,36,198,0,109,60,0,
		83,61,0,36,199,0,176,11,0,106,2,50,0,20,
		1,36,200,0,176,34,0,109,61,0,100,100,20,3,
		36,201,0,109,62,0,83,63,0,36,202,0,176,11,
		0,106,2,49,0,20,1,36,203,0,176,19,0,109,
		9,0,121,20,2,176,20,0,106,17,67,101,100,101,
		110,116,101,32,46,46,32,46,46,46,58,32,0,176,
		58,0,109,61,0,92,5,12,2,72,106,2,45,0,
		72,176,64,0,109,63,0,122,92,27,12,3,72,20,
		1,36,204,0,176,19,0,109,9,0,92,50,20,2,
		176,20,0,106,14,79,98,114,97,46,46,46,46,46,
		46,46,58,32,0,176,58,0,109,72,0,92,5,12,
		2,72,20,1,36,205,0,109,9,0,92,2,72,83,
		9,0,36,206,0,176,19,0,109,9,0,121,20,2,
		176,20,0,106,17,78,111,116,97,32,70,105,115,99,
		97,108,46,46,46,58,32,0,109,65,0,72,20,1,
		36,207,0,176,19,0,109,9,0,92,30,20,2,176,
		20,0,106,9,86,97,108,111,114,46,58,32,0,20,
		1,36,208,0,176,19,0,109,9,0,92,40,20,2,
		176,68,0,109,69,0,106,16,64,82,32,57,44,57,
		57,57,44,57,57,57,46,57,57,0,20,2,36,209,
		0,176,19,0,109,9,0,92,55,20,2,176,20,0,
		106,14,86,101,110,99,105,109,101,110,116,111,46,58,
		32,0,176,66,0,109,43,0,12,1,72,20,1,36,
		210,0,109,9,0,92,2,72,83,9,0,36,211,0,
		176,19,0,109,9,0,121,20,2,176,20,0,106,43,
		68,105,115,99,114,105,109,105,110,97,135,198,111,32,
		100,111,32,77,97,116,101,114,105,97,108,32,111,117,
		32,83,101,114,118,105,99,111,32,46,46,46,58,32,
		0,20,1,36,212,0,109,9,0,92,2,72,83,9,
		0,36,213,0,176,19,0,109,9,0,121,20,2,176,
		20,0,109,70,0,20,1,36,214,0,109,9,0,92,
		2,72,83,9,0,36,215,0,176,19,0,109,9,0,
		121,20,2,176,20,0,106,17,67,104,101,113,117,101,
		32,78,111,46,46,46,46,46,58,32,0,176,71,0,
		106,2,95,0,92,12,12,2,72,20,1,36,216,0,
		176,19,0,109,9,0,92,30,20,2,176,20,0,106,
		17,66,97,110,99,111,46,46,46,46,46,46,46,46,
		46,58,32,0,176,71,0,106,2,95,0,92,8,12,
		2,72,20,1,36,217,0,176,19,0,109,9,0,92,
		55,20,2,176,20,0,106,14,76,97,110,99,97,109,
		101,110,116,111,46,58,32,0,176,58,0,109,46,0,
		92,5,12,2,72,20,1,36,218,0,109,9,0,92,
		2,72,83,9,0,36,219,0,176,19,0,109,9,0,
		121,20,2,176,20,0,106,17,67,108,97,115,115,105,
		102,105,99,97,99,97,111,46,58,32,0,176,58,0,
		109,73,0,12,1,72,20,1,36,220,0,109,9,0,
		92,2,72,83,9,0,36,221,0,176,19,0,109,9,
		0,121,20,2,176,20,0,106,17,79,98,115,101,114,
		118,97,99,97,111,46,46,46,46,58,32,0,176,71,
		0,106,2,95,0,92,52,12,2,72,20,1,36,222,
		0,109,9,0,92,4,72,83,9,0,36,223,0,176,
		19,0,109,9,0,121,20,2,176,20,0,106,17,65,
		112,114,111,118,97,99,97,111,46,46,46,46,46,58,
		32,0,176,71,0,106,2,95,0,92,52,12,2,72,
		20,1,36,224,0,109,9,0,92,2,72,83,9,0,
		36,225,0,176,19,0,109,9,0,122,20,2,176,20,
		0,176,71,0,106,3,32,95,0,92,38,12,2,20,
		1,36,226,0,109,9,0,92,2,72,83,9,0,36,
		227,0,109,9,0,92,45,16,28,30,36,228,0,121,
		83,9,0,36,229,0,176,19,0,109,9,0,121,20,
		2,176,20,0,106,2,32,0,20,1,36,232,0,176,
		11,0,106,2,49,0,20,1,36,233,0,176,74,0,
		122,20,1,36,234,0,176,75,0,20,0,36,235,0,
		109,76,0,29,39,247,36,236,0,176,56,0,92,24,
		106,1,0,20,2,36,237,0,176,56,0,92,20,106,
		7,83,67,82,69,69,78,0,20,2,36,238,0,176,
		47,0,20,0,176,11,0,106,2,49,0,20,1,176,
		48,0,100,20,1,36,239,0,100,110,7,36,242,0,
		176,56,0,92,24,106,1,0,20,2,36,243,0,176,
		56,0,92,20,106,7,83,67,82,69,69,78,0,20,
		2,36,245,0,176,47,0,20,0,176,11,0,106,2,
		49,0,20,1,176,48,0,100,20,1,36,246,0,176,
		49,0,121,121,176,2,0,12,0,176,3,0,12,0,
		109,4,0,20,5,36,247,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

