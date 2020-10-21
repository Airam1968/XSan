/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_clie.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_CLIE );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
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
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( DBCREATEINDEX );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( ORDSETFOCUS );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_CLIE )
{ "REL_CLIE", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_CLIE )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ETIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "EEMPREEND", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESAIDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "NUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "DBCREATEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCREATEINDEX )}, NULL },
{ "PROPRIET_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "SISLETRAC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ORDSETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( ORDSETFOCUS )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "CLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "OPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_R_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETEL1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_R_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETEL2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_C_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_C_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STATUS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "END_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "BAIRRO_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CIDADE_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESTADO_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "CEP_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "END_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "BAIRRO_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CIDADE_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESTADO_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CEP_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MAIL_PROP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_CLIE, "C:/sis/SAN/xhb/rel_clie.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_CLIE
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_CLIE )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_CLIE )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,106,17,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,0,20,1,36,
		15,0,109,2,0,106,2,83,0,5,29,180,19,36,
		16,0,176,3,0,106,43,82,69,76,65,84,79,82,
		73,79,32,67,65,68,65,83,84,82,79,32,68,69,
		32,67,76,73,69,78,84,69,83,32,80,79,82,32,
		80,82,79,68,85,84,79,0,20,1,36,17,0,176,
		4,0,106,3,48,49,0,20,1,36,18,0,176,5,
		0,122,12,1,83,6,0,36,19,0,176,7,0,92,
		23,92,2,20,2,176,8,0,106,39,79,114,100,101,
		109,32,65,108,102,97,98,130,116,105,99,97,32,111,
		117,32,78,117,109,130,114,105,99,97,32,40,65,47,
		78,41,32,32,58,32,0,20,1,176,9,0,176,10,
		0,12,0,176,11,0,12,0,122,72,20,2,176,12,
		0,109,13,0,176,14,0,100,106,6,101,84,73,80,
		79,0,106,2,88,0,90,12,109,6,0,106,3,65,
		78,0,24,6,100,12,5,20,2,48,15,0,176,16,
		0,109,13,0,12,1,112,0,73,36,20,0,176,17,
		0,109,13,0,100,100,100,100,100,100,20,7,4,0,
		0,83,13,0,109,13,0,73,36,21,0,121,83,18,
		0,36,22,0,176,7,0,92,23,92,2,20,2,176,
		8,0,106,56,73,110,102,111,114,109,101,32,111,32,
		78,163,109,101,114,111,32,100,111,32,69,109,112,114,
		101,101,110,100,105,109,101,110,116,111,32,111,117,32,
		40,48,41,32,112,97,114,97,32,116,111,100,111,115,
		32,58,32,0,20,1,176,9,0,176,10,0,12,0,
		176,11,0,12,0,122,72,20,2,176,12,0,109,13,
		0,176,14,0,100,106,10,101,69,77,80,82,69,69,
		78,68,0,106,8,64,82,32,57,57,57,57,0,100,
		100,12,5,20,2,48,15,0,176,16,0,109,13,0,
		12,1,112,0,73,36,23,0,176,17,0,109,13,0,
		100,100,100,100,100,100,20,7,4,0,0,83,13,0,
		109,13,0,73,36,24,0,176,5,0,92,60,12,1,
		83,19,0,36,25,0,176,20,0,109,21,0,12,1,
		176,5,0,122,12,1,69,28,18,36,26,0,109,21,
		0,106,2,58,0,72,83,19,0,25,13,36,28,0,
		106,3,67,58,0,83,19,0,36,30,0,109,22,0,
		176,5,0,92,30,12,1,69,28,45,36,31,0,176,
		20,0,109,19,0,12,1,109,22,0,72,106,12,69,
		88,84,82,65,84,79,46,84,88,84,0,72,176,5,
		0,92,10,12,1,72,83,19,0,25,39,36,33,0,
		176,20,0,109,19,0,12,1,106,12,69,88,84,82,
		65,84,79,46,84,88,84,0,72,176,5,0,92,10,
		12,1,72,83,19,0,36,36,0,176,5,0,92,10,
		12,1,83,23,0,36,38,0,176,4,0,106,3,48,
		49,0,20,1,36,39,0,176,1,0,106,55,68,101,
		115,101,106,97,32,101,120,116,114,97,116,111,32,101,
		109,32,97,114,113,117,105,118,111,32,116,101,120,116,
		111,32,110,111,32,68,114,105,118,101,32,40,88,58,
		41,32,40,83,47,78,41,32,63,32,0,20,1,36,
		40,0,109,2,0,106,2,83,0,5,29,178,0,36,
		41,0,176,4,0,106,3,48,49,0,20,1,36,42,
		0,176,7,0,92,23,121,20,2,176,8,0,106,23,
		67,79,78,70,73,82,77,69,32,79,32,68,69,83,
		84,73,78,79,32,32,58,32,0,20,1,176,9,0,
		176,10,0,12,0,176,11,0,12,0,122,72,20,2,
		176,12,0,109,13,0,176,14,0,100,106,9,101,65,
		82,81,85,73,86,79,0,100,100,100,12,5,20,2,
		48,15,0,176,16,0,109,13,0,12,1,112,0,73,
		36,43,0,176,17,0,109,13,0,100,100,100,100,100,
		100,20,7,4,0,0,83,13,0,109,13,0,73,36,
		44,0,176,20,0,109,19,0,12,1,83,19,0,36,
		45,0,106,8,65,82,81,85,73,86,79,0,83,23,
		0,36,46,0,25,29,36,48,0,176,24,0,20,0,
		36,49,0,106,11,73,77,80,82,69,83,83,79,82,
		65,0,83,23,0,36,53,0,109,6,0,106,2,65,
		0,5,28,119,36,54,0,176,25,0,106,2,49,0,
		20,1,36,55,0,176,26,0,9,100,106,8,67,79,
		78,84,65,84,79,0,100,100,9,20,6,176,27,0,
		106,8,67,79,78,84,65,84,79,0,20,1,176,27,
		0,106,9,78,85,77,95,67,76,73,69,0,20,1,
		36,56,0,176,28,0,90,8,109,29,0,122,69,6,
		106,14,78,85,77,95,67,76,73,69,32,60,62,32,
		49,0,20,2,36,57,0,176,30,0,20,0,36,58,
		0,176,31,0,122,20,1,25,117,36,60,0,176,25,
		0,106,2,49,0,20,1,36,61,0,176,26,0,9,
		100,106,8,67,79,78,84,65,84,79,0,100,100,9,
		20,6,176,27,0,106,9,78,85,77,95,67,76,73,
		69,0,20,1,176,27,0,106,8,67,79,78,84,65,
		84,79,0,20,1,36,62,0,176,28,0,90,8,109,
		29,0,122,69,6,106,14,78,85,77,95,67,76,73,
		69,32,60,62,32,49,0,20,2,36,63,0,176,30,
		0,20,0,36,64,0,176,31,0,122,20,1,36,66,
		0,176,4,0,106,3,48,49,0,20,1,36,67,0,
		176,32,0,106,19,73,110,100,101,120,97,110,100,111,
		32,86,101,110,100,97,115,32,33,0,20,1,36,68,
		0,176,25,0,106,2,50,0,20,1,36,69,0,176,
		26,0,9,100,106,9,85,78,73,95,73,77,79,66,
		0,100,100,9,20,6,36,70,0,176,30,0,20,0,
		36,71,0,176,33,0,106,9,85,78,73,95,73,77,
		79,49,0,106,11,80,82,79,80,82,73,69,84,95,
		49,0,90,6,109,34,0,6,100,20,4,36,72,0,
		176,30,0,20,0,36,73,0,176,33,0,106,9,85,
		78,73,95,73,77,79,50,0,106,11,80,82,79,80,
		82,73,69,84,95,50,0,90,6,109,35,0,6,100,
		20,4,36,74,0,176,30,0,20,0,36,75,0,176,
		26,0,9,100,106,9,85,78,73,95,73,77,79,66,
		0,100,100,9,20,6,176,27,0,106,9,85,78,73,
		95,73,77,79,49,0,20,1,176,27,0,106,9,85,
		78,73,95,73,77,79,50,0,20,1,36,76,0,176,
		30,0,20,0,36,77,0,109,18,0,121,69,28,63,
		36,78,0,176,28,0,90,17,109,36,0,176,37,0,
		109,18,0,92,5,12,2,5,6,106,26,67,76,65,
		83,83,69,32,61,32,83,84,82,40,101,69,77,80,
		82,69,69,78,68,44,53,41,0,20,2,36,79,0,
		176,30,0,20,0,36,81,0,176,25,0,106,2,50,
		0,20,1,36,82,0,176,30,0,20,0,36,83,0,
		176,38,0,12,0,28,91,36,84,0,176,4,0,106,
		3,48,49,0,20,1,36,85,0,176,39,0,106,10,
		65,116,101,110,135,198,111,32,33,0,92,3,20,2,
		36,86,0,176,32,0,106,33,78,198,111,32,72,160,
		32,82,101,108,97,116,162,114,105,111,32,68,101,115,
		116,101,32,80,101,114,161,111,100,111,32,33,0,20,
		1,36,87,0,176,40,0,20,0,26,172,0,36,89,
		0,109,23,0,106,8,65,82,81,85,73,86,79,0,
		5,28,53,36,90,0,176,41,0,92,23,106,3,79,
		78,0,20,2,36,91,0,176,41,0,92,20,106,8,
		80,82,73,78,84,69,82,0,20,2,36,92,0,176,
		41,0,92,24,109,19,0,9,20,3,25,98,36,94,
		0,176,32,0,106,50,80,111,115,105,99,105,111,110,
		101,32,111,32,70,111,114,109,117,108,97,114,105,111,
		32,56,48,32,67,111,108,117,110,97,115,32,101,32,
		84,101,99,108,101,32,91,69,78,84,69,82,93,0,
		20,1,36,95,0,176,40,0,20,0,36,96,0,176,
		24,0,20,0,36,97,0,176,41,0,92,20,106,8,
		80,82,73,78,84,69,82,0,20,2,36,100,0,176,
		38,0,12,0,32,242,12,36,101,0,176,7,0,122,
		121,20,2,176,8,0,109,42,0,106,11,69,109,112,
		114,101,115,97,32,58,32,0,72,109,43,0,72,20,
		1,36,102,0,176,7,0,122,92,115,20,2,176,8,
		0,106,8,68,97,116,97,32,58,32,0,176,44,0,
		176,45,0,12,0,12,1,72,20,1,36,103,0,176,
		7,0,92,2,121,20,2,176,8,0,106,11,83,105,
		115,116,101,109,97,32,58,32,0,109,46,0,72,20,
		1,36,104,0,176,7,0,92,2,92,115,20,2,176,
		8,0,106,14,82,101,108,97,116,58,32,83,65,78,
		48,48,49,0,20,1,36,105,0,176,7,0,92,3,
		92,40,20,2,176,8,0,109,47,0,106,21,67,65,
		68,65,83,84,82,79,32,68,69,32,67,76,73,69,
		78,84,69,83,0,72,109,42,0,72,20,1,36,106,
		0,176,7,0,92,4,121,20,2,176,8,0,176,48,
		0,106,2,61,0,93,135,0,12,2,20,1,36,107,
		0,176,7,0,92,5,121,20,2,176,8,0,106,2,
		124,0,20,1,36,108,0,176,7,0,92,5,92,2,
		20,2,176,8,0,106,7,67,111,100,105,103,111,0,
		20,1,36,109,0,176,7,0,92,5,92,10,20,2,
		176,8,0,106,5,78,111,109,101,0,20,1,36,110,
		0,176,7,0,92,5,92,50,20,2,176,8,0,106,
		8,80,114,111,100,117,116,111,0,20,1,36,111,0,
		176,7,0,92,5,92,60,20,2,176,8,0,106,6,
		71,114,117,112,111,0,20,1,36,112,0,176,7,0,
		92,5,92,70,20,2,176,8,0,106,6,85,110,105,
		100,46,0,20,1,36,113,0,176,7,0,92,5,92,
		80,20,2,176,8,0,106,6,80,97,114,116,101,0,
		20,1,36,114,0,176,7,0,92,5,92,90,20,2,
		176,8,0,106,9,84,101,108,101,102,111,110,101,0,
		20,1,36,115,0,176,7,0,92,5,93,134,0,20,
		2,176,8,0,106,2,124,0,20,1,36,116,0,176,
		7,0,92,6,121,20,2,176,8,0,176,48,0,106,
		2,61,0,93,135,0,12,2,20,1,36,117,0,92,
		7,83,49,0,36,118,0,176,38,0,12,0,32,196,
		10,109,49,0,92,54,34,29,187,10,36,120,0,176,
		25,0,106,2,49,0,20,1,36,121,0,109,29,0,
		83,50,0,36,122,0,176,25,0,106,2,50,0,20,
		1,36,123,0,176,51,0,122,20,1,36,124,0,176,
		30,0,20,0,36,125,0,176,52,0,109,50,0,100,
		100,20,3,36,126,0,176,53,0,12,0,31,94,36,
		127,0,176,25,0,106,2,50,0,20,1,36,128,0,
		176,51,0,92,2,20,1,36,129,0,176,30,0,20,
		0,36,130,0,176,52,0,109,50,0,100,100,20,3,
		36,131,0,176,53,0,12,0,31,29,36,132,0,176,
		25,0,106,2,49,0,20,1,36,133,0,176,31,0,
		122,20,1,36,134,0,26,106,255,36,136,0,176,25,
		0,106,2,49,0,20,1,36,144,0,176,25,0,106,
		2,50,0,20,1,36,145,0,109,34,0,109,50,0,
		5,29,124,3,36,146,0,176,25,0,106,2,49,0,
		20,1,36,147,0,176,7,0,109,49,0,121,20,2,
		176,8,0,106,2,124,0,20,1,36,148,0,176,7,
		0,109,49,0,122,20,2,176,54,0,109,29,0,106,
		12,64,82,32,57,57,57,57,57,57,57,57,0,20,
		2,36,149,0,176,7,0,109,49,0,92,10,20,2,
		176,54,0,176,20,0,109,55,0,12,1,106,5,64,
		83,51,57,0,20,2,36,150,0,109,56,0,106,2,
		82,0,5,28,22,36,151,0,109,57,0,83,58,0,
		36,152,0,109,59,0,83,60,0,25,20,36,154,0,
		109,61,0,83,58,0,36,155,0,109,62,0,83,60,
		0,36,157,0,176,25,0,106,2,50,0,20,1,36,
		158,0,176,7,0,109,49,0,92,50,20,2,176,54,
		0,109,36,0,106,9,64,82,32,57,57,57,57,57,
		0,20,2,36,159,0,176,7,0,109,49,0,92,60,
		20,2,176,54,0,109,63,0,106,9,64,82,32,57,
		57,57,57,57,0,20,2,36,160,0,176,7,0,109,
		49,0,92,70,20,2,176,54,0,109,64,0,106,9,
		64,82,32,57,57,57,57,57,0,20,2,36,161,0,
		176,7,0,109,49,0,92,80,20,2,176,54,0,109,
		65,0,121,5,28,5,121,25,3,122,106,5,64,82,
		32,57,0,20,2,36,162,0,176,7,0,109,49,0,
		92,85,20,2,176,54,0,109,58,0,106,20,64,82,
		32,40,88,88,41,32,88,88,88,88,88,45,88,88,
		88,88,88,0,20,2,36,163,0,176,7,0,109,49,
		0,92,105,20,2,176,54,0,109,60,0,106,20,64,
		82,32,40,88,88,41,32,88,88,88,88,88,45,88,
		88,88,88,88,0,20,2,36,164,0,176,7,0,109,
		49,0,93,134,0,20,2,176,8,0,106,2,124,0,
		20,1,36,165,0,109,49,0,122,72,83,49,0,36,
		166,0,176,7,0,109,49,0,121,20,2,176,8,0,
		106,2,124,0,20,1,36,167,0,176,25,0,106,2,
		49,0,20,1,36,168,0,109,56,0,106,2,82,0,
		5,29,134,0,36,170,0,176,7,0,109,49,0,92,
		15,20,2,176,8,0,106,14,82,69,83,73,68,69,
		78,67,73,65,76,58,32,0,176,20,0,109,66,0,
		12,1,72,106,4,32,45,32,0,72,176,20,0,109,
		67,0,12,1,72,106,4,32,45,32,0,72,176,20,
		0,109,68,0,12,1,72,106,4,32,45,32,0,72,
		176,20,0,109,69,0,12,1,72,106,4,32,45,32,
		0,72,176,70,0,109,71,0,122,92,5,12,3,72,
		106,2,45,0,72,176,70,0,109,71,0,92,6,92,
		3,12,3,72,20,1,26,131,0,36,173,0,176,7,
		0,109,49,0,92,15,20,2,176,8,0,106,14,67,
		79,77,69,82,67,73,65,76,32,32,58,32,0,176,
		20,0,109,72,0,12,1,72,106,4,32,45,32,0,
		72,176,20,0,109,73,0,12,1,72,106,4,32,45,
		32,0,72,176,20,0,109,74,0,12,1,72,106,4,
		32,45,32,0,72,176,20,0,109,75,0,12,1,72,
		106,4,32,45,32,0,72,176,70,0,109,76,0,122,
		92,5,12,3,72,106,2,45,0,72,176,70,0,109,
		76,0,92,6,92,3,12,3,72,20,1,36,175,0,
		176,7,0,109,49,0,93,134,0,20,2,176,8,0,
		106,2,124,0,20,1,36,176,0,109,49,0,122,72,
		83,49,0,36,177,0,176,7,0,109,49,0,121,20,
		2,176,8,0,106,2,124,0,20,1,36,178,0,176,
		25,0,106,2,49,0,20,1,36,179,0,176,7,0,
		109,49,0,92,15,20,2,176,8,0,106,14,69,45,
		77,65,73,76,32,80,82,79,80,58,32,0,176,20,
		0,109,77,0,12,1,72,20,1,36,180,0,176,7,
		0,109,49,0,93,134,0,20,2,176,8,0,106,2,
		124,0,20,1,36,181,0,176,25,0,106,2,50,0,
		20,1,36,182,0,176,31,0,122,20,1,36,183,0,
		109,49,0,122,72,83,49,0,26,185,3,36,185,0,
		109,35,0,109,50,0,5,29,169,3,36,186,0,176,
		25,0,106,2,49,0,20,1,36,187,0,176,7,0,
		109,49,0,121,20,2,176,8,0,106,2,124,0,20,
		1,36,188,0,176,7,0,109,49,0,122,20,2,176,
		54,0,109,29,0,106,12,64,82,32,57,57,57,57,
		57,57,57,57,0,20,2,36,189,0,176,7,0,109,
		49,0,92,10,20,2,176,54,0,176,20,0,109,55,
		0,12,1,106,5,64,83,51,57,0,20,2,36,190,
		0,109,56,0,106,2,82,0,5,28,22,36,191,0,
		109,57,0,83,58,0,36,192,0,109,59,0,83,60,
		0,25,20,36,194,0,109,61,0,83,58,0,36,195,
		0,109,62,0,83,60,0,36,197,0,176,25,0,106,
		2,50,0,20,1,36,198,0,176,7,0,109,49,0,
		92,50,20,2,176,54,0,109,36,0,106,9,64,82,
		32,57,57,57,57,57,0,20,2,36,199,0,176,7,
		0,109,49,0,92,60,20,2,176,54,0,109,63,0,
		106,9,64,82,32,57,57,57,57,57,0,20,2,36,
		200,0,176,7,0,109,49,0,92,70,20,2,176,54,
		0,109,64,0,106,9,64,82,32,57,57,57,57,57,
		0,20,2,36,201,0,176,7,0,109,49,0,92,80,
		20,2,176,8,0,106,2,50,0,20,1,36,202,0,
		176,7,0,109,49,0,92,85,20,2,176,54,0,109,
		58,0,106,20,64,82,32,40,88,88,41,32,88,88,
		88,88,88,45,88,88,88,88,88,0,20,2,36,203,
		0,176,7,0,109,49,0,92,105,20,2,176,54,0,
		109,60,0,106,20,64,82,32,40,88,88,41,32,88,
		88,88,88,88,45,88,88,88,88,88,0,20,2,36,
		204,0,176,7,0,109,49,0,93,134,0,20,2,176,
		8,0,106,2,124,0,20,1,36,205,0,109,49,0,
		122,72,83,49,0,36,206,0,176,7,0,109,49,0,
		121,20,2,176,8,0,106,2,124,0,20,1,36,207,
		0,176,25,0,106,2,49,0,20,1,36,208,0,109,
		56,0,106,2,82,0,5,29,134,0,36,210,0,176,
		7,0,109,49,0,92,15,20,2,176,8,0,106,14,
		82,69,83,73,68,69,78,67,73,65,76,58,32,0,
		176,20,0,109,66,0,12,1,72,106,4,32,45,32,
		0,72,176,20,0,109,67,0,12,1,72,106,4,32,
		45,32,0,72,176,20,0,109,68,0,12,1,72,106,
		4,32,45,32,0,72,176,20,0,109,69,0,12,1,
		72,106,4,32,45,32,0,72,176,70,0,109,71,0,
		122,92,5,12,3,72,106,2,45,0,72,176,70,0,
		109,71,0,92,6,92,3,12,3,72,20,1,26,131,
		0,36,213,0,176,7,0,109,49,0,92,15,20,2,
		176,8,0,106,14,67,79,77,69,82,67,73,65,76,
		32,32,58,32,0,176,20,0,109,72,0,12,1,72,
		106,4,32,45,32,0,72,176,20,0,109,73,0,12,
		1,72,106,4,32,45,32,0,72,176,20,0,109,74,
		0,12,1,72,106,4,32,45,32,0,72,176,20,0,
		109,75,0,12,1,72,106,4,32,45,32,0,72,176,
		70,0,109,76,0,122,92,5,12,3,72,106,2,45,
		0,72,176,70,0,109,76,0,92,6,92,3,12,3,
		72,20,1,36,215,0,176,7,0,109,49,0,93,134,
		0,20,2,176,8,0,106,2,124,0,20,1,36,216,
		0,109,49,0,122,72,83,49,0,36,217,0,176,7,
		0,109,49,0,121,20,2,176,8,0,106,2,124,0,
		20,1,36,218,0,176,25,0,106,2,49,0,20,1,
		36,219,0,176,7,0,109,49,0,92,15,20,2,176,
		8,0,106,14,69,45,77,65,73,76,32,80,82,79,
		80,58,32,0,176,20,0,109,77,0,12,1,72,20,
		1,36,220,0,176,7,0,109,49,0,93,134,0,20,
		2,176,8,0,106,2,124,0,20,1,36,221,0,176,
		25,0,106,2,50,0,20,1,36,222,0,176,31,0,
		122,20,1,36,223,0,109,49,0,122,72,83,49,0,
		36,224,0,176,78,0,20,0,36,225,0,109,79,0,
		28,48,36,226,0,176,41,0,92,20,106,7,83,67,
		82,69,69,78,0,20,2,36,227,0,176,80,0,20,
		0,176,25,0,106,2,49,0,20,1,176,81,0,100,
		20,1,36,228,0,7,36,232,0,176,25,0,106,2,
		50,0,20,1,36,233,0,109,34,0,109,50,0,69,
		28,11,109,35,0,109,50,0,69,31,10,176,38,0,
		12,0,29,145,0,36,234,0,176,25,0,106,2,49,
		0,20,1,36,235,0,176,31,0,122,20,1,36,236,
		0,176,78,0,20,0,36,237,0,109,79,0,29,182,
		247,36,238,0,109,23,0,106,8,65,82,81,85,73,
		86,79,0,5,28,39,36,239,0,176,41,0,92,23,
		106,4,79,70,70,0,20,2,36,240,0,176,41,0,
		92,20,106,7,83,67,82,69,69,78,0,20,2,25,
		21,36,242,0,176,41,0,92,20,106,7,83,67,82,
		69,69,78,0,20,2,36,244,0,176,80,0,20,0,
		176,25,0,106,2,49,0,20,1,176,81,0,100,20,
		1,36,245,0,100,110,7,36,249,0,109,49,0,92,
		54,16,29,251,247,36,250,0,176,7,0,92,58,121,
		20,2,176,8,0,176,48,0,106,2,61,0,93,135,
		0,12,2,20,1,36,251,0,176,7,0,92,59,92,
		104,20,2,176,8,0,109,82,0,20,1,36,252,0,
		176,7,0,122,121,20,2,176,8,0,106,2,32,0,
		109,83,0,72,20,1,36,253,0,176,7,0,122,121,
		20,2,176,8,0,109,42,0,106,11,69,109,112,114,
		101,115,97,32,58,32,0,72,109,43,0,72,20,1,
		36,254,0,176,7,0,122,92,115,20,2,176,8,0,
		106,8,68,97,116,97,32,58,32,0,176,44,0,176,
		45,0,12,0,12,1,72,20,1,36,255,0,176,7,
		0,92,2,121,20,2,176,8,0,106,11,83,105,115,
		116,101,109,97,32,58,32,0,109,46,0,72,20,1,
		36,0,1,176,7,0,92,2,92,115,20,2,176,8,
		0,106,14,82,101,108,97,116,58,32,83,65,78,48,
		48,49,0,20,1,36,1,1,176,7,0,92,3,92,
		40,20,2,176,8,0,109,47,0,106,21,67,65,68,
		65,83,84,82,79,32,68,69,32,67,76,73,69,78,
		84,69,83,0,72,109,42,0,72,20,1,36,2,1,
		176,7,0,92,4,121,20,2,176,8,0,176,48,0,
		106,2,61,0,93,135,0,12,2,20,1,36,3,1,
		176,7,0,92,5,121,20,2,176,8,0,106,2,124,
		0,20,1,36,4,1,176,7,0,92,5,92,2,20,
		2,176,8,0,106,7,67,111,100,105,103,111,0,20,
		1,36,5,1,176,7,0,92,5,92,10,20,2,176,
		8,0,106,5,78,111,109,101,0,20,1,36,6,1,
		176,7,0,92,5,92,50,20,2,176,8,0,106,9,
		80,114,111,100,117,116,111,46,0,20,1,36,7,1,
		176,7,0,92,5,92,60,20,2,176,8,0,106,6,
		71,114,117,112,111,0,20,1,36,8,1,176,7,0,
		92,5,92,70,20,2,176,8,0,106,6,85,110,105,
		100,46,0,20,1,36,9,1,176,7,0,92,5,92,
		80,20,2,176,8,0,106,6,80,97,114,116,101,0,
		20,1,36,10,1,176,7,0,92,5,93,134,0,20,
		2,176,8,0,106,2,124,0,20,1,36,11,1,176,
		7,0,92,6,121,20,2,176,8,0,176,48,0,106,
		2,61,0,93,135,0,12,2,20,1,36,12,1,92,
		7,83,49,0,36,13,1,26,240,245,36,16,1,176,
		7,0,92,56,121,20,2,176,8,0,176,48,0,106,
		2,61,0,93,135,0,12,2,20,1,36,17,1,176,
		7,0,92,57,92,104,20,2,176,8,0,109,82,0,
		20,1,36,18,1,176,7,0,122,121,20,2,176,8,
		0,106,2,32,0,109,83,0,72,20,1,26,12,243,
		36,21,1,109,23,0,106,8,65,82,81,85,73,86,
		79,0,5,28,52,36,22,1,176,41,0,92,24,106,
		1,0,20,2,36,23,1,176,41,0,92,23,106,4,
		79,70,70,0,20,2,36,24,1,176,41,0,92,20,
		106,7,83,67,82,69,69,78,0,20,2,25,34,36,
		26,1,176,41,0,92,24,106,1,0,20,2,36,27,
		1,176,41,0,92,20,106,7,83,67,82,69,69,78,
		0,20,2,36,29,1,176,80,0,20,0,36,30,1,
		176,80,0,20,0,176,25,0,106,2,49,0,20,1,
		176,81,0,100,20,1,36,31,1,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

