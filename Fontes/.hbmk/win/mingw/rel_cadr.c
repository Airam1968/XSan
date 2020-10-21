/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_cadr.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_CADR );
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
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_CADR )
{ "REL_CADR", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_CADR )}, NULL },
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
{ "EARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESAIDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "SISLETRAC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "NUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "OPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "END_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CIDADE_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_R_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "END_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CIDADE_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_C_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_CADR, "C:/sis/SAN/xhb/rel_cadr.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_CADR
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_CADR )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_CADR )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,106,17,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,0,20,1,36,
		14,0,109,2,0,106,2,83,0,5,29,113,8,36,
		15,0,176,3,0,106,26,82,69,76,65,84,79,82,
		73,79,32,67,65,68,65,83,84,82,79,32,82,69,
		83,85,77,79,0,20,1,36,16,0,176,4,0,106,
		3,48,49,0,20,1,36,17,0,176,5,0,122,12,
		1,83,6,0,36,18,0,176,7,0,92,23,92,2,
		20,2,176,8,0,106,39,79,114,100,101,109,32,65,
		108,102,97,98,130,116,105,99,97,32,111,117,32,78,
		117,109,130,114,105,99,97,32,40,65,47,78,41,32,
		32,58,32,0,20,1,176,9,0,176,10,0,12,0,
		176,11,0,12,0,122,72,20,2,176,12,0,109,13,
		0,176,14,0,100,106,6,101,84,73,80,79,0,106,
		2,88,0,90,12,109,6,0,106,3,65,78,0,24,
		6,100,12,5,20,2,48,15,0,176,16,0,109,13,
		0,12,1,112,0,73,36,19,0,176,17,0,109,13,
		0,100,100,100,100,100,100,20,7,4,0,0,83,13,
		0,109,13,0,73,36,20,0,176,5,0,92,60,12,
		1,83,18,0,36,21,0,176,19,0,109,20,0,12,
		1,176,5,0,122,12,1,69,28,18,36,22,0,109,
		20,0,106,2,58,0,72,83,18,0,25,13,36,24,
		0,106,3,67,58,0,83,18,0,36,27,0,109,21,
		0,176,5,0,92,30,12,1,69,28,45,36,28,0,
		176,19,0,109,18,0,12,1,109,21,0,72,106,12,
		69,88,84,82,65,84,79,46,84,88,84,0,72,176,
		5,0,92,10,12,1,72,83,18,0,25,39,36,30,
		0,176,19,0,109,18,0,12,1,106,12,69,88,84,
		82,65,84,79,46,84,88,84,0,72,176,5,0,92,
		10,12,1,72,83,18,0,36,32,0,176,5,0,92,
		10,12,1,83,22,0,36,34,0,176,4,0,106,3,
		48,49,0,20,1,36,35,0,176,1,0,106,55,68,
		101,115,101,106,97,32,101,120,116,114,97,116,111,32,
		101,109,32,97,114,113,117,105,118,111,32,116,101,120,
		116,111,32,110,111,32,68,114,105,118,101,32,40,88,
		58,41,32,40,83,47,78,41,32,63,32,0,20,1,
		36,36,0,109,2,0,106,2,83,0,5,29,178,0,
		36,37,0,176,4,0,106,3,48,49,0,20,1,36,
		38,0,176,7,0,92,23,121,20,2,176,8,0,106,
		23,67,79,78,70,73,82,77,69,32,79,32,68,69,
		83,84,73,78,79,32,32,58,32,0,20,1,176,9,
		0,176,10,0,12,0,176,11,0,12,0,122,72,20,
		2,176,12,0,109,13,0,176,14,0,100,106,9,101,
		65,82,81,85,73,86,79,0,100,100,100,12,5,20,
		2,48,15,0,176,16,0,109,13,0,12,1,112,0,
		73,36,39,0,176,17,0,109,13,0,100,100,100,100,
		100,100,20,7,4,0,0,83,13,0,109,13,0,73,
		36,40,0,176,19,0,109,18,0,12,1,83,18,0,
		36,41,0,106,8,65,82,81,85,73,86,79,0,83,
		22,0,36,42,0,25,29,36,44,0,176,23,0,20,
		0,36,45,0,106,11,73,77,80,82,69,83,83,79,
		82,65,0,83,22,0,36,49,0,176,24,0,106,50,
		80,111,115,105,99,105,111,110,101,32,111,32,70,111,
		114,109,117,108,97,114,105,111,32,56,48,32,67,111,
		108,117,110,97,115,32,101,32,84,101,99,108,101,32,
		91,69,78,84,69,82,93,0,20,1,36,50,0,176,
		25,0,20,0,36,51,0,109,6,0,106,2,65,0,
		5,28,78,36,52,0,176,26,0,106,2,49,0,20,
		1,36,53,0,176,27,0,9,100,106,8,67,79,78,
		84,65,84,79,0,100,100,9,20,6,176,28,0,106,
		8,67,79,78,84,65,84,79,0,20,1,176,28,0,
		106,9,78,85,77,95,67,76,73,69,0,20,1,36,
		54,0,176,29,0,20,0,25,76,36,57,0,176,26,
		0,106,2,49,0,20,1,36,58,0,176,27,0,9,
		100,106,8,67,79,78,84,65,84,79,0,100,100,9,
		20,6,176,28,0,106,9,78,85,77,95,67,76,73,
		69,0,20,1,176,28,0,106,8,67,79,78,84,65,
		84,79,0,20,1,36,59,0,176,29,0,20,0,36,
		62,0,176,30,0,12,0,28,95,36,63,0,176,4,
		0,106,3,48,49,0,20,1,36,64,0,176,31,0,
		106,10,65,116,101,110,135,198,111,32,33,0,92,3,
		20,2,36,65,0,176,24,0,106,37,78,198,111,32,
		72,160,32,82,101,108,97,116,162,114,105,111,32,112,
		97,114,97,32,101,115,116,97,32,83,101,108,101,135,
		198,111,32,33,0,20,1,36,66,0,176,25,0,20,
		0,26,172,0,36,68,0,109,22,0,106,8,65,82,
		81,85,73,86,79,0,5,28,53,36,69,0,176,32,
		0,92,23,106,3,79,78,0,20,2,36,70,0,176,
		32,0,92,20,106,8,80,82,73,78,84,69,82,0,
		20,2,36,71,0,176,32,0,92,24,109,18,0,9,
		20,3,25,98,36,73,0,176,24,0,106,50,80,111,
		115,105,99,105,111,110,101,32,111,32,70,111,114,109,
		117,108,97,114,105,111,32,56,48,32,67,111,108,117,
		110,97,115,32,101,32,84,101,99,108,101,32,91,69,
		78,84,69,82,93,0,20,1,36,74,0,176,25,0,
		20,0,36,75,0,176,23,0,20,0,36,76,0,176,
		32,0,92,20,106,8,80,82,73,78,84,69,82,0,
		20,2,36,79,0,176,30,0,12,0,32,203,3,36,
		80,0,176,7,0,122,121,20,2,176,8,0,109,33,
		0,106,11,69,109,112,114,101,115,97,32,58,32,0,
		72,109,34,0,72,20,1,36,81,0,176,7,0,122,
		92,115,20,2,176,8,0,106,8,68,97,116,97,32,
		58,32,0,176,35,0,176,36,0,12,0,12,1,72,
		20,1,36,82,0,176,7,0,92,2,121,20,2,176,
		8,0,106,11,83,105,115,116,101,109,97,32,58,32,
		0,109,37,0,72,20,1,36,83,0,176,7,0,92,
		2,92,115,20,2,176,8,0,106,14,82,101,108,97,
		116,58,32,83,65,78,48,48,49,0,20,1,36,84,
		0,176,7,0,92,3,92,40,20,2,176,8,0,109,
		38,0,106,28,67,65,68,65,83,84,82,79,32,68,
		69,32,67,76,73,69,78,84,69,83,32,82,69,83,
		85,77,79,0,72,109,33,0,72,20,1,36,85,0,
		176,7,0,92,4,121,20,2,176,8,0,176,39,0,
		106,2,61,0,93,135,0,12,2,20,1,36,86,0,
		176,7,0,92,5,92,2,20,2,176,8,0,106,7,
		67,79,68,73,71,79,0,20,1,36,87,0,176,7,
		0,92,5,92,10,20,2,176,8,0,106,5,78,79,
		77,69,0,20,1,36,88,0,176,7,0,92,5,92,
		50,20,2,176,8,0,106,9,69,78,68,69,82,69,
		67,79,0,20,1,36,89,0,176,7,0,92,5,92,
		90,20,2,176,8,0,106,7,67,73,68,65,68,69,
		0,20,1,36,90,0,176,7,0,92,5,92,112,20,
		2,176,8,0,106,9,84,69,76,69,70,79,78,69,
		0,20,1,36,91,0,176,7,0,92,6,121,20,2,
		176,8,0,176,39,0,106,2,61,0,93,135,0,12,
		2,20,1,36,92,0,92,7,83,40,0,36,93,0,
		176,30,0,12,0,32,7,2,109,40,0,92,54,34,
		29,254,1,36,94,0,176,7,0,109,40,0,122,20,
		2,176,41,0,109,42,0,106,12,64,82,32,57,57,
		57,57,57,57,57,57,0,20,2,36,95,0,176,7,
		0,109,40,0,92,10,20,2,176,41,0,176,19,0,
		109,43,0,12,1,106,5,64,83,51,57,0,20,2,
		36,96,0,109,44,0,106,2,82,0,5,29,147,0,
		36,97,0,176,7,0,109,40,0,92,50,20,2,176,
		41,0,176,19,0,109,45,0,12,1,106,5,64,83,
		51,57,0,20,2,36,98,0,176,7,0,109,40,0,
		92,90,20,2,176,41,0,176,19,0,109,46,0,12,
		1,106,5,64,83,50,49,0,20,2,36,99,0,176,
		7,0,109,40,0,92,112,20,2,176,41,0,109,47,
		0,106,29,64,82,32,40,88,88,41,32,88,88,88,
		88,88,45,88,88,88,88,88,32,45,32,88,88,88,
		88,88,88,0,20,2,36,100,0,176,7,0,109,40,
		0,93,133,0,20,2,176,8,0,106,2,82,0,20,
		1,26,144,0,36,102,0,176,7,0,109,40,0,92,
		50,20,2,176,41,0,176,19,0,109,48,0,12,1,
		106,5,64,83,51,57,0,20,2,36,103,0,176,7,
		0,109,40,0,92,90,20,2,176,41,0,176,19,0,
		109,49,0,12,1,106,5,64,83,50,49,0,20,2,
		36,104,0,176,7,0,109,40,0,92,112,20,2,176,
		41,0,109,50,0,106,29,64,82,32,40,88,88,41,
		32,88,88,88,88,88,45,88,88,88,88,88,32,45,
		32,88,88,88,88,88,88,0,20,2,36,105,0,176,
		7,0,109,40,0,93,133,0,20,2,176,8,0,106,
		2,67,0,20,1,36,107,0,109,40,0,122,72,83,
		40,0,36,108,0,176,51,0,122,20,1,36,109,0,
		176,52,0,20,0,36,110,0,109,53,0,29,92,254,
		36,111,0,109,22,0,106,8,65,82,81,85,73,86,
		79,0,5,28,39,36,112,0,176,32,0,92,23,106,
		4,79,70,70,0,20,2,36,113,0,176,32,0,92,
		20,106,7,83,67,82,69,69,78,0,20,2,25,21,
		36,115,0,176,32,0,92,20,106,7,83,67,82,69,
		69,78,0,20,2,36,117,0,176,54,0,20,0,176,
		26,0,106,2,49,0,20,1,176,55,0,100,20,1,
		36,118,0,100,110,7,36,121,0,176,7,0,92,56,
		121,20,2,176,8,0,176,39,0,106,2,61,0,93,
		135,0,12,2,20,1,36,122,0,176,7,0,92,57,
		92,95,20,2,176,8,0,109,56,0,20,1,26,51,
		252,36,126,0,109,22,0,106,8,65,82,81,85,73,
		86,79,0,5,28,52,36,127,0,176,32,0,92,24,
		106,1,0,20,2,36,128,0,176,32,0,92,23,106,
		4,79,70,70,0,20,2,36,129,0,176,32,0,92,
		20,106,7,83,67,82,69,69,78,0,20,2,25,34,
		36,131,0,176,32,0,92,24,106,1,0,20,2,36,
		132,0,176,32,0,92,20,106,7,83,67,82,69,69,
		78,0,20,2,36,134,0,176,54,0,20,0,176,26,
		0,106,2,49,0,20,1,176,55,0,100,20,1,36,
		135,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

