/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/mal_forn.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MAL_FORN );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
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
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( PCENTRA );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MAL_FORN )
{ "MAL_FORN", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( MAL_FORN )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECOPIA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "EREMETENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ENOME_REME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "CIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECID_EMPRESA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "SELECAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "EAUX", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "PEXT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETIT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SEXO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NOME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "END", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "BAIRRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESTADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CEP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "LINHA_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_4", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_5", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_6", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_7", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_8", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_9", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_10", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_11", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_12", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_13", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_14", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_15", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_16", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_17", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_18", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LINHA_19", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PCENTRA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCENTRA )}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MAL_FORN, "C:/sis/SAN/xhb/mal_forn.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MAL_FORN
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MAL_FORN )
   #include "hbiniseg.h"
#endif

HB_FUNC( MAL_FORN )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,97,108,95,102,111,114,110,46,112,114,103,
		58,77,65,76,95,70,79,82,78,0,36,14,0,176,
		1,0,106,3,48,49,0,20,1,36,15,0,176,2,
		0,106,17,67,111,110,102,105,114,109,97,32,63,32,
		40,83,47,78,41,0,20,1,36,16,0,109,3,0,
		106,2,78,0,5,28,8,36,17,0,100,110,7,36,
		19,0,176,1,0,106,3,48,49,0,20,1,36,20,
		0,36,21,0,121,83,4,0,36,22,0,176,5,0,
		92,23,121,20,2,176,6,0,106,45,73,110,102,111,
		114,109,101,32,111,32,78,117,109,101,114,111,32,100,
		101,32,67,111,112,105,97,115,32,112,111,114,32,70,
		111,114,110,101,99,101,100,111,114,32,58,32,0,20,
		1,176,7,0,176,8,0,12,0,176,9,0,12,0,
		122,72,20,2,176,10,0,109,11,0,176,12,0,100,
		106,7,101,67,79,80,73,65,0,106,3,57,57,0,
		100,100,12,5,20,2,48,13,0,176,14,0,109,11,
		0,12,1,112,0,73,36,23,0,176,15,0,109,11,
		0,100,100,100,100,100,100,20,7,4,0,0,83,11,
		0,109,11,0,73,36,24,0,109,4,0,121,69,29,
		87,255,36,25,0,36,28,0,176,1,0,106,3,48,
		49,0,20,1,36,29,0,176,2,0,106,64,68,101,
		115,101,106,97,32,69,109,105,116,105,114,32,97,115,
		32,67,97,114,116,97,115,32,84,101,110,100,111,32,
		99,111,109,111,32,82,101,109,101,116,101,110,116,101,
		32,97,32,69,109,112,114,101,115,97,32,40,83,47,
		78,41,32,63,32,0,20,1,36,30,0,109,3,0,
		106,2,83,0,5,28,12,36,31,0,120,83,16,0,
		26,168,0,36,33,0,9,83,16,0,36,34,0,36,
		35,0,176,17,0,92,40,12,1,83,18,0,36,36,
		0,176,5,0,92,23,121,20,2,176,6,0,106,13,
		82,101,109,101,116,101,110,116,101,32,58,32,0,20,
		1,176,7,0,176,8,0,12,0,176,9,0,12,0,
		122,72,20,2,176,10,0,109,11,0,176,12,0,100,
		106,11,101,78,79,77,69,95,82,69,77,69,0,100,
		100,100,12,5,20,2,48,13,0,176,14,0,109,11,
		0,12,1,112,0,73,36,37,0,176,15,0,109,11,
		0,100,100,100,100,100,100,20,7,4,0,0,83,11,
		0,109,11,0,73,36,38,0,109,18,0,176,17,0,
		92,40,12,1,69,29,107,255,36,39,0,36,43,0,
		176,1,0,106,3,48,49,0,20,1,36,44,0,176,
		19,0,106,43,65,103,117,97,114,100,101,44,32,80,
		114,101,112,97,114,97,110,100,111,32,78,111,109,101,
		115,32,83,101,108,101,99,105,111,110,97,100,111,115,
		32,46,46,46,0,20,1,36,45,0,176,20,0,106,
		2,51,0,20,1,36,46,0,176,21,0,9,100,106,
		7,83,84,65,84,85,83,0,100,100,9,20,6,36,
		47,0,176,22,0,20,0,36,48,0,109,23,0,83,
		24,0,36,49,0,176,20,0,106,2,50,0,20,1,
		36,50,0,176,21,0,9,100,106,9,67,65,82,95,
		80,65,68,82,0,100,100,9,20,6,176,25,0,106,
		9,67,65,82,95,80,65,68,82,0,20,1,36,51,
		0,176,26,0,90,49,51,67,58,47,115,105,115,47,
		83,65,78,47,120,104,98,47,109,97,108,95,102,111,
		114,110,46,112,114,103,58,77,65,76,95,70,79,82,
		78,0,109,27,0,106,2,88,0,5,6,106,14,83,
		69,76,69,67,65,79,32,61,32,34,88,34,0,20,
		2,36,52,0,176,22,0,20,0,36,53,0,176,20,
		0,106,2,49,0,20,1,36,54,0,176,21,0,9,
		100,106,9,70,79,82,78,69,67,69,68,0,100,100,
		9,20,6,176,25,0,106,9,70,79,82,78,69,67,
		69,68,0,20,1,36,55,0,176,26,0,90,49,51,
		67,58,47,115,105,115,47,83,65,78,47,120,104,98,
		47,109,97,108,95,102,111,114,110,46,112,114,103,58,
		77,65,76,95,70,79,82,78,0,109,27,0,106,2,
		88,0,5,6,106,14,83,69,76,69,67,65,79,32,
		61,32,34,88,34,0,20,2,36,56,0,176,22,0,
		20,0,36,57,0,176,28,0,12,0,28,95,36,58,
		0,176,1,0,106,3,48,49,0,20,1,36,59,0,
		176,19,0,106,62,78,198,111,32,72,160,32,70,111,
		114,110,101,99,101,100,111,114,115,32,115,101,108,101,
		99,105,111,110,97,100,111,115,44,32,84,101,99,108,
		101,32,91,69,78,84,69,82,93,32,112,97,114,97,
		32,102,105,110,97,108,105,122,97,114,0,20,1,36,
		60,0,176,29,0,20,0,36,62,0,176,1,0,106,
		3,48,49,0,20,1,36,63,0,176,19,0,106,60,
		80,111,115,105,99,105,111,110,101,32,111,32,80,97,
		112,101,108,32,110,97,32,105,109,112,114,101,115,115,
		111,114,97,44,32,84,101,99,108,101,32,91,69,78,
		84,69,82,93,32,112,97,114,97,32,99,111,109,101,
		99,97,114,0,20,1,36,64,0,176,29,0,20,0,
		36,65,0,176,30,0,20,0,36,66,0,176,1,0,
		106,3,48,49,0,20,1,36,67,0,176,19,0,106,
		12,65,103,117,97,114,100,101,32,46,46,46,0,20,
		1,36,68,0,176,31,0,92,20,106,8,80,82,73,
		78,84,69,82,0,20,2,36,69,0,122,83,32,0,
		36,70,0,176,20,0,106,2,49,0,20,1,36,71,
		0,176,22,0,20,0,36,72,0,176,28,0,12,0,
		32,48,4,36,73,0,176,31,0,92,20,106,8,80,
		82,73,78,84,69,82,0,20,2,36,74,0,122,83,
		33,0,36,75,0,109,33,0,109,4,0,34,29,240,
		3,36,77,0,176,5,0,92,5,92,50,20,2,176,
		6,0,176,34,0,109,24,0,12,1,106,2,44,0,
		72,20,1,36,78,0,176,5,0,92,6,92,50,20,
		2,176,6,0,109,35,0,106,2,46,0,72,20,1,
		36,79,0,176,17,0,92,20,12,1,83,36,0,36,
		80,0,176,20,0,106,2,49,0,20,1,36,81,0,
		109,37,0,106,2,77,0,5,28,21,36,82,0,106,
		9,73,108,109,111,32,83,114,46,0,83,36,0,25,
		48,36,84,0,109,37,0,106,2,70,0,5,28,22,
		36,85,0,106,10,73,108,109,97,32,83,114,97,46,
		0,83,36,0,25,12,36,87,0,106,2,65,0,83,
		36,0,36,90,0,176,5,0,92,12,92,5,20,2,
		176,6,0,109,36,0,20,1,36,91,0,176,5,0,
		92,14,92,5,20,2,176,6,0,109,38,0,20,1,
		36,92,0,176,5,0,92,15,92,5,20,2,176,6,
		0,109,39,0,20,1,36,93,0,176,5,0,92,16,
		92,5,20,2,176,6,0,176,34,0,109,40,0,12,
		1,106,4,32,45,32,0,72,176,34,0,109,23,0,
		12,1,72,106,4,32,45,32,0,72,176,34,0,109,
		41,0,12,1,72,20,1,36,94,0,109,42,0,176,
		17,0,92,8,12,1,69,28,54,36,95,0,176,5,
		0,92,17,92,5,20,2,176,43,0,106,5,67,69,
		80,32,0,176,34,0,109,42,0,12,1,72,106,17,
		64,82,32,88,88,88,88,88,88,88,88,88,45,88,
		88,88,0,20,2,36,97,0,176,20,0,106,2,50,
		0,20,1,36,98,0,176,5,0,92,21,92,5,20,
		2,176,6,0,109,44,0,20,1,36,99,0,176,5,
		0,92,22,92,5,20,2,176,6,0,109,45,0,20,
		1,36,100,0,176,5,0,92,23,92,5,20,2,176,
		6,0,109,46,0,20,1,36,101,0,176,5,0,92,
		24,92,5,20,2,176,6,0,109,47,0,20,1,36,
		102,0,176,5,0,92,25,92,5,20,2,176,6,0,
		109,48,0,20,1,36,103,0,176,5,0,92,26,92,
		5,20,2,176,6,0,109,49,0,20,1,36,104,0,
		176,5,0,92,27,92,5,20,2,176,6,0,109,50,
		0,20,1,36,105,0,176,5,0,92,28,92,5,20,
		2,176,6,0,109,51,0,20,1,36,106,0,176,5,
		0,92,29,92,5,20,2,176,6,0,109,52,0,20,
		1,36,107,0,176,5,0,92,30,92,5,20,2,176,
		6,0,109,53,0,20,1,36,108,0,176,5,0,92,
		31,92,5,20,2,176,6,0,109,54,0,20,1,36,
		109,0,176,5,0,92,32,92,5,20,2,176,6,0,
		109,55,0,20,1,36,110,0,176,5,0,92,33,92,
		5,20,2,176,6,0,109,56,0,20,1,36,111,0,
		176,5,0,92,34,92,5,20,2,176,6,0,109,57,
		0,20,1,36,112,0,176,5,0,92,35,92,5,20,
		2,176,6,0,109,58,0,20,1,36,113,0,176,5,
		0,92,36,92,5,20,2,176,6,0,109,59,0,20,
		1,36,114,0,176,5,0,92,37,92,5,20,2,176,
		6,0,109,60,0,20,1,36,115,0,176,5,0,92,
		38,92,5,20,2,176,6,0,109,61,0,20,1,36,
		116,0,176,5,0,92,39,92,5,20,2,176,6,0,
		109,62,0,20,1,36,117,0,109,16,0,28,82,36,
		118,0,176,63,0,106,40,45,45,45,45,45,45,45,
		45,45,45,45,45,45,45,45,45,45,45,45,45,45,
		45,45,45,45,45,45,45,45,45,45,45,45,45,45,
		45,45,45,45,0,92,43,20,2,36,119,0,176,63,
		0,109,64,0,92,44,20,2,36,120,0,176,63,0,
		109,64,0,92,44,20,2,25,80,36,122,0,176,63,
		0,106,40,45,45,45,45,45,45,45,45,45,45,45,
		45,45,45,45,45,45,45,45,45,45,45,45,45,45,
		45,45,45,45,45,45,45,45,45,45,45,45,45,45,
		0,92,43,20,2,36,123,0,176,63,0,109,18,0,
		92,44,20,2,36,124,0,176,63,0,109,18,0,92,
		44,20,2,36,126,0,176,5,0,121,121,20,2,176,
		6,0,106,2,32,0,20,1,36,127,0,109,33,0,
		122,72,83,33,0,36,128,0,176,20,0,106,2,49,
		0,20,1,36,129,0,176,65,0,20,0,36,130,0,
		109,66,0,29,57,252,36,131,0,176,31,0,92,20,
		106,7,83,67,82,69,69,78,0,20,2,36,132,0,
		176,67,0,20,0,176,20,0,106,2,49,0,20,1,
		176,68,0,100,20,1,36,133,0,100,110,7,36,136,
		0,176,20,0,106,2,49,0,20,1,36,137,0,176,
		69,0,122,20,1,26,203,251,36,139,0,176,31,0,
		92,20,106,7,83,67,82,69,69,78,0,20,2,36,
		140,0,176,67,0,20,0,36,141,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,97,108,95,102,111,114,110,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,18,192,251,243,254,248,255,
		223,255,239,183,252,254,255,255,221,63,59,0,4,3,
		0,4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

