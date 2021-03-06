/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_tipo.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_TIPO );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_TIPO )
{ "REL_TIPO", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_TIPO )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "NPAG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "SISLETRAC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_TIPO, "C:/sis/SAN/xhb/rel_tipo.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_TIPO
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_TIPO )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_TIPO )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,106,17,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,0,20,1,36,
		15,0,109,2,0,106,2,83,0,5,29,203,3,36,
		16,0,176,3,0,106,35,82,69,76,65,84,79,82,
		73,79,32,84,73,80,79,32,68,69,32,73,84,69,
		78,83,32,68,69,32,69,83,84,79,81,85,69,0,
		20,1,36,17,0,176,4,0,106,2,49,0,20,1,
		36,18,0,176,5,0,9,100,106,9,65,82,81,95,
		84,73,80,79,0,100,100,9,20,6,176,6,0,106,
		9,65,82,81,95,84,73,80,79,0,20,1,36,19,
		0,176,7,0,20,0,36,20,0,176,8,0,92,60,
		12,1,83,9,0,36,21,0,176,10,0,109,11,0,
		12,1,176,8,0,122,12,1,69,28,18,36,22,0,
		109,11,0,106,2,58,0,72,83,9,0,25,13,36,
		24,0,106,3,67,58,0,83,9,0,36,27,0,109,
		12,0,176,8,0,92,30,12,1,69,28,45,36,28,
		0,176,10,0,109,9,0,12,1,109,12,0,72,106,
		12,69,88,84,82,65,84,79,46,84,88,84,0,72,
		176,8,0,92,10,12,1,72,83,9,0,25,39,36,
		30,0,176,10,0,109,9,0,12,1,106,12,69,88,
		84,82,65,84,79,46,84,88,84,0,72,176,8,0,
		92,10,12,1,72,83,9,0,36,32,0,176,13,0,
		106,3,48,49,0,20,1,36,33,0,176,14,0,106,
		50,80,111,115,105,99,105,111,110,101,32,111,32,70,
		111,114,109,117,108,97,114,105,111,32,56,48,32,67,
		111,108,117,110,97,115,32,101,32,84,101,99,108,101,
		32,91,69,78,84,69,82,93,0,20,1,36,34,0,
		176,15,0,20,0,36,35,0,176,16,0,20,0,36,
		36,0,176,17,0,92,20,106,8,80,82,73,78,84,
		69,82,0,20,2,36,37,0,122,83,18,0,36,38,
		0,176,19,0,12,0,32,0,2,36,39,0,176,20,
		0,122,121,20,2,176,21,0,109,22,0,106,11,69,
		109,112,114,101,115,97,32,58,32,0,72,109,23,0,
		72,20,1,36,40,0,176,20,0,122,92,120,20,2,
		176,21,0,106,8,68,97,116,97,32,58,32,0,176,
		24,0,176,25,0,12,0,12,1,72,20,1,36,41,
		0,176,20,0,92,2,121,20,2,176,21,0,106,11,
		83,105,115,116,101,109,97,32,58,32,0,109,26,0,
		72,20,1,36,42,0,176,20,0,92,2,92,121,20,
		2,176,21,0,106,14,82,101,108,97,116,58,32,83,
		67,76,48,48,49,0,20,1,36,43,0,176,20,0,
		92,3,92,40,20,2,176,21,0,109,27,0,106,15,
		84,65,66,69,76,65,32,68,69,32,84,73,80,79,
		0,72,109,22,0,72,20,1,36,44,0,176,20,0,
		92,4,121,20,2,176,21,0,176,28,0,106,2,61,
		0,93,135,0,12,2,20,1,36,45,0,176,20,0,
		92,5,121,20,2,176,21,0,106,2,124,0,20,1,
		36,46,0,176,20,0,92,5,92,2,20,2,176,21,
		0,106,7,67,79,68,73,71,79,0,20,1,36,47,
		0,176,20,0,92,5,92,15,20,2,176,21,0,106,
		10,68,69,83,67,82,73,67,65,79,0,20,1,36,
		48,0,176,20,0,92,6,121,20,2,176,21,0,176,
		28,0,106,2,61,0,93,135,0,12,2,20,1,36,
		49,0,92,7,83,29,0,36,50,0,109,29,0,92,
		58,34,29,166,0,176,19,0,12,0,32,158,0,36,
		51,0,176,20,0,109,29,0,121,20,2,176,30,0,
		109,31,0,106,12,64,82,32,57,57,57,57,57,57,
		57,57,0,20,2,36,52,0,176,20,0,109,29,0,
		92,15,20,2,176,21,0,109,32,0,20,1,36,53,
		0,176,33,0,122,20,1,36,54,0,176,34,0,20,
		0,36,55,0,109,35,0,28,63,36,56,0,176,17,
		0,92,24,106,1,0,20,2,36,57,0,176,17,0,
		92,20,106,7,83,67,82,69,69,78,0,20,2,36,
		58,0,176,36,0,20,0,176,4,0,106,2,49,0,
		20,1,176,37,0,100,20,1,36,59,0,100,110,7,
		36,61,0,109,29,0,122,72,83,29,0,26,84,255,
		36,63,0,109,18,0,122,72,83,18,0,26,251,253,
		36,65,0,176,20,0,92,56,121,20,2,176,21,0,
		176,28,0,106,2,61,0,93,132,0,12,2,20,1,
		36,66,0,176,20,0,92,57,92,110,20,2,176,21,
		0,109,38,0,20,1,36,67,0,176,20,0,121,121,
		20,2,176,21,0,109,39,0,106,2,32,0,72,20,
		1,36,69,0,176,17,0,92,24,106,1,0,20,2,
		36,70,0,176,17,0,92,20,106,7,83,67,82,69,
		69,78,0,20,2,36,71,0,176,36,0,20,0,176,
		4,0,106,2,49,0,20,1,176,37,0,100,20,1,
		36,72,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

