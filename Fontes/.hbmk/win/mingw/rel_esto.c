/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_esto.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_ESTO );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBCLEARRELATION );
HB_FUNC_EXTERN( DBSETRELATION );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_ESTO )
{ "REL_ESTO", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_ESTO )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBCLEARRELATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLEARRELATION )}, NULL },
{ "DBSETRELATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETRELATION )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
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
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "A", {HB_FS_PUBLIC}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "B", {HB_FS_PUBLIC}, {NULL}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SALDO_INIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENTRADA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SAIDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SALDO_FINA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MOVIMENTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_ESTO, "C:/sis/SAN/xhb/rel_esto.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_ESTO
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_ESTO )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_ESTO )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,106,17,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,0,20,1,36,
		14,0,109,2,0,106,2,83,0,5,29,192,4,36,
		15,0,176,3,0,106,31,82,69,76,65,84,79,82,
		73,79,32,69,83,84,79,81,85,69,32,68,69,32,
		77,65,84,69,82,73,65,73,83,0,20,1,36,16,
		0,176,4,0,106,2,50,0,20,1,36,17,0,176,
		5,0,9,100,106,6,73,84,69,78,83,0,100,100,
		9,20,6,176,6,0,106,6,73,84,69,78,83,0,
		20,1,36,18,0,176,4,0,106,2,49,0,20,1,
		36,19,0,176,5,0,9,100,106,8,69,83,84,79,
		81,85,69,0,100,100,9,20,6,176,6,0,106,8,
		69,83,84,79,81,85,69,0,20,1,36,20,0,176,
		7,0,20,0,176,8,0,106,2,66,0,90,6,109,
		9,0,6,106,7,67,79,68,73,71,79,0,9,20,
		4,36,21,0,176,10,0,20,0,36,22,0,176,11,
		0,106,3,48,49,0,20,1,36,23,0,176,12,0,
		106,50,80,111,115,105,99,105,111,110,101,32,111,32,
		70,111,114,109,117,108,97,114,105,111,32,56,48,32,
		67,111,108,117,110,97,115,32,101,32,84,101,99,108,
		101,32,91,69,78,84,69,82,93,0,20,1,36,24,
		0,176,13,0,20,0,36,25,0,176,14,0,20,0,
		36,26,0,176,15,0,92,20,106,8,80,82,73,78,
		84,69,82,0,20,2,36,27,0,122,83,16,0,36,
		28,0,176,17,0,12,0,32,54,3,36,29,0,176,
		18,0,121,121,20,2,176,19,0,109,20,0,106,2,
		32,0,72,20,1,36,30,0,176,18,0,122,121,20,
		2,176,19,0,109,21,0,20,1,36,31,0,176,18,
		0,122,92,115,20,2,176,19,0,106,9,80,97,103,
		105,110,97,58,32,0,176,22,0,109,16,0,92,8,
		12,2,72,20,1,36,32,0,176,18,0,92,2,121,
		20,2,176,19,0,109,23,0,20,1,36,33,0,176,
		18,0,92,2,92,115,20,2,176,19,0,106,9,68,
		97,116,97,32,32,58,32,0,176,24,0,176,25,0,
		12,0,12,1,72,20,1,36,34,0,176,18,0,92,
		3,92,50,20,2,176,19,0,106,21,69,83,84,79,
		81,85,69,32,68,69,32,77,65,84,69,82,73,65,
		73,83,0,20,1,36,35,0,176,18,0,92,3,92,
		50,20,2,176,19,0,106,21,69,83,84,79,81,85,
		69,32,68,69,32,77,65,84,69,82,73,65,73,83,
		0,20,1,36,36,0,176,18,0,92,4,121,20,2,
		176,19,0,176,26,0,106,2,61,0,93,132,0,12,
		2,20,1,36,37,0,176,18,0,92,5,121,20,2,
		176,19,0,106,7,67,111,100,105,103,111,0,20,1,
		36,38,0,176,18,0,92,5,92,10,20,2,176,19,
		0,106,10,68,101,115,99,114,105,99,97,111,0,20,
		1,36,39,0,176,18,0,92,5,92,50,20,2,176,
		19,0,106,11,83,68,46,73,110,105,99,105,97,108,
		0,20,1,36,40,0,176,18,0,92,5,92,65,20,
		2,176,19,0,106,9,69,110,116,114,97,100,97,115,
		0,20,1,36,41,0,176,18,0,92,5,92,80,20,
		2,176,19,0,106,7,83,97,105,100,97,115,0,20,
		1,36,42,0,176,18,0,92,5,92,95,20,2,176,
		19,0,106,10,83,68,46,32,70,105,110,97,108,0,
		20,1,36,43,0,176,18,0,92,5,92,110,20,2,
		176,19,0,106,9,85,108,116,46,77,111,118,46,0,
		20,1,36,44,0,176,18,0,92,6,121,20,2,176,
		19,0,176,26,0,106,2,61,0,93,132,0,12,2,
		20,1,36,45,0,92,7,83,27,0,36,46,0,109,
		27,0,92,58,34,29,64,1,176,17,0,12,0,32,
		56,1,36,47,0,176,18,0,109,27,0,121,20,2,
		176,28,0,108,29,87,9,106,12,64,82,32,57,57,
		57,57,57,57,57,57,0,20,2,36,48,0,176,18,
		0,109,27,0,92,10,20,2,176,19,0,176,30,0,
		108,31,87,32,12,1,20,1,36,49,0,176,18,0,
		109,27,0,92,50,20,2,176,28,0,108,29,87,33,
		106,11,64,82,32,57,57,57,44,57,57,57,0,20,
		2,36,50,0,176,18,0,109,27,0,92,65,20,2,
		176,28,0,108,29,87,34,106,11,64,82,32,57,57,
		57,44,57,57,57,0,20,2,36,51,0,176,18,0,
		109,27,0,92,80,20,2,176,28,0,108,29,87,35,
		106,11,64,82,32,57,57,57,44,57,57,57,0,20,
		2,36,52,0,176,18,0,109,27,0,92,95,20,2,
		176,28,0,108,29,87,36,106,11,64,82,32,57,57,
		57,44,57,57,57,0,20,2,36,53,0,176,18,0,
		109,27,0,92,110,20,2,176,19,0,108,29,87,37,
		20,1,36,54,0,176,38,0,122,20,1,36,55,0,
		176,39,0,20,0,36,56,0,109,40,0,28,48,36,
		57,0,176,15,0,92,20,106,7,83,67,82,69,69,
		78,0,20,2,36,58,0,176,41,0,20,0,176,4,
		0,106,2,49,0,20,1,176,42,0,100,20,1,36,
		59,0,7,36,61,0,109,27,0,122,72,83,27,0,
		26,186,254,36,63,0,109,16,0,122,72,83,16,0,
		26,197,252,36,65,0,176,18,0,92,56,121,20,2,
		176,19,0,176,26,0,106,2,61,0,93,132,0,12,
		2,20,1,36,66,0,176,18,0,92,57,92,110,20,
		2,176,19,0,106,19,77,99,81,117,97,121,32,73,
		110,102,111,114,109,97,116,105,99,97,0,20,1,36,
		67,0,176,18,0,121,121,20,2,176,19,0,109,43,
		0,106,2,32,0,72,20,1,36,69,0,176,15,0,
		92,20,106,7,83,67,82,69,69,78,0,20,2,36,
		70,0,176,41,0,20,0,176,4,0,106,2,49,0,
		20,1,176,42,0,100,20,1,36,71,0,7
	};

	hb_vmExecute( pcode, symbols );
}
