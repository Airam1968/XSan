/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_sigm.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_SIGM );
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
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBCREATEINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_SIGM )
{ "REL_SIGM", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_SIGM )}, NULL },
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
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBCREATEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCREATEINDEX )}, NULL },
{ "NOMS_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "SENU_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOTAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "SISLETRAC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "DTVE_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTCR_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "VLTI_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NSEQ_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_SIGM, "C:/sis/SAN/xhb/rel_sigm.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_SIGM
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_SIGM )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_SIGM )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,106,17,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,0,20,1,36,
		14,0,109,2,0,106,2,83,0,5,29,238,6,36,
		15,0,176,3,0,106,35,82,69,76,65,84,79,82,
		73,79,32,68,65,68,79,83,32,68,69,32,80,65,
		71,65,77,69,78,84,79,32,83,73,71,77,65,0,
		20,1,36,16,0,176,4,0,106,3,48,49,0,20,
		1,36,17,0,176,5,0,122,12,1,83,6,0,36,
		18,0,176,7,0,92,23,92,2,20,2,176,8,0,
		106,39,79,114,100,101,109,32,65,108,102,97,98,130,
		116,105,99,97,32,111,117,32,78,117,109,130,114,105,
		99,97,32,40,65,47,78,41,32,32,58,32,0,20,
		1,176,9,0,176,10,0,12,0,176,11,0,12,0,
		122,72,20,2,176,12,0,109,13,0,176,14,0,100,
		106,6,101,84,73,80,79,0,106,2,88,0,90,12,
		109,6,0,106,3,65,78,0,24,6,100,12,5,20,
		2,48,15,0,176,16,0,109,13,0,12,1,112,0,
		73,36,19,0,176,17,0,109,13,0,100,100,100,100,
		100,100,20,7,4,0,0,83,13,0,109,13,0,73,
		36,20,0,176,18,0,106,50,80,111,115,105,99,105,
		111,110,101,32,111,32,70,111,114,109,117,108,97,114,
		105,111,32,56,48,32,67,111,108,117,110,97,115,32,
		101,32,84,101,99,108,101,32,91,69,78,84,69,82,
		93,0,20,1,36,21,0,176,19,0,20,0,36,22,
		0,176,20,0,20,0,36,23,0,109,6,0,106,2,
		65,0,5,28,118,36,24,0,176,21,0,106,2,49,
		0,20,1,36,25,0,176,22,0,92,8,106,3,79,
		78,0,20,2,36,26,0,176,23,0,9,100,106,9,
		84,82,65,78,83,84,88,84,0,100,100,9,20,6,
		36,27,0,176,24,0,106,9,84,82,65,78,83,84,
		88,84,0,106,11,78,79,77,83,95,84,82,78,48,
		49,0,90,6,109,25,0,6,100,20,4,36,28,0,
		176,26,0,20,0,36,29,0,176,22,0,92,8,106,
		4,79,70,70,0,20,2,25,116,36,31,0,176,21,
		0,106,2,49,0,20,1,36,32,0,176,22,0,92,
		8,106,3,79,78,0,20,2,36,33,0,176,23,0,
		9,100,106,9,84,82,65,78,83,84,88,84,0,100,
		100,9,20,6,36,34,0,176,24,0,106,9,84,82,
		65,78,83,84,88,84,0,106,11,83,69,78,85,95,
		84,82,78,48,49,0,90,6,109,27,0,6,100,20,
		4,36,35,0,176,26,0,20,0,36,36,0,176,22,
		0,92,8,106,4,79,70,70,0,20,2,36,38,0,
		121,83,28,0,36,39,0,176,22,0,92,20,106,8,
		80,82,73,78,84,69,82,0,20,2,36,40,0,176,
		29,0,12,0,32,168,4,36,41,0,176,7,0,122,
		121,20,2,176,8,0,109,30,0,106,11,69,109,112,
		114,101,115,97,32,58,32,0,72,109,31,0,72,20,
		1,36,42,0,176,7,0,122,92,120,20,2,176,8,
		0,106,8,68,97,116,97,32,58,32,0,176,32,0,
		176,33,0,12,0,12,1,72,20,1,36,43,0,176,
		7,0,92,2,121,20,2,176,8,0,106,11,83,105,
		115,116,101,109,97,32,58,32,0,109,34,0,72,20,
		1,36,44,0,176,7,0,92,2,92,121,20,2,176,
		8,0,106,14,82,101,108,97,116,58,32,83,65,78,
		48,48,49,0,20,1,36,45,0,176,7,0,92,3,
		92,40,20,2,176,8,0,109,35,0,106,37,82,69,
		76,65,84,79,82,73,79,32,68,69,32,80,65,71,
		65,77,69,78,84,79,83,32,45,32,83,73,71,77,
		65,47,73,84,65,85,0,72,109,30,0,72,20,1,
		36,46,0,176,7,0,92,4,121,20,2,176,8,0,
		176,36,0,106,2,61,0,93,135,0,12,2,20,1,
		36,47,0,176,7,0,92,5,121,20,2,176,8,0,
		106,2,124,0,20,1,36,48,0,176,7,0,92,5,
		92,2,20,2,176,8,0,106,11,78,79,83,83,79,
		32,78,85,77,46,0,20,1,36,49,0,176,7,0,
		92,5,92,14,20,2,176,8,0,106,13,78,79,77,
		69,32,67,76,73,69,78,84,69,0,20,1,36,50,
		0,176,7,0,92,5,92,46,20,2,176,8,0,106,
		11,68,65,84,65,32,86,69,78,67,46,0,20,1,
		36,51,0,176,7,0,92,5,92,58,20,2,176,8,
		0,106,11,68,65,84,65,32,80,71,84,79,46,0,
		20,1,36,52,0,176,7,0,92,5,92,70,20,2,
		176,8,0,106,11,86,76,82,46,32,80,71,84,79,
		46,0,20,1,36,53,0,176,7,0,92,5,92,90,
		20,2,176,8,0,106,11,76,65,78,67,46,32,73,
		84,65,85,0,20,1,36,54,0,176,7,0,92,5,
		93,134,0,20,2,176,8,0,106,2,124,0,20,1,
		36,55,0,176,7,0,92,6,121,20,2,176,8,0,
		176,36,0,106,2,61,0,93,135,0,12,2,20,1,
		36,56,0,92,7,83,37,0,36,57,0,176,29,0,
		12,0,32,230,1,109,37,0,92,57,34,29,221,1,
		36,58,0,176,38,0,176,39,0,109,40,0,12,1,
		12,1,83,41,0,36,59,0,176,38,0,176,39,0,
		109,42,0,12,1,12,1,83,43,0,36,60,0,176,
		44,0,176,45,0,109,41,0,122,92,2,12,3,106,
		2,47,0,72,176,45,0,109,41,0,92,3,92,2,
		12,3,72,106,2,47,0,72,176,45,0,109,41,0,
		92,5,92,2,12,3,72,12,1,83,41,0,36,61,
		0,176,44,0,176,45,0,109,43,0,122,92,2,12,
		3,106,2,47,0,72,176,45,0,109,43,0,92,3,
		92,2,12,3,72,106,2,47,0,72,176,45,0,109,
		43,0,92,5,92,2,12,3,72,12,1,83,43,0,
		36,62,0,176,7,0,109,37,0,121,20,2,176,8,
		0,106,2,124,0,20,1,36,63,0,176,7,0,109,
		37,0,92,2,20,2,176,46,0,109,27,0,106,12,
		64,82,32,57,57,57,57,57,57,57,57,0,20,2,
		36,64,0,176,7,0,109,37,0,92,14,20,2,176,
		8,0,176,38,0,109,25,0,12,1,20,1,36,65,
		0,176,7,0,109,37,0,92,46,20,2,176,8,0,
		109,41,0,20,1,36,66,0,176,7,0,109,37,0,
		92,58,20,2,176,8,0,109,43,0,20,1,36,67,
		0,176,7,0,109,37,0,92,70,20,2,176,46,0,
		109,47,0,106,22,64,82,32,57,57,57,44,57,57,
		57,44,57,57,57,44,57,57,57,46,57,57,0,20,
		2,36,68,0,176,7,0,109,37,0,92,90,20,2,
		176,46,0,109,48,0,106,9,64,82,32,57,57,57,
		57,57,0,20,2,36,69,0,176,7,0,109,37,0,
		93,134,0,20,2,176,8,0,106,2,124,0,20,1,
		36,70,0,109,37,0,122,72,83,37,0,36,71,0,
		109,28,0,109,47,0,72,83,28,0,36,72,0,176,
		49,0,122,20,1,36,73,0,176,50,0,20,0,36,
		74,0,109,51,0,29,67,254,36,75,0,176,22,0,
		92,20,106,7,83,67,82,69,69,78,0,20,2,36,
		76,0,176,52,0,20,0,176,21,0,106,2,49,0,
		20,1,176,53,0,100,20,1,36,77,0,7,36,80,
		0,176,29,0,12,0,28,122,36,81,0,176,7,0,
		109,37,0,121,20,2,176,8,0,176,36,0,106,2,
		45,0,93,135,0,12,2,20,1,36,82,0,109,37,
		0,122,72,83,37,0,36,83,0,176,7,0,109,37,
		0,92,2,20,2,176,8,0,106,15,84,111,116,97,
		108,32,71,101,114,97,108,32,58,32,0,20,1,36,
		84,0,176,7,0,109,37,0,92,70,20,2,176,46,
		0,109,28,0,106,22,64,82,32,57,57,57,44,57,
		57,57,44,57,57,57,44,57,57,57,46,57,57,0,
		20,2,36,86,0,176,7,0,92,58,121,20,2,176,
		8,0,176,36,0,106,2,61,0,93,135,0,12,2,
		20,1,36,87,0,176,7,0,92,59,92,104,20,2,
		176,8,0,109,54,0,20,1,36,88,0,176,7,0,
		122,121,20,2,176,8,0,106,2,32,0,109,55,0,
		72,20,1,26,83,251,36,91,0,176,22,0,92,20,
		106,7,83,67,82,69,69,78,0,20,2,36,92,0,
		176,52,0,20,0,176,21,0,106,2,49,0,20,1,
		176,53,0,100,20,1,36,93,0,7
	};

	hb_vmExecute( pcode, symbols );
}

