/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_cnap.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_CNAP );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( SET );
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
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( __ATPROMPT );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( __MENUTO );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_CNAP )
{ "REL_CNAP", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_CNAP )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "ECOD_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "COD_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "NOME_ARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENOME_ARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "__ATPROMPT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __ATPROMPT )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISOP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MENUTO", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MENUTO )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "ECOD_PROT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "CDOC_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ETIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROW )}, NULL },
{ "COL", {HB_FS_PUBLIC}, {HB_FUNCNAME( COL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
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
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "DTCR_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETAM_DTC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDT_CRED", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EANO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
{ "EDATA_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTVE_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETAM_DTV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDT_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "UEMP_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NOMS_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "VLTI_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NSEQ_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FIL2_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NONU_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_CNAP, "C:/sis/SAN/xhb/rel_cnap.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_CNAP
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_CNAP )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_CNAP )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,106,17,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,0,20,1,36,
		15,0,109,2,0,106,2,83,0,5,29,119,22,36,
		16,0,176,3,0,106,57,82,69,76,65,84,79,82,
		73,79,32,67,79,66,82,65,78,67,65,32,66,65,
		78,67,65,82,73,65,32,45,32,65,82,81,85,73,
		86,79,32,82,69,84,79,82,78,79,32,45,32,80,
		82,79,84,69,83,84,79,0,20,1,36,17,0,176,
		4,0,106,3,48,49,0,20,1,36,18,0,176,5,
		0,92,8,106,3,79,78,0,20,2,36,19,0,176,
		6,0,106,2,50,0,20,1,36,20,0,176,7,0,
		9,100,106,7,66,65,78,67,79,83,0,100,100,9,
		20,6,176,8,0,106,7,66,65,78,67,79,83,0,
		20,1,36,21,0,176,9,0,20,0,36,22,0,176,
		10,0,20,0,36,23,0,176,11,0,106,9,87,47,
		78,44,43,87,47,82,0,20,1,36,24,0,121,83,
		12,0,36,25,0,176,13,0,92,5,92,2,20,2,
		176,14,0,106,11,66,97,110,99,111,32,32,32,58,
		32,0,20,1,36,26,0,176,9,0,20,0,36,27,
		0,92,2,3,1,0,92,2,3,1,0,92,2,3,
		1,0,92,2,3,1,0,176,19,0,108,15,108,16,
		108,17,108,18,20,4,81,18,0,81,17,0,81,16,
		0,81,15,0,36,28,0,176,20,0,109,15,0,120,
		109,18,0,20,3,36,29,0,106,7,64,82,57,57,
		57,57,0,98,16,0,122,2,36,30,0,106,5,64,
		83,53,48,0,98,16,0,92,2,2,36,31,0,106,
		5,32,32,32,32,0,98,17,0,122,2,36,32,0,
		106,5,32,32,32,32,0,98,17,0,92,2,2,36,
		33,0,176,21,0,92,5,92,15,92,5,92,75,109,
		15,0,106,11,70,95,67,79,78,70,95,67,79,66,
		0,109,16,0,109,17,0,106,2,32,0,106,4,32,
		45,32,0,106,2,32,0,20,11,36,34,0,109,22,
		0,83,12,0,36,35,0,176,23,0,109,24,0,12,
		1,83,25,0,36,37,0,176,11,0,106,10,87,47,
		78,44,43,87,47,66,71,0,20,1,36,38,0,176,
		26,0,92,15,92,60,92,20,92,79,20,4,176,27,
		0,92,15,92,60,20,2,36,39,0,176,28,0,92,
		15,92,60,92,20,92,79,122,20,5,36,40,0,176,
		29,0,92,16,92,61,106,19,32,49,32,32,76,105,
		113,117,105,100,97,135,198,111,32,32,32,32,0,106,
		53,32,84,161,116,117,108,111,115,32,76,105,113,117,
		105,100,97,100,111,115,32,101,109,32,67,97,114,116,
		162,114,105,111,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,0,20,4,
		36,41,0,176,29,0,92,17,92,61,106,19,32,50,
		32,66,97,105,120,97,32,112,111,114,32,80,114,111,
		116,46,0,106,53,32,84,161,116,117,108,111,32,69,
		102,101,116,105,118,97,109,101,110,116,101,32,80,114,
		111,116,101,115,116,97,100,111,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,0,20,4,36,42,0,176,29,0,92,18,92,61,
		106,19,32,51,32,84,161,116,117,108,111,32,80,114,
		111,116,101,115,116,46,0,106,53,32,84,161,116,117,
		108,111,32,69,110,99,97,109,105,110,104,97,100,111,
		32,97,111,32,67,97,114,116,162,114,105,111,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,0,20,4,36,43,0,176,29,0,
		92,19,92,61,106,19,32,52,32,83,117,115,116,97,
		135,198,111,32,80,114,111,116,46,32,0,106,53,32,
		73,110,115,116,114,117,135,198,111,32,100,101,32,80,
		114,111,116,101,115,116,111,32,83,117,115,116,97,100,
		97,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,0,20,4,36,44,
		0,176,30,0,109,31,0,100,100,100,100,100,100,20,
		7,4,0,0,83,31,0,109,31,0,73,36,45,0,
		122,83,32,0,36,46,0,176,33,0,89,28,0,1,
		0,0,0,176,34,0,12,0,121,8,28,7,109,32,
		0,25,8,95,1,165,83,32,0,6,106,6,115,105,
		115,79,80,0,12,2,83,32,0,36,48,0,109,32,
		0,122,5,29,58,1,36,50,0,109,12,0,122,5,
		28,16,36,51,0,106,3,48,55,0,83,35,0,26,
		223,4,36,52,0,109,12,0,92,8,5,28,16,36,
		53,0,106,3,49,53,0,83,35,0,26,198,4,36,
		54,0,109,12,0,92,33,5,28,16,36,55,0,106,
		3,32,32,0,83,35,0,26,173,4,36,56,0,109,
		12,0,92,104,5,28,16,36,57,0,106,3,50,50,
		0,83,35,0,26,148,4,36,58,0,109,12,0,93,
		215,0,5,28,16,36,59,0,106,3,48,56,0,83,
		35,0,26,122,4,36,60,0,109,12,0,93,230,0,
		5,28,16,36,61,0,106,3,48,56,0,83,35,0,
		26,96,4,36,62,0,109,12,0,93,237,0,5,28,
		16,36,63,0,106,3,48,56,0,83,35,0,26,70,
		4,36,64,0,109,12,0,93,19,1,5,28,16,36,
		65,0,106,3,48,56,0,83,35,0,26,44,4,36,
		66,0,109,12,0,93,35,1,5,28,16,36,67,0,
		106,3,48,56,0,83,35,0,26,18,4,36,68,0,
		109,12,0,93,85,1,5,28,16,36,69,0,106,3,
		48,56,0,83,35,0,26,248,3,36,70,0,109,12,
		0,93,153,1,5,28,16,36,71,0,106,3,56,57,
		0,83,35,0,26,222,3,36,72,0,109,12,0,93,
		129,2,5,29,209,3,36,73,0,106,3,49,53,0,
		83,35,0,36,75,0,26,192,3,36,76,0,109,32,
		0,92,2,5,29,58,1,36,78,0,109,12,0,122,
		5,28,16,36,79,0,106,3,50,51,0,83,35,0,
		26,156,3,36,80,0,109,12,0,92,8,5,28,16,
		36,81,0,106,3,51,51,0,83,35,0,26,131,3,
		36,82,0,109,12,0,92,33,5,28,16,36,83,0,
		106,3,32,32,0,83,35,0,26,106,3,36,84,0,
		109,12,0,92,104,5,28,16,36,85,0,106,3,50,
		53,0,83,35,0,26,81,3,36,86,0,109,12,0,
		93,215,0,5,28,16,36,87,0,106,3,48,56,0,
		83,35,0,26,55,3,36,88,0,109,12,0,93,230,
		0,5,28,16,36,89,0,106,3,48,56,0,83,35,
		0,26,29,3,36,90,0,109,12,0,93,237,0,5,
		28,16,36,91,0,106,3,48,56,0,83,35,0,26,
		3,3,36,92,0,109,12,0,93,19,1,5,28,16,
		36,93,0,106,3,48,56,0,83,35,0,26,233,2,
		36,94,0,109,12,0,93,35,1,5,28,16,36,95,
		0,106,3,48,56,0,83,35,0,26,207,2,36,96,
		0,109,12,0,93,85,1,5,28,16,36,97,0,106,
		3,51,50,0,83,35,0,26,181,2,36,98,0,109,
		12,0,93,153,1,5,28,16,36,99,0,106,3,52,
		49,0,83,35,0,26,155,2,36,100,0,109,12,0,
		93,129,2,5,29,142,2,36,101,0,106,3,53,48,
		0,83,35,0,36,103,0,26,125,2,36,104,0,109,
		32,0,92,3,5,29,58,1,36,106,0,109,12,0,
		122,5,28,16,36,107,0,106,3,50,51,0,83,35,
		0,26,89,2,36,108,0,109,12,0,92,8,5,28,
		16,36,109,0,106,3,51,51,0,83,35,0,26,64,
		2,36,110,0,109,12,0,92,33,5,28,16,36,111,
		0,106,3,32,32,0,83,35,0,26,39,2,36,112,
		0,109,12,0,92,104,5,28,16,36,113,0,106,3,
		50,54,0,83,35,0,26,14,2,36,114,0,109,12,
		0,93,215,0,5,28,16,36,115,0,106,3,49,53,
		0,83,35,0,26,244,1,36,116,0,109,12,0,93,
		230,0,5,28,16,36,117,0,106,3,49,53,0,83,
		35,0,26,218,1,36,118,0,109,12,0,93,237,0,
		5,28,16,36,119,0,106,3,49,53,0,83,35,0,
		26,192,1,36,120,0,109,12,0,93,19,1,5,28,
		16,36,121,0,106,3,49,53,0,83,35,0,26,166,
		1,36,122,0,109,12,0,93,35,1,5,28,16,36,
		123,0,106,3,49,53,0,83,35,0,26,140,1,36,
		124,0,109,12,0,93,85,1,5,28,16,36,125,0,
		106,3,51,50,0,83,35,0,26,114,1,36,126,0,
		109,12,0,93,153,1,5,28,16,36,127,0,106,3,
		52,49,0,83,35,0,26,88,1,36,128,0,109,12,
		0,93,129,2,5,29,75,1,36,129,0,106,3,49,
		48,0,83,35,0,36,131,0,26,58,1,36,132,0,
		109,32,0,92,4,5,29,64,250,36,134,0,109,12,
		0,122,5,28,16,36,135,0,106,3,50,52,0,83,
		35,0,26,22,1,36,136,0,109,12,0,92,8,5,
		28,16,36,137,0,106,3,50,54,0,83,35,0,26,
		253,0,36,138,0,109,12,0,92,33,5,28,16,36,
		139,0,106,3,32,32,0,83,35,0,26,228,0,36,
		140,0,109,12,0,92,104,5,28,16,36,141,0,106,
		3,50,55,0,83,35,0,26,203,0,36,142,0,109,
		12,0,93,215,0,5,28,16,36,143,0,106,3,50,
		48,0,83,35,0,26,177,0,36,144,0,109,12,0,
		93,230,0,5,28,16,36,145,0,106,3,50,48,0,
		83,35,0,26,151,0,36,146,0,109,12,0,93,237,
		0,5,28,15,36,147,0,106,3,50,48,0,83,35,
		0,25,125,36,148,0,109,12,0,93,19,1,5,28,
		15,36,149,0,106,3,50,48,0,83,35,0,25,100,
		36,150,0,109,12,0,93,35,1,5,28,15,36,151,
		0,106,3,50,48,0,83,35,0,25,75,36,152,0,
		109,12,0,93,85,1,5,28,15,36,153,0,106,3,
		50,48,0,83,35,0,25,50,36,154,0,109,12,0,
		93,153,1,5,28,15,36,155,0,106,3,57,57,0,
		83,35,0,25,25,36,156,0,109,12,0,93,129,2,
		5,28,13,36,157,0,106,3,50,48,0,83,35,0,
		36,163,0,109,12,0,122,5,28,100,36,164,0,176,
		6,0,106,2,49,0,20,1,36,165,0,176,7,0,
		9,100,106,9,84,82,65,78,83,48,48,49,0,100,
		100,9,20,6,36,166,0,176,9,0,20,0,36,167,
		0,176,36,0,90,10,109,37,0,109,35,0,5,6,
		106,23,67,68,79,67,95,84,82,78,48,49,32,61,
		32,101,67,79,68,95,80,82,79,84,0,20,2,36,
		168,0,176,9,0,20,0,26,74,5,36,169,0,109,
		12,0,92,8,5,28,100,36,170,0,176,6,0,106,
		2,49,0,20,1,36,171,0,176,7,0,9,100,106,
		9,84,82,65,78,83,48,48,56,0,100,100,9,20,
		6,36,172,0,176,9,0,20,0,36,173,0,176,36,
		0,90,10,109,37,0,109,35,0,5,6,106,23,67,
		68,79,67,95,84,82,78,48,49,32,61,32,101,67,
		79,68,95,80,82,79,84,0,20,2,36,174,0,176,
		9,0,20,0,26,221,4,36,175,0,109,12,0,92,
		33,5,28,97,36,176,0,176,6,0,106,2,49,0,
		20,1,36,177,0,176,7,0,9,100,106,9,84,82,
		65,78,83,48,51,51,0,100,100,9,20,6,36,178,
		0,176,9,0,20,0,36,179,0,176,36,0,90,12,
		109,37,0,106,3,32,32,0,5,6,106,18,67,68,
		79,67,95,84,82,78,48,49,32,61,32,34,32,32,
		34,0,20,2,36,180,0,176,9,0,20,0,26,115,
		4,36,181,0,109,12,0,92,104,5,28,100,36,182,
		0,176,6,0,106,2,49,0,20,1,36,183,0,176,
		7,0,9,100,106,9,84,82,65,78,83,49,48,52,
		0,100,100,9,20,6,36,184,0,176,9,0,20,0,
		36,185,0,176,36,0,90,10,109,37,0,109,35,0,
		5,6,106,23,67,68,79,67,95,84,82,78,48,49,
		32,61,32,101,67,79,68,95,80,82,79,84,0,20,
		2,36,186,0,176,9,0,20,0,26,6,4,36,187,
		0,109,12,0,93,215,0,5,28,100,36,188,0,176,
		6,0,106,2,49,0,20,1,36,189,0,176,7,0,
		9,100,106,9,84,82,65,78,83,50,49,53,0,100,
		100,9,20,6,36,190,0,176,9,0,20,0,36,191,
		0,176,36,0,90,10,109,37,0,109,35,0,5,6,
		106,23,67,68,79,67,95,84,82,78,48,49,32,61,
		32,101,67,79,68,95,80,82,79,84,0,20,2,36,
		192,0,176,9,0,20,0,26,152,3,36,193,0,109,
		12,0,93,230,0,5,28,100,36,194,0,176,6,0,
		106,2,49,0,20,1,36,195,0,176,7,0,9,100,
		106,9,84,82,65,78,83,50,51,48,0,100,100,9,
		20,6,36,196,0,176,9,0,20,0,36,197,0,176,
		36,0,90,10,109,37,0,109,35,0,5,6,106,23,
		67,68,79,67,95,84,82,78,48,49,32,61,32,101,
		67,79,68,95,80,82,79,84,0,20,2,36,198,0,
		176,9,0,20,0,26,42,3,36,199,0,109,12,0,
		93,237,0,5,28,100,36,200,0,176,6,0,106,2,
		49,0,20,1,36,201,0,176,7,0,9,100,106,9,
		84,82,65,78,83,50,57,49,0,100,100,9,20,6,
		36,202,0,176,9,0,20,0,36,203,0,176,36,0,
		90,10,109,37,0,109,35,0,5,6,106,23,67,68,
		79,67,95,84,82,78,48,49,32,61,32,101,67,79,
		68,95,80,82,79,84,0,20,2,36,204,0,176,9,
		0,20,0,26,188,2,36,205,0,109,12,0,93,19,
		1,5,28,100,36,206,0,176,6,0,106,2,49,0,
		20,1,36,207,0,176,7,0,9,100,106,9,84,82,
		65,78,83,50,55,53,0,100,100,9,20,6,36,208,
		0,176,9,0,20,0,36,209,0,176,36,0,90,10,
		109,37,0,109,35,0,5,6,106,23,67,68,79,67,
		95,84,82,78,48,49,32,61,32,101,67,79,68,95,
		80,82,79,84,0,20,2,36,210,0,176,9,0,20,
		0,26,78,2,36,211,0,109,12,0,93,35,1,5,
		28,100,36,212,0,176,6,0,106,2,49,0,20,1,
		36,213,0,176,7,0,9,100,106,9,84,82,65,78,
		83,50,57,49,0,100,100,9,20,6,36,214,0,176,
		9,0,20,0,36,215,0,176,36,0,90,10,109,37,
		0,109,35,0,5,6,106,23,67,68,79,67,95,84,
		82,78,48,49,32,61,32,101,67,79,68,95,80,82,
		79,84,0,20,2,36,216,0,176,9,0,20,0,26,
		224,1,36,217,0,109,12,0,93,85,1,5,28,100,
		36,218,0,176,6,0,106,2,49,0,20,1,36,219,
		0,176,7,0,9,100,106,9,84,82,65,78,83,51,
		52,49,0,100,100,9,20,6,36,220,0,176,9,0,
		20,0,36,221,0,176,36,0,90,10,109,37,0,109,
		35,0,5,6,106,23,67,68,79,67,95,84,82,78,
		48,49,32,61,32,101,67,79,68,95,80,82,79,84,
		0,20,2,36,222,0,176,9,0,20,0,26,114,1,
		36,223,0,109,12,0,93,153,1,5,28,100,36,224,
		0,176,6,0,106,2,49,0,20,1,36,225,0,176,
		7,0,9,100,106,9,84,82,65,78,83,52,48,57,
		0,100,100,9,20,6,36,226,0,176,9,0,20,0,
		36,227,0,176,36,0,90,10,109,37,0,109,35,0,
		5,6,106,23,67,68,79,67,95,84,82,78,48,49,
		32,61,32,101,67,79,68,95,80,82,79,84,0,20,
		2,36,228,0,176,9,0,20,0,26,4,1,36,229,
		0,109,12,0,93,168,1,5,28,97,36,230,0,176,
		6,0,106,2,49,0,20,1,36,231,0,176,7,0,
		9,100,106,9,84,82,65,78,83,52,50,52,0,100,
		100,9,20,6,36,232,0,176,9,0,20,0,36,233,
		0,176,36,0,90,12,109,37,0,106,3,32,32,0,
		5,6,106,18,67,68,79,67,95,84,82,78,48,49,
		32,61,32,34,32,32,34,0,20,2,36,234,0,176,
		9,0,20,0,26,153,0,36,235,0,109,12,0,93,
		129,2,5,28,96,36,236,0,176,6,0,106,2,49,
		0,20,1,36,237,0,176,7,0,9,100,106,9,84,
		82,65,78,83,54,52,49,0,100,100,9,20,6,36,
		238,0,176,9,0,20,0,36,239,0,176,36,0,90,
		12,109,37,0,106,3,32,32,0,5,6,106,18,67,
		68,79,67,95,84,82,78,48,49,32,61,32,34,32,
		32,34,0,20,2,36,240,0,176,9,0,20,0,25,
		46,36,242,0,176,6,0,106,2,49,0,20,1,36,
		243,0,176,7,0,9,100,106,9,84,82,65,78,83,
		84,88,84,0,100,100,9,20,6,36,244,0,176,9,
		0,20,0,36,246,0,176,38,0,122,12,1,83,39,
		0,36,247,0,176,13,0,92,23,92,2,20,2,176,
		14,0,106,39,79,114,100,101,109,32,65,108,102,97,
		98,130,116,105,99,97,32,111,117,32,78,117,109,130,
		114,105,99,97,32,40,65,47,78,41,32,32,58,32,
		0,20,1,176,27,0,176,40,0,12,0,176,41,0,
		12,0,122,72,20,2,176,42,0,109,31,0,176,43,
		0,100,106,6,101,84,73,80,79,0,106,2,88,0,
		90,12,109,39,0,106,3,65,78,0,24,6,100,12,
		5,20,2,48,44,0,176,45,0,109,31,0,12,1,
		112,0,73,36,248,0,176,30,0,109,31,0,100,100,
		100,100,100,100,20,7,4,0,0,83,31,0,109,31,
		0,73,36,249,0,176,4,0,106,3,48,49,0,20,
		1,36,250,0,176,46,0,106,66,80,111,115,105,99,
		105,111,110,101,32,111,32,70,111,114,109,117,108,97,
		114,105,111,32,56,48,32,99,111,108,46,32,101,32,
		116,101,99,108,101,32,91,69,78,84,69,82,93,44,
		32,112,97,114,97,32,99,111,110,116,105,110,117,97,
		114,46,46,46,0,20,1,36,251,0,176,47,0,20,
		0,36,252,0,176,48,0,20,0,36,253,0,121,83,
		49,0,36,254,0,176,5,0,92,20,106,8,80,82,
		73,78,84,69,82,0,20,2,36,255,0,176,50,0,
		12,0,32,247,6,36,0,1,176,13,0,122,121,20,
		2,176,14,0,109,51,0,106,11,69,109,112,114,101,
		115,97,32,58,32,0,72,109,52,0,72,20,1,36,
		1,1,176,13,0,122,92,115,20,2,176,14,0,106,
		8,68,97,116,97,32,58,32,0,176,53,0,176,54,
		0,12,0,12,1,72,20,1,36,2,1,176,13,0,
		92,2,121,20,2,176,14,0,106,11,83,105,115,116,
		101,109,97,32,58,32,0,109,55,0,72,20,1,36,
		3,1,176,13,0,92,2,92,115,20,2,176,14,0,
		106,14,82,101,108,97,116,58,32,83,65,78,48,48,
		49,0,20,1,36,4,1,176,13,0,92,3,92,30,
		20,2,176,14,0,109,56,0,106,46,82,69,76,65,
		84,79,82,73,79,32,68,69,32,67,79,66,82,65,
		78,67,65,32,66,65,78,67,65,82,73,65,32,45,
		32,80,82,79,84,69,83,84,79,83,32,45,32,0,
		72,109,25,0,72,109,51,0,72,20,1,36,5,1,
		176,13,0,92,4,121,20,2,176,14,0,176,57,0,
		106,2,61,0,93,135,0,12,2,20,1,36,6,1,
		176,13,0,92,5,121,20,2,176,14,0,106,2,124,
		0,20,1,36,7,1,176,13,0,92,5,92,2,20,
		2,176,14,0,106,11,78,79,83,83,79,32,78,85,
		77,46,0,20,1,36,8,1,176,13,0,92,5,92,
		14,20,2,176,14,0,106,13,78,79,77,69,32,67,
		76,73,69,78,84,69,0,20,1,36,9,1,176,13,
		0,92,5,92,46,20,2,176,14,0,106,11,68,65,
		84,65,32,86,69,78,67,46,0,20,1,36,10,1,
		176,13,0,92,5,92,60,20,2,176,14,0,106,12,
		86,76,82,46,32,84,73,84,85,76,79,0,20,1,
		36,11,1,176,13,0,92,5,92,90,20,2,176,14,
		0,106,7,79,67,79,82,82,46,0,20,1,36,12,
		1,176,13,0,92,5,92,100,20,2,176,14,0,106,
		11,78,85,77,46,32,76,65,78,67,46,0,20,1,
		36,13,1,176,13,0,92,5,92,115,20,2,176,14,
		0,106,11,67,79,68,46,66,79,76,69,84,79,0,
		20,1,36,14,1,176,13,0,92,5,93,134,0,20,
		2,176,14,0,106,2,124,0,20,1,36,15,1,176,
		13,0,92,6,121,20,2,176,14,0,176,57,0,106,
		2,61,0,93,135,0,12,2,20,1,36,16,1,92,
		7,83,58,0,36,17,1,176,50,0,12,0,32,13,
		4,109,58,0,92,54,34,29,4,4,36,19,1,176,
		59,0,176,23,0,176,60,0,109,61,0,92,6,121,
		12,3,12,1,12,1,83,62,0,36,20,1,109,62,
		0,92,6,69,28,36,176,57,0,106,2,48,0,92,
		6,109,62,0,49,12,2,176,23,0,176,60,0,109,
		61,0,92,6,121,12,3,12,1,72,25,18,176,23,
		0,176,60,0,109,61,0,92,6,121,12,3,12,1,
		83,63,0,36,21,1,121,83,64,0,36,22,1,176,
		65,0,176,66,0,109,63,0,92,5,92,2,12,3,
		12,1,83,64,0,36,23,1,109,64,0,92,50,16,
		28,76,109,64,0,92,99,34,28,68,36,24,1,176,
		67,0,176,66,0,109,63,0,122,92,2,12,3,106,
		2,47,0,72,176,66,0,109,63,0,92,3,92,2,
		12,3,72,106,2,47,0,72,106,3,49,57,0,72,
		176,66,0,109,63,0,92,5,92,2,12,3,72,12,
		1,83,68,0,25,66,36,26,1,176,67,0,176,66,
		0,109,63,0,122,92,2,12,3,106,2,47,0,72,
		176,66,0,109,63,0,92,3,92,2,12,3,72,106,
		2,47,0,72,106,3,50,48,0,72,176,66,0,109,
		63,0,92,5,92,2,12,3,72,12,1,83,68,0,
		36,29,1,176,59,0,176,23,0,176,60,0,109,69,
		0,92,6,121,12,3,12,1,12,1,83,70,0,36,
		30,1,109,70,0,92,6,69,28,36,176,57,0,106,
		2,48,0,92,6,109,70,0,49,12,2,176,23,0,
		176,60,0,109,69,0,92,6,121,12,3,12,1,72,
		25,18,176,23,0,176,60,0,109,69,0,92,6,121,
		12,3,12,1,83,71,0,36,31,1,121,83,64,0,
		36,32,1,176,65,0,176,66,0,109,71,0,92,5,
		92,2,12,3,12,1,83,64,0,36,33,1,109,64,
		0,92,50,16,28,76,109,64,0,92,99,34,28,68,
		36,34,1,176,67,0,176,66,0,109,71,0,122,92,
		2,12,3,106,2,47,0,72,176,66,0,109,71,0,
		92,3,92,2,12,3,72,106,2,47,0,72,106,3,
		49,57,0,72,176,66,0,109,71,0,92,5,92,2,
		12,3,72,12,1,83,72,0,25,66,36,36,1,176,
		67,0,176,66,0,109,71,0,122,92,2,12,3,106,
		2,47,0,72,176,66,0,109,71,0,92,3,92,2,
		12,3,72,106,2,47,0,72,106,3,50,48,0,72,
		176,66,0,109,71,0,92,5,92,2,12,3,72,12,
		1,83,72,0,36,38,1,176,13,0,109,58,0,121,
		20,2,176,14,0,106,2,124,0,20,1,36,39,1,
		176,13,0,109,58,0,92,2,20,2,176,14,0,176,
		23,0,109,73,0,12,1,20,1,36,40,1,109,12,
		0,93,85,1,5,28,28,36,41,1,176,13,0,109,
		58,0,92,14,20,2,176,14,0,176,23,0,109,74,
		0,12,1,20,1,36,43,1,176,13,0,109,58,0,
		92,46,20,2,176,14,0,109,72,0,20,1,36,44,
		1,176,13,0,109,58,0,92,60,20,2,176,75,0,
		109,76,0,106,22,64,82,32,57,57,57,44,57,57,
		57,44,57,57,57,44,57,57,57,46,57,57,0,20,
		2,36,45,1,176,13,0,109,58,0,92,90,20,2,
		176,75,0,109,37,0,106,6,64,82,32,57,57,0,
		20,2,36,46,1,176,13,0,109,58,0,92,100,20,
		2,176,75,0,109,77,0,106,9,64,82,32,57,57,
		57,57,57,0,20,2,36,48,1,109,12,0,93,35,
		1,5,28,45,36,49,1,176,13,0,109,58,0,92,
		120,20,2,176,75,0,109,78,0,106,18,64,82,32,
		57,57,57,57,57,57,57,57,57,57,57,57,57,57,
		0,20,2,25,99,36,50,1,109,12,0,93,129,2,
		5,28,46,36,51,1,176,13,0,109,58,0,92,119,
		20,2,176,75,0,109,79,0,106,19,64,82,32,57,
		57,57,57,57,57,57,57,57,57,57,57,57,57,57,
		0,20,2,25,43,36,53,1,176,13,0,109,58,0,
		92,120,20,2,176,75,0,109,79,0,106,18,64,82,
		32,57,57,57,57,57,57,57,57,57,57,57,57,57,
		57,0,20,2,36,55,1,176,13,0,109,58,0,93,
		134,0,20,2,176,14,0,106,2,124,0,20,1,36,
		56,1,109,58,0,122,72,83,58,0,36,57,1,109,
		49,0,109,76,0,72,83,49,0,36,58,1,176,80,
		0,122,20,1,36,59,1,176,81,0,20,0,36,60,
		1,109,82,0,29,30,252,36,61,1,176,5,0,92,
		20,106,7,83,67,82,69,69,78,0,20,2,36,62,
		1,176,83,0,20,0,176,6,0,106,2,49,0,20,
		1,176,84,0,100,20,1,36,63,1,100,110,7,36,
		66,1,176,50,0,12,0,28,122,36,67,1,176,13,
		0,109,58,0,121,20,2,176,14,0,176,57,0,106,
		2,45,0,93,135,0,12,2,20,1,36,68,1,109,
		58,0,122,72,83,58,0,36,69,1,176,13,0,109,
		58,0,92,2,20,2,176,14,0,106,15,84,111,116,
		97,108,32,71,101,114,97,108,32,58,32,0,20,1,
		36,70,1,176,13,0,109,58,0,92,60,20,2,176,
		75,0,109,49,0,106,22,64,82,32,57,57,57,44,
		57,57,57,44,57,57,57,44,57,57,57,46,57,57,
		0,20,2,36,72,1,176,13,0,92,56,121,20,2,
		176,14,0,176,57,0,106,2,61,0,93,135,0,12,
		2,20,1,36,73,1,176,13,0,92,57,92,95,20,
		2,176,14,0,109,85,0,20,1,36,74,1,176,13,
		0,122,121,20,2,176,14,0,106,2,32,0,109,86,
		0,72,20,1,26,4,249,36,77,1,176,5,0,92,
		24,106,1,0,20,2,36,78,1,176,5,0,92,20,
		106,7,83,67,82,69,69,78,0,20,2,36,79,1,
		176,83,0,20,0,176,6,0,106,2,49,0,20,1,
		176,84,0,100,20,1,36,80,1,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}
