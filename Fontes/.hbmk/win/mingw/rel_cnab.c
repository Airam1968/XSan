/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_cnab.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_CNAB );
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
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( DBCREATEINDEX );
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


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_CNAB )
{ "REL_CNAB", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_CNAB )}, NULL },
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
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "ECOD_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "COD_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NOME_ARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENOME_ARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "CDOC_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCREATEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCREATEINDEX )}, NULL },
{ "UEMP_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NOMS_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_TIT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_CRE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_CUS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "DTVE_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETAM_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTCR_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETAM_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETAM_DTC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDT_CRED", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EANO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
{ "ETAM_DTV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDT_VENC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "VLTI_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "JMMU_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NSEQ_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TACO_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FIL2_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NONU_TRN01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_CNAB, "C:/sis/SAN/xhb/rel_cnab.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_CNAB
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_CNAB )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_CNAB )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,106,17,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,0,20,1,36,
		15,0,109,2,0,106,2,83,0,5,29,106,23,36,
		16,0,176,3,0,106,37,82,69,76,65,84,79,82,
		73,79,32,67,79,66,82,65,78,67,65,32,66,65,
		78,67,65,82,73,65,32,45,32,66,65,73,88,65,
		83,0,20,1,36,17,0,176,4,0,106,3,48,49,
		0,20,1,36,18,0,176,5,0,122,12,1,83,6,
		0,36,19,0,176,7,0,92,23,92,2,20,2,176,
		8,0,106,39,79,114,100,101,109,32,65,108,102,97,
		98,130,116,105,99,97,32,111,117,32,78,117,109,130,
		114,105,99,97,32,40,65,47,78,41,32,32,58,32,
		0,20,1,176,9,0,176,10,0,12,0,176,11,0,
		12,0,122,72,20,2,176,12,0,109,13,0,176,14,
		0,100,106,6,101,84,73,80,79,0,106,2,88,0,
		90,12,109,6,0,106,3,65,78,0,24,6,100,12,
		5,20,2,48,15,0,176,16,0,109,13,0,12,1,
		112,0,73,36,20,0,176,17,0,109,13,0,100,100,
		100,100,100,100,20,7,4,0,0,83,13,0,109,13,
		0,73,36,21,0,176,5,0,92,60,12,1,83,18,
		0,36,22,0,109,19,0,106,3,58,92,0,72,109,
		20,0,72,106,11,67,84,65,80,65,71,46,84,88,
		84,0,72,176,5,0,92,30,12,1,72,83,18,0,
		36,23,0,176,21,0,109,19,0,12,1,106,1,0,
		5,28,62,176,21,0,109,20,0,12,1,106,1,0,
		5,28,48,36,24,0,106,38,67,58,92,82,69,76,
		65,84,79,82,73,79,83,92,82,69,76,65,84,79,
		82,73,79,46,84,88,84,32,32,32,32,32,32,32,
		32,32,32,0,83,18,0,36,26,0,176,22,0,106,
		50,80,111,115,105,99,105,111,110,101,32,111,32,70,
		111,114,109,117,108,97,114,105,111,32,56,48,32,67,
		111,108,117,110,97,115,32,101,32,84,101,99,108,101,
		32,91,69,78,84,69,82,93,0,20,1,36,27,0,
		176,23,0,20,0,36,28,0,176,24,0,20,0,36,
		29,0,176,25,0,106,2,50,0,20,1,36,30,0,
		176,26,0,9,100,106,7,66,65,78,67,79,83,0,
		100,100,9,20,6,176,27,0,106,7,66,65,78,67,
		79,83,0,20,1,36,31,0,176,28,0,20,0,36,
		32,0,176,29,0,20,0,36,33,0,176,30,0,106,
		9,87,47,78,44,43,87,47,82,0,20,1,36,34,
		0,121,83,31,0,36,35,0,176,7,0,92,5,92,
		2,20,2,176,8,0,106,11,66,97,110,99,111,32,
		32,32,58,32,0,20,1,36,36,0,176,28,0,20,
		0,36,37,0,92,2,3,1,0,92,2,3,1,0,
		92,2,3,1,0,92,2,3,1,0,176,36,0,108,
		32,108,33,108,34,108,35,20,4,81,35,0,81,34,
		0,81,33,0,81,32,0,36,38,0,176,37,0,109,
		32,0,120,109,35,0,20,3,36,39,0,106,7,64,
		82,57,57,57,57,0,98,33,0,122,2,36,40,0,
		106,5,64,83,53,48,0,98,33,0,92,2,2,36,
		41,0,106,5,32,32,32,32,0,98,34,0,122,2,
		36,42,0,106,5,32,32,32,32,0,98,34,0,92,
		2,2,36,43,0,176,38,0,92,5,92,15,92,5,
		92,75,109,32,0,106,11,70,95,67,79,78,70,95,
		67,79,66,0,109,33,0,109,34,0,106,2,32,0,
		106,4,32,45,32,0,106,2,32,0,20,11,36,44,
		0,109,39,0,83,31,0,36,45,0,176,21,0,109,
		40,0,12,1,83,41,0,36,46,0,176,42,0,92,
		8,106,3,79,78,0,20,2,36,48,0,109,31,0,
		122,5,28,97,36,49,0,176,25,0,106,2,49,0,
		20,1,36,50,0,176,26,0,9,100,106,9,84,82,
		65,78,83,48,48,49,0,100,100,9,20,6,36,51,
		0,176,28,0,20,0,36,52,0,176,43,0,90,12,
		109,44,0,106,3,48,54,0,5,6,106,18,67,68,
		79,67,95,84,82,78,48,49,32,61,32,34,48,54,
		34,0,20,2,36,53,0,176,28,0,20,0,26,99,
		11,36,54,0,109,31,0,92,8,5,28,97,36,55,
		0,176,25,0,106,2,49,0,20,1,36,56,0,176,
		26,0,9,100,106,9,84,82,65,78,83,48,48,56,
		0,100,100,9,20,6,36,57,0,176,28,0,20,0,
		36,58,0,176,43,0,90,12,109,44,0,106,3,48,
		54,0,5,6,106,18,67,68,79,67,95,84,82,78,
		48,49,32,61,32,34,48,54,34,0,20,2,36,59,
		0,176,28,0,20,0,26,249,10,36,60,0,109,31,
		0,92,33,5,29,181,0,36,61,0,176,25,0,106,
		2,49,0,20,1,36,62,0,176,26,0,9,100,106,
		9,84,82,65,78,83,48,51,51,0,100,100,9,20,
		6,36,63,0,176,28,0,20,0,36,64,0,176,43,
		0,90,25,109,44,0,106,3,48,54,0,5,21,31,
		12,73,109,44,0,106,3,49,53,0,5,6,106,41,
		67,68,79,67,95,84,82,78,48,49,32,61,32,34,
		48,54,34,32,46,79,82,46,32,67,68,79,67,95,
		84,82,78,48,49,32,61,32,34,49,53,34,0,20,
		2,36,65,0,176,28,0,20,0,36,66,0,176,45,
		0,106,9,84,82,65,78,83,84,88,84,0,106,11,
		85,69,77,80,95,84,82,78,48,49,0,90,6,109,
		46,0,6,100,20,4,36,67,0,176,28,0,20,0,
		26,59,10,36,68,0,109,31,0,92,104,5,29,181,
		0,36,69,0,176,25,0,106,2,49,0,20,1,36,
		70,0,176,26,0,9,100,106,9,84,82,65,78,83,
		49,48,52,0,100,100,9,20,6,36,71,0,176,28,
		0,20,0,36,72,0,176,43,0,90,25,109,44,0,
		106,3,50,49,0,5,21,31,12,73,109,44,0,106,
		3,50,50,0,5,6,106,41,67,68,79,67,95,84,
		82,78,48,49,32,61,32,34,50,49,34,32,46,79,
		82,46,32,67,68,79,67,95,84,82,78,48,49,32,
		61,32,34,50,50,34,0,20,2,36,73,0,176,28,
		0,20,0,36,74,0,176,45,0,106,9,84,82,65,
		78,83,84,88,84,0,106,11,85,69,77,80,95,84,
		82,78,48,49,0,90,6,109,46,0,6,100,20,4,
		36,75,0,176,28,0,20,0,26,125,9,36,76,0,
		109,31,0,93,215,0,5,29,181,0,36,77,0,176,
		25,0,106,2,49,0,20,1,36,78,0,176,26,0,
		9,100,106,9,84,82,65,78,83,50,49,53,0,100,
		100,9,20,6,36,79,0,176,28,0,20,0,36,80,
		0,176,43,0,90,25,109,44,0,106,3,48,54,0,
		5,21,31,12,73,109,44,0,106,3,48,56,0,5,
		6,106,41,67,68,79,67,95,84,82,78,48,49,32,
		61,32,34,48,54,34,32,46,79,82,46,32,67,68,
		79,67,95,84,82,78,48,49,32,61,32,34,48,56,
		34,0,20,2,36,81,0,176,28,0,20,0,36,82,
		0,176,45,0,106,9,84,82,65,78,83,84,88,84,
		0,106,11,85,69,77,80,95,84,82,78,48,49,0,
		90,6,109,46,0,6,100,20,4,36,83,0,176,28,
		0,20,0,26,190,8,36,84,0,109,31,0,93,230,
		0,5,29,181,0,36,85,0,176,25,0,106,2,49,
		0,20,1,36,86,0,176,26,0,9,100,106,9,84,
		82,65,78,83,50,51,48,0,100,100,9,20,6,36,
		87,0,176,28,0,20,0,36,88,0,176,43,0,90,
		25,109,44,0,106,3,48,54,0,5,21,31,12,73,
		109,44,0,106,3,48,56,0,5,6,106,41,67,68,
		79,67,95,84,82,78,48,49,32,61,32,34,48,54,
		34,32,46,79,82,46,32,67,68,79,67,95,84,82,
		78,48,49,32,61,32,34,48,56,34,0,20,2,36,
		89,0,176,28,0,20,0,36,90,0,176,45,0,106,
		9,84,82,65,78,83,84,88,84,0,106,11,85,69,
		77,80,95,84,82,78,48,49,0,90,6,109,46,0,
		6,100,20,4,36,91,0,176,28,0,20,0,26,255,
		7,36,92,0,109,31,0,93,237,0,5,29,181,0,
		36,93,0,176,25,0,106,2,49,0,20,1,36,94,
		0,176,26,0,9,100,106,9,84,82,65,78,83,50,
		51,55,0,100,100,9,20,6,36,95,0,176,28,0,
		20,0,36,96,0,176,43,0,90,25,109,44,0,106,
		3,48,54,0,5,21,31,12,73,109,44,0,106,3,
		48,56,0,5,6,106,41,67,68,79,67,95,84,82,
		78,48,49,32,61,32,34,48,54,34,32,46,79,82,
		46,32,67,68,79,67,95,84,82,78,48,49,32,61,
		32,34,48,56,34,0,20,2,36,97,0,176,28,0,
		20,0,36,98,0,176,45,0,106,9,84,82,65,78,
		83,84,88,84,0,106,11,85,69,77,80,95,84,82,
		78,48,49,0,90,6,109,46,0,6,100,20,4,36,
		99,0,176,28,0,20,0,26,64,7,36,100,0,109,
		31,0,93,19,1,5,29,181,0,36,101,0,176,25,
		0,106,2,49,0,20,1,36,102,0,176,26,0,9,
		100,106,9,84,82,65,78,83,50,55,53,0,100,100,
		9,20,6,36,103,0,176,28,0,20,0,36,104,0,
		176,43,0,90,25,109,44,0,106,3,48,54,0,5,
		21,31,12,73,109,44,0,106,3,48,56,0,5,6,
		106,41,67,68,79,67,95,84,82,78,48,49,32,61,
		32,34,48,54,34,32,46,79,82,46,32,67,68,79,
		67,95,84,82,78,48,49,32,61,32,34,48,56,34,
		0,20,2,36,105,0,176,28,0,20,0,36,106,0,
		176,45,0,106,9,84,82,65,78,83,84,88,84,0,
		106,11,85,69,77,80,95,84,82,78,48,49,0,90,
		6,109,46,0,6,100,20,4,36,107,0,176,28,0,
		20,0,26,129,6,36,108,0,109,31,0,93,35,1,
		5,29,181,0,36,109,0,176,25,0,106,2,49,0,
		20,1,36,110,0,176,26,0,9,100,106,9,84,82,
		65,78,83,50,57,49,0,100,100,9,20,6,36,111,
		0,176,28,0,20,0,36,112,0,176,43,0,90,25,
		109,44,0,106,3,48,54,0,5,21,31,12,73,109,
		44,0,106,3,48,56,0,5,6,106,41,67,68,79,
		67,95,84,82,78,48,49,32,61,32,34,48,54,34,
		32,46,79,82,46,32,67,68,79,67,95,84,82,78,
		48,49,32,61,32,34,48,56,34,0,20,2,36,113,
		0,176,28,0,20,0,36,114,0,176,45,0,106,9,
		84,82,65,78,83,84,88,84,0,106,11,85,69,77,
		80,95,84,82,78,48,49,0,90,6,109,46,0,6,
		100,20,4,36,115,0,176,28,0,20,0,26,194,5,
		36,116,0,109,31,0,93,85,1,5,29,27,1,36,
		117,0,176,25,0,106,2,49,0,20,1,36,118,0,
		176,26,0,9,100,106,9,84,82,65,78,83,51,52,
		49,0,100,100,9,20,6,36,119,0,176,28,0,20,
		0,36,120,0,176,43,0,90,38,109,44,0,106,3,
		48,54,0,5,21,31,25,73,109,44,0,106,3,48,
		55,0,5,21,31,12,73,109,44,0,106,3,48,56,
		0,5,6,106,64,67,68,79,67,95,84,82,78,48,
		49,32,61,32,34,48,54,34,32,46,79,82,46,32,
		67,68,79,67,95,84,82,78,48,49,32,61,32,34,
		48,55,34,32,46,79,82,46,32,67,68,79,67,95,
		84,82,78,48,49,32,61,32,34,48,56,34,0,20,
		2,36,121,0,176,28,0,20,0,36,122,0,109,6,
		0,106,2,65,0,5,28,52,36,123,0,176,45,0,
		106,9,84,82,65,78,83,84,88,84,0,106,11,78,
		79,77,83,95,84,82,78,48,49,0,90,6,109,47,
		0,6,100,20,4,36,124,0,176,28,0,20,0,26,
		210,4,36,126,0,176,45,0,106,9,84,82,65,78,
		83,84,88,84,0,106,11,85,69,77,80,95,84,82,
		78,48,49,0,90,6,109,46,0,6,100,20,4,36,
		127,0,176,28,0,20,0,36,128,0,26,157,4,36,
		129,0,109,31,0,93,153,1,5,29,27,1,36,130,
		0,176,25,0,106,2,49,0,20,1,36,131,0,176,
		26,0,9,100,106,9,84,82,65,78,83,52,48,57,
		0,100,100,9,20,6,36,132,0,176,28,0,20,0,
		36,133,0,176,43,0,90,38,109,44,0,106,3,48,
		54,0,5,21,31,25,73,109,44,0,106,3,57,56,
		0,5,21,31,12,73,109,44,0,106,3,57,57,0,
		5,6,106,64,67,68,79,67,95,84,82,78,48,49,
		32,61,32,34,48,54,34,32,46,79,82,46,32,67,
		68,79,67,95,84,82,78,48,49,32,61,32,34,57,
		56,34,32,46,79,82,46,32,67,68,79,67,95,84,
		82,78,48,49,32,61,32,34,57,57,34,0,20,2,
		36,134,0,176,28,0,20,0,36,135,0,109,6,0,
		106,2,65,0,5,28,52,36,136,0,176,45,0,106,
		9,84,82,65,78,83,84,88,84,0,106,11,78,79,
		77,83,95,84,82,78,48,49,0,90,6,109,47,0,
		6,100,20,4,36,137,0,176,28,0,20,0,26,173,
		3,36,139,0,176,45,0,106,9,84,82,65,78,83,
		84,88,84,0,106,11,85,69,77,80,95,84,82,78,
		48,49,0,90,6,109,46,0,6,100,20,4,36,140,
		0,176,28,0,20,0,36,141,0,26,120,3,36,142,
		0,109,31,0,93,166,1,5,29,247,0,36,143,0,
		176,25,0,106,2,49,0,20,1,36,144,0,176,26,
		0,9,100,106,9,84,82,65,78,83,52,50,50,0,
		100,100,9,20,6,36,145,0,176,28,0,20,0,36,
		146,0,176,43,0,90,25,109,44,0,106,3,48,54,
		0,5,21,31,12,73,109,44,0,106,3,49,53,0,
		5,6,106,41,67,68,79,67,95,84,82,78,48,49,
		32,61,32,34,48,54,34,32,46,79,82,46,32,67,
		68,79,67,95,84,82,78,48,49,32,61,32,34,49,
		53,34,0,20,2,36,147,0,176,28,0,20,0,36,
		148,0,109,6,0,106,2,65,0,5,28,52,36,149,
		0,176,45,0,106,9,84,82,65,78,83,84,88,84,
		0,106,11,78,79,77,83,95,84,82,78,48,49,0,
		90,6,109,47,0,6,100,20,4,36,150,0,176,28,
		0,20,0,26,172,2,36,152,0,176,45,0,106,9,
		84,82,65,78,83,84,88,84,0,106,11,85,69,77,
		80,95,84,82,78,48,49,0,90,6,109,46,0,6,
		100,20,4,36,153,0,176,28,0,20,0,36,154,0,
		26,119,2,36,155,0,109,31,0,93,168,1,5,29,
		247,0,36,156,0,176,25,0,106,2,49,0,20,1,
		36,157,0,176,26,0,9,100,106,9,84,82,65,78,
		83,52,50,52,0,100,100,9,20,6,36,158,0,176,
		28,0,20,0,36,159,0,176,43,0,90,25,109,44,
		0,106,3,48,54,0,5,21,31,12,73,109,44,0,
		106,3,49,53,0,5,6,106,41,67,68,79,67,95,
		84,82,78,48,49,32,61,32,34,48,54,34,32,46,
		79,82,46,32,67,68,79,67,95,84,82,78,48,49,
		32,61,32,34,49,53,34,0,20,2,36,160,0,176,
		28,0,20,0,36,161,0,109,6,0,106,2,65,0,
		5,28,52,36,162,0,176,45,0,106,9,84,82,65,
		78,83,84,88,84,0,106,11,78,79,77,83,95,84,
		82,78,48,49,0,90,6,109,47,0,6,100,20,4,
		36,163,0,176,28,0,20,0,26,171,1,36,165,0,
		176,45,0,106,9,84,82,65,78,83,84,88,84,0,
		106,11,85,69,77,80,95,84,82,78,48,49,0,90,
		6,109,46,0,6,100,20,4,36,166,0,176,28,0,
		20,0,36,167,0,26,118,1,36,168,0,109,31,0,
		93,129,2,5,29,61,1,36,169,0,176,25,0,106,
		2,49,0,20,1,36,170,0,176,26,0,9,100,106,
		9,84,82,65,78,83,54,52,49,0,100,100,9,20,
		6,36,171,0,176,28,0,20,0,36,172,0,176,43,
		0,90,51,109,44,0,106,3,48,54,0,5,21,31,
		38,73,109,44,0,106,3,49,53,0,5,21,31,25,
		73,109,44,0,106,3,49,54,0,5,21,31,12,73,
		109,44,0,106,3,49,55,0,5,6,106,87,67,68,
		79,67,95,84,82,78,48,49,32,61,32,34,48,54,
		34,32,46,79,82,46,32,67,68,79,67,95,84,82,
		78,48,49,32,61,32,34,49,53,34,32,46,79,82,
		46,32,67,68,79,67,95,84,82,78,48,49,32,61,
		32,34,49,54,34,32,46,79,82,46,32,67,68,79,
		67,95,84,82,78,48,49,32,61,32,34,49,55,34,
		0,20,2,36,173,0,176,28,0,20,0,36,174,0,
		109,6,0,106,2,65,0,5,28,51,36,175,0,176,
		45,0,106,9,84,82,65,78,83,84,88,84,0,106,
		11,78,79,77,83,95,84,82,78,48,49,0,90,6,
		109,47,0,6,100,20,4,36,176,0,176,28,0,20,
		0,25,98,36,178,0,176,45,0,106,9,84,82,65,
		78,83,84,88,84,0,106,11,85,69,77,80,95,84,
		82,78,48,49,0,90,6,109,46,0,6,100,20,4,
		36,179,0,176,28,0,20,0,36,180,0,25,46,36,
		182,0,176,25,0,106,2,49,0,20,1,36,183,0,
		176,26,0,9,100,106,9,84,82,65,78,83,84,88,
		84,0,100,100,9,20,6,36,184,0,176,28,0,20,
		0,36,186,0,176,42,0,92,8,106,4,79,70,70,
		0,20,2,36,187,0,121,83,48,0,36,188,0,121,
		83,49,0,36,189,0,121,83,50,0,36,190,0,176,
		42,0,92,20,106,8,80,82,73,78,84,69,82,0,
		20,2,36,191,0,176,51,0,12,0,32,82,8,36,
		192,0,176,7,0,122,121,20,2,176,8,0,109,52,
		0,106,11,69,109,112,114,101,115,97,32,58,32,0,
		72,109,53,0,72,20,1,36,193,0,176,7,0,122,
		92,115,20,2,176,8,0,106,8,68,97,116,97,32,
		58,32,0,176,54,0,176,55,0,12,0,12,1,72,
		20,1,36,194,0,176,7,0,92,2,121,20,2,176,
		8,0,106,11,83,105,115,116,101,109,97,32,58,32,
		0,109,56,0,72,20,1,36,195,0,176,7,0,92,
		2,92,115,20,2,176,8,0,106,14,82,101,108,97,
		116,58,32,83,65,78,48,48,49,0,20,1,36,196,
		0,176,7,0,92,3,92,30,20,2,176,8,0,109,
		57,0,106,40,82,69,76,65,84,79,82,73,79,32,
		67,79,66,82,65,78,67,65,32,66,65,78,67,65,
		82,73,65,32,45,32,66,65,73,88,65,83,32,45,
		32,0,72,109,41,0,72,109,52,0,72,20,1,36,
		197,0,176,7,0,92,4,121,20,2,176,8,0,176,
		58,0,106,2,61,0,93,135,0,12,2,20,1,36,
		198,0,176,7,0,92,5,121,20,2,176,8,0,106,
		2,124,0,20,1,36,199,0,176,7,0,92,5,92,
		2,20,2,176,8,0,106,11,78,79,83,83,79,32,
		78,85,77,46,0,20,1,36,200,0,176,7,0,92,
		5,92,14,20,2,176,8,0,106,13,78,79,77,69,
		32,67,76,73,69,78,84,69,0,20,1,36,201,0,
		176,7,0,92,5,92,46,20,2,176,8,0,106,11,
		68,65,84,65,32,86,69,78,67,46,0,20,1,36,
		202,0,176,7,0,92,5,92,58,20,2,176,8,0,
		106,12,86,76,82,46,32,84,73,84,85,76,79,0,
		20,1,36,203,0,176,7,0,92,5,92,70,20,2,
		176,8,0,106,11,68,65,84,65,32,80,71,84,79,
		46,0,20,1,36,204,0,176,7,0,92,5,92,82,
		20,2,176,8,0,106,11,86,76,82,46,32,80,71,
		84,79,46,0,20,1,36,205,0,176,7,0,92,5,
		92,97,20,2,176,8,0,106,5,67,79,68,46,0,
		20,1,36,206,0,176,7,0,92,5,92,103,20,2,
		176,8,0,106,5,83,69,81,46,0,20,1,36,207,
		0,176,7,0,92,5,92,108,20,2,176,8,0,106,
		6,67,85,83,84,79,0,20,1,36,208,0,176,7,
		0,92,5,92,121,20,2,176,8,0,106,7,66,79,
		76,69,84,79,0,20,1,36,209,0,176,7,0,92,
		5,93,134,0,20,2,176,8,0,106,2,124,0,20,
		1,36,210,0,176,7,0,92,6,121,20,2,176,8,
		0,176,58,0,106,2,61,0,93,135,0,12,2,20,
		1,36,211,0,92,7,83,59,0,36,212,0,176,51,
		0,12,0,32,214,4,109,59,0,92,54,34,29,205,
		4,36,213,0,176,60,0,176,21,0,176,61,0,109,
		62,0,12,1,12,1,12,1,83,63,0,36,214,0,
		176,60,0,176,21,0,176,61,0,109,64,0,12,1,
		12,1,12,1,83,65,0,36,215,0,134,0,0,0,
		0,83,66,0,36,216,0,134,0,0,0,0,83,67,
		0,36,218,0,176,60,0,176,21,0,176,61,0,109,
		64,0,92,6,121,12,3,12,1,12,1,83,68,0,
		36,219,0,109,68,0,92,6,69,28,36,176,58,0,
		106,2,48,0,92,6,109,68,0,49,12,2,176,21,
		0,176,61,0,109,64,0,92,6,121,12,3,12,1,
		72,25,18,176,21,0,176,61,0,109,64,0,92,6,
		121,12,3,12,1,83,69,0,36,220,0,121,83,70,
		0,36,221,0,176,71,0,176,72,0,109,69,0,92,
		5,92,2,12,3,12,1,83,70,0,36,222,0,109,
		70,0,92,50,16,28,76,109,70,0,92,99,34,28,
		68,36,223,0,176,73,0,176,72,0,109,69,0,122,
		92,2,12,3,106,2,47,0,72,176,72,0,109,69,
		0,92,3,92,2,12,3,72,106,2,47,0,72,106,
		3,49,57,0,72,176,72,0,109,69,0,92,5,92,
		2,12,3,72,12,1,83,67,0,25,66,36,225,0,
		176,73,0,176,72,0,109,69,0,122,92,2,12,3,
		106,2,47,0,72,176,72,0,109,69,0,92,3,92,
		2,12,3,72,106,2,47,0,72,106,3,50,48,0,
		72,176,72,0,109,69,0,92,5,92,2,12,3,72,
		12,1,83,67,0,36,228,0,176,60,0,176,21,0,
		176,61,0,109,62,0,92,6,121,12,3,12,1,12,
		1,83,74,0,36,229,0,109,74,0,92,6,69,28,
		36,176,58,0,106,2,48,0,92,6,109,74,0,49,
		12,2,176,21,0,176,61,0,109,62,0,92,6,121,
		12,3,12,1,72,25,18,176,21,0,176,61,0,109,
		62,0,92,6,121,12,3,12,1,83,75,0,36,230,
		0,121,83,70,0,36,231,0,176,71,0,176,72,0,
		109,75,0,92,5,92,2,12,3,12,1,83,70,0,
		36,232,0,109,70,0,92,50,16,28,76,109,70,0,
		92,99,34,28,68,36,233,0,176,73,0,176,72,0,
		109,75,0,122,92,2,12,3,106,2,47,0,72,176,
		72,0,109,75,0,92,3,92,2,12,3,72,106,2,
		47,0,72,106,3,49,57,0,72,176,72,0,109,75,
		0,92,5,92,2,12,3,72,12,1,83,66,0,25,
		66,36,235,0,176,73,0,176,72,0,109,75,0,122,
		92,2,12,3,106,2,47,0,72,176,72,0,109,75,
		0,92,3,92,2,12,3,72,106,2,47,0,72,106,
		3,50,48,0,72,176,72,0,109,75,0,92,5,92,
		2,12,3,72,12,1,83,66,0,36,237,0,176,7,
		0,109,59,0,121,20,2,176,8,0,106,2,124,0,
		20,1,36,238,0,176,7,0,109,59,0,92,2,20,
		2,176,8,0,176,21,0,109,46,0,12,1,20,1,
		36,239,0,109,31,0,93,85,1,5,28,28,36,240,
		0,176,7,0,109,59,0,92,14,20,2,176,8,0,
		176,21,0,109,47,0,12,1,20,1,36,242,0,176,
		7,0,109,59,0,92,46,20,2,176,8,0,109,66,
		0,20,1,36,243,0,176,7,0,109,59,0,92,58,
		20,2,176,76,0,109,77,0,106,14,64,82,32,57,
		57,57,44,57,57,57,46,57,57,0,20,2,36,244,
		0,176,7,0,109,59,0,92,70,20,2,176,8,0,
		109,67,0,20,1,36,245,0,176,7,0,109,59,0,
		92,82,20,2,176,76,0,109,77,0,109,78,0,72,
		106,14,64,82,32,57,57,57,44,57,57,57,46,57,
		57,0,20,2,36,246,0,176,7,0,109,59,0,92,
		97,20,2,176,76,0,109,44,0,106,6,64,82,32,
		57,57,0,20,2,36,247,0,176,7,0,109,59,0,
		92,102,20,2,176,76,0,109,79,0,106,9,64,82,
		32,57,57,57,57,57,0,20,2,36,248,0,176,7,
		0,109,59,0,92,108,20,2,176,76,0,109,80,0,
		106,14,64,82,32,57,57,57,44,57,57,57,46,57,
		57,0,20,2,36,250,0,109,31,0,93,35,1,5,
		28,45,36,251,0,176,7,0,109,59,0,92,120,20,
		2,176,76,0,109,81,0,106,18,64,82,32,57,57,
		57,57,57,57,57,57,57,57,57,57,57,57,0,20,
		2,25,99,36,252,0,109,31,0,93,129,2,5,28,
		46,36,253,0,176,7,0,109,59,0,92,119,20,2,
		176,76,0,109,82,0,106,19,64,82,32,57,57,57,
		57,57,57,57,57,57,57,57,57,57,57,57,0,20,
		2,25,43,36,255,0,176,7,0,109,59,0,92,120,
		20,2,176,76,0,109,82,0,106,18,64,82,32,57,
		57,57,57,57,57,57,57,57,57,57,57,57,57,0,
		20,2,36,1,1,176,7,0,109,59,0,93,134,0,
		20,2,176,8,0,106,2,124,0,20,1,36,2,1,
		109,59,0,122,72,83,59,0,36,3,1,109,48,0,
		109,77,0,72,83,48,0,36,4,1,109,49,0,109,
		77,0,72,109,78,0,72,83,49,0,36,5,1,109,
		31,0,122,69,28,15,36,6,1,109,50,0,109,80,
		0,72,83,50,0,36,8,1,176,83,0,122,20,1,
		36,9,1,176,84,0,20,0,36,10,1,109,85,0,
		29,85,251,36,11,1,176,42,0,92,20,106,7,83,
		67,82,69,69,78,0,20,2,36,12,1,176,86,0,
		20,0,176,25,0,106,2,49,0,20,1,176,87,0,
		100,20,1,36,13,1,100,110,7,36,16,1,176,51,
		0,12,0,29,201,0,36,17,1,176,7,0,109,59,
		0,121,20,2,176,8,0,176,58,0,106,2,45,0,
		93,135,0,12,2,20,1,36,18,1,109,59,0,122,
		72,83,59,0,36,19,1,176,7,0,109,59,0,92,
		2,20,2,176,8,0,106,15,84,111,116,97,108,32,
		71,101,114,97,108,32,58,32,0,20,1,36,20,1,
		176,7,0,109,59,0,92,54,20,2,176,76,0,109,
		48,0,106,18,64,82,32,57,57,57,44,57,57,57,
		44,57,57,57,46,57,57,0,20,2,36,21,1,176,
		7,0,109,59,0,92,78,20,2,176,76,0,109,49,
		0,106,18,64,82,32,57,57,57,44,57,57,57,44,
		57,57,57,46,57,57,0,20,2,36,22,1,176,7,
		0,109,59,0,92,104,20,2,176,76,0,109,50,0,
		106,18,64,82,32,57,57,57,44,57,57,57,44,57,
		57,57,46,57,57,0,20,2,36,24,1,176,7,0,
		92,56,121,20,2,176,8,0,176,58,0,106,2,61,
		0,93,135,0,12,2,20,1,36,25,1,176,7,0,
		92,57,92,95,20,2,176,8,0,109,88,0,20,1,
		36,26,1,176,7,0,122,121,20,2,176,8,0,106,
		2,32,0,109,89,0,72,20,1,26,169,247,36,29,
		1,176,42,0,92,24,106,1,0,20,2,36,30,1,
		176,42,0,92,20,106,7,83,67,82,69,69,78,0,
		20,2,36,31,1,176,86,0,20,0,176,25,0,106,
		2,49,0,20,1,176,87,0,100,20,1,36,32,1,
		100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

