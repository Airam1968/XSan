/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_ctrd.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_CTRD );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( __SETFORMAT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_CTRD )
{ "REL_CTRD", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_CTRD )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "EDATA_INI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_FIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "DATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_AUX", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_PRE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUB_NPRE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_PRE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_NPRE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "SISLETRAC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "ECONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMPREENDIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CODIGO_CLI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECODIGO_CLI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PREVISTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REALIZADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NPREVISTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_CTRD, "C:/sis/SAN/xhb/rel_ctrd.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_CTRD
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_CTRD )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_CTRD )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,14,0,176,5,
		0,106,41,82,69,76,65,84,224,82,73,79,32,68,
		69,32,67,79,78,84,65,83,32,65,32,82,69,67,
		69,66,69,82,32,80,79,82,32,68,69,90,69,78,
		65,0,20,1,36,15,0,176,6,0,106,2,49,0,
		20,1,36,16,0,176,7,0,9,100,106,9,67,82,
		69,67,69,66,69,82,0,100,100,9,20,6,176,8,
		0,106,9,68,65,84,65,95,67,82,69,0,20,1,
		176,8,0,106,9,67,82,69,67,69,66,69,82,0,
		20,1,176,8,0,106,8,68,65,84,65,95,78,70,
		0,20,1,36,17,0,176,9,0,20,0,36,18,0,
		176,6,0,106,2,50,0,20,1,36,19,0,176,7,
		0,9,100,106,9,67,76,65,83,95,80,82,79,0,
		100,100,9,20,6,176,8,0,106,9,67,76,65,83,
		95,80,82,79,0,20,1,36,20,0,176,9,0,20,
		0,36,21,0,176,6,0,106,2,52,0,20,1,36,
		22,0,176,7,0,9,100,106,9,67,76,73,69,78,
		84,69,83,0,100,100,9,20,6,176,8,0,106,9,
		67,76,73,69,78,84,69,83,0,20,1,36,23,0,
		176,9,0,20,0,36,24,0,176,6,0,106,2,49,
		0,20,1,36,25,0,176,10,0,20,0,36,26,0,
		134,0,0,0,0,83,11,0,36,27,0,134,0,0,
		0,0,83,12,0,36,29,0,176,13,0,92,8,92,
		2,20,2,176,14,0,106,11,80,101,114,105,111,100,
		111,32,58,32,0,20,1,176,15,0,176,16,0,12,
		0,176,17,0,12,0,122,72,20,2,176,18,0,109,
		19,0,176,20,0,100,106,10,101,68,65,84,65,95,
		73,78,73,0,100,100,100,12,5,20,2,48,21,0,
		176,22,0,109,19,0,12,1,112,0,73,36,30,0,
		176,13,0,92,8,92,25,20,2,176,14,0,106,7,
		65,116,101,32,58,32,0,20,1,176,15,0,176,16,
		0,12,0,176,17,0,12,0,122,72,20,2,176,18,
		0,109,19,0,176,20,0,100,106,10,101,68,65,84,
		65,95,70,73,77,0,100,100,100,12,5,20,2,48,
		21,0,176,22,0,109,19,0,12,1,112,0,73,36,
		31,0,176,23,0,109,19,0,100,100,100,100,100,100,
		20,7,4,0,0,83,19,0,109,19,0,73,36,32,
		0,109,11,0,134,0,0,0,0,5,31,13,109,12,
		0,134,0,0,0,0,5,28,99,36,33,0,176,24,
		0,106,3,48,49,0,20,1,36,34,0,176,25,0,
		106,18,68,97,116,97,115,32,73,110,118,97,108,105,
		100,97,115,32,33,0,92,3,20,2,36,35,0,176,
		26,0,92,20,106,7,83,67,82,69,69,78,0,20,
		2,36,36,0,176,27,0,20,0,36,37,0,176,28,
		0,121,121,176,2,0,12,0,176,3,0,12,0,109,
		4,0,20,5,36,38,0,7,36,40,0,109,11,0,
		109,12,0,15,28,59,36,41,0,176,24,0,106,3,
		48,49,0,20,1,36,42,0,176,25,0,106,26,68,
		97,116,97,32,73,110,105,99,105,111,32,62,32,81,
		117,101,32,70,105,110,97,108,32,33,0,92,3,20,
		2,36,43,0,26,121,254,36,49,0,176,29,0,106,
		50,80,111,115,105,99,105,111,110,101,32,111,32,70,
		111,114,109,117,108,97,114,105,111,32,56,48,32,67,
		111,108,117,110,97,115,32,101,32,84,101,99,108,101,
		32,91,69,78,84,69,82,93,0,20,1,36,50,0,
		176,30,0,20,0,36,51,0,176,31,0,20,0,36,
		52,0,176,26,0,92,20,106,8,80,82,73,78,84,
		69,82,0,20,2,36,53,0,176,6,0,106,2,49,
		0,20,1,36,54,0,176,32,0,90,21,109,33,0,
		109,11,0,16,21,28,10,73,109,33,0,109,12,0,
		34,6,106,42,68,65,84,65,32,62,61,32,101,68,
		65,84,65,95,73,78,73,32,46,65,78,68,46,32,
		68,65,84,65,32,60,61,32,101,68,65,84,65,95,
		70,73,77,0,20,2,36,55,0,176,9,0,20,0,
		36,56,0,109,11,0,83,34,0,36,57,0,121,83,
		35,0,36,58,0,121,83,36,0,36,59,0,121,83,
		37,0,36,60,0,121,83,38,0,36,61,0,121,83,
		39,0,36,62,0,121,83,40,0,36,63,0,176,6,
		0,106,2,50,0,20,1,36,64,0,176,9,0,20,
		0,36,65,0,109,41,0,83,42,0,36,66,0,176,
		43,0,12,0,32,104,5,36,67,0,176,13,0,122,
		121,20,2,176,14,0,109,44,0,106,11,69,109,112,
		114,101,115,97,32,58,32,0,72,109,45,0,72,20,
		1,36,68,0,176,13,0,122,92,115,20,2,176,14,
		0,106,8,68,97,116,97,32,58,32,0,176,46,0,
		176,47,0,12,0,12,1,72,20,1,36,69,0,176,
		13,0,92,2,121,20,2,176,14,0,106,11,83,105,
		115,116,101,109,97,32,58,32,0,109,48,0,72,20,
		1,36,70,0,176,13,0,92,2,92,115,20,2,176,
		14,0,106,14,82,101,108,97,116,58,32,83,65,78,
		48,48,52,0,20,1,36,71,0,176,13,0,92,3,
		92,30,20,2,176,14,0,109,49,0,106,21,67,79,
		78,84,65,83,32,65,32,82,69,67,69,66,69,82,
		32,68,69,32,0,72,176,46,0,109,11,0,12,1,
		72,106,6,32,65,84,69,32,0,72,176,46,0,109,
		12,0,12,1,72,109,44,0,72,20,1,36,72,0,
		176,13,0,92,4,121,20,2,176,14,0,106,18,84,
		105,112,111,32,82,101,108,97,116,162,114,105,111,32,
		58,32,0,109,50,0,106,2,80,0,5,28,15,106,
		9,80,82,69,86,73,83,84,79,0,25,14,106,10,
		82,69,65,76,73,90,65,68,79,0,72,20,1,36,
		73,0,176,13,0,92,4,92,50,20,2,176,14,0,
		106,13,67,111,110,116,97,32,78,111,46,32,58,32,
		0,176,51,0,109,52,0,92,5,12,2,72,20,1,
		36,74,0,176,13,0,92,5,121,20,2,176,14,0,
		176,53,0,106,2,61,0,93,135,0,12,2,20,1,
		36,75,0,176,13,0,92,6,92,2,20,2,176,14,
		0,106,10,78,111,46,32,76,65,78,67,46,0,20,
		1,36,76,0,176,13,0,92,6,92,12,20,2,176,
		14,0,106,5,68,65,84,65,0,20,1,36,77,0,
		176,13,0,92,6,92,22,20,2,176,14,0,106,8,
		67,76,73,69,78,84,69,0,20,1,36,78,0,176,
		13,0,92,6,92,32,20,2,176,14,0,106,21,68,
		69,83,67,82,73,67,65,79,32,68,65,32,82,69,
		67,69,73,84,65,0,20,1,36,79,0,176,13,0,
		92,6,92,86,20,2,176,14,0,106,9,80,82,69,
		86,73,83,84,79,0,20,1,36,80,0,176,13,0,
		92,6,92,102,20,2,176,14,0,106,10,82,69,65,
		76,73,90,65,68,79,0,20,1,36,81,0,176,13,
		0,92,6,92,118,20,2,176,14,0,106,11,78,46,
		80,82,69,86,73,83,84,79,0,20,1,36,82,0,
		176,13,0,92,8,121,20,2,176,14,0,176,53,0,
		106,2,61,0,93,135,0,12,2,20,1,36,83,0,
		92,9,83,54,0,36,84,0,176,43,0,12,0,32,
		233,1,109,54,0,92,54,34,29,224,1,36,85,0,
		176,6,0,106,2,49,0,20,1,36,86,0,176,9,
		0,20,0,36,87,0,176,43,0,12,0,31,82,36,
		88,0,109,55,0,109,42,0,5,28,59,36,89,0,
		109,56,0,83,57,0,36,90,0,109,58,0,83,59,
		0,36,91,0,109,35,0,109,60,0,72,83,35,0,
		36,92,0,109,36,0,109,61,0,72,83,36,0,36,
		93,0,109,37,0,109,62,0,72,83,37,0,36,95,
		0,176,63,0,122,20,1,25,168,36,97,0,176,6,
		0,106,2,50,0,20,1,36,98,0,109,35,0,121,
		69,31,17,109,36,0,121,69,31,10,109,37,0,121,
		69,29,231,0,36,99,0,176,13,0,109,54,0,92,
		22,20,2,176,64,0,109,57,0,106,6,57,57,57,
		57,57,0,20,2,36,100,0,176,13,0,109,54,0,
		92,32,20,2,176,14,0,176,65,0,109,59,0,12,
		1,20,1,36,101,0,176,13,0,109,54,0,92,84,
		20,2,176,64,0,109,35,0,106,18,64,82,32,57,
		57,57,44,57,57,57,44,57,57,57,46,57,57,0,
		20,2,36,102,0,176,13,0,109,54,0,92,100,20,
		2,176,64,0,109,36,0,106,18,64,82,32,57,57,
		57,44,57,57,57,44,57,57,57,46,57,57,0,20,
		2,36,103,0,176,13,0,109,54,0,92,116,20,2,
		176,64,0,109,37,0,106,18,64,82,32,57,57,57,
		44,57,57,57,44,57,57,57,46,57,57,0,20,2,
		36,104,0,109,38,0,109,35,0,72,83,38,0,36,
		105,0,109,39,0,109,36,0,72,83,39,0,36,106,
		0,109,40,0,109,37,0,72,83,40,0,36,107,0,
		109,54,0,122,72,83,54,0,36,109,0,176,63,0,
		122,20,1,36,110,0,109,41,0,83,42,0,36,111,
		0,121,83,35,0,36,112,0,121,83,36,0,36,113,
		0,121,83,37,0,36,114,0,176,66,0,20,0,36,
		115,0,109,67,0,29,64,254,36,116,0,176,26,0,
		92,20,106,7,83,67,82,69,69,78,0,20,2,36,
		117,0,176,27,0,20,0,176,6,0,106,2,49,0,
		20,1,176,68,0,100,20,1,36,118,0,7,36,121,
		0,176,43,0,12,0,29,202,0,36,122,0,176,13,
		0,109,54,0,92,84,20,2,176,14,0,176,53,0,
		106,2,95,0,92,48,12,2,20,1,36,123,0,109,
		54,0,122,72,83,54,0,36,124,0,176,13,0,109,
		54,0,92,2,20,2,176,14,0,106,10,84,111,116,
		97,105,115,32,58,32,0,20,1,36,125,0,176,13,
		0,109,54,0,92,82,20,2,176,64,0,109,38,0,
		106,20,64,82,32,57,44,57,57,57,44,57,57,57,
		44,57,57,57,46,57,57,0,20,2,36,126,0,176,
		13,0,109,54,0,92,98,20,2,176,64,0,109,39,
		0,106,20,64,82,32,57,44,57,57,57,44,57,57,
		57,44,57,57,57,46,57,57,0,20,2,36,127,0,
		176,13,0,109,54,0,92,114,20,2,176,64,0,109,
		40,0,106,20,64,82,32,57,44,57,57,57,44,57,
		57,57,44,57,57,57,46,57,57,0,20,2,36,129,
		0,176,13,0,92,56,121,20,2,176,14,0,176,53,
		0,106,2,61,0,93,135,0,12,2,20,1,36,130,
		0,176,13,0,92,57,92,95,20,2,176,14,0,109,
		69,0,20,1,36,131,0,176,13,0,122,121,20,2,
		176,14,0,106,2,32,0,109,70,0,72,20,1,26,
		147,250,36,133,0,176,26,0,92,20,106,7,83,67,
		82,69,69,78,0,20,2,36,134,0,176,27,0,20,
		0,36,135,0,176,28,0,121,121,176,2,0,12,0,
		176,3,0,12,0,109,4,0,20,5,36,136,0,7
	};

	hb_vmExecute( pcode, symbols );
}
