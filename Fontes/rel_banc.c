/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_banc.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_BANC );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( ALLTRIM );
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
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_BANC )
{ "REL_BANC", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_BANC )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA07", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
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
{ "COD_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NOME_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_BANC, "C:/sis/SAN/xhb/rel_banc.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_BANC
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_BANC )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_BANC )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,114,101,108,95,98,97,110,99,46,112,114,103,
		58,82,69,76,95,66,65,78,67,0,36,14,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,15,0,176,5,0,106,32,82,
		69,76,65,84,79,82,73,79,32,68,69,32,67,65,
		68,65,83,84,82,79,32,68,69,32,66,65,78,67,
		79,83,0,20,1,36,16,0,176,6,0,106,2,49,
		0,20,1,36,17,0,176,7,0,9,100,106,7,66,
		65,78,67,79,83,0,100,100,9,20,6,176,8,0,
		106,7,66,65,78,67,79,83,0,20,1,36,18,0,
		176,9,0,20,0,36,19,0,176,6,0,106,2,49,
		0,20,1,36,20,0,176,9,0,20,0,36,21,0,
		176,10,0,92,60,12,1,83,11,0,36,22,0,109,
		12,0,106,3,58,92,0,72,109,13,0,72,106,11,
		67,84,65,80,65,71,46,84,88,84,0,72,176,10,
		0,92,30,12,1,72,83,11,0,36,23,0,176,14,
		0,109,12,0,12,1,106,1,0,5,28,62,176,14,
		0,109,13,0,12,1,106,1,0,5,28,48,36,24,
		0,106,38,67,58,92,82,69,76,65,84,79,82,73,
		79,83,92,82,69,76,65,84,79,82,73,79,46,84,
		88,84,32,32,32,32,32,32,32,32,32,32,0,83,
		11,0,36,26,0,176,15,0,106,50,80,111,115,105,
		99,105,111,110,101,32,111,32,70,111,114,109,117,108,
		97,114,105,111,32,56,48,32,67,111,108,117,110,97,
		115,32,101,32,84,101,99,108,101,32,91,69,78,84,
		69,82,93,0,20,1,36,27,0,176,16,0,20,0,
		36,28,0,176,17,0,20,0,36,29,0,176,18,0,
		92,20,106,8,80,82,73,78,84,69,82,0,20,2,
		36,30,0,176,19,0,12,0,32,36,2,36,31,0,
		176,20,0,122,121,20,2,176,21,0,109,22,0,106,
		11,69,109,112,114,101,115,97,32,58,32,0,72,109,
		23,0,72,20,1,36,32,0,176,20,0,122,92,120,
		20,2,176,21,0,106,8,68,97,116,97,32,58,32,
		0,176,24,0,176,25,0,12,0,12,1,72,20,1,
		36,33,0,176,20,0,92,2,121,20,2,176,21,0,
		106,11,83,105,115,116,101,109,97,32,58,32,0,109,
		26,0,72,20,1,36,34,0,176,20,0,92,2,92,
		121,20,2,176,21,0,106,14,82,101,108,97,116,58,
		32,83,65,78,48,48,55,0,20,1,36,35,0,176,
		20,0,92,3,92,40,20,2,176,21,0,109,27,0,
		106,19,67,65,68,65,83,84,82,79,32,68,69,32,
		66,65,78,67,79,83,0,72,109,22,0,72,20,1,
		36,36,0,176,20,0,92,5,121,20,2,176,21,0,
		176,28,0,106,2,61,0,93,135,0,12,2,20,1,
		36,37,0,176,20,0,92,6,92,2,20,2,176,21,
		0,106,16,67,79,68,73,71,79,32,70,69,66,82,
		65,66,65,77,0,20,1,36,38,0,176,20,0,92,
		6,92,30,20,2,176,21,0,106,11,78,79,77,69,
		32,66,65,78,67,79,0,20,1,36,39,0,176,20,
		0,92,7,121,20,2,176,21,0,176,28,0,106,2,
		61,0,93,135,0,12,2,20,1,36,40,0,92,8,
		83,29,0,36,41,0,176,19,0,12,0,32,149,0,
		109,29,0,92,57,34,29,140,0,36,42,0,176,20,
		0,109,29,0,92,5,20,2,176,30,0,109,31,0,
		106,9,64,82,32,57,57,57,57,57,0,20,2,36,
		43,0,176,20,0,109,29,0,92,30,20,2,176,21,
		0,109,32,0,20,1,36,44,0,109,29,0,122,72,
		83,29,0,36,45,0,176,33,0,122,20,1,36,46,
		0,176,34,0,20,0,36,47,0,109,35,0,28,149,
		36,48,0,176,18,0,92,20,106,7,83,67,82,69,
		69,78,0,20,2,36,49,0,176,36,0,20,0,176,
		6,0,106,2,49,0,20,1,176,37,0,100,20,1,
		36,50,0,100,110,7,36,53,0,176,20,0,92,58,
		121,20,2,176,21,0,176,28,0,106,2,61,0,93,
		135,0,12,2,20,1,36,54,0,176,20,0,92,59,
		92,104,20,2,176,21,0,109,38,0,20,1,36,55,
		0,176,20,0,122,121,20,2,176,21,0,106,2,32,
		0,109,39,0,72,20,1,26,215,253,36,57,0,176,
		18,0,92,24,106,1,0,20,2,36,58,0,176,18,
		0,92,20,106,7,83,67,82,69,69,78,0,20,2,
		36,59,0,176,36,0,20,0,36,60,0,176,40,0,
		121,121,176,2,0,12,0,176,3,0,12,0,109,4,
		0,20,5,36,61,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,114,101,108,95,98,97,110,99,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,8,192,255,253,255,255,231,
		62,0,4,3,0,4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

