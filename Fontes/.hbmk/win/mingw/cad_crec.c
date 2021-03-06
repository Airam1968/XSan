/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/cad_crec.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CAD_CREC );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( BLOQ_R );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CAD_CREC )
{ "CAD_CREC", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CAD_CREC )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "R_D", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_LANC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECODIGO_CLI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EDESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EVALOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESITUACAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LANC_CPG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "NOME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "LANC_CRE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "BLOQ_R", {HB_FS_PUBLIC}, {HB_FUNCNAME( BLOQ_R )}, NULL },
{ "LANCAMENTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CODIGO_CLI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VALOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SITUACAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CAD_CREC, "C:/sis/SAN/xhb/cad_crec.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CAD_CREC
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CAD_CREC )
   #include "hbiniseg.h"
#endif

HB_FUNC( CAD_CREC )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,14,0,176,5,
		0,106,29,67,65,68,65,83,84,82,79,32,68,69,
		32,67,79,78,84,65,83,32,65,32,82,69,67,69,
		66,69,82,0,20,1,36,15,0,176,6,0,106,2,
		52,0,20,1,36,16,0,176,7,0,9,100,106,7,
		67,76,65,83,83,69,0,100,100,9,20,6,176,8,
		0,106,7,67,76,65,83,83,69,0,20,1,36,17,
		0,176,9,0,90,11,109,10,0,106,2,82,0,5,
		6,106,10,82,95,68,32,61,32,34,82,34,0,20,
		2,36,18,0,176,11,0,20,0,36,19,0,176,6,
		0,106,2,51,0,20,1,36,20,0,176,7,0,9,
		100,106,7,83,84,65,84,85,83,0,100,100,9,20,
		6,36,21,0,176,11,0,20,0,36,22,0,176,6,
		0,106,2,50,0,20,1,36,23,0,176,7,0,9,
		100,106,9,67,76,73,69,78,84,69,83,0,100,100,
		9,20,6,176,8,0,106,9,67,76,73,69,78,84,
		69,83,0,20,1,36,24,0,176,11,0,20,0,36,
		25,0,176,6,0,106,2,49,0,20,1,36,26,0,
		176,7,0,9,100,106,9,67,82,69,67,69,66,69,
		82,0,100,100,9,20,6,176,8,0,106,9,67,82,
		69,67,69,66,69,82,0,20,1,176,8,0,106,9,
		68,65,84,65,95,67,82,69,0,20,1,36,27,0,
		176,11,0,20,0,36,28,0,176,12,0,106,3,48,
		49,0,20,1,36,29,0,176,13,0,106,18,67,111,
		110,102,105,114,109,97,32,40,83,47,78,41,32,63,
		32,0,20,1,36,30,0,109,14,0,106,2,83,0,
		5,29,18,6,36,31,0,176,12,0,106,3,49,49,
		0,20,1,36,32,0,121,83,15,0,36,33,0,134,
		0,0,0,0,83,16,0,36,34,0,121,83,17,0,
		36,35,0,176,18,0,92,50,12,1,83,19,0,36,
		36,0,121,83,20,0,36,37,0,176,18,0,122,12,
		1,83,21,0,36,38,0,176,6,0,106,2,51,0,
		20,1,36,39,0,109,22,0,83,15,0,36,40,0,
		176,23,0,92,6,92,5,20,2,176,24,0,106,17,
		76,97,110,135,97,109,101,110,116,111,32,32,32,32,
		58,32,0,20,1,176,25,0,176,26,0,12,0,176,
		27,0,12,0,122,72,20,2,176,28,0,109,29,0,
		176,30,0,100,106,10,101,78,85,77,95,76,65,78,
		67,0,106,14,64,82,32,57,57,57,57,57,57,57,
		57,57,57,0,100,100,12,5,20,2,48,31,0,176,
		32,0,109,29,0,12,1,112,0,73,36,41,0,176,
		33,0,120,20,1,4,0,0,83,29,0,109,29,0,
		73,36,42,0,176,6,0,106,2,49,0,20,1,36,
		44,0,176,23,0,92,8,92,5,20,2,176,24,0,
		106,17,68,97,116,97,32,80,97,103,97,109,101,110,
		116,111,58,32,0,20,1,176,25,0,176,26,0,12,
		0,176,27,0,12,0,122,72,20,2,176,28,0,109,
		29,0,176,30,0,100,106,6,101,68,65,84,65,0,
		100,100,100,12,5,20,2,48,31,0,176,32,0,109,
		29,0,12,1,112,0,73,36,45,0,176,34,0,109,
		29,0,100,100,100,100,100,100,20,7,4,0,0,83,
		29,0,109,29,0,73,36,46,0,109,16,0,134,0,
		0,0,0,5,32,125,255,36,53,0,176,23,0,92,
		10,92,5,20,2,176,24,0,106,17,67,108,105,101,
		110,116,101,32,32,32,32,32,32,32,58,32,0,20,
		1,176,25,0,176,26,0,12,0,176,27,0,12,0,
		122,72,20,2,176,28,0,109,29,0,176,30,0,100,
		106,12,101,67,79,68,73,71,79,95,67,76,73,0,
		106,11,64,82,32,57,57,57,57,57,57,57,0,100,
		100,12,5,20,2,48,31,0,176,32,0,109,29,0,
		12,1,112,0,73,36,54,0,176,34,0,109,29,0,
		100,100,100,100,100,100,20,7,4,0,0,83,29,0,
		109,29,0,73,36,55,0,176,6,0,106,2,50,0,
		20,1,36,56,0,176,35,0,109,17,0,100,100,20,
		3,36,57,0,176,36,0,12,0,31,64,36,58,0,
		176,12,0,106,3,48,49,0,20,1,36,59,0,176,
		37,0,106,19,67,162,100,105,103,111,32,73,110,101,
		120,105,115,116,101,110,116,101,0,92,3,20,2,36,
		60,0,176,6,0,106,2,49,0,20,1,36,61,0,
		26,25,255,36,63,0,176,23,0,92,10,92,29,20,
		2,176,24,0,106,4,32,45,32,0,176,38,0,109,
		39,0,12,1,72,20,1,36,64,0,176,6,0,106,
		2,49,0,20,1,36,68,0,176,23,0,92,12,92,
		5,20,2,176,24,0,106,17,68,101,115,99,114,105,
		135,198,111,32,32,32,32,32,58,32,0,20,1,176,
		25,0,176,26,0,12,0,176,27,0,12,0,122,72,
		20,2,176,28,0,109,29,0,176,30,0,100,106,11,
		101,68,69,83,67,82,73,67,65,79,0,100,100,100,
		12,5,20,2,48,31,0,176,32,0,109,29,0,12,
		1,112,0,73,36,70,0,176,23,0,92,14,92,5,
		20,2,176,24,0,106,17,86,97,108,111,114,32,97,
		32,80,97,103,97,114,32,58,32,0,20,1,176,25,
		0,176,26,0,12,0,176,27,0,12,0,122,72,20,
		2,176,28,0,109,29,0,176,30,0,100,106,7,101,
		86,65,76,79,82,0,106,22,64,82,32,57,57,57,
		44,57,57,57,44,57,57,57,44,57,57,57,46,57,
		57,0,100,100,12,5,20,2,48,31,0,176,32,0,
		109,29,0,12,1,112,0,73,36,71,0,176,34,0,
		109,29,0,100,100,100,100,100,100,20,7,4,0,0,
		83,29,0,109,29,0,73,36,72,0,109,20,0,121,
		5,32,105,255,36,78,0,176,23,0,92,16,92,5,
		20,2,176,24,0,106,17,80,114,101,118,105,115,198,
		111,92,82,101,97,108,32,58,32,0,20,1,176,25,
		0,176,26,0,12,0,176,27,0,12,0,122,72,20,
		2,176,28,0,109,29,0,176,30,0,100,106,10,101,
		83,73,84,85,65,67,65,79,0,106,2,88,0,90,
		14,109,21,0,106,5,80,82,112,114,0,24,6,100,
		12,5,20,2,48,31,0,176,32,0,109,29,0,12,
		1,112,0,73,36,79,0,176,34,0,109,29,0,100,
		100,100,100,100,100,20,7,4,0,0,83,29,0,109,
		29,0,73,36,80,0,176,6,0,106,2,52,0,20,
		1,36,81,0,176,23,0,92,18,92,5,20,2,176,
		24,0,106,17,84,105,112,111,32,82,101,99,101,105,
		116,97,32,32,58,32,0,20,1,36,82,0,92,6,
		3,1,0,92,6,3,1,0,92,6,3,1,0,92,
		6,3,1,0,176,44,0,108,40,108,41,108,42,108,
		43,20,4,81,43,0,81,42,0,81,41,0,81,40,
		0,36,83,0,176,45,0,109,40,0,120,109,43,0,
		20,3,36,84,0,106,12,64,82,32,57,57,57,57,
		57,57,57,57,0,98,41,0,122,2,36,85,0,106,
		5,64,83,51,48,0,98,41,0,92,2,2,36,86,
		0,106,9,32,32,32,32,32,32,32,32,0,98,42,
		0,122,2,36,87,0,106,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,0,98,42,
		0,92,2,2,36,88,0,176,46,0,92,18,92,20,
		92,18,92,63,109,40,0,106,7,70,95,67,79,78,
		83,0,109,41,0,109,42,0,106,2,196,0,106,2,
		16,0,106,2,205,0,20,11,36,89,0,109,47,0,
		83,48,0,36,90,0,176,6,0,106,2,49,0,20,
		1,36,91,0,176,12,0,106,3,48,49,0,20,1,
		36,92,0,176,13,0,106,18,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,32,0,20,1,
		36,93,0,109,14,0,106,2,83,0,5,29,186,0,
		36,94,0,176,12,0,106,3,48,49,0,20,1,36,
		95,0,176,49,0,106,22,71,114,97,118,97,110,100,
		111,44,32,65,103,117,97,114,100,101,32,46,46,46,
		0,20,1,36,96,0,176,6,0,106,2,51,0,20,
		1,36,97,0,176,11,0,20,0,36,98,0,176,50,
		0,12,0,28,21,36,99,0,109,15,0,122,72,78,
		51,0,36,100,0,176,52,0,20,0,36,102,0,176,
		6,0,106,2,49,0,20,1,36,103,0,176,53,0,
		20,0,36,104,0,176,54,0,12,0,28,61,36,107,
		0,109,15,0,78,55,0,109,16,0,78,56,0,109,
		17,0,78,57,0,36,110,0,109,19,0,78,58,0,
		109,20,0,78,59,0,109,21,0,78,60,0,36,111,
		0,109,48,0,78,61,0,36,112,0,176,52,0,20,
		0,36,116,0,176,62,0,20,0,176,6,0,106,2,
		49,0,20,1,176,63,0,100,20,1,36,117,0,176,
		64,0,121,121,176,2,0,12,0,176,3,0,12,0,
		109,4,0,20,5,36,118,0,7
	};

	hb_vmExecute( pcode, symbols );
}

