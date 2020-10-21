/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/alt_crec.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( ALT_CREC );
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
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_ALT_CREC )
{ "ALT_CREC", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( ALT_CREC )}, NULL },
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
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "DATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CODIGO_CLI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECODIGO_CLI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "NOME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VALOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SITUACAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_ALT_CREC, "C:/sis/SAN/xhb/alt_crec.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_ALT_CREC
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_ALT_CREC )
   #include "hbiniseg.h"
#endif

HB_FUNC( ALT_CREC )
{
	static const HB_BYTE pcode[] =
	{
		36,2,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,3,0,176,5,
		0,106,30,65,76,84,69,82,65,128,199,79,32,68,
		69,32,67,79,78,84,65,83,32,65,32,82,69,67,
		69,66,69,82,0,20,1,36,4,0,176,6,0,106,
		2,52,0,20,1,36,5,0,176,7,0,9,100,106,
		7,67,76,65,83,83,69,0,100,100,9,20,6,176,
		8,0,106,7,67,76,65,83,83,69,0,20,1,36,
		6,0,176,9,0,90,11,109,10,0,106,2,82,0,
		5,6,106,10,82,95,68,32,61,32,34,82,34,0,
		20,2,36,7,0,176,11,0,20,0,36,8,0,176,
		6,0,106,2,51,0,20,1,36,9,0,176,7,0,
		9,100,106,7,83,84,65,84,85,83,0,100,100,9,
		20,6,36,10,0,176,11,0,20,0,36,11,0,176,
		6,0,106,2,50,0,20,1,36,12,0,176,7,0,
		9,100,106,9,67,76,73,69,78,84,69,83,0,100,
		100,9,20,6,176,8,0,106,9,67,76,73,69,78,
		84,69,83,0,20,1,36,13,0,176,11,0,20,0,
		36,14,0,176,6,0,106,2,49,0,20,1,36,15,
		0,176,7,0,9,100,106,9,67,82,69,67,69,66,
		69,82,0,100,100,9,20,6,176,8,0,106,9,67,
		82,69,67,69,66,69,82,0,20,1,176,8,0,106,
		9,68,65,84,65,95,67,82,69,0,20,1,36,16,
		0,176,11,0,20,0,36,17,0,176,12,0,106,3,
		48,49,0,20,1,36,18,0,176,13,0,106,18,67,
		111,110,102,105,114,109,97,32,40,83,47,78,41,32,
		63,32,0,20,1,36,19,0,109,14,0,106,2,83,
		0,5,29,214,6,36,21,0,176,12,0,106,3,49,
		49,0,20,1,36,23,0,121,83,15,0,36,24,0,
		176,16,0,92,6,92,5,20,2,176,17,0,106,17,
		76,97,110,135,97,109,101,110,116,111,32,32,32,32,
		58,32,0,20,1,176,18,0,176,19,0,12,0,176,
		20,0,12,0,122,72,20,2,176,21,0,109,22,0,
		176,23,0,100,106,10,101,78,85,77,95,76,65,78,
		67,0,106,14,64,82,32,57,57,57,57,57,57,57,
		57,57,57,0,100,100,12,5,20,2,48,24,0,176,
		25,0,109,22,0,12,1,112,0,73,36,25,0,176,
		26,0,109,22,0,100,100,100,100,100,100,20,7,4,
		0,0,83,22,0,109,22,0,73,36,26,0,109,15,
		0,121,5,28,52,36,27,0,176,27,0,20,0,176,
		6,0,106,2,49,0,20,1,176,28,0,100,20,1,
		36,28,0,176,29,0,121,121,176,2,0,12,0,176,
		3,0,12,0,109,4,0,20,5,36,29,0,7,36,
		31,0,176,6,0,106,2,49,0,20,1,36,32,0,
		176,30,0,109,15,0,100,100,20,3,36,33,0,176,
		31,0,12,0,31,55,36,34,0,176,12,0,106,3,
		48,49,0,20,1,36,35,0,176,32,0,106,22,76,
		97,110,135,97,109,101,110,116,111,32,73,110,118,160,
		108,105,100,111,32,33,0,92,3,20,2,36,36,0,
		26,222,254,36,43,0,176,33,0,12,0,29,129,0,
		36,44,0,176,16,0,92,8,92,5,20,2,176,17,
		0,106,17,68,97,116,97,32,80,97,103,97,109,101,
		110,116,111,58,32,0,20,1,176,18,0,176,19,0,
		12,0,176,20,0,12,0,122,72,20,2,176,21,0,
		109,22,0,176,23,0,100,106,5,68,65,84,65,0,
		100,100,100,12,5,20,2,48,24,0,176,25,0,109,
		22,0,12,1,112,0,73,36,45,0,176,26,0,109,
		22,0,100,100,100,100,100,100,20,7,4,0,0,83,
		22,0,109,22,0,73,36,46,0,176,34,0,20,0,
		36,48,0,109,35,0,134,0,0,0,0,5,32,107,
		255,36,55,0,176,33,0,12,0,29,147,0,36,56,
		0,176,16,0,92,10,92,5,20,2,176,17,0,106,
		17,67,108,105,101,110,116,101,32,32,32,32,32,32,
		32,58,32,0,20,1,176,18,0,176,19,0,12,0,
		176,20,0,12,0,122,72,20,2,176,21,0,109,22,
		0,176,23,0,100,106,11,67,79,68,73,71,79,95,
		67,76,73,0,106,11,64,82,32,57,57,57,57,57,
		57,57,0,100,100,12,5,20,2,48,24,0,176,25,
		0,109,22,0,12,1,112,0,73,36,57,0,176,26,
		0,109,22,0,100,100,100,100,100,100,20,7,4,0,
		0,83,22,0,109,22,0,73,36,58,0,176,34,0,
		20,0,36,60,0,109,36,0,83,37,0,36,61,0,
		176,6,0,106,2,50,0,20,1,36,62,0,176,30,
		0,109,37,0,100,100,20,3,36,63,0,176,31,0,
		12,0,31,64,36,64,0,176,12,0,106,3,48,49,
		0,20,1,36,65,0,176,32,0,106,19,67,162,100,
		105,103,111,32,73,110,101,120,105,115,116,101,110,116,
		101,0,92,3,20,2,36,66,0,176,6,0,106,2,
		49,0,20,1,36,67,0,26,254,254,36,69,0,176,
		16,0,92,10,92,29,20,2,176,17,0,106,4,32,
		45,32,0,176,38,0,109,39,0,12,1,72,20,1,
		36,70,0,176,6,0,106,2,49,0,20,1,36,74,
		0,176,33,0,12,0,29,134,0,36,75,0,176,16,
		0,92,12,92,5,20,2,176,17,0,106,17,68,101,
		115,99,114,105,135,198,111,32,32,32,32,32,58,32,
		0,20,1,176,18,0,176,19,0,12,0,176,20,0,
		12,0,122,72,20,2,176,21,0,109,22,0,176,23,
		0,100,106,10,68,69,83,67,82,73,67,65,79,0,
		100,100,100,12,5,20,2,48,24,0,176,25,0,109,
		22,0,12,1,112,0,73,36,76,0,176,26,0,109,
		22,0,100,100,100,100,100,100,20,7,4,0,0,83,
		22,0,109,22,0,73,36,77,0,176,34,0,20,0,
		36,80,0,176,33,0,12,0,29,153,0,36,81,0,
		176,16,0,92,14,92,5,20,2,176,17,0,106,17,
		86,97,108,111,114,32,97,32,80,97,103,97,114,32,
		58,32,0,20,1,176,18,0,176,19,0,12,0,176,
		20,0,12,0,122,72,20,2,176,21,0,109,22,0,
		176,23,0,100,106,6,86,65,76,79,82,0,106,22,
		64,82,32,57,57,57,44,57,57,57,44,57,57,57,
		44,57,57,57,46,57,57,0,100,100,12,5,20,2,
		48,24,0,176,25,0,109,22,0,12,1,112,0,73,
		36,82,0,176,26,0,109,22,0,100,100,100,100,100,
		100,20,7,4,0,0,83,22,0,109,22,0,73,36,
		83,0,176,34,0,20,0,36,85,0,109,40,0,121,
		5,32,87,255,36,91,0,176,33,0,12,0,29,149,
		0,36,92,0,176,16,0,92,16,92,5,20,2,176,
		17,0,106,17,80,114,101,118,105,115,198,111,92,82,
		101,97,108,32,58,32,0,20,1,176,18,0,176,19,
		0,12,0,176,20,0,12,0,122,72,20,2,176,21,
		0,109,22,0,176,23,0,100,106,9,83,73,84,85,
		65,67,65,79,0,106,2,88,0,90,14,109,41,0,
		106,5,80,82,112,114,0,24,6,100,12,5,20,2,
		48,24,0,176,25,0,109,22,0,12,1,112,0,73,
		36,93,0,176,26,0,109,22,0,100,100,100,100,100,
		100,20,7,4,0,0,83,22,0,109,22,0,73,36,
		94,0,176,34,0,20,0,36,96,0,176,16,0,92,
		18,92,5,20,2,176,17,0,106,17,84,105,112,111,
		32,82,101,99,101,105,116,97,32,32,58,32,0,20,
		1,176,18,0,176,19,0,12,0,176,20,0,12,0,
		122,72,20,2,176,21,0,109,22,0,176,23,0,100,
		106,5,84,73,80,79,0,100,100,100,12,5,20,2,
		48,24,0,176,25,0,109,22,0,12,1,112,0,73,
		36,97,0,176,42,0,120,20,1,4,0,0,83,22,
		0,109,22,0,73,36,98,0,176,12,0,106,3,48,
		49,0,20,1,36,99,0,176,13,0,106,42,68,101,
		115,101,106,97,32,65,108,116,101,114,97,114,32,111,
		32,84,105,112,111,32,100,101,32,82,101,99,101,105,
		116,97,32,40,83,47,78,41,32,63,32,0,20,1,
		36,100,0,109,14,0,106,2,83,0,5,29,16,1,
		36,101,0,176,6,0,106,2,52,0,20,1,36,102,
		0,92,6,3,1,0,92,6,3,1,0,92,6,3,
		1,0,92,6,3,1,0,176,47,0,108,43,108,44,
		108,45,108,46,20,4,81,46,0,81,45,0,81,44,
		0,81,43,0,36,103,0,176,48,0,109,43,0,120,
		109,46,0,20,3,36,104,0,106,12,64,82,32,57,
		57,57,57,57,57,57,57,0,98,44,0,122,2,36,
		105,0,106,5,64,83,51,48,0,98,44,0,92,2,
		2,36,106,0,106,9,32,32,32,32,32,32,32,32,
		0,98,45,0,122,2,36,107,0,106,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		0,98,45,0,92,2,2,36,108,0,176,49,0,92,
		18,92,20,92,18,92,63,109,43,0,106,7,70,95,
		67,79,78,83,0,109,44,0,109,45,0,106,2,196,
		0,106,2,16,0,106,2,205,0,20,11,36,109,0,
		109,50,0,83,51,0,36,110,0,176,6,0,106,2,
		49,0,20,1,36,111,0,176,33,0,12,0,28,19,
		36,112,0,109,51,0,78,52,0,36,113,0,176,34,
		0,20,0,36,116,0,176,12,0,106,3,48,49,0,
		20,1,36,117,0,176,13,0,106,18,67,111,110,116,
		105,110,117,97,32,40,83,47,78,41,32,63,32,0,
		20,1,36,118,0,109,14,0,106,2,83,0,5,32,
		48,249,36,125,0,176,27,0,20,0,176,6,0,106,
		2,49,0,20,1,176,28,0,100,20,1,36,126,0,
		176,29,0,121,121,176,2,0,12,0,176,3,0,12,
		0,109,4,0,20,5,36,127,0,7
	};

	hb_vmExecute( pcode, symbols );
}

