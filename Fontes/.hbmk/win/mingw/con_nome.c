/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_nome.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_NOME );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( FUNC_NOM );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( SETCURSOR );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBUNLOCK );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_NOME )
{ "CON_NOME", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_NOME )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "SELECAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "FUNC_NOM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( FUNC_NOM )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "SETCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCURSOR )}, NULL },
{ "FLD_NAME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FLD_PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "ROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROW )}, NULL },
{ "COL", {HB_FS_PUBLIC}, {HB_FUNCNAME( COL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_NOME, "C:/sis/SAN/xhb/con_nome.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_NOME
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_NOME )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_NOME )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,106,18,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,32,0,20,1,
		36,15,0,109,2,0,106,2,78,0,5,28,21,36,
		16,0,176,3,0,106,3,48,49,0,20,1,36,17,
		0,100,110,7,36,19,0,176,4,0,121,121,176,5,
		0,12,0,176,6,0,12,0,12,4,83,7,0,36,
		20,0,176,8,0,106,31,67,79,78,83,85,76,84,
		65,32,68,69,32,78,79,77,69,83,32,83,69,76,
		69,67,73,79,78,65,68,79,83,0,20,1,36,21,
		0,176,9,0,106,2,49,0,20,1,36,22,0,176,
		10,0,9,100,106,8,67,79,78,84,65,84,79,0,
		100,100,9,20,6,176,11,0,106,8,67,79,78,84,
		65,84,79,0,20,1,176,11,0,106,9,78,85,77,
		95,67,76,73,69,0,20,1,36,23,0,176,12,0,
		90,11,109,13,0,106,2,88,0,5,6,106,14,83,
		69,76,69,67,65,79,32,61,32,34,88,34,0,20,
		2,36,24,0,176,14,0,12,0,28,85,36,25,0,
		176,3,0,106,3,48,49,0,20,1,36,26,0,176,
		15,0,106,33,78,239,191,189,111,32,104,239,191,189,
		32,110,111,109,101,115,32,115,101,108,101,99,105,111,
		110,97,100,111,115,32,33,32,0,20,1,36,27,0,
		176,16,0,20,0,36,28,0,176,3,0,106,3,48,
		49,0,20,1,36,29,0,100,110,7,36,31,0,176,
		17,0,20,0,36,32,0,176,3,0,106,3,49,49,
		0,20,1,36,33,0,176,9,0,106,2,49,0,20,
		1,36,34,0,176,17,0,20,0,36,35,0,176,18,
		0,92,6,121,92,19,92,79,92,2,20,5,36,36,
		0,92,3,3,1,0,92,3,3,1,0,92,3,3,
		1,0,92,3,3,1,0,176,23,0,108,19,108,20,
		108,21,108,22,20,4,81,22,0,81,21,0,81,20,
		0,81,19,0,36,37,0,176,24,0,109,19,0,120,
		109,22,0,20,3,36,38,0,106,11,78,65,83,67,
		73,77,69,78,84,79,0,98,19,0,92,3,2,36,
		39,0,92,8,98,22,0,92,3,2,36,40,0,106,
		5,64,83,53,48,0,98,20,0,122,2,36,41,0,
		106,4,64,83,49,0,98,20,0,92,2,2,36,42,
		0,106,9,32,32,47,32,32,47,32,32,0,98,20,
		0,92,3,2,36,43,0,106,5,78,79,77,69,0,
		98,21,0,122,2,36,44,0,106,8,83,69,76,69,
		67,65,79,0,98,21,0,92,2,2,36,45,0,106,
		11,78,65,83,67,73,77,69,78,84,79,0,98,21,
		0,92,3,2,36,46,0,176,25,0,92,7,122,92,
		19,92,78,109,19,0,106,9,70,85,78,67,95,78,
		79,77,0,109,20,0,109,21,0,106,2,196,0,106,
		2,179,0,106,2,205,0,20,11,36,47,0,176,26,
		0,20,0,36,48,0,176,27,0,121,121,176,5,0,
		12,0,176,6,0,12,0,109,7,0,20,5,36,49,
		0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( FUNC_NOM )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,29,0,1,71,30,0,2,36,54,0,
		176,23,0,108,31,108,32,20,2,36,55,0,176,33,
		0,12,0,83,32,0,36,56,0,122,83,31,0,36,
		57,0,109,29,0,92,4,8,29,193,0,36,59,0,
		109,32,0,92,13,8,29,163,0,36,60,0,109,30,
		0,122,15,29,170,0,36,61,0,176,34,0,12,0,
		29,159,0,36,62,0,176,35,0,122,20,1,36,63,
		0,98,19,0,109,30,0,1,83,36,0,36,64,0,
		98,20,0,109,30,0,1,83,37,0,36,65,0,176,
		38,0,176,39,0,12,0,176,40,0,12,0,20,2,
		176,41,0,109,42,0,176,43,0,100,109,36,0,109,
		37,0,100,100,12,5,20,2,48,44,0,176,45,0,
		109,42,0,12,1,112,0,73,36,66,0,176,46,0,
		109,42,0,100,100,100,100,100,100,20,7,4,0,0,
		83,42,0,109,42,0,73,36,67,0,176,35,0,121,
		20,1,36,68,0,176,47,0,20,0,36,70,0,25,
		20,36,71,0,109,32,0,92,27,8,28,9,36,72,
		0,121,83,31,0,36,75,0,109,31,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

