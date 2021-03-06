/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/sel_forn.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( SEL_FORN );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( DBCREATEINDEX );
HB_FUNC_EXTERN( F_BLOQA );
HB_FUNC_EXTERN( DBEVAL );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( FUNC_SELFORN );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( F_BLOQR );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_SEL_FORN )
{ "SEL_FORN", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( SEL_FORN )}, NULL },
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
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "DBCREATEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCREATEINDEX )}, NULL },
{ "NOME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "F_BLOQA", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQA )}, NULL },
{ "DBEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEVAL )}, NULL },
{ "SELECAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "FUNC_SELFORN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( FUNC_SELFORN )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_SEL_FORN, "C:/sis/SAN/xhb/sel_forn.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_SEL_FORN
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_SEL_FORN )
   #include "hbiniseg.h"
#endif

HB_FUNC( SEL_FORN )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,106,18,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,32,0,20,1,
		36,15,0,109,2,0,106,2,78,0,5,28,21,36,
		16,0,176,3,0,106,3,48,49,0,20,1,36,17,
		0,100,110,7,36,19,0,176,4,0,121,121,176,5,
		0,12,0,176,6,0,12,0,12,4,83,7,0,36,
		20,0,176,8,0,106,25,83,69,76,69,67,65,79,
		32,80,65,82,65,32,77,65,76,65,32,68,73,82,
		69,84,65,0,20,1,36,21,0,176,3,0,106,3,
		48,49,0,20,1,36,22,0,176,1,0,106,46,68,
		101,115,101,106,97,32,111,32,83,101,108,101,135,198,
		111,32,101,109,32,79,114,100,101,109,32,65,108,102,
		97,98,130,116,105,99,97,32,40,83,47,78,41,32,
		63,32,0,20,1,36,23,0,109,2,0,106,2,78,
		0,5,28,64,36,24,0,176,9,0,106,2,49,0,
		20,1,36,25,0,176,10,0,9,100,106,9,70,79,
		82,78,69,67,69,68,0,100,100,9,20,6,176,11,
		0,106,9,70,79,82,78,69,67,69,68,0,20,1,
		36,26,0,176,12,0,20,0,25,79,36,28,0,176,
		9,0,106,2,49,0,20,1,36,29,0,176,10,0,
		9,100,106,9,70,79,82,78,69,67,69,68,0,100,
		100,9,20,6,36,30,0,176,13,0,106,9,84,69,
		77,80,95,70,79,82,0,106,5,78,79,77,69,0,
		90,6,109,14,0,6,100,20,4,36,31,0,176,12,
		0,20,0,36,33,0,176,3,0,106,3,48,49,0,
		20,1,36,34,0,176,1,0,106,42,68,101,115,101,
		106,97,32,101,108,105,109,105,110,97,114,32,115,101,
		108,101,135,198,111,32,97,110,116,101,114,105,111,114,
		32,40,83,47,78,41,32,63,32,0,20,1,36,35,
		0,109,2,0,106,2,83,0,5,28,44,36,36,0,
		176,15,0,12,0,28,34,36,37,0,176,16,0,90,
		11,106,2,32,0,165,78,17,0,6,100,100,100,100,
		9,20,6,36,38,0,176,18,0,20,0,36,41,0,
		176,3,0,106,3,49,49,0,20,1,36,42,0,176,
		9,0,106,2,49,0,20,1,36,43,0,176,12,0,
		20,0,36,44,0,176,19,0,106,44,69,78,84,69,
		82,32,80,65,82,65,32,77,65,82,67,65,82,32,
		91,88,93,32,79,85,32,80,65,82,65,32,68,69,
		83,77,65,82,67,65,82,32,91,32,93,0,20,1,
		36,45,0,176,20,0,92,6,92,4,92,21,92,71,
		92,2,20,5,36,46,0,92,2,3,1,0,92,2,
		3,1,0,92,2,3,1,0,92,2,3,1,0,176,
		25,0,108,21,108,22,108,23,108,24,20,4,81,24,
		0,81,23,0,81,22,0,81,21,0,36,47,0,106,
		5,78,79,77,69,0,98,21,0,122,2,36,48,0,
		106,8,83,69,76,69,67,65,79,0,98,21,0,92,
		2,2,36,49,0,92,50,98,24,0,122,2,36,50,
		0,122,98,24,0,92,2,2,36,51,0,106,5,64,
		83,53,48,0,98,22,0,122,2,36,52,0,106,4,
		64,83,49,0,98,22,0,92,2,2,36,53,0,106,
		5,78,79,77,69,0,98,23,0,122,2,36,54,0,
		106,8,83,69,76,69,67,65,79,0,98,23,0,92,
		2,2,36,55,0,176,26,0,92,7,92,5,92,21,
		92,70,109,21,0,106,13,70,85,78,67,95,83,69,
		76,70,79,82,78,0,109,22,0,109,23,0,106,2,
		196,0,106,2,179,0,106,2,205,0,20,11,36,56,
		0,176,27,0,20,0,36,57,0,176,28,0,121,121,
		176,5,0,12,0,176,6,0,12,0,109,7,0,20,
		5,36,58,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( FUNC_SELFORN )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,30,0,1,71,31,0,2,36,63,0,
		176,25,0,108,32,108,33,20,2,36,64,0,176,34,
		0,12,0,83,33,0,36,65,0,122,83,32,0,36,
		66,0,109,30,0,92,4,8,28,89,36,68,0,109,
		33,0,92,13,8,28,60,36,69,0,176,35,0,12,
		0,28,68,36,70,0,109,17,0,106,2,88,0,5,
		28,14,36,71,0,106,2,32,0,78,17,0,25,12,
		36,73,0,106,2,88,0,78,17,0,36,75,0,176,
		18,0,20,0,36,76,0,25,20,36,77,0,109,33,
		0,92,27,8,28,9,36,78,0,121,83,32,0,36,
		81,0,109,32,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

