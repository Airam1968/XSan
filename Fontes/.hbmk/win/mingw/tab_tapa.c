/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/tab_tapa.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( TAB_TAPA );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( F_CAD_TAPA2 );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( __XSAVESCREEN );
HB_FUNC_EXTERN( SETCURSOR );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( PCENTRA );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( __XRESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TAB_TAPA )
{ "TAB_TAPA", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( TAB_TAPA )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "TIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "F_CAD_TAPA2", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_CAD_TAPA2 )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "__XSAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XSAVESCREEN )}, NULL },
{ "SETCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCURSOR )}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "PCENTRA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCENTRA )}, NULL },
{ "ELOTEAMENTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EBLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ENOME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROW )}, NULL },
{ "COL", {HB_FS_PUBLIC}, {HB_FUNCNAME( COL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "NUM_LOTEAM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "__XRESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XRESTSCREEN )}, NULL },
{ "FLD_NAME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FLD_PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TAB_TAPA, "C:/sis/SAN/xhb/tab_tapa.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TAB_TAPA
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TAB_TAPA )
   #include "hbiniseg.h"
#endif

HB_FUNC( TAB_TAPA )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,23,84,65,66,69,76,65,32,68,69,32,71,
		82,85,80,79,32,84,73,80,79,32,50,0,20,1,
		36,16,0,176,6,0,106,3,48,49,0,20,1,36,
		17,0,176,7,0,106,18,67,111,110,102,105,114,109,
		97,32,40,83,47,78,41,32,63,32,0,20,1,36,
		18,0,109,8,0,106,2,83,0,5,29,32,4,36,
		19,0,176,9,0,106,2,49,0,20,1,36,20,0,
		176,10,0,9,100,106,9,67,76,65,83,95,80,82,
		79,0,100,100,9,20,6,176,11,0,106,9,67,76,
		65,83,95,80,82,79,0,20,1,36,21,0,176,12,
		0,90,19,109,13,0,92,2,5,21,31,9,73,109,
		13,0,92,3,5,6,106,23,84,73,80,79,32,61,
		32,50,32,46,79,82,46,32,84,73,80,79,32,61,
		32,51,0,20,2,36,22,0,176,14,0,20,0,36,
		23,0,176,9,0,106,2,50,0,20,1,36,24,0,
		176,10,0,9,100,106,9,84,73,80,79,95,80,82,
		79,0,100,100,9,20,6,176,11,0,106,9,84,73,
		80,79,95,80,82,79,0,20,1,36,25,0,176,12,
		0,90,19,109,15,0,92,2,5,21,31,9,73,109,
		15,0,92,3,5,6,106,27,67,79,68,73,71,79,
		32,61,32,50,32,46,79,82,46,32,67,79,68,73,
		71,79,32,61,32,51,0,20,2,36,26,0,176,14,
		0,20,0,36,27,0,176,9,0,106,2,51,0,20,
		1,36,28,0,176,10,0,9,100,106,9,67,65,68,
		95,65,80,65,82,0,100,100,9,20,6,176,11,0,
		106,9,67,65,68,95,65,80,65,82,0,20,1,36,
		29,0,176,14,0,20,0,36,30,0,176,16,0,20,
		0,36,31,0,176,17,0,106,9,87,47,78,44,43,
		87,47,82,0,20,1,36,32,0,92,8,3,1,0,
		92,8,3,1,0,92,8,3,1,0,92,8,3,1,
		0,176,22,0,108,18,108,19,108,20,108,21,20,4,
		81,21,0,81,20,0,81,19,0,81,18,0,36,33,
		0,176,23,0,109,18,0,120,109,21,0,20,3,36,
		34,0,106,8,64,82,57,57,57,57,57,0,98,19,
		0,122,2,36,35,0,106,8,64,82,57,57,57,57,
		57,0,98,19,0,92,2,2,36,36,0,106,5,64,
		83,51,48,0,98,19,0,92,3,2,36,37,0,106,
		8,64,82,57,57,57,57,57,0,98,19,0,92,4,
		2,36,38,0,106,4,64,83,49,0,98,19,0,92,
		5,2,36,39,0,106,17,64,82,57,57,57,44,57,
		57,57,44,57,57,57,46,57,57,0,98,19,0,92,
		6,2,36,40,0,106,5,64,83,50,48,0,98,19,
		0,92,7,2,36,41,0,106,5,64,83,50,48,0,
		98,19,0,92,8,2,36,42,0,106,11,78,111,46,
		80,82,79,68,85,84,79,0,98,20,0,122,2,36,
		43,0,106,9,78,111,46,71,82,85,80,79,0,98,
		20,0,92,2,2,36,44,0,106,5,78,79,77,69,
		0,98,20,0,92,3,2,36,45,0,106,11,81,85,
		65,78,84,73,68,65,68,69,0,98,20,0,92,4,
		2,36,46,0,106,6,65,84,73,86,79,0,98,20,
		0,92,5,2,36,47,0,106,12,86,65,76,79,82,
		32,77,69,68,73,79,0,98,20,0,92,6,2,36,
		48,0,106,10,79,66,83,69,82,86,65,95,49,0,
		98,20,0,92,7,2,36,49,0,106,10,79,66,83,
		69,82,86,65,95,50,0,98,20,0,92,8,2,36,
		50,0,176,24,0,92,5,121,92,21,92,79,92,2,
		20,5,36,51,0,176,6,0,106,3,48,49,0,20,
		1,36,52,0,176,17,0,106,1,0,20,1,36,53,
		0,176,17,0,106,6,87,47,78,43,42,0,20,1,
		36,54,0,176,25,0,92,23,121,20,2,176,26,0,
		106,8,91,69,78,84,69,82,93,0,20,1,36,55,
		0,176,25,0,92,23,92,21,20,2,176,26,0,106,
		6,91,69,83,67,93,0,20,1,36,56,0,176,25,
		0,92,23,92,37,20,2,176,26,0,106,9,91,69,
		83,80,65,128,79,93,0,20,1,36,57,0,176,25,
		0,92,23,92,59,20,2,176,26,0,106,7,91,26,
		27,24,25,93,0,20,1,36,58,0,176,17,0,106,
		1,0,20,1,36,59,0,176,17,0,106,9,87,47,
		78,44,43,87,47,82,0,20,1,36,60,0,176,25,
		0,92,23,92,7,20,2,176,26,0,106,13,80,97,
		114,97,32,65,108,116,101,114,97,114,0,20,1,36,
		61,0,176,25,0,92,23,92,26,20,2,176,26,0,
		106,10,80,97,114,97,32,83,97,105,114,0,20,1,
		36,62,0,176,25,0,92,23,92,45,20,2,176,26,
		0,106,13,80,97,114,97,32,73,110,99,108,117,105,
		114,0,20,1,36,63,0,176,25,0,92,23,92,65,
		20,2,176,26,0,106,13,77,111,118,105,109,101,110,
		116,97,135,198,111,0,20,1,36,64,0,176,27,0,
		92,6,122,92,21,92,78,109,18,0,106,12,70,95,
		67,65,68,95,84,65,80,65,50,0,109,19,0,109,
		20,0,106,2,196,0,106,2,179,0,106,2,205,0,
		20,11,36,66,0,176,28,0,20,0,176,9,0,106,
		2,49,0,20,1,176,29,0,100,20,1,36,67,0,
		176,30,0,121,121,176,2,0,12,0,176,3,0,12,
		0,109,4,0,20,5,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_CAD_TAPA2 )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,32,0,1,71,33,0,2,36,72,0,
		176,22,0,108,34,108,35,20,2,36,73,0,176,36,
		0,12,0,83,35,0,36,74,0,122,83,34,0,36,
		75,0,109,32,0,92,4,8,29,18,5,36,77,0,
		109,35,0,92,32,8,29,47,4,36,78,0,176,37,
		0,20,0,36,79,0,176,38,0,122,20,1,36,80,
		0,176,6,0,106,3,48,49,0,20,1,36,81,0,
		176,7,0,106,27,67,111,110,102,105,114,109,97,32,
		73,110,99,108,117,115,97,111,32,40,83,47,78,41,
		32,63,32,0,20,1,36,82,0,109,8,0,106,2,
		83,0,5,29,202,3,36,84,0,176,39,0,92,10,
		92,10,92,17,92,70,20,4,176,40,0,92,10,92,
		10,20,2,36,85,0,176,24,0,92,10,92,10,92,
		17,92,70,92,2,20,5,36,86,0,176,41,0,106,
		14,68,97,100,111,115,32,80,114,111,100,117,116,111,
		0,92,11,20,2,36,87,0,121,83,42,0,36,88,
		0,121,83,43,0,36,89,0,176,44,0,92,30,12,
		1,83,45,0,36,90,0,121,83,46,0,36,91,0,
		176,25,0,92,13,92,12,20,2,176,26,0,106,16,
		80,114,111,100,117,116,111,32,32,32,32,32,32,58,
		32,0,20,1,176,40,0,176,47,0,12,0,176,48,
		0,12,0,122,72,20,2,176,49,0,109,50,0,176,
		51,0,100,106,12,101,76,79,84,69,65,77,69,78,
		84,79,0,106,6,57,57,57,57,57,0,100,100,12,
		5,20,2,48,52,0,176,53,0,109,50,0,12,1,
		112,0,73,36,92,0,176,54,0,109,50,0,100,100,
		100,100,100,100,20,7,4,0,0,83,50,0,109,50,
		0,73,36,93,0,109,42,0,121,5,32,213,2,36,
		96,0,176,9,0,106,2,49,0,20,1,36,97,0,
		176,55,0,109,42,0,100,100,20,3,36,98,0,176,
		56,0,12,0,31,58,36,99,0,176,6,0,106,3,
		48,49,0,20,1,36,100,0,176,57,0,106,25,80,
		114,111,100,117,116,111,32,110,97,111,32,67,97,100,
		97,115,116,114,97,100,111,32,33,0,92,3,20,2,
		36,101,0,26,179,254,36,103,0,176,25,0,92,13,
		92,33,20,2,176,26,0,106,4,32,45,32,0,176,
		58,0,109,59,0,12,1,72,20,1,36,105,0,176,
		9,0,106,2,51,0,20,1,36,107,0,176,25,0,
		92,14,92,12,20,2,176,26,0,106,16,71,114,117,
		112,111,32,32,32,32,32,32,32,32,58,32,0,20,
		1,176,40,0,176,47,0,12,0,176,48,0,12,0,
		122,72,20,2,176,49,0,109,50,0,176,51,0,100,
		106,7,101,66,76,79,67,79,0,106,6,57,57,57,
		57,57,0,100,100,12,5,20,2,48,52,0,176,53,
		0,109,50,0,12,1,112,0,73,36,108,0,176,54,
		0,109,50,0,100,100,100,100,100,100,20,7,4,0,
		0,83,50,0,109,50,0,73,36,109,0,176,55,0,
		176,60,0,109,42,0,92,5,12,2,176,60,0,109,
		43,0,92,5,12,2,72,100,100,20,3,36,110,0,
		176,56,0,12,0,28,55,36,111,0,176,6,0,106,
		3,48,49,0,20,1,36,112,0,176,57,0,106,22,
		71,114,117,112,111,32,106,97,32,67,97,100,97,115,
		116,114,97,100,111,32,33,0,92,3,20,2,36,113,
		0,26,39,255,36,118,0,176,25,0,92,15,92,12,
		20,2,176,26,0,106,16,78,111,109,101,32,32,32,
		32,32,32,32,32,32,58,32,0,20,1,176,40,0,
		176,47,0,12,0,176,48,0,12,0,122,72,20,2,
		176,49,0,109,50,0,176,51,0,100,106,6,101,78,
		79,77,69,0,106,5,64,83,51,48,0,100,100,12,
		5,20,2,48,52,0,176,53,0,109,50,0,12,1,
		112,0,73,36,119,0,176,25,0,92,16,92,12,20,
		2,176,26,0,106,16,81,117,97,110,116,105,100,97,
		100,101,32,32,32,58,32,0,20,1,176,40,0,176,
		47,0,12,0,176,48,0,12,0,122,72,20,2,176,
		49,0,109,50,0,176,51,0,100,106,12,101,81,85,
		65,78,84,73,68,65,68,69,0,106,6,57,57,57,
		57,57,0,100,100,12,5,20,2,48,52,0,176,53,
		0,109,50,0,12,1,112,0,73,36,120,0,176,54,
		0,109,50,0,100,100,100,100,100,100,20,7,4,0,
		0,83,50,0,109,50,0,73,36,121,0,176,6,0,
		106,3,48,49,0,20,1,36,122,0,176,7,0,106,
		18,67,111,110,102,105,114,109,97,32,40,83,47,78,
		41,32,63,32,0,20,1,36,123,0,109,8,0,106,
		2,83,0,5,28,87,36,124,0,176,9,0,106,2,
		51,0,20,1,36,125,0,176,61,0,20,0,36,126,
		0,176,62,0,12,0,28,57,36,129,0,176,60,0,
		109,42,0,92,5,12,2,78,63,0,176,60,0,109,
		43,0,92,5,12,2,78,64,0,109,46,0,78,65,
		0,36,130,0,109,45,0,78,59,0,36,131,0,176,
		66,0,20,0,36,133,0,36,139,0,176,38,0,121,
		20,1,36,140,0,176,67,0,20,0,26,218,0,36,
		141,0,109,35,0,92,13,8,29,188,0,36,142,0,
		109,33,0,122,15,29,195,0,109,33,0,92,5,69,
		29,186,0,36,143,0,176,62,0,12,0,29,175,0,
		36,144,0,176,37,0,20,0,36,145,0,176,38,0,
		122,20,1,36,146,0,98,18,0,109,33,0,1,83,
		68,0,36,147,0,98,19,0,109,33,0,1,83,69,
		0,36,148,0,176,40,0,176,47,0,12,0,176,48,
		0,12,0,20,2,176,49,0,109,50,0,176,51,0,
		100,109,68,0,109,69,0,100,100,12,5,20,2,48,
		52,0,176,53,0,109,50,0,12,1,112,0,73,36,
		149,0,176,54,0,109,50,0,100,100,100,100,100,100,
		20,7,4,0,0,83,50,0,109,50,0,73,36,150,
		0,176,38,0,121,20,1,36,151,0,176,66,0,20,
		0,36,152,0,176,67,0,20,0,36,154,0,25,20,
		36,155,0,109,35,0,92,27,8,28,9,36,156,0,
		121,83,34,0,36,159,0,109,34,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

