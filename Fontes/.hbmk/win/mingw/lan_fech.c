/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/lan_fech.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( LAN_FECH );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( F_CON_FECH );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( DBCREATEINDEX );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( F_LACESSO );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_LAN_FECH )
{ "LAN_FECH", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( LAN_FECH )}, NULL },
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
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "SITUACAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "F_CON_FECH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_CON_FECH )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "MENOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMENOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUMERO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECOTACAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TOTAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOTAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TELA04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_LANC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLASSE_D", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLASSE_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECODIGO_FOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EDESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_PREV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPREVISTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EREALIZADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECOD_CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_CHEQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "ENUM_DOCUME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "LANC_CPG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ORDE_FORN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EORDE_FORN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ORDE_CLAD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EORDE_CLAD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "DBCREATEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCREATEINDEX )}, NULL },
{ "NOME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_ANTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
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
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "LANCAMENTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE_D", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMPREENDIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CODIGO_FOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_PREV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PREVISTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REALIZADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_CHEQUE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_DOCUME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CONTABIL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_LAN_FECH, "C:/sis/SAN/xhb/lan_fech.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_LAN_FECH
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_LAN_FECH )
   #include "hbiniseg.h"
#endif

HB_FUNC( LAN_FECH )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,14,0,176,5,
		0,106,32,67,79,78,83,85,76,84,65,32,65,32,
		83,73,84,85,65,67,65,79,32,68,69,32,67,79,
		84,65,67,79,69,83,0,20,1,36,15,0,176,6,
		0,106,3,48,49,0,20,1,36,16,0,176,7,0,
		106,18,67,111,110,102,105,114,109,97,32,40,83,47,
		78,41,32,63,32,0,20,1,36,17,0,109,8,0,
		106,2,83,0,5,29,95,4,36,18,0,176,9,0,
		106,2,50,0,20,1,36,19,0,176,10,0,9,100,
		106,8,67,79,84,65,67,65,79,0,100,100,9,20,
		6,176,11,0,106,8,67,79,84,65,67,65,79,0,
		20,1,36,20,0,176,12,0,20,0,36,21,0,176,
		13,0,90,26,109,14,0,106,17,80,69,68,73,68,
		79,32,68,69,32,67,79,77,80,82,65,0,5,6,
		106,30,83,73,84,85,65,67,65,79,32,61,32,34,
		80,69,68,73,68,79,32,68,69,32,67,79,77,80,
		82,65,34,0,20,2,36,22,0,176,12,0,20,0,
		36,23,0,176,15,0,20,0,36,24,0,176,16,0,
		106,9,87,47,78,44,43,87,47,82,0,20,1,36,
		25,0,92,10,3,1,0,92,10,3,1,0,92,10,
		3,1,0,92,10,3,1,0,176,21,0,108,17,108,
		18,108,19,108,20,20,4,81,20,0,81,19,0,81,
		18,0,81,17,0,36,26,0,106,7,78,85,77,69,
		82,79,0,98,17,0,122,2,36,27,0,106,11,70,
		79,82,78,69,67,69,68,95,49,0,98,17,0,92,
		2,2,36,28,0,106,11,70,79,82,78,69,67,69,
		68,95,50,0,98,17,0,92,3,2,36,29,0,106,
		11,70,79,82,78,69,67,69,68,95,51,0,98,17,
		0,92,4,2,36,30,0,106,11,70,79,82,78,69,
		67,69,68,95,52,0,98,17,0,92,5,2,36,31,
		0,106,11,70,79,82,78,69,67,69,68,95,53,0,
		98,17,0,92,6,2,36,32,0,106,9,83,73,84,
		85,65,67,65,79,0,98,17,0,92,7,2,36,33,
		0,106,6,77,69,78,79,82,0,98,17,0,92,8,
		2,36,34,0,106,6,84,79,84,65,76,0,98,17,
		0,92,9,2,36,35,0,106,11,76,65,78,67,65,
		77,69,78,84,79,0,98,17,0,92,10,2,36,36,
		0,92,10,98,20,0,122,2,36,37,0,92,10,98,
		20,0,92,2,2,36,38,0,92,10,98,20,0,92,
		3,2,36,39,0,92,10,98,20,0,92,4,2,36,
		40,0,92,10,98,20,0,92,5,2,36,41,0,92,
		10,98,20,0,92,6,2,36,42,0,92,10,98,20,
		0,92,7,2,36,43,0,92,10,98,20,0,92,8,
		2,36,44,0,92,19,98,20,0,92,9,2,36,45,
		0,92,10,98,20,0,92,10,2,36,46,0,106,11,
		64,82,32,57,57,57,57,57,57,57,0,98,18,0,
		122,2,36,47,0,106,11,64,82,32,57,57,57,57,
		57,57,57,0,98,18,0,92,2,2,36,48,0,106,
		11,64,82,32,57,57,57,57,57,57,57,0,98,18,
		0,92,3,2,36,49,0,106,11,64,82,32,57,57,
		57,57,57,57,57,0,98,18,0,92,4,2,36,50,
		0,106,11,64,82,32,57,57,57,57,57,57,57,0,
		98,18,0,92,5,2,36,51,0,106,11,64,82,32,
		57,57,57,57,57,57,57,0,98,18,0,92,6,2,
		36,52,0,106,5,64,83,50,48,0,98,18,0,92,
		7,2,36,53,0,106,12,64,82,32,57,57,57,57,
		57,57,57,57,0,98,18,0,92,8,2,36,54,0,
		106,22,64,82,32,57,57,57,44,57,57,57,44,57,
		57,57,44,57,57,57,46,57,57,0,98,18,0,92,
		9,2,36,55,0,106,12,64,82,32,57,57,57,57,
		57,57,57,57,0,98,18,0,92,10,2,36,56,0,
		106,8,67,79,84,65,128,183,79,0,98,19,0,122,
		2,36,57,0,106,7,70,79,82,78,32,49,0,98,
		19,0,92,2,2,36,58,0,106,7,70,79,82,78,
		32,50,0,98,19,0,92,3,2,36,59,0,106,7,
		70,79,82,78,32,51,0,98,19,0,92,4,2,36,
		60,0,106,7,70,79,82,78,32,52,0,98,19,0,
		92,5,2,36,61,0,106,7,70,79,82,78,32,53,
		0,98,19,0,92,6,2,36,62,0,106,9,83,73,
		84,85,65,128,199,79,0,98,19,0,92,7,2,36,
		63,0,106,6,77,69,78,79,82,0,98,19,0,92,
		8,2,36,64,0,106,6,84,79,84,65,76,0,98,
		19,0,92,9,2,36,65,0,106,9,76,65,78,67,
		46,32,67,80,0,98,19,0,92,10,2,36,66,0,
		176,22,0,92,5,121,92,21,92,79,92,2,20,5,
		36,67,0,176,6,0,106,3,48,49,0,20,1,36,
		68,0,176,16,0,106,1,0,20,1,36,69,0,176,
		16,0,106,6,87,47,78,43,42,0,20,1,36,70,
		0,176,23,0,92,23,121,20,2,176,24,0,106,6,
		91,69,83,67,93,0,20,1,36,71,0,176,16,0,
		106,1,0,20,1,36,72,0,176,16,0,106,9,87,
		47,78,44,43,87,47,82,0,20,1,36,73,0,176,
		23,0,92,23,92,7,20,2,176,24,0,106,10,80,
		97,114,97,32,83,97,105,114,0,20,1,36,74,0,
		176,25,0,92,6,122,92,21,92,78,109,17,0,106,
		11,70,95,67,79,78,95,70,69,67,72,0,109,18,
		0,109,19,0,106,2,196,0,106,2,179,0,106,2,
		205,0,20,11,36,76,0,176,26,0,20,0,176,9,
		0,106,2,49,0,20,1,176,27,0,100,20,1,36,
		77,0,176,28,0,121,121,176,2,0,12,0,176,3,
		0,12,0,109,4,0,20,5,36,78,0,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_CON_FECH )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,30,0,1,71,31,0,2,36,83,0,
		176,21,0,108,32,108,33,20,2,36,84,0,176,34,
		0,12,0,83,33,0,36,85,0,122,83,32,0,36,
		86,0,109,30,0,92,4,8,29,52,18,36,88,0,
		109,33,0,92,13,8,29,22,18,36,89,0,176,6,
		0,106,3,48,49,0,20,1,36,90,0,176,7,0,
		106,47,68,101,115,101,106,97,32,69,102,101,116,117,
		97,114,32,111,32,70,101,99,104,97,109,101,110,116,
		111,32,100,97,32,67,111,109,112,114,97,32,40,83,
		47,78,41,32,63,32,0,20,1,36,91,0,109,8,
		0,106,2,83,0,5,29,212,17,36,92,0,109,35,
		0,83,36,0,36,93,0,109,37,0,83,38,0,36,
		94,0,109,39,0,83,40,0,36,95,0,176,6,0,
		106,3,48,49,0,20,1,36,96,0,176,1,0,121,
		121,176,2,0,12,0,176,3,0,12,0,12,4,83,
		41,0,36,97,0,176,15,0,20,0,36,98,0,176,
		5,0,106,27,67,65,68,65,83,84,82,79,32,68,
		69,32,67,79,78,84,65,83,32,65,32,80,65,71,
		65,82,0,20,1,36,99,0,176,9,0,106,2,53,
		0,20,1,36,100,0,176,10,0,9,100,106,9,67,
		76,65,83,95,80,82,79,0,100,100,9,20,6,176,
		11,0,106,9,67,76,65,83,95,80,82,79,0,20,
		1,36,101,0,176,12,0,20,0,36,102,0,176,9,
		0,106,2,52,0,20,1,36,103,0,176,10,0,9,
		100,106,9,67,76,65,83,83,69,95,68,0,100,100,
		9,20,6,176,11,0,106,9,67,76,65,83,83,69,
		95,68,0,20,1,36,104,0,176,12,0,20,0,36,
		105,0,176,9,0,106,2,51,0,20,1,36,106,0,
		176,10,0,9,100,106,7,83,84,65,84,85,83,0,
		100,100,9,20,6,36,107,0,176,12,0,20,0,36,
		108,0,176,9,0,106,2,56,0,20,1,36,109,0,
		176,10,0,9,100,106,9,70,79,82,78,69,67,69,
		68,0,100,100,9,20,6,176,11,0,106,9,70,79,
		82,78,69,67,69,68,0,20,1,36,110,0,176,12,
		0,20,0,36,111,0,176,9,0,106,2,49,0,20,
		1,36,112,0,176,10,0,9,100,106,7,67,80,65,
		71,65,82,0,100,100,9,20,6,176,11,0,106,7,
		67,80,65,71,65,82,0,20,1,176,11,0,106,9,
		68,65,84,65,95,67,80,71,0,20,1,176,11,0,
		106,9,68,65,84,65,95,67,80,86,0,20,1,176,
		11,0,106,9,78,85,77,95,67,72,69,81,0,20,
		1,36,113,0,176,12,0,20,0,36,114,0,176,6,
		0,106,3,48,49,0,20,1,36,115,0,176,7,0,
		106,18,67,111,110,102,105,114,109,97,32,40,83,47,
		78,41,32,63,32,0,20,1,36,116,0,109,8,0,
		106,2,83,0,5,29,189,15,36,118,0,176,6,0,
		106,3,49,49,0,20,1,36,119,0,121,83,42,0,
		36,120,0,121,83,43,0,36,121,0,121,83,44,0,
		36,122,0,109,36,0,83,45,0,36,123,0,176,46,
		0,92,50,12,1,83,47,0,36,124,0,134,0,0,
		0,0,83,48,0,36,125,0,109,40,0,83,49,0,
		36,126,0,134,0,0,0,0,83,50,0,36,127,0,
		121,83,51,0,36,128,0,121,83,52,0,36,129,0,
		121,83,53,0,36,130,0,106,7,79,67,32,78,111,
		46,0,176,54,0,109,38,0,92,5,12,2,72,83,
		55,0,36,131,0,176,9,0,106,2,51,0,20,1,
		36,132,0,176,56,0,12,0,28,48,36,133,0,109,
		57,0,83,42,0,36,134,0,109,58,0,83,59,0,
		36,135,0,109,60,0,83,61,0,36,136,0,109,42,
		0,122,72,78,57,0,36,137,0,176,62,0,20,0,
		36,139,0,109,59,0,28,79,36,140,0,176,9,0,
		106,2,56,0,20,1,36,141,0,176,10,0,9,100,
		106,9,70,79,82,78,69,67,69,68,0,100,100,9,
		20,6,36,142,0,176,63,0,106,9,84,69,77,80,
		95,70,79,82,0,106,5,78,79,77,69,0,90,6,
		109,64,0,6,100,20,4,36,143,0,176,12,0,20,
		0,36,145,0,109,61,0,28,84,36,146,0,176,9,
		0,106,2,52,0,20,1,36,147,0,176,10,0,9,
		100,106,9,67,76,65,83,83,69,95,68,0,100,100,
		9,20,6,36,148,0,176,63,0,106,9,84,69,77,
		80,95,67,76,68,0,106,10,68,69,83,67,82,73,
		67,65,79,0,90,6,109,65,0,6,100,20,4,36,
		149,0,176,12,0,20,0,36,151,0,109,42,0,83,
		66,0,36,152,0,176,23,0,92,6,92,5,20,2,
		176,24,0,106,17,76,97,110,135,97,109,101,110,116,
		111,32,32,32,32,58,32,0,20,1,176,67,0,176,
		68,0,12,0,176,69,0,12,0,122,72,20,2,176,
		70,0,109,71,0,176,72,0,100,106,10,101,78,85,
		77,95,76,65,78,67,0,106,14,64,82,32,57,57,
		57,57,57,57,57,57,57,57,0,100,100,12,5,20,
		2,48,73,0,176,74,0,109,71,0,12,1,112,0,
		73,36,153,0,176,75,0,109,71,0,100,100,100,100,
		100,100,20,7,4,0,0,83,71,0,109,71,0,73,
		36,154,0,109,42,0,121,5,28,91,36,155,0,176,
		9,0,106,2,51,0,20,1,36,156,0,176,56,0,
		12,0,28,19,36,157,0,109,66,0,78,57,0,36,
		158,0,176,62,0,20,0,36,160,0,176,26,0,20,
		0,176,9,0,106,2,49,0,20,1,176,27,0,100,
		20,1,36,161,0,176,28,0,121,121,176,2,0,12,
		0,176,3,0,12,0,109,41,0,20,5,36,162,0,
		7,36,164,0,176,9,0,106,2,49,0,20,1,36,
		165,0,176,12,0,20,0,36,166,0,176,76,0,109,
		42,0,100,100,20,3,36,167,0,176,77,0,12,0,
		28,82,36,168,0,176,6,0,106,3,48,49,0,20,
		1,36,169,0,176,78,0,106,10,65,116,101,110,135,
		198,111,32,33,0,92,3,20,2,36,170,0,176,79,
		0,106,21,76,97,110,135,97,109,101,110,116,111,32,
		74,160,32,69,120,105,115,116,101,0,20,1,36,171,
		0,176,80,0,20,0,36,172,0,26,25,253,36,174,
		0,176,9,0,106,2,52,0,20,1,36,175,0,176,
		12,0,20,0,36,176,0,109,61,0,32,71,2,36,
		178,0,121,83,43,0,36,179,0,176,23,0,92,8,
		92,5,20,2,176,24,0,106,17,67,111,100,46,32,
		68,101,115,112,101,115,97,32,32,58,32,0,20,1,
		176,67,0,176,68,0,12,0,176,69,0,12,0,122,
		72,20,2,176,70,0,109,71,0,176,72,0,100,106,
		10,101,67,76,65,83,83,69,95,68,0,106,12,64,
		82,32,57,57,57,57,57,57,57,57,0,100,100,12,
		5,20,2,48,73,0,176,74,0,109,71,0,12,1,
		112,0,73,36,180,0,176,75,0,109,71,0,100,100,
		100,100,100,100,20,7,4,0,0,83,71,0,109,71,
		0,73,36,181,0,109,43,0,121,5,29,43,1,36,
		182,0,176,16,0,106,9,87,47,78,44,43,87,47,
		82,0,20,1,36,183,0,121,83,43,0,36,184,0,
		176,81,0,92,8,121,92,8,20,3,176,67,0,92,
		8,121,20,2,36,185,0,176,23,0,92,8,92,5,
		20,2,176,24,0,106,17,67,111,100,46,32,68,101,
		115,112,101,115,97,32,32,58,32,0,20,1,36,186,
		0,176,9,0,106,2,52,0,20,1,36,187,0,176,
		12,0,20,0,36,188,0,92,2,3,1,0,92,2,
		3,1,0,92,2,3,1,0,92,2,3,1,0,176,
		21,0,108,17,108,18,108,19,108,20,20,4,81,20,
		0,81,19,0,81,18,0,81,17,0,36,189,0,176,
		82,0,109,17,0,120,109,20,0,20,3,36,190,0,
		106,8,64,82,57,57,57,57,57,0,98,18,0,122,
		2,36,191,0,106,5,64,83,52,48,0,98,18,0,
		92,2,2,36,192,0,106,5,32,32,32,32,0,98,
		19,0,122,2,36,193,0,106,5,32,32,32,32,0,
		98,19,0,92,2,2,36,194,0,176,25,0,92,8,
		92,20,92,8,92,75,109,17,0,106,10,70,67,79,
		78,95,67,84,80,71,0,109,18,0,109,19,0,106,
		2,32,0,106,4,32,45,32,0,106,2,32,0,20,
		11,36,195,0,109,83,0,83,43,0,36,196,0,25,
		126,36,198,0,176,76,0,109,43,0,100,100,20,3,
		36,199,0,176,77,0,12,0,28,51,36,200,0,176,
		23,0,92,8,92,31,20,2,176,24,0,106,4,32,
		45,32,0,176,84,0,109,65,0,12,1,72,20,1,
		36,201,0,176,9,0,106,2,49,0,20,1,36,202,
		0,25,54,36,204,0,176,6,0,106,3,48,49,0,
		20,1,36,205,0,176,78,0,106,21,67,108,97,115,
		115,101,32,73,110,101,120,105,115,116,101,110,116,101,
		32,33,0,92,3,20,2,36,206,0,26,197,253,36,
		209,0,26,195,1,36,212,0,176,46,0,92,30,12,
		1,83,43,0,36,213,0,176,23,0,92,8,92,5,
		20,2,176,24,0,106,17,67,111,100,46,32,68,101,
		115,112,101,115,97,32,32,58,32,0,20,1,176,67,
		0,176,68,0,12,0,176,69,0,12,0,122,72,20,
		2,176,70,0,109,71,0,176,72,0,100,106,10,101,
		67,76,65,83,83,69,95,68,0,106,5,64,83,51,
		48,0,100,100,12,5,20,2,48,73,0,176,74,0,
		109,71,0,12,1,112,0,73,36,214,0,176,75,0,
		109,71,0,100,100,100,100,100,100,20,7,4,0,0,
		83,71,0,109,71,0,73,36,215,0,176,84,0,109,
		43,0,12,1,83,43,0,36,216,0,176,9,0,106,
		2,52,0,20,1,36,217,0,176,76,0,109,43,0,
		100,100,20,3,36,218,0,176,16,0,106,9,87,47,
		78,44,43,87,47,82,0,20,1,36,219,0,176,81,
		0,92,8,121,92,8,20,3,176,67,0,92,8,121,
		20,2,36,220,0,176,23,0,92,8,92,5,20,2,
		176,24,0,106,17,67,111,100,46,32,68,101,115,112,
		101,115,97,32,32,58,32,0,20,1,36,221,0,92,
		2,3,1,0,92,2,3,1,0,92,2,3,1,0,
		92,2,3,1,0,176,21,0,108,17,108,18,108,19,
		108,20,20,4,81,20,0,81,19,0,81,18,0,81,
		17,0,36,222,0,176,82,0,109,17,0,120,109,20,
		0,20,3,36,223,0,106,8,64,82,57,57,57,57,
		57,0,98,18,0,122,2,36,224,0,106,5,64,83,
		52,48,0,98,18,0,92,2,2,36,225,0,106,5,
		32,32,32,32,0,98,19,0,122,2,36,226,0,106,
		5,32,32,32,32,0,98,19,0,92,2,2,36,227,
		0,176,25,0,92,8,92,20,92,8,92,75,109,17,
		0,106,10,70,67,79,78,95,67,84,80,71,0,109,
		18,0,109,19,0,106,2,32,0,106,4,32,45,32,
		0,106,2,32,0,20,11,36,228,0,109,83,0,83,
		43,0,36,232,0,176,9,0,106,2,53,0,20,1,
		36,233,0,176,12,0,20,0,36,235,0,176,23,0,
		92,9,92,5,20,2,176,24,0,106,17,67,111,100,
		46,32,69,109,112,114,101,101,110,100,46,58,32,0,
		20,1,176,67,0,176,68,0,12,0,176,69,0,12,
		0,122,72,20,2,176,70,0,109,71,0,176,72,0,
		100,106,12,101,67,76,65,83,83,69,95,80,82,79,
		0,106,12,64,82,32,57,57,57,57,57,57,57,57,
		0,100,100,12,5,20,2,48,73,0,176,74,0,109,
		71,0,12,1,112,0,73,36,236,0,176,75,0,109,
		71,0,100,100,100,100,100,100,20,7,4,0,0,83,
		71,0,109,71,0,73,36,237,0,109,44,0,121,5,
		32,135,0,36,240,0,176,76,0,109,44,0,100,100,
		20,3,36,241,0,176,77,0,12,0,28,51,36,242,
		0,176,23,0,92,9,92,31,20,2,176,24,0,106,
		4,32,45,32,0,176,84,0,109,65,0,12,1,72,
		20,1,36,243,0,176,9,0,106,2,49,0,20,1,
		36,244,0,25,62,36,246,0,176,6,0,106,3,48,
		49,0,20,1,36,247,0,176,78,0,106,29,69,109,
		112,114,101,101,110,100,105,109,101,110,116,111,32,73,
		110,101,120,105,115,116,101,110,116,101,32,33,0,92,
		3,20,2,36,248,0,26,234,254,36,252,0,109,59,
		0,32,192,1,36,254,0,176,23,0,92,10,92,5,
		20,2,176,24,0,106,17,70,111,114,110,101,99,101,
		100,111,114,32,32,32,32,58,32,0,20,1,176,67,
		0,176,68,0,12,0,176,69,0,12,0,122,72,20,
		2,176,70,0,109,71,0,176,72,0,100,106,12,101,
		67,79,68,73,71,79,95,70,79,82,0,106,11,64,
		82,32,57,57,57,57,57,57,57,0,100,100,12,5,
		20,2,48,73,0,176,74,0,109,71,0,12,1,112,
		0,73,36,255,0,176,75,0,109,71,0,100,100,100,
		100,100,100,20,7,4,0,0,83,71,0,109,71,0,
		73,36,0,1,176,9,0,106,2,56,0,20,1,36,
		1,1,176,76,0,109,45,0,100,100,20,3,36,2,
		1,176,16,0,106,9,87,47,78,44,43,87,47,82,
		0,20,1,36,3,1,121,83,45,0,36,4,1,176,
		81,0,92,10,121,92,10,20,3,176,67,0,92,10,
		121,20,2,36,5,1,176,23,0,92,10,92,5,20,
		2,176,24,0,106,17,70,111,114,110,101,99,101,100,
		111,114,32,32,32,32,58,32,0,20,1,36,6,1,
		176,9,0,106,2,56,0,20,1,36,7,1,92,2,
		3,1,0,92,2,3,1,0,92,2,3,1,0,92,
		2,3,1,0,176,21,0,108,17,108,18,108,19,108,
		20,20,4,81,20,0,81,19,0,81,18,0,81,17,
		0,36,8,1,176,82,0,109,17,0,120,109,20,0,
		20,3,36,9,1,106,8,64,82,57,57,57,57,57,
		0,98,18,0,122,2,36,10,1,106,5,64,83,52,
		48,0,98,18,0,92,2,2,36,11,1,106,5,32,
		32,32,32,0,98,19,0,122,2,36,12,1,106,5,
		32,32,32,32,0,98,19,0,92,2,2,36,13,1,
		176,25,0,92,10,92,20,92,10,92,75,109,17,0,
		106,10,70,67,79,78,95,67,84,80,71,0,109,18,
		0,109,19,0,106,2,32,0,106,4,32,45,32,0,
		106,2,32,0,20,11,36,14,1,109,83,0,83,45,
		0,36,18,1,176,9,0,106,2,49,0,20,1,36,
		20,1,176,23,0,92,11,92,5,20,2,176,24,0,
		106,17,68,97,116,97,32,80,114,101,118,105,115,116,
		97,32,58,32,0,20,1,176,67,0,176,68,0,12,
		0,176,69,0,12,0,122,72,20,2,176,70,0,109,
		71,0,176,72,0,100,106,11,101,68,65,84,65,95,
		80,82,69,86,0,100,100,100,12,5,20,2,48,73,
		0,176,74,0,109,71,0,12,1,112,0,73,36,21,
		1,176,75,0,109,71,0,100,100,100,100,100,100,20,
		7,4,0,0,83,71,0,109,71,0,73,36,22,1,
		109,48,0,134,0,0,0,0,5,32,120,255,36,28,
		1,176,23,0,92,12,92,5,20,2,176,24,0,106,
		17,86,108,114,46,32,80,114,101,118,105,115,116,111,
		32,58,32,0,20,1,176,67,0,176,68,0,12,0,
		176,69,0,12,0,122,72,20,2,176,70,0,109,71,
		0,176,72,0,100,106,10,101,80,82,69,86,73,83,
		84,79,0,106,22,64,82,32,57,57,57,44,57,57,
		57,44,57,57,57,44,57,57,57,46,57,57,0,100,
		100,12,5,20,2,48,73,0,176,74,0,109,71,0,
		12,1,112,0,73,36,29,1,176,75,0,109,71,0,
		100,100,100,100,100,100,20,7,4,0,0,83,71,0,
		109,71,0,73,36,30,1,176,23,0,92,13,92,5,
		20,2,176,24,0,106,17,78,117,109,46,32,68,111,
		99,117,109,101,110,116,111,58,32,0,20,1,176,67,
		0,176,68,0,12,0,176,69,0,12,0,122,72,20,
		2,176,70,0,109,71,0,176,72,0,100,106,12,101,
		78,85,77,95,68,79,67,85,77,69,0,100,100,100,
		12,5,20,2,48,73,0,176,74,0,109,71,0,12,
		1,112,0,73,36,31,1,176,75,0,109,71,0,100,
		100,100,100,100,100,20,7,4,0,0,83,71,0,109,
		71,0,73,36,32,1,176,23,0,92,14,92,5,20,
		2,176,24,0,106,17,68,101,115,99,114,105,135,198,
		111,32,32,32,32,32,58,32,0,20,1,176,67,0,
		176,68,0,12,0,176,69,0,12,0,122,72,20,2,
		176,70,0,109,71,0,176,72,0,100,106,11,101,68,
		69,83,67,82,73,67,65,79,0,100,100,100,12,5,
		20,2,48,73,0,176,74,0,109,71,0,12,1,112,
		0,73,36,33,1,176,75,0,109,71,0,100,100,100,
		100,100,100,20,7,4,0,0,83,71,0,109,71,0,
		73,36,34,1,176,23,0,92,15,92,5,20,2,176,
		24,0,106,17,68,97,116,97,32,80,97,103,97,109,
		101,110,116,111,58,32,0,20,1,176,67,0,176,68,
		0,12,0,176,69,0,12,0,122,72,20,2,176,70,
		0,109,71,0,176,72,0,100,106,11,101,68,65,84,
		65,95,82,69,65,76,0,100,100,100,12,5,20,2,
		48,73,0,176,74,0,109,71,0,12,1,112,0,73,
		36,35,1,176,75,0,109,71,0,100,100,100,100,100,
		100,20,7,4,0,0,83,71,0,109,71,0,73,36,
		36,1,176,23,0,92,16,92,5,20,2,176,24,0,
		106,17,86,108,114,46,32,82,101,97,108,105,122,97,
		100,111,58,32,0,20,1,176,67,0,176,68,0,12,
		0,176,69,0,12,0,122,72,20,2,176,70,0,109,
		71,0,176,72,0,100,106,11,101,82,69,65,76,73,
		90,65,68,79,0,106,22,64,82,32,57,57,57,44,
		57,57,57,44,57,57,57,44,57,57,57,46,57,57,
		0,100,100,12,5,20,2,48,73,0,176,74,0,109,
		71,0,12,1,112,0,73,36,37,1,176,75,0,109,
		71,0,100,100,100,100,100,100,20,7,4,0,0,83,
		71,0,109,71,0,73,36,38,1,176,23,0,92,17,
		92,5,20,2,176,24,0,106,17,67,162,100,105,103,
		111,32,67,111,110,116,97,32,32,58,32,0,20,1,
		176,67,0,176,68,0,12,0,176,69,0,12,0,122,
		72,20,2,176,70,0,109,71,0,176,72,0,100,106,
		11,101,67,79,68,95,67,79,78,84,65,0,106,13,
		64,82,32,57,57,57,57,57,57,57,57,57,0,100,
		100,12,5,20,2,48,73,0,176,74,0,109,71,0,
		12,1,112,0,73,36,39,1,176,75,0,109,71,0,
		100,100,100,100,100,100,20,7,4,0,0,83,71,0,
		109,71,0,73,36,40,1,176,23,0,92,18,92,5,
		20,2,176,24,0,106,17,78,117,109,46,32,67,104,
		101,113,117,101,32,32,32,58,32,0,20,1,176,67,
		0,176,68,0,12,0,176,69,0,12,0,122,72,20,
		2,176,70,0,109,71,0,176,72,0,100,106,10,101,
		78,85,77,95,67,72,69,81,0,106,13,64,82,32,
		57,57,57,57,57,57,57,57,57,0,100,100,12,5,
		20,2,48,73,0,176,74,0,109,71,0,12,1,112,
		0,73,36,41,1,176,75,0,109,71,0,100,100,100,
		100,100,100,20,7,4,0,0,83,71,0,109,71,0,
		73,36,42,1,176,9,0,106,2,49,0,20,1,36,
		43,1,176,6,0,106,3,48,49,0,20,1,36,44,
		1,176,7,0,106,18,67,111,110,102,105,114,109,97,
		32,40,83,47,78,41,32,63,32,0,20,1,36,45,
		1,109,8,0,106,2,83,0,5,29,42,1,36,46,
		1,176,6,0,106,3,48,49,0,20,1,36,47,1,
		176,79,0,106,22,71,114,97,118,97,110,100,111,44,
		32,65,103,117,97,114,100,101,32,46,46,46,0,20,
		1,36,48,1,176,9,0,106,2,49,0,20,1,36,
		49,1,176,85,0,20,0,36,50,1,176,56,0,12,
		0,28,101,36,52,1,109,42,0,78,86,0,109,43,
		0,78,87,0,36,54,1,109,44,0,78,88,0,109,
		45,0,78,89,0,36,57,1,109,47,0,78,65,0,
		109,48,0,78,90,0,109,49,0,78,91,0,36,60,
		1,109,50,0,78,92,0,109,51,0,78,93,0,109,
		52,0,78,94,0,36,63,1,109,53,0,78,95,0,
		109,55,0,78,96,0,120,78,97,0,36,64,1,176,
		62,0,20,0,36,66,1,176,98,0,109,99,0,106,
		15,67,79,78,84,65,83,32,65,32,80,65,71,65,
		82,0,106,18,73,78,67,76,85,83,65,79,32,76,
		65,78,67,46,32,58,32,0,176,84,0,176,54,0,
		109,42,0,92,5,12,2,12,1,72,20,3,36,67,
		1,176,9,0,106,2,50,0,20,1,36,68,1,176,
		56,0,12,0,28,54,36,70,1,109,42,0,78,86,
		0,106,8,70,69,67,72,65,68,79,0,78,14,0,
		36,71,1,176,62,0,20,0,36,73,1,25,19,36,
		75,1,176,9,0,106,2,50,0,20,1,36,76,1,
		25,2,36,80,1,176,28,0,121,121,176,2,0,12,
		0,176,3,0,12,0,109,41,0,20,5,36,81,1,
		25,20,36,82,1,109,33,0,92,27,8,28,9,36,
		83,1,121,83,32,0,36,86,1,109,32,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

