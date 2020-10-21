/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/imp_ctrc.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( IMP_CTRC );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( ADIR );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( ASORT );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( ACHOICE );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( __DBZAP );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( F_BLOQA );
HB_FUNC_EXTERN( DBEVAL );
HB_FUNC_EXTERN( DBDELETE );
HB_FUNC_EXTERN( __DBPACK );
HB_FUNC_EXTERN( __DBSDF );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( DBCLOSEAREA );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( FCOUNT );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC( F_INDIIMP2 );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_IMP_CTRC )
{ "IMP_CTRC", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( IMP_CTRC )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EDRIVE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESUBDIR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDIR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "ECORI_RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "ADIR", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADIR )}, NULL },
{ "ARQ_TXT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "ASORT", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASORT )}, NULL },
{ "TELA06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "ACHOICE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ACHOICE )}, NULL },
{ "MENU_ARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "ENOME_ARQ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "__DBZAP", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBZAP )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "F_BLOQA", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQA )}, NULL },
{ "DBEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEVAL )}, NULL },
{ "DBDELETE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBDELETE )}, NULL },
{ "__DBPACK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBPACK )}, NULL },
{ "__DBSDF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBSDF )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "DBCLOSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEAREA )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCOUNT )}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "ECLASSE_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EEMPREENDIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EVALOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENOTAFISCAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATANF", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUMCOMPRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDATAPAGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EVLRPAGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUMCHEQUE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_LANC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RECEITA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMPREENDIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VALOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NF", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATANF", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_CHEQUE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "VLR_PGTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LOK", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "LANC_CRE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "LANCAMENTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CODIGO_CLI", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
{ "DATA_PREV", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PREVISTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_REAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REALIZADO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "COD_CONTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_DOCUME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CONTABIL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_NOTA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "INTEGRACAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "F_INDIIMP2", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_INDIIMP2 )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_IMP_CTRC, "C:/sis/SAN/xhb/imp_ctrc.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_IMP_CTRC
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_IMP_CTRC )
   #include "hbiniseg.h"
#endif

HB_FUNC( IMP_CTRC )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,105,109,112,95,99,116,114,99,46,112,114,103,
		58,73,77,80,95,67,84,82,67,0,36,14,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,15,0,176,5,0,106,3,49,
		49,0,20,1,36,16,0,176,6,0,106,43,73,77,
		80,79,82,84,65,128,199,79,32,68,65,68,79,83,
		32,67,79,78,84,65,83,32,65,32,82,69,67,69,
		66,69,82,32,69,77,32,84,69,88,84,79,0,20,
		1,36,17,0,176,7,0,92,5,12,1,83,8,0,
		36,18,0,176,7,0,92,20,12,1,83,9,0,36,
		19,0,176,7,0,92,40,12,1,83,10,0,36,20,
		0,106,4,72,32,32,0,83,8,0,36,21,0,106,
		59,92,65,82,81,85,73,86,79,83,32,68,69,32,
		66,65,73,88,65,92,67,82,69,67,69,66,69,82,
		92,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,0,83,9,0,36,22,0,176,11,0,109,
		8,0,12,1,106,2,58,0,72,176,11,0,109,9,
		0,12,1,72,83,10,0,36,23,0,106,4,84,88,
		84,0,83,12,0,36,24,0,176,13,0,92,7,121,
		20,2,176,14,0,106,21,68,114,105,118,101,32,100,
		101,32,68,97,100,111,115,32,32,32,32,58,32,0,
		20,1,176,15,0,176,16,0,12,0,176,17,0,12,
		0,122,72,20,2,176,18,0,109,19,0,176,20,0,
		100,106,7,101,68,82,73,86,69,0,106,6,64,33,
		88,88,88,0,100,100,12,5,20,2,48,21,0,176,
		22,0,109,19,0,12,1,112,0,73,36,25,0,176,
		13,0,92,8,121,20,2,176,14,0,106,21,83,117,
		98,68,105,114,101,116,162,114,105,111,32,32,32,32,
		32,32,58,32,0,20,1,176,15,0,176,16,0,12,
		0,176,17,0,12,0,122,72,20,2,176,18,0,109,
		19,0,176,20,0,100,106,8,101,83,85,66,68,73,
		82,0,106,5,64,83,52,48,0,100,100,12,5,20,
		2,48,21,0,176,22,0,109,19,0,12,1,112,0,
		73,36,26,0,176,13,0,92,9,121,20,2,176,14,
		0,106,21,69,120,116,101,110,135,198,111,32,32,32,
		32,32,32,32,32,32,32,58,32,0,20,1,176,15,
		0,176,16,0,12,0,176,17,0,12,0,122,72,20,
		2,176,18,0,109,19,0,176,20,0,100,106,10,101,
		67,79,82,73,95,82,69,84,0,106,6,64,33,88,
		88,88,0,100,100,12,5,20,2,48,21,0,176,22,
		0,109,19,0,12,1,112,0,73,36,27,0,176,23,
		0,109,19,0,100,100,100,100,100,100,20,7,4,0,
		0,83,19,0,109,19,0,73,36,28,0,176,5,0,
		106,3,48,49,0,20,1,36,29,0,176,24,0,106,
		35,68,101,115,101,106,97,32,67,97,112,116,117,114,
		97,114,32,111,32,65,114,113,117,105,118,111,32,40,
		83,47,78,41,32,63,32,0,20,1,36,30,0,109,
		25,0,106,2,83,0,5,29,193,4,36,31,0,176,
		5,0,106,3,48,49,0,20,1,36,32,0,176,26,
		0,106,47,65,103,117,97,114,100,101,32,97,32,67,
		97,112,116,117,114,97,32,100,111,115,32,65,114,113,
		117,105,118,111,115,32,68,105,115,112,111,110,161,118,
		101,105,115,32,46,46,46,0,20,1,36,33,0,36,
		35,0,176,11,0,176,11,0,109,8,0,12,1,106,
		2,58,0,72,176,11,0,109,9,0,12,1,72,106,
		3,42,46,0,72,176,11,0,109,12,0,12,1,72,
		12,1,83,10,0,36,36,0,176,27,0,92,11,121,
		92,11,20,3,176,15,0,92,11,121,20,2,36,37,
		0,176,13,0,92,11,121,20,2,176,14,0,106,21,
		83,117,98,68,105,114,101,116,162,114,105,111,32,32,
		32,32,32,32,58,32,0,20,1,176,15,0,176,16,
		0,12,0,176,17,0,12,0,122,72,20,2,176,18,
		0,109,19,0,176,20,0,100,106,5,101,68,73,82,
		0,100,100,100,12,5,20,2,48,21,0,176,22,0,
		109,19,0,12,1,112,0,73,36,38,0,176,28,0,
		109,10,0,12,1,83,29,0,36,39,0,109,29,0,
		83,30,0,36,40,0,109,29,0,3,1,0,176,32,
		0,108,31,20,1,81,31,0,36,41,0,176,28,0,
		109,10,0,109,31,0,20,2,36,42,0,176,33,0,
		109,31,0,20,1,36,43,0,176,1,0,121,121,176,
		2,0,12,0,176,3,0,12,0,12,4,83,34,0,
		36,44,0,176,27,0,92,12,92,25,92,21,92,40,
		20,4,176,15,0,92,12,92,25,20,2,36,45,0,
		176,35,0,92,12,92,25,92,21,92,40,92,2,20,
		5,36,46,0,36,47,0,176,13,0,92,13,92,5,
		20,2,176,14,0,106,16,65,114,113,117,105,118,111,
		115,32,32,32,32,32,58,32,0,20,1,36,48,0,
		176,36,0,92,13,92,27,92,20,92,39,109,31,0,
		12,5,83,37,0,36,50,0,176,38,0,12,0,92,
		13,8,28,185,36,51,0,98,31,0,109,37,0,1,
		83,39,0,36,52,0,36,55,0,176,40,0,121,121,
		176,2,0,12,0,176,3,0,12,0,109,34,0,20,
		5,36,56,0,176,13,0,92,14,121,20,2,176,14,
		0,106,24,65,114,113,117,105,118,111,32,83,101,108,
		101,99,105,111,110,97,100,111,32,32,58,32,0,20,
		1,176,15,0,176,16,0,12,0,176,17,0,12,0,
		122,72,20,2,176,18,0,109,19,0,176,20,0,100,
		106,10,101,78,79,77,69,95,65,82,81,0,100,100,
		100,12,5,20,2,48,21,0,176,22,0,109,19,0,
		12,1,112,0,73,36,57,0,176,41,0,120,20,1,
		4,0,0,83,19,0,109,19,0,73,36,58,0,36,
		62,0,176,5,0,106,3,48,49,0,20,1,36,63,
		0,176,26,0,106,36,65,103,117,97,114,100,101,32,
		97,32,80,114,101,112,97,114,97,135,198,111,32,100,
		111,32,65,114,113,117,105,118,111,32,46,46,46,0,
		20,1,36,64,0,176,42,0,106,2,50,0,20,1,
		36,65,0,176,43,0,92,8,106,3,79,78,0,20,
		2,36,66,0,176,44,0,9,100,106,9,84,82,65,
		78,83,67,82,69,0,100,100,9,20,6,36,67,0,
		176,45,0,20,0,36,68,0,176,46,0,20,0,36,
		69,0,176,47,0,12,0,29,219,1,36,70,0,176,
		5,0,106,3,48,49,0,20,1,36,71,0,176,24,
		0,106,35,68,101,115,101,106,97,32,67,97,114,114,
		101,103,97,114,32,111,32,65,114,113,117,105,118,111,
		32,40,83,47,78,41,32,63,32,0,20,1,36,72,
		0,109,25,0,106,2,83,0,5,29,182,0,36,73,
		0,176,42,0,106,2,49,0,20,1,36,74,0,176,
		11,0,109,39,0,12,1,83,39,0,36,75,0,176,
		42,0,106,2,50,0,20,1,36,76,0,176,48,0,
		90,46,51,67,58,47,115,105,115,47,83,65,78,47,
		120,104,98,47,105,109,112,95,99,116,114,99,46,112,
		114,103,58,73,77,80,95,67,84,82,67,0,176,49,
		0,12,0,6,100,100,100,100,9,20,6,36,77,0,
		176,50,0,20,0,36,78,0,176,46,0,20,0,36,
		79,0,176,11,0,109,8,0,12,1,106,2,58,0,
		72,176,11,0,109,9,0,12,1,72,176,11,0,109,
		39,0,12,1,72,83,31,0,36,80,0,176,51,0,
		9,109,31,0,4,0,0,100,100,100,100,9,100,20,
		9,36,81,0,176,46,0,20,0,36,83,0,176,46,
		0,20,0,36,84,0,176,49,0,20,0,36,85,0,
		176,50,0,20,0,36,86,0,176,48,0,90,57,51,
		67,58,47,115,105,115,47,83,65,78,47,120,104,98,
		47,105,109,112,95,99,116,114,99,46,112,114,103,58,
		73,77,80,95,67,84,82,67,0,176,52,0,109,53,
		0,92,2,92,40,12,3,165,78,53,0,6,100,100,
		100,100,9,20,6,36,87,0,176,54,0,20,0,36,
		88,0,176,42,0,106,2,50,0,20,1,36,89,0,
		176,55,0,20,0,36,90,0,176,44,0,9,100,106,
		9,84,82,65,78,83,67,82,69,0,100,100,9,20,
		6,36,91,0,176,46,0,20,0,36,92,0,176,43,
		0,92,8,106,4,79,70,70,0,20,2,36,93,0,
		176,5,0,106,3,48,49,0,20,1,36,94,0,176,
		56,0,106,23,73,109,112,111,114,116,97,135,198,111,
		32,67,111,110,99,108,117,161,100,97,32,33,0,92,
		3,20,2,36,95,0,36,97,0,176,5,0,106,3,
		48,49,0,20,1,36,98,0,176,24,0,106,51,68,
		101,115,101,106,97,32,86,105,115,117,97,108,105,122,
		97,114,32,111,115,32,82,101,103,105,115,116,114,111,
		115,32,67,97,112,116,117,114,97,100,111,115,32,40,
		83,47,78,41,32,63,32,0,20,1,36,99,0,109,
		25,0,106,2,83,0,5,29,206,3,36,100,0,176,
		5,0,106,3,48,49,0,20,1,36,101,0,176,42,
		0,106,2,50,0,20,1,36,102,0,176,55,0,20,
		0,36,103,0,176,44,0,9,100,106,9,84,82,65,
		78,83,67,82,69,0,100,100,9,20,6,36,104,0,
		176,46,0,20,0,36,105,0,176,57,0,20,0,36,
		106,0,176,58,0,106,9,87,47,78,44,43,87,47,
		82,0,20,1,36,107,0,176,60,0,12,0,3,1,
		0,176,60,0,12,0,3,1,0,176,60,0,12,0,
		3,1,0,176,60,0,12,0,3,1,0,176,32,0,
		108,59,108,61,108,62,108,63,20,4,81,63,0,81,
		62,0,81,61,0,81,59,0,36,108,0,176,64,0,
		109,59,0,120,109,63,0,20,3,36,109,0,106,3,
		57,57,0,98,61,0,122,2,36,110,0,106,7,57,
		57,57,57,57,57,0,98,61,0,92,2,2,36,111,
		0,106,4,57,57,57,0,98,61,0,92,3,2,36,
		112,0,106,7,57,57,57,57,57,57,0,98,61,0,
		92,4,2,36,113,0,106,9,57,57,57,57,57,57,
		57,57,0,98,61,0,92,5,2,36,114,0,106,15,
		57,57,57,44,57,57,57,44,57,57,57,46,57,57,
		0,98,61,0,92,6,2,36,115,0,106,5,64,83,
		52,48,0,98,61,0,92,7,2,36,116,0,106,3,
		57,57,0,98,61,0,92,8,2,36,117,0,106,10,
		57,57,57,57,57,57,57,57,57,0,98,61,0,92,
		9,2,36,118,0,106,9,57,57,57,57,57,57,57,
		57,0,98,61,0,92,10,2,36,119,0,106,9,57,
		57,57,57,57,57,57,57,0,98,61,0,92,11,2,
		36,120,0,106,15,57,57,57,44,57,57,57,44,57,
		57,57,46,57,57,0,98,61,0,92,12,2,36,121,
		0,106,11,57,57,57,57,57,57,57,57,57,57,0,
		98,61,0,92,13,2,36,122,0,106,11,57,57,57,
		57,57,57,57,57,57,57,0,98,61,0,92,14,2,
		36,124,0,106,7,67,224,68,73,71,79,0,98,62,
		0,122,2,36,125,0,106,8,82,69,67,69,73,84,
		65,0,98,62,0,92,2,2,36,126,0,106,15,69,
		77,80,82,69,69,78,68,73,77,69,78,84,79,0,
		98,62,0,92,3,2,36,127,0,106,8,67,76,73,
		69,78,84,69,0,98,62,0,92,4,2,36,128,0,
		106,5,68,65,84,65,0,98,62,0,92,5,2,36,
		129,0,106,6,86,65,76,79,82,0,98,62,0,92,
		6,2,36,130,0,106,10,68,69,83,67,82,73,128,
		199,79,0,98,62,0,92,7,2,36,131,0,106,6,
		67,79,78,84,65,0,98,62,0,92,8,2,36,132,
		0,106,12,78,79,84,65,32,70,73,83,67,65,76,
		0,98,62,0,92,9,2,36,133,0,106,8,68,65,
		84,65,32,78,70,0,98,62,0,92,10,2,36,134,
		0,106,11,68,65,84,65,32,80,65,71,84,79,0,
		98,62,0,92,11,2,36,135,0,106,10,86,76,82,
		46,80,65,71,84,79,0,98,62,0,92,12,2,36,
		136,0,106,11,78,85,77,46,67,72,69,81,85,69,
		0,98,62,0,92,13,2,36,137,0,106,15,78,85,
		77,46,76,65,78,128,65,77,69,78,84,79,0,98,
		62,0,92,14,2,36,139,0,176,35,0,92,5,121,
		92,21,92,79,92,2,20,5,36,140,0,176,5,0,
		106,3,48,49,0,20,1,36,141,0,176,58,0,106,
		1,0,20,1,36,142,0,176,58,0,106,6,87,47,
		78,43,42,0,20,1,36,143,0,176,13,0,92,23,
		92,26,20,2,176,14,0,106,6,91,69,83,67,93,
		0,20,1,36,144,0,176,13,0,92,23,92,59,20,
		2,176,14,0,106,7,91,26,27,24,25,93,0,20,
		1,36,145,0,176,58,0,106,1,0,20,1,36,146,
		0,176,58,0,106,9,87,47,78,44,43,87,47,82,
		0,20,1,36,147,0,176,13,0,92,23,92,31,20,
		2,176,14,0,106,10,80,97,114,97,32,83,97,105,
		114,0,20,1,36,148,0,176,13,0,92,23,92,65,
		20,2,176,14,0,106,13,77,111,118,105,109,101,110,
		116,97,135,198,111,0,20,1,36,149,0,176,65,0,
		92,6,122,92,21,92,78,109,59,0,106,11,70,95,
		73,78,68,73,73,77,80,50,0,109,61,0,109,62,
		0,106,2,196,0,106,2,179,0,106,2,205,0,20,
		11,36,153,0,176,5,0,106,3,48,49,0,20,1,
		36,154,0,176,24,0,106,48,67,111,110,102,105,114,
		109,97,32,111,32,112,114,111,99,101,115,115,97,109,
		101,110,116,111,32,100,111,115,32,114,101,103,105,115,
		116,114,111,115,32,40,83,47,78,41,32,63,32,0,
		20,1,36,155,0,109,25,0,106,2,83,0,5,29,
		93,6,36,156,0,176,5,0,106,3,48,49,0,20,
		1,36,157,0,176,42,0,106,2,49,0,20,1,36,
		158,0,176,44,0,9,100,106,9,67,82,69,67,69,
		66,69,82,0,100,100,9,20,6,176,66,0,106,9,
		67,82,69,67,69,66,69,82,0,20,1,176,66,0,
		106,9,68,65,84,65,95,67,82,69,0,20,1,176,
		66,0,106,9,68,65,84,65,95,67,82,86,0,20,
		1,176,66,0,106,8,68,65,84,65,95,78,70,0,
		20,1,36,159,0,176,46,0,20,0,36,160,0,176,
		42,0,106,2,53,0,20,1,36,161,0,176,44,0,
		9,100,106,9,67,76,65,83,95,80,82,79,0,100,
		100,9,20,6,176,66,0,106,9,67,76,65,83,95,
		80,82,79,0,20,1,176,66,0,106,9,68,69,83,
		67,95,80,82,79,0,20,1,36,162,0,176,46,0,
		20,0,36,163,0,176,42,0,106,2,52,0,20,1,
		36,164,0,176,44,0,9,100,106,9,67,76,65,83,
		83,69,95,82,0,100,100,9,20,6,176,66,0,106,
		9,67,76,65,83,83,69,95,82,0,20,1,36,165,
		0,176,46,0,20,0,36,166,0,176,42,0,106,2,
		51,0,20,1,36,167,0,176,44,0,9,100,106,7,
		83,84,65,84,85,83,0,100,100,9,20,6,36,168,
		0,176,46,0,20,0,36,169,0,176,42,0,106,2,
		54,0,20,1,36,170,0,176,44,0,9,100,106,9,
		67,76,73,69,78,84,69,83,0,100,100,9,20,6,
		176,66,0,106,9,67,76,73,69,78,84,69,83,0,
		20,1,36,171,0,176,46,0,20,0,36,172,0,176,
		42,0,106,2,50,0,20,1,36,173,0,176,46,0,
		20,0,36,174,0,176,67,0,12,0,32,165,4,36,
		175,0,121,83,68,0,36,176,0,121,83,69,0,36,
		177,0,121,83,70,0,36,178,0,106,1,0,83,71,
		0,36,179,0,121,83,72,0,36,180,0,106,1,0,
		83,73,0,36,181,0,121,83,74,0,36,182,0,121,
		83,75,0,36,183,0,106,1,0,83,76,0,36,184,
		0,106,1,0,83,77,0,36,185,0,106,1,0,83,
		78,0,36,186,0,121,83,79,0,36,187,0,121,83,
		80,0,36,188,0,121,83,81,0,36,190,0,176,42,
		0,106,2,50,0,20,1,36,191,0,109,82,0,83,
		68,0,36,192,0,109,83,0,83,69,0,36,193,0,
		109,84,0,83,70,0,36,194,0,176,11,0,109,85,
		0,12,1,83,71,0,36,195,0,176,52,0,109,71,
		0,122,92,2,12,3,106,2,47,0,72,176,52,0,
		109,71,0,92,3,92,2,12,3,72,106,2,47,0,
		72,176,52,0,109,71,0,92,5,92,4,12,3,72,
		83,71,0,36,196,0,109,86,0,83,72,0,36,197,
		0,109,53,0,83,73,0,36,198,0,109,87,0,83,
		74,0,36,199,0,109,88,0,83,75,0,36,200,0,
		176,11,0,109,89,0,12,1,83,76,0,36,201,0,
		176,52,0,109,76,0,122,92,2,12,3,106,2,47,
		0,72,176,52,0,109,76,0,92,3,92,2,12,3,
		72,106,2,47,0,72,176,52,0,109,76,0,92,5,
		92,4,12,3,72,83,76,0,36,202,0,176,11,0,
		109,90,0,12,1,83,78,0,36,203,0,176,52,0,
		109,78,0,122,92,2,12,3,106,2,47,0,72,176,
		52,0,109,78,0,92,3,92,2,12,3,72,106,2,
		47,0,72,176,52,0,109,78,0,92,5,92,4,12,
		3,72,83,78,0,36,204,0,109,91,0,83,80,0,
		36,206,0,109,92,0,83,79,0,36,220,0,120,83,
		93,0,36,221,0,176,42,0,106,2,52,0,20,1,
		36,222,0,176,46,0,20,0,36,223,0,176,94,0,
		109,68,0,100,100,20,3,36,224,0,176,95,0,12,
		0,31,95,36,225,0,9,83,93,0,36,226,0,176,
		5,0,106,3,48,49,0,20,1,36,227,0,176,26,
		0,106,35,67,162,100,105,103,111,32,100,101,32,82,
		101,99,101,105,116,97,32,78,198,111,32,69,110,99,
		111,110,116,114,97,100,97,32,40,0,176,11,0,176,
		96,0,109,68,0,12,1,12,1,72,106,3,41,33,
		0,72,20,1,36,228,0,176,97,0,20,0,36,231,
		0,176,42,0,106,2,54,0,20,1,36,232,0,176,
		46,0,20,0,36,233,0,176,94,0,109,70,0,100,
		100,20,3,36,234,0,176,95,0,12,0,31,85,36,
		235,0,9,83,93,0,36,236,0,176,5,0,106,3,
		48,49,0,20,1,36,237,0,176,26,0,106,25,67,
		108,105,101,110,116,101,32,78,198,111,32,69,110,99,
		111,110,116,114,97,100,111,32,40,0,176,11,0,176,
		96,0,109,70,0,12,1,12,1,72,106,3,41,33,
		0,72,20,1,36,238,0,176,97,0,20,0,36,241,
		0,176,42,0,106,2,53,0,20,1,36,242,0,176,
		46,0,20,0,36,243,0,176,94,0,109,69,0,100,
		100,20,3,36,244,0,176,95,0,12,0,31,92,36,
		245,0,9,83,93,0,36,246,0,176,5,0,106,3,
		48,49,0,20,1,36,247,0,176,26,0,106,32,69,
		109,112,114,101,101,110,100,105,109,101,110,116,111,32,
		78,198,111,32,69,110,99,111,110,116,114,97,100,111,
		32,40,0,176,11,0,176,96,0,109,69,0,12,1,
		12,1,72,106,3,41,33,0,72,20,1,36,248,0,
		176,97,0,20,0,36,252,0,109,93,0,31,29,36,
		253,0,176,42,0,106,2,50,0,20,1,36,254,0,
		176,98,0,122,20,1,36,255,0,26,177,252,36,3,
		1,176,42,0,106,2,51,0,20,1,36,4,1,176,
		46,0,20,0,36,5,1,176,99,0,12,0,28,30,
		36,6,1,109,100,0,83,81,0,36,7,1,109,81,
		0,122,72,78,100,0,36,8,1,176,54,0,20,0,
		36,12,1,176,5,0,106,3,48,49,0,20,1,36,
		13,1,176,26,0,106,11,71,114,97,118,97,110,100,
		111,32,40,0,176,11,0,176,96,0,109,81,0,12,
		1,12,1,72,106,15,41,44,32,65,103,117,97,114,
		100,101,32,46,46,46,0,72,20,1,36,14,1,176,
		42,0,106,2,49,0,20,1,36,15,1,176,101,0,
		20,0,36,16,1,176,99,0,12,0,29,132,0,36,
		18,1,109,81,0,78,102,0,109,68,0,78,103,0,
		36,20,1,109,69,0,78,83,0,109,70,0,78,104,
		0,36,23,1,109,73,0,78,53,0,176,105,0,109,
		71,0,12,1,78,106,0,109,72,0,78,107,0,36,
		26,1,176,105,0,109,78,0,12,1,78,108,0,109,
		79,0,78,109,0,109,74,0,78,110,0,36,29,1,
		109,80,0,78,91,0,109,75,0,78,111,0,120,78,
		112,0,36,31,1,176,105,0,109,76,0,12,1,78,
		113,0,106,1,0,78,114,0,36,32,1,176,54,0,
		20,0,36,36,1,176,42,0,106,2,50,0,20,1,
		36,37,1,176,99,0,12,0,28,19,36,38,1,109,
		81,0,78,102,0,36,39,1,176,54,0,20,0,36,
		41,1,176,42,0,106,2,50,0,20,1,36,42,1,
		176,98,0,122,20,1,26,86,251,36,44,1,176,5,
		0,106,3,48,49,0,20,1,36,45,1,176,56,0,
		106,26,80,114,111,99,101,115,115,97,109,101,110,116,
		111,32,67,111,110,99,108,117,161,100,111,32,33,0,
		92,3,20,2,36,47,1,176,115,0,20,0,176,42,
		0,106,2,49,0,20,1,176,116,0,100,20,1,36,
		48,1,176,40,0,121,121,176,2,0,12,0,176,3,
		0,12,0,109,4,0,20,5,36,49,1,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_INDIIMP2 )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,51,67,58,47,115,105,115,47,83,65,78,
		47,120,104,98,47,105,109,112,95,99,116,114,99,46,
		112,114,103,58,70,95,73,78,68,73,73,77,80,50,
		0,71,118,0,1,71,119,0,2,36,54,1,176,32,
		0,108,120,108,121,20,2,36,55,1,176,38,0,12,
		0,83,121,0,36,56,1,122,83,120,0,36,57,1,
		109,118,0,92,4,8,28,23,36,59,1,109,121,0,
		92,27,8,28,12,36,60,1,121,83,120,0,36,61,
		1,36,63,1,109,120,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,105,109,112,95,99,116,114,99,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,40,192,255,255,251,255,157,
		199,255,255,251,255,254,255,255,247,255,251,63,254,255,
		255,255,223,255,95,0,240,159,127,254,241,248,241,149,
		164,241,182,195,187,0,4,3,0,4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}
