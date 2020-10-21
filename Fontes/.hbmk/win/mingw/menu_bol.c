/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/menu_bol.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MENU_BOL );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( REL_B001 );
HB_FUNC_EXTERN( REL_B008 );
HB_FUNC_EXTERN( REL_B033 );
HB_FUNC_EXTERN( REL_B104 );
HB_FUNC_EXTERN( REL_B151 );
HB_FUNC_EXTERN( REL_B291 );
HB_FUNC_EXTERN( REL_B341 );
HB_FUNC_EXTERN( REL_B353 );
HB_FUNC_EXTERN( REL_B409 );
HB_FUNC_EXTERN( PDESENV );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC( F_BOLE );
HB_FUNC_EXTERN( LASTKEY );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MENU_BOL )
{ "MENU_BOL", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( MENU_BOL )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "COD_BANCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REL_B001", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_B001 )}, NULL },
{ "REL_B008", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_B008 )}, NULL },
{ "REL_B033", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_B033 )}, NULL },
{ "REL_B104", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_B104 )}, NULL },
{ "REL_B151", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_B151 )}, NULL },
{ "REL_B291", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_B291 )}, NULL },
{ "REL_B341", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_B341 )}, NULL },
{ "REL_B353", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_B353 )}, NULL },
{ "REL_B409", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_B409 )}, NULL },
{ "PDESENV", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDESENV )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "F_BOLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_BOLE )}, NULL },
{ "DB_MODE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DB_COLUMN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RET", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "KEY", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MENU_BOL, "C:/sis/SAN/xhb/menu_bol.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MENU_BOL
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MENU_BOL )
   #include "hbiniseg.h"
#endif

HB_FUNC( MENU_BOL )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,27,77,69,78,85,32,68,69,32,69,77,73,
		83,83,199,79,32,68,69,32,66,79,76,69,84,79,
		83,0,20,1,36,16,0,176,6,0,106,2,49,0,
		20,1,36,17,0,176,7,0,9,100,106,7,66,65,
		78,67,79,83,0,100,100,9,20,6,176,8,0,106,
		7,66,65,78,67,79,83,0,20,1,36,18,0,176,
		9,0,20,0,36,19,0,176,10,0,106,9,87,47,
		78,44,43,87,47,82,0,20,1,36,20,0,92,2,
		3,1,0,92,2,3,1,0,92,2,3,1,0,92,
		2,3,1,0,176,15,0,108,11,108,12,108,13,108,
		14,20,4,81,14,0,81,13,0,81,12,0,81,11,
		0,36,21,0,176,16,0,109,11,0,120,109,14,0,
		20,3,36,22,0,106,8,64,82,32,57,57,57,57,
		0,98,12,0,122,2,36,23,0,106,5,64,83,53,
		48,0,98,12,0,92,2,2,36,24,0,106,16,67,
		162,100,105,103,111,32,70,101,98,114,97,98,97,110,
		0,98,13,0,122,2,36,25,0,106,17,78,111,109,
		101,32,73,110,115,116,105,116,117,105,135,198,111,0,
		98,13,0,92,2,2,36,26,0,176,17,0,92,23,
		122,92,23,92,78,109,11,0,106,7,70,95,66,79,
		76,69,0,109,12,0,109,13,0,106,2,32,0,106,
		2,32,0,106,2,32,0,20,11,36,27,0,109,18,
		0,83,19,0,36,29,0,109,19,0,122,5,28,13,
		36,30,0,176,20,0,20,0,26,209,0,36,31,0,
		109,19,0,92,8,5,28,13,36,32,0,176,21,0,
		20,0,26,187,0,36,33,0,109,19,0,92,33,5,
		28,13,36,34,0,176,22,0,20,0,26,165,0,36,
		35,0,109,19,0,92,104,5,28,13,36,36,0,176,
		23,0,20,0,26,143,0,36,37,0,109,19,0,93,
		151,0,5,28,12,36,38,0,176,24,0,20,0,25,
		120,36,39,0,109,19,0,93,35,1,5,28,12,36,
		40,0,176,25,0,20,0,25,98,36,41,0,109,19,
		0,93,85,1,5,28,12,36,42,0,176,26,0,20,
		0,25,76,36,43,0,109,19,0,93,97,1,5,28,
		12,36,44,0,176,27,0,20,0,25,54,36,45,0,
		109,19,0,93,153,1,5,28,12,36,46,0,176,28,
		0,20,0,25,32,36,47,0,109,19,0,93,159,1,
		5,28,12,36,48,0,176,28,0,20,0,25,10,36,
		50,0,176,29,0,20,0,36,52,0,176,30,0,20,
		0,176,6,0,106,2,49,0,20,1,176,31,0,100,
		20,1,36,53,0,176,32,0,121,121,176,2,0,12,
		0,176,3,0,12,0,109,4,0,20,5,36,54,0,
		100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_BOLE )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,71,34,0,1,71,35,0,2,36,59,0,
		176,15,0,108,36,108,37,20,2,36,60,0,176,38,
		0,12,0,83,37,0,36,61,0,122,83,36,0,36,
		62,0,109,34,0,92,4,8,28,20,36,64,0,109,
		37,0,92,13,8,28,9,36,65,0,121,83,36,0,
		36,68,0,109,36,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}
