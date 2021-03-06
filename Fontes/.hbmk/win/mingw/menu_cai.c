/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/menu_cai.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MENU_CAI );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( __ATPROMPT );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( __MENUTO );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( MENU_COC );
HB_FUNC_EXTERN( MENU_REC );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MENU_CAI )
{ "MENU_CAI", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( MENU_CAI )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "__ATPROMPT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __ATPROMPT )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISOP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MENUTO", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MENUTO )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MENU_COC", {HB_FS_PUBLIC}, {HB_FUNCNAME( MENU_COC )}, NULL },
{ "MENU_REC", {HB_FS_PUBLIC}, {HB_FUNCNAME( MENU_REC )}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MENU_CAI, "C:/sis/SAN/xhb/menu_cai.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MENU_CAI
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MENU_CAI )
   #include "hbiniseg.h"
#endif

HB_FUNC( MENU_CAI )
{
	static const HB_BYTE pcode[] =
	{
		36,15,0,176,1,0,20,0,36,16,0,176,2,0,
		106,20,77,69,78,85,32,67,65,73,88,65,32,69,
		32,66,65,78,67,79,83,0,20,1,36,17,0,176,
		3,0,92,31,106,3,79,78,0,20,2,36,18,0,
		176,4,0,106,1,0,20,1,36,19,0,176,4,0,
		106,10,87,47,78,44,43,87,47,66,71,0,20,1,
		36,20,0,176,5,0,92,9,92,40,92,13,92,60,
		20,4,176,6,0,92,9,92,40,20,2,36,21,0,
		176,7,0,92,9,92,40,92,13,92,60,122,20,5,
		36,22,0,176,8,0,92,10,92,41,106,20,32,49,
		32,32,32,32,67,111,110,115,117,108,116,97,115,32,
		32,32,32,0,106,47,32,77,101,110,117,32,100,101,
		32,67,111,110,115,117,108,116,97,115,32,100,101,32,
		67,97,105,120,97,47,66,97,110,99,111,115,32,32,
		32,32,32,32,32,32,32,32,32,32,0,20,4,36,
		23,0,176,8,0,92,11,92,41,106,20,32,50,32,
		32,32,32,82,101,108,97,116,162,114,105,111,115,32,
		32,32,0,106,47,32,77,101,110,117,32,100,101,32,
		82,101,108,97,116,111,114,105,111,115,32,100,101,32,
		67,97,105,120,97,47,66,97,110,99,111,115,32,32,
		32,32,32,32,32,32,32,32,32,0,20,4,36,24,
		0,176,8,0,92,12,92,41,106,20,32,69,115,99,
		32,32,32,32,32,70,105,109,32,32,32,32,32,32,
		32,0,106,47,32,70,105,109,32,100,101,32,79,112,
		101,114,97,135,198,111,32,100,111,32,77,162,100,117,
		108,111,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,0,20,4,36,25,0,
		176,9,0,109,10,0,100,100,100,100,100,100,20,7,
		4,0,0,83,10,0,109,10,0,73,36,26,0,122,
		83,11,0,36,27,0,176,12,0,89,28,0,1,0,
		0,0,176,13,0,12,0,121,8,28,7,109,11,0,
		25,8,95,1,165,83,11,0,6,106,6,115,105,115,
		79,80,0,12,2,83,11,0,36,28,0,176,14,0,
		121,121,176,15,0,12,0,176,16,0,12,0,12,4,
		83,17,0,36,30,0,109,11,0,122,5,28,12,36,
		31,0,176,18,0,20,0,25,58,36,32,0,109,11,
		0,92,2,5,28,12,36,33,0,176,19,0,20,0,
		25,37,36,34,0,176,20,0,12,0,92,27,5,28,
		24,36,35,0,176,1,0,20,0,36,36,0,176,1,
		0,20,0,36,37,0,100,110,7,36,39,0,176,21,
		0,121,121,176,15,0,12,0,176,16,0,12,0,109,
		17,0,20,5,26,190,253,7
	};

	hb_vmExecute( pcode, symbols );
}

