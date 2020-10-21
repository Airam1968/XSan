/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/menu_rcm.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MENU_RCM );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
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
HB_FUNC_EXTERN( REL_PECO );
HB_FUNC_EXTERN( REL_COTA );
HB_FUNC_EXTERN( REL_PEDC );
HB_FUNC_EXTERN( REL_PDC2 );
HB_FUNC_EXTERN( REL_FORN );
HB_FUNC_EXTERN( REL_TIPO );
HB_FUNC_EXTERN( REL_ITEM );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MENU_RCM )
{ "MENU_RCM", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( MENU_RCM )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "REL_PECO", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_PECO )}, NULL },
{ "REL_COTA", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_COTA )}, NULL },
{ "REL_PEDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_PEDC )}, NULL },
{ "REL_PDC2", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_PDC2 )}, NULL },
{ "REL_FORN", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_FORN )}, NULL },
{ "REL_TIPO", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_TIPO )}, NULL },
{ "REL_ITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_ITEM )}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MENU_RCM, "C:/sis/SAN/xhb/menu_rcm.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MENU_RCM
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MENU_RCM )
   #include "hbiniseg.h"
#endif

HB_FUNC( MENU_RCM )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,101,110,117,95,114,99,109,46,112,114,103,
		58,77,69,78,85,95,82,67,77,0,36,14,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,15,0,36,16,0,176,5,0,
		106,25,77,69,78,85,32,82,69,76,65,84,79,82,
		73,79,83,32,67,79,84,65,128,229,69,83,0,20,
		1,36,17,0,176,6,0,92,31,106,3,79,78,0,
		20,2,36,18,0,176,7,0,106,1,0,20,1,36,
		19,0,176,7,0,106,10,87,47,78,44,43,87,47,
		66,71,0,20,1,36,20,0,176,8,0,92,11,92,
		50,92,20,92,70,20,4,176,9,0,92,11,92,50,
		20,2,36,21,0,176,10,0,92,11,92,50,92,20,
		92,70,122,20,5,36,22,0,176,11,0,92,12,92,
		51,106,20,32,49,32,80,101,100,105,100,111,32,67,
		111,116,97,135,198,111,32,32,0,106,47,32,69,109,
		105,115,115,97,111,32,100,101,32,82,101,108,97,116,
		162,114,105,111,32,100,101,32,80,101,100,105,100,111,
		32,100,101,32,67,111,116,97,135,198,111,32,32,32,
		32,0,20,4,36,23,0,176,11,0,92,13,92,51,
		106,20,32,50,32,32,32,32,32,67,111,116,97,135,
		198,111,32,32,32,32,32,0,106,47,32,69,109,105,
		115,115,198,111,32,100,101,32,67,111,116,97,135,198,
		111,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		0,20,4,36,24,0,176,11,0,92,14,92,51,106,
		20,32,51,32,32,80,101,100,46,32,67,111,109,112,
		114,97,32,49,32,32,0,106,47,32,69,109,105,115,
		115,198,111,32,100,101,32,80,101,100,105,100,111,115,
		32,100,101,32,67,111,109,112,114,97,32,45,32,77,
		111,100,101,108,111,32,49,32,32,32,32,32,32,0,
		20,4,36,25,0,176,11,0,92,15,92,51,106,20,
		32,52,32,32,80,101,100,46,32,67,111,109,112,114,
		97,32,50,32,32,0,106,47,32,69,109,105,115,115,
		198,111,32,100,101,32,80,101,100,105,100,111,115,32,
		100,101,32,67,111,109,112,114,97,32,45,32,77,111,
		100,101,108,111,32,50,32,32,32,32,32,32,0,20,
		4,36,26,0,176,11,0,92,16,92,51,106,20,32,
		53,32,32,32,70,111,114,110,101,99,101,100,111,114,
		101,115,32,32,0,106,47,32,69,109,105,115,115,198,
		111,32,100,101,32,67,97,100,97,115,116,114,111,32,
		100,101,32,70,111,114,110,101,99,101,100,111,114,101,
		115,32,32,32,32,32,32,32,32,32,32,0,20,4,
		36,27,0,176,11,0,92,17,92,51,106,20,32,54,
		32,32,32,32,32,32,84,105,112,111,32,32,32,32,
		32,32,32,0,106,47,32,69,109,105,115,115,198,111,
		32,100,101,32,67,97,100,97,115,116,114,111,32,100,
		101,32,84,105,112,111,32,100,101,32,80,114,111,100,
		117,116,111,115,32,32,32,32,32,32,0,20,4,36,
		28,0,176,11,0,92,18,92,51,106,20,32,55,32,
		32,32,32,32,32,73,116,101,110,115,32,32,32,32,
		32,32,0,106,47,32,69,109,105,115,115,198,111,32,
		100,101,32,67,97,100,97,115,116,114,111,32,100,101,
		32,80,114,111,100,117,116,111,115,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,0,20,4,36,29,
		0,176,11,0,92,19,92,51,106,20,32,69,115,99,
		32,32,32,32,32,70,105,109,32,32,32,32,32,32,
		32,0,106,47,32,70,105,109,32,100,101,32,79,112,
		101,114,97,135,198,111,32,100,111,32,77,162,100,117,
		108,111,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,0,20,4,36,30,0,
		176,12,0,109,13,0,100,100,100,100,100,100,20,7,
		4,0,0,83,13,0,109,13,0,73,36,31,0,122,
		83,14,0,36,32,0,176,15,0,89,72,0,1,0,
		0,0,51,67,58,47,115,105,115,47,83,65,78,47,
		120,104,98,47,109,101,110,117,95,114,99,109,46,112,
		114,103,58,77,69,78,85,95,82,67,77,0,37,1,
		0,95,49,0,176,16,0,12,0,121,8,28,7,109,
		14,0,25,8,95,1,165,83,14,0,6,106,6,115,
		105,115,79,80,0,12,2,83,14,0,36,33,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,35,0,109,14,0,122,5,28,
		13,36,36,0,176,17,0,20,0,26,165,0,36,37,
		0,109,14,0,92,2,5,28,13,36,38,0,176,18,
		0,20,0,26,143,0,36,39,0,109,14,0,92,3,
		5,28,12,36,40,0,176,19,0,20,0,25,121,36,
		41,0,109,14,0,92,4,5,28,12,36,42,0,176,
		20,0,20,0,25,100,36,43,0,109,14,0,92,5,
		5,28,12,36,44,0,176,21,0,20,0,25,79,36,
		45,0,109,14,0,92,6,5,28,12,36,46,0,176,
		22,0,20,0,25,58,36,47,0,109,14,0,92,7,
		5,28,12,36,48,0,176,23,0,20,0,25,37,36,
		49,0,176,24,0,12,0,92,27,5,28,24,36,50,
		0,176,25,0,20,0,36,51,0,176,25,0,20,0,
		36,52,0,100,110,7,36,54,0,176,26,0,121,121,
		176,2,0,12,0,176,3,0,12,0,109,4,0,20,
		5,26,136,251,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,101,110,117,95,114,99,109,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,7,192,255,255,251,255,95,
		0,4,3,0,4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

