/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/menu_rt2.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MENU_RT2 );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( __ATPROMPT );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( __MENUTO );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( F_ACESSO );
HB_FUNC_EXTERN( REL_BANC );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( F_LACESSO );
HB_FUNC_EXTERN( REL_CCOR );
HB_FUNC_EXTERN( REL_CCTP );
HB_FUNC_EXTERN( REL_CLAR );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MENU_RT2 )
{ "MENU_RT2", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( MENU_RT2 )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "__ATPROMPT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __ATPROMPT )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISOP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MENUTO", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MENUTO )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "F_ACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_ACESSO )}, NULL },
{ "REL_BANC", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_BANC )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REL_CCOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_CCOR )}, NULL },
{ "REL_CCTP", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_CCTP )}, NULL },
{ "REL_CLAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( REL_CLAR )}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MENU_RT2, "C:/sis/SAN/xhb/menu_rt2.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MENU_RT2
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MENU_RT2 )
   #include "hbiniseg.h"
#endif

HB_FUNC( MENU_RT2 )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,101,110,117,95,114,116,50,46,112,114,103,
		58,77,69,78,85,95,82,84,50,0,36,14,0,176,
		1,0,20,0,36,15,0,176,2,0,121,121,176,3,
		0,12,0,176,4,0,12,0,12,4,83,5,0,36,
		16,0,176,6,0,106,3,48,49,0,20,1,36,17,
		0,36,18,0,176,7,0,106,9,87,47,78,44,43,
		87,47,82,0,20,1,36,19,0,176,8,0,106,40,
		77,69,78,85,32,82,69,76,65,84,224,82,73,79,
		83,32,68,65,83,32,84,65,66,69,76,65,83,32,
		68,79,32,67,82,69,67,69,66,69,82,0,20,1,
		36,20,0,176,9,0,92,31,106,3,79,78,0,20,
		2,36,21,0,176,7,0,106,1,0,20,1,36,22,
		0,176,7,0,106,10,87,47,78,44,43,87,47,66,
		71,0,20,1,36,23,0,176,10,0,92,12,92,60,
		92,18,92,79,20,4,176,11,0,92,12,92,60,20,
		2,36,24,0,176,12,0,92,12,92,60,92,18,92,
		79,122,20,5,36,25,0,176,13,0,92,13,92,61,
		106,19,32,49,32,32,32,32,32,66,97,110,99,111,
		115,32,32,32,32,32,0,106,55,32,82,101,108,97,
		116,243,114,105,111,32,100,101,32,84,97,98,101,108,
		97,32,100,101,32,66,97,110,99,111,115,32,100,111,
		32,83,105,115,116,101,109,97,32,32,32,32,32,32,
		32,32,32,32,32,32,32,0,20,4,36,26,0,176,
		13,0,92,14,92,61,106,19,32,50,32,32,32,67,
		111,110,116,97,115,32,67,111,114,114,46,32,0,106,
		55,32,82,101,108,97,116,243,114,105,111,32,100,101,
		32,84,97,98,101,108,97,32,100,101,32,67,111,110,
		116,97,115,32,67,111,114,114,101,110,116,101,115,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,0,
		20,4,36,27,0,176,13,0,92,15,92,61,106,19,
		32,51,32,32,32,32,67,108,105,101,110,116,101,115,
		32,32,32,32,0,106,55,32,82,101,108,97,116,243,
		114,105,111,32,100,101,32,84,97,98,101,108,97,32,
		100,101,32,67,108,105,101,110,116,101,115,32,100,111,
		32,67,111,110,116,97,115,32,97,32,82,101,99,101,
		98,101,114,32,32,0,20,4,36,28,0,176,13,0,
		92,16,92,61,106,19,32,52,32,32,67,108,97,115,
		46,82,101,99,101,105,116,97,115,32,0,106,55,32,
		82,101,108,97,116,243,114,105,111,32,100,101,32,84,
		97,98,101,108,97,32,100,101,32,67,108,97,115,115,
		105,102,105,99,97,231,227,111,32,100,101,32,82,101,
		99,101,105,116,97,115,32,32,32,32,32,0,20,4,
		36,29,0,176,13,0,92,17,92,61,106,19,32,69,
		115,99,32,32,32,32,70,105,109,32,32,32,32,32,
		32,32,0,106,55,32,70,105,109,32,100,101,32,79,
		112,101,114,97,231,227,111,32,100,111,32,77,243,100,
		117,108,111,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,0,20,4,36,30,0,176,14,0,109,15,
		0,100,100,100,100,100,100,20,7,4,0,0,83,15,
		0,109,15,0,73,36,31,0,122,83,16,0,36,32,
		0,176,17,0,89,72,0,1,0,0,0,51,67,58,
		47,115,105,115,47,83,65,78,47,120,104,98,47,109,
		101,110,117,95,114,116,50,46,112,114,103,58,77,69,
		78,85,95,82,84,50,0,37,1,0,95,49,0,176,
		18,0,12,0,121,8,28,7,109,16,0,25,8,95,
		1,165,83,16,0,6,106,6,115,105,115,79,80,0,
		12,2,83,16,0,36,34,0,109,16,0,122,5,29,
		153,0,36,35,0,176,19,0,106,9,68,73,95,82,
		69,95,48,54,0,12,1,28,29,36,36,0,176,20,
		0,20,0,36,37,0,176,1,0,20,0,36,38,0,
		176,1,0,20,0,26,233,252,36,40,0,176,6,0,
		106,3,48,49,0,20,1,36,41,0,176,21,0,106,
		16,65,99,101,115,115,111,32,78,101,103,97,100,111,
		32,33,0,122,20,2,36,42,0,176,22,0,109,23,
		0,106,27,82,69,76,65,84,79,82,73,79,83,32,
		67,80,65,71,65,82,32,45,32,66,65,78,67,79,
		83,0,106,14,65,67,69,83,83,79,32,78,69,71,
		65,68,79,0,20,3,36,43,0,26,131,252,36,44,
		0,109,16,0,92,2,5,29,150,0,36,45,0,176,
		19,0,106,9,68,73,95,82,69,95,48,54,0,12,
		1,28,29,36,46,0,176,24,0,20,0,36,47,0,
		176,1,0,20,0,36,48,0,176,1,0,20,0,26,
		71,252,36,50,0,176,6,0,106,3,48,49,0,20,
		1,36,51,0,176,21,0,106,16,65,99,101,115,115,
		111,32,78,101,103,97,100,111,32,33,0,122,20,2,
		36,52,0,176,22,0,109,23,0,106,24,82,69,76,
		65,84,79,82,73,79,83,32,67,80,65,71,65,82,
		32,45,32,67,47,67,0,106,14,65,67,69,83,83,
		79,32,78,69,71,65,68,79,0,20,3,36,53,0,
		26,228,251,36,54,0,109,16,0,92,3,5,29,155,
		0,36,55,0,176,19,0,106,9,68,73,95,82,69,
		95,48,54,0,12,1,28,29,36,56,0,176,25,0,
		20,0,36,57,0,176,1,0,20,0,36,58,0,176,
		1,0,20,0,26,168,251,36,60,0,176,6,0,106,
		3,48,49,0,20,1,36,61,0,176,21,0,106,16,
		65,99,101,115,115,111,32,78,101,103,97,100,111,32,
		33,0,122,20,2,36,62,0,176,22,0,109,23,0,
		106,29,82,69,76,65,84,79,82,73,79,83,32,67,
		80,65,71,65,82,32,45,32,67,76,73,69,78,84,
		69,83,0,106,14,65,67,69,83,83,79,32,78,69,
		71,65,68,79,0,20,3,36,63,0,26,64,251,36,
		64,0,109,16,0,92,4,5,29,160,0,36,65,0,
		176,19,0,106,9,68,73,95,82,69,95,48,54,0,
		12,1,28,29,36,66,0,176,26,0,20,0,36,67,
		0,176,1,0,20,0,36,68,0,176,1,0,20,0,
		26,4,251,36,70,0,176,6,0,106,3,48,49,0,
		20,1,36,71,0,176,21,0,106,16,65,99,101,115,
		115,111,32,78,101,103,97,100,111,32,33,0,122,20,
		2,36,72,0,176,22,0,109,23,0,106,34,82,69,
		76,65,84,79,82,73,79,83,32,67,80,65,71,65,
		82,32,45,32,67,76,65,83,46,82,69,67,69,73,
		84,65,83,0,106,14,65,67,69,83,83,79,32,78,
		69,71,65,68,79,0,20,3,36,73,0,26,151,250,
		36,74,0,176,27,0,12,0,92,27,5,29,137,250,
		36,75,0,176,1,0,20,0,36,76,0,176,1,0,
		20,0,36,77,0,176,28,0,121,121,176,3,0,12,
		0,176,4,0,12,0,109,5,0,20,5,36,78,0,
		100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,101,110,117,95,114,116,50,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,10,192,255,255,125,255,253,
		247,223,127,0,4,3,0,4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

