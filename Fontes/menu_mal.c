/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/menu_mal.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MENU_MAL );
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
HB_FUNC_EXTERN( SEL_DIRE );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( F_LACESSO );
HB_FUNC_EXTERN( SEL_ANIV );
HB_FUNC_EXTERN( SEL_DEVE );
HB_FUNC_EXTERN( SEL_CLIE );
HB_FUNC_EXTERN( CON_NOME );
HB_FUNC_EXTERN( MAL_DIRE );
HB_FUNC_EXTERN( SEL_CART );
HB_FUNC_EXTERN( MAL_CART );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MENU_MAL )
{ "MENU_MAL", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( MENU_MAL )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "SEL_DIRE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SEL_DIRE )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SEL_ANIV", {HB_FS_PUBLIC}, {HB_FUNCNAME( SEL_ANIV )}, NULL },
{ "SEL_DEVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SEL_DEVE )}, NULL },
{ "SEL_CLIE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SEL_CLIE )}, NULL },
{ "CON_NOME", {HB_FS_PUBLIC}, {HB_FUNCNAME( CON_NOME )}, NULL },
{ "MAL_DIRE", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAL_DIRE )}, NULL },
{ "SEL_CART", {HB_FS_PUBLIC}, {HB_FUNCNAME( SEL_CART )}, NULL },
{ "MAL_CART", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAL_CART )}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MENU_MAL, "C:/sis/SAN/xhb/menu_mal.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MENU_MAL
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MENU_MAL )
   #include "hbiniseg.h"
#endif

HB_FUNC( MENU_MAL )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,101,110,117,95,109,97,108,46,112,114,103,
		58,77,69,78,85,95,77,65,76,0,36,14,0,176,
		1,0,20,0,36,15,0,176,2,0,121,121,176,3,
		0,12,0,176,4,0,12,0,12,4,83,5,0,36,
		16,0,176,6,0,106,3,48,49,0,20,1,36,17,
		0,36,18,0,176,7,0,106,9,87,47,78,44,43,
		87,47,82,0,20,1,36,19,0,176,8,0,106,17,
		77,69,78,85,32,77,65,76,65,32,68,73,82,69,
		84,65,0,20,1,36,20,0,176,9,0,92,31,106,
		3,79,78,0,20,2,36,21,0,176,7,0,106,1,
		0,20,1,36,22,0,176,7,0,106,10,87,47,78,
		44,43,87,47,66,71,0,20,1,36,23,0,176,10,
		0,92,11,92,49,92,21,92,70,20,4,176,11,0,
		92,11,92,49,20,2,36,24,0,176,12,0,92,11,
		92,49,92,21,92,70,122,20,5,36,25,0,176,13,
		0,92,12,92,51,106,19,32,49,32,32,83,101,108,
		101,135,198,111,32,78,111,109,101,115,32,0,106,49,
		32,83,101,108,101,135,198,111,32,112,97,114,97,32,
		73,109,112,114,101,115,115,97,111,32,100,101,32,77,
		97,108,97,32,68,105,114,101,116,97,32,111,117,32,
		67,97,114,116,97,32,0,20,4,36,26,0,176,13,
		0,92,13,92,51,106,19,32,50,32,65,110,105,118,
		101,114,115,97,114,105,97,110,116,101,115,0,106,44,
		32,83,101,108,101,135,198,111,32,112,97,114,97,32,
		73,109,112,114,101,115,115,97,111,32,100,101,32,65,
		110,105,118,101,114,115,97,114,105,97,110,116,101,115,
		9,0,20,4,36,27,0,176,13,0,92,14,92,51,
		106,19,32,51,32,32,32,32,68,101,118,101,100,111,
		114,101,115,32,32,32,0,106,39,32,83,101,108,101,
		135,198,111,32,112,97,114,97,32,73,109,112,114,101,
		115,115,97,111,32,100,101,32,68,101,118,101,100,111,
		114,101,115,9,9,0,20,4,36,28,0,176,13,0,
		92,15,92,51,106,19,32,52,32,69,109,112,114,101,
		101,110,100,105,109,101,110,116,111,115,0,106,49,32,
		83,101,108,101,135,198,111,32,112,97,114,97,32,73,
		109,112,114,46,32,100,101,32,67,108,105,101,110,116,
		101,115,32,69,109,112,114,101,101,110,100,105,109,101,
		110,116,111,115,32,0,20,4,36,29,0,176,13,0,
		92,16,92,51,106,19,32,53,32,32,32,32,67,111,
		110,115,117,108,116,97,115,32,32,32,0,106,49,32,
		67,111,110,115,117,108,116,97,32,100,101,32,78,111,
		109,101,115,32,83,101,108,101,99,105,111,110,97,100,
		111,115,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,0,20,4,36,30,0,176,13,0,
		92,17,92,51,106,19,32,54,32,32,32,69,109,105,
		116,105,114,32,77,97,108,97,32,32,0,106,49,32,
		69,109,105,115,115,97,111,32,100,97,32,77,97,108,
		97,32,68,105,114,101,116,97,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,0,20,4,36,31,0,176,13,0,
		92,18,92,51,106,19,32,55,32,83,101,108,101,135,
		198,111,32,67,97,114,116,97,115,32,0,106,42,32,
		83,101,108,101,135,198,111,32,100,97,32,67,97,114,
		116,97,32,80,97,100,114,198,111,32,97,32,115,101,
		114,32,73,109,112,114,101,115,115,97,9,9,0,20,
		4,36,32,0,176,13,0,92,19,92,51,106,19,32,
		56,32,32,69,109,105,116,105,114,32,67,97,114,116,
		97,115,32,0,106,49,32,69,109,105,115,115,97,111,
		32,100,97,32,67,97,114,116,97,32,80,97,100,114,
		198,111,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,0,20,
		4,36,33,0,176,13,0,92,20,92,51,106,19,32,
		69,115,99,32,32,32,32,32,70,105,109,32,32,32,
		32,32,32,0,106,49,32,70,105,109,32,100,101,32,
		79,112,101,114,97,135,198,111,32,100,111,32,77,162,
		100,117,108,111,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,0,20,
		4,36,34,0,176,14,0,109,15,0,100,100,100,100,
		100,100,20,7,4,0,0,83,15,0,109,15,0,73,
		36,35,0,122,83,16,0,36,36,0,176,17,0,89,
		72,0,1,0,0,0,51,67,58,47,115,105,115,47,
		83,65,78,47,120,104,98,47,109,101,110,117,95,109,
		97,108,46,112,114,103,58,77,69,78,85,95,77,65,
		76,0,37,1,0,95,49,0,176,18,0,12,0,121,
		8,28,7,109,16,0,25,8,95,1,165,83,16,0,
		6,106,6,115,105,115,79,80,0,12,2,83,16,0,
		36,37,0,176,2,0,121,121,176,3,0,12,0,176,
		4,0,12,0,12,4,83,5,0,36,39,0,109,16,
		0,122,5,29,136,0,36,40,0,176,19,0,106,9,
		68,73,95,65,76,95,48,49,0,12,1,28,13,36,
		41,0,176,20,0,20,0,26,154,4,36,43,0,176,
		6,0,106,3,48,49,0,20,1,36,44,0,176,21,
		0,106,16,65,99,101,115,115,111,32,78,101,103,97,
		100,111,32,33,0,122,20,2,36,45,0,176,22,0,
		109,23,0,106,26,83,69,76,69,67,65,79,32,78,
		79,77,69,83,32,68,69,32,67,76,73,69,78,84,
		69,83,0,106,14,65,67,69,83,83,79,32,78,69,
		71,65,68,79,0,20,3,36,46,0,26,53,4,36,
		47,0,109,16,0,92,2,5,29,137,0,36,48,0,
		176,19,0,106,9,68,73,95,65,76,95,48,49,0,
		12,1,28,13,36,49,0,176,24,0,20,0,26,9,
		4,36,51,0,176,6,0,106,3,48,49,0,20,1,
		36,52,0,176,21,0,106,16,65,99,101,115,115,111,
		32,78,101,103,97,100,111,32,33,0,122,20,2,36,
		53,0,176,22,0,109,23,0,106,27,83,69,76,69,
		67,65,79,32,68,69,32,65,78,73,86,69,82,83,
		65,82,73,65,78,84,69,83,0,106,14,65,67,69,
		83,83,79,32,78,69,71,65,68,79,0,20,3,36,
		54,0,26,163,3,36,55,0,109,16,0,92,3,5,
		29,131,0,36,56,0,176,19,0,106,9,68,73,95,
		65,76,95,48,49,0,12,1,28,13,36,57,0,176,
		25,0,20,0,26,119,3,36,59,0,176,6,0,106,
		3,48,49,0,20,1,36,60,0,176,21,0,106,16,
		65,99,101,115,115,111,32,78,101,103,97,100,111,32,
		33,0,122,20,2,36,61,0,176,22,0,109,23,0,
		106,21,83,69,76,69,67,65,79,32,68,69,32,68,
		69,86,69,68,79,82,69,83,0,106,14,65,67,69,
		83,83,79,32,78,69,71,65,68,79,0,20,3,36,
		62,0,26,23,3,36,63,0,109,16,0,92,4,5,
		29,143,0,36,64,0,176,19,0,106,9,68,73,95,
		65,76,95,48,49,0,12,1,28,13,36,65,0,176,
		26,0,20,0,26,235,2,36,67,0,176,6,0,106,
		3,48,49,0,20,1,36,68,0,176,21,0,106,16,
		65,99,101,115,115,111,32,78,101,103,97,100,111,32,
		33,0,122,20,2,36,69,0,176,22,0,109,23,0,
		106,33,83,69,76,69,67,65,79,32,67,76,73,69,
		78,84,69,83,32,69,77,80,82,69,69,78,68,73,
		77,69,78,84,79,83,0,106,14,65,67,69,83,83,
		79,32,78,69,71,65,68,79,0,20,3,36,70,0,
		26,127,2,36,71,0,109,16,0,92,5,5,29,141,
		0,36,72,0,176,19,0,106,9,68,73,95,67,79,
		95,48,49,0,12,1,28,13,36,73,0,176,27,0,
		20,0,26,83,2,36,75,0,176,6,0,106,3,48,
		49,0,20,1,36,76,0,176,21,0,106,16,65,99,
		101,115,115,111,32,78,101,103,97,100,111,32,33,0,
		122,20,2,36,77,0,176,22,0,109,23,0,106,31,
		67,79,78,83,85,76,84,65,32,68,69,32,78,79,
		77,69,83,32,83,69,76,69,67,73,79,78,65,68,
		79,83,0,106,14,65,67,69,83,83,79,32,78,69,
		71,65,68,79,0,20,3,36,78,0,26,233,1,36,
		79,0,109,16,0,92,6,5,29,133,0,36,80,0,
		176,19,0,106,9,68,73,95,82,69,95,48,49,0,
		12,1,28,13,36,81,0,176,28,0,20,0,26,189,
		1,36,83,0,176,6,0,106,3,48,49,0,20,1,
		36,84,0,176,21,0,106,16,65,99,101,115,115,111,
		32,78,101,103,97,100,111,32,33,0,122,20,2,36,
		85,0,176,22,0,109,23,0,106,23,69,77,73,83,
		83,65,79,32,68,69,32,77,65,76,65,32,68,73,
		82,69,84,65,0,106,14,65,67,69,83,83,79,32,
		78,69,71,65,68,79,0,20,3,36,86,0,26,91,
		1,36,87,0,109,16,0,92,7,5,29,135,0,36,
		88,0,176,19,0,106,9,68,73,95,65,76,95,48,
		49,0,12,1,28,13,36,89,0,176,29,0,20,0,
		26,47,1,36,91,0,176,6,0,106,3,48,49,0,
		20,1,36,92,0,176,21,0,106,16,65,99,101,115,
		115,111,32,78,101,103,97,100,111,32,33,0,122,20,
		2,36,93,0,176,22,0,109,23,0,106,25,83,69,
		76,69,67,65,79,32,68,69,32,67,65,82,84,65,
		83,32,80,65,68,82,65,79,0,106,14,65,67,69,
		83,83,79,32,78,69,71,65,68,79,0,20,3,36,
		94,0,26,203,0,36,95,0,109,16,0,92,8,5,
		29,133,0,36,96,0,176,19,0,106,9,68,73,95,
		82,69,95,48,49,0,12,1,28,13,36,97,0,176,
		30,0,20,0,26,159,0,36,99,0,176,6,0,106,
		3,48,49,0,20,1,36,100,0,176,21,0,106,16,
		65,99,101,115,115,111,32,78,101,103,97,100,111,32,
		33,0,122,20,2,36,101,0,176,22,0,109,23,0,
		106,24,69,77,73,83,83,65,79,32,68,69,32,67,
		65,82,84,65,32,80,65,68,82,65,79,0,106,14,
		65,67,69,83,83,79,32,78,69,71,65,68,79,0,
		20,3,36,102,0,25,60,36,103,0,176,31,0,12,
		0,92,27,5,28,47,36,104,0,176,1,0,20,0,
		36,105,0,176,1,0,20,0,36,106,0,176,32,0,
		121,121,176,3,0,12,0,176,4,0,12,0,109,5,
		0,20,5,36,107,0,100,110,7,36,109,0,176,32,
		0,121,121,176,3,0,12,0,176,4,0,12,0,109,
		5,0,20,5,26,44,247,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,101,110,117,95,109,97,108,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,14,192,255,255,191,251,251,
		251,251,251,251,251,251,47,0,4,3,0,4,1,0,
		110,7
	};

	hb_vmExecute( pcode, symbols );
}

