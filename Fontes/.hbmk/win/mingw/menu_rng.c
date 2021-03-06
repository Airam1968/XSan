/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/menu_rng.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MENU_RNG );
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
HB_FUNC_EXTERN( CORRECAO );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( F_LACESSO );
HB_FUNC_EXTERN( QUI_IMOV );
HB_FUNC_EXTERN( PER_IMOV );
HB_FUNC_EXTERN( RNG_IMOV );
HB_FUNC_EXTERN( DES_IMOV );
HB_FUNC_EXTERN( PRO_IMOV );
HB_FUNC_EXTERN( IND_IMOV );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MENU_RNG )
{ "MENU_RNG", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( MENU_RNG )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "CORRECAO", {HB_FS_PUBLIC}, {HB_FUNCNAME( CORRECAO )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUI_IMOV", {HB_FS_PUBLIC}, {HB_FUNCNAME( QUI_IMOV )}, NULL },
{ "PER_IMOV", {HB_FS_PUBLIC}, {HB_FUNCNAME( PER_IMOV )}, NULL },
{ "RNG_IMOV", {HB_FS_PUBLIC}, {HB_FUNCNAME( RNG_IMOV )}, NULL },
{ "DES_IMOV", {HB_FS_PUBLIC}, {HB_FUNCNAME( DES_IMOV )}, NULL },
{ "PRO_IMOV", {HB_FS_PUBLIC}, {HB_FUNCNAME( PRO_IMOV )}, NULL },
{ "IND_IMOV", {HB_FS_PUBLIC}, {HB_FUNCNAME( IND_IMOV )}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MENU_RNG, "C:/sis/SAN/xhb/menu_rng.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MENU_RNG
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MENU_RNG )
   #include "hbiniseg.h"
#endif

HB_FUNC( MENU_RNG )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,20,0,36,15,0,176,2,0,
		121,121,176,3,0,12,0,176,4,0,12,0,12,4,
		83,5,0,36,16,0,176,6,0,106,3,48,49,0,
		20,1,36,18,0,176,7,0,106,9,87,47,78,44,
		43,87,47,82,0,20,1,36,19,0,176,8,0,106,
		26,77,69,78,85,32,68,69,32,82,79,84,73,78,
		65,83,32,69,83,80,69,67,73,65,73,83,0,20,
		1,36,20,0,176,9,0,92,31,106,3,79,78,0,
		20,2,36,21,0,176,7,0,106,1,0,20,1,36,
		22,0,176,7,0,106,10,87,47,78,44,43,87,47,
		66,71,0,20,1,36,23,0,176,10,0,92,11,92,
		50,92,20,92,70,20,4,176,11,0,92,11,92,50,
		20,2,36,24,0,176,12,0,92,11,92,50,92,20,
		92,70,122,20,5,36,25,0,176,13,0,92,12,92,
		51,106,20,32,49,32,32,32,32,32,67,111,114,114,
		101,135,198,111,32,32,32,32,0,106,50,32,82,111,
		116,105,110,97,32,69,115,112,101,99,105,97,108,32,
		100,101,32,67,111,114,114,101,135,198,111,32,77,111,
		110,101,116,160,114,105,97,32,32,32,32,32,32,32,
		32,32,32,32,0,20,4,36,26,0,176,13,0,92,
		13,92,51,106,20,32,50,32,32,32,32,32,81,117,
		105,116,97,135,198,111,32,32,32,32,0,106,50,32,
		82,111,116,105,110,97,32,69,115,112,101,99,105,97,
		108,32,100,101,32,81,117,105,116,97,135,198,111,32,
		100,111,32,70,105,110,97,110,99,105,97,109,101,110,
		116,111,32,32,32,32,0,20,4,36,27,0,176,13,
		0,92,14,92,51,106,20,32,51,32,32,32,32,32,
		80,101,114,109,117,116,97,115,32,32,32,32,0,106,
		50,32,82,111,116,105,110,97,32,69,115,112,101,99,
		105,97,108,32,100,101,32,80,101,114,109,117,116,97,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,0,20,4,36,28,0,
		176,13,0,92,15,92,51,106,20,32,52,32,32,32,
		82,101,110,101,103,111,99,105,97,135,198,111,32,32,
		0,106,50,32,82,111,116,105,110,97,32,69,115,112,
		101,99,105,97,108,32,100,101,32,82,101,110,101,103,
		111,99,105,97,135,198,111,32,100,111,32,70,105,110,
		97,110,99,105,97,109,101,110,116,111,0,20,4,36,
		29,0,176,13,0,92,16,92,51,106,20,32,53,32,
		32,32,32,68,101,115,105,115,116,136,110,99,105,97,
		32,32,0,106,50,32,82,111,116,105,110,97,32,69,
		115,112,101,99,105,97,108,32,100,101,32,68,101,115,
		105,115,116,136,110,99,105,97,32,100,111,32,70,105,
		110,97,110,99,105,97,109,101,110,116,111,32,0,20,
		4,36,30,0,176,13,0,92,17,92,51,106,20,32,
		54,32,32,32,80,114,111,112,114,105,101,116,160,114,
		105,111,32,32,0,106,50,32,82,111,116,105,110,97,
		32,69,115,112,101,99,105,97,108,32,100,101,32,84,
		114,97,110,115,102,101,114,136,110,99,105,97,32,100,
		101,32,80,114,111,112,114,105,101,116,160,114,105,111,
		0,20,4,36,31,0,176,13,0,92,18,92,51,106,
		20,32,55,32,84,114,111,99,97,32,73,110,100,101,
		120,97,100,111,114,32,0,106,50,32,82,111,116,105,
		110,97,32,69,115,112,101,99,105,97,108,32,100,101,
		32,77,117,100,97,110,135,97,32,100,101,32,73,110,
		100,101,120,97,100,111,114,32,32,32,32,32,32,32,
		32,32,0,20,4,36,32,0,176,13,0,92,19,92,
		51,106,20,32,69,115,99,32,32,32,32,70,105,109,
		32,32,32,32,32,32,32,32,0,106,50,32,70,105,
		109,32,100,101,32,79,112,101,114,97,135,198,111,32,
		100,111,32,77,162,100,117,108,111,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,0,20,4,36,33,0,176,14,0,109,
		15,0,100,100,100,100,100,100,20,7,4,0,0,83,
		15,0,109,15,0,73,36,34,0,122,83,16,0,36,
		35,0,176,17,0,89,28,0,1,0,0,0,176,18,
		0,12,0,121,8,28,7,109,16,0,25,8,95,1,
		165,83,16,0,6,106,6,115,105,115,79,80,0,12,
		2,83,16,0,36,36,0,176,2,0,121,121,176,3,
		0,12,0,176,4,0,12,0,12,4,83,5,0,36,
		38,0,109,16,0,122,5,29,136,0,36,39,0,176,
		19,0,106,9,68,73,95,65,76,95,48,51,0,12,
		1,28,13,36,40,0,176,20,0,20,0,26,223,3,
		36,42,0,176,6,0,106,3,48,49,0,20,1,36,
		43,0,176,21,0,106,16,65,99,101,115,115,111,32,
		78,101,103,97,100,111,32,33,0,122,20,2,36,44,
		0,176,22,0,109,23,0,106,26,82,79,84,73,78,
		65,32,68,69,32,67,79,82,82,69,67,65,79,32,
		77,69,78,83,65,76,0,106,14,65,67,69,83,83,
		79,32,78,69,71,65,68,79,0,20,3,36,45,0,
		26,122,3,36,46,0,109,16,0,92,2,5,29,129,
		0,36,47,0,176,19,0,106,9,68,73,95,65,76,
		95,48,51,0,12,1,28,13,36,48,0,176,24,0,
		20,0,26,78,3,36,50,0,176,6,0,106,3,48,
		49,0,20,1,36,51,0,176,21,0,106,16,65,99,
		101,115,115,111,32,78,101,103,97,100,111,32,33,0,
		122,20,2,36,52,0,176,22,0,109,23,0,106,19,
		82,79,84,73,78,65,32,68,69,32,81,85,73,84,
		65,67,65,79,0,106,14,65,67,69,83,83,79,32,
		78,69,71,65,68,79,0,20,3,36,53,0,26,240,
		2,36,54,0,109,16,0,92,3,5,29,138,0,36,
		55,0,176,19,0,106,9,68,73,95,65,76,95,48,
		51,0,12,1,28,13,36,56,0,176,25,0,20,0,
		26,196,2,36,58,0,176,6,0,106,3,48,49,0,
		20,1,36,59,0,176,21,0,106,16,65,99,101,115,
		115,111,32,78,101,103,97,100,111,32,33,0,122,20,
		2,36,60,0,176,22,0,109,23,0,106,28,82,79,
		84,73,78,65,32,68,69,32,80,69,82,77,85,84,
		65,32,68,69,32,73,77,79,86,69,76,0,106,14,
		65,67,69,83,83,79,32,78,69,71,65,68,79,0,
		20,3,36,61,0,26,93,2,36,62,0,109,16,0,
		92,4,5,29,133,0,36,63,0,176,19,0,106,9,
		68,73,95,65,76,95,48,51,0,12,1,28,13,36,
		64,0,176,26,0,20,0,26,49,2,36,66,0,176,
		6,0,106,3,48,49,0,20,1,36,67,0,176,21,
		0,106,16,65,99,101,115,115,111,32,78,101,103,97,
		100,111,32,33,0,122,20,2,36,68,0,176,22,0,
		109,23,0,106,23,82,79,84,73,78,65,32,68,69,
		32,82,69,78,69,71,79,67,73,65,67,65,79,0,
		106,14,65,67,69,83,83,79,32,78,69,71,65,68,
		79,0,20,3,36,69,0,26,207,1,36,70,0,109,
		16,0,92,5,5,29,132,0,36,71,0,176,19,0,
		106,9,68,73,95,65,76,95,48,51,0,12,1,28,
		13,36,72,0,176,27,0,20,0,26,163,1,36,74,
		0,176,6,0,106,3,48,49,0,20,1,36,75,0,
		176,21,0,106,16,65,99,101,115,115,111,32,78,101,
		103,97,100,111,32,33,0,122,20,2,36,76,0,176,
		22,0,109,23,0,106,22,82,79,84,73,78,65,32,
		68,69,32,68,69,83,73,83,84,69,78,67,73,65,
		0,106,14,65,67,69,83,83,79,32,78,69,71,65,
		68,79,0,20,3,36,77,0,26,66,1,36,78,0,
		109,16,0,92,6,5,29,128,0,36,79,0,176,19,
		0,106,9,68,73,95,65,76,95,48,51,0,12,1,
		28,13,36,80,0,176,28,0,20,0,26,22,1,36,
		82,0,176,6,0,106,3,48,49,0,20,1,36,83,
		0,176,21,0,106,16,65,99,101,115,115,111,32,78,
		101,103,97,100,111,32,33,0,122,20,2,36,84,0,
		176,22,0,109,23,0,106,18,82,79,84,73,78,65,
		32,68,69,32,80,69,82,77,85,84,65,0,106,14,
		65,67,69,83,83,79,32,78,69,71,65,68,79,0,
		20,3,36,85,0,26,185,0,36,86,0,109,16,0,
		92,7,5,29,138,0,36,87,0,176,19,0,106,9,
		68,73,95,65,76,95,48,51,0,12,1,28,13,36,
		88,0,176,29,0,20,0,26,141,0,36,90,0,176,
		6,0,106,3,48,49,0,20,1,36,91,0,176,21,
		0,106,16,65,99,101,115,115,111,32,78,101,103,97,
		100,111,32,33,0,122,20,2,36,92,0,176,22,0,
		109,23,0,106,29,82,79,84,73,78,65,32,68,69,
		32,84,82,79,67,65,32,68,69,32,73,78,68,69,
		88,65,68,79,82,0,106,14,65,67,69,83,83,79,
		32,78,69,71,65,68,79,0,20,3,36,93,0,25,
		37,36,94,0,176,30,0,12,0,92,27,5,28,24,
		36,95,0,176,1,0,20,0,36,96,0,176,1,0,
		20,0,36,97,0,100,110,7,36,99,0,176,31,0,
		121,121,176,3,0,12,0,176,4,0,12,0,109,5,
		0,20,5,26,59,248,7
	};

	hb_vmExecute( pcode, symbols );
}

