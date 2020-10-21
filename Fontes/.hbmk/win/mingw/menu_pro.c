/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/menu_pro.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MENU_PRO );
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
HB_FUNC_EXTERN( INC_PROD );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( F_LACESSO );
HB_FUNC_EXTERN( ALT_PROD );
HB_FUNC_EXTERN( EXC_PROD );
HB_FUNC_EXTERN( MENU_CPR );
HB_FUNC_EXTERN( MENU_RPR );
HB_FUNC_EXTERN( MENU_TPR );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MENU_PRO )
{ "MENU_PRO", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( MENU_PRO )}, NULL },
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
{ "TELA01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "F_ACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_ACESSO )}, NULL },
{ "INC_PROD", {HB_FS_PUBLIC}, {HB_FUNCNAME( INC_PROD )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALT_PROD", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALT_PROD )}, NULL },
{ "EXC_PROD", {HB_FS_PUBLIC}, {HB_FUNCNAME( EXC_PROD )}, NULL },
{ "MENU_CPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MENU_CPR )}, NULL },
{ "MENU_RPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MENU_RPR )}, NULL },
{ "MENU_TPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MENU_TPR )}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MENU_PRO, "C:/sis/SAN/xhb/menu_pro.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MENU_PRO
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MENU_PRO )
   #include "hbiniseg.h"
#endif

HB_FUNC( MENU_PRO )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,20,0,36,15,0,176,2,0,
		121,121,176,3,0,12,0,176,4,0,12,0,12,4,
		83,5,0,36,16,0,176,6,0,106,3,48,49,0,
		20,1,36,18,0,176,7,0,106,9,87,47,78,44,
		43,87,47,82,0,20,1,36,19,0,176,8,0,106,
		14,77,69,78,85,32,80,82,79,68,85,84,79,83,
		0,20,1,36,20,0,176,9,0,92,31,106,3,79,
		78,0,20,2,36,21,0,176,7,0,106,1,0,20,
		1,36,22,0,176,7,0,106,10,87,47,78,44,43,
		87,47,66,71,0,20,1,36,23,0,176,10,0,92,
		9,92,40,92,17,92,60,20,4,176,11,0,92,9,
		92,40,20,2,36,24,0,176,12,0,92,9,92,40,
		92,17,92,60,122,20,5,36,25,0,176,13,0,92,
		10,92,41,106,20,32,49,32,32,32,32,73,110,99,
		108,117,115,198,111,32,32,32,32,32,0,106,47,32,
		73,110,99,108,117,115,198,111,32,100,101,32,80,114,
		111,100,117,116,111,115,32,112,97,114,97,32,86,101,
		110,100,97,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,0,20,4,36,26,0,176,13,0,92,11,
		92,41,106,20,32,50,32,32,32,32,65,108,116,101,
		114,97,135,198,111,32,32,32,32,0,106,47,32,65,
		108,116,101,114,97,135,198,111,32,100,101,32,80,114,
		111,100,117,116,111,115,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,0,20,4,36,27,0,176,13,0,92,12,92,
		41,106,20,32,51,32,32,32,32,69,120,99,108,117,
		115,198,111,32,32,32,32,32,0,106,47,32,69,120,
		99,108,117,115,198,111,32,100,101,32,80,114,111,100,
		117,116,111,115,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,0,20,4,36,28,0,176,13,0,92,13,92,41,
		106,20,32,52,32,32,32,32,67,111,110,115,117,108,
		116,97,115,32,32,32,32,0,106,47,32,77,101,110,
		117,32,100,101,32,67,111,110,115,117,108,116,97,115,
		32,97,32,80,114,111,100,117,116,111,115,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		0,20,4,36,29,0,176,13,0,92,14,92,41,106,
		20,32,53,32,32,32,82,101,108,97,116,162,114,105,
		111,115,32,32,32,32,0,106,47,32,77,101,110,117,
		32,69,109,105,115,115,198,111,32,100,101,32,82,101,
		108,97,116,162,114,105,111,115,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,0,
		20,4,36,30,0,176,13,0,92,15,92,41,106,20,
		32,54,32,32,32,32,84,97,98,101,108,97,115,32,
		32,32,32,32,32,0,106,47,32,77,101,110,117,32,
		84,97,98,101,108,97,115,32,100,101,32,73,110,102,
		111,114,109,97,135,228,101,115,32,100,101,32,80,114,
		111,100,117,116,111,115,32,32,32,32,32,32,0,20,
		4,36,31,0,176,13,0,92,16,92,41,106,20,32,
		69,115,99,32,32,32,32,32,70,105,109,32,32,32,
		32,32,32,32,0,106,47,32,70,105,109,32,100,101,
		32,79,112,101,114,97,135,198,111,32,100,111,32,77,
		162,100,117,108,111,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,0,20,4,
		36,32,0,176,14,0,109,15,0,100,100,100,100,100,
		100,20,7,4,0,0,83,15,0,109,15,0,73,36,
		33,0,122,83,16,0,36,34,0,176,17,0,89,28,
		0,1,0,0,0,176,18,0,12,0,121,8,28,7,
		109,16,0,25,8,95,1,165,83,16,0,6,106,6,
		115,105,115,79,80,0,12,2,83,16,0,36,35,0,
		176,2,0,121,121,176,3,0,12,0,176,4,0,12,
		0,12,4,83,19,0,36,37,0,109,16,0,122,5,
		29,131,0,36,38,0,176,20,0,106,9,68,73,95,
		67,65,95,48,50,0,12,1,28,13,36,39,0,176,
		21,0,20,0,26,221,1,36,41,0,176,6,0,106,
		3,48,49,0,20,1,36,42,0,176,22,0,106,16,
		65,99,101,115,115,111,32,78,101,103,97,100,111,32,
		33,0,122,20,2,36,43,0,176,23,0,109,24,0,
		106,21,67,65,68,65,83,84,82,79,32,68,69,32,
		80,82,79,68,85,84,79,83,0,106,14,65,67,69,
		83,83,79,32,78,69,71,65,68,79,0,20,3,36,
		44,0,26,125,1,36,45,0,109,16,0,92,2,5,
		29,132,0,36,46,0,176,20,0,106,9,68,73,95,
		65,76,95,48,50,0,12,1,28,13,36,47,0,176,
		25,0,20,0,26,81,1,36,49,0,176,6,0,106,
		3,48,49,0,20,1,36,50,0,176,22,0,106,16,
		65,99,101,115,115,111,32,78,101,103,97,100,111,32,
		33,0,122,20,2,36,51,0,176,23,0,109,24,0,
		106,22,65,76,84,69,82,65,67,65,79,32,68,69,
		32,80,82,79,68,85,84,79,83,0,106,14,65,67,
		69,83,83,79,32,78,69,71,65,68,79,0,20,3,
		36,52,0,26,240,0,36,53,0,109,16,0,92,3,
		5,29,130,0,36,54,0,176,20,0,106,9,68,73,
		95,69,88,95,48,50,0,12,1,28,13,36,55,0,
		176,26,0,20,0,26,196,0,36,57,0,176,6,0,
		106,3,48,49,0,20,1,36,58,0,176,22,0,106,
		16,65,99,101,115,115,111,32,78,101,103,97,100,111,
		32,33,0,122,20,2,36,59,0,176,23,0,109,24,
		0,106,21,69,88,67,76,85,83,65,79,32,68,69,
		32,80,82,79,68,85,84,79,83,0,106,14,65,67,
		69,83,83,79,32,78,69,71,65,68,79,0,20,3,
		36,60,0,25,100,36,61,0,109,16,0,92,4,5,
		28,12,36,62,0,176,27,0,20,0,25,79,36,63,
		0,109,16,0,92,5,5,28,12,36,64,0,176,28,
		0,20,0,25,58,36,65,0,109,16,0,92,6,5,
		28,12,36,66,0,176,29,0,20,0,25,37,36,67,
		0,176,30,0,12,0,92,27,5,28,24,36,68,0,
		176,1,0,20,0,36,69,0,176,1,0,20,0,36,
		70,0,100,110,7,36,72,0,176,31,0,121,121,176,
		3,0,12,0,176,4,0,12,0,109,19,0,20,5,
		26,180,250,7
	};

	hb_vmExecute( pcode, symbols );
}

