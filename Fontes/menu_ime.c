/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/menu_ime.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MENU_IME );
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
HB_FUNC_EXTERN( EXP_PROS );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( F_LACESSO );
HB_FUNC_EXTERN( EXP_CEF1 );
HB_FUNC_EXTERN( EXP_EPIS );
HB_FUNC_EXTERN( MENU_REG );
HB_FUNC_EXTERN( LASTKEY );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MENU_IME )
{ "MENU_IME", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( MENU_IME )}, NULL },
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
{ "TELA04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EXP_PROS", {HB_FS_PUBLIC}, {HB_FUNCNAME( EXP_PROS )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EXP_CEF1", {HB_FS_PUBLIC}, {HB_FUNCNAME( EXP_CEF1 )}, NULL },
{ "EXP_EPIS", {HB_FS_PUBLIC}, {HB_FUNCNAME( EXP_EPIS )}, NULL },
{ "MENU_REG", {HB_FS_PUBLIC}, {HB_FUNCNAME( MENU_REG )}, NULL },
{ "LASTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTKEY )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MENU_IME, "C:/sis/SAN/xhb/menu_ime.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MENU_IME
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MENU_IME )
   #include "hbiniseg.h"
#endif

HB_FUNC( MENU_IME )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,101,110,117,95,105,109,101,46,112,114,103,
		58,77,69,78,85,95,73,77,69,0,36,14,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,15,0,176,5,0,106,3,48,
		49,0,20,1,36,16,0,36,17,0,176,6,0,106,
		9,87,47,78,44,43,87,47,82,0,20,1,36,18,
		0,176,7,0,106,36,77,69,78,85,32,73,77,80,
		79,82,84,65,128,199,79,47,69,88,80,79,82,84,
		65,128,199,79,32,68,69,32,68,65,68,79,83,0,
		20,1,36,19,0,176,8,0,92,31,106,3,79,78,
		0,20,2,36,20,0,176,6,0,106,1,0,20,1,
		36,21,0,176,6,0,106,10,87,47,78,44,43,87,
		47,66,71,0,20,1,36,22,0,176,9,0,92,11,
		92,50,92,17,92,69,20,4,176,10,0,92,11,92,
		50,20,2,36,23,0,176,11,0,92,11,92,50,92,
		17,92,69,122,20,5,36,24,0,176,12,0,92,12,
		92,51,106,19,32,49,32,67,111,110,116,97,98,46,
		45,80,114,111,83,111,102,116,0,106,44,32,69,120,
		112,111,114,116,97,135,198,111,32,112,97,114,97,32,
		67,111,110,116,97,98,105,108,105,100,97,100,101,32,
		80,114,111,115,111,102,116,32,32,32,32,32,0,20,
		4,36,25,0,176,12,0,92,13,92,51,106,19,32,
		50,32,67,97,105,120,97,32,69,99,111,110,111,109,
		105,99,97,0,106,44,32,69,120,112,111,114,116,97,
		135,198,111,32,112,97,114,97,32,67,69,70,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,0,20,4,36,26,0,176,
		12,0,92,14,92,51,106,19,32,51,32,69,120,112,
		111,114,116,97,135,198,111,32,80,73,83,32,0,106,
		44,32,69,120,112,111,114,116,97,135,198,111,32,112,
		97,114,97,32,80,73,83,47,67,79,70,73,78,83,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,0,20,4,36,27,0,176,12,0,92,15,92,
		51,106,19,32,52,32,32,82,101,103,105,115,116,114,
		111,115,32,80,73,83,32,0,106,44,32,77,97,110,
		117,116,101,110,135,198,111,32,82,101,103,105,115,116,
		114,111,115,32,112,97,114,97,32,80,73,83,47,67,
		79,70,73,78,83,32,32,32,32,32,32,0,20,4,
		36,28,0,176,12,0,92,16,92,51,106,19,32,69,
		115,99,32,32,32,32,32,70,105,109,32,32,32,32,
		32,32,0,106,44,32,70,105,109,32,100,101,32,79,
		112,101,114,97,135,198,111,32,100,111,32,77,162,100,
		117,108,111,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,0,20,4,36,29,0,176,13,
		0,109,14,0,100,100,100,100,100,100,20,7,4,0,
		0,83,14,0,109,14,0,73,36,30,0,122,83,15,
		0,36,31,0,176,16,0,89,72,0,1,0,0,0,
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,101,110,117,95,105,109,101,46,112,114,103,
		58,77,69,78,85,95,73,77,69,0,37,1,0,95,
		49,0,176,17,0,12,0,121,8,28,7,109,15,0,
		25,8,95,1,165,83,15,0,6,106,6,115,105,115,
		79,80,0,12,2,83,15,0,36,33,0,109,15,0,
		122,5,29,183,0,36,34,0,176,18,0,106,9,68,
		73,95,67,65,95,48,57,0,12,1,28,59,36,35,
		0,176,1,0,121,121,176,2,0,12,0,176,3,0,
		12,0,12,4,83,19,0,36,36,0,176,20,0,20,
		0,36,37,0,176,21,0,121,121,176,2,0,12,0,
		176,3,0,12,0,109,19,0,20,5,26,6,253,36,
		39,0,176,5,0,106,3,48,49,0,20,1,36,40,
		0,176,22,0,106,16,65,99,101,115,115,111,32,78,
		101,103,97,100,111,32,33,0,122,20,2,36,41,0,
		176,23,0,109,24,0,106,27,77,65,78,85,84,69,
		78,67,65,79,32,69,88,80,79,82,84,46,32,80,
		82,79,83,79,70,84,0,106,14,65,67,69,83,83,
		79,32,78,69,71,65,68,79,0,20,3,36,42,0,
		26,160,252,36,43,0,109,15,0,92,2,5,29,179,
		0,36,44,0,176,18,0,106,9,68,73,95,67,65,
		95,48,57,0,12,1,28,59,36,45,0,176,1,0,
		121,121,176,2,0,12,0,176,3,0,12,0,12,4,
		83,19,0,36,46,0,176,25,0,20,0,36,47,0,
		176,21,0,121,121,176,2,0,12,0,176,3,0,12,
		0,109,19,0,20,5,26,70,252,36,49,0,176,5,
		0,106,3,48,49,0,20,1,36,50,0,176,22,0,
		106,16,65,99,101,115,115,111,32,78,101,103,97,100,
		111,32,33,0,122,20,2,36,51,0,176,23,0,109,
		24,0,106,23,77,65,78,85,84,69,78,67,65,79,
		32,69,88,80,79,82,84,46,32,67,69,70,0,106,
		14,65,67,69,83,83,79,32,78,69,71,65,68,79,
		0,20,3,36,52,0,26,228,251,36,53,0,109,15,
		0,92,3,5,29,186,0,36,54,0,176,18,0,106,
		9,68,73,95,67,65,95,48,57,0,12,1,28,59,
		36,55,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,19,0,36,56,0,176,26,
		0,20,0,36,57,0,176,21,0,121,121,176,2,0,
		12,0,176,3,0,12,0,109,19,0,20,5,26,138,
		251,36,59,0,176,5,0,106,3,48,49,0,20,1,
		36,60,0,176,22,0,106,16,65,99,101,115,115,111,
		32,78,101,103,97,100,111,32,33,0,122,20,2,36,
		61,0,176,23,0,109,24,0,106,30,77,65,78,85,
		84,69,78,67,65,79,32,69,88,80,79,82,84,46,
		32,80,73,83,47,67,79,70,73,78,83,0,106,14,
		65,67,69,83,83,79,32,78,69,71,65,68,79,0,
		20,3,36,62,0,26,33,251,36,63,0,109,15,0,
		92,4,5,29,189,0,36,64,0,176,18,0,106,9,
		68,73,95,67,65,95,48,57,0,12,1,28,59,36,
		65,0,176,1,0,121,121,176,2,0,12,0,176,3,
		0,12,0,12,4,83,19,0,36,66,0,176,27,0,
		20,0,36,67,0,176,21,0,121,121,176,2,0,12,
		0,176,3,0,12,0,109,19,0,20,5,26,199,250,
		36,69,0,176,5,0,106,3,48,49,0,20,1,36,
		70,0,176,22,0,106,16,65,99,101,115,115,111,32,
		78,101,103,97,100,111,32,33,0,122,20,2,36,71,
		0,176,23,0,109,24,0,106,33,77,65,78,85,84,
		69,78,67,65,79,32,82,69,71,73,83,84,82,79,
		46,45,32,80,73,83,47,67,79,70,73,78,83,0,
		106,14,65,67,69,83,83,79,32,78,69,71,65,68,
		79,0,20,3,36,72,0,26,91,250,36,73,0,176,
		28,0,12,0,92,27,5,29,77,250,36,74,0,176,
		29,0,20,0,36,75,0,176,21,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,4,0,20,5,36,
		76,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,109,101,110,117,95,105,109,101,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,10,192,255,255,190,255,254,
		251,239,31,0,4,3,0,4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

