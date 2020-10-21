/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/teste.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( TESTE );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
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
HB_FUNC_EXTERN( ALLTRIM );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TESTE )
{ "TESTE", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( TESTE )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ENOME_EMPRE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "ECLASSE_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
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
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TESTE, "C:/sis/SAN/xhb/teste.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TESTE
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TESTE )
   #include "hbiniseg.h"
#endif

HB_FUNC( TESTE )
{
	static const HB_BYTE pcode[] =
	{
		36,13,0,176,1,0,92,50,12,1,83,2,0,36,
		14,0,176,3,0,106,2,54,0,20,1,36,15,0,
		176,4,0,9,100,106,9,67,76,65,83,95,80,82,
		79,0,100,100,9,20,6,176,5,0,106,9,67,76,
		65,83,95,80,82,79,0,20,1,36,16,0,176,6,
		0,20,0,36,18,0,121,83,7,0,36,19,0,176,
		8,0,106,3,48,49,0,20,1,36,20,0,176,9,
		0,92,23,121,20,2,176,10,0,106,36,32,32,32,
		80,114,111,100,117,116,111,32,32,32,32,32,111,117,
		32,40,48,41,32,112,97,114,97,32,84,111,100,111,
		115,32,58,32,0,20,1,176,11,0,176,12,0,12,
		0,176,13,0,12,0,122,72,20,2,176,14,0,109,
		15,0,176,16,0,100,106,12,101,67,76,65,83,83,
		69,95,80,82,79,0,106,11,64,82,32,57,57,57,
		57,57,57,57,0,100,100,12,5,20,2,48,17,0,
		176,18,0,109,15,0,12,1,112,0,73,36,21,0,
		176,19,0,109,15,0,100,100,100,100,100,100,20,7,
		4,0,0,83,15,0,109,15,0,73,36,22,0,176,
		3,0,106,2,54,0,20,1,36,23,0,176,20,0,
		109,7,0,100,100,20,3,36,24,0,176,21,0,12,
		0,32,133,0,36,25,0,109,7,0,121,69,28,72,
		36,26,0,176,8,0,106,3,48,49,0,20,1,36,
		27,0,176,22,0,106,27,32,32,80,114,111,100,117,
		116,111,32,32,32,73,110,101,120,105,115,116,101,110,
		116,101,32,32,32,0,92,3,20,2,36,28,0,176,
		3,0,106,2,54,0,20,1,36,29,0,26,224,254,
		36,31,0,176,9,0,92,23,92,48,20,2,176,10,
		0,106,14,32,45,32,69,115,99,114,105,116,111,114,
		105,111,0,20,1,36,32,0,176,3,0,106,2,54,
		0,20,1,36,33,0,25,55,36,36,0,176,9,0,
		92,23,92,48,20,2,176,10,0,106,4,32,45,32,
		0,176,23,0,109,24,0,12,1,72,20,1,36,37,
		0,109,24,0,83,2,0,36,38,0,176,3,0,106,
		2,54,0,20,1,36,41,0,7
	};

	hb_vmExecute( pcode, symbols );
}
