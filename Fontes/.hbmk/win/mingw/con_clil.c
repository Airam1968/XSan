/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/con_clil.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( CON_CLIL );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_CON_CLIL )
{ "CON_CLIL", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( CON_CLIL )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_CON_CLIL, "C:/sis/SAN/xhb/con_clil.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_CON_CLIL
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_CON_CLIL )
   #include "hbiniseg.h"
#endif

HB_FUNC( CON_CLIL )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,33,67,79,78,83,85,76,84,65,32,68,69,
		32,67,76,73,69,78,84,69,83,32,67,65,68,65,
		83,84,82,65,68,79,83,0,20,1,36,16,0,176,
		6,0,106,3,48,49,0,20,1,36,17,0,176,7,
		0,106,46,68,101,115,101,106,97,32,67,111,110,115,
		117,108,116,97,114,32,101,109,32,79,114,100,101,109,
		32,65,108,102,97,98,130,116,105,99,97,32,40,83,
		47,78,41,32,63,32,0,20,1,36,18,0,109,8,
		0,106,2,83,0,5,28,78,36,19,0,176,9,0,
		106,2,49,0,20,1,36,20,0,176,10,0,9,100,
		106,8,67,79,78,84,65,84,79,0,100,100,9,20,
		6,176,11,0,106,8,67,79,78,84,65,84,79,0,
		20,1,176,11,0,106,9,78,85,77,95,67,76,73,
		69,0,20,1,36,21,0,176,12,0,20,0,25,76,
		36,23,0,176,9,0,106,2,49,0,20,1,36,24,
		0,176,10,0,9,100,106,8,67,79,78,84,65,84,
		79,0,100,100,9,20,6,176,11,0,106,9,78,85,
		77,95,67,76,73,69,0,20,1,176,11,0,106,8,
		67,79,78,84,65,84,79,0,20,1,36,25,0,176,
		12,0,20,0,36,27,0,176,6,0,106,3,49,49,
		0,20,1,36,28,0,176,9,0,106,2,49,0,20,
		1,36,29,0,176,12,0,20,0,36,30,0,176,13,
		0,92,6,121,92,20,92,79,92,2,20,5,36,31,
		0,92,6,3,1,0,92,6,3,1,0,92,6,3,
		1,0,92,6,3,1,0,176,18,0,108,14,108,15,
		108,16,108,17,20,4,81,17,0,81,16,0,81,15,
		0,81,14,0,36,32,0,176,19,0,109,14,0,120,
		109,17,0,20,3,36,33,0,106,9,78,85,77,95,
		67,76,73,69,0,98,14,0,122,2,36,34,0,92,
		10,98,17,0,122,2,36,35,0,106,8,67,76,73,
		69,78,84,69,0,98,14,0,92,2,2,36,36,0,
		92,50,98,17,0,92,2,2,36,37,0,106,8,67,
		80,70,95,67,71,67,0,98,14,0,92,3,2,36,
		38,0,92,18,98,17,0,92,3,2,36,39,0,106,
		10,77,65,73,76,95,80,82,79,80,0,98,14,0,
		92,4,2,36,40,0,92,50,98,17,0,92,4,2,
		36,41,0,106,7,77,65,73,76,95,67,0,98,14,
		0,92,5,2,36,42,0,92,50,98,17,0,92,5,
		2,36,43,0,106,10,77,65,73,76,95,67,79,78,
		74,0,98,14,0,92,6,2,36,44,0,92,50,98,
		17,0,92,6,2,36,45,0,106,9,64,82,32,57,
		57,57,57,57,0,98,15,0,122,2,36,46,0,106,
		5,64,83,53,48,0,98,15,0,92,2,2,36,47,
		0,106,5,64,83,50,48,0,98,15,0,92,3,2,
		36,48,0,106,5,64,83,53,48,0,98,15,0,92,
		4,2,36,49,0,106,5,64,83,53,48,0,98,15,
		0,92,5,2,36,50,0,106,5,64,83,53,48,0,
		98,15,0,92,6,2,36,51,0,106,7,67,79,68,
		73,71,79,0,98,16,0,122,2,36,52,0,106,5,
		78,79,77,69,0,98,16,0,92,2,2,36,53,0,
		106,8,67,80,70,47,67,71,67,0,98,16,0,92,
		3,2,36,54,0,106,27,69,77,65,73,76,32,80,
		82,79,80,82,73,69,84,181,82,73,79,32,80,69,
		83,83,79,65,76,0,98,16,0,92,4,2,36,55,
		0,106,29,69,77,65,73,76,32,80,82,79,80,82,
		73,69,84,181,82,73,79,32,67,79,77,69,82,67,
		73,65,76,0,98,16,0,92,5,2,36,56,0,106,
		27,69,77,65,73,76,32,80,82,79,80,82,73,69,
		84,181,82,73,79,32,67,79,78,74,85,71,69,0,
		98,16,0,92,6,2,36,58,0,176,20,0,92,7,
		122,92,20,92,78,109,14,0,106,9,70,85,78,67,
		95,78,79,77,0,109,15,0,109,16,0,106,2,196,
		0,106,2,179,0,106,2,205,0,20,11,36,59,0,
		176,21,0,20,0,36,60,0,176,22,0,121,121,176,
		2,0,12,0,176,3,0,12,0,109,4,0,20,5,
		36,61,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}
