/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/exclui.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( EXCLUI );
HB_FUNC_EXTERN( SAVESCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( __DBLOCATE );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( F_LACESSO );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( DBDELETE );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_EXCLUI )
{ "EXCLUI", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( EXCLUI )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EPESQUISA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "NUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "SEXO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "__DBLOCATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBLOCATE )}, NULL },
{ "PROPRIET_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "DBDELETE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBDELETE )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_EXCLUI, "C:/sis/SAN/xhb/exclui.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_EXCLUI
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_EXCLUI )
   #include "hbiniseg.h"
#endif

HB_FUNC( EXCLUI )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,21,69,88,67,76,85,83,65,79,32,68,69,
		32,67,76,73,69,78,84,69,83,0,20,1,36,16,
		0,176,6,0,106,2,49,0,20,1,36,17,0,176,
		7,0,9,100,106,8,67,79,78,84,65,84,79,0,
		100,100,9,20,6,176,8,0,106,8,67,79,78,84,
		65,84,79,0,20,1,176,8,0,106,9,78,85,77,
		95,67,76,73,69,0,20,1,36,18,0,176,9,0,
		20,0,36,20,0,176,10,0,20,0,36,21,0,176,
		11,0,106,9,87,47,78,44,43,87,47,82,0,20,
		1,36,22,0,176,12,0,92,49,12,1,83,13,0,
		36,23,0,176,14,0,92,5,92,2,20,2,176,15,
		0,106,14,67,108,105,101,110,116,101,32,32,32,32,
		32,58,0,20,1,176,16,0,176,17,0,12,0,176,
		18,0,12,0,122,72,20,2,176,19,0,109,20,0,
		176,21,0,100,106,10,101,80,69,83,81,85,73,83,
		65,0,106,63,64,33,32,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,0,100,100,12,
		5,20,2,48,22,0,176,23,0,109,20,0,12,1,
		112,0,73,36,24,0,176,24,0,109,20,0,100,100,
		100,100,100,100,20,7,4,0,0,83,20,0,109,20,
		0,73,36,25,0,176,25,0,109,13,0,12,1,83,
		13,0,36,26,0,176,26,0,109,13,0,100,100,20,
		3,36,27,0,109,27,0,83,28,0,36,28,0,122,
		3,1,0,122,3,1,0,122,3,1,0,122,3,1,
		0,176,33,0,108,29,108,30,108,31,108,32,20,4,
		81,32,0,81,31,0,81,30,0,81,29,0,36,29,
		0,176,34,0,109,29,0,120,109,32,0,20,3,36,
		30,0,106,5,64,83,53,48,0,98,30,0,122,2,
		36,31,0,106,7,32,32,78,79,77,69,0,98,31,
		0,122,2,36,32,0,176,35,0,92,5,92,15,92,
		5,92,65,109,29,0,106,7,75,69,89,83,95,52,
		0,109,30,0,109,31,0,106,2,32,0,106,2,32,
		0,106,2,32,0,20,11,36,33,0,176,36,0,12,
		0,29,35,3,36,34,0,176,14,0,92,7,92,2,
		20,2,176,15,0,106,17,67,80,70,32,111,117,32,
		67,71,67,32,32,32,32,58,32,0,20,1,176,16,
		0,176,17,0,12,0,176,18,0,12,0,122,72,20,
		2,176,19,0,109,20,0,176,21,0,100,106,8,67,
		80,70,95,67,71,67,0,100,100,100,12,5,20,2,
		48,22,0,176,23,0,109,20,0,12,1,112,0,73,
		36,35,0,176,14,0,92,8,92,2,20,2,176,15,
		0,106,17,82,71,32,32,32,32,32,32,32,32,32,
		32,32,32,58,32,0,20,1,176,16,0,176,17,0,
		12,0,176,18,0,12,0,122,72,20,2,176,19,0,
		109,20,0,176,21,0,100,106,3,82,71,0,100,100,
		100,12,5,20,2,48,22,0,176,23,0,109,20,0,
		12,1,112,0,73,36,36,0,176,14,0,92,8,92,
		44,20,2,176,15,0,106,6,85,70,32,58,32,0,
		20,1,176,16,0,176,17,0,12,0,176,18,0,12,
		0,122,72,20,2,176,19,0,109,20,0,176,21,0,
		100,106,6,85,70,95,82,71,0,106,5,64,33,88,
		88,0,100,100,12,5,20,2,48,22,0,176,23,0,
		109,20,0,12,1,112,0,73,36,37,0,176,14,0,
		92,9,92,2,20,2,176,15,0,106,17,69,109,105,
		115,115,198,111,32,32,32,32,32,32,32,58,32,0,
		20,1,176,16,0,176,17,0,12,0,176,18,0,12,
		0,122,72,20,2,176,19,0,109,20,0,176,21,0,
		100,106,11,69,77,73,83,83,65,79,95,82,71,0,
		100,100,100,12,5,20,2,48,22,0,176,23,0,109,
		20,0,12,1,112,0,73,36,38,0,176,14,0,92,
		9,92,41,20,2,176,15,0,106,9,79,114,103,198,
		111,32,58,32,0,20,1,176,16,0,176,17,0,12,
		0,176,18,0,12,0,122,72,20,2,176,19,0,109,
		20,0,176,21,0,100,106,9,79,82,71,65,79,95,
		82,71,0,100,100,100,12,5,20,2,48,22,0,176,
		23,0,109,20,0,12,1,112,0,73,36,39,0,176,
		14,0,92,10,92,2,20,2,176,15,0,106,17,83,
		101,120,111,32,40,77,47,70,47,79,41,32,32,58,
		32,0,20,1,176,16,0,176,17,0,12,0,176,18,
		0,12,0,122,72,20,2,176,19,0,109,20,0,176,
		21,0,100,106,5,83,69,88,79,0,106,2,88,0,
		90,13,109,37,0,106,4,77,70,79,0,24,6,100,
		12,5,20,2,48,22,0,176,23,0,109,20,0,12,
		1,112,0,73,36,40,0,176,14,0,92,10,92,36,
		20,2,176,15,0,106,14,78,97,115,99,105,109,101,
		110,116,111,32,58,32,0,20,1,176,16,0,176,17,
		0,12,0,176,18,0,12,0,122,72,20,2,176,19,
		0,109,20,0,176,21,0,100,106,11,78,65,83,67,
		73,77,69,78,84,79,0,100,100,100,12,5,20,2,
		48,22,0,176,23,0,109,20,0,12,1,112,0,73,
		36,41,0,176,14,0,92,15,92,2,20,2,176,15,
		0,106,17,78,117,109,46,67,108,105,101,110,116,101,
		32,32,32,58,32,0,20,1,176,16,0,176,17,0,
		12,0,176,18,0,12,0,122,72,20,2,176,19,0,
		109,20,0,176,21,0,100,106,9,78,85,77,95,67,
		76,73,69,0,106,14,64,82,32,57,57,57,57,57,
		57,57,57,57,57,0,100,100,12,5,20,2,48,22,
		0,176,23,0,109,20,0,12,1,112,0,73,36,42,
		0,176,38,0,120,20,1,4,0,0,83,20,0,109,
		20,0,73,36,43,0,109,27,0,83,28,0,36,44,
		0,176,39,0,20,0,36,46,0,176,6,0,106,2,
		49,0,20,1,36,47,0,176,40,0,106,3,48,49,
		0,20,1,36,48,0,176,41,0,106,18,67,111,110,
		102,105,114,109,97,32,40,83,47,78,41,32,63,32,
		0,20,1,36,49,0,109,42,0,106,2,83,0,5,
		29,78,2,36,50,0,109,43,0,106,8,32,32,32,
		32,32,32,32,0,69,29,58,2,36,51,0,176,40,
		0,106,3,48,49,0,20,1,36,52,0,176,44,0,
		106,10,65,116,101,110,135,198,111,32,33,0,92,3,
		20,2,36,53,0,176,45,0,106,54,65,103,117,97,
		114,100,101,44,32,86,101,114,105,102,105,99,97,110,
		100,111,32,115,101,32,111,32,67,108,105,101,110,116,
		101,32,80,111,115,115,117,105,32,85,110,105,100,97,
		100,101,115,32,46,46,46,0,20,1,36,54,0,176,
		6,0,106,2,50,0,20,1,36,55,0,176,7,0,
		9,100,106,9,85,78,73,95,73,77,79,66,0,100,
		100,9,20,6,176,8,0,106,9,85,78,73,95,73,
		77,79,66,0,20,1,36,56,0,176,9,0,20,0,
		36,57,0,176,46,0,90,21,109,47,0,109,28,0,
		5,21,31,10,73,109,48,0,109,28,0,5,6,100,
		100,100,9,20,5,36,58,0,176,49,0,12,0,32,
		211,0,36,59,0,176,40,0,106,3,48,49,0,20,
		1,36,60,0,176,44,0,106,10,65,116,101,110,135,
		198,111,32,33,0,92,3,20,2,36,61,0,176,45,
		0,106,67,67,108,105,101,110,116,101,32,101,109,32,
		113,117,101,115,116,198,111,32,130,32,80,114,111,112,
		114,105,101,116,97,114,105,111,32,100,101,32,85,110,
		105,100,97,100,101,115,44,32,69,120,99,108,117,115,
		198,111,32,67,97,110,99,101,108,97,100,97,46,0,
		20,1,36,62,0,176,50,0,20,0,36,63,0,176,
		51,0,109,52,0,106,18,67,65,68,65,83,84,82,
		79,32,67,76,73,69,78,84,69,83,0,106,35,84,
		69,78,84,65,84,73,86,65,32,68,69,32,69,88,
		67,76,85,83,65,79,32,67,76,73,69,78,84,69,
		32,78,111,46,32,0,176,25,0,176,53,0,109,28,
		0,92,10,121,12,3,12,1,72,20,3,26,159,0,
		36,65,0,176,6,0,106,2,49,0,20,1,36,66,
		0,176,36,0,12,0,28,18,36,67,0,176,54,0,
		20,0,36,68,0,176,39,0,20,0,36,70,0,176,
		51,0,109,52,0,106,18,67,65,68,65,83,84,82,
		79,32,67,76,73,69,78,84,69,83,0,106,22,69,
		88,67,76,85,83,65,79,32,67,76,73,69,78,84,
		69,32,78,111,46,32,0,176,25,0,176,53,0,109,
		28,0,92,10,121,12,3,12,1,72,20,3,36,71,
		0,176,40,0,106,3,48,49,0,20,1,36,72,0,
		176,44,0,106,21,69,120,99,108,117,115,198,111,32,
		67,111,110,99,108,117,161,100,97,32,33,0,92,3,
		20,2,36,80,0,176,55,0,121,121,176,2,0,12,
		0,176,3,0,12,0,109,4,0,20,5,36,81,0,
		100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

