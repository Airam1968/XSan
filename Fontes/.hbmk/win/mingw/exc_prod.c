/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/exc_prod.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( EXC_PROD );
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
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( F_BLOQA );
HB_FUNC_EXTERN( DBEVAL );
HB_FUNC_EXTERN( DBDELETE );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( F_LACESSO );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( __DBPACK );
HB_FUNC_EXTERN( PDESENV );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_EXC_PROD )
{ "EXC_PROD", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( EXC_PROD )}, NULL },
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
{ "ECLAS_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETIPO_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVPOS )}, NULL },
{ "DEVOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUT )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "NUM_LOTEAM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "CADASTRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISGRUPO1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISUNID1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "F_BLOQA", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQA )}, NULL },
{ "DBEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEVAL )}, NULL },
{ "DBDELETE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBDELETE )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "EBLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISGRUPO2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISUNID2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__DBPACK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBPACK )}, NULL },
{ "PDESENV", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDESENV )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_EXC_PROD, "C:/sis/SAN/xhb/exc_prod.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_EXC_PROD
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_EXC_PROD )
   #include "hbiniseg.h"
#endif

HB_FUNC( EXC_PROD )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,21,69,88,67,76,85,83,65,79,32,68,69,
		32,80,82,79,68,85,84,79,83,0,20,1,36,16,
		0,176,6,0,106,2,49,0,20,1,36,17,0,176,
		7,0,9,100,106,9,85,78,73,95,73,77,79,66,
		0,100,100,9,20,6,176,8,0,106,9,85,78,73,
		95,73,77,79,66,0,20,1,36,18,0,176,9,0,
		20,0,36,19,0,176,6,0,106,2,50,0,20,1,
		36,20,0,176,7,0,9,100,106,9,67,76,65,83,
		95,80,82,79,0,100,100,9,20,6,176,8,0,106,
		9,67,76,65,83,95,80,82,79,0,20,1,36,21,
		0,176,9,0,20,0,36,22,0,176,6,0,106,2,
		51,0,20,1,36,23,0,176,7,0,9,100,106,9,
		67,65,68,95,81,85,65,68,0,100,100,9,20,6,
		176,8,0,106,9,67,65,68,95,81,85,65,68,0,
		20,1,36,24,0,176,9,0,20,0,36,25,0,176,
		6,0,106,2,52,0,20,1,36,26,0,176,7,0,
		9,100,106,9,67,65,68,95,65,80,65,82,0,100,
		100,9,20,6,176,8,0,106,9,67,65,68,95,65,
		80,65,82,0,20,1,36,27,0,176,9,0,20,0,
		36,28,0,176,6,0,106,2,53,0,20,1,36,29,
		0,176,7,0,9,100,106,9,67,65,68,95,86,69,
		78,68,0,100,100,9,20,6,176,8,0,106,9,67,
		65,68,95,86,69,78,68,0,20,1,36,30,0,176,
		9,0,20,0,36,31,0,176,6,0,106,2,50,0,
		20,1,36,34,0,176,10,0,20,0,36,35,0,176,
		11,0,106,9,87,47,78,44,43,87,47,82,0,20,
		1,36,36,0,121,83,12,0,36,37,0,121,83,13,
		0,36,38,0,176,14,0,92,5,92,2,20,2,176,
		15,0,106,11,80,114,111,100,117,116,111,32,58,32,
		0,20,1,36,39,0,176,9,0,20,0,36,40,0,
		92,3,3,1,0,92,3,3,1,0,92,3,3,1,
		0,92,3,3,1,0,176,20,0,108,16,108,17,108,
		18,108,19,20,4,81,19,0,81,18,0,81,17,0,
		81,16,0,36,41,0,176,21,0,109,16,0,120,109,
		19,0,20,3,36,42,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,122,2,36,43,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,92,2,2,36,
		44,0,106,5,64,83,52,48,0,98,17,0,92,3,
		2,36,45,0,106,5,32,32,32,32,0,98,18,0,
		122,2,36,46,0,106,5,32,32,32,32,0,98,18,
		0,92,2,2,36,47,0,106,5,32,32,32,32,0,
		98,18,0,92,3,2,36,48,0,176,22,0,92,5,
		92,15,92,5,92,70,109,16,0,106,7,75,69,89,
		83,95,52,0,109,17,0,109,18,0,106,2,32,0,
		106,4,32,45,32,0,106,2,32,0,20,11,36,49,
		0,109,23,0,83,12,0,36,50,0,109,24,0,83,
		13,0,36,55,0,109,13,0,122,5,29,96,4,36,
		57,0,176,6,0,106,2,51,0,20,1,36,58,0,
		176,25,0,90,24,109,26,0,176,27,0,109,12,0,
		92,5,12,2,5,21,28,6,73,109,28,0,6,106,
		45,78,85,77,95,76,79,84,69,65,77,32,61,32,
		83,84,82,40,101,67,76,65,83,95,80,82,79,44,
		53,41,32,46,65,78,68,46,32,67,65,68,65,83,
		84,82,79,0,20,2,36,59,0,176,9,0,20,0,
		36,60,0,121,83,29,0,36,61,0,176,14,0,92,
		6,92,2,20,2,176,15,0,109,30,0,106,4,32,
		58,32,0,72,20,1,36,62,0,176,9,0,20,0,
		36,63,0,92,4,3,1,0,92,4,3,1,0,92,
		4,3,1,0,92,4,3,1,0,176,20,0,108,16,
		108,17,108,18,108,19,20,4,81,19,0,81,18,0,
		81,17,0,81,16,0,36,64,0,176,21,0,109,16,
		0,120,109,19,0,20,3,36,65,0,106,8,64,82,
		57,57,57,57,57,0,98,17,0,122,2,36,66,0,
		106,8,64,82,57,57,57,57,57,0,98,17,0,92,
		2,2,36,67,0,106,8,64,82,57,57,57,57,57,
		0,98,17,0,92,3,2,36,68,0,106,8,64,83,
		57,57,57,57,57,0,98,17,0,92,4,2,36,69,
		0,106,5,32,32,32,32,0,98,18,0,122,2,36,
		70,0,106,5,32,32,32,32,0,98,18,0,92,2,
		2,36,71,0,106,5,32,32,32,32,0,98,18,0,
		92,3,2,36,72,0,106,5,32,32,32,32,0,98,
		18,0,92,4,2,36,73,0,176,22,0,92,6,92,
		14,92,6,92,45,109,16,0,106,7,75,69,89,83,
		95,52,0,109,17,0,109,18,0,106,2,32,0,106,
		4,32,45,32,0,106,2,32,0,20,11,36,74,0,
		109,31,0,83,29,0,36,75,0,109,32,0,83,33,
		0,36,76,0,109,34,0,83,35,0,36,77,0,176,
		6,0,106,2,49,0,20,1,36,78,0,176,25,0,
		90,28,109,36,0,176,27,0,109,12,0,92,5,12,
		2,5,21,28,10,73,109,37,0,109,29,0,5,6,
		106,49,67,76,65,83,83,69,32,61,32,83,84,82,
		40,101,67,76,65,83,95,80,82,79,44,53,41,32,
		46,65,78,68,46,32,81,85,65,68,82,65,32,61,
		32,101,81,85,65,68,82,65,0,20,2,36,79,0,
		176,9,0,20,0,36,80,0,176,38,0,106,3,48,
		49,0,20,1,36,81,0,176,39,0,106,21,67,111,
		110,102,105,114,109,97,32,97,32,69,120,99,108,117,
		115,97,111,32,0,109,40,0,72,106,10,32,40,83,
		47,78,41,32,63,32,0,72,20,1,36,82,0,109,
		41,0,106,2,83,0,5,29,147,10,36,83,0,176,
		6,0,106,2,53,0,20,1,36,84,0,176,25,0,
		90,28,109,36,0,176,27,0,109,12,0,92,5,12,
		2,5,21,28,10,73,109,37,0,109,29,0,5,6,
		106,49,67,76,65,83,83,69,32,61,32,83,84,82,
		40,101,67,76,65,83,95,80,82,79,44,53,41,32,
		46,65,78,68,46,32,81,85,65,68,82,65,32,61,
		32,101,81,85,65,68,82,65,0,20,2,36,85,0,
		176,9,0,20,0,36,86,0,176,42,0,12,0,29,
		60,1,36,87,0,176,6,0,106,2,49,0,20,1,
		36,88,0,176,38,0,106,3,48,49,0,20,1,36,
		89,0,176,43,0,106,39,65,103,117,97,114,100,101,
		32,97,32,69,108,105,109,105,110,97,99,97,111,32,
		100,111,115,32,82,101,103,105,115,116,114,111,115,32,
		46,46,46,0,20,1,36,90,0,176,44,0,12,0,
		28,31,36,91,0,176,45,0,90,8,176,46,0,12,
		0,6,100,100,100,100,9,20,6,36,92,0,176,47,
		0,20,0,36,94,0,176,6,0,106,2,51,0,20,
		1,36,95,0,176,25,0,90,17,109,26,0,176,27,
		0,109,12,0,92,5,12,2,5,6,106,30,78,85,
		77,95,76,79,84,69,65,77,32,61,32,83,84,82,
		40,101,67,76,65,83,95,80,82,79,44,53,41,0,
		20,2,36,96,0,176,48,0,12,0,28,17,36,97,
		0,9,78,28,0,36,98,0,176,47,0,20,0,36,
		100,0,176,49,0,109,50,0,106,18,67,65,68,65,
		83,84,82,79,32,67,76,73,69,78,84,69,83,0,
		106,30,69,88,67,76,85,83,65,79,32,68,79,32,
		80,82,79,68,85,84,79,32,67,76,65,83,83,69,
		32,58,32,0,176,51,0,176,27,0,109,12,0,92,
		5,12,2,12,1,72,106,12,32,81,85,65,47,66,
		76,79,32,58,32,0,72,176,51,0,109,29,0,12,
		1,72,20,3,26,228,8,36,102,0,176,38,0,106,
		3,48,49,0,20,1,36,103,0,176,43,0,106,50,
		69,120,99,108,117,115,198,111,32,66,108,111,113,117,
		101,97,100,97,44,32,69,120,105,115,116,101,109,32,
		118,101,110,100,97,115,32,110,101,115,116,101,32,80,
		114,111,100,117,116,111,32,0,20,1,36,104,0,176,
		52,0,20,0,36,106,0,26,141,8,36,107,0,109,
		13,0,92,2,5,29,50,4,36,109,0,176,6,0,
		106,2,52,0,20,1,36,110,0,176,25,0,90,24,
		109,26,0,176,27,0,109,12,0,92,5,12,2,5,
		21,28,6,73,109,28,0,6,106,45,78,85,77,95,
		76,79,84,69,65,77,32,61,32,83,84,82,40,101,
		67,76,65,83,95,80,82,79,44,53,41,32,46,65,
		78,68,46,32,67,65,68,65,83,84,82,79,0,20,
		2,36,111,0,176,9,0,20,0,36,112,0,121,83,
		53,0,36,113,0,176,14,0,92,6,92,2,20,2,
		176,15,0,109,54,0,106,4,32,58,32,0,72,20,
		1,36,114,0,176,9,0,20,0,36,115,0,92,3,
		3,1,0,92,3,3,1,0,92,3,3,1,0,92,
		3,3,1,0,176,20,0,108,16,108,17,108,18,108,
		19,20,4,81,19,0,81,18,0,81,17,0,81,16,
		0,36,116,0,176,21,0,109,16,0,120,109,19,0,
		20,3,36,117,0,106,8,64,82,57,57,57,57,57,
		0,98,17,0,122,2,36,118,0,106,8,64,82,57,
		57,57,57,57,0,98,17,0,92,2,2,36,119,0,
		106,8,64,82,57,57,57,57,57,0,98,17,0,92,
		3,2,36,120,0,106,5,32,32,32,32,0,98,18,
		0,122,2,36,121,0,106,5,32,32,32,32,0,98,
		18,0,92,2,2,36,122,0,106,5,32,32,32,32,
		0,98,18,0,92,3,2,36,123,0,176,22,0,92,
		6,92,8,92,6,92,76,109,16,0,106,7,75,69,
		89,83,95,52,0,109,17,0,109,18,0,106,2,32,
		0,106,4,32,45,32,0,106,2,32,0,20,11,36,
		124,0,109,55,0,83,53,0,36,125,0,109,56,0,
		83,57,0,36,126,0,176,6,0,106,2,49,0,20,
		1,36,127,0,176,25,0,90,28,109,36,0,176,27,
		0,109,12,0,92,5,12,2,5,21,28,10,73,109,
		37,0,109,53,0,5,6,106,48,67,76,65,83,83,
		69,32,61,32,83,84,82,40,101,67,76,65,83,95,
		80,82,79,44,53,41,32,46,65,78,68,46,32,81,
		85,65,68,82,65,32,61,32,101,66,76,79,67,79,
		0,20,2,36,128,0,176,9,0,20,0,36,129,0,
		176,38,0,106,3,48,49,0,20,1,36,130,0,176,
		39,0,106,21,67,111,110,102,105,114,109,97,32,97,
		32,69,120,99,108,117,115,97,111,32,0,109,58,0,
		72,106,10,32,40,83,47,78,41,32,63,32,0,72,
		20,1,36,131,0,109,41,0,106,2,83,0,5,29,
		87,6,36,132,0,176,6,0,106,2,53,0,20,1,
		36,133,0,176,25,0,90,28,109,36,0,176,27,0,
		109,12,0,92,5,12,2,5,21,28,10,73,109,37,
		0,109,53,0,5,6,106,48,67,76,65,83,83,69,
		32,61,32,83,84,82,40,101,67,76,65,83,95,80,
		82,79,44,53,41,32,46,65,78,68,46,32,81,85,
		65,68,82,65,32,61,32,101,66,76,79,67,79,0,
		20,2,36,134,0,176,9,0,20,0,36,135,0,176,
		42,0,12,0,29,60,1,36,136,0,176,6,0,106,
		2,49,0,20,1,36,137,0,176,38,0,106,3,48,
		49,0,20,1,36,138,0,176,43,0,106,39,65,103,
		117,97,114,100,101,32,97,32,69,108,105,109,105,110,
		97,99,97,111,32,100,111,115,32,82,101,103,105,115,
		116,114,111,115,32,46,46,46,0,20,1,36,139,0,
		176,44,0,12,0,28,31,36,140,0,176,45,0,90,
		8,176,46,0,12,0,6,100,100,100,100,9,20,6,
		36,141,0,176,47,0,20,0,36,143,0,176,6,0,
		106,2,52,0,20,1,36,144,0,176,25,0,90,17,
		109,26,0,176,27,0,109,12,0,92,5,12,2,5,
		6,106,30,78,85,77,95,76,79,84,69,65,77,32,
		61,32,83,84,82,40,101,67,76,65,83,95,80,82,
		79,44,53,41,0,20,2,36,145,0,176,48,0,12,
		0,28,17,36,146,0,9,78,28,0,36,147,0,176,
		47,0,20,0,36,149,0,176,49,0,109,50,0,106,
		18,67,65,68,65,83,84,82,79,32,67,76,73,69,
		78,84,69,83,0,106,30,69,88,67,76,85,83,65,
		79,32,68,79,32,80,82,79,68,85,84,79,32,67,
		76,65,83,83,69,32,58,32,0,176,51,0,176,27,
		0,109,12,0,92,5,12,2,12,1,72,106,12,32,
		81,85,65,47,66,76,79,32,58,32,0,72,176,51,
		0,109,53,0,12,1,72,20,3,26,169,4,36,151,
		0,176,38,0,106,3,48,49,0,20,1,36,152,0,
		176,43,0,106,50,69,120,99,108,117,115,198,111,32,
		66,108,111,113,117,101,97,100,97,44,32,69,120,105,
		115,116,101,109,32,118,101,110,100,97,115,32,110,101,
		115,116,101,32,80,114,111,100,117,116,111,32,0,20,
		1,36,153,0,176,52,0,20,0,36,155,0,26,82,
		4,36,156,0,109,13,0,92,3,5,29,62,4,36,
		158,0,176,6,0,106,2,52,0,20,1,36,159,0,
		176,25,0,90,24,109,26,0,176,27,0,109,12,0,
		92,5,12,2,5,21,28,6,73,109,28,0,6,106,
		45,78,85,77,95,76,79,84,69,65,77,32,61,32,
		83,84,82,40,101,67,76,65,83,95,80,82,79,44,
		53,41,32,46,65,78,68,46,32,67,65,68,65,83,
		84,82,79,0,20,2,36,160,0,176,9,0,20,0,
		36,161,0,121,83,53,0,36,162,0,176,14,0,92,
		6,92,2,20,2,176,15,0,106,10,66,108,111,99,
		111,32,32,58,32,0,20,1,36,163,0,176,9,0,
		20,0,36,164,0,92,3,3,1,0,92,3,3,1,
		0,92,3,3,1,0,92,3,3,1,0,176,20,0,
		108,16,108,17,108,18,108,19,20,4,81,19,0,81,
		18,0,81,17,0,81,16,0,36,165,0,176,21,0,
		109,16,0,120,109,19,0,20,3,36,166,0,106,8,
		64,82,57,57,57,57,57,0,98,17,0,122,2,36,
		167,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,92,2,2,36,168,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,92,3,2,36,169,0,106,5,
		32,32,32,32,0,98,18,0,122,2,36,170,0,106,
		5,32,32,32,32,0,98,18,0,92,2,2,36,171,
		0,106,5,32,32,32,32,0,98,18,0,92,3,2,
		36,172,0,176,22,0,92,6,92,10,92,6,92,41,
		109,16,0,106,7,75,69,89,83,95,52,0,109,17,
		0,109,18,0,106,2,32,0,106,4,32,45,32,0,
		106,2,32,0,20,11,36,173,0,109,55,0,83,53,
		0,36,174,0,109,56,0,83,57,0,36,175,0,176,
		6,0,106,2,49,0,20,1,36,176,0,176,25,0,
		90,28,109,36,0,176,27,0,109,12,0,92,5,12,
		2,5,21,28,10,73,109,37,0,109,53,0,5,6,
		106,48,67,76,65,83,83,69,32,61,32,83,84,82,
		40,101,67,76,65,83,95,80,82,79,44,53,41,32,
		46,65,78,68,46,32,81,85,65,68,82,65,32,61,
		32,101,66,76,79,67,79,0,20,2,36,177,0,176,
		9,0,20,0,36,178,0,176,38,0,106,3,48,49,
		0,20,1,36,179,0,176,39,0,106,42,67,111,110,
		102,105,114,109,97,32,97,32,69,120,99,108,117,115,
		97,111,32,100,97,115,32,85,110,105,100,97,100,101,
		115,32,40,83,47,78,41,32,63,32,0,20,1,36,
		180,0,109,41,0,106,2,83,0,5,29,22,2,36,
		181,0,176,6,0,106,2,53,0,20,1,36,182,0,
		176,25,0,90,28,109,36,0,176,27,0,109,12,0,
		92,5,12,2,5,21,28,10,73,109,37,0,109,53,
		0,5,6,106,48,67,76,65,83,83,69,32,61,32,
		83,84,82,40,101,67,76,65,83,95,80,82,79,44,
		53,41,32,46,65,78,68,46,32,81,85,65,68,82,
		65,32,61,32,101,66,76,79,67,79,0,20,2,36,
		183,0,176,9,0,20,0,36,184,0,176,42,0,12,
		0,29,67,1,36,185,0,176,6,0,106,2,49,0,
		20,1,36,186,0,176,38,0,106,3,48,49,0,20,
		1,36,187,0,176,43,0,106,39,65,103,117,97,114,
		100,101,32,97,32,69,108,105,109,105,110,97,99,97,
		111,32,100,111,115,32,82,101,103,105,115,116,114,111,
		115,32,46,46,46,0,20,1,36,188,0,176,44,0,
		12,0,28,39,36,189,0,176,45,0,90,8,176,46,
		0,12,0,6,100,100,100,100,9,20,6,36,190,0,
		176,59,0,20,0,36,191,0,176,47,0,20,0,36,
		193,0,176,6,0,106,2,52,0,20,1,36,194,0,
		176,25,0,90,17,109,26,0,176,27,0,109,12,0,
		92,5,12,2,5,6,106,30,78,85,77,95,76,79,
		84,69,65,77,32,61,32,83,84,82,40,101,67,76,
		65,83,95,80,82,79,44,53,41,0,20,2,36,195,
		0,176,48,0,12,0,28,17,36,196,0,9,78,28,
		0,36,197,0,176,47,0,20,0,36,199,0,176,49,
		0,109,50,0,106,18,67,65,68,65,83,84,82,79,
		32,67,76,73,69,78,84,69,83,0,106,30,69,88,
		67,76,85,83,65,79,32,68,79,32,80,82,79,68,
		85,84,79,32,67,76,65,83,83,69,32,58,32,0,
		176,51,0,176,27,0,109,12,0,92,5,12,2,12,
		1,72,106,12,32,81,85,65,47,66,76,79,32,58,
		32,0,72,176,51,0,109,53,0,12,1,72,20,3,
		25,96,36,201,0,176,38,0,106,3,48,49,0,20,
		1,36,202,0,176,43,0,106,50,69,120,99,108,117,
		115,198,111,32,66,108,111,113,117,101,97,100,97,44,
		32,69,120,105,115,116,101,109,32,118,101,110,100,97,
		115,32,110,101,115,116,101,32,80,114,111,100,117,116,
		111,32,0,20,1,36,203,0,176,52,0,20,0,36,
		205,0,25,10,36,207,0,176,60,0,20,0,36,210,
		0,176,61,0,20,0,36,211,0,176,62,0,121,121,
		176,2,0,12,0,176,3,0,12,0,109,4,0,20,
		5,36,212,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

