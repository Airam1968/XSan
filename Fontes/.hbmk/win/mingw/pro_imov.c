/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/pro_imov.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( PRO_IMOV );
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
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC( F_TRANS );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( RANGECHECK );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( PDESENV );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( PCENTRA );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( F_LACESSO );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_PRO_IMOV )
{ "PRO_IMOV", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( PRO_IMOV )}, NULL },
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
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EQUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EBLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSETFILTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETFILTER )}, NULL },
{ "NUM_LOTEAM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "CADASTRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISGRUPO1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_INICIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_FINAL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISUNID1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STATUS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "NUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESTATUS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROPR_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROPR_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "F_TRANS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( F_TRANS )}, NULL },
{ "EOPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SETPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOS )}, NULL },
{ "ROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROW )}, NULL },
{ "COL", {HB_FS_PUBLIC}, {HB_FUNCNAME( COL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETLIST", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__GET", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GET )}, NULL },
{ "RANGECHECK", {HB_FS_PUBLIC}, {HB_FUNCNAME( RANGECHECK )}, NULL },
{ "DISPLAY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ATAIL )}, NULL },
{ "READMODAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READMODAL )}, NULL },
{ "SISGRUPO2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISUNID2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PDESENV", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDESENV )}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "PT_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PQUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PUNIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PM_UNID", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "EMSG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PCENTRA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCENTRA )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "NUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "EPESQUISA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_NOVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_PRO_IMOV, "C:/sis/SAN/xhb/pro_imov.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_PRO_IMOV
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_PRO_IMOV )
   #include "hbiniseg.h"
#endif

HB_FUNC( PRO_IMOV )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,40,84,82,65,78,83,70,69,82,210,78,67,
		73,65,32,68,69,32,80,82,79,80,82,73,69,84,
		65,82,73,79,32,68,79,32,73,77,79,86,69,76,
		0,20,1,36,16,0,176,6,0,106,2,49,0,20,
		1,36,17,0,176,7,0,9,100,106,9,85,78,73,
		95,73,77,79,66,0,100,100,9,20,6,176,8,0,
		106,9,85,78,73,95,73,77,79,66,0,20,1,36,
		18,0,176,9,0,20,0,36,19,0,176,6,0,106,
		2,50,0,20,1,36,20,0,176,7,0,9,100,106,
		9,67,76,65,83,95,80,82,79,0,100,100,9,20,
		6,176,8,0,106,9,67,76,65,83,95,80,82,79,
		0,20,1,36,21,0,176,9,0,20,0,36,22,0,
		176,6,0,106,2,51,0,20,1,36,23,0,176,7,
		0,9,100,106,9,67,65,68,95,81,85,65,68,0,
		100,100,9,20,6,176,8,0,106,9,67,65,68,95,
		81,85,65,68,0,20,1,36,24,0,176,9,0,20,
		0,36,25,0,176,6,0,106,2,52,0,20,1,36,
		26,0,176,7,0,9,100,106,9,67,65,68,95,65,
		80,65,82,0,100,100,9,20,6,176,8,0,106,9,
		67,65,68,95,65,80,65,82,0,20,1,36,27,0,
		176,9,0,20,0,36,28,0,176,6,0,106,2,53,
		0,20,1,36,29,0,176,7,0,9,100,106,8,67,
		79,78,84,65,84,79,0,100,100,9,20,6,176,8,
		0,106,9,78,85,77,95,67,76,73,69,0,20,1,
		176,8,0,106,8,67,79,78,84,65,84,79,0,20,
		1,36,30,0,176,9,0,20,0,36,31,0,176,6,
		0,106,2,54,0,20,1,36,32,0,176,7,0,9,
		100,106,9,67,65,68,95,86,69,78,68,0,100,100,
		9,20,6,176,8,0,106,9,67,65,68,95,86,69,
		78,68,0,20,1,176,8,0,106,9,68,65,84,65,
		95,86,69,78,0,20,1,36,33,0,176,9,0,20,
		0,36,34,0,176,6,0,106,2,50,0,20,1,36,
		37,0,176,10,0,20,0,36,38,0,176,11,0,106,
		9,87,47,78,44,43,87,47,82,0,20,1,36,39,
		0,121,83,12,0,36,40,0,121,83,13,0,36,41,
		0,176,14,0,92,5,92,2,20,2,176,15,0,106,
		11,80,114,111,100,117,116,111,32,58,32,0,20,1,
		36,42,0,176,9,0,20,0,36,43,0,92,3,3,
		1,0,92,3,3,1,0,92,3,3,1,0,92,3,
		3,1,0,176,20,0,108,16,108,17,108,18,108,19,
		20,4,81,19,0,81,18,0,81,17,0,81,16,0,
		36,44,0,176,21,0,109,16,0,120,109,19,0,20,
		3,36,45,0,106,8,64,82,57,57,57,57,57,0,
		98,17,0,122,2,36,46,0,106,8,64,82,57,57,
		57,57,57,0,98,17,0,92,2,2,36,47,0,106,
		5,64,83,52,48,0,98,17,0,92,3,2,36,48,
		0,106,5,32,32,32,32,0,98,18,0,122,2,36,
		49,0,106,5,32,32,32,32,0,98,18,0,92,2,
		2,36,50,0,106,5,32,32,32,32,0,98,18,0,
		92,3,2,36,51,0,176,22,0,92,5,92,15,92,
		5,92,70,109,16,0,106,10,70,85,78,67,95,80,
		82,79,86,0,109,17,0,109,18,0,106,2,32,0,
		106,4,32,45,32,0,106,2,32,0,20,11,36,52,
		0,109,23,0,83,12,0,36,53,0,109,24,0,83,
		13,0,36,57,0,176,25,0,122,12,1,83,26,0,
		36,58,0,176,25,0,122,12,1,83,27,0,36,60,
		0,109,13,0,122,5,29,56,7,36,62,0,176,6,
		0,106,2,51,0,20,1,36,63,0,176,28,0,90,
		24,109,29,0,176,30,0,109,12,0,92,5,12,2,
		5,21,28,6,73,109,31,0,6,106,45,78,85,77,
		95,76,79,84,69,65,77,32,61,32,83,84,82,40,
		101,67,76,65,83,95,80,82,79,44,53,41,32,46,
		65,78,68,46,32,67,65,68,65,83,84,82,79,0,
		20,2,36,64,0,176,9,0,20,0,36,65,0,176,
		25,0,92,5,12,1,83,26,0,36,66,0,176,14,
		0,92,6,92,2,20,2,176,15,0,109,32,0,106,
		4,32,58,32,0,72,20,1,36,67,0,176,9,0,
		20,0,36,68,0,92,6,3,1,0,92,6,3,1,
		0,92,6,3,1,0,92,6,3,1,0,176,20,0,
		108,16,108,17,108,18,108,19,20,4,81,19,0,81,
		18,0,81,17,0,81,16,0,36,69,0,176,21,0,
		109,16,0,120,109,19,0,20,3,36,70,0,106,8,
		64,82,57,57,57,57,57,0,98,17,0,122,2,36,
		71,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,92,2,2,36,72,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,92,3,2,36,73,0,106,8,
		64,83,57,57,57,57,57,0,98,17,0,92,4,2,
		36,74,0,106,4,64,83,49,0,98,17,0,92,5,
		2,36,75,0,106,5,64,83,50,48,0,98,17,0,
		92,6,2,36,76,0,106,5,32,32,32,32,0,98,
		18,0,122,2,36,77,0,106,5,32,32,32,32,0,
		98,18,0,92,2,2,36,78,0,106,5,32,32,32,
		32,0,98,18,0,92,3,2,36,79,0,106,5,32,
		32,32,32,0,98,18,0,92,4,2,36,80,0,106,
		2,32,0,98,18,0,92,5,2,36,81,0,106,5,
		32,32,32,32,0,98,18,0,92,6,2,36,82,0,
		176,22,0,92,6,92,14,92,6,92,75,109,16,0,
		106,10,70,85,78,67,95,80,82,79,86,0,109,17,
		0,109,18,0,106,2,32,0,106,4,32,45,32,0,
		106,2,32,0,20,11,36,83,0,109,33,0,83,26,
		0,36,84,0,109,34,0,83,35,0,36,85,0,109,
		36,0,83,37,0,36,86,0,176,14,0,92,7,92,
		2,20,2,176,15,0,109,38,0,106,4,32,58,32,
		0,72,20,1,36,87,0,176,6,0,106,2,49,0,
		20,1,36,88,0,176,28,0,90,60,109,39,0,176,
		30,0,109,12,0,92,5,12,2,5,21,28,42,73,
		109,40,0,109,26,0,5,21,28,31,73,109,41,0,
		121,5,28,9,109,42,0,122,69,25,16,109,42,0,
		122,69,21,31,8,73,109,43,0,122,69,6,106,128,
		67,76,65,83,83,69,32,61,32,83,84,82,40,101,
		67,76,65,83,95,80,82,79,44,53,41,32,46,65,
		78,68,46,32,81,85,65,68,82,65,32,61,32,101,
		81,85,65,68,82,65,32,46,65,78,68,46,32,73,
		73,70,40,83,84,65,84,85,83,32,61,32,48,44,
		40,80,82,79,80,82,73,69,84,95,49,32,60,62,
		32,49,41,44,40,80,82,79,80,82,73,69,84,95,
		49,32,60,62,32,49,32,46,79,82,46,32,80,82,
		79,80,82,73,69,84,95,50,32,60,62,32,49,41,
		41,0,20,2,36,89,0,176,9,0,20,0,36,90,
		0,176,44,0,12,0,28,108,36,91,0,176,45,0,
		106,3,48,49,0,20,1,36,92,0,176,46,0,106,
		10,65,116,101,110,135,198,111,32,33,0,92,3,20,
		2,36,93,0,176,47,0,106,11,32,78,198,111,32,
		32,72,160,32,32,0,109,38,0,72,106,32,32,32,
		68,105,115,112,111,110,161,118,101,105,115,32,32,110,
		101,115,116,101,32,32,80,114,111,100,117,116,111,46,
		32,0,72,20,1,36,94,0,176,48,0,20,0,26,
		183,13,36,96,0,92,6,3,1,0,92,6,3,1,
		0,92,6,3,1,0,92,6,3,1,0,176,20,0,
		108,16,108,17,108,18,108,19,20,4,81,19,0,81,
		18,0,81,17,0,81,16,0,36,97,0,176,21,0,
		109,16,0,120,109,19,0,20,3,36,98,0,106,11,
		80,82,79,80,82,73,69,84,95,49,0,98,16,0,
		92,5,2,36,99,0,106,11,80,82,79,80,82,73,
		69,84,95,50,0,98,16,0,92,6,2,36,100,0,
		92,5,98,19,0,92,5,2,36,101,0,92,5,98,
		19,0,92,6,2,36,102,0,106,8,64,82,57,57,
		57,57,57,0,98,17,0,122,2,36,103,0,106,8,
		64,82,57,57,57,57,57,0,98,17,0,92,2,2,
		36,104,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,92,3,2,36,105,0,106,8,64,82,57,57,
		57,57,57,0,98,17,0,92,4,2,36,106,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,92,5,
		2,36,107,0,106,8,64,82,57,57,57,57,57,0,
		98,17,0,92,6,2,36,108,0,106,6,32,32,32,
		32,32,0,98,18,0,122,2,36,109,0,106,6,32,
		32,32,32,32,0,98,18,0,92,2,2,36,110,0,
		106,6,32,32,32,32,32,0,98,18,0,92,3,2,
		36,111,0,106,6,32,32,32,32,32,0,98,18,0,
		92,4,2,36,112,0,106,6,32,32,32,32,32,0,
		98,18,0,92,5,2,36,113,0,106,6,32,32,32,
		32,32,0,98,18,0,92,6,2,36,114,0,176,22,
		0,92,7,92,15,92,7,92,69,109,16,0,106,10,
		70,85,78,67,95,80,82,79,86,0,109,17,0,109,
		18,0,106,2,32,0,106,4,32,45,32,0,106,2,
		32,0,20,11,36,115,0,109,49,0,83,50,0,36,
		116,0,109,41,0,83,51,0,36,117,0,109,42,0,
		83,52,0,36,118,0,109,43,0,83,53,0,36,119,
		0,109,51,0,121,5,28,118,36,120,0,109,52,0,
		122,5,28,77,36,121,0,176,45,0,106,3,48,49,
		0,20,1,36,122,0,176,46,0,106,10,65,116,101,
		110,135,198,111,32,33,0,92,3,20,2,36,123,0,
		176,47,0,109,38,0,106,15,32,78,198,111,32,86,
		101,110,100,105,100,111,32,33,0,72,20,1,36,124,
		0,176,48,0,20,0,26,170,11,36,126,0,121,83,
		54,0,36,127,0,176,55,0,109,12,0,109,26,0,
		109,50,0,121,20,4,36,128,0,26,139,11,36,130,
		0,121,83,56,0,36,131,0,176,45,0,106,3,48,
		49,0,20,1,36,132,0,176,14,0,92,23,121,20,
		2,176,15,0,106,51,73,110,102,111,114,109,101,32,
		97,32,86,101,110,100,97,32,112,97,114,97,32,111,
		32,40,49,41,111,46,32,111,117,32,40,50,41,111,
		46,32,77,101,105,111,32,76,111,116,101,32,63,32,
		0,20,1,176,57,0,176,58,0,12,0,176,59,0,
		12,0,122,72,20,2,176,60,0,109,61,0,176,62,
		0,100,106,7,101,79,80,67,65,79,0,106,6,57,
		57,57,57,57,0,89,19,0,1,0,0,0,176,63,
		0,95,1,100,122,92,2,12,4,6,100,12,5,20,
		2,48,64,0,176,65,0,109,61,0,12,1,112,0,
		73,36,133,0,176,66,0,109,61,0,100,100,100,100,
		100,100,20,7,4,0,0,83,61,0,109,61,0,73,
		36,134,0,109,56,0,122,5,28,118,36,135,0,109,
		52,0,122,5,28,77,36,136,0,176,45,0,106,3,
		48,49,0,20,1,36,137,0,176,46,0,106,10,65,
		116,101,110,135,198,111,32,33,0,92,3,20,2,36,
		138,0,176,47,0,109,38,0,106,15,32,78,198,111,
		32,86,101,110,100,105,100,111,32,33,0,72,20,1,
		36,139,0,176,48,0,20,0,26,102,10,36,141,0,
		122,83,54,0,36,142,0,176,55,0,109,12,0,109,
		26,0,109,50,0,122,20,4,36,143,0,26,71,10,
		36,145,0,109,53,0,122,5,28,77,36,146,0,176,
		45,0,106,3,48,49,0,20,1,36,147,0,176,46,
		0,106,10,65,116,101,110,135,198,111,32,33,0,92,
		3,20,2,36,148,0,176,47,0,109,38,0,106,15,
		32,78,198,111,32,86,101,110,100,105,100,111,32,33,
		0,72,20,1,36,149,0,176,48,0,20,0,26,242,
		9,36,151,0,92,2,83,54,0,36,152,0,176,55,
		0,109,12,0,109,26,0,109,50,0,92,2,20,4,
		36,156,0,26,209,9,36,157,0,109,13,0,92,2,
		5,29,231,4,36,159,0,176,6,0,106,2,52,0,
		20,1,36,160,0,176,28,0,90,24,109,29,0,176,
		30,0,109,12,0,92,5,12,2,5,21,28,6,73,
		109,31,0,6,106,45,78,85,77,95,76,79,84,69,
		65,77,32,61,32,83,84,82,40,101,67,76,65,83,
		95,80,82,79,44,53,41,32,46,65,78,68,46,32,
		67,65,68,65,83,84,82,79,0,20,2,36,161,0,
		176,9,0,20,0,36,162,0,176,25,0,92,5,12,
		1,83,27,0,36,163,0,176,14,0,92,6,92,2,
		20,2,176,15,0,109,67,0,106,4,32,58,32,0,
		72,20,1,36,164,0,176,9,0,20,0,36,165,0,
		92,4,3,1,0,92,4,3,1,0,92,4,3,1,
		0,92,4,3,1,0,176,20,0,108,16,108,17,108,
		18,108,19,20,4,81,19,0,81,18,0,81,17,0,
		81,16,0,36,166,0,176,21,0,109,16,0,120,109,
		19,0,20,3,36,167,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,122,2,36,168,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,92,2,2,36,
		169,0,106,5,64,83,51,48,0,98,17,0,92,3,
		2,36,170,0,106,8,64,82,57,57,57,57,57,0,
		98,17,0,92,4,2,36,171,0,106,5,32,32,32,
		32,0,98,18,0,122,2,36,172,0,106,5,32,32,
		32,32,0,98,18,0,92,2,2,36,173,0,106,5,
		32,32,32,32,0,98,18,0,92,3,2,36,174,0,
		106,5,32,32,32,32,0,98,18,0,92,4,2,36,
		175,0,176,22,0,92,6,92,8,92,6,92,76,109,
		16,0,106,10,70,85,78,67,95,80,82,79,86,0,
		109,17,0,109,18,0,106,2,32,0,106,4,32,45,
		32,0,106,2,32,0,20,11,36,176,0,109,68,0,
		83,27,0,36,177,0,109,69,0,83,70,0,36,178,
		0,176,14,0,92,7,92,2,20,2,176,15,0,109,
		71,0,106,4,32,58,32,0,72,20,1,36,179,0,
		176,6,0,106,2,49,0,20,1,36,180,0,176,28,
		0,90,60,109,39,0,176,30,0,109,12,0,92,5,
		12,2,5,21,28,42,73,109,40,0,109,27,0,5,
		21,28,31,73,109,41,0,121,5,28,9,109,42,0,
		122,69,25,16,109,42,0,122,69,21,31,8,73,109,
		43,0,122,69,6,106,127,67,76,65,83,83,69,32,
		61,32,83,84,82,40,101,67,76,65,83,95,80,82,
		79,44,53,41,32,46,65,78,68,46,32,81,85,65,
		68,82,65,32,61,32,101,66,76,79,67,79,32,46,
		65,78,68,46,32,73,73,70,40,83,84,65,84,85,
		83,32,61,32,48,44,40,80,82,79,80,82,73,69,
		84,95,49,32,60,62,32,49,41,44,40,80,82,79,
		80,82,73,69,84,95,49,32,60,62,32,49,32,46,
		79,82,46,32,80,82,79,80,82,73,69,84,95,50,
		32,60,62,32,49,41,41,0,20,2,36,181,0,176,
		9,0,20,0,36,182,0,176,44,0,12,0,28,106,
		36,183,0,176,45,0,106,3,48,49,0,20,1,36,
		184,0,176,46,0,106,10,65,116,101,110,135,198,111,
		32,33,0,92,3,20,2,36,185,0,176,47,0,106,
		8,78,198,111,32,72,160,32,0,109,71,0,72,106,
		33,32,32,68,105,115,112,111,110,161,118,101,105,115,
		32,32,110,101,115,116,101,32,32,80,114,111,100,117,
		116,111,32,46,32,0,72,20,1,36,186,0,176,48,
		0,20,0,26,193,6,36,188,0,92,5,3,1,0,
		92,5,3,1,0,92,5,3,1,0,92,5,3,1,
		0,176,20,0,108,16,108,17,108,18,108,19,20,4,
		81,19,0,81,18,0,81,17,0,81,16,0,36,189,
		0,176,21,0,109,16,0,120,109,19,0,20,3,36,
		190,0,106,11,80,82,79,80,82,73,69,84,95,49,
		0,98,16,0,92,5,2,36,191,0,92,5,98,19,
		0,92,5,2,36,192,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,122,2,36,193,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,92,2,2,36,
		194,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,92,3,2,36,195,0,106,8,64,82,57,57,57,
		57,57,0,98,17,0,92,4,2,36,196,0,106,8,
		64,82,57,57,57,57,57,0,98,17,0,92,5,2,
		36,197,0,106,10,32,80,114,111,100,117,116,111,32,
		0,98,18,0,122,2,36,198,0,106,6,71,114,117,
		112,111,0,98,18,0,92,2,2,36,199,0,106,6,
		85,110,105,100,46,0,98,18,0,92,3,2,36,200,
		0,106,7,83,116,97,116,117,115,0,98,18,0,92,
		4,2,36,201,0,106,13,80,114,111,112,114,105,101,
		116,97,114,105,111,0,98,18,0,92,5,2,36,202,
		0,176,22,0,92,7,92,14,92,7,92,64,109,16,
		0,106,10,70,85,78,67,95,80,82,79,86,0,109,
		17,0,109,18,0,106,2,32,0,106,4,32,45,32,
		0,106,2,32,0,20,11,36,203,0,109,49,0,83,
		50,0,36,204,0,109,41,0,83,51,0,36,205,0,
		109,42,0,83,52,0,36,206,0,121,83,54,0,36,
		207,0,109,52,0,122,5,28,77,36,208,0,176,45,
		0,106,3,48,49,0,20,1,36,209,0,176,46,0,
		106,10,65,116,101,110,135,198,111,32,33,0,92,3,
		20,2,36,210,0,176,47,0,109,71,0,106,15,32,
		78,198,111,32,86,101,110,100,105,100,111,32,33,0,
		72,20,1,36,211,0,176,48,0,20,0,26,249,4,
		36,213,0,176,55,0,109,12,0,109,27,0,109,50,
		0,121,20,4,36,215,0,26,225,4,36,216,0,109,
		13,0,92,3,5,29,205,4,36,218,0,176,6,0,
		106,2,52,0,20,1,36,219,0,176,28,0,90,24,
		109,29,0,176,30,0,109,12,0,92,5,12,2,5,
		21,28,6,73,109,31,0,6,106,45,78,85,77,95,
		76,79,84,69,65,77,32,61,32,83,84,82,40,101,
		67,76,65,83,95,80,82,79,44,53,41,32,46,65,
		78,68,46,32,67,65,68,65,83,84,82,79,0,20,
		2,36,220,0,176,9,0,20,0,36,221,0,176,25,
		0,92,5,12,1,83,27,0,36,222,0,176,14,0,
		92,6,92,2,20,2,176,15,0,106,11,66,108,111,
		99,111,32,32,32,58,32,0,20,1,36,223,0,176,
		9,0,20,0,36,224,0,92,3,3,1,0,92,3,
		3,1,0,92,3,3,1,0,92,3,3,1,0,176,
		20,0,108,16,108,17,108,18,108,19,20,4,81,19,
		0,81,18,0,81,17,0,81,16,0,36,225,0,176,
		21,0,109,16,0,120,109,19,0,20,3,36,226,0,
		106,8,64,82,57,57,57,57,57,0,98,17,0,122,
		2,36,227,0,106,8,64,82,57,57,57,57,57,0,
		98,17,0,92,2,2,36,228,0,106,8,64,82,57,
		57,57,57,57,0,98,17,0,92,3,2,36,229,0,
		106,5,32,32,32,32,0,98,18,0,122,2,36,230,
		0,106,5,32,32,32,32,0,98,18,0,92,2,2,
		36,231,0,106,5,32,32,32,32,0,98,18,0,92,
		3,2,36,232,0,176,22,0,92,6,92,10,92,6,
		92,41,109,16,0,106,10,70,85,78,67,95,80,82,
		79,86,0,109,17,0,109,18,0,106,2,32,0,106,
		4,32,45,32,0,106,2,32,0,20,11,36,233,0,
		109,68,0,83,27,0,36,234,0,109,69,0,83,70,
		0,36,235,0,176,14,0,92,7,92,2,20,2,176,
		15,0,106,11,83,97,108,97,32,32,32,32,58,32,
		0,20,1,36,236,0,176,6,0,106,2,49,0,20,
		1,36,237,0,176,28,0,90,60,109,39,0,176,30,
		0,109,12,0,92,5,12,2,5,21,28,42,73,109,
		40,0,109,27,0,5,21,28,31,73,109,41,0,121,
		5,28,9,109,42,0,122,69,25,16,109,42,0,122,
		69,21,31,8,73,109,43,0,122,69,6,106,127,67,
		76,65,83,83,69,32,61,32,83,84,82,40,101,67,
		76,65,83,95,80,82,79,44,53,41,32,46,65,78,
		68,46,32,81,85,65,68,82,65,32,61,32,101,66,
		76,79,67,79,32,46,65,78,68,46,32,73,73,70,
		40,83,84,65,84,85,83,32,61,32,48,44,40,80,
		82,79,80,82,73,69,84,95,49,32,60,62,32,49,
		41,44,40,80,82,79,80,82,73,69,84,95,49,32,
		60,62,32,49,32,46,79,82,46,32,80,82,79,80,
		82,73,69,84,95,50,32,60,62,32,49,41,41,0,
		20,2,36,238,0,176,9,0,20,0,36,239,0,176,
		44,0,12,0,28,107,36,240,0,176,45,0,106,3,
		48,49,0,20,1,36,241,0,176,46,0,106,10,65,
		116,101,110,135,198,111,32,33,0,92,3,20,2,36,
		242,0,176,47,0,106,49,78,198,111,32,72,160,32,
		73,109,135,118,101,105,115,32,68,105,115,112,111,110,
		161,118,101,105,115,32,110,101,115,116,101,32,69,109,
		112,114,101,101,110,100,105,109,101,110,116,111,46,0,
		20,1,36,243,0,176,48,0,20,0,26,234,1,36,
		245,0,92,5,3,1,0,92,5,3,1,0,92,5,
		3,1,0,92,5,3,1,0,176,20,0,108,16,108,
		17,108,18,108,19,20,4,81,19,0,81,18,0,81,
		17,0,81,16,0,36,246,0,176,21,0,109,16,0,
		120,109,19,0,20,3,36,247,0,106,11,80,82,79,
		80,82,73,69,84,95,49,0,98,16,0,92,5,2,
		36,248,0,92,5,98,19,0,92,5,2,36,249,0,
		106,8,64,82,57,57,57,57,57,0,98,17,0,122,
		2,36,250,0,106,8,64,82,57,57,57,57,57,0,
		98,17,0,92,2,2,36,251,0,106,8,64,82,57,
		57,57,57,57,0,98,17,0,92,3,2,36,252,0,
		106,8,64,82,57,57,57,57,57,0,98,17,0,92,
		4,2,36,253,0,106,8,64,82,57,57,57,57,57,
		0,98,17,0,92,5,2,36,254,0,106,10,69,109,
		112,114,101,101,110,100,46,0,98,18,0,122,2,36,
		255,0,106,6,66,108,111,99,111,0,98,18,0,92,
		2,2,36,0,1,106,5,83,97,108,97,0,98,18,
		0,92,3,2,36,1,1,106,7,83,116,97,116,117,
		115,0,98,18,0,92,4,2,36,2,1,106,13,80,
		114,111,112,114,105,101,116,97,114,105,111,0,98,18,
		0,92,5,2,36,3,1,176,22,0,92,7,92,14,
		92,7,92,64,109,16,0,106,10,70,85,78,67,95,
		80,82,79,86,0,109,17,0,109,18,0,106,2,32,
		0,106,4,32,45,32,0,106,2,32,0,20,11,36,
		4,1,109,49,0,83,50,0,36,5,1,109,41,0,
		83,51,0,36,6,1,109,42,0,83,52,0,36,7,
		1,121,83,54,0,36,8,1,109,52,0,122,5,28,
		78,36,9,1,176,45,0,106,3,48,49,0,20,1,
		36,10,1,176,46,0,106,10,65,116,101,110,135,198,
		111,32,33,0,92,3,20,2,36,11,1,176,47,0,
		106,21,73,109,111,118,101,108,32,78,198,111,32,86,
		101,110,100,105,100,111,32,33,0,20,1,36,12,1,
		176,48,0,20,0,25,33,36,14,1,176,55,0,109,
		12,0,109,27,0,109,50,0,121,20,4,36,16,1,
		25,10,36,19,1,176,72,0,20,0,36,21,1,176,
		73,0,20,0,36,22,1,176,74,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,4,0,20,5,36,
		23,1,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( F_TRANS )
{
	static const HB_BYTE pcode[] =
	{
		13,0,4,71,75,0,1,71,76,0,2,71,77,0,
		3,71,78,0,4,36,28,1,106,46,84,82,65,78,
		83,70,69,82,135,78,67,73,65,32,68,69,32,80,
		82,79,80,82,73,69,84,65,82,73,79,32,68,65,
		32,85,78,73,68,65,68,69,32,78,111,46,32,0,
		176,79,0,109,77,0,12,1,72,106,4,32,45,32,
		0,72,109,78,0,121,5,28,25,106,19,80,114,111,
		112,114,105,101,116,97,114,105,111,32,85,110,105,99,
		111,0,25,49,109,78,0,122,5,28,23,106,17,49,
		111,46,32,80,82,79,80,82,73,69,84,65,82,73,
		79,0,25,21,106,17,50,111,46,32,80,82,79,80,
		82,73,69,84,65,82,73,79,0,72,83,80,0,36,
		29,1,176,81,0,109,80,0,92,9,20,2,36,30,
		1,176,45,0,106,3,48,49,0,20,1,36,31,1,
		176,82,0,106,27,67,111,110,102,105,114,109,97,32,
		111,32,73,109,111,118,101,108,32,40,83,47,78,41,
		32,63,32,0,20,1,36,32,1,109,83,0,106,2,
		78,0,5,28,8,36,33,1,121,110,7,36,35,1,
		176,45,0,106,3,48,49,0,20,1,36,36,1,176,
		6,0,106,2,54,0,20,1,36,37,1,176,9,0,
		20,0,36,38,1,176,84,0,176,30,0,109,75,0,
		92,5,12,2,109,76,0,72,109,77,0,72,176,30,
		0,109,78,0,122,12,2,72,100,100,20,3,36,39,
		1,109,85,0,83,86,0,36,40,1,176,6,0,106,
		2,53,0,20,1,36,41,1,176,7,0,9,100,106,
		8,67,79,78,84,65,84,79,0,100,100,9,20,6,
		176,8,0,106,9,78,85,77,95,67,76,73,69,0,
		20,1,176,8,0,106,8,67,79,78,84,65,84,79,
		0,20,1,36,42,1,176,9,0,20,0,36,43,1,
		176,84,0,109,86,0,100,100,20,3,36,44,1,176,
		14,0,92,11,92,2,20,2,176,15,0,106,7,78,
		111,109,101,32,58,0,20,1,176,57,0,176,58,0,
		12,0,176,59,0,12,0,122,72,20,2,176,60,0,
		109,61,0,176,62,0,100,106,8,67,76,73,69,78,
		84,69,0,100,100,100,12,5,20,2,48,64,0,176,
		65,0,109,61,0,12,1,112,0,73,36,45,1,176,
		87,0,120,20,1,4,0,0,83,61,0,109,61,0,
		73,36,47,1,176,6,0,106,2,53,0,20,1,36,
		48,1,176,7,0,9,100,106,8,67,79,78,84,65,
		84,79,0,100,100,9,20,6,176,8,0,106,8,67,
		79,78,84,65,84,79,0,20,1,176,8,0,106,9,
		78,85,77,95,67,76,73,69,0,20,1,36,49,1,
		176,9,0,20,0,36,50,1,176,25,0,92,49,12,
		1,83,88,0,36,51,1,176,14,0,92,13,92,2,
		20,2,176,15,0,106,7,78,111,118,111,32,58,0,
		20,1,176,57,0,176,58,0,12,0,176,59,0,12,
		0,122,72,20,2,176,60,0,109,61,0,176,62,0,
		100,106,10,101,80,69,83,81,85,73,83,65,0,106,
		54,64,33,32,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,0,100,
		100,12,5,20,2,48,64,0,176,65,0,109,61,0,
		12,1,112,0,73,36,52,1,176,66,0,109,61,0,
		100,100,100,100,100,100,20,7,4,0,0,83,61,0,
		109,61,0,73,36,53,1,176,79,0,109,88,0,12,
		1,83,88,0,36,54,1,176,84,0,109,88,0,100,
		100,20,3,36,55,1,92,2,3,1,0,92,2,3,
		1,0,92,2,3,1,0,92,2,3,1,0,176,20,
		0,108,16,108,17,108,18,108,19,20,4,81,19,0,
		81,18,0,81,17,0,81,16,0,36,56,1,176,21,
		0,109,16,0,120,109,19,0,20,3,36,57,1,106,
		8,67,80,70,95,67,71,67,0,98,16,0,92,2,
		2,36,58,1,92,14,98,19,0,92,2,2,36,59,
		1,106,5,64,83,53,48,0,98,17,0,122,2,36,
		60,1,106,5,64,83,49,52,0,98,17,0,92,2,
		2,36,61,1,106,7,32,32,78,79,77,69,0,98,
		18,0,122,2,36,62,1,106,8,67,71,67,47,67,
		80,70,0,98,18,0,92,2,2,36,63,1,176,22,
		0,92,13,92,5,92,13,92,79,109,16,0,106,7,
		75,69,89,83,95,52,0,109,17,0,109,18,0,106,
		2,32,0,106,2,32,0,106,2,32,0,20,11,36,
		64,1,176,45,0,106,3,48,49,0,20,1,36,65,
		1,109,85,0,83,89,0,36,66,1,176,45,0,106,
		3,48,49,0,20,1,36,67,1,176,82,0,106,33,
		67,111,110,102,105,114,109,97,32,97,32,84,114,97,
		110,102,101,114,135,110,99,105,97,32,40,83,47,78,
		41,32,63,32,0,20,1,36,68,1,109,83,0,106,
		2,83,0,5,29,59,1,36,69,1,176,6,0,106,
		2,54,0,20,1,36,70,1,176,90,0,12,0,28,
		19,36,71,1,109,89,0,78,85,0,36,72,1,176,
		91,0,20,0,36,74,1,176,6,0,106,2,49,0,
		20,1,36,75,1,109,78,0,121,5,31,9,109,78,
		0,122,5,28,34,36,76,1,176,90,0,12,0,28,
		19,36,77,1,109,89,0,78,42,0,36,78,1,176,
		91,0,20,0,36,79,1,25,32,36,81,1,176,90,
		0,12,0,28,19,36,82,1,109,89,0,78,43,0,
		36,83,1,176,91,0,20,0,36,86,1,176,92,0,
		109,93,0,106,18,82,79,84,73,78,65,83,32,69,
		83,80,69,67,73,65,73,83,0,106,37,84,82,65,
		78,83,70,69,82,69,78,67,73,65,32,80,82,79,
		80,82,73,69,84,65,82,73,79,32,73,77,79,86,
		69,76,32,58,32,0,176,79,0,176,30,0,109,75,
		0,92,5,12,2,12,1,72,106,2,47,0,72,176,
		79,0,109,76,0,12,1,72,106,2,47,0,72,176,
		79,0,109,77,0,12,1,72,106,2,47,0,72,176,
		79,0,176,30,0,109,78,0,122,12,2,12,1,72,
		20,3,36,87,1,176,45,0,106,3,48,49,0,20,
		1,36,88,1,176,46,0,106,26,84,114,97,110,115,
		102,101,114,135,110,99,105,97,32,67,111,110,99,108,
		117,105,100,97,32,33,0,92,3,20,2,36,90,1,
		121,110,7
	};

	hb_vmExecute( pcode, symbols );
}

