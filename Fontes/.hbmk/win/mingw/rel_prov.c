/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_prov.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_PROV );
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
HB_FUNC_EXTERN( PDESENV );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( PTECLA );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( __SETFORMAT );
HB_FUNC_EXTERN( RESTSCREEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_PROV )
{ "REL_PROV", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_PROV )}, NULL },
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
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDESC_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "EBLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "CLASSE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUADRA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISGRUPO2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_BLOCO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANTIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PDESENV", {HB_FS_PUBLIC}, {HB_FUNCNAME( PDESENV )}, NULL },
{ "EUNIDADES", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETOT_M2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "SISLETRAC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "ETIPO_PRD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "NUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_IMOVEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STATUS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPARTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROPRIET_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DATA_VENDA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUITADO_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "OPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_R_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_C_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROPRIET_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROPRIET_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUITADO_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "PTECLA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTECLA )}, NULL },
{ "SISINTERR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_PROV, "C:/sis/SAN/xhb/rel_prov.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_PROV
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_PROV )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_PROV )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,121,121,176,2,0,12,0,176,
		3,0,12,0,12,4,83,4,0,36,15,0,176,5,
		0,106,31,82,69,76,65,84,79,82,73,79,32,68,
		69,32,85,78,73,68,65,68,69,83,32,86,69,78,
		68,73,68,65,83,0,20,1,36,16,0,176,6,0,
		106,2,49,0,20,1,36,17,0,176,7,0,9,100,
		106,9,85,78,73,95,73,77,79,66,0,100,100,9,
		20,6,176,8,0,106,9,85,78,73,95,73,77,79,
		66,0,20,1,36,18,0,176,9,0,20,0,36,19,
		0,176,6,0,106,2,50,0,20,1,36,20,0,176,
		7,0,9,100,106,9,67,76,65,83,95,80,82,79,
		0,100,100,9,20,6,176,8,0,106,9,67,76,65,
		83,95,80,82,79,0,20,1,36,21,0,176,9,0,
		20,0,36,22,0,176,6,0,106,2,51,0,20,1,
		36,23,0,176,7,0,9,100,106,9,67,65,68,95,
		81,85,65,68,0,100,100,9,20,6,176,8,0,106,
		9,67,65,68,95,81,85,65,68,0,20,1,36,24,
		0,176,9,0,20,0,36,25,0,176,6,0,106,2,
		52,0,20,1,36,26,0,176,7,0,9,100,106,9,
		67,65,68,95,65,80,65,82,0,100,100,9,20,6,
		176,8,0,106,9,67,65,68,95,65,80,65,82,0,
		20,1,36,27,0,176,9,0,20,0,36,28,0,176,
		6,0,106,2,53,0,20,1,36,29,0,176,7,0,
		9,100,106,8,67,79,78,84,65,84,79,0,100,100,
		9,20,6,176,8,0,106,9,78,85,77,95,67,76,
		73,69,0,20,1,176,8,0,106,8,67,79,78,84,
		65,84,79,0,20,1,36,30,0,176,9,0,20,0,
		36,31,0,176,6,0,106,2,54,0,20,1,36,32,
		0,176,7,0,9,100,106,9,84,73,80,79,95,80,
		82,79,0,100,100,9,20,6,176,8,0,106,9,84,
		73,80,79,95,80,82,79,0,20,1,36,33,0,176,
		9,0,20,0,36,34,0,176,6,0,106,2,55,0,
		20,1,36,35,0,176,7,0,9,100,106,9,67,65,
		68,95,86,69,78,68,0,100,100,9,20,6,176,8,
		0,106,9,67,65,68,95,86,69,78,68,0,20,1,
		176,8,0,106,9,68,65,84,65,95,86,69,78,0,
		20,1,36,36,0,176,9,0,20,0,36,37,0,176,
		6,0,106,2,50,0,20,1,36,40,0,176,10,0,
		20,0,36,41,0,176,11,0,106,9,87,47,78,44,
		43,87,47,82,0,20,1,36,42,0,121,83,12,0,
		36,43,0,121,83,13,0,36,44,0,176,14,0,92,
		5,92,2,20,2,176,15,0,106,11,80,114,111,100,
		117,116,111,32,58,32,0,20,1,36,45,0,176,9,
		0,20,0,36,46,0,92,3,3,1,0,92,3,3,
		1,0,92,3,3,1,0,92,3,3,1,0,176,20,
		0,108,16,108,17,108,18,108,19,20,4,81,19,0,
		81,18,0,81,17,0,81,16,0,36,47,0,176,21,
		0,109,16,0,120,109,19,0,20,3,36,48,0,106,
		8,64,82,57,57,57,57,57,0,98,17,0,122,2,
		36,49,0,106,8,64,82,57,57,57,57,57,0,98,
		17,0,92,2,2,36,50,0,106,5,64,83,52,48,
		0,98,17,0,92,3,2,36,51,0,106,5,32,32,
		32,32,0,98,18,0,122,2,36,52,0,106,5,32,
		32,32,32,0,98,18,0,92,2,2,36,53,0,106,
		5,32,32,32,32,0,98,18,0,92,3,2,36,54,
		0,176,22,0,92,5,92,15,92,5,92,70,109,16,
		0,106,10,70,85,78,67,95,80,82,79,86,0,109,
		17,0,109,18,0,106,2,32,0,106,4,32,45,32,
		0,106,2,32,0,20,11,36,55,0,109,23,0,83,
		12,0,36,56,0,109,24,0,83,13,0,36,57,0,
		109,25,0,83,26,0,36,58,0,176,27,0,122,12,
		1,83,28,0,36,59,0,176,27,0,122,12,1,83,
		29,0,36,64,0,109,13,0,122,5,29,87,2,36,
		66,0,176,6,0,106,2,51,0,20,1,36,67,0,
		176,30,0,90,24,109,31,0,176,32,0,109,12,0,
		92,5,12,2,5,21,28,6,73,109,33,0,6,106,
		45,78,85,77,95,76,79,84,69,65,77,32,61,32,
		83,84,82,40,101,67,76,65,83,95,80,82,79,44,
		53,41,32,46,65,78,68,46,32,67,65,68,65,83,
		84,82,79,0,20,2,36,68,0,176,9,0,20,0,
		36,69,0,176,27,0,92,5,12,1,83,29,0,36,
		70,0,176,14,0,92,6,92,2,20,2,176,15,0,
		109,34,0,106,4,32,58,32,0,72,20,1,36,71,
		0,176,9,0,20,0,36,72,0,92,6,3,1,0,
		92,6,3,1,0,92,6,3,1,0,92,6,3,1,
		0,176,20,0,108,16,108,17,108,18,108,19,20,4,
		81,19,0,81,18,0,81,17,0,81,16,0,36,73,
		0,176,21,0,109,16,0,120,109,19,0,20,3,36,
		74,0,106,8,64,82,57,57,57,57,57,0,98,17,
		0,122,2,36,75,0,106,8,64,82,57,57,57,57,
		57,0,98,17,0,92,2,2,36,76,0,106,8,64,
		82,57,57,57,57,57,0,98,17,0,92,3,2,36,
		77,0,106,8,64,83,57,57,57,57,57,0,98,17,
		0,92,4,2,36,78,0,106,4,64,83,49,0,98,
		17,0,92,5,2,36,79,0,106,5,64,83,50,48,
		0,98,17,0,92,6,2,36,80,0,106,5,32,32,
		32,32,0,98,18,0,122,2,36,81,0,106,5,32,
		32,32,32,0,98,18,0,92,2,2,36,82,0,106,
		5,32,32,32,32,0,98,18,0,92,3,2,36,83,
		0,106,5,32,32,32,32,0,98,18,0,92,4,2,
		36,84,0,106,2,32,0,98,18,0,92,5,2,36,
		85,0,106,5,32,32,32,32,0,98,18,0,92,6,
		2,36,86,0,176,22,0,92,6,92,14,92,6,92,
		75,109,16,0,106,10,70,85,78,67,95,80,82,79,
		86,0,109,17,0,109,18,0,106,2,32,0,106,4,
		32,45,32,0,106,2,32,0,20,11,36,87,0,109,
		35,0,83,29,0,36,88,0,109,36,0,83,37,0,
		36,89,0,109,38,0,83,39,0,36,90,0,176,6,
		0,106,2,49,0,20,1,36,91,0,176,30,0,90,
		28,109,40,0,176,32,0,109,12,0,92,5,12,2,
		5,21,28,10,73,109,41,0,109,29,0,5,6,106,
		49,67,76,65,83,83,69,32,61,32,83,84,82,40,
		101,67,76,65,83,95,80,82,79,44,53,41,32,46,
		65,78,68,46,32,81,85,65,68,82,65,32,61,32,
		101,81,85,65,68,82,65,0,20,2,36,92,0,176,
		9,0,20,0,26,33,2,36,93,0,109,13,0,92,
		2,5,29,13,2,36,95,0,176,6,0,106,2,52,
		0,20,1,36,96,0,176,30,0,90,24,109,31,0,
		176,32,0,109,12,0,92,5,12,2,5,21,28,6,
		73,109,33,0,6,106,45,78,85,77,95,76,79,84,
		69,65,77,32,61,32,83,84,82,40,101,67,76,65,
		83,95,80,82,79,44,53,41,32,46,65,78,68,46,
		32,67,65,68,65,83,84,82,79,0,20,2,36,97,
		0,176,9,0,20,0,36,98,0,176,27,0,92,5,
		12,1,83,28,0,36,99,0,176,14,0,92,6,92,
		2,20,2,176,15,0,109,42,0,106,4,32,58,32,
		0,72,20,1,36,100,0,176,9,0,20,0,36,101,
		0,92,4,3,1,0,92,4,3,1,0,92,4,3,
		1,0,92,4,3,1,0,176,20,0,108,16,108,17,
		108,18,108,19,20,4,81,19,0,81,18,0,81,17,
		0,81,16,0,36,102,0,176,21,0,109,16,0,120,
		109,19,0,20,3,36,103,0,106,8,64,82,57,57,
		57,57,57,0,98,17,0,122,2,36,104,0,106,8,
		64,82,57,57,57,57,57,0,98,17,0,92,2,2,
		36,105,0,106,5,64,83,51,48,0,98,17,0,92,
		3,2,36,106,0,106,8,64,82,57,57,57,57,57,
		0,98,17,0,92,4,2,36,107,0,106,5,32,32,
		32,32,0,98,18,0,122,2,36,108,0,106,5,32,
		32,32,32,0,98,18,0,92,2,2,36,109,0,106,
		5,32,32,32,32,0,98,18,0,92,3,2,36,110,
		0,106,5,32,32,32,32,0,98,18,0,92,4,2,
		36,111,0,176,22,0,92,6,92,8,92,6,92,76,
		109,16,0,106,10,70,85,78,67,95,80,82,79,86,
		0,109,17,0,109,18,0,106,2,32,0,106,4,32,
		45,32,0,106,2,32,0,20,11,36,112,0,109,43,
		0,83,28,0,36,113,0,109,44,0,83,45,0,36,
		114,0,176,6,0,106,2,49,0,20,1,36,115,0,
		176,30,0,90,28,109,40,0,176,32,0,109,12,0,
		92,5,12,2,5,21,28,10,73,109,41,0,109,28,
		0,5,6,106,48,67,76,65,83,83,69,32,61,32,
		83,84,82,40,101,67,76,65,83,95,80,82,79,44,
		53,41,32,46,65,78,68,46,32,81,85,65,68,82,
		65,32,61,32,101,66,76,79,67,79,0,20,2,36,
		116,0,176,9,0,20,0,25,10,36,119,0,176,46,
		0,20,0,36,121,0,121,83,47,0,36,122,0,121,
		83,48,0,36,123,0,176,27,0,92,60,12,1,83,
		49,0,36,124,0,176,50,0,109,51,0,12,1,176,
		27,0,122,12,1,69,28,18,36,125,0,109,51,0,
		106,2,58,0,72,83,49,0,25,13,36,127,0,106,
		3,67,58,0,83,49,0,36,130,0,109,52,0,176,
		27,0,92,30,12,1,69,28,45,36,131,0,176,50,
		0,109,49,0,12,1,109,52,0,72,106,12,69,88,
		84,82,65,84,79,46,84,88,84,0,72,176,27,0,
		92,10,12,1,72,83,49,0,25,39,36,133,0,176,
		50,0,109,49,0,12,1,106,12,69,88,84,82,65,
		84,79,46,84,88,84,0,72,176,27,0,92,10,12,
		1,72,83,49,0,36,136,0,176,6,0,106,2,49,
		0,20,1,36,137,0,176,9,0,20,0,36,138,0,
		176,53,0,106,50,80,111,115,105,99,105,111,110,101,
		32,111,32,70,111,114,109,117,108,97,114,105,111,32,
		56,48,32,67,111,108,117,110,97,115,32,101,32,84,
		101,99,108,101,32,91,69,78,84,69,82,93,0,20,
		1,36,139,0,176,54,0,20,0,36,140,0,176,55,
		0,20,0,36,141,0,176,56,0,92,20,106,8,80,
		82,73,78,84,69,82,0,20,2,36,142,0,176,57,
		0,12,0,32,202,7,36,143,0,176,14,0,122,121,
		20,2,176,15,0,109,58,0,106,11,69,109,112,114,
		101,115,97,32,58,32,0,72,109,59,0,72,20,1,
		36,144,0,176,14,0,122,92,115,20,2,176,15,0,
		106,8,68,97,116,97,32,58,32,0,176,60,0,176,
		61,0,12,0,12,1,72,20,1,36,145,0,176,14,
		0,92,2,121,20,2,176,15,0,106,11,83,105,115,
		116,101,109,97,32,58,32,0,109,62,0,72,20,1,
		36,146,0,176,14,0,92,2,92,115,20,2,176,15,
		0,106,14,82,101,108,97,116,58,32,83,65,78,48,
		48,51,0,20,1,36,147,0,176,6,0,106,2,54,
		0,20,1,36,148,0,176,63,0,109,13,0,100,100,
		20,3,36,149,0,109,25,0,83,64,0,36,150,0,
		176,6,0,106,2,49,0,20,1,36,151,0,176,14,
		0,92,3,92,40,20,2,176,15,0,109,65,0,106,
		31,82,69,76,65,84,79,82,73,79,32,68,69,32,
		85,78,73,68,65,68,69,83,32,86,69,78,68,73,
		68,65,83,0,72,109,58,0,72,20,1,36,152,0,
		176,14,0,92,4,121,20,2,176,15,0,106,11,80,
		114,111,100,117,116,111,32,58,32,0,176,32,0,109,
		12,0,92,5,12,2,72,106,4,32,45,32,0,72,
		176,50,0,109,26,0,12,1,72,106,19,32,45,32,
		81,117,97,100,114,97,92,66,108,111,99,111,32,58,
		32,0,72,176,50,0,109,29,0,12,1,72,106,2,
		92,0,72,176,50,0,109,28,0,12,1,72,20,1,
		36,153,0,176,14,0,92,5,121,20,2,176,15,0,
		176,66,0,106,2,61,0,93,135,0,12,2,20,1,
		36,154,0,176,14,0,92,6,92,2,20,2,176,15,
		0,106,11,78,111,46,32,73,77,79,86,69,76,0,
		20,1,36,155,0,176,14,0,92,6,92,15,20,2,
		176,15,0,106,7,83,84,65,84,85,83,0,20,1,
		36,156,0,176,14,0,92,6,92,25,20,2,176,15,
		0,106,15,78,79,77,69,32,67,79,77,80,82,65,
		68,79,82,0,20,1,36,157,0,176,14,0,92,6,
		92,75,20,2,176,15,0,106,11,68,65,84,65,32,
		86,69,78,68,65,0,20,1,36,158,0,176,14,0,
		92,6,92,87,20,2,176,15,0,106,8,81,85,73,
		84,65,68,79,0,20,1,36,159,0,176,14,0,92,
		6,92,97,20,2,176,15,0,106,9,84,69,76,69,
		70,79,78,69,0,20,1,36,160,0,176,14,0,92,
		7,121,20,2,176,15,0,176,66,0,106,2,61,0,
		93,135,0,12,2,20,1,36,161,0,92,8,83,67,
		0,36,162,0,176,57,0,12,0,32,46,5,109,67,
		0,92,54,34,29,37,5,36,163,0,109,68,0,122,
		69,29,78,2,36,164,0,176,14,0,109,67,0,92,
		2,20,2,176,69,0,109,70,0,106,9,64,82,32,
		57,57,57,57,57,0,20,2,36,165,0,109,70,0,
		83,71,0,36,166,0,109,72,0,121,5,28,37,36,
		167,0,176,14,0,109,67,0,92,15,20,2,176,15,
		0,106,6,85,110,105,99,111,0,20,1,36,168,0,
		121,83,73,0,25,38,36,170,0,176,14,0,109,67,
		0,92,15,20,2,176,15,0,106,9,49,111,46,80,
		114,111,112,46,0,20,1,36,171,0,122,83,73,0,
		36,173,0,109,68,0,83,74,0,36,174,0,176,6,
		0,106,2,53,0,20,1,36,175,0,176,63,0,109,
		74,0,100,100,20,3,36,176,0,176,14,0,109,67,
		0,92,25,20,2,176,15,0,109,75,0,20,1,36,
		177,0,109,13,0,122,5,28,83,36,178,0,176,6,
		0,106,2,55,0,20,1,36,179,0,176,9,0,20,
		0,36,180,0,176,63,0,176,32,0,109,12,0,92,
		5,12,2,109,29,0,72,109,71,0,72,176,32,0,
		109,73,0,122,12,2,72,100,100,20,3,36,181,0,
		176,14,0,109,67,0,92,75,20,2,176,15,0,109,
		76,0,20,1,25,81,36,183,0,176,6,0,106,2,
		55,0,20,1,36,184,0,176,9,0,20,0,36,185,
		0,176,63,0,176,32,0,109,12,0,92,5,12,2,
		109,28,0,72,109,71,0,72,176,32,0,109,73,0,
		122,12,2,72,100,100,20,3,36,186,0,176,14,0,
		109,67,0,92,75,20,2,176,15,0,109,76,0,20,
		1,36,188,0,176,6,0,106,2,49,0,20,1,36,
		189,0,176,14,0,109,67,0,92,87,20,2,176,15,
		0,109,77,0,28,10,106,4,83,105,109,0,25,8,
		106,4,78,97,111,0,20,1,36,190,0,176,6,0,
		106,2,53,0,20,1,36,191,0,109,78,0,106,2,
		82,0,5,28,69,36,192,0,176,14,0,109,67,0,
		92,97,20,2,176,69,0,109,79,0,106,20,64,82,
		32,40,88,88,41,32,88,88,88,88,88,45,88,88,
		88,88,88,0,20,2,36,193,0,176,14,0,109,67,
		0,92,118,20,2,176,15,0,106,2,82,0,20,1,
		25,67,36,195,0,176,14,0,109,67,0,92,97,20,
		2,176,69,0,109,80,0,106,20,64,82,32,40,88,
		88,41,32,88,88,88,88,88,45,88,88,88,88,88,
		0,20,2,36,196,0,176,14,0,109,67,0,92,118,
		20,2,176,15,0,106,2,67,0,20,1,36,198,0,
		176,6,0,106,2,49,0,20,1,36,199,0,109,67,
		0,122,72,83,67,0,36,200,0,109,47,0,122,72,
		83,47,0,36,202,0,109,72,0,121,69,29,42,2,
		109,81,0,122,69,29,34,2,36,203,0,176,14,0,
		109,67,0,92,2,20,2,176,69,0,109,70,0,106,
		9,64,82,32,57,57,57,57,57,0,20,2,36,204,
		0,109,70,0,83,71,0,36,205,0,92,2,83,73,
		0,36,206,0,176,14,0,109,67,0,92,15,20,2,
		176,15,0,106,9,50,111,46,80,114,111,112,46,0,
		20,1,36,207,0,109,81,0,83,82,0,36,208,0,
		176,6,0,106,2,53,0,20,1,36,209,0,176,63,
		0,109,82,0,100,100,20,3,36,210,0,176,14,0,
		109,67,0,92,25,20,2,176,15,0,109,75,0,20,
		1,36,211,0,109,13,0,122,5,28,83,36,212,0,
		176,6,0,106,2,55,0,20,1,36,213,0,176,9,
		0,20,0,36,214,0,176,63,0,176,32,0,109,12,
		0,92,5,12,2,109,29,0,72,109,71,0,72,176,
		32,0,109,73,0,122,12,2,72,100,100,20,3,36,
		215,0,176,14,0,109,67,0,92,75,20,2,176,15,
		0,109,76,0,20,1,25,81,36,217,0,176,6,0,
		106,2,55,0,20,1,36,218,0,176,9,0,20,0,
		36,219,0,176,63,0,176,32,0,109,12,0,92,5,
		12,2,109,28,0,72,109,71,0,72,176,32,0,109,
		73,0,122,12,2,72,100,100,20,3,36,220,0,176,
		14,0,109,67,0,92,75,20,2,176,15,0,109,76,
		0,20,1,36,222,0,176,6,0,106,2,49,0,20,
		1,36,223,0,176,14,0,109,67,0,92,87,20,2,
		176,15,0,109,83,0,28,10,106,4,83,105,109,0,
		25,8,106,4,78,97,111,0,20,1,36,224,0,176,
		6,0,106,2,53,0,20,1,36,225,0,109,78,0,
		106,2,82,0,5,28,69,36,226,0,176,14,0,109,
		67,0,92,97,20,2,176,69,0,109,79,0,106,20,
		64,82,32,40,88,88,41,32,88,88,88,88,88,45,
		88,88,88,88,88,0,20,2,36,227,0,176,14,0,
		109,67,0,92,118,20,2,176,15,0,106,2,82,0,
		20,1,25,67,36,229,0,176,14,0,109,67,0,92,
		97,20,2,176,69,0,109,80,0,106,20,64,82,32,
		40,88,88,41,32,88,88,88,88,88,45,88,88,88,
		88,88,0,20,2,36,230,0,176,14,0,109,67,0,
		92,118,20,2,176,15,0,106,2,67,0,20,1,36,
		232,0,176,6,0,106,2,49,0,20,1,36,233,0,
		109,67,0,122,72,83,67,0,36,234,0,109,47,0,
		122,72,83,47,0,36,236,0,176,84,0,122,20,1,
		36,237,0,176,85,0,20,0,36,238,0,109,86,0,
		28,50,36,239,0,176,56,0,92,20,106,7,83,67,
		82,69,69,78,0,20,2,36,240,0,176,87,0,20,
		0,176,6,0,106,2,49,0,20,1,176,88,0,100,
		20,1,36,241,0,100,110,7,36,243,0,176,57,0,
		12,0,29,19,251,36,244,0,109,67,0,92,3,72,
		83,67,0,36,245,0,176,14,0,109,67,0,92,25,
		20,2,176,15,0,106,21,84,111,116,97,108,32,100,
		101,32,85,110,105,100,97,100,101,115,32,58,32,0,
		176,32,0,109,47,0,92,7,12,2,72,20,1,36,
		246,0,26,205,250,36,248,0,176,14,0,92,56,121,
		20,2,176,15,0,176,66,0,106,2,61,0,93,135,
		0,12,2,20,1,36,249,0,176,14,0,92,57,92,
		95,20,2,176,15,0,109,89,0,20,1,36,250,0,
		176,14,0,122,121,20,2,176,15,0,106,2,32,0,
		109,90,0,72,20,1,26,49,248,36,252,0,176,56,
		0,92,24,106,1,0,20,2,36,253,0,176,56,0,
		92,20,106,7,83,67,82,69,69,78,0,20,2,36,
		254,0,176,87,0,20,0,36,255,0,176,91,0,121,
		121,176,2,0,12,0,176,3,0,12,0,109,4,0,
		20,5,36,0,1,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

