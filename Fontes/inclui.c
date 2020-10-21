/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/inclui.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( INCLUI );
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
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( TESTACPF );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( TESTACGC );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( READKILL );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBCOMMITALL );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( F_LACESSO );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_INCLUI )
{ "INCLUI", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( INCLUI )}, NULL },
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
{ "ECLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECPF_CGC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ERG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EUF_RG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EEMISSAO_RG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EORGAO_RG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESEXO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EEND_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EBAIRRO_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECIDADE_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EESTADO_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECEP_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETEL_R_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETEL_R_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENASCIMENTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMAIL_PROP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TESTACPF", {HB_FS_PUBLIC}, {HB_FUNCNAME( TESTACPF )}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "TESTACGC", {HB_FS_PUBLIC}, {HB_FUNCNAME( TESTACGC )}, NULL },
{ "EPROFISSAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EEMPRESA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EEND_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EBAIRRO_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECIDADE_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EESTADO_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECEP_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETEL_C_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETEL_C_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONJUGE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECPF_CONJ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETEL_CONJ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMAIL_CONJ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENASC_CONJ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMAIL_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "EOBSERVA_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOBSERVA_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOBSERVA_3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOBSERVA_4", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EOBSERVA_5", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENUM_CLIE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "READKILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( READKILL )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "CLIENTE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CPF_CGC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "RG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "UF_RG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMISSAO_RG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ORGAO_RG", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SEXO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "END_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "BAIRRO_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CIDADE_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CEP_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_R_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESTADO_R", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ESTADO_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_R_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "END_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "BAIRRO_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CIDADE_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CEP_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_C_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_C_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "OBSERVA_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "OBSERVA_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "OBSERVA_3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "OBSERVA_4", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "OBSERVA_5", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "OPCAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMPRESA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NASCIMENTO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PROFISSAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CONJUGE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "NASC_CONJ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CPF_CONJ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_CONJ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MAIL_CONJ", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MAIL_PROP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MAIL_C", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBCOMMITALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCOMMITALL )}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_INCLUI, "C:/sis/SAN/xhb/inclui.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_INCLUI
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_INCLUI )
   #include "hbiniseg.h"
#endif

HB_FUNC( INCLUI )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,105,110,99,108,117,105,46,112,114,103,58,73,
		78,67,76,85,73,0,36,13,0,176,1,0,121,121,
		176,2,0,12,0,176,3,0,12,0,12,4,83,4,
		0,36,14,0,176,5,0,106,21,73,78,67,76,85,
		83,65,79,32,68,69,32,67,76,73,69,78,84,69,
		83,0,20,1,36,15,0,176,6,0,106,2,53,0,
		20,1,36,16,0,176,7,0,9,100,106,8,67,79,
		78,84,65,84,79,0,100,100,9,20,6,176,8,0,
		106,8,67,79,78,84,65,84,79,0,20,1,176,8,
		0,106,9,78,85,77,95,67,76,73,69,0,20,1,
		36,17,0,176,9,0,20,0,36,18,0,36,19,0,
		176,10,0,20,0,36,20,0,176,11,0,106,9,87,
		47,78,44,43,87,47,82,0,20,1,36,21,0,176,
		12,0,92,50,12,1,83,13,0,36,22,0,176,14,
		0,92,5,92,2,20,2,176,15,0,106,15,67,108,
		105,101,110,116,101,32,32,32,32,32,58,32,0,20,
		1,176,16,0,176,17,0,12,0,176,18,0,12,0,
		122,72,20,2,176,19,0,109,20,0,176,21,0,100,
		106,9,101,67,76,73,69,78,84,69,0,106,54,64,
		33,32,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,88,88,88,88,0,100,100,12,
		5,20,2,48,22,0,176,23,0,109,20,0,12,1,
		112,0,73,36,23,0,176,24,0,109,20,0,100,100,
		100,100,100,100,20,7,4,0,0,83,20,0,109,20,
		0,73,36,24,0,176,6,0,106,2,53,0,20,1,
		36,25,0,176,9,0,20,0,36,26,0,176,25,0,
		109,13,0,12,1,83,13,0,36,27,0,109,13,0,
		176,12,0,92,50,12,1,5,32,225,22,36,30,0,
		176,26,0,109,13,0,100,100,20,3,36,31,0,176,
		27,0,12,0,29,140,0,36,32,0,176,28,0,106,
		3,48,49,0,20,1,36,33,0,176,29,0,106,18,
		67,108,105,101,110,116,101,32,106,160,32,101,120,105,
		115,116,101,0,92,3,20,2,36,34,0,176,28,0,
		106,3,48,49,0,20,1,36,35,0,176,30,0,106,
		57,68,101,115,101,106,97,32,67,97,100,97,115,116,
		114,97,114,32,79,117,116,114,111,32,67,108,105,101,
		110,116,101,32,99,111,109,32,111,32,77,101,115,109,
		111,32,78,79,77,69,32,40,83,47,78,41,32,63,
		32,0,20,1,36,36,0,109,31,0,106,2,78,0,
		5,32,85,254,36,40,0,176,12,0,92,18,12,1,
		83,32,0,36,41,0,176,12,0,92,14,12,1,83,
		33,0,36,42,0,176,12,0,92,2,12,1,83,34,
		0,36,43,0,134,0,0,0,0,83,35,0,36,44,
		0,176,12,0,92,10,12,1,83,36,0,36,45,0,
		176,12,0,122,12,1,83,37,0,36,46,0,176,12,
		0,92,50,12,1,83,38,0,36,47,0,176,12,0,
		92,20,12,1,83,39,0,36,48,0,176,12,0,92,
		20,12,1,83,40,0,36,49,0,176,12,0,92,2,
		12,1,83,41,0,36,50,0,176,12,0,92,8,12,
		1,83,42,0,36,51,0,176,12,0,92,12,12,1,
		83,43,0,36,52,0,176,12,0,92,12,12,1,83,
		44,0,36,53,0,134,0,0,0,0,83,45,0,36,
		54,0,176,12,0,92,50,12,1,83,46,0,36,55,
		0,176,14,0,92,7,92,2,20,2,176,15,0,106,
		17,67,80,70,32,111,117,32,67,71,67,32,32,32,
		32,58,32,0,20,1,176,16,0,176,17,0,12,0,
		176,18,0,12,0,122,72,20,2,176,19,0,109,20,
		0,176,21,0,100,106,9,101,67,80,70,95,67,71,
		67,0,100,100,100,12,5,20,2,48,22,0,176,23,
		0,109,20,0,12,1,112,0,73,36,56,0,176,14,
		0,92,8,92,2,20,2,176,15,0,106,17,82,71,
		32,32,32,32,32,32,32,32,32,32,32,32,58,32,
		0,20,1,176,16,0,176,17,0,12,0,176,18,0,
		12,0,122,72,20,2,176,19,0,109,20,0,176,21,
		0,100,106,4,101,82,71,0,100,100,100,12,5,20,
		2,48,22,0,176,23,0,109,20,0,12,1,112,0,
		73,36,57,0,176,14,0,92,8,92,44,20,2,176,
		15,0,106,6,85,70,32,58,32,0,20,1,176,16,
		0,176,17,0,12,0,176,18,0,12,0,122,72,20,
		2,176,19,0,109,20,0,176,21,0,100,106,7,101,
		85,70,95,82,71,0,106,5,64,33,88,88,0,100,
		100,12,5,20,2,48,22,0,176,23,0,109,20,0,
		12,1,112,0,73,36,58,0,176,14,0,92,9,92,
		2,20,2,176,15,0,106,17,69,109,105,115,115,198,
		111,32,32,32,32,32,32,32,58,32,0,20,1,176,
		16,0,176,17,0,12,0,176,18,0,12,0,122,72,
		20,2,176,19,0,109,20,0,176,21,0,100,106,12,
		101,69,77,73,83,83,65,79,95,82,71,0,100,100,
		100,12,5,20,2,48,22,0,176,23,0,109,20,0,
		12,1,112,0,73,36,59,0,176,14,0,92,9,92,
		41,20,2,176,15,0,106,9,79,114,103,131,111,32,
		58,32,0,20,1,176,16,0,176,17,0,12,0,176,
		18,0,12,0,122,72,20,2,176,19,0,109,20,0,
		176,21,0,100,106,10,101,79,82,71,65,79,95,82,
		71,0,100,100,100,12,5,20,2,48,22,0,176,23,
		0,109,20,0,12,1,112,0,73,36,60,0,176,14,
		0,92,10,92,2,20,2,176,15,0,106,17,83,101,
		120,111,32,40,77,47,70,47,79,41,32,32,58,32,
		0,20,1,176,16,0,176,17,0,12,0,176,18,0,
		12,0,122,72,20,2,176,19,0,109,20,0,176,21,
		0,100,106,6,101,83,69,88,79,0,106,4,64,33,
		88,0,90,47,51,67,58,47,115,105,115,47,83,65,
		78,47,120,104,98,47,105,110,99,108,117,105,46,112,
		114,103,58,73,78,67,76,85,73,0,109,37,0,106,
		4,77,70,79,0,24,6,100,12,5,20,2,36,60,
		0,48,22,0,176,23,0,109,20,0,12,1,112,0,
		73,36,61,0,176,14,0,92,10,92,36,20,2,176,
		15,0,106,14,78,97,115,99,105,109,101,110,116,111,
		32,58,32,0,20,1,176,16,0,176,17,0,12,0,
		176,18,0,12,0,122,72,20,2,176,19,0,109,20,
		0,176,21,0,100,106,12,101,78,65,83,67,73,77,
		69,78,84,79,0,100,100,100,12,5,20,2,48,22,
		0,176,23,0,109,20,0,12,1,112,0,73,36,62,
		0,176,24,0,109,20,0,100,100,100,100,100,100,20,
		7,4,0,0,83,20,0,109,20,0,73,36,63,0,
		109,37,0,106,2,77,0,5,31,12,109,37,0,106,
		2,70,0,5,28,93,36,64,0,176,47,0,109,32,
		0,12,1,31,75,36,65,0,176,28,0,106,3,48,
		49,0,20,1,36,66,0,176,29,0,106,12,65,116,
		101,110,135,198,111,32,33,33,33,0,92,3,20,2,
		36,67,0,176,48,0,106,18,67,80,70,32,110,97,
		111,32,99,111,110,102,101,114,101,32,33,0,20,1,
		36,68,0,176,49,0,20,0,36,69,0,25,91,36,
		71,0,176,50,0,109,32,0,12,1,31,75,36,72,
		0,176,28,0,106,3,48,49,0,20,1,36,73,0,
		176,29,0,106,12,65,116,101,110,135,198,111,32,33,
		33,33,0,92,3,20,2,36,74,0,176,48,0,106,
		18,67,71,67,32,110,97,111,32,99,111,110,102,101,
		114,101,32,33,0,20,1,36,75,0,176,49,0,20,
		0,36,78,0,176,11,0,106,5,87,47,78,43,0,
		20,1,36,79,0,176,14,0,92,12,92,2,20,2,
		176,15,0,106,21,69,78,68,69,82,69,67,79,32,
		82,69,83,73,68,69,78,67,73,65,76,0,20,1,
		36,80,0,176,11,0,106,9,87,47,78,44,43,87,
		47,82,0,20,1,36,81,0,176,14,0,92,14,92,
		2,20,2,176,15,0,106,17,69,110,100,101,114,101,
		135,111,32,32,32,32,32,32,58,32,0,20,1,176,
		16,0,176,17,0,12,0,176,18,0,12,0,122,72,
		20,2,176,19,0,109,20,0,176,21,0,100,106,7,
		101,69,78,68,95,82,0,100,100,100,12,5,20,2,
		48,22,0,176,23,0,109,20,0,12,1,112,0,73,
		36,82,0,176,14,0,92,15,92,2,20,2,176,15,
		0,106,17,66,97,105,114,114,111,32,32,32,32,32,
		32,32,32,58,32,0,20,1,176,16,0,176,17,0,
		12,0,176,18,0,12,0,122,72,20,2,176,19,0,
		109,20,0,176,21,0,100,106,10,101,66,65,73,82,
		82,79,95,82,0,100,100,100,12,5,20,2,48,22,
		0,176,23,0,109,20,0,12,1,112,0,73,36,83,
		0,176,14,0,92,16,92,2,20,2,176,15,0,106,
		17,67,105,100,97,100,101,32,32,32,32,32,32,32,
		32,58,32,0,20,1,176,16,0,176,17,0,12,0,
		176,18,0,12,0,122,72,20,2,176,19,0,109,20,
		0,176,21,0,100,106,10,101,67,73,68,65,68,69,
		95,82,0,100,100,100,12,5,20,2,48,22,0,176,
		23,0,109,20,0,12,1,112,0,73,36,84,0,176,
		14,0,92,17,92,2,20,2,176,15,0,106,17,69,
		115,116,97,100,111,32,32,32,32,32,32,32,32,58,
		32,0,20,1,176,16,0,176,17,0,12,0,176,18,
		0,12,0,122,72,20,2,176,19,0,109,20,0,176,
		21,0,100,106,10,101,69,83,84,65,68,79,95,82,
		0,106,5,64,33,88,88,0,100,100,12,5,20,2,
		48,22,0,176,23,0,109,20,0,12,1,112,0,73,
		36,85,0,176,14,0,92,18,92,2,20,2,176,15,
		0,106,17,67,69,80,32,32,32,32,32,32,32,32,
		32,32,32,58,32,0,20,1,176,16,0,176,17,0,
		12,0,176,18,0,12,0,122,72,20,2,176,19,0,
		109,20,0,176,21,0,100,106,7,101,67,69,80,95,
		82,0,106,13,64,82,32,57,57,57,57,57,45,57,
		57,57,0,100,100,12,5,20,2,48,22,0,176,23,
		0,109,20,0,12,1,112,0,73,36,86,0,176,14,
		0,92,19,92,2,20,2,176,15,0,106,17,84,101,
		108,101,102,111,110,101,32,32,35,32,49,32,58,32,
		0,20,1,176,16,0,176,17,0,12,0,176,18,0,
		12,0,122,72,20,2,176,19,0,109,20,0,176,21,
		0,100,106,9,101,84,69,76,95,82,95,49,0,106,
		20,64,82,32,40,88,88,41,32,88,88,88,88,88,
		45,88,88,88,88,88,0,100,100,12,5,20,2,48,
		22,0,176,23,0,109,20,0,12,1,112,0,73,36,
		87,0,176,14,0,92,20,92,2,20,2,176,15,0,
		106,17,84,101,108,101,102,111,110,101,32,32,35,32,
		50,32,58,32,0,20,1,176,16,0,176,17,0,12,
		0,176,18,0,12,0,122,72,20,2,176,19,0,109,
		20,0,176,21,0,100,106,9,101,84,69,76,95,82,
		95,50,0,106,20,64,82,32,40,88,88,41,32,88,
		88,88,88,88,45,88,88,88,88,88,0,100,100,12,
		5,20,2,48,22,0,176,23,0,109,20,0,12,1,
		112,0,73,36,88,0,176,14,0,92,21,92,2,20,
		2,176,15,0,106,17,69,45,77,97,105,108,32,32,
		32,32,32,32,32,32,58,32,0,20,1,176,16,0,
		176,17,0,12,0,176,18,0,12,0,122,72,20,2,
		176,19,0,109,20,0,176,21,0,100,106,11,101,77,
		65,73,76,95,80,82,79,80,0,100,100,100,12,5,
		20,2,48,22,0,176,23,0,109,20,0,12,1,112,
		0,73,36,89,0,176,24,0,109,20,0,100,100,100,
		100,100,100,20,7,4,0,0,83,20,0,109,20,0,
		73,36,90,0,176,48,0,106,44,80,114,101,115,115,
		105,111,110,101,32,81,117,97,108,113,117,101,114,32,
		84,101,99,108,97,32,80,97,114,97,32,67,111,110,
		116,105,110,117,97,114,32,46,46,46,0,20,1,36,
		91,0,176,49,0,20,0,36,92,0,176,28,0,106,
		3,48,49,0,20,1,36,93,0,176,12,0,92,30,
		12,1,83,51,0,36,94,0,176,12,0,92,50,12,
		1,83,52,0,36,95,0,176,12,0,92,50,12,1,
		83,53,0,36,96,0,176,12,0,92,20,12,1,83,
		54,0,36,97,0,176,12,0,92,20,12,1,83,55,
		0,36,98,0,176,12,0,92,2,12,1,83,56,0,
		36,99,0,176,12,0,92,15,12,1,83,57,0,36,
		100,0,176,12,0,92,40,12,1,83,58,0,36,101,
		0,176,12,0,92,40,12,1,83,59,0,36,102,0,
		176,12,0,122,12,1,83,60,0,36,103,0,176,12,
		0,92,50,12,1,83,61,0,36,104,0,176,12,0,
		92,14,12,1,83,62,0,36,105,0,176,12,0,92,
		40,12,1,83,63,0,36,106,0,176,12,0,92,50,
		12,1,83,64,0,36,107,0,134,0,0,0,0,83,
		65,0,36,108,0,176,12,0,92,50,12,1,83,66,
		0,36,109,0,176,67,0,92,6,122,92,21,92,79,
		20,4,176,16,0,92,6,122,20,2,36,110,0,176,
		11,0,106,5,87,47,78,43,0,20,1,36,111,0,
		176,14,0,92,7,92,2,20,2,176,15,0,106,19,
		69,78,68,69,82,69,67,79,32,67,79,77,69,82,
		67,73,65,76,0,20,1,36,112,0,176,11,0,106,
		9,87,47,78,44,43,87,47,82,0,20,1,36,114,
		0,176,14,0,92,9,92,2,20,2,176,15,0,106,
		17,69,109,112,114,101,115,97,32,32,32,32,32,32,
		32,58,32,0,20,1,176,16,0,176,17,0,12,0,
		176,18,0,12,0,122,72,20,2,176,19,0,109,20,
		0,176,21,0,100,106,9,101,69,77,80,82,69,83,
		65,0,100,100,100,12,5,20,2,48,22,0,176,23,
		0,109,20,0,12,1,112,0,73,36,115,0,176,14,
		0,92,10,92,2,20,2,176,15,0,106,17,80,114,
		111,102,105,115,115,97,111,32,32,32,32,32,58,32,
		0,20,1,176,16,0,176,17,0,12,0,176,18,0,
		12,0,122,72,20,2,176,19,0,109,20,0,176,21,
		0,100,106,11,101,80,82,79,70,73,83,83,65,79,
		0,100,100,100,12,5,20,2,48,22,0,176,23,0,
		109,20,0,12,1,112,0,73,36,116,0,176,14,0,
		92,11,92,2,20,2,176,15,0,106,17,69,110,100,
		101,114,101,135,111,32,32,32,32,32,32,58,32,0,
		20,1,176,16,0,176,17,0,12,0,176,18,0,12,
		0,122,72,20,2,176,19,0,109,20,0,176,21,0,
		100,106,7,101,69,78,68,95,67,0,100,100,100,12,
		5,20,2,48,22,0,176,23,0,109,20,0,12,1,
		112,0,73,36,117,0,176,14,0,92,12,92,2,20,
		2,176,15,0,106,17,66,97,105,114,114,111,32,32,
		32,32,32,32,32,32,58,32,0,20,1,176,16,0,
		176,17,0,12,0,176,18,0,12,0,122,72,20,2,
		176,19,0,109,20,0,176,21,0,100,106,10,101,66,
		65,73,82,82,79,95,67,0,100,100,100,12,5,20,
		2,48,22,0,176,23,0,109,20,0,12,1,112,0,
		73,36,118,0,176,14,0,92,13,92,2,20,2,176,
		15,0,106,17,67,105,100,97,100,101,32,32,32,32,
		32,32,32,32,58,32,0,20,1,176,16,0,176,17,
		0,12,0,176,18,0,12,0,122,72,20,2,176,19,
		0,109,20,0,176,21,0,100,106,10,101,67,73,68,
		65,68,69,95,67,0,100,100,100,12,5,20,2,48,
		22,0,176,23,0,109,20,0,12,1,112,0,73,36,
		119,0,176,14,0,92,14,92,2,20,2,176,15,0,
		106,17,69,115,116,97,100,111,32,32,32,32,32,32,
		32,32,58,32,0,20,1,176,16,0,176,17,0,12,
		0,176,18,0,12,0,122,72,20,2,176,19,0,109,
		20,0,176,21,0,100,106,10,101,69,83,84,65,68,
		79,95,67,0,106,5,64,33,88,88,0,100,100,12,
		5,20,2,48,22,0,176,23,0,109,20,0,12,1,
		112,0,73,36,120,0,176,14,0,92,15,92,2,20,
		2,176,15,0,106,17,67,69,80,32,32,32,32,32,
		32,32,32,32,32,32,58,32,0,20,1,176,16,0,
		176,17,0,12,0,176,18,0,12,0,122,72,20,2,
		176,19,0,109,20,0,176,21,0,100,106,7,101,67,
		69,80,95,67,0,106,13,64,82,32,57,57,57,57,
		57,45,57,57,57,0,100,100,12,5,20,2,48,22,
		0,176,23,0,109,20,0,12,1,112,0,73,36,121,
		0,176,14,0,92,16,92,2,20,2,176,15,0,106,
		17,84,101,108,101,102,111,110,101,32,32,35,32,49,
		32,58,32,0,20,1,176,16,0,176,17,0,12,0,
		176,18,0,12,0,122,72,20,2,176,19,0,109,20,
		0,176,21,0,100,106,9,101,84,69,76,95,67,95,
		49,0,106,45,64,82,32,40,88,88,41,32,88,88,
		88,88,88,45,88,88,88,88,88,32,45,32,40,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,41,0,100,100,12,5,20,2,48,
		22,0,176,23,0,109,20,0,12,1,112,0,73,36,
		122,0,176,14,0,92,17,92,2,20,2,176,15,0,
		106,17,84,101,108,101,102,111,110,101,32,32,35,32,
		50,32,58,32,0,20,1,176,16,0,176,17,0,12,
		0,176,18,0,12,0,122,72,20,2,176,19,0,109,
		20,0,176,21,0,100,106,9,101,84,69,76,95,67,
		95,50,0,106,45,64,82,32,40,88,88,41,32,88,
		88,88,88,88,45,88,88,88,88,88,32,45,32,40,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,88,88,88,41,0,100,100,12,5,20,2,
		48,22,0,176,23,0,109,20,0,12,1,112,0,73,
		36,123,0,176,14,0,92,18,92,2,20,2,176,15,
		0,106,17,69,45,77,97,105,108,32,32,32,32,32,
		32,32,32,58,32,0,20,1,176,16,0,176,17,0,
		12,0,176,18,0,12,0,122,72,20,2,176,19,0,
		109,20,0,176,21,0,100,106,8,101,77,65,73,76,
		95,67,0,100,100,100,12,5,20,2,48,22,0,176,
		23,0,109,20,0,12,1,112,0,73,36,124,0,176,
		14,0,92,20,92,2,20,2,176,15,0,106,53,67,
		111,114,114,101,115,112,111,110,100,101,110,99,105,97,
		32,110,97,32,82,101,115,105,100,101,110,99,105,97,
		32,111,117,32,69,115,99,114,105,116,111,114,105,111,
		32,40,82,47,69,41,32,63,32,0,20,1,176,16,
		0,176,17,0,12,0,176,18,0,12,0,122,72,20,
		2,176,19,0,109,20,0,176,21,0,100,106,7,101,
		79,80,67,65,79,0,106,4,64,33,88,0,90,46,
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,105,110,99,108,117,105,46,112,114,103,58,73,
		78,67,76,85,73,0,109,60,0,106,3,82,69,0,
		24,6,100,12,5,20,2,36,124,0,48,22,0,176,
		23,0,109,20,0,12,1,112,0,73,36,125,0,176,
		24,0,109,20,0,100,100,100,100,100,100,20,7,4,
		0,0,83,20,0,109,20,0,73,36,126,0,176,48,
		0,106,44,80,114,101,115,115,105,111,110,101,32,81,
		117,97,108,113,117,101,114,32,84,101,99,108,97,32,
		80,97,114,97,32,70,105,110,97,108,105,122,97,114,
		32,46,46,46,0,20,1,36,127,0,176,49,0,20,
		0,36,128,0,176,12,0,92,80,12,1,83,68,0,
		36,129,0,176,12,0,92,80,12,1,83,69,0,36,
		130,0,176,12,0,92,80,12,1,83,70,0,36,131,
		0,176,12,0,92,80,12,1,83,71,0,36,132,0,
		176,12,0,92,80,12,1,83,72,0,36,133,0,176,
		67,0,92,7,122,92,21,92,79,20,4,176,16,0,
		92,7,122,20,2,36,134,0,176,11,0,106,5,87,
		47,78,43,0,20,1,36,135,0,176,14,0,92,7,
		92,2,20,2,176,15,0,106,19,79,66,83,69,82,
		86,65,67,79,69,83,32,71,69,82,65,73,83,0,
		20,1,36,136,0,176,11,0,106,9,87,47,78,44,
		43,87,47,82,0,20,1,36,138,0,176,16,0,92,
		8,121,20,2,176,19,0,109,20,0,176,21,0,100,
		106,11,101,79,66,83,69,82,86,65,95,49,0,100,
		100,100,12,5,20,2,48,22,0,176,23,0,109,20,
		0,12,1,112,0,73,36,139,0,176,16,0,92,9,
		121,20,2,176,19,0,109,20,0,176,21,0,100,106,
		11,101,79,66,83,69,82,86,65,95,50,0,100,100,
		100,12,5,20,2,48,22,0,176,23,0,109,20,0,
		12,1,112,0,73,36,140,0,176,16,0,92,10,121,
		20,2,176,19,0,109,20,0,176,21,0,100,106,11,
		101,79,66,83,69,82,86,65,95,51,0,100,100,100,
		12,5,20,2,48,22,0,176,23,0,109,20,0,12,
		1,112,0,73,36,141,0,176,16,0,92,11,121,20,
		2,176,19,0,109,20,0,176,21,0,100,106,11,101,
		79,66,83,69,82,86,65,95,52,0,100,100,100,12,
		5,20,2,48,22,0,176,23,0,109,20,0,12,1,
		112,0,73,36,142,0,176,16,0,92,12,121,20,2,
		176,19,0,109,20,0,176,21,0,100,106,11,101,79,
		66,83,69,82,86,65,95,53,0,100,100,100,12,5,
		20,2,48,22,0,176,23,0,109,20,0,12,1,112,
		0,73,36,143,0,176,24,0,109,20,0,100,100,100,
		100,100,100,20,7,4,0,0,83,20,0,109,20,0,
		73,36,144,0,176,11,0,106,5,87,47,78,43,0,
		20,1,36,145,0,176,14,0,92,14,92,2,20,2,
		176,15,0,106,17,68,65,68,79,83,32,68,79,32,
		67,79,78,74,85,71,69,0,20,1,36,146,0,176,
		11,0,106,9,87,47,78,44,43,87,47,82,0,20,
		1,36,147,0,176,14,0,92,15,92,2,20,2,176,
		15,0,106,17,78,111,109,101,32,67,111,110,106,117,
		103,101,32,32,58,32,0,20,1,176,16,0,176,17,
		0,12,0,176,18,0,12,0,122,72,20,2,176,19,
		0,109,20,0,176,21,0,100,106,9,101,67,79,78,
		74,85,71,69,0,100,100,100,12,5,20,2,48,22,
		0,176,23,0,109,20,0,12,1,112,0,73,36,148,
		0,176,14,0,92,16,92,2,20,2,176,15,0,106,
		17,68,97,116,97,32,78,97,115,99,105,109,46,32,
		32,58,32,0,20,1,176,16,0,176,17,0,12,0,
		176,18,0,12,0,122,72,20,2,176,19,0,109,20,
		0,176,21,0,100,106,11,101,78,65,83,67,95,67,
		79,78,74,0,100,100,100,12,5,20,2,48,22,0,
		176,23,0,109,20,0,12,1,112,0,73,36,149,0,
		176,14,0,92,17,92,2,20,2,176,15,0,106,17,
		67,80,70,32,67,111,110,106,117,103,101,32,32,32,
		58,32,0,20,1,176,16,0,176,17,0,12,0,176,
		18,0,12,0,122,72,20,2,176,19,0,109,20,0,
		176,21,0,100,106,10,101,67,80,70,95,67,79,78,
		74,0,100,100,100,12,5,20,2,48,22,0,176,23,
		0,109,20,0,12,1,112,0,73,36,150,0,176,14,
		0,92,18,92,2,20,2,176,15,0,106,17,84,101,
		108,101,102,111,110,101,32,67,111,110,106,32,58,32,
		0,20,1,176,16,0,176,17,0,12,0,176,18,0,
		12,0,122,72,20,2,176,19,0,109,20,0,176,21,
		0,100,106,10,101,84,69,76,95,67,79,78,74,0,
		106,45,64,82,32,40,88,88,41,32,88,88,88,88,
		88,45,88,88,88,88,88,32,45,32,40,88,88,88,
		88,88,88,88,88,88,88,88,88,88,88,88,88,88,
		88,88,88,41,0,100,100,12,5,20,2,48,22,0,
		176,23,0,109,20,0,12,1,112,0,73,36,151,0,
		176,14,0,92,19,92,2,20,2,176,15,0,106,17,
		101,45,109,97,105,108,32,32,32,32,32,32,32,32,
		58,32,0,20,1,176,16,0,176,17,0,12,0,176,
		18,0,12,0,122,72,20,2,176,19,0,109,20,0,
		176,21,0,100,106,11,101,77,65,73,76,95,67,79,
		78,74,0,100,100,100,12,5,20,2,48,22,0,176,
		23,0,109,20,0,12,1,112,0,73,36,152,0,176,
		24,0,109,20,0,100,100,100,100,100,100,20,7,4,
		0,0,83,20,0,109,20,0,73,36,153,0,176,6,
		0,106,2,57,0,20,1,36,154,0,176,7,0,9,
		100,106,7,83,84,65,84,85,83,0,100,100,9,20,
		6,36,155,0,176,9,0,20,0,36,156,0,109,73,
		0,83,74,0,36,157,0,176,14,0,92,21,92,2,
		20,2,176,15,0,106,17,78,117,109,46,67,108,105,
		101,110,116,101,32,32,32,58,32,0,20,1,176,16,
		0,176,17,0,12,0,176,18,0,12,0,122,72,20,
		2,176,19,0,109,20,0,176,21,0,100,106,10,101,
		78,85,77,95,67,76,73,69,0,106,14,64,82,32,
		57,57,57,57,57,57,57,57,57,57,0,100,100,12,
		5,20,2,48,22,0,176,23,0,109,20,0,12,1,
		112,0,73,36,158,0,176,75,0,120,20,1,4,0,
		0,83,20,0,109,20,0,73,36,159,0,176,6,0,
		106,2,53,0,20,1,36,160,0,176,48,0,106,44,
		80,114,101,115,115,105,111,110,101,32,81,117,97,108,
		113,117,101,114,32,84,101,99,108,97,32,80,97,114,
		97,32,70,105,110,97,108,105,122,97,114,32,46,46,
		46,0,20,1,36,161,0,176,49,0,20,0,36,162,
		0,176,28,0,106,3,48,49,0,20,1,36,163,0,
		176,30,0,106,18,67,111,110,102,105,114,109,97,32,
		40,83,47,78,41,32,63,32,0,20,1,36,164,0,
		109,31,0,106,2,83,0,5,29,60,234,36,165,0,
		176,48,0,106,22,71,82,65,86,65,78,68,79,44,
		32,65,71,85,65,82,68,69,32,46,46,46,0,20,
		1,36,166,0,176,6,0,106,2,53,0,20,1,36,
		167,0,176,76,0,20,0,36,168,0,176,77,0,12,
		0,29,24,1,36,171,0,109,13,0,78,78,0,109,
		32,0,78,79,0,109,33,0,78,80,0,36,174,0,
		109,34,0,78,81,0,109,35,0,78,82,0,109,36,
		0,78,83,0,36,177,0,109,37,0,78,84,0,109,
		38,0,78,85,0,109,39,0,78,86,0,36,180,0,
		109,40,0,78,87,0,109,42,0,78,88,0,109,43,
		0,78,89,0,36,182,0,109,41,0,78,90,0,109,
		56,0,78,91,0,36,185,0,109,44,0,78,92,0,
		109,53,0,78,93,0,109,54,0,78,94,0,36,188,
		0,109,55,0,78,95,0,109,57,0,78,96,0,109,
		58,0,78,97,0,36,190,0,109,59,0,78,98,0,
		109,68,0,78,99,0,36,193,0,109,69,0,78,100,
		0,109,70,0,78,101,0,109,71,0,78,102,0,36,
		196,0,109,72,0,78,103,0,109,60,0,78,104,0,
		109,52,0,78,105,0,36,199,0,109,45,0,78,106,
		0,109,51,0,78,107,0,109,61,0,78,108,0,36,
		202,0,109,65,0,78,109,0,109,62,0,78,110,0,
		109,63,0,78,111,0,36,205,0,109,64,0,78,112,
		0,109,46,0,78,113,0,109,66,0,78,114,0,36,
		206,0,176,115,0,20,0,36,207,0,176,116,0,20,
		0,36,209,0,176,6,0,106,2,57,0,20,1,36,
		210,0,176,7,0,9,100,106,7,83,84,65,84,85,
		83,0,100,100,9,20,6,36,211,0,176,9,0,20,
		0,36,212,0,176,77,0,12,0,28,30,36,213,0,
		109,73,0,83,74,0,36,214,0,109,74,0,122,72,
		78,73,0,36,215,0,176,116,0,20,0,36,217,0,
		176,6,0,106,2,53,0,20,1,36,218,0,176,77,
		0,12,0,28,19,36,219,0,109,74,0,78,73,0,
		36,220,0,176,116,0,20,0,36,222,0,176,117,0,
		109,118,0,106,18,67,65,68,65,83,84,82,79,32,
		67,76,73,69,78,84,69,83,0,106,22,67,65,68,
		65,83,84,82,79,32,67,76,73,69,78,84,69,32,
		78,111,46,32,0,176,119,0,176,120,0,109,74,0,
		92,10,12,2,12,1,72,20,3,36,223,0,176,6,
		0,106,2,53,0,20,1,36,224,0,26,24,232,36,
		226,0,176,121,0,121,121,176,2,0,12,0,176,3,
		0,12,0,109,4,0,20,5,36,227,0,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,105,110,99,108,117,105,46,112,114,103,58,40,
		95,73,78,73,84,76,73,78,69,83,41,0,106,2,
		67,0,92,8,106,29,224,255,207,31,255,255,255,191,
		207,255,255,255,255,253,255,255,253,255,255,255,73,82,
		82,146,228,254,222,13,0,4,3,0,4,1,0,110,
		7
	};

	hb_vmExecute( pcode, symbols );
}
