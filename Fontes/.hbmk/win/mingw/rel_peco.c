/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/rel_peco.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( REL_PECO );
HB_FUNC_EXTERN( PTIT );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( DBSELECTAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( PLIMPA );
HB_FUNC_EXTERN( SETCOLOR );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DEVOUT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( MAXROW );
HB_FUNC_EXTERN( MAXCOL );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( PIMPR );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( DEVOUTPICT );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( LENNUM );
HB_FUNC_EXTERN( INT );
HB_FUNC_EXTERN( __SETFORMAT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_REL_PECO )
{ "REL_PECO", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( REL_PECO )}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "ECOTACAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PLIMPA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLIMPA )}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCOLOR )}, NULL },
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
{ "DBCLOSEALL", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBCLOSEALL )}, NULL },
{ "RESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( RESTSCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "SITUACAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "FORNECED_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORN_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FORNECED_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORN_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FORNECED_3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORN_3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FORNECED_4", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORN_4", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FORNECED_5", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORN_5", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ECONDIC_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONDIC_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONDIC_3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONDIC_4", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONDIC_5", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_07", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_07", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_08", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_08", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_09", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_09", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_10", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_10", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_11", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_11", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_12", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_12", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_13", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_13", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_14", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_14", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_15", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_15", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_16", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_16", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_17", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_17", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_18", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_18", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_19", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_19", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_20", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_20", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_21", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_07", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_07", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_08", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_08", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_09", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_09", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_10", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_10", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_11", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_11", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_12", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_12", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_13", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_13", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_14", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_14", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_15", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_15", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_16", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_16", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_17", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_17", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_18", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_18", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_19", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_19", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_20", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_20", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_21", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_01_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_01_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_01_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_01_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_01_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_02_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_02_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_02_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_02_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_02_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_03_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_03_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_03_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_03_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_03_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_04_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_04_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_04_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_04_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_04_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_05_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_05_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_05_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_05_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_05_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_06_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_06_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_06_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_06_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_06_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_07_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_07_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_07_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_07_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_07_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_08_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_08_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_08_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_08_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_08_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_09_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_09_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_09_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_09_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_09_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_10_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_10_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_10_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_10_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_10_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_11_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_11_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_11_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_11_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_11_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_12_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_12_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_12_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_12_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_12_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_13_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_13_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_13_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_13_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_13_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_14_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_14_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_14_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_14_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_14_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_15_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_15_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_15_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_15_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_15_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_16_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_16_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_16_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_16_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_16_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_17_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_17_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_17_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_17_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_17_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_18_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_18_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_18_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_18_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_18_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_19_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_19_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_19_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_19_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_19_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_20_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_20_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_20_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_20_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_20_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EARQUIVO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "SISDRIVE_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISPASTA_REL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "PIMPR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIMPR )}, NULL },
{ "EFORNE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EAUX", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EAUX_FOR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "SISCONTATO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISEMAIL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DEVOUTPICT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DEVOUTPICT )}, NULL },
{ "SISTEL", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "NOME", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CONTATO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TEL_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "LIN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ETAM_PROD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENNUM", {HB_FS_PUBLIC}, {HB_FUNCNAME( LENNUM )}, NULL },
{ "INT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INT )}, NULL },
{ "ECAMPO_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECAMPO_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "TIPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MARCA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "UNIDADE", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISTIT3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SISLETRAN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ENOME_FORN", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__SETFORMAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __SETFORMAT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_REL_PECO, "C:/sis/SAN/xhb/rel_peco.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_REL_PECO
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_REL_PECO )
   #include "hbiniseg.h"
#endif

HB_FUNC( REL_PECO )
{
	static const HB_BYTE pcode[] =
	{
		36,14,0,176,1,0,106,28,82,69,76,65,84,79,
		82,73,79,32,80,69,68,73,68,79,32,68,69,32,
		67,79,84,65,67,65,79,0,20,1,36,15,0,176,
		2,0,106,3,48,49,0,20,1,36,16,0,176,3,
		0,106,2,49,0,20,1,36,17,0,176,4,0,9,
		100,106,8,67,79,84,65,67,65,79,0,100,100,9,
		20,6,176,5,0,106,8,67,79,84,65,67,65,79,
		0,20,1,36,18,0,176,6,0,20,0,36,19,0,
		176,3,0,106,2,50,0,20,1,36,20,0,176,4,
		0,9,100,106,9,65,82,81,95,84,73,80,79,0,
		100,100,9,20,6,176,5,0,106,9,65,82,81,95,
		84,73,80,79,0,20,1,36,21,0,176,6,0,20,
		0,36,22,0,176,3,0,106,2,51,0,20,1,36,
		23,0,176,4,0,9,100,106,9,65,82,81,95,73,
		84,69,77,0,100,100,9,20,6,176,5,0,106,9,
		65,82,81,95,73,84,69,77,0,20,1,176,5,0,
		106,9,73,84,69,77,95,68,69,83,0,20,1,36,
		24,0,176,6,0,20,0,36,25,0,176,3,0,106,
		2,52,0,20,1,36,26,0,176,4,0,9,100,106,
		9,70,79,82,78,69,67,69,68,0,100,100,9,20,
		6,176,5,0,106,9,70,79,82,78,69,67,69,68,
		0,20,1,36,27,0,176,6,0,20,0,36,29,0,
		176,7,0,92,20,106,7,83,67,82,69,69,78,0,
		20,2,36,30,0,121,83,8,0,36,31,0,176,3,
		0,106,2,49,0,20,1,36,32,0,176,9,0,20,
		0,36,33,0,176,10,0,106,9,87,47,78,44,43,
		87,47,82,0,20,1,36,34,0,176,11,0,92,5,
		92,2,20,2,176,12,0,106,15,67,111,116,97,135,
		198,111,32,32,32,32,32,58,32,0,20,1,176,13,
		0,176,14,0,12,0,176,15,0,12,0,122,72,20,
		2,176,16,0,109,17,0,176,18,0,100,106,9,101,
		67,79,84,65,67,65,79,0,106,11,64,82,32,57,
		57,57,44,57,57,57,0,100,100,12,5,20,2,48,
		19,0,176,20,0,109,17,0,12,1,112,0,73,36,
		35,0,176,21,0,109,17,0,100,100,100,100,100,100,
		20,7,4,0,0,83,17,0,109,17,0,73,36,36,
		0,109,8,0,121,5,28,39,36,37,0,176,22,0,
		20,0,36,38,0,176,23,0,121,121,176,24,0,12,
		0,176,25,0,12,0,109,26,0,20,5,36,39,0,
		100,110,7,36,41,0,176,3,0,106,2,49,0,20,
		1,36,42,0,176,6,0,20,0,36,43,0,176,27,
		0,109,8,0,100,100,20,3,36,44,0,176,28,0,
		12,0,31,51,36,45,0,176,2,0,106,3,48,49,
		0,20,1,36,46,0,176,29,0,106,18,67,162,100,
		105,103,111,32,78,198,111,32,101,120,105,115,116,101,
		0,92,3,20,2,36,47,0,26,179,254,36,49,0,
		176,30,0,12,0,28,36,36,50,0,106,18,80,69,
		68,73,68,79,32,70,79,82,78,69,67,69,68,79,
		82,0,78,31,0,36,51,0,176,32,0,20,0,36,
		53,0,109,33,0,83,34,0,36,54,0,109,35,0,
		83,36,0,36,55,0,109,37,0,83,38,0,36,56,
		0,109,39,0,83,40,0,36,57,0,109,41,0,83,
		42,0,36,58,0,176,43,0,92,60,12,1,83,44,
		0,36,59,0,176,43,0,92,60,12,1,83,45,0,
		36,60,0,176,43,0,92,60,12,1,83,46,0,36,
		61,0,176,43,0,92,60,12,1,83,47,0,36,62,
		0,176,43,0,92,60,12,1,83,48,0,36,63,0,
		109,49,0,83,50,0,36,64,0,109,51,0,83,52,
		0,36,65,0,109,53,0,83,54,0,36,66,0,109,
		55,0,83,56,0,36,67,0,109,57,0,83,58,0,
		36,68,0,109,59,0,83,60,0,36,69,0,109,61,
		0,83,62,0,36,70,0,109,63,0,83,64,0,36,
		71,0,109,65,0,83,66,0,36,72,0,109,67,0,
		83,68,0,36,73,0,109,69,0,83,70,0,36,74,
		0,109,71,0,83,72,0,36,75,0,109,73,0,83,
		74,0,36,76,0,109,75,0,83,76,0,36,77,0,
		109,77,0,83,78,0,36,78,0,109,79,0,83,80,
		0,36,79,0,109,81,0,83,82,0,36,80,0,109,
		83,0,83,84,0,36,81,0,109,85,0,83,86,0,
		36,82,0,109,87,0,83,88,0,36,83,0,121,83,
		89,0,36,84,0,109,90,0,83,91,0,36,85,0,
		109,92,0,83,93,0,36,86,0,109,94,0,83,95,
		0,36,87,0,109,96,0,83,97,0,36,88,0,109,
		98,0,83,99,0,36,89,0,109,100,0,83,101,0,
		36,90,0,109,102,0,83,103,0,36,91,0,109,104,
		0,83,105,0,36,92,0,109,106,0,83,107,0,36,
		93,0,109,108,0,83,109,0,36,94,0,109,110,0,
		83,111,0,36,95,0,109,112,0,83,113,0,36,96,
		0,109,114,0,83,115,0,36,97,0,109,116,0,83,
		117,0,36,98,0,109,118,0,83,119,0,36,99,0,
		109,120,0,83,121,0,36,100,0,109,122,0,83,123,
		0,36,101,0,109,124,0,83,125,0,36,102,0,109,
		126,0,83,127,0,36,103,0,109,128,0,83,129,0,
		36,104,0,121,83,130,0,36,105,0,121,83,131,0,
		36,106,0,121,83,132,0,36,107,0,121,83,133,0,
		36,108,0,121,83,134,0,36,109,0,121,83,135,0,
		36,110,0,121,83,136,0,36,111,0,121,83,137,0,
		36,112,0,121,83,138,0,36,113,0,121,83,139,0,
		36,114,0,121,83,140,0,36,115,0,121,83,141,0,
		36,116,0,121,83,142,0,36,117,0,121,83,143,0,
		36,118,0,121,83,144,0,36,119,0,121,83,145,0,
		36,120,0,121,83,146,0,36,121,0,121,83,147,0,
		36,122,0,121,83,148,0,36,123,0,121,83,149,0,
		36,124,0,121,83,150,0,36,125,0,121,83,151,0,
		36,126,0,121,83,152,0,36,127,0,121,83,153,0,
		36,128,0,121,83,154,0,36,129,0,121,83,155,0,
		36,130,0,121,83,156,0,36,131,0,121,83,157,0,
		36,132,0,121,83,158,0,36,133,0,121,83,159,0,
		36,134,0,121,83,160,0,36,135,0,121,83,161,0,
		36,136,0,121,83,162,0,36,137,0,121,83,163,0,
		36,138,0,121,83,164,0,36,139,0,121,83,165,0,
		36,140,0,121,83,166,0,36,141,0,121,83,167,0,
		36,142,0,121,83,168,0,36,143,0,121,83,169,0,
		36,144,0,121,83,170,0,36,145,0,121,83,171,0,
		36,146,0,121,83,172,0,36,147,0,121,83,173,0,
		36,148,0,121,83,174,0,36,149,0,121,83,175,0,
		36,150,0,121,83,176,0,36,151,0,121,83,177,0,
		36,152,0,121,83,178,0,36,153,0,121,83,179,0,
		36,154,0,121,83,180,0,36,155,0,121,83,181,0,
		36,156,0,121,83,182,0,36,157,0,121,83,183,0,
		36,158,0,121,83,184,0,36,159,0,121,83,185,0,
		36,160,0,121,83,186,0,36,161,0,121,83,187,0,
		36,162,0,121,83,188,0,36,163,0,121,83,189,0,
		36,164,0,121,83,190,0,36,165,0,121,83,191,0,
		36,166,0,121,83,192,0,36,167,0,121,83,193,0,
		36,168,0,121,83,194,0,36,169,0,121,83,195,0,
		36,170,0,121,83,196,0,36,171,0,121,83,197,0,
		36,172,0,121,83,198,0,36,173,0,121,83,199,0,
		36,174,0,121,83,200,0,36,175,0,121,83,201,0,
		36,176,0,121,83,202,0,36,177,0,121,83,203,0,
		36,178,0,121,83,204,0,36,179,0,121,83,205,0,
		36,180,0,121,83,206,0,36,181,0,121,83,207,0,
		36,182,0,121,83,208,0,36,183,0,121,83,209,0,
		36,184,0,121,83,210,0,36,185,0,121,83,211,0,
		36,186,0,121,83,212,0,36,187,0,121,83,213,0,
		36,188,0,121,83,214,0,36,189,0,121,83,215,0,
		36,190,0,121,83,216,0,36,191,0,121,83,217,0,
		36,192,0,121,83,218,0,36,193,0,121,83,219,0,
		36,194,0,121,83,220,0,36,195,0,121,83,221,0,
		36,196,0,121,83,222,0,36,197,0,121,83,223,0,
		36,198,0,121,83,224,0,36,199,0,121,83,225,0,
		36,200,0,121,83,226,0,36,201,0,121,83,227,0,
		36,202,0,121,83,228,0,36,203,0,121,83,229,0,
		36,204,0,121,83,230,0,36,205,0,120,83,231,0,
		36,206,0,176,43,0,92,60,12,1,83,232,0,36,
		207,0,176,233,0,109,234,0,12,1,176,43,0,122,
		12,1,69,28,18,36,208,0,109,234,0,106,2,58,
		0,72,83,232,0,25,13,36,210,0,106,3,67,58,
		0,83,232,0,36,213,0,109,235,0,176,43,0,92,
		30,12,1,69,28,45,36,214,0,176,233,0,109,232,
		0,12,1,109,235,0,72,106,12,69,88,84,82,65,
		84,79,46,84,88,84,0,72,176,43,0,92,10,12,
		1,72,83,232,0,25,39,36,216,0,176,233,0,109,
		232,0,12,1,106,12,69,88,84,82,65,84,79,46,
		84,88,84,0,72,176,43,0,92,10,12,1,72,83,
		232,0,36,218,0,176,3,0,106,2,49,0,20,1,
		36,219,0,176,236,0,106,50,80,111,115,105,99,105,
		111,110,101,32,111,32,70,111,114,109,117,108,97,114,
		105,111,32,56,48,32,67,111,108,117,110,97,115,32,
		101,32,84,101,99,108,101,32,91,69,78,84,69,82,
		93,0,20,1,36,220,0,176,237,0,20,0,36,221,
		0,176,238,0,20,0,36,222,0,176,7,0,92,20,
		106,8,80,82,73,78,84,69,82,0,20,2,36,223,
		0,109,34,0,83,239,0,36,224,0,122,83,240,0,
		36,225,0,122,83,241,0,36,227,0,122,83,240,0,
		36,228,0,176,11,0,122,121,20,2,176,12,0,109,
		242,0,106,11,69,109,112,114,101,115,97,32,58,32,
		0,72,109,243,0,72,20,1,36,229,0,176,11,0,
		122,92,120,20,2,176,12,0,106,8,68,97,116,97,
		32,58,32,0,176,244,0,176,245,0,12,0,12,1,
		72,20,1,36,230,0,176,11,0,92,2,121,20,2,
		176,12,0,106,14,67,111,110,116,97,116,111,32,32,
		32,32,58,32,0,109,246,0,72,106,4,32,45,32,
		0,72,109,247,0,72,20,1,36,231,0,176,11,0,
		92,2,92,80,20,2,176,12,0,106,14,84,101,108,
		101,102,111,110,101,32,32,32,58,32,0,20,1,36,
		232,0,176,11,0,92,2,92,94,20,2,176,248,0,
		109,249,0,106,20,64,82,32,40,88,88,41,32,88,
		88,88,88,88,45,88,88,88,88,88,0,20,2,36,
		233,0,176,11,0,92,3,121,20,2,176,12,0,106,
		11,83,105,115,116,101,109,97,32,58,32,0,109,250,
		0,72,20,1,36,234,0,176,11,0,92,3,92,121,
		20,2,176,12,0,106,14,82,101,108,97,116,58,32,
		83,67,67,48,48,49,0,20,1,36,235,0,176,11,
		0,92,4,92,40,20,2,176,12,0,109,251,0,106,
		22,80,69,68,73,68,79,32,68,69,32,67,79,84,
		65,67,65,79,32,78,111,46,0,72,176,252,0,109,
		8,0,92,10,12,2,72,109,242,0,72,20,1,36,
		236,0,176,3,0,106,2,52,0,20,1,36,237,0,
		176,27,0,109,239,0,100,100,20,3,36,238,0,176,
		11,0,92,5,121,20,2,176,12,0,106,14,70,111,
		114,110,101,99,101,100,111,114,32,58,32,0,176,252,
		0,109,239,0,92,10,12,2,72,106,4,32,45,32,
		0,72,109,253,0,72,106,4,32,45,32,0,72,109,
		254,0,72,20,1,36,239,0,176,11,0,92,6,121,
		20,2,176,12,0,106,14,84,101,108,101,102,111,110,
		101,32,32,32,58,32,0,20,1,36,240,0,176,11,
		0,92,6,92,14,20,2,176,248,0,109,255,0,106,
		20,64,82,32,40,88,88,41,32,88,88,88,88,88,
		45,88,88,88,88,88,0,20,2,36,241,0,176,11,
		0,92,6,92,40,20,2,176,12,0,106,14,70,97,
		120,32,32,32,32,32,32,32,32,58,32,0,20,1,
		36,242,0,176,11,0,92,6,92,54,20,2,176,248,
		0,109,0,1,106,20,64,82,32,40,88,88,41,32,
		88,88,88,88,88,45,88,88,88,88,88,0,20,2,
		36,243,0,176,3,0,106,2,49,0,20,1,36,244,
		0,176,11,0,92,7,121,20,2,176,12,0,176,1,
		1,106,2,61,0,93,135,0,12,2,20,1,36,245,
		0,176,11,0,92,8,121,20,2,176,12,0,106,2,
		124,0,20,1,36,246,0,176,11,0,92,8,92,2,
		20,2,176,12,0,106,7,67,79,68,73,71,79,0,
		20,1,36,247,0,176,11,0,92,8,92,12,20,2,
		176,12,0,106,5,84,73,80,79,0,20,1,36,248,
		0,176,11,0,92,8,92,18,20,2,176,12,0,106,
		10,68,69,83,67,82,73,67,65,79,0,20,1,36,
		249,0,176,11,0,92,8,92,68,20,2,176,12,0,
		106,6,77,65,82,67,65,0,20,1,36,250,0,176,
		11,0,92,8,92,88,20,2,176,12,0,106,3,85,
		78,0,20,1,36,251,0,176,11,0,92,8,92,94,
		20,2,176,12,0,106,7,81,85,65,78,84,46,0,
		20,1,36,252,0,176,11,0,92,8,92,102,20,2,
		176,12,0,106,10,80,82,69,67,79,32,85,78,46,
		0,20,1,36,253,0,176,11,0,92,8,92,118,20,
		2,176,12,0,106,12,80,82,69,67,79,32,84,79,
		84,65,76,0,20,1,36,254,0,176,11,0,92,8,
		93,134,0,20,2,176,12,0,106,2,124,0,20,1,
		36,255,0,176,11,0,92,9,121,20,2,176,12,0,
		176,1,1,106,2,61,0,93,135,0,12,2,20,1,
		36,0,1,92,10,83,2,1,36,1,1,109,240,0,
		92,31,35,29,255,1,109,2,1,92,57,34,29,246,
		1,36,2,1,176,11,0,109,2,1,121,20,2,176,
		12,0,106,2,124,0,20,1,36,3,1,121,83,3,
		1,36,4,1,176,4,1,176,5,1,109,240,0,12,
		1,12,1,83,3,1,36,5,1,106,8,101,80,82,
		79,68,85,95,0,109,3,1,92,2,49,121,5,28,
		14,176,252,0,109,240,0,92,2,12,2,25,16,106,
		2,48,0,176,252,0,109,240,0,122,12,2,72,72,
		83,6,1,36,6,1,106,8,101,81,85,65,78,84,
		95,0,109,3,1,92,2,49,121,5,28,14,176,252,
		0,109,240,0,92,2,12,2,25,16,106,2,48,0,
		176,252,0,109,240,0,122,12,2,72,72,83,7,1,
		36,7,1,109,6,1,40,11,121,69,29,80,1,36,
		8,1,176,11,0,109,2,1,92,2,20,2,176,248,
		0,109,6,1,42,11,106,11,64,82,32,57,57,57,
		44,57,57,57,0,122,124,2,0,36,9,1,176,3,
		0,106,2,51,0,20,1,36,10,1,176,27,0,109,
		6,1,42,11,100,100,92,2,124,2,0,36,11,1,
		176,11,0,109,2,1,92,12,20,2,176,248,0,109,
		8,1,106,7,64,82,32,57,57,57,0,20,2,36,
		12,1,176,11,0,109,2,1,92,18,20,2,176,12,
		0,109,9,1,20,1,36,13,1,176,11,0,109,2,
		1,92,68,20,2,176,12,0,109,10,1,20,1,36,
		14,1,176,11,0,109,2,1,92,88,20,2,176,12,
		0,109,11,1,20,1,36,15,1,176,3,0,106,2,
		49,0,20,1,36,16,1,176,11,0,109,2,1,92,
		94,20,2,176,248,0,109,7,1,42,11,106,11,64,
		82,32,57,57,57,57,46,57,57,0,122,124,2,0,
		36,17,1,176,11,0,109,2,1,92,102,20,2,176,
		12,0,106,17,95,95,95,95,95,95,95,95,95,95,
		95,44,95,95,95,95,0,20,1,36,18,1,176,11,
		0,109,2,1,92,118,20,2,176,12,0,106,17,95,
		95,95,95,95,95,95,95,95,95,95,44,95,95,95,
		95,0,20,1,36,19,1,176,11,0,109,2,1,93,
		134,0,20,2,176,12,0,106,2,124,0,20,1,36,
		20,1,109,2,1,122,72,83,2,1,36,21,1,109,
		240,0,122,72,83,240,0,26,251,253,36,26,1,176,
		11,0,92,58,121,20,2,176,12,0,176,1,1,106,
		2,61,0,93,135,0,12,2,20,1,36,27,1,176,
		11,0,92,59,92,100,20,2,176,12,0,109,12,1,
		20,1,36,28,1,176,11,0,122,121,20,2,176,12,
		0,106,2,32,0,109,13,1,72,20,1,36,29,1,
		109,241,0,122,72,83,241,0,36,30,1,109,241,0,
		92,5,34,28,53,36,31,1,106,10,70,79,82,78,
		69,67,69,68,95,0,176,252,0,109,241,0,122,12,
		2,72,83,14,1,36,32,1,176,3,0,106,2,49,
		0,20,1,36,33,1,109,14,1,40,11,83,239,0,
		36,35,1,109,241,0,92,5,15,31,10,109,239,0,
		121,5,29,215,249,36,39,1,109,241,0,92,5,15,
		31,10,109,239,0,121,5,29,130,242,36,43,1,176,
		7,0,92,24,106,1,0,20,2,36,44,1,176,7,
		0,92,20,106,7,83,67,82,69,69,78,0,20,2,
		36,45,1,176,22,0,20,0,176,3,0,106,2,49,
		0,20,1,176,15,1,100,20,1,36,46,1,100,110,
		7
	};

	hb_vmExecute( pcode, symbols );
}

