/*
 * Harbour 2.0.0 (Rev. 13372)
 * MinGW GNU C 4.4.1 (32-bit)
 * Generated C source from "C:/sis/SAN/xhb/inc_pedc.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( INC_PEDC );
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
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( __GET );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC_EXTERN( READMODAL );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( RESTSCREEN );
HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( PLQUA );
HB_FUNC_EXTERN( PMSGR );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( SCROLL );
HB_FUNC_EXTERN( DISPBOX );
HB_FUNC_EXTERN( LENNUM );
HB_FUNC_EXTERN( INT );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( PMENSAG );
HB_FUNC_EXTERN( PPARA );
HB_FUNC_EXTERN( __XSAVESCREEN );
HB_FUNC_EXTERN( __MVPRIVATE );
HB_FUNC_EXTERN( AFIELDS );
HB_FUNC_EXTERN( DBEDIT );
HB_FUNC_EXTERN( __XRESTSCREEN );
HB_FUNC_EXTERN( PPERG );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( F_BLOQR );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( F_LACESSO );
HB_FUNC_EXTERN( __DBGENTRY );
HB_FUNC_INITLINES();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_INC_PEDC )
{ "INC_PEDC", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( INC_PEDC )}, NULL },
{ "SAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SAVESCREEN )}, NULL },
{ "MAXROW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXROW )}, NULL },
{ "MAXCOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAXCOL )}, NULL },
{ "TELA03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PTIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTIT )}, NULL },
{ "DBSELECTAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSELECTAREA )}, NULL },
{ "DBUSEAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUSEAREA )}, NULL },
{ "DBSETINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSETINDEX )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "EPED_COMP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PED_COMP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ORDE_CLAS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EORDE_CLAS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "DBSEEK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSEEK )}, NULL },
{ "FOUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOUND )}, NULL },
{ "PLQUA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLQUA )}, NULL },
{ "PMSGR", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMSGR )}, NULL },
{ "EFORN_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORN_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORN_3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORN_4", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EFORN_5", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "ECONDIC_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONDIC_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONDIC_3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONDIC_4", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONDIC_5", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EENTREG_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EENTREG_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EENTREG_3", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EENTREG_4", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EENTREG_5", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_07", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_08", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_09", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_10", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_11", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_12", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_13", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_14", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_15", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_16", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_07", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_08", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_09", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_10", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_11", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_12", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_13", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_14", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_15", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_16", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_17", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_18", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_19", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPRODU_20", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_17", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_18", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EQUANT_19", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
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
{ "EPR_21_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_21_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_21_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_21_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPR_21_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECONDICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EENTREGA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SCROLL", {HB_FS_PUBLIC}, {HB_FUNCNAME( SCROLL )}, NULL },
{ "DISPBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISPBOX )}, NULL },
{ "EAUX", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EPROD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENNUM", {HB_FS_PUBLIC}, {HB_FUNCNAME( LENNUM )}, NULL },
{ "INT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INT )}, NULL },
{ "ETAM_PROD", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "ECAMPO_1", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECAMPO_2", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "DESCRICAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PMENSAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PMENSAG )}, NULL },
{ "PPARA", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPARA )}, NULL },
{ "__XSAVESCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XSAVESCREEN )}, NULL },
{ "CAMPO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PIC", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CAB", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "LENS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__MVPRIVATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPRIVATE )}, NULL },
{ "AFIELDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFIELDS )}, NULL },
{ "DBEDIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBEDIT )}, NULL },
{ "__XRESTSCREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( __XRESTSCREEN )}, NULL },
{ "CODIGO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PPERG", {HB_FS_PUBLIC}, {HB_FUNCNAME( PPERG )}, NULL },
{ "SISRESP", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ECLAS_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EDESC_PRO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PESQUISA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "DBAPPEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBAPPEND )}, NULL },
{ "F_BLOQR", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_BLOQR )}, NULL },
{ "NUMERO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_07", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_08", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_09", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_10", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_11", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_12", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_13", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_14", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_15", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_16", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_17", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_18", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_19", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "PRODUTO_20", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_01", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_02", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_03", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_04", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_05", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_06", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_07", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_08", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_09", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_10", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_11", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_12", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_13", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_14", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_15", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_16", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_17", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_18", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_19", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "QUANT_20", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "SITUACAO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMPREENDIM", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DBUNLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBUNLOCK )}, NULL },
{ "F_LACESSO", {HB_FS_PUBLIC}, {HB_FUNCNAME( F_LACESSO )}, NULL },
{ "SISUSUARIO", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__DBGENTRY", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DBGENTRY )}, NULL },
{ "(_INITLINES)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITLINES}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_INC_PEDC, "C:/sis/SAN/xhb/inc_pedc.prg", 0x0, 0x0002 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_INC_PEDC
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_INC_PEDC )
   #include "hbiniseg.h"
#endif

HB_FUNC( INC_PEDC )
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,105,110,99,95,112,101,100,99,46,112,114,103,
		58,73,78,67,95,80,69,68,67,0,36,14,0,176,
		1,0,121,121,176,2,0,12,0,176,3,0,12,0,
		12,4,83,4,0,36,15,0,176,5,0,106,31,73,
		78,67,76,85,83,65,79,32,68,69,32,80,69,68,
		73,68,79,32,68,69,32,77,65,84,69,82,73,65,
		76,0,20,1,36,16,0,176,6,0,106,2,54,0,
		20,1,36,17,0,176,7,0,9,100,106,9,67,76,
		65,83,95,80,82,79,0,100,100,9,20,6,176,8,
		0,106,9,67,76,65,83,95,80,82,79,0,20,1,
		176,8,0,106,9,68,69,83,67,95,80,82,79,0,
		20,1,36,18,0,176,9,0,20,0,36,19,0,176,
		6,0,106,2,49,0,20,1,36,20,0,176,7,0,
		9,100,106,9,80,69,68,95,67,79,77,80,0,100,
		100,9,20,6,176,8,0,106,9,80,69,68,95,67,
		79,77,80,0,20,1,36,21,0,176,9,0,20,0,
		36,22,0,176,6,0,106,2,50,0,20,1,36,23,
		0,176,7,0,9,100,106,9,65,82,81,95,84,73,
		80,79,0,100,100,9,20,6,176,8,0,106,9,65,
		82,81,95,84,73,80,79,0,20,1,36,24,0,176,
		9,0,20,0,36,25,0,176,6,0,106,2,51,0,
		20,1,36,26,0,176,7,0,9,100,106,9,65,82,
		81,95,73,84,69,77,0,100,100,9,20,6,176,8,
		0,106,9,65,82,81,95,73,84,69,77,0,20,1,
		176,8,0,106,9,73,84,69,77,95,68,69,83,0,
		20,1,36,27,0,176,9,0,20,0,36,28,0,36,
		29,0,176,6,0,106,2,53,0,20,1,36,30,0,
		176,7,0,9,100,106,7,83,84,65,84,85,83,0,
		100,100,9,20,6,36,31,0,176,9,0,20,0,36,
		32,0,121,83,10,0,36,33,0,109,11,0,83,10,
		0,36,34,0,109,12,0,83,13,0,36,35,0,176,
		6,0,106,2,49,0,20,1,36,36,0,176,14,0,
		20,0,36,37,0,176,15,0,106,9,87,47,78,44,
		43,87,47,82,0,20,1,36,38,0,176,16,0,92,
		5,92,2,20,2,176,17,0,106,15,67,111,116,97,
		135,198,111,32,32,32,32,32,58,32,0,20,1,176,
		18,0,176,19,0,12,0,176,20,0,12,0,122,72,
		20,2,176,21,0,109,22,0,176,23,0,100,106,10,
		101,80,69,68,95,67,79,77,80,0,106,11,64,82,
		32,57,57,57,44,57,57,57,0,100,100,12,5,20,
		2,48,24,0,176,25,0,109,22,0,12,1,112,0,
		73,36,39,0,176,26,0,109,22,0,100,100,100,100,
		100,100,20,7,4,0,0,83,22,0,109,22,0,73,
		36,40,0,109,10,0,121,5,28,39,36,41,0,176,
		27,0,20,0,36,42,0,176,28,0,121,121,176,2,
		0,12,0,176,3,0,12,0,109,4,0,20,5,36,
		43,0,100,110,7,36,45,0,176,6,0,106,2,49,
		0,20,1,36,46,0,176,9,0,20,0,36,47,0,
		176,29,0,109,10,0,100,100,20,3,36,48,0,176,
		30,0,12,0,28,50,36,49,0,176,31,0,106,3,
		48,49,0,20,1,36,50,0,176,32,0,106,17,67,
		162,100,105,103,111,32,106,135,32,101,120,105,115,116,
		101,0,92,3,20,2,36,51,0,26,135,254,36,53,
		0,121,83,33,0,36,54,0,121,83,34,0,36,55,
		0,121,83,35,0,36,56,0,121,83,36,0,36,57,
		0,121,83,37,0,36,58,0,176,38,0,92,60,12,
		1,83,39,0,36,59,0,176,38,0,92,60,12,1,
		83,40,0,36,60,0,176,38,0,92,60,12,1,83,
		41,0,36,61,0,176,38,0,92,60,12,1,83,42,
		0,36,62,0,176,38,0,92,60,12,1,83,43,0,
		36,63,0,176,38,0,92,20,12,1,83,44,0,36,
		64,0,176,38,0,92,20,12,1,83,45,0,36,65,
		0,176,38,0,92,20,12,1,83,46,0,36,66,0,
		176,38,0,92,20,12,1,83,47,0,36,67,0,176,
		38,0,92,20,12,1,83,48,0,36,68,0,121,83,
		49,0,36,69,0,121,83,50,0,36,70,0,121,83,
		51,0,36,71,0,121,83,52,0,36,72,0,121,83,
		53,0,36,73,0,121,83,54,0,36,74,0,121,83,
		55,0,36,75,0,121,83,56,0,36,76,0,121,83,
		57,0,36,77,0,121,83,58,0,36,78,0,121,83,
		59,0,36,79,0,121,83,60,0,36,80,0,121,83,
		61,0,36,81,0,121,83,62,0,36,82,0,121,83,
		63,0,36,83,0,121,83,64,0,36,84,0,121,83,
		65,0,36,85,0,121,83,66,0,36,86,0,121,83,
		67,0,36,87,0,121,83,68,0,36,88,0,121,83,
		69,0,36,89,0,121,83,70,0,36,90,0,121,83,
		71,0,36,91,0,121,83,72,0,36,92,0,121,83,
		73,0,36,93,0,121,83,74,0,36,94,0,121,83,
		75,0,36,95,0,121,83,76,0,36,96,0,121,83,
		77,0,36,97,0,121,83,78,0,36,98,0,121,83,
		79,0,36,99,0,121,83,80,0,36,100,0,121,83,
		81,0,36,101,0,121,83,82,0,36,102,0,121,83,
		83,0,36,103,0,121,83,84,0,36,104,0,121,83,
		65,0,36,105,0,121,83,66,0,36,106,0,121,83,
		67,0,36,107,0,121,83,68,0,36,108,0,121,83,
		69,0,36,109,0,121,83,70,0,36,110,0,121,83,
		71,0,36,111,0,121,83,72,0,36,112,0,121,83,
		73,0,36,113,0,121,83,74,0,36,114,0,121,83,
		75,0,36,115,0,121,83,76,0,36,116,0,121,83,
		77,0,36,117,0,121,83,78,0,36,118,0,121,83,
		79,0,36,119,0,121,83,80,0,36,120,0,121,83,
		85,0,36,121,0,121,83,86,0,36,122,0,121,83,
		87,0,36,123,0,121,83,88,0,36,124,0,121,83,
		89,0,36,125,0,121,83,90,0,36,126,0,121,83,
		91,0,36,127,0,121,83,92,0,36,128,0,121,83,
		93,0,36,129,0,121,83,94,0,36,130,0,121,83,
		95,0,36,131,0,121,83,96,0,36,132,0,121,83,
		97,0,36,133,0,121,83,98,0,36,134,0,121,83,
		99,0,36,135,0,121,83,100,0,36,136,0,121,83,
		101,0,36,137,0,121,83,102,0,36,138,0,121,83,
		103,0,36,139,0,121,83,104,0,36,140,0,121,83,
		105,0,36,141,0,121,83,106,0,36,142,0,121,83,
		107,0,36,143,0,121,83,108,0,36,144,0,121,83,
		109,0,36,145,0,121,83,110,0,36,146,0,121,83,
		111,0,36,147,0,121,83,112,0,36,148,0,121,83,
		113,0,36,149,0,121,83,114,0,36,150,0,121,83,
		115,0,36,151,0,121,83,116,0,36,152,0,121,83,
		117,0,36,153,0,121,83,118,0,36,154,0,121,83,
		119,0,36,155,0,121,83,120,0,36,156,0,121,83,
		121,0,36,157,0,121,83,122,0,36,158,0,121,83,
		123,0,36,159,0,121,83,124,0,36,160,0,121,83,
		125,0,36,161,0,121,83,126,0,36,162,0,121,83,
		127,0,36,163,0,121,83,128,0,36,164,0,121,83,
		129,0,36,165,0,121,83,130,0,36,166,0,121,83,
		131,0,36,167,0,121,83,132,0,36,168,0,121,83,
		133,0,36,169,0,121,83,134,0,36,170,0,121,83,
		135,0,36,171,0,121,83,136,0,36,172,0,121,83,
		137,0,36,173,0,121,83,138,0,36,174,0,121,83,
		139,0,36,175,0,121,83,140,0,36,176,0,121,83,
		141,0,36,177,0,121,83,142,0,36,178,0,121,83,
		143,0,36,179,0,121,83,144,0,36,180,0,121,83,
		145,0,36,181,0,121,83,146,0,36,182,0,121,83,
		147,0,36,183,0,121,83,148,0,36,184,0,121,83,
		149,0,36,185,0,121,83,150,0,36,186,0,121,83,
		151,0,36,187,0,121,83,152,0,36,188,0,121,83,
		153,0,36,189,0,121,83,154,0,36,190,0,121,83,
		155,0,36,191,0,121,83,156,0,36,192,0,121,83,
		157,0,36,193,0,121,83,158,0,36,194,0,121,83,
		159,0,36,195,0,121,83,160,0,36,196,0,121,83,
		161,0,36,197,0,121,83,162,0,36,198,0,121,83,
		163,0,36,199,0,121,83,164,0,36,200,0,121,83,
		165,0,36,201,0,121,83,166,0,36,202,0,121,83,
		167,0,36,203,0,121,83,168,0,36,204,0,121,83,
		169,0,36,205,0,121,83,170,0,36,206,0,121,83,
		171,0,36,207,0,121,83,172,0,36,208,0,121,83,
		173,0,36,209,0,121,83,174,0,36,210,0,121,83,
		175,0,36,211,0,121,83,176,0,36,212,0,121,83,
		177,0,36,213,0,121,83,178,0,36,214,0,121,83,
		179,0,36,215,0,121,83,180,0,36,216,0,121,83,
		181,0,36,217,0,121,83,182,0,36,218,0,121,83,
		183,0,36,219,0,121,83,184,0,36,220,0,121,83,
		185,0,36,221,0,121,83,186,0,36,222,0,121,83,
		187,0,36,223,0,121,83,188,0,36,224,0,121,83,
		189,0,36,225,0,121,83,190,0,36,226,0,121,83,
		191,0,36,227,0,121,83,192,0,36,228,0,121,83,
		193,0,36,229,0,121,83,194,0,36,230,0,176,38,
		0,92,50,12,1,83,195,0,36,231,0,176,38,0,
		92,20,12,1,83,196,0,36,232,0,176,31,0,106,
		3,48,49,0,20,1,36,233,0,176,197,0,92,6,
		122,92,21,92,79,20,4,176,18,0,92,6,122,20,
		2,36,234,0,176,15,0,106,5,87,47,78,43,0,
		20,1,36,235,0,176,16,0,92,5,92,35,20,2,
		176,17,0,106,21,67,65,68,65,83,84,82,79,32,
		68,69,32,80,82,79,68,85,84,79,83,0,20,1,
		36,236,0,176,15,0,106,9,87,47,78,44,43,87,
		47,82,0,20,1,36,237,0,176,16,0,92,6,92,
		2,20,2,176,17,0,106,7,67,111,100,105,103,111,
		0,20,1,36,238,0,176,198,0,92,6,92,10,92,
		21,92,10,122,20,5,36,239,0,176,16,0,92,6,
		92,12,20,2,176,17,0,106,10,68,101,115,99,114,
		105,135,198,111,0,20,1,36,240,0,176,198,0,92,
		6,92,30,92,21,92,30,122,20,5,36,241,0,176,
		16,0,92,6,92,32,20,2,176,17,0,106,7,81,
		117,97,110,116,46,0,20,1,36,242,0,176,198,0,
		92,6,92,43,92,21,92,43,122,20,5,36,243,0,
		122,83,199,0,36,244,0,122,83,200,0,36,245,0,
		36,246,0,176,6,0,106,2,51,0,20,1,36,247,
		0,176,9,0,20,0,36,248,0,176,201,0,176,202,
		0,109,200,0,12,1,12,1,83,203,0,36,249,0,
		106,8,101,80,82,79,68,85,95,0,109,203,0,92,
		2,49,121,5,28,14,176,204,0,109,200,0,92,2,
		12,2,25,16,106,2,48,0,176,204,0,109,200,0,
		122,12,2,72,72,83,205,0,36,250,0,106,8,101,
		81,85,65,78,84,95,0,109,203,0,92,2,49,121,
		5,28,14,176,204,0,109,200,0,92,2,12,2,25,
		16,106,2,48,0,176,204,0,109,200,0,122,12,2,
		72,72,83,206,0,36,251,0,109,200,0,92,20,34,
		29,93,6,36,252,0,176,18,0,109,199,0,92,6,
		72,92,2,20,2,176,21,0,109,22,0,176,23,0,
		100,109,205,0,106,11,64,82,32,57,57,57,44,57,
		57,57,0,100,100,12,5,20,2,48,24,0,176,25,
		0,109,22,0,12,1,112,0,73,36,253,0,176,26,
		0,109,22,0,100,100,100,100,100,100,20,7,4,0,
		0,83,22,0,109,22,0,73,36,254,0,109,205,0,
		40,11,97,63,66,15,0,5,28,17,36,255,0,121,
		109,205,0,38,11,36,0,1,26,229,5,36,5,1,
		109,205,0,40,11,121,69,29,51,1,109,199,0,92,
		20,34,29,42,1,36,6,1,176,6,0,106,2,51,
		0,20,1,36,7,1,176,29,0,109,205,0,42,11,
		100,100,92,2,124,2,0,36,8,1,176,30,0,12,
		0,28,36,36,9,1,176,16,0,109,199,0,92,6,
		72,92,12,20,2,176,17,0,176,207,0,109,208,0,
		122,92,17,12,3,20,1,25,107,36,11,1,176,31,
		0,106,3,48,49,0,20,1,36,12,1,176,32,0,
		106,10,65,116,101,110,135,198,111,32,33,0,92,3,
		20,2,36,13,1,176,209,0,106,46,80,114,111,100,
		117,116,111,32,78,198,111,32,76,111,99,97,108,105,
		122,97,100,111,44,32,114,101,112,105,116,97,32,97,
		32,79,112,101,114,97,135,198,111,32,46,46,46,0,
		20,1,36,14,1,176,210,0,20,0,36,15,1,26,
		34,254,36,17,1,176,18,0,109,199,0,92,6,72,
		92,32,20,2,176,21,0,109,22,0,176,23,0,100,
		109,206,0,106,13,64,82,32,57,57,44,57,57,57,
		46,57,57,0,100,100,12,5,20,2,48,24,0,176,
		25,0,109,22,0,12,1,112,0,73,36,18,1,176,
		26,0,109,22,0,100,100,100,100,100,100,20,7,4,
		0,0,83,22,0,109,22,0,73,36,19,1,109,199,
		0,122,72,83,199,0,36,20,1,109,200,0,122,72,
		83,200,0,26,95,3,36,22,1,176,6,0,106,2,
		51,0,20,1,36,23,1,176,7,0,9,100,106,9,
		65,82,81,95,73,84,69,77,0,100,100,9,20,6,
		176,8,0,106,9,73,84,69,77,95,68,69,83,0,
		20,1,176,8,0,106,9,65,82,81,95,73,84,69,
		77,0,20,1,36,24,1,176,9,0,20,0,36,25,
		1,176,211,0,20,0,36,26,1,176,15,0,106,9,
		87,47,78,44,43,87,47,82,0,20,1,36,27,1,
		92,10,3,1,0,92,10,3,1,0,92,10,3,1,
		0,92,10,3,1,0,176,216,0,108,212,108,213,108,
		214,108,215,20,4,81,215,0,81,214,0,81,213,0,
		81,212,0,36,28,1,176,217,0,109,212,0,120,109,
		215,0,20,3,36,29,1,106,6,65,84,73,86,65,
		0,98,212,0,92,10,2,36,30,1,122,98,215,0,
		92,10,2,36,31,1,106,9,64,82,32,57,57,57,
		57,57,0,98,213,0,122,2,36,32,1,106,9,64,
		82,32,57,57,57,57,57,0,98,213,0,92,2,2,
		36,33,1,106,4,64,83,50,0,98,213,0,92,3,
		2,36,34,1,106,5,64,83,52,48,0,98,213,0,
		92,4,2,36,35,1,106,5,64,83,49,53,0,98,
		213,0,92,5,2,36,36,1,106,5,64,83,49,53,
		0,98,213,0,92,6,2,36,37,1,106,14,64,82,
		32,57,57,57,44,57,57,57,46,57,57,0,98,213,
		0,92,7,2,36,38,1,106,17,64,82,32,57,57,
		44,57,57,57,44,57,57,57,46,57,57,0,98,213,
		0,92,8,2,36,39,1,106,17,64,82,32,57,57,
		44,57,57,57,44,57,57,57,46,57,57,0,98,213,
		0,92,9,2,36,40,1,106,2,76,0,98,213,0,
		92,10,2,36,41,1,106,7,67,162,100,105,103,111,
		0,98,214,0,122,2,36,42,1,106,5,84,73,80,
		79,0,98,214,0,92,2,2,36,43,1,106,6,85,
		78,73,68,46,0,98,214,0,92,3,2,36,44,1,
		106,10,68,69,83,67,82,73,135,198,111,0,98,214,
		0,92,4,2,36,45,1,106,6,77,65,82,67,65,
		0,98,214,0,92,5,2,36,46,1,106,7,77,79,
		68,69,76,79,0,98,214,0,92,6,2,36,47,1,
		106,12,69,83,84,79,81,46,32,77,73,78,46,0,
		98,214,0,92,7,2,36,48,1,106,6,67,85,83,
		84,79,0,98,214,0,92,8,2,36,49,1,106,8,
		80,82,69,67,79,32,49,0,98,214,0,92,9,2,
		36,50,1,106,6,65,84,73,86,65,0,98,214,0,
		92,10,2,36,51,1,176,198,0,92,9,121,92,17,
		92,79,92,2,20,5,36,52,1,176,31,0,106,3,
		48,49,0,20,1,36,53,1,176,15,0,106,9,87,
		47,78,44,43,87,47,82,0,20,1,36,54,1,176,
		218,0,92,10,122,92,17,92,78,109,212,0,106,11,
		70,95,84,65,66,95,73,84,69,77,0,109,213,0,
		109,214,0,106,2,196,0,106,2,179,0,106,2,205,
		0,20,11,36,55,1,176,219,0,20,0,36,56,1,
		109,220,0,109,205,0,38,11,36,57,1,176,6,0,
		106,2,51,0,20,1,36,58,1,176,7,0,9,100,
		106,9,65,82,81,95,73,84,69,77,0,100,100,9,
		20,6,176,8,0,106,9,65,82,81,95,73,84,69,
		77,0,20,1,176,8,0,106,9,73,84,69,77,95,
		68,69,83,0,20,1,36,59,1,176,9,0,20,0,
		36,60,1,109,205,0,40,11,121,5,28,83,36,61,
		1,176,31,0,106,3,48,49,0,20,1,36,62,1,
		176,221,0,106,38,68,101,115,101,106,97,32,69,110,
		99,101,114,114,97,114,32,111,32,70,111,114,109,117,
		108,97,114,105,111,32,40,83,47,78,41,32,63,32,
		0,20,1,36,63,1,109,222,0,106,2,78,0,5,
		29,85,1,36,64,1,26,85,250,36,70,1,109,199,
		0,92,6,72,92,21,16,29,70,250,36,71,1,176,
		209,0,106,44,80,114,101,115,115,105,111,110,101,32,
		81,117,97,108,113,117,101,114,32,84,101,99,108,97,
		32,80,97,114,97,32,67,111,110,116,105,110,117,97,
		114,32,46,46,46,0,20,1,36,72,1,176,210,0,
		20,0,36,73,1,176,31,0,106,3,48,49,0,20,
		1,36,74,1,176,197,0,92,6,122,92,21,92,79,
		20,4,176,18,0,92,6,122,20,2,36,75,1,176,
		15,0,106,5,87,47,78,43,0,20,1,36,76,1,
		176,16,0,92,5,92,35,20,2,176,17,0,106,21,
		67,65,68,65,83,84,82,79,32,68,69,32,80,82,
		79,68,85,84,79,83,0,20,1,36,77,1,176,15,
		0,106,9,87,47,78,44,43,87,47,82,0,20,1,
		36,78,1,176,16,0,92,6,92,2,20,2,176,17,
		0,106,7,67,111,100,105,103,111,0,20,1,36,79,
		1,176,198,0,92,6,92,10,92,21,92,10,122,20,
		5,36,80,1,176,16,0,92,6,92,12,20,2,176,
		17,0,106,10,68,101,115,99,114,105,135,198,111,0,
		20,1,36,81,1,176,198,0,92,6,92,30,92,21,
		92,30,122,20,5,36,82,1,176,16,0,92,6,92,
		32,20,2,176,17,0,106,7,81,117,97,110,116,46,
		0,20,1,36,83,1,176,198,0,92,6,92,43,92,
		21,92,43,122,20,5,36,84,1,122,83,199,0,36,
		85,1,26,9,249,36,87,1,176,6,0,106,2,54,
		0,20,1,36,88,1,176,9,0,20,0,36,89,1,
		109,13,0,32,239,2,36,90,1,121,83,223,0,36,
		91,1,176,38,0,92,30,12,1,83,224,0,36,92,
		1,36,93,1,176,16,0,92,23,92,2,20,2,176,
		17,0,106,15,67,111,100,46,32,80,114,111,100,117,
		116,111,58,32,0,20,1,176,18,0,176,19,0,12,
		0,176,20,0,12,0,122,72,20,2,176,21,0,109,
		22,0,176,23,0,100,106,10,101,67,76,65,83,95,
		80,82,79,0,106,12,64,82,32,57,57,57,57,57,
		57,57,57,0,100,100,12,5,20,2,48,24,0,176,
		25,0,109,22,0,12,1,112,0,73,36,94,1,176,
		26,0,109,22,0,100,100,100,100,100,100,20,7,4,
		0,0,83,22,0,109,22,0,73,36,95,1,176,15,
		0,106,9,87,47,78,44,43,87,47,82,0,20,1,
		36,96,1,176,197,0,92,23,121,92,23,20,3,176,
		18,0,92,23,121,20,2,36,97,1,176,16,0,92,
		23,92,2,20,2,176,17,0,106,15,67,111,100,46,
		32,80,114,111,100,117,116,111,58,32,0,20,1,36,
		98,1,176,1,0,121,121,176,2,0,12,0,176,3,
		0,12,0,12,4,83,225,0,36,99,1,176,6,0,
		106,2,54,0,20,1,36,100,1,176,7,0,9,100,
		106,9,67,76,65,83,95,80,82,79,0,100,100,9,
		20,6,176,8,0,106,9,67,76,65,83,95,80,82,
		79,0,20,1,176,8,0,106,9,68,69,83,67,95,
		80,82,79,0,20,1,36,101,1,176,9,0,20,0,
		36,102,1,109,223,0,121,69,28,15,36,103,1,176,
		29,0,109,223,0,100,100,20,3,36,105,1,92,3,
		3,1,0,92,3,3,1,0,92,3,3,1,0,92,
		3,3,1,0,176,216,0,108,212,108,213,108,214,108,
		215,20,4,81,215,0,81,214,0,81,213,0,81,212,
		0,36,106,1,176,217,0,109,212,0,120,109,215,0,
		20,3,36,107,1,106,8,64,82,57,57,57,57,57,
		0,98,213,0,122,2,36,108,1,106,8,64,82,57,
		57,57,57,57,0,98,213,0,92,2,2,36,109,1,
		106,5,64,83,51,53,0,98,213,0,92,3,2,36,
		110,1,106,7,67,162,100,105,103,111,0,98,214,0,
		122,2,36,111,1,106,5,84,105,112,111,0,98,214,
		0,92,2,2,36,112,1,106,8,80,114,111,100,117,
		116,111,0,98,214,0,92,3,2,36,113,1,176,198,
		0,92,8,92,19,92,18,92,79,92,2,20,5,36,
		114,1,176,218,0,92,9,92,20,92,18,92,78,109,
		212,0,106,10,70,67,79,78,95,67,84,80,71,0,
		109,213,0,109,214,0,106,2,196,0,106,2,179,0,
		106,2,205,0,20,11,36,115,1,109,220,0,83,223,
		0,36,116,1,109,208,0,83,224,0,36,117,1,176,
		28,0,121,121,176,2,0,12,0,176,3,0,12,0,
		109,225,0,20,5,36,118,1,176,197,0,92,23,121,
		92,23,20,3,176,18,0,92,23,121,20,2,36,119,
		1,176,16,0,92,23,92,2,20,2,176,17,0,106,
		15,67,111,100,46,32,80,114,111,100,117,116,111,58,
		32,0,20,1,36,120,1,176,16,0,92,23,92,18,
		20,2,176,17,0,176,204,0,109,220,0,92,5,12,
		2,106,4,32,45,32,0,72,176,226,0,109,208,0,
		12,1,72,20,1,36,121,1,176,6,0,106,2,49,
		0,20,1,36,122,1,36,123,1,26,249,2,36,125,
		1,121,83,223,0,36,126,1,176,38,0,92,30,12,
		1,83,224,0,36,127,1,36,128,1,176,16,0,92,
		23,92,2,20,2,176,17,0,106,15,67,111,100,46,
		32,80,114,111,100,117,116,111,58,32,0,20,1,176,
		18,0,176,19,0,12,0,176,20,0,12,0,122,72,
		20,2,176,21,0,109,22,0,176,23,0,100,106,10,
		101,68,69,83,67,95,80,82,79,0,106,5,64,83,
		51,48,0,100,100,12,5,20,2,48,24,0,176,25,
		0,109,22,0,12,1,112,0,73,36,129,1,176,26,
		0,109,22,0,100,100,100,100,100,100,20,7,4,0,
		0,83,22,0,109,22,0,73,36,130,1,176,226,0,
		109,224,0,12,1,83,224,0,36,131,1,176,6,0,
		106,2,54,0,20,1,36,132,1,176,7,0,9,100,
		106,9,67,76,65,83,95,80,82,79,0,100,100,9,
		20,6,176,8,0,106,9,68,69,83,67,95,80,82,
		79,0,20,1,176,8,0,106,9,67,76,65,83,95,
		80,82,79,0,20,1,36,133,1,176,9,0,20,0,
		36,134,1,109,224,0,176,38,0,92,30,12,1,69,
		28,15,36,135,1,176,29,0,109,224,0,100,100,20,
		3,36,137,1,176,15,0,106,9,87,47,78,44,43,
		87,47,82,0,20,1,36,138,1,176,197,0,92,23,
		121,92,23,20,3,176,18,0,92,23,121,20,2,36,
		139,1,176,16,0,92,23,92,2,20,2,176,17,0,
		106,15,67,111,100,46,32,80,114,111,100,117,116,111,
		58,32,0,20,1,36,140,1,176,1,0,121,121,176,
		2,0,12,0,176,3,0,12,0,12,4,83,225,0,
		36,141,1,92,3,3,1,0,92,3,3,1,0,92,
		3,3,1,0,92,3,3,1,0,176,216,0,108,212,
		108,213,108,214,108,215,20,4,81,215,0,81,214,0,
		81,213,0,81,212,0,36,142,1,176,217,0,109,212,
		0,120,109,215,0,20,3,36,143,1,106,8,64,82,
		57,57,57,57,57,0,98,213,0,122,2,36,144,1,
		106,8,64,82,57,57,57,57,57,0,98,213,0,92,
		2,2,36,145,1,106,5,64,83,51,53,0,98,213,
		0,92,3,2,36,146,1,106,7,67,162,100,105,103,
		111,0,98,214,0,122,2,36,147,1,106,5,84,105,
		112,111,0,98,214,0,92,2,2,36,148,1,106,8,
		80,114,111,100,117,116,111,0,98,214,0,92,3,2,
		36,149,1,176,198,0,92,8,92,19,92,18,92,79,
		92,2,20,5,36,150,1,176,218,0,92,9,92,20,
		92,18,92,78,109,212,0,106,10,70,67,79,78,95,
		67,84,80,71,0,109,213,0,109,214,0,106,2,196,
		0,106,2,179,0,106,2,205,0,20,11,36,151,1,
		109,220,0,83,223,0,36,152,1,109,208,0,83,224,
		0,36,153,1,176,28,0,121,121,176,2,0,12,0,
		176,3,0,12,0,109,225,0,20,5,36,154,1,176,
		197,0,92,23,121,92,23,20,3,176,18,0,92,23,
		121,20,2,36,155,1,176,16,0,92,23,92,2,20,
		2,176,17,0,106,15,67,111,100,46,32,80,114,111,
		100,117,116,111,58,32,0,20,1,36,156,1,176,16,
		0,92,23,92,18,20,2,176,17,0,176,204,0,109,
		220,0,92,5,12,2,106,4,32,45,32,0,72,176,
		226,0,109,208,0,12,1,72,20,1,36,157,1,176,
		6,0,106,2,49,0,20,1,36,158,1,36,161,1,
		176,209,0,106,44,80,114,101,115,115,105,111,110,101,
		32,81,117,97,108,113,117,101,114,32,84,101,99,108,
		97,32,80,97,114,97,32,70,105,110,97,108,105,122,
		97,114,32,46,46,46,0,20,1,36,162,1,176,210,
		0,20,0,36,163,1,176,6,0,106,2,49,0,20,
		1,36,164,1,176,31,0,106,3,48,49,0,20,1,
		36,165,1,176,221,0,106,18,67,111,110,102,105,114,
		109,97,32,40,83,47,78,41,32,63,32,0,20,1,
		36,166,1,109,222,0,106,2,83,0,5,29,227,234,
		36,167,1,176,209,0,106,22,71,82,65,86,65,78,
		68,79,44,32,65,71,85,65,82,68,69,32,46,46,
		46,0,20,1,36,168,1,176,6,0,106,2,49,0,
		20,1,36,169,1,176,227,0,20,0,36,170,1,176,
		228,0,12,0,29,70,1,36,171,1,109,10,0,78,
		229,0,36,174,1,109,49,0,78,230,0,109,50,0,
		78,231,0,109,51,0,78,232,0,36,177,1,109,52,
		0,78,233,0,109,53,0,78,234,0,109,54,0,78,
		235,0,36,180,1,109,55,0,78,236,0,109,56,0,
		78,237,0,109,57,0,78,238,0,36,183,1,109,58,
		0,78,239,0,109,59,0,78,240,0,109,60,0,78,
		241,0,36,186,1,109,61,0,78,242,0,109,62,0,
		78,243,0,109,63,0,78,244,0,36,189,1,109,64,
		0,78,245,0,109,81,0,78,246,0,109,82,0,78,
		247,0,36,191,1,109,83,0,78,248,0,109,84,0,
		78,249,0,36,194,1,109,65,0,78,250,0,109,66,
		0,78,251,0,109,67,0,78,252,0,36,197,1,109,
		68,0,78,253,0,109,69,0,78,254,0,109,70,0,
		78,255,0,36,200,1,109,71,0,78,0,1,109,72,
		0,78,1,1,109,73,0,78,2,1,36,203,1,109,
		74,0,78,3,1,109,75,0,78,4,1,109,76,0,
		78,5,1,36,206,1,109,77,0,78,6,1,109,78,
		0,78,7,1,109,79,0,78,8,1,36,209,1,109,
		80,0,78,9,1,109,85,0,78,10,1,109,86,0,
		78,11,1,36,211,1,109,87,0,78,12,1,109,88,
		0,78,13,1,36,213,1,106,10,69,77,32,65,66,
		69,82,84,79,0,78,14,1,109,223,0,78,15,1,
		36,214,1,176,16,1,20,0,36,216,1,176,6,0,
		106,2,57,0,20,1,36,217,1,176,7,0,9,100,
		106,7,83,84,65,84,85,83,0,100,100,9,20,6,
		36,218,1,176,9,0,20,0,36,219,1,176,228,0,
		12,0,28,21,36,220,1,109,10,0,122,72,78,11,
		0,36,221,1,176,16,1,20,0,36,223,1,176,17,
		1,109,18,1,106,25,67,65,68,65,83,84,82,79,
		32,80,69,68,73,68,79,32,77,65,84,69,82,73,
		65,76,0,106,14,67,65,68,65,83,84,82,79,32,
		78,111,46,32,0,176,226,0,176,204,0,109,10,0,
		92,10,12,2,12,1,72,20,3,36,224,1,176,6,
		0,106,2,49,0,20,1,36,225,1,26,194,232,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITLINES()
{
	static const HB_BYTE pcode[] =
	{
		51,67,58,47,115,105,115,47,83,65,78,47,120,104,
		98,47,105,110,99,95,112,101,100,99,46,112,114,103,
		58,40,95,73,78,73,84,76,73,78,69,83,41,0,
		106,2,67,0,92,8,106,61,192,255,255,255,239,239,
		255,255,255,255,255,255,255,255,255,255,255,255,255,255,
		255,255,255,255,255,255,255,255,255,255,255,225,251,222,
		255,255,255,255,255,193,255,191,255,255,254,255,239,255,
		254,255,127,254,79,146,164,36,73,106,191,3,0,4,
		3,0,4,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

