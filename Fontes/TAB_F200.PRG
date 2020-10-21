*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_F200.PRG
* Data        : 01/01/99
* Objetivo    : Atualizacao da Tabela de Registro F200 PIS COFINS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_F200()
SAVE SCREEN TO TELA08
PTIT( "MANUTEN‡Æo REGISTRO F200 PIS COFINS")
SELE 1                     
USE PIS_F200
GOTO TOP
IF EOF()
  APPEND BLANK
  REPLACE CAMPO01 WITH "F200"
  GOTO TOP
ENDIF
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[FCOUNT()],PIC[FCOUNT()],CAB[FCOUNT()],LENS[FCOUNT()]
AFIELDS(CAMPO,.T.,LENS)
CAB[01]="REGISTRO"
CAB[02]="IND_OPER"
CAB[03]="UNID_IMOB"
CAB[04]="IDENT_IMOB"
CAB[05]="DESC_UNID_IMOB"
CAB[06]="NUM_CONT"
CAB[07]="CPF_CNPJ_ADQU"
CAB[08]="DT_OPER"
CAB[09]="VL_TOT_VEND"
CAB[10]="VL_REC_ACUM"
CAB[11]="VL_TOT_REC"
CAB[12]="CST_PIS"
CAB[13]="VL_BC_PIS"
CAB[14]="ALIQ_PIS"
CAB[15]="VL_PIS"
CAB[16]="CST_COFINS"
CAB[17]="VL_BC_COFINS"
CAB[18]="ALIQ_COFINS"
CAB[19]="VL_COFINS"
CAB[20]="PERC_REC_RECEB"
CAB[21]="IND_NAT_EMP"
CAB[22]="INF_CONF"
@ 5,00 TO 21,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ENTER]"
@ 23,21 SAY "[ESC]"
@ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,07 SAY "Para Alterar"
@ 23,26 SAY "Para Sair"
@ 23,65 SAY "Movimenta‡Æo"
DBEDIT(6,01,21,78,CAMPO,"F_PF200",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA08
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_PF200
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         SAVE SCREEN
         IF DB_COLUMN > 1
            IF F_BLOQR()
               SAVE SCREEN
               SET CURSOR ON
               FLD_NAME = CAMPO[DB_COLUMN]
               FLD_PIC  = PIC[DB_COLUMN]
               @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
               READ
               SET CURSOR OFF
               UNLOCK
               RESTORE SCREEN
            ENDIF
         ENDIF
         RESTORE SCREEN
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
