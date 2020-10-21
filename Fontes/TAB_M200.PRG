*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_M200.PRG
* Data        : 01/01/99
* Objetivo    : Atualizacao da Tabela de Registro M200 PIS COFINS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_M200()
SAVE SCREEN TO TELA08
PTIT( "MANUTEN‹¨«‹¨«O REGISTRO M200 PIS COFINS")
SELE 1                     
USE PIS_M200
GOTO TOP
IF EOF()
  APPEND BLANK
  REPLACE CAMPO01 WITH "M200"
  GOTO TOP
ENDIF
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[FCOUNT()],PIC[FCOUNT()],CAB[FCOUNT()],LENS[FCOUNT()]
AFIELDS(CAMPO,.T.,LENS)
CAB[01]="REGISTRO"
CAB[02]="VL_TOT_CONT_NC_PER"
CAB[03]="VL_TOT_CRED_DESC"
CAB[04]="VL_TOT_CRED_DESC_ANT" 
CAB[05]="VL_TOT_CONT_NC_DEV"
CAB[06]="VL_RET_NC"
CAB[07]="VL_OUT_DED_NC"
CAB[08]="VL_CONT_NC_REC"
CAB[09]="VL_TOT_CONT_CUM_PER"
CAB[10]="VL_RET_CUM"
CAB[11]="VL_OUT_DED_CUM"
CAB[12]="VL_CONT_CUM_REC"
CAB[13]="VL_TOT_CONT_REC"
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
DBEDIT(6,01,21,78,CAMPO,"F_PM200",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA08
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_PM200
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
