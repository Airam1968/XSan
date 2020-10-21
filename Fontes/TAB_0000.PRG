*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_0000.PRG
* Data        : 01/01/99
* Objetivo    : Atualizacao da Tabela de Registro 0000 PIS COFINS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION TAB_0000()
SAVE SCREEN TO TELA08
PTIT( "MANUTEN€ÇO REGISTRO 0000 PIS COFINS")
SELE 1                     
USE PIS_0000
GOTO TOP
IF EOF()
  APPEND BLANK
  REPLACE CAMPO01 WITH "0000"
  GOTO TOP
ENDIF
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[FCOUNT()],PIC[FCOUNT()],CAB[FCOUNT()],LENS[FCOUNT()]
AFIELDS(CAMPO,.T.,LENS)
CAB[01]="REGISTRO"
CAB[02]="COD_VER"
CAB[03]="TIPO ESCRIT"
CAB[04]="IND_SIT_ESP"
CAB[05]="NUM_REC_ANTERIOR"
CAB[06]="DT_INI"
CAB[07]="DT_FIM"
CAB[08]="NOME"
CAB[09]="CNPJ"
CAB[10]="UF"
CAB[11]="COD_MUN"
CAB[12]="SUFRAMA"
CAB[13]="IND_NAT_PJ"
CAB[14]="IND_ATIV"
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
DBEDIT(6,01,21,78,CAMPO,"F_P0000",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA08
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_P0000
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
