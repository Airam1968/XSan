*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_0100.PRG
* Data        : 01/01/99
* Objetivo    : Atualizacao da Tabela de Registro 0100 PIS COFINS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION TAB_0100()
SAVE SCREEN TO TELA08
PTIT( "MANUTEN€ÇO REGISTRO 0100 PIS COFINS")
SELE 1                     
USE PIS_0100
GOTO TOP
IF EOF()
  APPEND BLANK
  REPLACE CAMPO01 WITH "0100"
  GOTO TOP
ENDIF
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[FCOUNT()],PIC[FCOUNT()],CAB[FCOUNT()],LENS[FCOUNT()]
AFIELDS(CAMPO,.T.,LENS)
CAB[01]="REGISTRO"
CAB[02]="NOME CONTABILISTA"
CAB[03]="CPF"
CAB[04]="CRC"
CAB[05]="CNPJ ESCRIT‹¨«RIO"
CAB[06]="CEP"
CAB[07]="ENDERE‹¨«O"
CAB[08]="N‹¨«MERO"
CAB[09]="COMPLEMENTO"
CAB[10]="BAIRRO"
CAB[11]="TELEFONE"
CAB[12]="FAX"
CAB[13]="E-MAIL"
CAB[14]="CODIGO IBGE MUNICIPIO"
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
DBEDIT(6,01,21,78,CAMPO,"F_P0100",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA08
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_P0100
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
