*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : SEL_FORN.PRG
* Data        : 20/02/2001
* Objetivo    : SELECAO PARA MALA DIRETA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION SEL_FORN()
PPERG ("Confirma (S/N) ? ")
IF sisRESP = "N"
   PLQUA("01")
   RETURN nil
ENDIF
SAVE SCREEN TO TELA03
PTIT("SELECAO PARA MALA DIRETA")
PLQUA("01")
PPERG("Deseja o Sele‡Æo em Ordem Alfab‚tica (S/N) ? ")
IF sisRESP = "N"
   SELE 1
   USE FORNECED INDEX FORNECED
   GOTO TOP
ELSE
   SELE 1
   USE FORNECED
   INDEX ON NOME TO TEMP_FOR
   GOTO TOP
ENDIF
PLQUA("01")
PPERG("Deseja eliminar sele‡Æo anterior (S/N) ? ")
IF sisRESP = "S"
   IF F_BLOQA()
      REPLACE ALL SELECAO WITH " "
      UNLOCK
   ENDIF
ENDIF
PLQUA("11")
SELE 1
GOTO TOP
PMENSAG("ENTER PARA MARCAR [X] OU PARA DESMARCAR [ ]")
@ 6,04 TO 21,71 DOUBLE
DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
CAMPO[1]="NOME"
CAMPO[2]="SELECAO"
LENS[1]=50
LENS[2]=1
PIC[1]="@S50"
PIC[2]="@S1"
CAB[1]="NOME"
CAB[2]="SELECAO"
DBEDIT(7,05,21,70,CAMPO,"FUNC_SELFORN",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION FUNC_SELFORN
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         IF F_BLOQR()
            IF SELECAO = "X"
               REPLACE SELECAO WITH " "
            ELSE
               REPLACE SELECAO WITH "X"
            ENDIF
            UNLOCK
	 ENDIF
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************

