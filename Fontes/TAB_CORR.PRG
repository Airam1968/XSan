*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_CORR.PRG
* Data        : 01/01/99
* Objetivo    : Atualizacao da Tabela de Corretores
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_CORR()
SAVE SCREEN TO TELA03
PTIT( "ATUALIZACAO DA TABELA DE CORRETORES")
SELE 1                     
USE CORRETOR INDEX CORRETOR
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[FCOUNT()],PIC[FCOUNT()],CAB[FCOUNT()],LENS[FCOUNT()]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="@R 99999"
PIC[2]="@S12"
PIC[3]="@S40"
CAB[1]="C¢digo"
CAB[2]="APELIDO"
CAB[3]="NOME COMPLETO"
@ 5,00 TO 21,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ENTER]"
@ 23,21 SAY "[ESC]"
@ 23,37 SAY "[ESPA‡O]"
@ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,07 SAY "Para Alterar"
@ 23,26 SAY "Para Sair"
@ 23,45 SAY "Para Incluir"
@ 23,65 SAY "Movimenta‡Æo"
DBEDIT(6,01,21,78,CAMPO,"F_CORR",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_CORR
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 32
         SAVE SCREEN
         SET CURSOR ON
         PLQUA("01")
         PPERG ( "Confirma Inclusao (S/N) ? ")
         IF sisRESP = "S"
            eCODIGO = 0
            DO WHILE .T.
               PLQUA("01")
			   @ 23,01 SAY "C¢digo Corretor: " GET eCODIGO PICT "@R 99999"
               READ
               IF eCODIGO = 0 
				  PMSGR( "C¢digo Invalido",3)
                  LOOP
               ELSE
                  GOTO TOP
                  SEEK eCODIGO                  
                  IF FOUND()
					 PMSGR( "C¢digo j‡ cadastrado",3)
                     LOOP
                  ELSE
                     SELE 1
                     APPEND BLANK
                     IF F_BLOQR()
                        REPLACE CODIGO WITH eCODIGO
                        UNLOCK
                     ENDIF
                     EXIT
                  ENDIF
               ENDIF
            ENDDO
        ENDIF
        SET CURSOR OFF
        RESTORE SCREEN
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
