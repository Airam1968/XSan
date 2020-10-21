*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_TAXD.PRG
* Data        : 01/01/99
* Objetivo    : Atualizacao da Tabela de Taxas diarias
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_TAXD()
SAVE SCREEN TO TELA03
PTIT( "ATUALIZACAO DA TABELA DE TAXAS DIARIAS")
SELE 2
USE INDICES INDEX INDICES
GOTO TOP
SELE 1
USE TAXA_DIA INDEX TAXA_DIA
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[FCOUNT()],PIC[FCOUNT()],CAB[FCOUNT()],LENS[FCOUNT()]
AFIELDS(CAMPO,.T.,LENS)

PIC[1]="  /  /  "
CAB[1]="  DATA"
eCAMPOS = 0
eCAMPOS = FCOUNT()
eAUX = 2
DO WHILE eAUX <= eCAMPOS
   CAB[eAUX]=FIELD(eAUX)
   SELE 2
   SEEK CAB[eAUX]
   DO CASE
      CASE TIPO = "TAXA"
         PIC[eAUX]="@R 999.999999"
      CASE TIPO = "VALOR"
         PIC[eAUX]="@R 9,999,999.9999"
      CASE TIPO = "FATOR"
         PIC[eAUX] = "@R 9.99999999"
   ENDCASE
   eAUX = eAUX + 1
   SELE 1
   GOTO BOTTOM
   SKIP -9
ENDDO
@ 5,00 TO 21,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ENTER]"
@ 23,21 SAY "[ESC]"
@ 23,37 SAY "[ESPAáO]"
@ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,07 SAY "Para Alterar"
@ 23,26 SAY "Para Sair"
@ 23,45 SAY "Para Incluir"
@ 23,65 SAY "Movimentaá∆o"
DBEDIT(6,01,21,78,CAMPO,"F_TAXD",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_TAXD
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
            eDATA = CTOD("")
            DO WHILE .T.
               PLQUA("01")
               @ 23,01 SAY "Informe a Data : " GET eDATA
               READ
               IF eDATA = CTOD("")
				  PMSGR( "Data Invalida",3)
                  LOOP
               ELSE
                  GOTO TOP
                  SEEK eDATA
                  IF FOUND()
					 PMSGR( "Data já cadastrada",3)
                     LOOP
                  ELSE
                     SELE 1
                     APPEND BLANK
                     IF F_BLOQR()
                        REPLACE DATA WITH eDATA
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
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
