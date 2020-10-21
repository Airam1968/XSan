*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_CLAF.PRG
* Data        : 01/01/99
* Objetivo    : Tabela de Classificacao de Fornecedores
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_CLAF()
SAVE SCREEN TO TELA03
PTIT( "TABELA CLASSIFICACAO DE FORNECEDORES")
PLQUA("01")
PPERG ( "Deseja Consulta por Ordem Alfab‹¨«tica (S/N) ? ")
IF sisRESP = "N"
   SELE 1
   USE CLAS_FOR INDEX CLAS_FOR
   GOTO TOP
ELSE
   SELE 1
   USE CLAS_FOR
   INDEX ON DESCRICAO TO TEM_CLAF
   GOTO TOP
   USE CLAS_FOR INDEX TEM_CLAF,CLAS_FOR
   GOTO TOP
ENDIF
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
AFIELDS(CAMPO,.T.,LENS)
PIC[1] ="@R 9999"
PIC[2] ="@S50"
CAB[1] ="C‹¨«digo"
CAB[2] ="Descricao"
@ 6,00 TO 19,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ENTER]"
@ 23,21 SAY "[ESC]"
@ 23,37 SAY "[ESPA€O]"
@ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,07 SAY "Para Alterar"
@ 23,26 SAY "Para Sair"
@ 23,45 SAY "Para Incluir"
@ 23,65 SAY "Movimenta‡Æo"
DBEDIT(7,01,19,78,CAMPO,"F_CLAFOR",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
**************************** funcao *********************************
FUNCTION F_CLAFOR
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 32
         SAVE SCREEN TO TELA05
         SET CURSOR ON
         PLQUA("01")
         PPERG ( "Confirma Inclus‹¨«o (S/N) ? ")
         IF sisRESP = "S"
            SAVE SCREEN TO TELA04
            DO WHILE .T.
               @ 10,05 CLEAR TO 14,75
               @ 10,05 TO 14,75 DOUBLE
               SET COLOR TO W/N,+W/R
               eCODIGO = 0
               @ 12,07 SAY "C‹¨«digo      : " GET eCODIGO
               READ
               IF eCODIGO = 0
                  PLQUA("01")
				  PMSGR( "C‹¨«digo Invalido", 3)
                  LOOP
               ELSE
                  SELE 1
                  GOTO TOP
                  SEEK eCODIGO
                  IF FOUND()
                     PLQUA("01")
                     PMSGR( "ATEN‹¨«‹¨«O !",3)
					 PMENSAG("C‹¨«digo j‹¨« Cadastrado")
                     PPARA()
                     LOOP
                  ENDIF
               ENDIF
               eDESCRICAO  = SPACE(50)
               @ 13,07 SAY "Descri‹¨«‹¨«o     : " GET eDESCRICAO
               READ
               PLQUA("01")
               PPERG ( "Confirma (S/N) ? ")
               IF sisRESP = "S"
                  PMENSAG("GRAVANDO, AGUARDE ...")
                  SELE 1
                  APPEND BLANK
                  IF F_BLOQR()
                     REPLACE CODIGO     WITH eCODIGO,;
                             DESCRICAO  WITH eDESCRICAO
                     UNLOCK
                  ENDIF
                  EXIT
               ELSE
                  EXIT
               ENDIF
            ENDDO
            RESTORE SCREEN FROM TELA04
         ENDIF
         SET CURSOR OFF
         RESTORE SCREEN FROM TELA05
      CASE KEY == 13
        SAVE SCREEN
        IF F_BLOQR()
           SET CURSOR ON
           FLD_NAME = CAMPO[DB_COLUMN]
           FLD_PIC  = PIC[DB_COLUMN]
           @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
           READ
           SET CURSOR OFF
           UNLOCK
        ENDIF
        RESTORE SCREEN
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
