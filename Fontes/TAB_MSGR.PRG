*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_MSGR.PRG
* Data        : 01/01/99
* Objetivo    : Consulta a banco de dados de Mensagens de Boleto
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_MSGR()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA A MENSAGENS")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   SELE 1
   USE MSG_BOLE INDEX MSG_BOLE,MSG_PROD
   GOTO TOP
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[7],PIC[7],CAB[7],LENS[7]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1] ="@R 99999"
   PIC[2] ="@R 99999"
   PIC[3] ="@S50"
   PIC[4] ="@R50"
   PIC[5] ="@R50"
   PIC[6] ="@R50"
   PIC[7] ="@R50"
   @ 5,00 TO 21,79 DOUBLE
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
   DBEDIT(06,01,21,78,CAMPO,"F_MSGR",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
**************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_MSGR
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
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
      CASE KEY == 32
         SAVE SCREEN
         SET CURSOR ON
         PLQUA("01")
         PPERG ( "Confirma Inclusao (S/N) ? ")
         IF sisRESP = "S"
            eCOD_MSG = 0
            DO WHILE .T.
               PLQUA("01")
               @ 23,01 SAY "Codigo da Mensagem : " GET eCOD_MSG PICT "@R 99999"
               READ
               IF eCOD_MSG = 0
                  EXIT
               ELSE
                  GOTO TOP
                  SEEK eCOD_MSG
                  IF FOUND()
					 PMSGR( "Codigo ja Cadastrado",3)
                     LOOP
                  ELSE
                     APPEND BLANK
                     IF F_BLOQR()
                        REPLACE COD_MSG WITH eCOD_MSG
                        UNLOCK
                     ENDIF
                     EXIT
                  ENDIF
               ENDIF
            ENDDO
        ENDIF
        SET CURSOR OFF
        RESTORE SCREEN
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
