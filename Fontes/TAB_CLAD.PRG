*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_CLAD.PRG
* Data        : 01/01/99
* Objetivo    : Tabela de Classe de Receita\Despesa
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_CLAD()
SAVE SCREEN TO TELA03
PTIT( "TABELA TIPO DE DESPESA")
PLQUA("01")
PPERG ( "Deseja Consulta por Ordem Alfab‡tica (S/N) ? ")
IF sisRESP = "N"
   SELE 1
   USE CLASSE_D INDEX CLASSE_D,TEMP_CLD
   GOTO TOP
   eINCLUI = .T.
ELSE
   SELE 1
   USE CLASSE_D INDEX TEMP_CLD,CLASSE_D
   GOTO TOP
   eINCLUI = .F.
ENDIF
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="@R 9999999"
PIC[2]="@S40"
PIC[3]="@S15"
CAB[1]="C¢digo"
CAB[2]="Descri‡Æo"
CAB[3]="Conta Contabil"
@ 6,01 TO 19,79 DOUBLE
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
DBEDIT(7,02,19,78,CAMPO,"F_CLAS",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
**************************** funcao *********************************
FUNCTION F_CLAS
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
         PPERG ( "Confirma Inclus‡o (S/N) ? ")
         IF sisRESP = "S"
            eCODIGO = 0
            DO WHILE .T.
               PLQUA("01")
               @ 23,01 SAY "Informe o C¢digo : " GET eCODIGO PICT "@R 9999999"
               READ
               IF eCODIGO = 0
				  PMSGR( "C¢digo Inv‡lido",3)
                  RESTORE SCREEN
                  RETURN RET
               ELSE
                  IF eINCLUI
                     GOTO TOP
                     SEEK eCODIGO
                     IF FOUND()
                        PLQUA("01")
						PMSGR( "C¢digo j‡ Cadastrado",3)
                        LOOP
                     ELSE
                        APPEND BLANK
                        IF F_BLOQR()
                           REPLACE CODIGO WITH eCODIGO
                           EXIT
                        ENDIF
                        EXIT
                     ENDIF
                  ELSE
                     PLQUA("01")
                     PMSGR( "Aten‡Æo !",3)
                     PMENSAG("NÆo ‡ Possivel Cadastrar C¢digo, por Ordem Alfab‡tica !") 
                     LOOP
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
              SET CURSOR ON
              FLD_NAME = CAMPO[DB_COLUMN]
              FLD_PIC  = PIC[DB_COLUMN]
              @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
              READ
              SET CURSOR OFF
              UNLOCK
           ENDIF
        ENDIF
        RESTORE SCREEN
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
