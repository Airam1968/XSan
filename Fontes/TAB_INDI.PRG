*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_INDI.PRG
* Data        : 01/01/99
* Objetivo    : Atualizacao da Tabela de Taxas mensais
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_INDI()
SAVE SCREEN TO TELA03
PTIT( "ATUALIZACAO DA TABELA DE INDICES DO SISTEMA")
eFAZ = .F.
SELE 1
USE INDICES INDEX INDICES
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[FCOUNT()],PIC[FCOUNT()],CAB[FCOUNT()],LENS[FCOUNT()]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="XXXXXXXXXX"
PIC[2]="XXXXXXXXXX"
PIC[3]="99"
PIC[4]="99"
PIC[5]="99"
PIC[6]="XXXXXXXXXX"
PIC[7]="99"
CAB[1]="  NOME"
CAB[2]="PERIODICIDADE"
CAB[3]="DIA INICIO"
CAB[4]="DIA FIM"
CAB[5]="DIA DIVUL."
CAB[6]="TIPO"
CAB[7]="D + ?"
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
DBEDIT(6,01,21,78,CAMPO,"F_INDI",PIC,CAB,CHR(196),CHR(179),CHR(205))
IF eFAZ
   * CONFIGURANDO ARQUIVO DIARIO
   PLQUA("01")
   PMENSAG("Aguarde, Criando ou Excluindo indice no Sistema")
   GOTO TOP
   DO WHILE .NOT. EOF()
      IF NOME = SPACE(10)
         IF F_BLOQR()
            DELETE
            UNLOCK
         ENDIF
      ENDIF
      SKIP
   ENDDO
   GOTO TOP
   SELE 3
   USE TAXA_DIA
   COPY TO TEMP_DIA.DBF
   SELE 1
   COUNT FOR PERIODO = "DIARIO" TO eELEMENTO
   eELEMENTO = eELEMENTO + 1
   DECLARE CAMPO[eELEMENTO],TIPO[eELEMENTO],TAM[eELEMENTO],DECI[eELEMENTO]
   CAMPO[1] = "DATA"
   TIPO[1]  = "D"
   TAM[1]   = 10
   DECI[1]  = 0
   GOTO TOP
   eAUX = 2
   DO WHILE eAUX <= eELEMENTO
      IF PERIODO = "DIARIO"
         CAMPO[eAUX] = ALLTRIM(A->NOME)
         TIPO[eAUX]  = "N"
         DO CASE 
            CASE A->TIPO = "TAXA"
               TAM[eAUX]  = 8
               DECI[eAUX] = 4
            CASE A->TIPO = "VALOR"
               TAM[eAUX]  = 10
               DECI[eAUX] = 4
            CASE A->TIPO = "FATOR"
               TAM[eAUX]  = 8
               DECI[eAUX] = 6
         ENDCASE
         eAUX = eAUX + 1
      ENDIF
      SKIP
   ENDDO
   SELE 2
   CREATE ESTRUTU.DBF
   IF F_BLOQA()
      ZAP
      UNLOCK
   ENDIF
   eAUX = 1
   DO WHILE eAUX <= eELEMENTO
      APPEND BLANK
      IF F_BLOQR()
         REPLACE FIELD_NAME WITH CAMPO[eAUX],;
                 FIELD_TYPE WITH TIPO[eAUX],;
                 FIELD_LEN  WITH TAM[eAUX]
         REPLACE FIELD_DEC  WITH DECI[eAUX]
         UNLOCK
      ENDIF
      eAUX = eAUX + 1
   ENDDO
   USE
   SELE 3
   USE
   SELE 2
   USE
   CREATE TAXA_DIA.DBF FROM ESTRUTU.DBF
   USE TAXA_DIA
   IF F_BLOQA()
      APPEND FROM TEMP_DIA
      UNLOCK
   ENDIF
   * FIM DA CONFIGURACAO DO ARQUIVO DIARIO
   * COMFIGURANDO ARQUIVO MENSAL
   PLQUA("01")
   SELE 3
   USE TAXA_MES
   COPY TO TEMP_MES.DBF
   SELE 1
   USE INDICES INDEX INDICES
   GOTO TOP
   COUNT FOR PERIODO = "MENSAL" TO eELEMENTO
   eELEMENTO = eELEMENTO + 2
   DECLARE CAMPO[eELEMENTO],TIPO[eELEMENTO],TAM[eELEMENTO],DECI[eELEMENTO]
   CAMPO[1] = "MES"
   TIPO[1]  = "C"
   TAM[1]   = 2
   DECI[1]  = 0
   CAMPO[2] = "ANO"
   TIPO[2]  = "C"
   TAM[2]   = 4
   DECI[2]  = 0
   GOTO TOP
   eAUX = 3
   DO WHILE eAUX <= eELEMENTO
      IF PERIODO = "MENSAL"
         CAMPO[eAUX] = ALLTRIM(A->NOME)
         TIPO[eAUX]  = "N"
         DO CASE 
            CASE A->TIPO = "TAXA"
               TAM[eAUX]  = 8
               DECI[eAUX] = 4
            CASE A->TIPO = "VALOR"
               TAM[eAUX]  = 10
               DECI[eAUX] = 4
            CASE A->TIPO = "FATOR"
               TAM[eAUX]  = 8
               DECI[eAUX] = 6
         ENDCASE
         eAUX = eAUX + 1
      ENDIF
      SKIP
   ENDDO
   SELE 2
   CREATE ESTRUTU.DBF
   IF F_BLOQA()
      ZAP
      UNLOCK
   ENDIF
   eAUX = 1
   DO WHILE eAUX <= eELEMENTO
      APPEND BLANK
      IF F_BLOQR()
         REPLACE FIELD_NAME WITH CAMPO[eAUX],;
                 FIELD_TYPE WITH TIPO[eAUX],;
                 FIELD_LEN  WITH TAM[eAUX]
         REPLACE FIELD_DEC  WITH DECI[eAUX]
         UNLOCK
      ENDIF
      eAUX = eAUX + 1
   ENDDO
   USE
   SELE 3
   USE
   SELE 1
   USE
   CREATE TAXA_MES.DBF FROM ESTRUTU.DBF
   USE TAXA_MES
   IF F_BLOQA()
      APPEND FROM TEMP_MES
      UNLOCK
   ENDIF
   * FIM DA CONFIGURACAO DO ARQUIVO MENSAL
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_INDI                
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
            eINDICE = SPACE(10)
            DO WHILE .T.
               PLQUA("01")
               @ 23,01 SAY "Informe o Indice : " GET eINDICE
               READ
               IF eINDICE = SPACE(10)
				  PMSGR( "Indice Invalido",3)
                  LOOP
               ELSE
                  GOTO TOP
                  SEEK eINDICE
                  IF FOUND()
					 PMSGR( "Indice ja cadastrado",3)
                     LOOP
                  ELSE
                     SELE 1
                     APPEND BLANK
                     IF F_BLOQR()
                        REPLACE NOME WITH eINDICE
                        UNLOCK
                     ENDIF
                     eFAZ = .T.
                     EXIT
                  ENDIF
               ENDIF
            ENDDO
        ENDIF
        SET CURSOR OFF
        RESTORE SCREEN
      CASE KEY == 13
         IF DB_COLUMN > 0
            IF F_BLOQR()
               SAVE SCREEN
               SET CURSOR ON
               FLD_NAME = CAMPO[DB_COLUMN]
               FLD_PIC  = PIC[DB_COLUMN]
               @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
               READ
               IF DB_COLUMN = 1 .AND. &FLD_NAME = SPACE(10)
                  eFAZ = .T.
               ENDIF
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
