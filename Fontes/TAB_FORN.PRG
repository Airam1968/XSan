*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_FORN.PRG
* Data        : 01/01/99
* Objetivo    : Tabela de Forncedores do Contas a Pagar
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_FORN()
SAVE SCREEN TO TELA03
PTIT( "TABELA FORNECEDORES CONTAS A PAGAR")
SELE 3
USE CLAS_FOR INDEX CLAS_FOR
GOTO TOP
SELE 2
USE STATUS
GOTO TOP
PLQUA("01")
PPERG ( "Deseja Consulta por Ordem Alfab‹¨«tica (S/N) ? ")
IF sisRESP = "N"
   SELE 1
   USE FORNECED INDEX FORNECED,TEMP_FOR
   GOTO TOP
ELSE
   SELE 1
   USE FORNECED INDEX TEMP_FOR,FORNECED
   GOTO TOP
ENDIF
PLQUA("01")
eCLASSE = 0
@ 23,00 SAY "Informe a Classe de Fornecedor ou (0) para Todas : " GET eCLASSE PICT "@R 999"
READ
IF eCLASSE <> 0
   SELE 1
   SET FILTER TO CLASSE = eCLASSE
   GOTO TOP
ENDIF
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[24],PIC[24],CAB[24],LENS[24]
AFIELDS(CAMPO,.T.,LENS)
PIC[1] ="@R 99999999"
PIC[2] ="@S50"
PIC[3] ="@99999" 
PIC[4] ="@S10"
PIC[5] ="@S20"
PIC[6] ="@S14"
PIC[7] ="@S19"
PIC[8] ="@S12"
PIC[9] ="@S2"
PIC[10] ="  /  /  "
PIC[11]="@S10"
PIC[12]="@S1"
PIC[13]="  /  /  "
PIC[14]="@S50"
PIC[15]="@S20"
PIC[16]="@S20"
PIC[17]="@S2"
PIC[18]="@R XXXXX-XXX"
PIC[19]="@R (XXXX) XXXX-XXXX"
PIC[20]="@R (XXXX) XXXX-XXXX"
PIC[21]="@S80"
PIC[22]="@S80"
PIC[23]="@S80"
PIC[24]="@S1"
CAB[1] ="C‹¨«digo"
CAB[2] ="Nome"
CAB[3] ="Apelido" 
CAB[4] ="Classe"
CAB[5] ="Contato"
CAB[6] ="CPF/CGC"
CAB[7] ="Inscri‹¨«‹¨«o"
CAB[8] ="RG"
CAB[9] ="UF RG"
CAB[10]="Emiss‹¨«o"
CAB[11]="Org‹¨«o"
CAB[12]="Sexo"
CAB[13]="Nascimento"
CAB[14]="Endere‹¨«o"
CAB[15]="Bairro"
CAB[16]="Cidade"
CAB[17]="Estado"
CAB[18]="CEP"
CAB[19]="Telefone"
CAB[20]="FaxS‹¨«mile"
CAB[21]="Observa‹¨«‹¨«es"
CAB[22]="Observa‹¨«‹¨«es"
CAB[23]="Observa‹¨«‹¨«es"
CAB[24]="Tipo"
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
DBEDIT(7,01,19,78,CAMPO,"F_FORN",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
**************************** funcao *********************************
FUNCTION F_FORN
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
               @ 05,05 CLEAR TO 12,75
               @ 05,05 TO 12,75 DOUBLE
               SET COLOR TO W/N,+W/R
               SELE 2
               eCODIGO = FORN_CPG
               @ 06,07 SAY "C‹¨«digo      : " GET eCODIGO
               CLEAR GETS
               eCLIENTE    = SPACE(50)
               @ 08,07 SAY "Cliente       : " GET eCLIENTE
               READ
               SELE 3
               eCLASSE    = 0
               @ 10,07 SAY "Classe        : " GET eCLASSE PICT "@R 999"
               READ
               PLQUA("01")
               PPERG ( "Confirma (S/N) ? ")
               IF sisRESP = "S"
                  PMENSAG("GRAVANDO, AGUARDE ...")
                  SELE 1
                  APPEND BLANK
                  IF F_BLOQR()
                     REPLACE CODIGO     WITH eCODIGO,;
                             NOME       WITH eCLIENTE,;
                             CLASSE     WITH eCLASSE
                     UNLOCK
                  ENDIF
                  SELE 2
                  USE STATUS
                  GOTO TOP
                  eCODIGO = FORN_CPG + 1
                  IF F_BLOQR()
                     REPLACE FORN_CPG WITH eCODIGO
                     UNLOCK
                  ENDIF
                  SELE 1
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
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
