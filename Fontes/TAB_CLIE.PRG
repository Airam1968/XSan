*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_CLIE.PRG
* Data        : 01/01/99
* Objetivo    : Tabela de Clientes do Contas a Receber
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_CLIE()
SAVE SCREEN TO TELA03
PTIT( "TABELA CLIENTES CONTAS A RECEBER")
SELE 2
USE STATUS
GOTO TOP
PLQUA("01")
PPERG ( "Deseja Consulta por Ordem Alfab‡tica (S/N) ? ")
IF sisRESP = "N"
   SELE 1
   USE CLIENTES INDEX CLIENTES
   GOTO TOP
ELSE
   SELE 1
   USE CLIENTES
   INDEX ON NOME TO TEM_CLIE
   GOTO TOP
   USE CLIENTES INDEX TEM_CLIE,CLIENTES
   GOTO TOP
ENDIF
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[22],PIC[22],CAB[22],LENS[22]
AFIELDS(CAMPO,.T.,LENS)
PIC[1] ="@R 99999999"
PIC[2] ="@S50"
PIC[3] ="@S12"
PIC[4] ="@S14"
PIC[5] ="@S19"
PIC[6] ="@S12"
PIC[7] ="@S2"
PIC[8] ="  /  /  "
PIC[9] ="@S10"
PIC[10]="@S1"
PIC[11]="  /  /  "
PIC[12]="@S50"
PIC[13]="@S20"
PIC[14]="@S20"
PIC[15]="@S2"
PIC[16]="@R XXXXX-XXX"
PIC[17]="@R (XXXX) XXXX-XXXX"
PIC[18]="@R (XXXX) XXXX-XXXX"
PIC[19]="@S80"
PIC[20]="@S80"
PIC[21]="@S80"
PIC[22]="@S1"
CAB[1] ="C¢digo"
CAB[2] ="Nome"
CAB[3] ="Apelido"
CAB[4] ="CGC/CPF"
CAB[5] ="INSCR. EST."
CAB[6] ="RG"
CAB[7] ="UF RG"
CAB[8] ="Emiss‡o"
CAB[9] ="Org‡o"
CAB[10]="Sexo"
CAB[11]="Nascimento"
CAB[12]="Endere‡o"
CAB[13]="Bairro"
CAB[14]="Cidade"
CAB[15]="Estado"
CAB[16]="CEP"
CAB[17]="Telefone"
CAB[18]="FaxS‡mile"
CAB[19]="Observa‡‡es"
CAB[20]="Observa‡‡es"
CAB[21]="Observa‡‡es"
CAB[22]="Tipo"
@ 6,00 TO 21,79 DOUBLE
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
DBEDIT(7,01,21,78,CAMPO,"F_CLIE",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
**************************** funcao *********************************
FUNCTION F_CLIE
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
            SAVE SCREEN TO TELA03
            @ 05,05 CLEAR TO 10,75
            @ 05,05 TO 10,75 DOUBLE
            SET COLOR TO W/N,+W/R
            SELE 2
            eCODIGO = CLIE_CPG
            @ 06,07 SAY "C¢digo      : " GET eCODIGO
            CLEAR GETS
            eCLIENTE    = SPACE(50)
            @ 08,07 SAY "Cliente       : " GET eCLIENTE
            READ
            PLQUA("01")
            PPERG ( "Confirma (S/N) ? ")
            IF sisRESP = "S"
               PMENSAG("GRAVANDO, AGUARDE ...")
               SELE 1
               APPEND BLANK
               IF F_BLOQR()
                  REPLACE CODIGO     WITH eCODIGO,;
                          NOME       WITH eCLIENTE
                  UNLOCK
               ENDIF
               SELE 2
               USE STATUS
               GOTO TOP
               eCODIGO = CLIE_CPG + 1
               IF F_BLOQR()
                  REPLACE CLIE_CPG WITH eCODIGO
                  UNLOCK
               ENDIF
               SELE 1
               RESTORE SCREEN FROM TELA03
            ENDIF
         ENDIF
         SET CURSOR OFF
         RESTORE SCREEN
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
