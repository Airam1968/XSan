*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CNAP.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE DADOS DE ARQUIVO RETORNO - PROTESTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_CNAP()
SAVE SCREEN TO TELA04
SELE 2
USE BANCOS INDEX BANCOS
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
eCOD_BANCO = 0
@ 05,02 SAY "Banco   : "
GO TOP
DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="@R9999"
PIC[2]="@S50"
CAB[1]="    "
CAB[2]="    "
DBEDIT(5,15,5,75,CAMPO,"F_CONF_COB",PIC,CAB," "," - "," ")
eCOD_BANCO = COD_BANCO
eNOME_ARQ = ALLTRIM(NOME_ARQ)
SET EXCLUSIVE ON
DO WHILE .T.
   SET COLOR TO W/N,+W/BG
   @ 15,60 CLEAR TO 20,79
   @ 15,60 TO 20,79
   @ 16,61 PROMPT " 1  Liquidaá∆o    " MESSAGE " Tátulos Liquidados em Cartário                     "
   @ 17,61 PROMPT " 2 Baixa por Prot." MESSAGE " Tátulo Efetivamente Protestado                     "
   @ 18,61 PROMPT " 3 Tátulo Protest." MESSAGE " Tátulo Encaminhado ao Cartário                     "
   @ 19,61 PROMPT " 4 Sustaá∆o Prot. " MESSAGE " Instruá∆o de Protesto Sustada                      "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
      CASE sisOP = 1
         DO CASE
            CASE eCOD_BANCO = 1
               eCOD_PROT = "15"
            CASE eCOD_BANCO = 8
               eCOD_PROT = "15"
            CASE eCOD_BANCO = 104
               eCOD_PROT = "22"
            CASE eCOD_BANCO = 215
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 230
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 237
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 275
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 291
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 341
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 409
               eCOD_PROT = "99"
            CASE eCOD_BANCO = 641
               eCOD_PROT = "15"
         ENDCASE
         EXIT
      CASE sisOP = 2
         DO CASE
            CASE eCOD_BANCO = 1
               eCOD_PROT = "07"
            CASE eCOD_BANCO = 8
               eCOD_PROT = "33"
            CASE eCOD_BANCO = 104
               eCOD_PROT = "25"
            CASE eCOD_BANCO = 215
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 230
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 237
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 275
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 291
               eCOD_PROT = "08"
            CASE eCOD_BANCO = 341
               eCOD_PROT = "32"
            CASE eCOD_BANCO = 409
               eCOD_PROT = "41"
            CASE eCOD_BANCO = 641
               eCOD_PROT = "50"
         ENDCASE
         EXIT
      CASE sisOP = 3
         DO CASE
            CASE eCOD_BANCO = 1
               eCOD_PROT = "23"
            CASE eCOD_BANCO = 8
               eCOD_PROT = "33"
            CASE eCOD_BANCO = 104
               eCOD_PROT = "26"
            CASE eCOD_BANCO = 215
               eCOD_PROT = "15"
            CASE eCOD_BANCO = 230
               eCOD_PROT = "15"
            CASE eCOD_BANCO = 237
               eCOD_PROT = "15"
            CASE eCOD_BANCO = 275
               eCOD_PROT = "15"
            CASE eCOD_BANCO = 291
               eCOD_PROT = "15"
            CASE eCOD_BANCO = 341
               eCOD_PROT = "32"
            CASE eCOD_BANCO = 409
               eCOD_PROT = "41"
            CASE eCOD_BANCO = 641
               eCOD_PROT = "10"
         ENDCASE
         EXIT
      CASE sisOP = 4
         DO CASE
            CASE eCOD_BANCO = 1
               eCOD_PROT = "24"
            CASE eCOD_BANCO = 8
               eCOD_PROT = "26"
            CASE eCOD_BANCO = 104
               eCOD_PROT = "27"
            CASE eCOD_BANCO = 215
               eCOD_PROT = "20"
            CASE eCOD_BANCO = 230
               eCOD_PROT = "20"
            CASE eCOD_BANCO = 237
               eCOD_PROT = "20"
            CASE eCOD_BANCO = 275
               eCOD_PROT = "20"
            CASE eCOD_BANCO = 291
               eCOD_PROT = "20"
            CASE eCOD_BANCO = 341
               eCOD_PROT = "20"
            CASE eCOD_BANCO = 409
               eCOD_PROT = "31"
            CASE eCOD_BANCO = 641
               eCOD_PROT = "20"
         ENDCASE
         EXIT
   ENDCASE
ENDDO
DO CASE
   CASE eCOD_BANCO = 1
      SELE 1
      USE TRANS001
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = eCOD_PROT
      GO TOP
   CASE eCOD_BANCO = 8
      SELE 1
      USE TRANS008
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = eCOD_PROT
      GO TOP
   CASE eCOD_BANCO = 033
      SELE 1
      USE TRANS033
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "  "
      GO TOP
   CASE eCOD_BANCO = 104
      SELE 1
      USE TRANS104
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = eCOD_PROT
      GO TOP
   CASE eCOD_BANCO = 215
      SELE 1
      USE TRANS215
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = eCOD_PROT
      GO TOP
   CASE eCOD_BANCO = 230
      SELE 1
      USE TRANS230
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = eCOD_PROT
      GO TOP
   CASE eCOD_BANCO = 237
      SELE 1
      USE TRANS291
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = eCOD_PROT
      GO TOP
   CASE eCOD_BANCO = 275
      SELE 1
      USE TRANS275
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = eCOD_PROT
      GO TOP
   CASE eCOD_BANCO = 291
      SELE 1
      USE TRANS291
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = eCOD_PROT
      GO TOP
   CASE eCOD_BANCO = 341
      SELE 1
      USE TRANS341
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = eCOD_PROT
      GO TOP
   CASE eCOD_BANCO = 409
      SELE 1
      USE TRANS409
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = eCOD_PROT
      GO TOP
   CASE eCOD_BANCO = 424
      SELE 1
      USE TRANS424
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "  "
      GO TOP
   CASE eCOD_BANCO = 641
      SELE 1
      USE TRANS641
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "  "
      GO TOP
   OTHERWISE
      SELE 1
      USE TRANSTXT
      GOTO TOP
ENDCASE
SET EXCLUSIVE OFF
PTIT( "CONSULTA DE DADOS - TITULOS PROTESTADOS - "+eNOME_ARQ)
PLQUA("11")
@ 5,00 TO 21,79 DOUBLE
DECLARE CAMPO[40],PIC[40],CAB[40],LENS[40]
AFIELDS(CAMPO,.T.,LENS)
DBEDIT(6,01,21,78,CAMPO,"F_CNAB",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE DATABASES
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
