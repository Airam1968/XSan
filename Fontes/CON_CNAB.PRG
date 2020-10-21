*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CNAB.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE DADOS DE ARQUIVO SIGMA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_CNAB()
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
DO CASE
   CASE eCOD_BANCO = 1
      SELE 1
      USE TRANS001
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06"
      GO TOP
   CASE eCOD_BANCO = 8
      SELE 1
      USE TRANS008
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06"
      GO TOP
   CASE eCOD_BANCO = 33
      SELE 1
      USE TRANS033
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06".OR. CDOC_TRN01 = "15"
      GO TOP
   CASE eCOD_BANCO = 104
      SELE 1
      USE TRANS104
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "21" .OR. CDOC_TRN01 = "22"
      GO TOP
   CASE eCOD_BANCO = 215
      SELE 1
      USE TRANS215
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "08"
      GO TOP
   CASE eCOD_BANCO = 230
      SELE 1
      USE TRANS230
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "08"
      GO TOP
   CASE eCOD_BANCO = 237
      SELE 1
      USE TRANS237
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "08"
      GO TOP
   CASE eCOD_BANCO = 275
      SELE 1
      USE TRANS275
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "08"
      GO TOP
   CASE eCOD_BANCO = 291
      SELE 1
      USE TRANS291
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "08"
      GO TOP
   CASE eCOD_BANCO = 341
      SELE 1
      USE TRANS341
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "07" .OR. CDOC_TRN01 = "08" 
      GO TOP
   CASE eCOD_BANCO = 409
      SELE 1
      USE TRANS409
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "98" .OR. CDOC_TRN01 = "99"
      GO TOP
   CASE eCOD_BANCO = 422
      SELE 1
      USE TRANS422
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "07" .OR. CDOC_TRN01 = "10"
      GO TOP
   CASE eCOD_BANCO = 424
      SELE 1
      USE TRANS424
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "15"
      GO TOP
   CASE eCOD_BANCO = 641
      SELE 1
      USE TRANS641
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "15" .OR. CDOC_TRN01 = "16" .OR. CDOC_TRN01 = "17"
      GO TOP
   OTHERWISE
      SELE 1
      USE TRANSTXT
      GOTO TOP
ENDCASE
SET EXCLUSIVE OFF
PTIT( "CONSULTA DE DADOS - BAIXAS - "+eNOME_ARQ)
PLQUA("11")
SELE 1
GOTO TOP
@ 5,00 TO 21,79 DOUBLE
DECLARE CAMPO[40],PIC[40],CAB[40],LENS[40]
AFIELDS(CAMPO,.T.,LENS)
DBEDIT(6,01,21,78,CAMPO,"F_CNAB",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE DATABASES
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
