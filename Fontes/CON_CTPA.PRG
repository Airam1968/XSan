*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CTPA.PRG
* Data        : 01/01/99
* Objetivo    : Consulta dos Lan‡amentos do Contas a Pagar Tit. em Aberto
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
function CON_CTPA()
SAVE SCREEN TO TELA04
PTIT( "CONSULTA CONTAS A PAGAR - TÖTULOS EM ABERTO")
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ou (0) para Todas ? " GET eCONTA PICT "@R 999"
   READ
   SELE 2
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA = 0
         EXIT
      ELSE
         PLQUA("01")
         PMSGR( "Aten‡Æo !",3)
         PMENSAG("Conta NÆo Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,02 SAY "Periodo : " GET eDATA_INI
   @ 23,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv lidas !",3)
      LOOP
   ELSE
      IF eDATA_INI > eDATA_FIM
         PLQUA("01")
         PMSGR( "Data Inicio > Que Final !",3)
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDIF
ENDDO
DO CASE
   CASE eCONTA = 0
      SELE 1
      USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
      SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. DATA_REAL = CTOD("") .AND. REALIZADO = 0
      GOTO TOP
   CASE eCONTA <> 0
      SELE 1
      USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
      SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. DATA_REAL = CTOD("") .AND. REALIZADO = 0 .AND. COD_CONTA = eCONTA
      GOTO TOP
ENDCASE
SELE 1
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[17],PIC[17],CAB[17],LENS[17]
AFIELDS(CAMPO,.T.,LENS)
CAMPO[8] ="GRUPO"
CAMPO[9] ="DATA_REAL"
CAMPO[10]="REALIZADO"
CAMPO[11]="COD_CONTA"
CAMPO[12]="NUM_CHEQUE"
CAMPO[13]="DATA_COMPE" 
CAMPO[14]="NUM_DOCUME"
CAMPO[15]="DATA_NOTA"
CAMPO[16]="CONTABIL"
CAMPO[17]="NOMINAL"
LENS[8]  ="1"
LENS[9]  ="10"
LENS[10] ="19"
LENS[11] ="3"
LENS[12] ="10"
LENS[13] ="10" 
LENS[14] ="10"
LENS[15] ="10"
LENS[16] ="1"
LENS[17] ="1"
PIC[1] ="@R 99999999"
PIC[2] ="@R 99999999"
PIC[3] ="@R 99999999"
PIC[4] ="@R 99999999"
PIC[5] ="@S50"
PIC[6] ="@D"
PIC[7] ="@R 999,999,999,999.99"
PIC[8] ="9"
PIC[9] ="@D"
PIC[10]="@R 999,999,999,999.99"
PIC[11]="@R 999999"
PIC[12]="@R 9999999999"
PIC[13]="@D" 
PIC[14]="@S20"
PIC[15]="@D"
PIC[16]="@S3"
PIC[17]="@!X"

CAB[1] ="Lan‡amento"
CAB[2] ="Classe Desp."
CAB[3] ="   Produto    "
CAB[4] ="Fornecedor"
CAB[5] ="Descri‡Æo"
CAB[6] ="Data Prev."
CAB[7] ="Vlr. Previsto"
CAB[8] ="Grupo Cheque"
CAB[9] ="Data Pgto."
CAB[10]="Vlr. Pago"
CAB[11]="Cod. Conta"
CAB[12]="Num. Cheque"
CAB[13]="Data Compe." 
CAB[14]="Num. Docum."
CAB[15]="Data NF"
CAB[16]=" "
CAB[17]="Nominal (S/N)"

@ 6,00 TO 20,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ESC]"
@ 23,20 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,05 SAY "Para Alterar"
@ 23,26 SAY "Movimenta‡Æo"
DBEDIT(7,01,20,78,CAMPO,"F_CONCTP",PIC,CAB,CHR(196),CHR(179),CHR(205))
eTOT_PREV = 0
eTOT_REAL = 0
eTOT_TITU = 0
GOTO TOP
DO WHILE .NOT. EOF()
   eTOT_PREV = eTOT_PREV + PREVISTO
   eTOT_REAL = eTOT_REAL + REALIZADO
   eTOT_TITU = eTOT_TITU + 1
   SKIP
ENDDO
PLIMPA()
@ 10,10 SAY "Total Previsto    : " GET eTOT_PREV PICT "@R 999,999,999.99"
@ 12,10 SAY "Total Realizado   : " GET eTOT_REAL PICT "@R 999,999,999.99"
@ 14,10 SAY "Total Titulos     : " GET eTOT_TITU PICT "@R 999,999,999.99"
PLQUA("01")
PMENSAG("Pressione Qualquer Tecla para Finalizar ...")
PPARA()
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
