*********************************************************************
* Sistema     : Gest�o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CTRB.PRG
* Data        : 01/01/99
* Objetivo    : Consulta dos Lan�amentos do Contas a Receber Baixados   
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_CTRB()
SAVE SCREEN TO TELA04
PTIT( "CONSULTA CONTAS A RECEBER - CONTAS BAIXADAS")
PLQUA("01")
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
SELE 3
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 4
USE CLIENTES INDEX CLIENTES
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
         PMSGR( "Aten��o !",3)
         PMENSAG("Conta N�o Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
PLQUA("01")
DO WHILE .T.
   @ 23,02 SAY "Periodo : " GET eDATA_INI
   @ 23,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv�lidas !",3)
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
IF eCONTA = 0
   SELE 1
   USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE,DATA_NF
   SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0
   GOTO TOP
ELSE
   SELE 1
   USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE,DATA_NF
   SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0 .AND.COD_CONTA= eCONTA
   GOTO TOP
ENDIF
SELE 1
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[13],PIC[13],CAB[13],LENS[13]
AFIELDS(CAMPO,.T.,LENS)
PIC[1] ="@R 99999999"
PIC[2] ="@R 99999999"
PIC[3] ="@R 99999999"
PIC[4] ="@R 99999999"
PIC[5] ="@S50"
PIC[6] ="@D"
PIC[7] ="@R 999,999,999,999.99"
PIC[8] ="@D"
PIC[9] ="@R 999,999,999,999.99"
PIC[10]="@R 999999"
PIC[11]="@D" 
PIC[12]="@S20"
PIC[13]="@D"
CAB[1] ="Lan�amento"
CAB[2] ="Classe Rec."
CAB[3] ="   Produto    "
CAB[4] ="Cliente"
CAB[5] ="Descri��o"
CAB[6] ="Data Prev."
CAB[7] ="Vlr. Previsto"
CAB[8] ="Data Pgto."
CAB[9] ="Vlr. Pago"
CAB[10]="Cod. Conta"
CAB[11]="Data Compe." 
CAB[12]="Num. Docum." 
CAB[13]="Data NF"
@ 6,00 TO 20,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ESC]"
@ 23,20 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,05 SAY "Para Alterar"
@ 23,26 SAY "Movimenta��o"
DBEDIT(7,01,20,78,CAMPO,"F_CONCTR",PIC,CAB,CHR(196),CHR(179),CHR(205))
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
