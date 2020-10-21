*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : EXC_CTRE.PRG
* Data        : 01/01/99
* Objetivo    : EXCLUSAO DE CONTAS A RECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION EXC_CTRE()
SAVE SCREEN TO TELA04
PTIT( "EXCLUS«O DE CONTAS A RECEBER")
SELE 5
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 4
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 3
USE STATUS
GOTO TOP
SELE 2
USE CLIENTES INDEX CLIENTES
GOTO TOP
SELE 1
USE CRECEBER INDEX CRECEBER,DATA_CRE,DATA_CRV,DATA_NF
GOTO TOP
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   PLQUA("11")
   eNUM_LANC   = 0
   eCLASSE_R   = 0
   eCLASSE_PRO = 0
   eCODIGO_CLI = 0   
   eDESCRICAO  = SPACE(50)   
   eDATA_PREV  = CTOD("")
   ePREVISTO   = 0
   eDATA_REAL  = CTOD("")   
   eREALIZADO  = 0
   eCOD_CONTA  = 0
   eNUM_DOCUME = SPACE(20)
   eDATA_NOTA  = CTOD("")
   DO WHILE .T.
      @ 06,05 SAY "Lanáamento    : " GET eNUM_LANC PICT "@R 9999999999"
      READ
      IF eNUM_LANC = 0
         CLOSE ALL
         RESTORE SCREEN FROM TELA04
         RETURN nil
      ELSE
         SEEK eNUM_LANC
         IF FOUND()
            IF F_BLOQR()
               eCLASSE_R   = CLASSE_R
               eCLASSE_PRO = EMPREENDIM
               eCODIGO_CLI = CODIGO_CLI   
               eDESCRICAO  = DESCRICAO   
               eDATA_PREV  = DATA_PREV
               ePREVISTO   = PREVISTO
               eDATA_REAL  = DATA_REAL  
               eREALIZADO  = REALIZADO
               eCOD_CONTA  = COD_CONTA
               eNUM_DOCUME = NUM_DOCUME
               eDATA_NOTA  = DATA_NOTA
               eINTEGRA    = INTEGRACAO
               UNLOCK
            ENDIF
            EXIT
         ELSE
            PLQUA("01")
            PMSGR( "Lanáamento Inexistente !",3)
            LOOP
         ENDIF
      ENDIF
   ENDDO
   CLEAR GETS
   SELE 4
   GOTO TOP
   @ 08,05 SAY "Cod. Despesa  : " GET eCLASSE_R PICT "@R 99999999"
   SEEK eCLASSE_R
   @ 08,31 SAY " - "+ALLTRIM(DESCRICAO)
   @ 09,05 SAY "   Produto    : " GET eCLASSE_PRO PICT "@R 9999999"
   SELE 5
   SEEK eCLASSE_PRO
   @ 09,31 SAY " - "+ALLTRIM(DESCRICAO)
   SELE 1
   @ 10,05 SAY "Cliente       : " GET eCODIGO_CLI PICT "@R 9999999"
   SELE 2
   SEEK eCODIGO_CLI
   @ 10,29 SAY " - "+ALLTRIM(NOME)
   SELE 1
   @ 11,05 SAY "Data Prevista : " GET eDATA_PREV
   @ 12,05 SAY "Vlr. Previsto : " GET ePREVISTO  PICT "@R 999,999,999,999.99"
   @ 13,05 SAY "Num. Documento: " GET eNUM_DOCUME
   @ 14,05 SAY "Dt.Nota Fiscal: " GET eDATA_NOTA
   @ 15,05 SAY "Descriá∆o     : " GET eDESCRICAO
   @ 16,05 SAY "Data Pagamento: " GET eDATA_REAL
   @ 17,05 SAY "Vlr. Realizado: " GET eREALIZADO PICT "@R 999,999,999,999.99"
   @ 18,05 SAY "C¢digo Conta  : " GET eCOD_CONTA PICT "@R 999999999"
   @ 19,05 SAY "Int.Contabil  : " GET eINTEGRA  
   CLEAR GETS
   SELE 1
   PLQUA("01")
   PPERG ( "Confirma a Exclus∆o (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
      PMENSAG("Excluindo, Aguarde ...")
      SELE 1
      IF F_BLOQR()
         DELETE
         UNLOCK
      ENDIF
      F_LACESSO(sisUSUARIO,"CONTAS A RECEBER","EXCLUSAO LANC. : "+ALLTRIM(STR(eNUM_LANC,5)))
      SELE 1
   ENDIF
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
