*********************************************************************
* Sistema     : Gest�o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : EXC_CTPG.PRG
* Data        : 01/01/99
* Objetivo    : EXCLUSAO DE CONTAS A PAGAR
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION EXC_CTPG()
SAVE SCREEN TO TELA04
PTIT( "EXCLUS�O DE CONTAS A PAGAR")
SELE 5
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 4
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 3
USE STATUS
GOTO TOP
SELE 2
USE FORNECED INDEX FORNECED
GOTO TOP
SELE 1
USE CPAGAR INDEX CPAGAR,DATA_CPG,DATA_CPV,NUM_CHEQ,DATA_NF
GOTO TOP
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   PLQUA("11")
   eNUM_LANC   = 0
   eCLASSE_D   = 0
   eCLASSE_PRO = 0
   eCODIGO_FOR = 0   
   eDESCRICAO  = SPACE(50)   
   eDATA_PREV  = CTOD("")
   ePREVISTO   = 0
   eDATA_REAL  = CTOD("")   
   eREALIZADO  = 0
   eCOD_CONTA  = 0
   eNUM_CHEQ   = 0
   eNUM_DOCUME = SPACE(20)
   eDATA_NOTA  = CTOD("")
   eNOMINAL    = SPACE(1)
   DO WHILE .T.
      @ 06,05 SAY "Lan�amento    : " GET eNUM_LANC PICT "@R 9999999999"
      READ
      IF eNUM_LANC = 0
         CLOSE ALL
         RESTORE SCREEN FROM TELA04
         RETURN nil
      ELSE
         SEEK eNUM_LANC
         IF FOUND()
            IF F_BLOQR()
               eCLASSE_D   = CLASSE_D
               eCLASSE_PRO = EMPREENDIM
               eCODIGO_FOR = CODIGO_FOR   
               eDESCRICAO  = DESCRICAO   
               eDATA_PREV  = DATA_PREV
               ePREVISTO   = PREVISTO
               eDATA_REAL  = DATA_REAL  
               eREALIZADO  = REALIZADO
               eCOD_CONTA  = COD_CONTA
               eNUM_CHEQ   = NUM_CHEQUE
               eNUM_DOCUME = NUM_DOCUME
               eDATA_NOTA  = DATA_NOTA
               eNOMINAL    = NOMINAL
               eINTEGRA    = INTEGRACAO
               eNUM_COMPRA = NUM_COMPRA
               UNLOCK
            ENDIF
            EXIT
         ELSE
            PLQUA("01")
            PMSGR( "Lan�amento Inexistente !",3)
            LOOP
         ENDIF
      ENDIF
   ENDDO
   CLEAR GETS
   SELE 4
   GOTO TOP
   @ 08,05 SAY "Cod. Despesa  : " GET eCLASSE_D PICT "@R 99999999"
   SEEK eCLASSE_D
   @ 08,31 SAY " - "+ALLTRIM(DESCRICAO)
   @ 09,05 SAY "Produto       : " GET eCLASSE_PRO PICT "@R 9999999"
   SELE 5
   SEEK eCLASSE_PRO
   @ 09,31 SAY " - "+ALLTRIM(DESCRICAO)
   SELE 1
   @ 10,05 SAY "Fornecedor    : " GET eCODIGO_FOR PICT "@R 9999999"
   SELE 2
   SEEK eCODIGO_FOR
   @ 10,29 SAY " - "+ALLTRIM(NOME)
   SELE 1
   @ 11,05 SAY "Data Prevista : " GET eDATA_PREV
   @ 12,05 SAY "Vlr. Previsto : " GET ePREVISTO  PICT "@R 999,999,999,999.99"
   @ 13,05 SAY "Num. Documento: " GET eNUM_DOCUME
   @ 13,45 SAY "Num. Compra   : " GET eNUM_COMPRA
   @ 14,05 SAY "Dt.Nota Fiscal: " GET eDATA_NOTA
   @ 15,05 SAY "Descri��o     : " GET eDESCRICAO
   @ 16,05 SAY "Data Pagamento: " GET eDATA_REAL
   @ 17,05 SAY "Vlr. Realizado: " GET eREALIZADO PICT "@R 999,999,999,999.99"
   @ 18,05 SAY "C�digo Conta  : " GET eCOD_CONTA PICT "@R 999999999"
   @ 19,05 SAY "Num. Cheque   : " GET eNUM_CHEQ  PICT "@R 999999999"
   @ 20,05 SAY "Nominal (S/N) : " GET eNOMINAL   PICT "@! X" VALID eNOMINAL $("SN")
   @ 21,05 SAY "Int. Contabil : " GET eINTEGRA   
   CLEAR GETS
   SELE 1
   PLQUA("01")
   PPERG ( "Confirma a Exclus�o (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
      PMENSAG("Excluindo, Aguarde ...")
      SELE 1
      IF F_BLOQR()
         DELETE
         UNLOCK
      ENDIF
      F_LACESSO(sisUSUARIO,"CONTAS A PAGAR","EXCLUSAO LANC. : "+ALLTRIM(STR(eNUM_LANC,5)))
      SELE 1
   ENDIF
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
