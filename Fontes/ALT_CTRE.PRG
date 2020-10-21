*********************************************************************
FUNCTION ALT_CTRE()
SAVE SCREEN TO TELA04
PTIT( "MANUTEN€ÇO DE CONTAS A RECEBER")
SELECT 5
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 4
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 3
USE STATUS
GOTO TOP
SELECT 2
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
   DO WHILE .T.
      @ 06,05 SAY "Lan‡amento    : " GET eNUM_LANC PICT "@R 9999999999"
      READ
      IF eNUM_LANC = 0
         CLOSE ALL
         RESTORE SCREEN FROM TELA04
         RETURN nil
      ELSE
         SEEK eNUM_LANC
         IF FOUND()
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
            EXIT
         ELSE
            PLQUA("01")
            PMSGR( "Lan‡amento Inexistente !",3)
            LOOP
         ENDIF
      ENDIF
   ENDDO
   CLEAR GETS
   SELE 4
   GOTO TOP
   DO WHILE .T.
      @ 08,05 SAY "Cod. Receita  : " GET eCLASSE_R PICT "@R 99999999"
      READ
      IF eCLASSE_R = 0
         LOOP
      ELSE
         SEEK eCLASSE_R
         IF FOUND()
            @ 08,31 SAY " - "+ALLTRIM(DESCRICAO)
            SELE 1
            EXIT
         ELSE
            PLQUA("01")
            PMSGR( "Classe Inexistente !",3)
            LOOP
         ENDIF
      ENDIF
   ENDDO
   SELE 5
   GOTO TOP
   DO WHILE .T.
      @ 09,05 SAY "Cod.  Produto : " GET eCLASSE_PRO PICT "@R 99999999"
      READ
      SEEK eCLASSE_PRO
      IF FOUND()
         @ 09,31 SAY " - "+ALLTRIM(DESCRICAO)
         SELE 1
         EXIT
      ELSE
         PLQUA("01")
         PMSGR( "Produto Inexistente !",3)
         PMENSAG("Produto Inexistente !")
         PPARA()
         CLOSE ALL
         RESTORE SCREEN FROM TELA04
         RETURN nil
      ENDIF
   ENDDO
   DO WHILE .T.
      @ 10,05 SAY "Cliente       : " GET eCODIGO_CLI PICT "@R 9999999"
      READ
      SELE 2
      SEEK eCODIGO_CLI
      IF .NOT. FOUND()
         PLQUA("01")
		 PMSGR( "C¢digo Inexistente",3)
         SELE 1
         LOOP
      ELSE
         @ 10,29 SAY " - "+ALLTRIM(NOME)
         SELE 1
         EXIT
      ENDIF
   ENDDO
   SELE 1
   DO WHILE .T.
      @ 11,05 SAY "Data Prevista : " GET eDATA_PREV
      READ
      IF eDATA_PREV = CTOD("")
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   @ 12,05 SAY "Vlr. Previsto : " GET ePREVISTO  PICT "@R 999,999,999,999.99"
   @ 13,05 SAY "Num.Documento : " GET eNUM_DOCUME
   @ 14,05 SAY "Dt.Nota Fiscal: " GET eDATA_NOTA
   @ 15,05 SAY "Descri‡Æo     : " GET eDESCRICAO
   @ 16,05 SAY "Data Pagamento: " GET eDATA_REAL PICT "@D" 
   @ 17,05 SAY "Vlr. Realizado: " GET eREALIZADO PICT "@R 999,999,999,999.99"
   @ 18,05 SAY "C¢digo Conta  : " GET eCOD_CONTA PICT "@R 999999999"
   @ 19,05 SAY "Int.Contabil  : " GET eINTEGRA  
   READ
   SELE 1
   PLQUA("01")
   PPERG ( "Confirma a Altera‡Æo (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
      PMENSAG("Gravando, Aguarde ...")
      SELE 1
      IF F_BLOQR()
         REPLACE CLASSE_R   WITH eCLASSE_R,;
                 CODIGO_CLI WITH eCODIGO_CLI,;
                 EMPREENDIM WITH eCLASSE_PRO
         REPLACE DESCRICAO  WITH eDESCRICAO,;
                 DATA_PREV  WITH eDATA_PREV,;
                 PREVISTO   WITH ePREVISTO
         REPLACE DATA_REAL  WITH eDATA_REAL,;
                 REALIZADO  WITH eREALIZADO,;
                 COD_CONTA  WITH eCOD_CONTA
         REPLACE NUM_DOCUME WITH eNUM_DOCUME,;
                 DATA_NOTA  WITH eDATA_NOTA,;
                 INTEGRACAO WITH eINTEGRA
         UNLOCK
      ENDIF
      F_LACESSO(sisUSUARIO,"CONTAS A RECEBER","ALTERACAO LANC. : "+ALLTRIM(STR(eNUM_LANC,5)))
      SELE 1
   ENDIF
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
