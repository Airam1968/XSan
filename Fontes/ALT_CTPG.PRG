*********************************************************************
FUNCTION ALT_CTPG()
SAVE SCREEN TO TELA04
PTIT( "MANUTEN€ÇO DE CONTAS A PAGAR")
SELECT 5
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELECT 4
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELECT 3
USE STATUS
GOTO TOP
SELECT 2
USE FORNECED INDEX FORNECED
GOTO TOP
SELECT 1
USE CPAGAR INDEX CPAGAR,DATA_CPG,DATA_CPV,NUM_CHEQ,DATA_NF
GOTO TOP
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
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
   eNUM_DOCUME = 0
   eDATA_NOTA  = CTOD("")
   eNOMINAL    = SPACE(1)
   eNUM_COMPRA = SPACE(10)
   DO WHILE .T.
      PLQUA("11")
      SELE 1
      @ 06,05 SAY "Lan‡amento    : " GET eNUM_LANC PICT "@R 9999999999"
      READ
      IF eNUM_LANC = 0
         CLOSE ALL
         RESTORE SCREEN FROM TELA04
         RETURN nil
      ELSE
         SEEK eNUM_LANC
         IF FOUND()
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
         ELSE
            PLQUA("01")
            PMSGR( "Lan‡amento Inexistente !",3)
            LOOP
         ENDIF
      ENDIF

   CLEAR GETS
   SELE 4
   GOTO TOP
   DO WHILE .T.
      @ 08,05 SAY "Cod. Despesa  : " GET eCLASSE_D PICT "@R 99999999"
      READ
      IF eCLASSE_D = 0
         SET COLOR TO W/N,+W/R
         eCLASSE_D = 0
         @ 08,00
         @ 08,05 SAY "Cod. Despesa  : "
         SELE 4
         GO TOP
         DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@S40"
         CAB[1]="    "
         CAB[2]="    "
         DBEDIT(8,20,8,75,CAMPO,"FCON_CTPG",PIC,CAB," "," - "," ")
         eCLASSE_D = CODIGO
         EXIT
      ELSE
         SEEK eCLASSE_D
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
      @ 09,05 SAY "Cod. Produto: " GET eCLASSE_PRO PICT "@R 99999999"
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
      @ 10,05 SAY "Fornecedor    : " GET eCODIGO_FOR PICT "@R 9999999"
      READ
      SELE 2
      SEEK eCODIGO_FOR
      IF eCODIGO_FOR = 0
         SET COLOR TO W/N,+W/R
         eCODIGO_FOR = 0
         @ 10,00
         @ 10,05 SAY "Fornecedor    : "
         SELE 2
         GO TOP
         DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@S40"
         CAB[1]="    "
         CAB[2]="    "
         DBEDIT(10,20,10,75,CAMPO,"FCON_CTPG",PIC,CAB," "," - "," ")
         eCODIGO_FOR = CODIGO
         EXIT
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
   @ 13,05 SAY "Num. Documento: " GET eNUM_DOCUME
   @ 13,45 SAY "Num. Compra   : " GET eNUM_COMPRA
   @ 14,05 SAY "Dt.Nota Fiscal: " GET eDATA_NOTA
   @ 15,05 SAY "Descri‡Æo     : " GET eDESCRICAO
   @ 16,05 SAY "Data Pagamento: " GET eDATA_REAL PICT "@D" 
   @ 17,05 SAY "Vlr. Realizado: " GET eREALIZADO PICT "@R 999,999,999,999.99"
   @ 18,05 SAY "C¢digo Conta  : " GET eCOD_CONTA PICT "@R 9999999"
   @ 19,05 SAY "Num. Cheque   : " GET eNUM_CHEQ  PICT "@R 9999999999"
   @ 20,05 SAY "Nominal (S/N) : " GET eNOMINAL   PICT "@! X" VALID eNOMINAL $("SN")
   @ 20,40 SAY "Int. Contabil : " GET eINTEGRA   
   READ
   SELE 1
   PLQUA("01")
   PPERG ( "Confirma a Altera‡Æo (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
      PMENSAG("Gravando, Aguarde ...")
      SELE 1
      IF F_BLOQR()
         REPLACE CLASSE_D   WITH eCLASSE_D,;
                 CODIGO_FOR WITH eCODIGO_FOR,;
                 EMPREENDIM WITH eCLASSE_PRO
         REPLACE DESCRICAO  WITH eDESCRICAO,;
                 DATA_PREV  WITH eDATA_PREV,;
                 PREVISTO   WITH ePREVISTO
         REPLACE DATA_REAL  WITH eDATA_REAL,;
                 REALIZADO  WITH eREALIZADO,;
                 COD_CONTA  WITH eCOD_CONTA
         REPLACE NUM_CHEQUE WITH eNUM_CHEQ,;
                 NUM_DOCUME WITH eNUM_DOCUME,;
                 DATA_NOTA  WITH eDATA_NOTA
         REPLACE NOMINAL    WITH eNOMINAL,;
                 INTEGRACAO WITH eINTEGRA,;
                 NUM_COMPRA WITH eNUM_COMPRA
         UNLOCK
      ENDIF
      F_LACESSO(sisUSUARIO,"CONTAS A PAGAR","ALTERACAO LANC. : "+ALLTRIM(STR(eNUM_LANC,5)))
   ENDIF
   ENDDO
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
