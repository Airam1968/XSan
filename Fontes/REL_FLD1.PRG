*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_FLD1.PRG
* Data        : 01/01/99
* Objetivo    : Relatorio dos Fluxo de Caixa Previsto - Diario
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_FLD1()
SAVE SCREEN TO TELA04
PTIT( "RELATORIO DE FLUXO DE CAIXA - DIARIO 1")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "N"
   RESTORE SCREEN FROM TELA04
   RETURN nil
ENDIF
SELE 7
USE BANCOS INDEX BANCOS
GOTO TOP
SELE 4
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 5
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
PLQUA("01")
PPERG ( "Deseja Relat‡rio com Informa‡‡es de Descricao (S/N) ? ")
IF sisRESP = "S"
   eFAZ = .T.
ELSE
   eFAZ = .F.
ENDIF
PLQUA("01")
PPERG ( "Deseja Relat¢rio com Informa‡äes de Previsao Nao Realizada (S/N) ? ")
IF sisRESP = "S"
   eFAZ2 = .T.
ELSE
   eFAZ2 = .F.
ENDIF
PLQUA("01")
PPERG ( "Deseja Relat¢rio com Apenas Totais (S/N) ? ")
IF sisRESP = "S"
   eFAZ3 = .T.
ELSE
   eFAZ3 = .F.
ENDIF
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
eNOME_EMPRE = SPACE(50)
SELE 7
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
DO WHILE .T.
   eCLAS_PRO = 0
   PLQUA("01")
   @ 23,00 SAY " Produto   ou  (0)  para  Todos  : " GET eCLAS_PRO PICT "@R 9999999"
   READ
   SELE 7
   SEEK eCLAS_PRO
   IF .NOT. FOUND()
      IF eCLAS_PRO <> 0
         PLQUA("01")
		 PMSGR( "  Produto   Inexistente   ",3)
		 SELE 7
         LOOP
      ELSE
         @ 23,48 SAY " - Escritorio"
         SELE 7
         EXIT
      ENDIF
   ELSE
      @ 23,48 SAY " - "+ALLTRIM(DESCRICAO)
      eNOME_EMPRE = DESCRICAO
      SELE 7
      EXIT
   ENDIF
ENDDO
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"FLUXOCAIXA.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "FLUXOCAIXA.TXT"+SPACE(10) 
ENDIF    
eSAIDA   = SPACE(10)
DO WHILE .T.
   PLQUA("01")
   PPERG ( "Deseja Relat¢rio em arquivo texto no Drive (X:) (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
	  @ 23,00 SAY "CONFIRME O DESTINO  : " GET eARQUIVO
      READ
      eARQUIVO = ALLTRIM(eARQUIVO)
      eSAIDA = "ARQUIVO"
      EXIT
   ELSE
      DO PIMPR
      eSAIDA = "IMPRESSORA"
      EXIT
   ENDIF
ENDDO

PLQUA("01")
PPERG ( "Deseja Consolida‡Æo de Saldos (S/N) ? ")
IF sisRESP = "N"
   SELE 6
   USE FLUXO_D
   INDEX ON DATA TO FLUXO_D
   GOTO TOP
   IF eCLAS_PRO = 0
      SELE 1
      USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
      SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. REALIZADO = 0 .AND. DATA_COMPE = CTOD("") .AND. CONTABIL
      GOTO TOP
   ELSE
      SELE 1
      USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
      SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. REALIZADO = 0 .AND. DATA_COMPE = CTOD("") .AND. CONTABIL
      GOTO TOP
   ENDIF
   IF eCLAS_PRO = 0
      SELE 3
      USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
      SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO = 0 .AND. CONTABIL
      GOTO TOP
   ELSE
      SELE 3
      USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
      SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO = 0 .AND. CONTABIL
      GOTO TOP
   ENDIF
ELSE
   * Consolidacao de Saldos Normais
   SET EXCLUSIVE ON
   SELE 6
   USE FLUXO_D
   GOTO TOP
   DELE ALL FOR DATA >= eDATA_INI .AND. DATA <= eDATA_FIM
   PACK
   GOTO TOP
   SET EXCLUSIVE OFF
   SELE 6
   USE
   USE FLUXO_D
   INDEX ON DATA TO FLUXO_D
   GOTO TOP
   IF eCLAS_PRO = 0
      SELE 1
      USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
      SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. REALIZADO = 0 .AND. DATA_COMPE = CTOD("") .AND. CONTABIL
      GOTO TOP
   ELSE
      SELE 1
      USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
      SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. REALIZADO = 0 .AND. DATA_COMPE = CTOD("") .AND. CONTABIL
      GOTO TOP
   ENDIF
   IF eCLAS_PRO = 0
      IF eFAZ2
         SELE 3
         USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
         SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO = 0 .AND. CONTABIL 
         GOTO TOP
      ELSE
         SELE 3
         USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
         SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO = 0 .AND. CONTABIL
         GOTO TOP
      ENDIF
   ELSE
      IF eFAZ2
         SELE 3
         USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
         SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO = 0 .AND. CONTABIL 
         GOTO TOP
      ELSE
         SELE 3
         USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
         SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO = 0 .AND. CONTABIL
         GOTO TOP
      ENDIF
   ENDIF
   eDATA_AUX = eDATA_INI
   eSALDO_I  = 0
   eAPLIC_D  = 0
   eAPLIC_B  = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o Saldo Inicial : " GET eSALDO_I PICT "@R 999,999,999.99"
   READ
   eDATA_AUX = eDATA_INI
   SELE 6
   APPEND BLANK
   IF F_BLOQR()
      REPLACE DATA       WITH eDATA_AUX,;
              CONTA      WITH 0,;
              DESCRICAO  WITH "SALDO INICIAL "
      REPLACE SALDO_I    WITH eSALDO_I,;
              ENTRADAS   WITH 0,;
              SAIDAS     WITH 0
      REPLACE SALDO_F    WITH eSALDO_I,;
              APLICACAOD WITH eAPLIC_D,;
              APLICACAOB WITH eAPLIC_B
      UNLOCK
      eTRANSP  = eSALDO_I
      eAPLIC_D = APLICACAOD
      eAPLIC_B = APLICACAOB
   ENDIF
   DO WHILE eDATA_AUX <= eDATA_FIM
      PLQUA("01")
	  PMENSAG("Aguarde o Processamento, Dia "+DTOC(eDATA_AUX))
      IF eFAZ2
         IF eCLAS_PRO = 0
            SELE 3
            GOTO TOP
            SET FILTER TO DATA_PREV = eDATA_AUX .AND. (REALIZADO = 0 .OR. (REALIZADO <> 0 .AND. PREVISTO > REALIZADO))
            GOTO TOP
         ELSE
            SELE 3
            GOTO TOP
            SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_PREV = eDATA_AUX .AND. (REALIZADO = 0 .OR. (REALIZADO <> 0 .AND. PREVISTO > REALIZADO))
            GOTO TOP
         ENDIF
      ELSE
         IF eCLAS_PRO = 0
            SELE 3
            GOTO TOP
            SET FILTER TO DATA_PREV = eDATA_AUX .AND. REALIZADO = 0 .AND. CONTABIL
            GOTO TOP
         ELSE
            SELE 3
            GOTO TOP
            SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_PREV = eDATA_AUX .AND. REALIZADO = 0 .AND. CONTABIL
            GOTO TOP
         ENDIF
      ENDIF
      DO WHILE .NOT. EOF()
         eVLR_REAL  = 0
         eCLASSE_R  = 0
         eDESCRICAO = SPACE(40)
         IF REALIZADO = 0
            eVLR_REAL  = PREVISTO
         ELSE
            eVLR_REAL  = PREVISTO    && alteracao teste para viga 04/11/1999 ( - REALIZADO )
         ENDIF
         eCLASSE_R   = CLASSE_R
         eDESCRICA2  = DESCRICAO
         eEMPREENDIM = EMPREENDIM
         IF eCLASSE_R < 0
            IF eCLASSE_R = -1
               eAPLIC_D = eAPLIC_D - eVLR_REAL
            ENDIF
            IF eCLASSE_R = -2
               eAPLIC_B = eAPLIC_B - eVLR_REAL
            ENDIF
         ENDIF
         SELE 5
         SEEK eCLASSE_R
         eDESCRICAO = DESCRICAO
         SELE 6
         APPEND BLANK
         IF F_BLOQR()
            REPLACE DATA       WITH eDATA_AUX,;
                    CONTA      WITH 0,;
                    DESCRICAO  WITH eDESCRICAO
            REPLACE SALDO_I    WITH eTRANSP,;
                    ENTRADAS   WITH eVLR_REAL,;
                    SAIDAS     WITH 0
            REPLACE SALDO_F    WITH (SALDO_I+ENTRADAS)-SAIDAS,;
                    APLICACAOD WITH eAPLIC_D,;
                    APLICACAOB WITH eAPLIC_B
            REPLACE DESCRICA2  WITH eDESCRICA2,;
                    EMPREENDIM WITH eEMPREENDIM
            UNLOCK
            eTRANSP = SALDO_F
            eAPLIC_D = APLICACAOD
            eAPLIC_B = APLICACAOB
         ENDIF
         SELE 3
         SKIP
      ENDDO
      IF eCLAS_PRO = 0
         SELE 1
         GOTO TOP
         SET FILTER TO DATA_PREV = eDATA_AUX .AND. REALIZADO = 0 .AND. CONTABIL
         GOTO TOP
      ELSE
         SELE 1
         GOTO TOP
         SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_PREV = eDATA_AUX .AND. REALIZADO = 0 .AND. CONTABIL
         GOTO TOP
      ENDIF
      DO WHILE .NOT. EOF()
         eVLR_REAL   = 0
         eCLASSE_D   = 0
         eDESCRICAO  = SPACE(40)
         eNUM_CHEQUE = 0
         eVLR_REAL   = PREVISTO
         eCLASSE_D   = CLASSE_D
         eNUM_CHEQUE = NUM_CHEQUE
         eDESCRICA2  = DESCRICAO
         eEMPREENDIM = EMPREENDIM
         IF eCLASSE_D < 0
            IF eCLASSE_D = -1
               eAPLIC_D = eAPLIC_D + eVLR_REAL
            ENDIF
            IF eCLASSE_D = -2
               eAPLIC_B = eAPLIC_B + eVLR_REAL
            ENDIF
         ENDIF
         SELE 4
         SEEK eCLASSE_D
         eDESCRICAO = DESCRICAO
         SELE 6
         APPEND BLANK
         IF F_BLOQR()
            REPLACE DATA       WITH eDATA_AUX,;
                    CONTA      WITH 0,;
                    DESCRICAO  WITH eDESCRICAO
            REPLACE SALDO_I    WITH eTRANSP,;
                    ENTRADAS   WITH 0,;
                    SAIDAS     WITH eVLR_REAL
            REPLACE SALDO_F    WITH (SALDO_I+ENTRADAS)-SAIDAS,;
                    NUM_CHEQUE WITH eNUM_CHEQUE
            REPLACE APLICACAOD WITH eAPLIC_D,;
                    APLICACAOB WITH eAPLIC_B,;
                    DESCRICA2  WITH eDESCRICA2
            REPLACE EMPREENDIM WITH eEMPREENDIM
            UNLOCK
            eTRANSP = SALDO_F
            eAPLIC_D = APLICACAOD
            eAPLIC_B = APLICACAOB
         ENDIF
         SELE 1
         SKIP
      ENDDO
      eDATA_AUX = eDATA_AUX + 1
   ENDDO
ENDIF
SELE 6
GOTO TOP
SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM
GOTO TOP
      IF eSAIDA = "ARQUIVO"
         SET PRINT ON
         SET DEVICE TO PRINT
         SET PRINTER TO &eARQUIVO
      ELSE
         PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
         PPARA()
         DO PIMPR
         SET DEVICE TO PRINT
      ENDIF
eDATA_AUX = DATA
eSUB_ENTR = 0
eSUB_SAID = 0
eSUB_SALD = 0
eTOT_ENTR = 0
eTOT_SAID = 0
eTOT_SALD = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,05  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO DIARIO DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 04,00  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "DATA"
   @ 06,10  SAY "CONTA"
   @ 06,20  SAY "DESCRICAO"
   @ 06,50  SAY "PRODUTO"
   @ 06,60  SAY "SALDO INICIAL"
   @ 06,80  SAY "ENTRADAS"
   @ 06,100 SAY "SAIDAS"
   @ 06,120 SAY "SALDO FINAL"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF eDATA_AUX = DATA
         IF .NOT. eFAZ3
            @ LIN,00  SAY DATA                         
            @ LIN,10  SAY CONTA      PICT "999999"
            @ LIN,20  SAY SUBSTR(DESCRICAO,1,28)
            @ LIN,50  SAY EMPREENDIM PICT "@R 999999999"
            @ LIN,60  SAY SALDO_I    PICT "@R 999,999,999.99"
            @ LIN,80  SAY ENTRADAS   PICT "@R 999,999,999.99"
            @ LIN,100 SAY SAIDAS     PICT "@R 999,999,999.99"
            @ LIN,119 SAY SALDO_F    PICT "@R 999,999,999.99"
            LIN = LIN + 1
            IF eFAZ
               @ LIN,20  SAY SUBSTR(DESCRICA2,1,28)
               LIN = LIN + 1
            ENDIF
         ENDIF
         eDATA_ANT = DATA
         eSUB_ENTR = eSUB_ENTR + ENTRADAS
         eSUB_SAID = eSUB_SAID + SAIDAS
         eSALDO_F  = SALDO_F
            SKIP
            DO PTECLA
            IF sisINTERR
               IF eSAIDA = "ARQUIVO"
                  SET PRINT OFF
                  SET DEVICE TO SCREEN
               ELSE
                  SET DEVICE TO SCREEN
               ENDIF
               CLOSE ALL
               RETURN nil
            ENDIF
      ELSE
         @ LIN,80  SAY REPLICATE("_",50)
         LIN = LIN + 1
*        @ LIN,02  SAY "SubTotal: "
         @ LIN,30  SAY eDATA_ANT
         @ LIN,80  SAY eSUB_ENTR  PICT "@R 999,999,999.99"
         @ LIN,100 SAY eSUB_SAID  PICT "@R 999,999,999.99"
         @ LIN,119 SAY eSALDO_F   PICT "@R 999,999,999.99"
         LIN = LIN + 2
         eTOT_ENTR = eTOT_ENTR + eSUB_ENTR
         eTOT_SAID = eTOT_SAID + eSUB_SAID
         eSALDO_F  = SALDO_F
         eSUB_ENTR = 0
         eSUB_SAID = 0
         eSUB_SALD = 0
         eDATA_AUX = DATA
      ENDIF
   ENDDO
   IF EOF()
      @ LIN,80  SAY REPLICATE("_",50)
      LIN = LIN + 1
      @ LIN,02  SAY "SubTotal: "
      @ LIN,30  SAY eDATA_ANT
      @ LIN,80  SAY eSUB_ENTR  PICT "@R 999,999,999.99"
      @ LIN,100 SAY eSUB_SAID  PICT "@R 999,999,999.99"
      @ LIN,119 SAY eSALDO_F   PICT "@R 999,999,999.99"
      eTOT_ENTR = eTOT_ENTR + eSUB_ENTR
      eTOT_SAID = eTOT_SAID + eSUB_SAID
*     eSALDO_F  = SALDO_F
      eSUB_ENTR = 0
      eSUB_SAID = 0
      eSUB_SALD = 0
      eDATA_AUX = DATA
      LIN = LIN + 2
      @ LIN,80  SAY REPLICATE("_",50)
      LIN = LIN + 1
      @ LIN,02  SAY "Totais : "
      @ LIN,80  SAY eTOT_ENTR  PICT "@R 999,999,999.99"
      @ LIN,100 SAY eTOT_SAID  PICT "@R 999,999,999.99"
      @ LIN,119 SAY eSALDO_F   PICT "@R 999,999,999.99"
      LIN = LIN + 2
      IF LIN >= 52
         @ 58,00 SAY REPLICATE("=",135)
         @ 59,104 SAY sisTIT3
         @ 01,00 SAY " "+sisLETRAN
         @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
         @ 01,120 SAY "Data : "+dtoc(DATE())
         @ 02,00  SAY "Sistema : "+sisTIT2
         @ 02,121 SAY "Relat: SAN004"
         @ 03,05  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO DIARIO DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
         @ 04,00  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
         @ 05,00  SAY REPLICATE("=",135)
         @ 06,02  SAY "DATA"
         @ 06,10  SAY "CONTA"
         @ 06,20  SAY "DESCRICAO"
         @ 06,50  SAY " PRODUTO "
         @ 06,60  SAY "SALDO INICIAL"
         @ 06,80  SAY "ENTRADAS"
         @ 06,100 SAY "SAIDAS"
         @ 06,120 SAY "SALDO FINAL"
         @ 08,00 SAY REPLICATE("=",135)
         LIN = 9
      ENDIF
      GOTO BOTTOM
      @ LIN,02 SAY "Saldo Final          : "
      @ LIN,119 SAY SALDO_F     PICT "@R 999,999,999.99"
      SELE 6
      SKIP
   ENDIF
   @ 56,00 SAY REPLICATE("=",135)
   @ 57,95 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
IF eSAIDA = "ARQUIVO"
   SET PRINT TO
   SET PRINT OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINT TO
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
