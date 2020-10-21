*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CHEA.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CONTAS A PAGAR - EMISS‡O CHEQUES"
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CHEA()
SAVE SCREEN TO TELA06
PTIT( "EMISSÇO DE CHEQUES CONTAS A PAGAR")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   PLIMPA()
   SELE 1
   USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG
   GOTO TOP
   SELE 2
   USE FORNECED INDEX FORNECED
   GOTO TOP
   SELE 3
   USE CONTAS INDEX CONTAS
   GOTO TOP
   SELE 4
   USE STATUS
   GOTO TOP
   eVALOR      = 0
   eVALOR_PAGO = 0
   eTOT_CPMF   = 0
   eCIDADE     = CIDADE
   PLQUA("01")
   eDATA_INI   = CTOD("")
   eDATA_FIM   = CTOD("")
   DO WHILE .T.
      @ 10,02 SAY "Periodo     : " GET eDATA_INI
      @ 11,02 SAY "Ate         : " GET eDATA_FIM
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
   DO WHILE .T.
      eCOD_CONTA = 0
      @ 12,02 SAY "Num. Conta  : " GET eCOD_CONTA PICT "@R 999,999,999"
      READ
      IF eCOD_CONTA <> 0
         SELE 3
         SEEK eCOD_CONTA
         IF FOUND()
            @ 12,30 SAY " AG. : "+ALLTRIM(COD_AGENCI)+" C/C : "+ALLTRIM(COD_CC)
            eCPMF_DESP = CPMF_DESP
            eCPMF_FORN = CPMF_FORN
            eCPMF_PROD = CPMF_PROD
            eCPMF_PERC = CPMF_PERC/100
            eCPMF_DATA = CPMF_DATA
            EXIT
         ELSE
            PLQUA("01")
            PMSGR( "Aten‡Æo !",3)
            PMENSAG("C¢digo de Conta Inv‡lido !")
            PPARA()
            LOOP
         ENDIF
      ENDIF
   ENDDO
   DO WHILE .T.
      eNUM_CHEQ = 0
      @ 13,02 SAY "Num. Cheque : " GET eNUM_CHEQ PICT "@R 999,999,999"
      READ
      IF eNUM_CHEQ <> 0
         EXIT
      ENDIF
   ENDDO
   PLQUA("01")
   eDT_CHEQ = SPACE(1)
   @ 23,00 SAY "Data de EmissÆo do Cheque (P)revista ou (N)ova ? " GET eDT_CHEQ PICT "@!X" VALID eDT_CHEQ $("PN")
   READ
   IF eDT_CHEQ = "N"
      eDATA_CHEQ   = eDATA_FIM 
      DO WHILE .T.
         @ 14,02 SAY "Data Cheque : " GET eDATA_CHEQ
         READ
         IF eDATA_CHEQ = CTOD("")
            PLQUA("01")
            PMSGR( "Data Invalida !",3)
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
      eFAZ = .F.
   ELSE
      eFAZ = .T.
   ENDIF
   SELE 1
   SET FILTER TO COD_CONTA = eCOD_CONTA .AND. DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. DATA_REAL = CTOD("") .AND. REALIZADO = 0 .AND. GRUPO = 1
   GOTO TOP
   IF EOF()
      PLQUA("01")
      PMSGR( "NÆo H  lan‡amentos !",3)
      CLOSE ALL
      RESTORE SCREEN FROM TELA06
      RETURN nil
   ENDIF
   PLQUA("01")
   eBANCO   = SPACE(1)
   eIMPRIME = .T.
   @ 23,00 SAY "Cheque do Bradesco, Real ou Outros (B/R/O) ? " GET eBANCO PICT "@!X" VALID eBANCO $("BRO")
   READ
   PLQUA("01")
   PPERG ( "Deseja cria‡Æo do lan‡amento de CPMF (S/N) ? ")
   IF sisRESP = "S"
      eCPMF_ANTE = eCPMF_DATA
      PLQUA("01")
      @ 23,00 SAY "Informe a Data do Lan‡amento de CPMF : " GET eCPMF_DATA
      READ
      IF eCPMF_ANTE <> eCPMF_DATA
         SELE 3
         SEEK eCOD_CONTA
         IF FOUND()
            IF F_BLOQR()
               REPLACE CPMF_DATA WITH eCPMF_DATA
               UNLOCK
            ENDIF
         ENDIF
      ENDIF
      eCPMF = .T.
   ELSE
      eCPMF = .F.
   ENDIF
   eARQUIVO = SPACE(60)
   eARQUIVO = sisDRIVE_REL+":\"+sisPASTA_REL+"CTAPAG.TXT"+SPACE(30)
   IF (ALLTRIM(sisDRIVE_REL) = "").and.(ALLTRIM(sisPASTA_REL) = "") 
      eARQUIVO = "C:\RELATORIOS\RELATORIO.TXT          "
   ENDIF        
   PMENSAG("Posicione o Formulario e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
   DO CASE
      CASE eBANCO = "B"
         LIN = PROW()
         SELE 1
         GOTO TOP
         eUNICOFORN = .T.
         eFORNECED  = CODIGO_FOR
         DO WHILE .NOT. EOF()
            IF eFORNECED <> CODIGO_FOR
               eUNICOFORN = .F.
            ENDIF
            eVALOR      = eVALOR      + PREVISTO
            eVALOR_PAGO = eVALOR_PAGO + PREVISTO
            eTOT_CPMF   = eTOT_CPMF   + (PREVISTO * eCPMF_PERC)
            SKIP
         ENDDO
         SELE 1
         GOTO TOP
         eNUM_LANC = LANCAMENTO
         @ LIN,00  SAY " "
         LIN = LIN + 1
         @ LIN,54  SAY eVALOR   PICT "@R 99,999,999.99"
         LIN = LIN + 1
         pNUM_EXT1 = SPACE(57)
         pNUM_EXT2 = SPACE(57)
         pNUM_EXT3 = SPACE(57)
         eNOMINAL  = "N"
         IF eFAZ
            eDATA = DTOC(DATA_PREV)
            DO PDATAEXT WITH eDATA
         ELSE
            eDATA = DTOC(eDATA_CHEQ)
            DO PDATAEXT WITH eDATA
         ENDIF
         DO PNUMEXT WITH eVALOR,52
         @ LIN,15  SAY pNUM_EXT1
         LIN = LIN + 2
         @ LIN,02  SAY pNUM_EXT2+REPLICATE("*",13)
         LIN = LIN + 2
         eCODIGO_FOR = CODIGO_FOR
         SELE 2
         SEEK eCODIGO_FOR
         IF eNOMINAL = "S" .OR. eUNICOFORN
            @ LIN,02  SAY NOME
         ENDIF
         LIN = LIN + 1
         @ LIN,26  SAY eCIDADE+" "+sisEXT
         LIN = LIN + 11
         @ LIN,00  SAY " "
         SELE 1
         SET DEVICE TO SCREEN
         SELE 1
         GOTO TOP
         DO WHILE .NOT. EOF()
            IF F_BLOQR()
               REPLACE   DATA_REAL   WITH CTOD(eDATA),;
                         REALIZADO   WITH PREVISTO,;
                         COD_CONTA   WITH eCOD_CONTA
               REPLACE   NUM_CHEQUE  WITH eNUM_CHEQ
               UNLOCK
            ENDIF
            SKIP
         ENDDO
         * Cria‡Æo do Lan‡amento de CPMF
         IF eCPMF
            SELE 4
            IF F_BLOQR()
               eNUM_LANC  = LANC_CPG
               REPLACE LANC_CPG WITH eNUM_LANC + 1
               UNLOCK
            ENDIF
            SELE 1
            APPEND BLANK
            IF F_BLOQR()
               REPLACE LANCAMENTO WITH eNUM_LANC,;
                       CLASSE_D   WITH eCPMF_DESP
               REPLACE EMPREENDIM WITH eCPMF_PROD,;
                       CODIGO_FOR WITH eCPMF_FORN
               REPLACE DESCRICAO  WITH "Ref. CPMF sobre cheques No."+ALLTRIM(STR(eNUM_CHEQ,5)),;
                       DATA_PREV  WITH eCPMF_DATA,;
                       PREVISTO   WITH eTOT_CPMF
               REPLACE DATA_REAL  WITH eCPMF_DATA,;
                       REALIZADO  WITH eTOT_CPMF,;
                       COD_CONTA  WITH eCOD_CONTA
               REPLACE NUM_CHEQUE WITH 0,;
                       NUM_DOCUME WITH SPACE(10),;
                       CONTABIL   WITH .T.
               REPLACE DATA_NOTA  WITH eCPMF_DATA,;
                       NOMINAL    WITH "S",;
                       INTEGRACAO WITH SPACE(1)
               REPLACE DATA_COMPE WITH eCPMF_DATA
               UNLOCK
            ENDIF
         ENDIF
         SELE 1
         eNUM_CHEQ = eNUM_CHEQ + 1
         SET DEVICE TO SCREEN
      CASE eBANCO = "R"
         LIN = PROW()
         SELE 1
         GOTO TOP
         eUNICOFORN = .T.
         eFORNECED  = CODIGO_FOR
         DO WHILE .NOT. EOF()
            IF eFORNECED <> CODIGO_FOR
               eUNICOFORN = .F.
            ENDIF
            eVALOR      = eVALOR + PREVISTO
            eVALOR_PAGO = eVALOR_PAGO + PREVISTO
            eTOT_CPMF   = eTOT_CPMF   + (PREVISTO * eCPMF_PERC)
            SKIP
         ENDDO
         SELE 1
         GOTO TOP
         eNUM_LANC = LANCAMENTO
         @ LIN,00  SAY " "
         LIN = LIN + 1
         @ LIN,54  SAY eVALOR   PICT "@R 99,999,999.99"
         LIN = LIN + 2
         pNUM_EXT1 = SPACE(57)
         pNUM_EXT2 = SPACE(57)
         pNUM_EXT3 = SPACE(57)
         eNOMINAL  = "N"
         IF eFAZ
            eDATA = DTOC(DATA_PREV)
            DO PDATAEXT WITH eDATA
         ELSE
            eDATA = DTOC(eDATA_CHEQ)
            DO PDATAEXT WITH eDATA
         ENDIF
         DO PNUMEXT WITH eVALOR,52
         @ LIN,15  SAY pNUM_EXT1
         LIN = LIN + 2
         @ LIN,02  SAY pNUM_EXT2+REPLICATE("*",13)
         LIN = LIN + 2
         eCODIGO_FOR = CODIGO_FOR
         SELE 2
         SEEK eCODIGO_FOR
         IF eNOMINAL = "S" .OR. eUNICOFORN
            @ LIN,02  SAY NOME
         ENDIF
         LIN = LIN + 2
         @ LIN,26  SAY eCIDADE+" "+sisEXT
         LIN = LIN + 09
         @ LIN,00  SAY " "
         SELE 1
         GOTO TOP
         DO WHILE .NOT. EOF()
            IF F_BLOQR()
               REPLACE   DATA_REAL   WITH CTOD(eDATA),;
                         REALIZADO   WITH PREVISTO,;
                         COD_CONTA   WITH eCOD_CONTA
               REPLACE   NUM_CHEQUE  WITH eNUM_CHEQ
               UNLOCK
            ENDIF
            SKIP
         ENDDO
         * Cria‡Æo do Lan‡amento de CPMF
         IF eCPMF
            SELE 4
            IF F_BLOQR()
               eNUM_LANC  = LANC_CPG
               REPLACE LANC_CPG WITH eNUM_LANC + 1
               UNLOCK
            ENDIF
            SELE 1
            APPEND BLANK
            IF F_BLOQR()
               REPLACE LANCAMENTO WITH eNUM_LANC,;
                       CLASSE_D   WITH eCPMF_DESP
               REPLACE EMPREENDIM WITH eCPMF_PROD,;
                       CODIGO_FOR WITH eCPMF_FORN
               REPLACE DESCRICAO  WITH "Ref. CPMF sobre cheques No."+ALLTRIM(STR(eNUM_CHEQ,5)),;
                       DATA_PREV  WITH eCPMF_DATA,;
                       PREVISTO   WITH eTOT_CPMF
               REPLACE DATA_REAL  WITH eCPMF_DATA,;
                       REALIZADO  WITH eTOT_CPMF,;
                       COD_CONTA  WITH eCOD_CONTA
               REPLACE NUM_CHEQUE WITH 0,;
                       NUM_DOCUME WITH SPACE(10),;
                       CONTABIL   WITH .T.
               REPLACE DATA_NOTA  WITH eCPMF_DATA,;
                       NOMINAL    WITH "S",;
                       INTEGRACAO WITH SPACE(1)
               REPLACE DATA_COMPE WITH eCPMF_DATA
               UNLOCK
            ENDIF
         ENDIF
         SELE 1
         SET DEVICE TO PRINT
         eNUM_CHEQ = eNUM_CHEQ + 1
         SET DEVICE TO SCREEN
      OTHERWISE
         LIN = PROW()
         SELE 1
         eUNICOFORN = .T.
         eFORNECED  = CODIGO_FOR
         DO WHILE .NOT. EOF()
            IF eFORNECED <> CODIGO_FOR
               eUNICOFORN = .F.
            ENDIF
            eVALOR      = eVALOR + PREVISTO
            eVALOR_PAGO = eVALOR_PAGO + PREVISTO
            eTOT_CPMF   = eTOT_CPMF   + (PREVISTO * eCPMF_PERC)
            SKIP
         ENDDO
         SELE 1
         GOTO TOP
         eNUM_LANC = LANCAMENTO
         @ LIN,00  SAY " "
         LIN = LIN + 1
         @ LIN,52  SAY eVALOR   PICT "@R 99,999,999.99"
         LIN = LIN + 2
         pNUM_EXT1 = SPACE(57)
         pNUM_EXT2 = SPACE(57)
         pNUM_EXT3 = SPACE(57)
         eNOMINAL  = "N"
         IF eFAZ
            eDATA = DTOC(DATA_PREV)
            DO PDATAEXT WITH eDATA
         ELSE
            eDATA = DTOC(eDATA_CHEQ)
            DO PDATAEXT WITH eDATA
         ENDIF
         DO PNUMEXT WITH eVALOR,52
         @ LIN,12  SAY pNUM_EXT1+"***"
         LIN = LIN + 1
         @ LIN,02  SAY pNUM_EXT2+REPLICATE("*",13)
         LIN = LIN + 2
         eCODIGO_FOR = CODIGO_FOR
         SELE 2
         SEEK eCODIGO_FOR
         IF eNOMINAL = "S" .OR. eUNICOFORN
            @ LIN,02  SAY NOME
         ENDIF
         LIN = LIN + 1
         eTAMANHO = LEN(ALLTRIM(eCIDADE))
         @ LIN,26  SAY REPLICATE(" ",20-eTAMANHO)+ALLTRIM(eCIDADE)+", "+sisEXT
         LIN = LIN + 11
         @ LIN,00  SAY " "
         SELE 1
         SET DEVICE TO SCREEN
         SELE 1
         GOTO TOP
         DO WHILE .NOT. EOF()
            IF F_BLOQR()
               REPLACE   DATA_REAL   WITH CTOD(eDATA),;
                         REALIZADO   WITH PREVISTO,;
                         COD_CONTA   WITH eCOD_CONTA
               REPLACE   NUM_CHEQUE  WITH eNUM_CHEQ
               UNLOCK
            ENDIF
            SKIP
         ENDDO
         * Cria‡Æo do Lan‡amento de CPMF
         IF eCPMF
            SELE 4
            IF F_BLOQR()
               eNUM_LANC  = LANC_CPG
               REPLACE LANC_CPG WITH eNUM_LANC + 1
               UNLOCK
            ENDIF
            SELE 1
            APPEND BLANK
            IF F_BLOQR()
               REPLACE LANCAMENTO WITH eNUM_LANC,;
                       CLASSE_D   WITH eCPMF_DESP
               REPLACE EMPREENDIM WITH eCPMF_PROD,;
                       CODIGO_FOR WITH eCPMF_FORN
               REPLACE DESCRICAO  WITH "Ref. CPMF sobre cheques No."+ALLTRIM(STR(eNUM_CHEQ,5)),;
                       DATA_PREV  WITH eCPMF_DATA,;
                       PREVISTO   WITH eTOT_CPMF
               REPLACE DATA_REAL  WITH eCPMF_DATA,;
                       REALIZADO  WITH eTOT_CPMF,;
                       COD_CONTA  WITH eCOD_CONTA
               REPLACE NUM_CHEQUE WITH 0,;
                       NUM_DOCUME WITH SPACE(10),;
                       CONTABIL   WITH .T.
               REPLACE DATA_NOTA  WITH eCPMF_DATA,;
                       NOMINAL    WITH "S",;
                       INTEGRACAO WITH SPACE(1)
               REPLACE DATA_COMPE WITH eCPMF_DATA
               UNLOCK
            ENDIF
         ENDIF
         SELE 1
         SET DEVICE TO PRINT
         eNUM_CHEQ = eNUM_CHEQ + 1
         SET DEVICE TO SCREEN
   ENDCASE
ENDIF
SET PRINT TO
CLOSE ALL
RESTORE SCREEN FROM TELA06
RETURN nil
*************************  fim  *********************************
