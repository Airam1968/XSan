*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_FLPM.PRG
* Data        : 01/01/99
* Objetivo    : Relatorio dos Fluxo de Caixa - Previsto 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_FLPM()
SAVE SCREEN TO TELA06
PTIT( "RELAT‡RIO DE FLUXO DE CAIXA - PREVISTO")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "N"
   RESTORE SCREEN FROM TELA06
   RETURN nil
ENDIF
SELE 1
USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
GOTO TOP
SELE 2
USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG
GOTO TOP
SELE 3
SET EXCLUSIVE ON
USE FLUXO_C
DELE ALL
PACK
SET EXCLUSIVE OFF
SELE 4
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 5
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SET COLOR TO W/N,+W/BG
@ 11,49 CLEAR TO 16,70
@ 11,49 TO 16,70
@ 12,51 PROMPT " 1o. Trimestre    " 
@ 13,51 PROMPT " 2o. Trimestre    " 
@ 14,51 PROMPT " 3o. Trimestre    " 
@ 15,51 PROMPT " 4o. Trimestre    " 
READ
sisOP=1
MENU TO sisOP
DO CASE
  CASE sisOP = 1
     eDATA_INI = "01/01/"
     eDATA_FIM = "31/03/"
     eTRIM     = 1
  CASE sisOP = 2
     eDATA_INI = "01/04/"
     eDATA_FIM = "30/06/"
     eTRIM     = 2
  CASE sisOP = 3
     eDATA_INI = "01/07/"
     eDATA_FIM = "30/09/"
     eTRIM     = 3
  CASE sisOP = 4
     eDATA_INI = "01/10/"
     eDATA_FIM = "31/12/"
     eTRIM     = 4
ENDCASE
eANO = 0
DO WHILE .T.
   PLQUA("01")
   @ 23,00 SAY "Informe o Ano com o formato de 4 digitos : " GET eANO PICT "@R 9999"
   READ
   IF eANO = 0
      PLQUA("01")
      PMSGR( "Atencáo !",3)
      PMENSAG("Ano Inv†lido !")
      LOOP
   ELSE
      eANO = ALLTRIM(STR(eANO,6))
      EXIT
   ENDIF
ENDDO
DO WHILE .T.
   PLQUA("01")
   eSALDO = 0
   @ 23,00 SAY "Informe o Saldo Inicial : " GET eSALDO PICT "@R 999,999,999.99"
   READ
   IF eSALDO <> 0 
      EXIT
   ENDIF
ENDDO
eNOME_EMPRE = SPACE(50)
SELE 6
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
DO WHILE .T.
   eCLAS_PRO = 0
   PLQUA("01")
   @ 23,00 SAY "Produto ou (0) para Todos : " GET eCLAS_PRO PICT "@R 9999999"
   READ
   SELE 6
   SEEK eCLAS_PRO
   IF .NOT. FOUND()
      IF eCLAS_PRO <> 0
         PLQUA("01")
		 PMSGR( "Produto Inexistente",3)
         SELE 6
         LOOP
      ELSE
         @ 23,48 SAY " - Escritorio"
         SELE 6
         EXIT
      ENDIF
   ELSE
      @ 23,48 SAY " - "+ALLTRIM(DESCRICAO)
      eNOME_EMPRE = DESCRICAO
      SELE 6
      EXIT
   ENDIF
ENDDO
eDATA_INI   = ALLTRIM(eDATA_INI)+ALLTRIM(eANO)
eDATA_FIM   = ALLTRIM(eDATA_FIM)+ALLTRIM(eANO)
eDATA_INI   = CTOD(eDATA_INI)
eDATA_FIM   = CTOD(eDATA_FIM)
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

PMENSAG("Aguarde a Preparaá∆o do Arquivo, de "+DTOC(eDATA_INI)+" atá "+DTOC(eDATA_FIM)+"... ")
IF eCLAS_PRO = 0
   SELE 1
   SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. CONTABIL
   GOTO TOP
ELSE
   SELE 1
   SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. CONTABIL
   GOTO TOP
ENDIF
DO WHILE .NOT. EOF()
   eDATA_ATU  = CTOD("")
   eDATA_ATU  = DATA_PREV
   eDIA       = VAL(SUBSTR(DTOC(eDATA_ATU),1,2))
   eMES       = VAL(SUBSTR(DTOC(eDATA_ATU),4,2))   
   eCODIGO    = CLASSE_R
   SELE 5
   SEEK eCODIGO
   eDESCR_R = DESCRICAO
   SELE 1
   eDESCRICAO = DESCRICAO
   IF REALIZADO = 0
      ePREVISTO  = PREVISTO
   ELSE
      ePREVISTO  = REALIZADO
   ENDIF
   DO CASE
      CASE eDIA >= 1  .AND. eDIA <= 10 
         eDEZENA = 1
      CASE eDIA >= 11 .AND. eDIA <= 20
         eDEZENA = 2
      CASE eDIA >= 21 .AND. eDIA <= 31
         eDEZENA = 3
   ENDCASE
   SELE 3
   GOTO TOP
   PLQUA("01")
   PMENSAG("Pesquisando : "+ALLTRIM(eDESCR_R))
   LOCATE FOR CODIGO = eCODIGO .AND. R_D = "R"
   IF EOF()
      APPEND BLANK
      IF F_BLOQR()
         REPLACE CODIGO    WITH eCODIGO
         REPLACE DESCRICAO WITH eDESCR_R
         UNLOCK
      ENDIF
   ENDIF
   IF F_BLOQR()
      DO CASE
         CASE eMES = 1 .OR. eMES = 4 .OR. eMES = 7 .OR. eMES = 10
            DO CASE
               CASE eDEZENA = 1
                  REPLACE DEZENA01 WITH DEZENA01 + ePREVISTO,;
                          R_D      WITH "R"
               CASE eDEZENA = 2
                  REPLACE DEZENA02 WITH DEZENA02 + ePREVISTO,;
                          R_D      WITH "R"
               CASE eDEZENA = 3
                  REPLACE DEZENA03 WITH DEZENA03 + ePREVISTO,;
                          R_D      WITH "R"
            ENDCASE
         CASE eMES = 2 .OR. eMES = 5 .OR. eMES = 8 .OR. eMES = 11
            DO CASE
               CASE eDEZENA = 1
                  REPLACE DEZENA04 WITH DEZENA04 + ePREVISTO,;
                          R_D      WITH "R"
               CASE eDEZENA = 2
                  REPLACE DEZENA05 WITH DEZENA05 + ePREVISTO,;
                          R_D      WITH "R"
               CASE eDEZENA = 3
                  REPLACE DEZENA06 WITH DEZENA06 + ePREVISTO,;
                          R_D      WITH "R"
            ENDCASE
         CASE eMES = 3 .OR. eMES = 6 .OR. eMES = 9 .OR. eMES = 12
            DO CASE
               CASE eDEZENA = 1
                  REPLACE DEZENA07 WITH DEZENA07 + ePREVISTO,;
                          R_D      WITH "R"
               CASE eDEZENA = 2
                  REPLACE DEZENA08 WITH DEZENA08 + ePREVISTO,;
                          R_D      WITH "R"
               CASE eDEZENA = 3
                  REPLACE DEZENA09 WITH DEZENA09 + ePREVISTO,;
                          R_D      WITH "R"
            ENDCASE
      ENDCASE
      UNLOCK
   ENDIF
   SELE 1
   SKIP
ENDDO
IF eCLAS_PRO = 0
   SELE 2
   SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. CONTABIL
   GOTO TOP
ELSE
   SELE 2
   SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. CONTABIL
   GOTO TOP
ENDIF
DO WHILE .NOT. EOF()
   eDATA_ATU  = CTOD("")
   eDATA_ATU  = DATA_PREV
   eDIA       = VAL(SUBSTR(DTOC(eDATA_ATU),1,2))
   eMES       = VAL(SUBSTR(DTOC(eDATA_ATU),4,2))   
   eCODIGO    = CLASSE_D  
   SELE 4
   SEEK eCODIGO
   eDESCR_D = DESCRICAO
   SELE 2
   eDESCRICAO = DESCRICAO
   IF REALIZADO = 0
      ePREVISTO  = PREVISTO
   ELSE
      ePREVISTO  = REALIZADO
   ENDIF
   DO CASE
      CASE eDIA >= 1  .AND. eDIA <= 10 
         eDEZENA = 1
      CASE eDIA >= 11 .AND. eDIA <= 20
         eDEZENA = 2
      CASE eDIA >= 21 .AND. eDIA <= 31
         eDEZENA = 3
   ENDCASE
   SELE 3
   GOTO TOP
   PLQUA("01")
   PMENSAG("Pesquisando : "+ALLTRIM(eDESCRICAO))
   LOCATE FOR CODIGO = eCODIGO .AND. R_D = "D"
   IF EOF()
      APPEND BLANK
      IF F_BLOQR()
         REPLACE CODIGO    WITH eCODIGO,;
                 DESCRICAO WITH eDESCR_D
         UNLOCK
      ENDIF
   ENDIF
   IF F_BLOQR()
      DO CASE
         CASE eMES = 1 .OR. eMES = 4 .OR. eMES = 7 .OR. eMES = 10
            DO CASE
               CASE eDEZENA = 1
                  REPLACE DEZENA01 WITH DEZENA01 + ePREVISTO,;
                          R_D      WITH "D"
               CASE eDEZENA = 2
                  REPLACE DEZENA02 WITH DEZENA02 + ePREVISTO,;
                          R_D      WITH "D"
               CASE eDEZENA = 3
                  REPLACE DEZENA03 WITH DEZENA03 + ePREVISTO,;
                          R_D      WITH "D"
            ENDCASE
         CASE eMES = 2 .OR. eMES = 5 .OR. eMES = 8 .OR. eMES = 11
            DO CASE
               CASE eDEZENA = 1
                  REPLACE DEZENA04 WITH DEZENA04 + ePREVISTO,;
                          R_D      WITH "D"
               CASE eDEZENA = 2
                  REPLACE DEZENA05 WITH DEZENA05 + ePREVISTO,;
                          R_D      WITH "D"
               CASE eDEZENA = 3
                  REPLACE DEZENA06 WITH DEZENA06 + ePREVISTO,;
                          R_D      WITH "D"
            ENDCASE
         CASE eMES = 3 .OR. eMES = 6 .OR. eMES = 9 .OR. eMES = 12
            DO CASE
               CASE eDEZENA = 1
                  REPLACE DEZENA07 WITH DEZENA07 + ePREVISTO,;
                          R_D      WITH "D"
               CASE eDEZENA = 2
                  REPLACE DEZENA08 WITH DEZENA08 + ePREVISTO,;
                          R_D      WITH "D"
               CASE eDEZENA = 3
                  REPLACE DEZENA09 WITH DEZENA09 + ePREVISTO,;
                          R_D      WITH "D"
            ENDCASE
      ENDCASE
      UNLOCK
   ENDIF
   SELE 2
   SKIP
ENDDO
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
SELE 3
GOTO TOP
eSUB_R01  = 0
eSUB_R02  = 0
eSUB_R03  = 0
eSUB_R04  = 0
eSUB_R05  = 0
eSUB_R06  = 0
eSUB_R07  = 0
eSUB_R08  = 0
eSUB_R09  = 0
eSUB_D01  = 0
eSUB_D02  = 0
eSUB_D03  = 0
eSUB_D04  = 0
eSUB_D05  = 0
eSUB_D06  = 0
eSUB_D07  = 0
eSUB_D08  = 0
eSUB_D09  = 0
eR_D      = R_D
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,02  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 04,00  SAY "1o. Mes do Trimestre - "+IIF(eTRIM = 1,"Jan.",IIF(eTRIM = 2,"Abr.",IIF(eTRIM = 3,"Jul.","Out.")))
   @ 04,40  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
   @ 05,00  SAY REPLICATE("=",134)
   @ 06,02  SAY "Codigo"
   @ 06,12  SAY "Descricao"
   @ 06,53  SAY "|"
   @ 06,54  SAY "1a. Dezena"
   @ 06,70  SAY "2a. Dezena"
   @ 06,85  SAY "3a. Dezena"
   @ 06,100 SAY "Totais"
   @ 06,120 SAY "Limites"
   @ 06,134 SAY "|"
   @ 07,00 SAY REPLICATE("=",134)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      SELE 3
      @ LIN,02  SAY CODIGO PICT "@R 9999999"
      @ LIN,12  SAY SUBSTR(DESCRICAO,1,40)
      @ LIN,53  SAY "|"
      @ LIN,54  SAY DEZENA01 PICT "@R 999,999,999.99"
      @ LIN,70  SAY DEZENA02 PICT "@R 999,999,999.99"
      @ LIN,85  SAY DEZENA03 PICT "@R 999,999,999.99"
      eTOTAL = 0
      eTOTAL = DEZENA01 + DEZENA02 + DEZENA03
      @ LIN,100 SAY eTOTAL   PICT "@R 999,999,999.99"
      SELE 3
      eCODIGO = CODIGO
      IF R_D = "D"
         SELE 4
         SEEK eCODIGO
         IF LIMITE < eTOTAL 
            @ LIN,120  SAY LIMITE  PICT "@R 999999999.99"
         ELSE
            @ LIN,120  SAY LIMITE  PICT "@R 999999999.99"
            @ LIN,120  SAY LIMITE  PICT "@R 999999999.99"
         ENDIF
         SELE 3
      ENDIF
      @ LIN,134 SAY "|"
      IF R_D = "R"
         eSUB_R01 = eSUB_R01 + DEZENA01 
         eSUB_R02 = eSUB_R02 + DEZENA02 
         eSUB_R03 = eSUB_R03 + DEZENA03 
      ELSE
         eSUB_D01 = eSUB_D01 + DEZENA01 
         eSUB_D02 = eSUB_D02 + DEZENA02 
         eSUB_D03 = eSUB_D03 + DEZENA03 
      ENDIF
      LIN = LIN + 1
      IF eR_D <> R_D
         @ LIN,00 SAY REPLICATE("-",134)
         LIN = LIN + 1
         IF eR_D = "R"
            @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
            @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
            @ LIN,53  SAY "|"
            @ LIN,54  SAY eSUB_R01 PICT "@R 999,999,999.99"
            @ LIN,70  SAY eSUB_R02 PICT "@R 999,999,999.99"
            @ LIN,85  SAY eSUB_R03 PICT "@R 999,999,999.99"
            @ LIN,134 SAY "|"
            LIN = LIN + 2
         ELSE
            @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
            @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
            @ LIN,53  SAY "|"
            @ LIN,54  SAY eSUB_D01 PICT "@R 999,999,999.99"
            @ LIN,70  SAY eSUB_D02 PICT "@R 999,999,999.99"
            @ LIN,85  SAY eSUB_D03 PICT "@R 999,999,999.99"
            @ LIN,134 SAY "|"
            LIN = LIN + 2
         ENDIF
      ENDIF   
      SELE 3
      SKIP
      eR_D = R_D
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
   ENDDO
   IF EOF()
      @ LIN,00 SAY REPLICATE("-",134)
      LIN = LIN + 1
      @ LIN,12 SAY "TOTAIS             : "
      @ LIN,12 SAY "TOTAIS             : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_R01 - eSUB_D01 PICT "@R 999,999,999.99"
      @ LIN,70  SAY eSUB_R02 - eSUB_D02 PICT "@R 999,999,999.99"
      @ LIN,85  SAY eSUB_R03 - eSUB_D03 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 2
      IF LIN > 45
         @ 58,00 SAY REPLICATE("=",134)
         @ 59,204 SAY sisTIT3
         @ 01,00 SAY " "+sisLETRAN
         @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
         @ 01,100 SAY "Data : "+dtoc(DATE())
         @ 02,00  SAY "Sistema : "+sisTIT2
         @ 02,100 SAY "Relat: SAN004"
         @ 03,02  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
         @ 04,00  SAY "1o. Mes do Trimestre - "+IIF(eTRIM = 1,"Jan.",IIF(eTRIM = 2,"Abr.",IIF(eTRIM = 3,"Jul.","Out.")))
         @ 04,40  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
         @ 05,00  SAY REPLICATE("=",134)
         @ 06,02  SAY "Codigo"
         @ 06,12  SAY "Descricao"
         @ 06,53  SAY "|"
         @ 06,54  SAY "1a. Dezena"
         @ 06,70  SAY "2a. Dezena"
         @ 06,85  SAY "3a. Dezena"
         @ 06,134 SAY "|"
         @ 07,00 SAY REPLICATE("=",134)
         LIN = 10
      ENDIF
      @ LIN,10  SAY sisLETRAE+"RESUMO FLUXO DE CAIXA : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",134)
      LIN = LIN + 1
      @ LIN,12 SAY "SALDO INICIAL      : "
      @ LIN,12 SAY "SALDO INICIAL      : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSALDO     PICT "@R 999,999,999.99"
      eSALDO_D01 = 0
      eSALDO_D01 = eSALDO + eSUB_R01 - eSUB_D01
      @ LIN,70  SAY eSALDO_D01 PICT "@R 999,999,999.99"
      eSALDO_D02 = 0
      eSALDO_D02 = eSALDO_D01 + eSUB_R02 - eSUB_D02
      @ LIN,85  SAY eSALDO_D02 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
      @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_R01 PICT "@R 999,999,999.99"
      @ LIN,70  SAY eSUB_R02 PICT "@R 999,999,999.99"
      @ LIN,85  SAY eSUB_R03 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
      @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_D01 PICT "@R 999,999,999.99"
      @ LIN,70  SAY eSUB_D02 PICT "@R 999,999,999.99"
      @ LIN,85  SAY eSUB_D03 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SALDO FINAL        : "
      @ LIN,12 SAY "SALDO FINAL        : "
      @ LIN,53  SAY "|"
      eSALDO_D01 = 0
      eSALDO_D01 = eSALDO + eSUB_R01 - eSUB_D01
      @ LIN,54  SAY eSALDO_D01 PICT "@R 999,999,999.99"
      eSALDO_D02 = 0
      eSALDO_D02 = eSALDO_D01 + eSUB_R02 - eSUB_D02
      @ LIN,70  SAY eSALDO_D02 PICT "@R 999,999,999.99"
      eSALDO_D03 = 0
      eSALDO_D03 = eSALDO_D02 + eSUB_R03 - eSUB_D03
      @ LIN,85  SAY eSALDO_D03 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",134)
   ENDIF
   @ 56,00 SAY REPLICATE("=",134)
   @ 57,85 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
eSALDO = eSALDO_D03
GOTO TOP
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,100 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,100 SAY "Relat: SAN004"
   @ 03,02  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 04,00  SAY "2o. Mes do Trimestre - "+IIF(eTRIM = 1,"Fev.",IIF(eTRIM = 2,"Mai.",IIF(eTRIM = 3,"Ago.","Nov.")))
   @ 04,40  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
   @ 05,00  SAY REPLICATE("=",134)
   @ 06,02  SAY "Codigo"
   @ 06,12  SAY "Descricao"
   @ 06,53  SAY "|"
   @ 06,54  SAY "1a. Dezena"
   @ 06,70  SAY "2a. Dezena"
   @ 06,85  SAY "3a. Dezena"
   @ 06,100 SAY "Totais"
   @ 06,120 SAY "Limites"
   @ 06,134 SAY "|"
   @ 07,00 SAY REPLICATE("=",134)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 55
      @ LIN,02  SAY CODIGO PICT "@R 9999999"
      @ LIN,12  SAY DESCRICAO
      @ LIN,53  SAY "|"
      @ LIN,54  SAY DEZENA04 PICT "@R 999,999,999.99"
      @ LIN,70  SAY DEZENA05 PICT "@R 999,999,999.99"
      @ LIN,85  SAY DEZENA06 PICT "@R 999,999,999.99"
      eTOTAL = 0
      eTOTAL = DEZENA04 + DEZENA05 + DEZENA06
      @ LIN,100 SAY eTOTAL   PICT "@R 999,999,999.99"
      SELE 3
      eCODIGO = CODIGO
      IF R_D = "D"
         SELE 4
         SEEK eCODIGO
         IF LIMITE < eTOTAL 
            @ LIN,120  SAY LIMITE  PICT "@R 999999999.99"
         ELSE
            @ LIN,120  SAY LIMITE  PICT "@R 999999999.99"
            @ LIN,120  SAY LIMITE  PICT "@R 999999999.99"
         ENDIF
         SELE 3
      ENDIF
      @ LIN,134 SAY "|"
      IF R_D = "R"
         eSUB_R04 = eSUB_R04 + DEZENA04 
         eSUB_R05 = eSUB_R05 + DEZENA05 
         eSUB_R06 = eSUB_R06 + DEZENA06 
      ELSE
         eSUB_D04 = eSUB_D04 + DEZENA04 
         eSUB_D05 = eSUB_D05 + DEZENA05 
         eSUB_D06 = eSUB_D06 + DEZENA06 
      ENDIF
      LIN = LIN + 1
      IF eR_D <> R_D
         @ LIN,00 SAY REPLICATE("-",134)
         LIN = LIN + 1
         IF eR_D = "R"
            @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
            @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
            @ LIN,53  SAY "|"
            @ LIN,54  SAY eSUB_R04 PICT "@R 999,999,999.99"
            @ LIN,70  SAY eSUB_R05 PICT "@R 999,999,999.99"
            @ LIN,85  SAY eSUB_R06 PICT "@R 999,999,999.99"
            @ LIN,134 SAY "|"
            LIN = LIN + 2
         ELSE
            @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
            @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
            @ LIN,53  SAY "|"
            @ LIN,54  SAY eSUB_D04 PICT "@R 999,999,999.99"
            @ LIN,70  SAY eSUB_D05 PICT "@R 999,999,999.99"
            @ LIN,85  SAY eSUB_D06 PICT "@R 999,999,999.99"
            @ LIN,134 SAY "|"
            LIN = LIN + 2
         ENDIF
      ENDIF   
      SELE 3
      SKIP
      eR_D = R_D
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
   ENDDO
   IF EOF()
      @ LIN,00 SAY REPLICATE("-",134)
      LIN = LIN + 1
      @ LIN,12 SAY "TOTAIS             : "
      @ LIN,12 SAY "TOTAIS             : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_R04 - eSUB_D04 PICT "@R 999,999,999.99"
      @ LIN,70  SAY eSUB_R05 - eSUB_D05 PICT "@R 999,999,999.99"
      @ LIN,85  SAY eSUB_R06 - eSUB_D06 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 2
      IF LIN > 45
         @ 58,00 SAY REPLICATE("=",134)
         @ 59,204 SAY sisTIT3
         @ 01,00 SAY " "+sisLETRAN
         @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
         @ 01,100 SAY "Data : "+dtoc(DATE())
         @ 02,00  SAY "Sistema : "+sisTIT2
         @ 02,100 SAY "Relat: SAN004"
         @ 03,02  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
         @ 04,00  SAY "2o. Mes do Trimestre - "+IIF(eTRIM = 1,"Fev.",IIF(eTRIM = 2,"Mai.",IIF(eTRIM = 3,"Ago.","Nov.")))
         @ 04,40  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
         @ 05,00  SAY REPLICATE("=",134)
         @ 06,02  SAY "Codigo"
         @ 06,12  SAY "Descricao"
         @ 06,53  SAY "|"
         @ 06,54  SAY "1a. Dezena"
         @ 06,70  SAY "2a. Dezena"
         @ 06,85  SAY "3a. Dezena"
         @ 06,134 SAY "|"
         @ 07,00 SAY REPLICATE("=",134)
         LIN = 10
      ENDIF
      @ LIN,10  SAY sisLETRAE+"RESUMO FLUXO DE CAIXA : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",134)
      LIN = LIN + 1
      @ LIN,12 SAY "SALDO INICIAL      : "
      @ LIN,12 SAY "SALDO INICIAL      : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSALDO     PICT "@R 999,999,999.99"
      eSALDO_D04 = 0
      eSALDO_D04 = eSALDO + eSUB_R04 - eSUB_D04
      @ LIN,70  SAY eSALDO_D04 PICT "@R 999,999,999.99"
      eSALDO_D05 = 0
      eSALDO_D05 = eSALDO_D04 + eSUB_R05 - eSUB_D05
      @ LIN,85  SAY eSALDO_D05 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
      @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_R04 PICT "@R 999,999,999.99"
      @ LIN,70  SAY eSUB_R05 PICT "@R 999,999,999.99"
      @ LIN,85  SAY eSUB_R06 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
      @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_D04 PICT "@R 999,999,999.99"
      @ LIN,70  SAY eSUB_D05 PICT "@R 999,999,999.99"
      @ LIN,85  SAY eSUB_D06 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SALDO FINAL        : "
      @ LIN,12 SAY "SALDO FINAL        : "
      @ LIN,53  SAY "|"
      eSALDO_D04 = 0
      eSALDO_D04 = eSALDO + eSUB_R04 - eSUB_D04
      @ LIN,54  SAY eSALDO_D04 PICT "@R 999,999,999.99"
      eSALDO_D05 = 0
      eSALDO_D05 = eSALDO_D04 + eSUB_R05 - eSUB_D05
      @ LIN,70  SAY eSALDO_D05 PICT "@R 999,999,999.99"
      eSALDO_D06 = 0
      eSALDO_D06 = eSALDO_D05 + eSUB_R06 - eSUB_D06
      @ LIN,85  SAY eSALDO_D06 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",134)
   ENDIF
   @ 58,00 SAY REPLICATE("=",134)
   @ 59,204 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
eSALDO = eSALDO_D06
GOTO TOP
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,100 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,100 SAY "Relat: SAN004"
   @ 03,02  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 04,00  SAY "3o. Mes do Trimestre - "+IIF(eTRIM = 1,"Mar.",IIF(eTRIM = 2,"Jun.",IIF(eTRIM = 3,"Set.","Dez.")))
   @ 04,40  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
   @ 05,00  SAY REPLICATE("=",134)
   @ 06,02  SAY "Codigo"
   @ 06,12  SAY "Descricao"
   @ 06,53  SAY "|"
   @ 06,54  SAY "1a. Dezena"
   @ 06,70  SAY "2a. Dezena"
   @ 06,85  SAY "3a. Dezena"
   @ 06,100 SAY "Totais"
   @ 06,120 SAY "Limites"
   @ 06,134 SAY "|"
   @ 07,00 SAY REPLICATE("=",134)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 55
      @ LIN,02  SAY CODIGO PICT "@R 9999999"
      @ LIN,12  SAY DESCRICAO
      @ LIN,53  SAY "|"
      @ LIN,54  SAY DEZENA07 PICT "@R 999,999,999.99"
      @ LIN,70  SAY DEZENA08 PICT "@R 999,999,999.99"
      @ LIN,85  SAY DEZENA09 PICT "@R 999,999,999.99"
      eTOTAL = 0
      eTOTAL = DEZENA07 + DEZENA08 + DEZENA09 
      @ LIN,100 SAY eTOTAL   PICT "@R 999,999,999.99"
      SELE 3
      eCODIGO = CODIGO
      IF R_D = "D"
         SELE 4
         SEEK eCODIGO
         IF LIMITE < eTOTAL 
            @ LIN,120  SAY LIMITE  PICT "@R 999999999.99"
         ELSE
            @ LIN,120  SAY LIMITE  PICT "@R 999999999.99"
            @ LIN,120  SAY LIMITE  PICT "@R 999999999.99"
         ENDIF
         SELE 3
      ENDIF
      @ LIN,134 SAY "|"
      IF R_D = "R"
         eSUB_R07 = eSUB_R07 + DEZENA07 
         eSUB_R08 = eSUB_R08 + DEZENA08 
         eSUB_R09 = eSUB_R09 + DEZENA09 
      ELSE
         eSUB_D07 = eSUB_D07 + DEZENA07 
         eSUB_D08 = eSUB_D08 + DEZENA08 
         eSUB_D09 = eSUB_D09 + DEZENA09 
      ENDIF
      LIN = LIN + 1
      IF eR_D <> R_D
         @ LIN,00 SAY REPLICATE("-",134)
         LIN = LIN + 1
         IF eR_D = "R"
            @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
            @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
            @ LIN,53  SAY "|"
            @ LIN,54  SAY eSUB_R07 PICT "@R 999,999,999.99"
            @ LIN,70  SAY eSUB_R08 PICT "@R 999,999,999.99"
            @ LIN,85  SAY eSUB_R09 PICT "@R 999,999,999.99"
            @ LIN,134 SAY "|"
            LIN = LIN + 2
         ELSE
            @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
            @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
            @ LIN,53  SAY "|"
            @ LIN,54  SAY eSUB_D07 PICT "@R 999,999,999.99"
            @ LIN,70  SAY eSUB_D08 PICT "@R 999,999,999.99"
            @ LIN,85  SAY eSUB_D09 PICT "@R 999,999,999.99"
            @ LIN,134 SAY "|"
            LIN = LIN + 2
         ENDIF
      ENDIF   
      SELE 3
      SKIP
      eR_D = R_D
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
   ENDDO
   IF EOF()
      @ LIN,00 SAY REPLICATE("-",134)
      LIN = LIN + 1
      @ LIN,12 SAY "TOTAIS             : "
      @ LIN,12 SAY "TOTAIS             : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_R07 - eSUB_D07 PICT "@R 999,999,999.99"
      @ LIN,70  SAY eSUB_R08 - eSUB_D08 PICT "@R 999,999,999.99"
      @ LIN,85  SAY eSUB_R09 - eSUB_D09 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 2
      IF LIN > 45
         @ 58,00 SAY REPLICATE("=",134)
         @ 59,204 SAY sisTIT3
         @ 01,00 SAY " "+sisLETRAN
         @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
         @ 01,100 SAY "Data : "+dtoc(DATE())
         @ 02,00  SAY "Sistema : "+sisTIT2
         @ 02,100 SAY "Relat: SAN004"
         @ 03,02  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
         @ 04,00  SAY "3o. Mes do Trimestre - "+IIF(eTRIM = 1,"Mar.",IIF(eTRIM = 2,"Jun.",IIF(eTRIM = 3,"Set.","Dez.")))
         @ 04,40  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
         @ 05,00  SAY REPLICATE("=",134)
         @ 06,02  SAY "Codigo"
         @ 06,12  SAY "Descricao"
         @ 06,53  SAY "|"
         @ 06,54  SAY "1a. Dezena"
         @ 06,70  SAY "2a. Dezena"
         @ 06,85  SAY "3a. Dezena"
         @ 06,134 SAY "|"
         @ 07,00 SAY REPLICATE("=",134)
         LIN = 10
      ENDIF
      @ LIN,10  SAY sisLETRAE+"RESUMO FLUXO DE CAIXA : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",134)
      LIN = LIN + 1
      @ LIN,12 SAY "SALDO INICIAL      : "
      @ LIN,12 SAY "SALDO INICIAL      : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSALDO     PICT "@R 999,999,999.99"
      eSALDO_D07 = 0
      eSALDO_D07 = eSALDO + eSUB_R07 - eSUB_D07
      @ LIN,70  SAY eSALDO_D07 PICT "@R 999,999,999.99"
      eSALDO_D08 = 0
      eSALDO_D08 = eSALDO_D07 + eSUB_R08 - eSUB_D08
      @ LIN,85  SAY eSALDO_D08 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
      @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_R07 PICT "@R 999,999,999.99"
      @ LIN,70  SAY eSUB_R08 PICT "@R 999,999,999.99"
      @ LIN,85  SAY eSUB_R09 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
      @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_D07 PICT "@R 999,999,999.99"
      @ LIN,70  SAY eSUB_D08 PICT "@R 999,999,999.99"
      @ LIN,85  SAY eSUB_D09 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SALDO FINAL        : "
      @ LIN,12 SAY "SALDO FINAL        : "
      @ LIN,53  SAY "|"
      eSALDO_D07 = 0
      eSALDO_D07 = eSALDO + eSUB_R07 - eSUB_D07
      @ LIN,54  SAY eSALDO_D07 PICT "@R 999,999,999.99"
      eSALDO_D08 = 0
      eSALDO_D08 = eSALDO_D07 + eSUB_R08 - eSUB_D08
      @ LIN,70  SAY eSALDO_D08 PICT "@R 999,999,999.99"
      eSALDO_D09 = 0
      eSALDO_D09 = eSALDO_D08 + eSUB_R09 - eSUB_D09
      @ LIN,85  SAY eSALDO_D09 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",134)
   ENDIF
   @ 58,00 SAY REPLICATE("=",134)
   @ 59,207 SAY sisTIT3
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
CLOSE DATABASES
RESTORE SCREEN FROM TELA06
RETURN nil
*************************  fim  *********************************
