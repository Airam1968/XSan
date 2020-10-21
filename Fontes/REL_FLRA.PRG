*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_FLRA.PRG
* Data        : 30/10/2001
* Objetivo    : RELATORIO DE FLUXO DE CAIXA TOTAL - RATEIO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_FLRA()
SAVE SCREEN TO TELA06
PTIT( "RELATàRIO DE FLUXO DE CAIXA TOTAL - RATEIO")
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
SELE 6
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
eTOT_PERCD  = 0
eTOT_PERCR  = 0
PLIMPA()
DO WHILE .T.
   @ 07,02 SAY "Periodo : " GET eDATA_INI
   @ 07,25 SAY "Ate : "     GET eDATA_FIM
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
eSALDO = 0
PLQUA("01")
eORIGEM = SPACE(1)
PLQUA("01")
@ 23,00 SAY " Relatorio  por (P)roduto ou (C)lasses  ? " GET eORIGEM PICT "@!X" VALID eORIGEM $("PC")
READ
eNOME_EMPRE = SPACE(50)
SELE 6
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
DO WHILE .T.
   eCLAS_PRO = 0
   PLQUA("01")
   @ 23,00 SAY " Produto  ou  (0)  para  Todos  :  " GET eCLAS_PRO PICT "@R 9999999"
   READ
   SELE 6
   SEEK eCLAS_PRO
   IF .NOT. FOUND()
      IF eCLAS_PRO <> 0
         PLQUA("01")
		 PMSGR( "  Produto	Inexistente  ",3)
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
IF eCLAS_PRO = 0
   PLQUA("01")
   PPERG ( " Deseja  que  o  Produto  (0)  seja  emitido (S/N) ? ")
   IF sisRESP = "S"
      eESCRITORIO = .T.
   ELSE
      eESCRITORIO = .F.
   ENDIF
ELSE
   eESCRITORIO = .T.
ENDIF
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

PMENSAG("Aguarde a Prepara‡Æo do Arquivo, de "+DTOC(eDATA_INI)+" at‡ "+DTOC(eDATA_FIM)+"... ")
IF eCLAS_PRO = 0
   IF eESCRITORIO
      SELE 1
      SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. CONTABIL
      GOTO TOP
   ELSE
      SELE 1
      SET FILTER TO EMPREENDIM <> 0 .AND. DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. CONTABIL
      GOTO TOP
   ENDIF
ELSE
   SELE 1
   SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. CONTABIL
   GOTO TOP
ENDIF
DO WHILE .NOT. EOF()
   eDATA_ATU  = CTOD("")
   eDATA_ATU  = DATA_REAL
   eDIA       = VAL(SUBSTR(DTOC(eDATA_ATU),1,2))
   eMES       = VAL(SUBSTR(DTOC(eDATA_ATU),4,2))   
   IF eORIGEM = "P"
      eCODIGO    = EMPREENDIM
      SELE 6
      SEEK eCODIGO
      eDESCR_R = DESCRICAO
   ELSE
      eCODIGO    = CLASSE_R
      SELE 5
      SEEK eCODIGO
      eDESCR_R = DESCRICAO
   ENDIF
   SELE 1
   eDESCRICAO  = DESCRICAO
   eREALIZADO  = REALIZADO
   eDEZENA = 1
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
      REPLACE DEZENA01 WITH DEZENA01 + eREALIZADO,;
              R_D      WITH "R"
      eTOT_PERCR = eTOT_PERCR + eREALIZADO
      UNLOCK
   ENDIF
   SELE 1
   SKIP
ENDDO
IF eCLAS_PRO = 0
   IF eESCRITORIO
      SELE 2
      SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. CONTABIL
      GOTO TOP
   ELSE
      SELE 2
      SET FILTER TO EMPREENDIM <> 0 .AND. DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. CONTABIL
      GOTO TOP
   ENDIF
ELSE
   SELE 2
   SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. CONTABIL
   GOTO TOP
ENDIF
DO WHILE .NOT. EOF()
   eDATA_ATU  = CTOD("")
   eDATA_ATU  = DATA_REAL
   eDIA       = VAL(SUBSTR(DTOC(eDATA_ATU),1,2))
   eMES       = VAL(SUBSTR(DTOC(eDATA_ATU),4,2))   
   IF eORIGEM = "P"
      eCODIGO    = EMPREENDIM
      SELE 6
      SEEK eCODIGO
      eDESCR_D = DESCRICAO
   ELSE
      eCODIGO    = CLASSE_D
      SELE 4
      SEEK eCODIGO
      eDESCR_D = DESCRICAO
   ENDIF
   SELE 2
   eDESCRICAO  = DESCRICAO
   eREALIZADO  = REALIZADO
   eDEZENA = 1
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
      REPLACE DEZENA01 WITH DEZENA01 + eREALIZADO,;
              R_D      WITH "D"
      eTOT_PERCD = eTOT_PERCD + eREALIZADO
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
SET EXCLUSIVE ON
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
   @ 03,30  SAY sisLETRAE+"FLUXO DE CAIXA - RATEIO DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 04,00  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
   @ 05,00  SAY REPLICATE("=",134)
   @ 06,02  SAY "Codigo"
   @ 06,12  SAY "Descricao"
   @ 06,53  SAY "|"
   @ 06,56  SAY "Totais"
   @ 06,75  SAY "% Percentual"
   @ 06,134 SAY "|"
   @ 07,00 SAY REPLICATE("=",134)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      @ LIN,02  SAY CODIGO PICT "@R 9999999"
      @ LIN,12  SAY SUBSTR(DESCRICAO,1,40)
      @ LIN,53  SAY "|"
      @ LIN,54  SAY DEZENA01 PICT "@R 999,999,999.99"
      IF R_D = "R"
         @ LIN,80  SAY ((DEZENA01/eTOT_PERCR)*100) PICT "@R 999.99"
      ELSE
         @ LIN,80  SAY ((DEZENA01/eTOT_PERCD)*100) PICT "@R 999.99"
      ENDIF
      eTOTAL = 0
      eTOTAL = DEZENA01
      @ LIN,134 SAY "|"
      IF R_D = "R"
         eSUB_R01 = eSUB_R01 + DEZENA01 
      ELSE
         eSUB_D01 = eSUB_D01 + DEZENA01 
      ENDIF
      LIN = LIN + 1
      SKIP
      IF eR_D <> R_D
         @ LIN,00 SAY REPLICATE("-",134)
         LIN = LIN + 1
         IF eR_D = "R"
            @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
            @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
            @ LIN,53  SAY "|"
            @ LIN,54  SAY eSUB_R01 PICT "@R 999,999,999.99"
            @ LIN,134 SAY "|"
            LIN = LIN + 2
         ELSE
            @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
            @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
            @ LIN,53  SAY "|"
            @ LIN,54  SAY eSUB_D01 PICT "@R 999,999,999.99"
            @ LIN,134 SAY "|"
            LIN = LIN + 2
         ENDIF
         eR_D = R_D
      ENDIF   
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
         @ 03,30  SAY sisLETRAE+"FLUXO DE CAIXA - RATEIO DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
         @ 04,00  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
         @ 05,00  SAY REPLICATE("=",134)
         @ 06,02  SAY "Codigo"
         @ 06,12  SAY "Descricao"
         @ 06,53  SAY "|"
         @ 06,56  SAY "Totais"
         @ 06,75  SAY "% Percentual"
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
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
      @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_R01 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
      @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
      @ LIN,53  SAY "|"
      @ LIN,54  SAY eSUB_D01 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SALDO FINAL        : "
      @ LIN,12 SAY "SALDO FINAL        : "
      @ LIN,53  SAY "|"
      eSALDO_D01 = 0
      eSALDO_D01 = eSALDO + eSUB_R01 - eSUB_D01
      @ LIN,54  SAY eSALDO_D01 PICT "@R 999,999,999.99"
      @ LIN,134 SAY "|"
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",134)
   ENDIF
   @ 56,00 SAY REPLICATE("=",134)
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
CLOSE DATABASES
RESTORE SCREEN FROM TELA06
RETURN nil
*************************  fim  *********************************
