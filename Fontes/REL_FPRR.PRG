*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_FPRR.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE FLUXO DE CAIXA - REALIZADO   
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_FPRR()
SAVE SCREEN TO TELA06
PTIT( "RELAT‡RIO DE FLUXO DE CAIXA - REALIZADO"
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "N"
   RESTORE SCREEN FROM TELA06
   RETURN nil
ENDIF
SELE 1
USE CRECEBER INDEX DATA_CRE,CRECEBER
GOTO TOP
SELE 2
USE CPAGAR INDEX DATA_CPG,CPAGAR
GOTO TOP
SELE 3
SET EXCLUSIVE ON
USE FLUXO_C
DELE ALL
PACK
SET EXCLUSIVE OFF
SELE 4
USE FORNECED INDEX FORNECED
GOTO TOP
SELE 5
USE CLIENTES INDEX CLIENTES
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
  CASE sisOP = 2
     eDATA_INI = "01/04/"
     eDATA_FIM = "30/06/"
  CASE sisOP = 3
     eDATA_INI = "01/07/"
     eDATA_FIM = "30/09/"
  CASE sisOP = 4
     eDATA_INI = "01/10/"
     eDATA_FIM = "31/12/"
ENDCASE
SET COLOR TO W/N,+W/BG
@ 09,59 CLEAR TO 18,67
@ 09,59 TO 18,67
@ 10,61 PROMPT " 1994 " 
@ 11,61 PROMPT " 1995 " 
@ 12,61 PROMPT " 1996 " 
@ 13,61 PROMPT " 1997 " 
@ 14,61 PROMPT " 1998 " 
@ 15,61 PROMPT " 1999 " 
@ 16,61 PROMPT " 2000 " 
@ 17,61 PROMPT " 2001 " 
READ
sisOP=1
MENU TO sisOP
DO CASE
  CASE sisOP = 1
     eANO = "1994"
  CASE sisOP = 2
     eANO = "1995"     
  CASE sisOP = 3
     eANO = "1996"     
  CASE sisOP = 4
     eANO = "1997"
  CASE sisOP = 5
     eANO = "1998"
  CASE sisOP = 6
     eANO = "1999"     
  CASE sisOP = 7
     eANO = "2000"     
  CASE sisOP = 8
     eANO = "2001"
ENDCASE
DO WHILE .T.
   PLQUA("01")
   eSALDO = 0
   @ 23,00 SAY "Informe o Saldo Inicial : " GET eSALDO PICT "@R 999,999,999.99"
   READ
   IF eSALDO <> 0 
      EXIT
   ENDIF
ENDDO
eDATA_INI   = ALLTRIM(eDATA_INI)+ALLTRIM(eANO)
eDATA_FIM   = ALLTRIM(eDATA_FIM)+ALLTRIM(eANO)
eDATA_INI   = CTOD(eDATA_INI)
eDATA_FIM   = CTOD(eDATA_FIM)
PMENSAG("Aguarde a Preparaá∆o do Arquivo, de "+DTOC(eDATA_INI)+" atá "+DTOC(eDATA_FIM)+"... "
SELE 1
SET FILTER TO (DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM) 
GOTO TOP
DO WHILE .NOT. EOF()
   eDATA_ATU  = CTOD("")
   eDATA_ATU  = DATA_PREV
   eDIA       = VAL(SUBSTR(DTOC(eDATA_ATU),1,2))
   eMES       = VAL(SUBSTR(DTOC(eDATA_ATU),4,2))   
   eCODIGO    = CODIGO_CLI
   SELE 5
   SEEK eCODIGO
   eDESCR_CLI = NOME
   SELE 1
   eDESCRICAO = DESCRICAO
   ePREVISTO  = REALIZADO
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
   PMENSAG("Pesquisando : "+ALLTRIM(eDESCRICAO)
   IF eCODIGO = 1
      LOCATE FOR CODIGO = eCODIGO .AND. DESCRICAO = eDESCRICAO .AND. R_D = "R"
   ELSE
      LOCATE FOR CODIGO = eCODIGO .AND. DESCRICAO = eDESCR_CLI .AND. R_D = "R"
   ENDIF
   IF EOF()
      APPEND BLANK
      IF F_BLOQR()
         REPLACE CODIGO    WITH eCODIGO
         IF eCODIGO = 1
            REPLACE DESCRICAO WITH eDESCRICAO
         ELSE
            REPLACE DESCRICAO WITH eDESCR_CLI
         ENDIF
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
SELE 2
SET FILTER TO (DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM) 
GOTO TOP
DO WHILE .NOT. EOF()
   eDATA_ATU  = CTOD("")
   eDATA_ATU  = DATA
   eDIA       = VAL(SUBSTR(DTOC(eDATA_ATU),1,2))
   eMES       = VAL(SUBSTR(DTOC(eDATA_ATU),4,2))   
   eCODIGO    = CODIGO_FOR
   SELE 4
   SEEK eCODIGO
   eDESCR_FOR = NOME
   SELE 2
   eDESCRICAO = DESCRICAO
   ePREVISTO  = REALIZADO
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
   PMENSAG("Pesquisando : "+ALLTRIM(eDESCRICAO)
   LOCATE FOR CODIGO = eCODIGO .AND. R_D = "D"
   IF EOF()
      APPEND BLANK
      IF F_BLOQR()
         REPLACE CODIGO    WITH eCODIGO,;
                 DESCRICAO WITH eDESCR_FOR
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
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"EXTRATO.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "EXTRATO.TXT"+SPACE(10) 
ENDIF    
DO PIMPR
SET DEVICE TO PRINT
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
   @ 01,210 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,210 SAY "Relat: SAN004"
   @ 03,60  SAY sisLETRAE+"REALIZADO DE FLUXO DE CAIXA : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 05,00  SAY REPLICATE("=",225)
   @ 06,02  SAY "Codigo"
   @ 06,12  SAY "Descricao"
   @ 06,64  SAY "|"
   @ 06,65  SAY "1a. Dezena"
   @ 06,80  SAY "2a. Dezena"
   @ 06,95  SAY "3a. Dezena"
   @ 06,110 SAY "|"
   @ 06,115 SAY "1a. Dezena"
   @ 06,130 SAY "2a. Dezena"
   @ 06,145 SAY "3a. Dezena"
   @ 06,160 SAY "|"
   @ 06,165 SAY "1a. Dezena"
   @ 06,180 SAY "2a. Dezena"
   @ 06,195 SAY "3a. Dezena"
   @ 06,210 SAY "|"
   @ 07,00 SAY REPLICATE("=",225)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 55
      @ LIN,02  SAY CODIGO PICT "@R 99999"
      @ LIN,12  SAY DESCRICAO
      @ LIN,64  SAY "|"
      @ LIN,65  SAY DEZENA01 PICT "@R 999,999,999.99"
      @ LIN,80  SAY DEZENA02 PICT "@R 999,999,999.99"
      @ LIN,95  SAY DEZENA03 PICT "@R 999,999,999.99"
      @ LIN,110 SAY "|"
      @ LIN,115 SAY DEZENA04 PICT "@R 999,999,999.99"
      @ LIN,130 SAY DEZENA05 PICT "@R 999,999,999.99"
      @ LIN,145 SAY DEZENA06 PICT "@R 999,999,999.99"
      @ LIN,160 SAY "|"
      @ LIN,165 SAY DEZENA07 PICT "@R 999,999,999.99"
      @ LIN,180 SAY DEZENA08 PICT "@R 999,999,999.99"
      @ LIN,195 SAY DEZENA09 PICT "@R 999,999,999.99"
      @ LIN,210 SAY "|"
      IF R_D = "R"
         eSUB_R01 = eSUB_R01 + DEZENA01 
         eSUB_R02 = eSUB_R02 + DEZENA02 
         eSUB_R03 = eSUB_R03 + DEZENA03 
         eSUB_R04 = eSUB_R04 + DEZENA04 
         eSUB_R05 = eSUB_R05 + DEZENA05 
         eSUB_R06 = eSUB_R06 + DEZENA06 
         eSUB_R07 = eSUB_R07 + DEZENA07 
         eSUB_R08 = eSUB_R08 + DEZENA08 
         eSUB_R09 = eSUB_R09 + DEZENA09 
      ELSE
         eSUB_D01 = eSUB_D01 + DEZENA01 
         eSUB_D02 = eSUB_D02 + DEZENA02 
         eSUB_D03 = eSUB_D03 + DEZENA03 
         eSUB_D04 = eSUB_D04 + DEZENA04 
         eSUB_D05 = eSUB_D05 + DEZENA05 
         eSUB_D06 = eSUB_D06 + DEZENA06 
         eSUB_D07 = eSUB_D07 + DEZENA07 
         eSUB_D08 = eSUB_D08 + DEZENA08 
         eSUB_D09 = eSUB_D09 + DEZENA09 
      ENDIF
      LIN = LIN + 1
      SKIP
      IF eR_D <> R_D
         @ LIN,00 SAY REPLICATE("-",225)
         LIN = LIN + 1
         IF eR_D = "R"
            @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
            @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
            @ LIN,64  SAY "|"
            @ LIN,65  SAY eSUB_R01 PICT "@R 999,999,999.99"
            @ LIN,80  SAY eSUB_R02 PICT "@R 999,999,999.99"
            @ LIN,95  SAY eSUB_R03 PICT "@R 999,999,999.99"
            @ LIN,110 SAY "|"
            @ LIN,115 SAY eSUB_R04 PICT "@R 999,999,999.99"
            @ LIN,130 SAY eSUB_R05 PICT "@R 999,999,999.99"
            @ LIN,145 SAY eSUB_R06 PICT "@R 999,999,999.99"
            @ LIN,160 SAY "|"
            @ LIN,165 SAY eSUB_R07 PICT "@R 999,999,999.99"
            @ LIN,180 SAY eSUB_R08 PICT "@R 999,999,999.99"
            @ LIN,195 SAY eSUB_R09 PICT "@R 999,999,999.99"
            @ LIN,210 SAY "|"
            LIN = LIN + 2
         ELSE
            @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
            @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
            @ LIN,64  SAY "|"
            @ LIN,65  SAY eSUB_D01 PICT "@R 999,999,999.99"
            @ LIN,80  SAY eSUB_D02 PICT "@R 999,999,999.99"
            @ LIN,95  SAY eSUB_D03 PICT "@R 999,999,999.99"
            @ LIN,110 SAY "|"
            @ LIN,115 SAY eSUB_D04 PICT "@R 999,999,999.99"
            @ LIN,130 SAY eSUB_D05 PICT "@R 999,999,999.99"
            @ LIN,145 SAY eSUB_D06 PICT "@R 999,999,999.99"
            @ LIN,160 SAY "|"
            @ LIN,165 SAY eSUB_D07 PICT "@R 999,999,999.99"
            @ LIN,180 SAY eSUB_D08 PICT "@R 999,999,999.99"
            @ LIN,195 SAY eSUB_D09 PICT "@R 999,999,999.99"
            @ LIN,210 SAY "|"
            LIN = LIN + 2
         ENDIF
         eR_D = R_D
      ENDIF   
      DO PTECLA
      IF sisINTERR
         SET DEVICE TO SCREEN
         CLOSE ALL
         RETURN nil
      ENDIF
   ENDDO
   IF EOF()
      @ LIN,00 SAY REPLICATE("-",225)
      LIN = LIN + 1
      @ LIN,12 SAY "TOTAIS             : "
      @ LIN,12 SAY "TOTAIS             : "
      @ LIN,64  SAY "|"
      @ LIN,65  SAY eSUB_R01 - eSUB_D01 PICT "@R 999,999,999.99"
      @ LIN,80  SAY eSUB_R02 - eSUB_D02 PICT "@R 999,999,999.99"
      @ LIN,95  SAY eSUB_R03 - eSUB_D03 PICT "@R 999,999,999.99"
      @ LIN,110 SAY "|"
      @ LIN,115 SAY eSUB_R04 - eSUB_D04 PICT "@R 999,999,999.99"
      @ LIN,130 SAY eSUB_R05 - eSUB_D05 PICT "@R 999,999,999.99"
      @ LIN,145 SAY eSUB_R06 - eSUB_D06 PICT "@R 999,999,999.99"
      @ LIN,160 SAY "|"
      @ LIN,165 SAY eSUB_R07 - eSUB_D07 PICT "@R 999,999,999.99"
      @ LIN,180 SAY eSUB_R08 - eSUB_D08 PICT "@R 999,999,999.99"
      @ LIN,195 SAY eSUB_R09 - eSUB_D09 PICT "@R 999,999,999.99"
      @ LIN,210 SAY "|"
      LIN = LIN + 2
      @ LIN,60  SAY sisLETRAE+"RESUMO FLUXO DE CAIXA : "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",225)
      LIN = LIN + 1
      @ LIN,12 SAY "SALDO INICIAL      : "
      @ LIN,12 SAY "SALDO INICIAL      : "
      @ LIN,64  SAY "|"
      @ LIN,65  SAY eSALDO     PICT "@R 999,999,999.99"
      eSALDO_D01 = 0
      eSALDO_D01 = eSALDO + eSUB_R01 - eSUB_D01
      @ LIN,80  SAY eSALDO_D01 PICT "@R 999,999,999.99"
      eSALDO_D02 = 0
      eSALDO_D02 = eSALDO_D01 + eSUB_R02 - eSUB_D02
      @ LIN,95  SAY eSALDO_D02 PICT "@R 999,999,999.99"
      @ LIN,110 SAY "|"
      eSALDO_D03 = 0
      eSALDO_D03 = eSALDO_D02 + eSUB_R03 - eSUB_D03
      @ LIN,115 SAY eSALDO_D03 PICT "@R 999,999,999.99"
      eSALDO_D04 = 0
      eSALDO_D04 = eSALDO_D03 + eSUB_R04 - eSUB_D04
      @ LIN,130 SAY eSALDO_D04 PICT "@R 999,999,999.99"
      eSALDO_D05 = 0
      eSALDO_D05 = eSALDO_D04 + eSUB_R05 - eSUB_D05
      @ LIN,145 SAY eSALDO_D05 PICT "@R 999,999,999.99"
      @ LIN,160 SAY "|"
      eSALDO_D06 = 0
      eSALDO_D06 = eSALDO_D05 + eSUB_R06 - eSUB_D06
      @ LIN,165 SAY eSALDO_D06 PICT "@R 999,999,999.99"
      eSALDO_D07 = 0
      eSALDO_D07 = eSALDO_D06 + eSUB_R07 - eSUB_D07
      @ LIN,180 SAY eSALDO_D07 PICT "@R 999,999,999.99"
      eSALDO_D08 = 0
      eSALDO_D08 = eSALDO_D07 + eSUB_R08 - eSUB_D08
      @ LIN,195 SAY eSALDO_D08 PICT "@R 999,999,999.99"
      @ LIN,210 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
      @ LIN,12 SAY "SUB-TOTAL ENTRADAS : "
      @ LIN,64  SAY "|"
      @ LIN,65  SAY eSUB_R01 PICT "@R 999,999,999.99"
      @ LIN,80  SAY eSUB_R02 PICT "@R 999,999,999.99"
      @ LIN,95  SAY eSUB_R03 PICT "@R 999,999,999.99"
      @ LIN,110 SAY "|"
      @ LIN,115 SAY eSUB_R04 PICT "@R 999,999,999.99"
      @ LIN,130 SAY eSUB_R05 PICT "@R 999,999,999.99"
      @ LIN,145 SAY eSUB_R06 PICT "@R 999,999,999.99"
      @ LIN,160 SAY "|"
      @ LIN,165 SAY eSUB_R07 PICT "@R 999,999,999.99"
      @ LIN,180 SAY eSUB_R08 PICT "@R 999,999,999.99"
      @ LIN,195 SAY eSUB_R09 PICT "@R 999,999,999.99"
      @ LIN,210 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
      @ LIN,12 SAY "SUB-TOTAL SAIDAS   : "
      @ LIN,64  SAY "|"
      @ LIN,65  SAY eSUB_D01 PICT "@R 999,999,999.99"
      @ LIN,80  SAY eSUB_D02 PICT "@R 999,999,999.99"
      @ LIN,95  SAY eSUB_D03 PICT "@R 999,999,999.99"
      @ LIN,110 SAY "|"
      @ LIN,115 SAY eSUB_D04 PICT "@R 999,999,999.99"
      @ LIN,130 SAY eSUB_D05 PICT "@R 999,999,999.99"
      @ LIN,145 SAY eSUB_D06 PICT "@R 999,999,999.99"
      @ LIN,160 SAY "|"
      @ LIN,165 SAY eSUB_D07 PICT "@R 999,999,999.99"
      @ LIN,180 SAY eSUB_D08 PICT "@R 999,999,999.99"
      @ LIN,195 SAY eSUB_D09 PICT "@R 999,999,999.99"
      @ LIN,210 SAY "|"
      LIN = LIN + 1
      @ LIN,12 SAY "SALDO FINAL        : "
      @ LIN,12 SAY "SALDO FINAL        : "
      @ LIN,64  SAY "|"
      eSALDO_D01 = 0
      eSALDO_D01 = eSALDO + eSUB_R01 - eSUB_D01
      @ LIN,65  SAY eSALDO_D01 PICT "@R 999,999,999.99"
      eSALDO_D02 = 0
      eSALDO_D02 = eSALDO_D01 + eSUB_R02 - eSUB_D02
      @ LIN,80  SAY eSALDO_D02 PICT "@R 999,999,999.99"
      eSALDO_D03 = 0
      eSALDO_D03 = eSALDO_D02 + eSUB_R03 - eSUB_D03
      @ LIN,95  SAY eSALDO_D03 PICT "@R 999,999,999.99"
      @ LIN,110 SAY "|"
      eSALDO_D04 = 0
      eSALDO_D04 = eSALDO_D03 + eSUB_R04 - eSUB_D04
      @ LIN,115 SAY eSALDO_D04 PICT "@R 999,999,999.99"
      eSALDO_D05 = 0
      eSALDO_D05 = eSALDO_D04 + eSUB_R05 - eSUB_D05
      @ LIN,130 SAY eSALDO_D05 PICT "@R 999,999,999.99"
      eSALDO_D06 = 0
      eSALDO_D06 = eSALDO_D05 + eSUB_R06 - eSUB_D06
      @ LIN,145 SAY eSALDO_D06 PICT "@R 999,999,999.99"
      @ LIN,160 SAY "|"
      eSALDO_D07 = 0
      eSALDO_D07 = eSALDO_D06 + eSUB_R07 - eSUB_D07
      @ LIN,165 SAY eSALDO_D07 PICT "@R 999,999,999.99"
      eSALDO_D08 = 0
      eSALDO_D08 = eSALDO_D07 + eSUB_R08 - eSUB_D08
      @ LIN,180 SAY eSALDO_D08 PICT "@R 999,999,999.99"
      eSALDO_D09 = 0
      eSALDO_D09 = eSALDO_D08 + eSUB_R09 - eSUB_D09
      @ LIN,195 SAY eSALDO_D09 PICT "@R 999,999,999.99"
      @ LIN,210 SAY "|"
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",225)
   ENDIF
   @ 58,00 SAY REPLICATE("=",225)
   @ 59,204 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA06
RETURN nil
*************************  fim  *********************************
