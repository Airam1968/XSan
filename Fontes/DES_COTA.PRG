*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : DES_COTA.PRG
* Data        : 01/01/99
* Objetivo    : DESMEMBRAMENTO DE COTACOES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION DES_COTA()
SAVE SCREEN TO TELA03
PTIT( "DESMEMBRAMENTO DE COTACÂES")
SELE 1
USE COTACAO INDEX COTACAO
GOTO TOP
SELE 2
USE ARQ_TIPO INDEX ARQ_TIPO
GOTO TOP
SELE 3
USE ARQ_ITEM INDEX ARQ_ITEM,ITEM_DES
GOTO TOP
SELE 4
USE FORNECED INDEX FORNECED
GOTO TOP
DO WHILE .T.
   eCOTACAO = 0
   SELE 1
   PLIMPA()
   SET COLOR TO W/N,+W/R
   @ 05,02 SAY "Cotaá∆o     : " GET eCOTACAO PICT "@R 999,999"
   READ
   IF eCOTACAO = 0
      CLOSE DATABASES
      RESTORE SCREEN FROM TELA03
      RETURN nil
   ENDIF
   SELE 1
   GO TOP
   SEEK eCOTACAO
   IF .NOT. FOUND()
      PLQUA("01")
	  PMSGR( "C¢digo N∆o existe",3)
      LOOP
   ENDIF
   eFORN_1     = FORNECED_1
   eFORN_2     = FORNECED_2
   eFORN_3     = FORNECED_3
   eFORN_4     = FORNECED_4
   eFORN_5     = FORNECED_5
   eCONDIC_1   = SPACE(60)
   eCONDIC_2   = SPACE(60)
   eCONDIC_3   = SPACE(60)
   eCONDIC_4   = SPACE(60)
   eCONDIC_5   = SPACE(60)
   ePRODU_01   = PRODUTO_01
   ePRODU_02   = PRODUTO_02
   ePRODU_03   = PRODUTO_03
   ePRODU_04   = PRODUTO_04
   ePRODU_05   = PRODUTO_05
   ePRODU_06   = PRODUTO_06
   ePRODU_07   = PRODUTO_07
   ePRODU_08   = PRODUTO_08
   ePRODU_09   = PRODUTO_09
   ePRODU_10   = PRODUTO_10
   ePRODU_11   = PRODUTO_11
   ePRODU_12   = PRODUTO_12
   ePRODU_13   = PRODUTO_13
   ePRODU_14   = PRODUTO_14
   ePRODU_15   = PRODUTO_15
   ePRODU_16   = PRODUTO_16
   ePRODU_17   = PRODUTO_17
   ePRODU_18   = PRODUTO_18
   ePRODU_19   = PRODUTO_19
   ePRODU_20   = PRODUTO_20
   ePRODU_21   = 0
   eQUANT_01   = QUANT_01
   eQUANT_02   = QUANT_02
   eQUANT_03   = QUANT_03
   eQUANT_04   = QUANT_04
   eQUANT_05   = QUANT_05
   eQUANT_06   = QUANT_06
   eQUANT_07   = QUANT_07
   eQUANT_08   = QUANT_08
   eQUANT_09   = QUANT_09
   eQUANT_10   = QUANT_10
   eQUANT_11   = QUANT_11
   eQUANT_12   = QUANT_12
   eQUANT_13   = QUANT_13
   eQUANT_14   = QUANT_14
   eQUANT_15   = QUANT_15
   eQUANT_16   = QUANT_16
   eQUANT_17   = QUANT_17
   eQUANT_18   = QUANT_18
   eQUANT_19   = QUANT_19
   eQUANT_20   = QUANT_20
   eQUANT_21   = 0
   ePR_01_01   = PR_01_01
   ePR_01_02   = PR_01_02
   ePR_01_03   = PR_01_03
   ePR_01_04   = PR_01_04
   ePR_01_05   = PR_01_05
   ePR_02_01   = PR_02_01
   ePR_02_02   = PR_02_02
   ePR_02_03   = PR_02_03
   ePR_02_04   = PR_02_04
   ePR_02_05   = PR_02_05
   ePR_03_01   = PR_03_01
   ePR_03_02   = PR_03_02
   ePR_03_03   = PR_03_03
   ePR_03_04   = PR_03_04
   ePR_03_05   = PR_03_05
   ePR_04_01   = PR_04_01
   ePR_04_02   = PR_04_02
   ePR_04_03   = PR_04_03
   ePR_04_04   = PR_04_04
   ePR_04_05   = PR_04_05
   ePR_05_01   = PR_05_01
   ePR_05_02   = PR_05_02
   ePR_05_03   = PR_05_03
   ePR_05_04   = PR_05_04
   ePR_05_05   = PR_05_05
   ePR_06_01   = PR_06_01
   ePR_06_02   = PR_06_02
   ePR_06_03   = PR_06_03
   ePR_06_04   = PR_06_04
   ePR_06_05   = PR_06_05
   ePR_07_01   = PR_07_01
   ePR_07_02   = PR_07_02
   ePR_07_03   = PR_07_03
   ePR_07_04   = PR_07_04
   ePR_07_05   = PR_07_05
   ePR_08_01   = PR_08_01
   ePR_08_02   = PR_08_02
   ePR_08_03   = PR_08_03 
   ePR_08_04   = PR_08_04
   ePR_08_05   = PR_08_05
   ePR_09_01   = PR_09_01
   ePR_09_02   = PR_09_02
   ePR_09_03   = PR_09_03
   ePR_09_04   = PR_09_04
   ePR_09_05   = PR_09_05
   ePR_10_01   = PR_10_01
   ePR_10_02   = PR_10_02
   ePR_10_03   = PR_10_03
   ePR_10_04   = PR_10_04
   ePR_10_05   = PR_10_05
   ePR_11_01   = PR_11_01
   ePR_11_02   = PR_11_02
   ePR_11_03   = PR_11_03
   ePR_11_04   = PR_11_04
   ePR_11_05   = PR_11_05
   ePR_12_01   = PR_12_01
   ePR_12_02   = PR_12_02
   ePR_12_03   = PR_12_03
   ePR_12_04   = PR_12_04
   ePR_12_05   = PR_12_05
   ePR_13_01   = PR_13_01
   ePR_13_02   = PR_13_02
   ePR_13_03   = PR_13_03
   ePR_13_04   = PR_13_04
   ePR_13_05   = PR_13_05
   ePR_14_01   = PR_14_01
   ePR_14_02   = PR_14_02
   ePR_14_03   = PR_14_03
   ePR_14_04   = PR_14_04
   ePR_14_05   = PR_14_05
   ePR_15_01   = PR_15_01
   ePR_15_02   = PR_15_02
   ePR_15_03   = PR_15_03
   ePR_15_04   = PR_15_04
   ePR_15_05   = PR_15_05
   ePR_16_01   = PR_16_01
   ePR_16_02   = PR_16_02
   ePR_16_03   = PR_16_03
   ePR_16_04   = PR_16_04
   ePR_16_05   = PR_16_05
   ePR_17_01   = PR_17_01
   ePR_17_02   = PR_17_02
   ePR_17_03   = PR_17_03
   ePR_17_04   = PR_17_04
   ePR_17_05   = PR_17_05
   ePR_18_01   = PR_18_01
   ePR_18_02   = PR_18_02
   ePR_18_03   = PR_18_03
   ePR_18_04   = PR_18_04
   ePR_18_05   = PR_18_05
   ePR_19_01   = PR_19_01
   ePR_19_02   = PR_19_02
   ePR_19_03   = PR_19_03
   ePR_19_04   = PR_19_04
   ePR_19_05   = PR_19_05
   ePR_20_01   = PR_20_01
   ePR_20_02   = PR_20_02
   ePR_20_03   = PR_20_03
   ePR_20_04   = PR_20_04
   ePR_20_05   = PR_20_05
   eSELEC_01   = SPACE(1)
   eSELEC_02   = SPACE(1)
   eSELEC_03   = SPACE(1)
   eSELEC_04   = SPACE(1)
   eSELEC_05   = SPACE(1)
   eSELEC_06   = SPACE(1)
   eSELEC_07   = SPACE(1)
   eSELEC_08   = SPACE(1)
   eSELEC_09   = SPACE(1)
   eSELEC_10   = SPACE(1)
   eSELEC_11   = SPACE(1)
   eSELEC_12   = SPACE(1)
   eSELEC_13   = SPACE(1)
   eSELEC_14   = SPACE(1)
   eSELEC_15   = SPACE(1)
   eSELEC_16   = SPACE(1)
   eSELEC_17   = SPACE(1)
   eSELEC_18   = SPACE(1)
   eSELEC_19   = SPACE(1)
   eSELEC_20   = SPACE(1)
   eSELEC_21   = SPACE(1)
   eSELEC_22   = SPACE(1)
   eSELEC_23   = SPACE(1)
   eSELEC_24   = SPACE(1)
   eSELEC_25   = SPACE(1)
   eSELEC_26   = SPACE(1)
   eSELEC_27   = SPACE(1)
   eSELEC_28   = SPACE(1)
   eSELEC_29   = SPACE(1)
   eSELEC_30   = SPACE(1)
   eSELEC_31   = SPACE(1)
   eGERAL_1    = 0
   eGERAL_2    = 0
   eGERAL_3    = 0
   eGERAL_4    = 0
   eGERAL_5    = 0
   eCONDICAO   = SPACE(50)
   eENTREGA    = SPACE(20)
   eNUM_FORN = 1
   DO WHILE eNUM_FORN <= 5
      ePROD       = 1
      DO WHILE ePROD <= 20
         eTAM_PROD = LENNUM(INT(ePROD))
         eTAM_FORN = LENNUM(INT(eNUM_FORN))
         eCAMPO_1 = "eGERAL_"+ALLTRIM(STR(eNUM_FORN,2))
         eCAMPO_2 = "eQUANT_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
         eCAMPO_3 = "ePR_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))+"_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
         eCAMPO_4 = "eSELEC_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
         &eCAMPO_1 = &eCAMPO_1 + (&eCAMPO_2 * &eCAMPO_3)
         ePROD = ePROD + 1
      ENDDO
      eNUM_FORN = eNUM_FORN + 1
   ENDDO
   eFORN = .T.
   SET COLOR TO W/N+
   @ 07,02 SAY "FORNECEDORES ENVOLVIDOS"
   SET COLOR TO W/N,+W/R
   DO WHILE .T.
      @ 09,02 SAY "Fornecedor 1  : " GET eFORN_1 PICT "@R 999,999"
      @ 09,60 SAY eGERAL_1 PICT "@R 9,999,999.9999"
      @ 10,02 SAY "Fornecedor 2  : " GET eFORN_2 PICT "@R 999,999"
      @ 10,60 SAY eGERAL_2 PICT "@R 9,999,999.9999"
      @ 11,02 SAY "Fornecedor 3  : " GET eFORN_3 PICT "@R 999,999"
      @ 11,60 SAY eGERAL_3 PICT "@R 9,999,999.9999"
      @ 12,02 SAY "Fornecedor 4  : " GET eFORN_4 PICT "@R 999,999"
      @ 12,60 SAY eGERAL_4 PICT "@R 9,999,999.9999"
      @ 13,02 SAY "Fornecedor 5  : " GET eFORN_5 PICT "@R 999,999"
      @ 13,60 SAY eGERAL_5 PICT "@R 9,999,999.9999"
      CLEAR GETS
      SELE 4
      GOTO TOP
      IF eFORN_1 <> 0
         SEEK eFORN_1
         IF FOUND()
            @ 09,27 SAY "- "+SUBSTR(NOME,1,25)
         ELSE
            PLQUA("01")
			PMSGR( "Fornecedor No. 1 N∆o Encontrado",3)
            eFORN = .F.
         ENDIF
      ENDIF
      SELE 4
      GOTO TOP
      IF eFORN_2 <> 0
         SEEK eFORN_2
         IF FOUND()
            @ 10,27 SAY "- "+SUBSTR(NOME,1,25)
         ELSE
            PLQUA("01")
			PMSGR( "Fornecedor No. 2 N∆o Encontrado",3)
            eFORN = .F.
         ENDIF
      ENDIF
      SELE 4
      GOTO TOP
      IF eFORN_3 <> 0
         SEEK eFORN_3
         IF FOUND()
            @ 11,27 SAY "- "+SUBSTR(NOME,1,25)
         ELSE
            PLQUA("01")
			PMSGR( "Fornecedor No. 3 N∆o Encontrado",3)
            eFORN = .F.
         ENDIF
      ENDIF
      SELE 4
      GOTO TOP
      IF eFORN_4 <> 0
         SEEK eFORN_4
         IF FOUND()
            @ 12,27 SAY "- "+SUBSTR(NOME,1,25)
         ELSE
            PLQUA("01")
			PMSGR( "Fornecedor No. 4 N∆o Encontrado",3)
            eFORN = .F.
         ENDIF
      ENDIF
      SELE 4
      GOTO TOP
      IF eFORN_5 <> 0
         SEEK eFORN_5
         IF FOUND()
            @ 13,27 SAY "- "+SUBSTR(NOME,1,25)
         ELSE
            PLQUA("01")
			PMSGR( "Fornecedor No. 5 N∆o Encontrado",3)
            eFORN = .F.
         ENDIF
      ENDIF
      IF .NOT. eFORN
         PLQUA("01")
         PMSGR( "Atená∆o !",3)
		 PMENSAG("Fornecedor N∆o encontrado, repita a Operaá∆o ...")
         eFORN = .T.
         PPARA()
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   eNUM_FORN = SPACE(1)
   PLQUA("01")
   @ 23,00 SAY "Informe o fornecedor que deseja desmembrar (1/2/3/4/5) ? " GET eNUM_FORN PICT "@! X" VALID eNUM_FORN $("12345")
   READ
   eNUM_FORN = VAL(eNUM_FORN)
   SELE 4
   GOTO TOP
   SEEK eNUM_FORN
   @ 23,55 SAY "- "+SUBSTR(NOME,1,40)
   PMENSAG("Pressione Qualquer Tecla Para Continuar ...")
   PPARA()
   PLQUA("01")
   @ 06,01 CLEAR TO 21,79
   SET COLOR TO W/N+
   @ 05,35 SAY "CADASTRO DE PRODUTOS"
   SET COLOR TO W/N,+W/R
   @ 06,02 SAY "Codigo"
   @ 06,10 TO 21,10
   @ 06,12 SAY "Descriá∆o"
   @ 06,30 TO 21,30
   @ 06,32 SAY "Quant."
   @ 06,43 TO 21,43
   @ 06,45 SAY "Preáo"
   @ 06,58 TO 21,58
   @ 06,60 SAY "Total"
   @ 06,74 TO 21,74
   @ 06,76 SAY "S"
   @ 06,79 TO 21,79
   eAUX    = 1
   ePROD   = 1
   DO WHILE .T.
      SELE 3
      GOTO TOP
      eTAM_PROD = LENNUM(INT(ePROD))
      eTAM_FORN = LENNUM(INT(eNUM_FORN))
      eCAMPO_1 = "ePRODU_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
      eCAMPO_2 = "eQUANT_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
      eCAMPO_3 = "ePR_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))+"_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
      eCAMPO_4 = "eSELEC_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
      IF ePROD <= 20
         @ eAUX+06,02 GET &eCAMPO_1 PICT "@R 999,999"
*        READ
         CLEAR GETS
      ELSE
         EXIT
      ENDIF
      IF &eCAMPO_1 <> 0 .AND. eAUX <= 20
         SELE 3
         SEEK &eCAMPO_1
         IF FOUND()
            @ eAUX+06,12 SAY SUBSTR(DESCRICAO,1,17)
         ELSE
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG("Produto N∆o Localizado, repita a Operaá∆o ...")
            PPARA()
            LOOP
         ENDIF
         @ eAUX+06,32 GET &eCAMPO_2 PICT "@R 99,999.99"
         @ eAUX+06,45 GET &eCAMPO_3 PICT "@R 99,999.9999"
*        READ
         CLEAR GETS
         eTOTAL = 0
         eTOTAL = &eCAMPO_2 * &eCAMPO_3
         @ eAUX+06,60 GET eTOTAL    PICT "@R 9999,999.9999"
         CLEAR GETS
         @ eAUX+06,76 GET &eCAMPO_4 PICT "@!X" VALID &eCAMPO_4 $("X ")
         READ
         eAUX = eAUX + 1
         ePROD = ePROD + 1
         IF eAUX+06 >= 21
            PMENSAG("Pressione Qualquer Tecla Para Continuar ...")
            PPARA()
            PLQUA("01")
            @ 06,01 CLEAR TO 21,79
            SET COLOR TO W/N+
            @ 05,35 SAY "CADASTRO DE PRODUTOS"
            SET COLOR TO W/N,+W/R
            @ 06,02 SAY "Codigo"
            @ 06,10 TO 21,10
            @ 06,12 SAY "Descriá∆o"
            @ 06,30 TO 21,30
            @ 06,32 SAY "Quant."
            @ 06,43 TO 21,43
            @ 06,45 SAY "Preáo"
            @ 06,58 TO 21,58
            @ 06,60 SAY "Total"
            @ 06,74 TO 21,74
            @ 06,76 SAY "S"
            @ 06,79 TO 21,79
            eAUX    = 1
         ENDIF
      ELSE
         PLQUA("01")
         PPERG ( "Deseja Encerrar o Formulario (S/N) ? ")
         IF sisRESP = "N"
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDIF
   ENDDO
*   READ
   @ 23,00 SAY "Condiá∆o : " GET eCONDICAO
*   READ
   CLEAR GETS
   PMENSAG("Pressione Qualquer Tecla Para Finalizar ...")
   PPARA()
   SELE 1
   PLQUA("01")
   PPERG ( "Confirma o Desmembramento (S/N) ? ")
   IF sisRESP = "S"
      PMENSAG("GRAVANDO, AGUARDE ...")
      * NUMERO DA PROXIMA COTAá∆o
      DO WHILE .T.
         SELE 5
         USE STATUS
         GOTO TOP
         eNOVACOTA = 0
         eNOVACOTA = COTACAO
         SELE 1
         PLQUA("01")
         SET COLOR TO W/N,+W/R
         @ 23,00 SAY "Cotaá∆o     : " GET eNOVACOTA PICT "@R 999,999"
         READ
         IF eNOVACOTA = 0
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
         ENDIF
         SELE 1
         GOTO TOP
         SEEK eNOVACOTA
         IF FOUND()
            PLQUA("01")
			PMSGR( "C¢digo já existe",3)
            LOOP
         ELSE
            SELE 5
            USE STATUS
            GOTO TOP
            IF F_BLOQR()
               REPLACE COTACAO WITH eNOVACOTA + 1
               UNLOCK
            ENDIF
            EXIT
         ENDIF
      ENDDO
      * GRAVACAO DA NOVA COTACAO
      SELE 1
      APPEND BLANK
      IF F_BLOQR()
         REPLACE NUMERO     WITH eNOVACOTA
         DO CASE
            CASE eNUM_FORN = 1
         REPLACE FORNECED_1 WITH eFORN_1
            CASE eNUM_FORN = 2
         REPLACE FORNECED_1 WITH eFORN_2
            CASE eNUM_FORN = 3
         REPLACE FORNECED_1 WITH eFORN_3
            CASE eNUM_FORN = 4
         REPLACE FORNECED_1 WITH eFORN_4
            CASE eNUM_FORN = 5
         REPLACE FORNECED_1 WITH eFORN_5
         ENDCASE
         ePROD = 1
         ePRON = 1
         DO WHILE ePROD <= 20
            SELE 3
            GOTO TOP
            eTAM_PROD = LENNUM(INT(ePROD))
            eTAM_PRON = LENNUM(INT(ePRON))
            eTAM_FORN = LENNUM(INT(eNUM_FORN))
            eCAMPO_1 = "ePRODU_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eCAMPO_2 = "eQUANT_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eCAMPO_3 = "ePR_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))+"_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
            eCAMPO_4 = "eSELEC_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eBANCO_1 = "PRODUTO_"+IIF(eTAM_PRON - 2 = 0,STR(ePRON,2),"0"+STR(ePRON,1))
            eBANCO_2 = "QUANT_"+IIF(eTAM_PRON - 2 = 0,STR(ePRON,2),"0"+STR(ePRON,1))
            eBANCO_3 = "PR_"+IIF(eTAM_PRON - 2 = 0,STR(ePRON,2),"0"+STR(ePRON,1))+"_"+"01"
            SELE 1
            IF &eCAMPO_4 = "X"
               REPLACE &eBANCO_1  WITH &eCAMPO_1,;
                       &eBANCO_2  WITH &eCAMPO_2,;
                       &eBANCO_3  WITH &eCAMPO_3
               ePRON = ePRON + 1
            ENDIF
            ePROD = ePROD + 1
         ENDDO
         eCAMPO_4 = "CONDICA_01"
         REPLACE &eCAMPO_4 WITH eCONDICAO
	 UNLOCK
      ENDIF
      * APAGANDO CAMPOS DA COTACAO ANTIGA
      SELE 1
      GOTO TOP
      SEEK eCOTACAO
      IF F_BLOQR()
         DO CASE
            CASE eNUM_FORN = 1
         REPLACE FORNECED_1 WITH 0
            CASE eNUM_FORN = 2
         REPLACE FORNECED_1 WITH 0
            CASE eNUM_FORN = 3
         REPLACE FORNECED_1 WITH 0
            CASE eNUM_FORN = 4
         REPLACE FORNECED_1 WITH 0
            CASE eNUM_FORN = 5
         REPLACE FORNECED_1 WITH 0
         ENDCASE
         ePROD   = 1
         DO WHILE ePROD <= 20
            SELE 3
            GOTO TOP
            eTAM_PROD = LENNUM(INT(ePROD))
            eTAM_FORN = LENNUM(INT(eNUM_FORN))
            eCAMPO_1 = "ePRODU_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eCAMPO_2 = "eQUANT_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eCAMPO_3 = "ePR_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))+"_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
            eCAMPO_4 = "eSELEC_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eBANCO_1 = "PRODUTO_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eBANCO_2 = "QUANT_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eBANCO_3 = "PR_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))+"_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
            SELE 1
            IF &eCAMPO_4 = "X"
               REPLACE &eBANCO_1  WITH 0,;
                       &eBANCO_2  WITH 0
            ENDIF
            REPLACE    &eBANCO_3  WITH 0
            ePROD = ePROD + 1
         ENDDO
         eCAMPO_4 = "CONDICA_01"
         REPLACE &eCAMPO_4 WITH SPACE(50)
	 UNLOCK
      ENDIF
      * COLOCANDO EM ORDEM
      eFORN_1     = FORNECED_1
      eFORN_2     = FORNECED_2
      eFORN_3     = FORNECED_3
      eFORN_4     = FORNECED_4
      eFORN_5     = FORNECED_5
      eCONDIC_1   = CONDICA_01
      eCONDIC_2   = CONDICA_02
      eCONDIC_3   = CONDICA_03
      eCONDIC_4   = CONDICA_04
      eCONDIC_5   = CONDICA_05
      ePRODU_01   = PRODUTO_01
      ePRODU_02   = PRODUTO_02
      ePRODU_03   = PRODUTO_03
      ePRODU_04   = PRODUTO_04
      ePRODU_05   = PRODUTO_05
      ePRODU_06   = PRODUTO_06
      ePRODU_07   = PRODUTO_07
      ePRODU_08   = PRODUTO_08
      ePRODU_09   = PRODUTO_09
      ePRODU_10   = PRODUTO_10
      ePRODU_11   = PRODUTO_11
      ePRODU_12   = PRODUTO_12
      ePRODU_13   = PRODUTO_13
      ePRODU_14   = PRODUTO_14
      ePRODU_15   = PRODUTO_15
      ePRODU_16   = PRODUTO_16
      ePRODU_17   = PRODUTO_17
      ePRODU_18   = PRODUTO_18
      ePRODU_19   = PRODUTO_19
      ePRODU_20   = PRODUTO_20
      ePRODU_21   = PRODUTO_21
      ePRODU_22   = PRODUTO_22
      ePRODU_23   = PRODUTO_23
      ePRODU_24   = PRODUTO_24
      ePRODU_25   = PRODUTO_25
      ePRODU_26   = PRODUTO_26
      ePRODU_27   = PRODUTO_27
      ePRODU_28   = PRODUTO_28
      ePRODU_29   = PRODUTO_29
      ePRODU_30   = PRODUTO_30
      ePRODU_31   = 0
      eQUANT_01   = QUANT_01
      eQUANT_02   = QUANT_02
      eQUANT_03   = QUANT_03
      eQUANT_04   = QUANT_04
      eQUANT_05   = QUANT_05
      eQUANT_06   = QUANT_06
      eQUANT_07   = QUANT_07
      eQUANT_08   = QUANT_08
      eQUANT_09   = QUANT_09
      eQUANT_10   = QUANT_10
      eQUANT_11   = QUANT_11
      eQUANT_12   = QUANT_12
      eQUANT_13   = QUANT_13
      eQUANT_14   = QUANT_14
      eQUANT_15   = QUANT_15
      eQUANT_16   = QUANT_16
      eQUANT_17   = QUANT_17
      eQUANT_18   = QUANT_18
      eQUANT_19   = QUANT_19
      eQUANT_20   = QUANT_20
      eQUANT_21   = QUANT_21
      eQUANT_22   = QUANT_22
      eQUANT_23   = QUANT_23
      eQUANT_24   = QUANT_24
      eQUANT_25   = QUANT_25
      eQUANT_26   = QUANT_26
      eQUANT_27   = QUANT_27
      eQUANT_28   = QUANT_28
      eQUANT_29   = QUANT_29
      eQUANT_30   = QUANT_30
      eQUANT_31   = 0
      ePR_01_01   = PR_01_01
      ePR_01_02   = PR_01_02
      ePR_01_03   = PR_01_03
      ePR_01_04   = PR_01_04
      ePR_01_05   = PR_01_05
      ePR_02_01   = PR_02_01
      ePR_02_02   = PR_02_02
      ePR_02_03   = PR_02_03
      ePR_02_04   = PR_02_04
      ePR_02_05   = PR_02_05
      ePR_03_01   = PR_03_01
      ePR_03_02   = PR_03_02
      ePR_03_03   = PR_03_03
      ePR_03_04   = PR_03_04
      ePR_03_05   = PR_03_05
      ePR_04_01   = PR_04_01
      ePR_04_02   = PR_04_02
      ePR_04_03   = PR_04_03
      ePR_04_04   = PR_04_04
      ePR_04_05   = PR_04_05
      ePR_05_01   = PR_05_01
      ePR_05_02   = PR_05_02
      ePR_05_03   = PR_05_03
      ePR_05_04   = PR_05_04
      ePR_05_05   = PR_05_05
      ePR_06_01   = PR_06_01
      ePR_06_02   = PR_06_02
      ePR_06_03   = PR_06_03
      ePR_06_04   = PR_06_04
      ePR_06_05   = PR_06_05
      ePR_07_01   = PR_07_01
      ePR_07_02   = PR_07_02
      ePR_07_03   = PR_07_03
      ePR_07_04   = PR_07_04
      ePR_07_05   = PR_07_05
      ePR_08_01   = PR_08_01
      ePR_08_02   = PR_08_02
      ePR_08_03   = PR_08_03 
      ePR_08_04   = PR_08_04
      ePR_08_05   = PR_08_05
      ePR_09_01   = PR_09_01
      ePR_09_02   = PR_09_02
      ePR_09_03   = PR_09_03
      ePR_09_04   = PR_09_04
      ePR_09_05   = PR_09_05
      ePR_10_01   = PR_10_01
      ePR_10_02   = PR_10_02
      ePR_10_03   = PR_10_03
      ePR_10_04   = PR_10_04
      ePR_10_05   = PR_10_05
      ePR_11_01   = PR_11_01
      ePR_11_02   = PR_11_02
      ePR_11_03   = PR_11_03
      ePR_11_04   = PR_11_04
      ePR_11_05   = PR_11_05
      ePR_12_01   = PR_12_01
      ePR_12_02   = PR_12_02
      ePR_12_03   = PR_12_03
      ePR_12_04   = PR_12_04
      ePR_12_05   = PR_12_05
      ePR_13_01   = PR_13_01
      ePR_13_02   = PR_13_02
      ePR_13_03   = PR_13_03
      ePR_13_04   = PR_13_04
      ePR_13_05   = PR_13_05
      ePR_14_01   = PR_14_01
      ePR_14_02   = PR_14_02
      ePR_14_03   = PR_14_03
      ePR_14_04   = PR_14_04
      ePR_14_05   = PR_14_05
      ePR_15_01   = PR_15_01
      ePR_15_02   = PR_15_02
      ePR_15_03   = PR_15_03
      ePR_15_04   = PR_15_04
      ePR_15_05   = PR_15_05
      ePR_16_01   = PR_16_01
      ePR_16_02   = PR_16_02
      ePR_16_03   = PR_16_03
      ePR_16_04   = PR_16_04
      ePR_16_05   = PR_16_05
      ePR_17_01   = PR_17_01
      ePR_17_02   = PR_17_02
      ePR_17_03   = PR_17_03
      ePR_17_04   = PR_17_04
      ePR_17_05   = PR_17_05
      ePR_18_01   = PR_18_01
      ePR_18_02   = PR_18_02
      ePR_18_03   = PR_18_03
      ePR_18_04   = PR_18_04
      ePR_18_05   = PR_18_05
      ePR_19_01   = PR_19_01
      ePR_19_02   = PR_19_02
      ePR_19_03   = PR_19_03
      ePR_19_04   = PR_19_04
      ePR_19_05   = PR_19_05
      ePR_20_01   = PR_20_01
      ePR_20_02   = PR_20_02
      ePR_20_03   = PR_20_03
      ePR_20_04   = PR_20_04
      ePR_20_05   = PR_20_05
      ePR_21_01   = PR_21_01
      ePR_21_02   = PR_21_02
      ePR_21_03   = PR_21_03
      ePR_21_04   = PR_21_04
      ePR_21_05   = PR_21_05
      ePR_22_01   = PR_22_01
      ePR_22_02   = PR_22_02
      ePR_22_03   = PR_22_03
      ePR_22_04   = PR_22_04
      ePR_22_05   = PR_22_05
      ePR_23_01   = PR_23_01
      ePR_23_02   = PR_23_02
      ePR_23_03   = PR_23_03 
      ePR_23_04   = PR_23_04
      ePR_23_05   = PR_23_05
      ePR_24_01   = PR_24_01
      ePR_24_02   = PR_24_02
      ePR_24_03   = PR_24_03
      ePR_24_04   = PR_24_04
      ePR_24_05   = PR_24_05
      ePR_25_01   = PR_25_01
      ePR_25_02   = PR_25_02
      ePR_25_03   = PR_25_03
      ePR_25_04   = PR_25_04
      ePR_25_05   = PR_25_05
      ePR_26_01   = PR_26_01
      ePR_26_02   = PR_26_02
      ePR_26_03   = PR_26_03
      ePR_26_04   = PR_26_04
      ePR_26_05   = PR_26_05
      ePR_27_01   = PR_27_01
      ePR_27_02   = PR_27_02
      ePR_27_03   = PR_27_03
      ePR_27_04   = PR_27_04
      ePR_27_05   = PR_27_05
      ePR_28_01   = PR_28_01
      ePR_28_02   = PR_28_02
      ePR_28_03   = PR_28_03
      ePR_28_04   = PR_28_04
      ePR_28_05   = PR_28_05
      ePR_29_01   = PR_29_01
      ePR_29_02   = PR_29_02
      ePR_29_03   = PR_29_03
      ePR_29_04   = PR_29_04
      ePR_29_05   = PR_29_05
      ePR_30_01   = PR_30_01
      ePR_30_02   = PR_30_02
      ePR_30_03   = PR_30_03
      ePR_30_04   = PR_30_04
      ePR_30_05   = PR_30_05
      * ZERA CAMPOS
      eCONT_FORN = 1
      DO WHILE eCONT_FORN < 6
         eNUM_FORN = eCONT_FORN
         ePRON = 1
         DO WHILE ePRON <= 20
            SELE 3
            GOTO TOP
            eTAM_PRON = LENNUM(INT(ePRON))
            eTAM_FORN = LENNUM(INT(eNUM_FORN))
            eBANCO_1 = "PRODUTO_"+IIF(eTAM_PRON - 2 = 0,STR(ePRON,2),"0"+STR(ePRON,1))
            eBANCO_2 = "QUANT_"+IIF(eTAM_PRON - 2 = 0,STR(ePRON,2),"0"+STR(ePRON,1))
            eBANCO_3 = "PR_"+IIF(eTAM_PRON - 2 = 0,STR(ePRON,2),"0"+STR(ePRON,1))+"_"+"01"
            SELE 1
            IF F_BLOQR()
               REPLACE &eBANCO_1  WITH 0,;
                       &eBANCO_2  WITH 0,;
                       &eBANCO_3  WITH 0
               UNLOCK
            ENDIF
            ePRON = ePRON + 1
         ENDDO
         eCONT_FORN = eCONT_FORN + 1
      ENDDO
      * PREENCHE CAMPOS NA SEQUENCIA
      eCONT_FORN = 1
      DO WHILE eCONT_FORN < 6
         eNUM_FORN = eCONT_FORN
         ePROD = 1
         ePRON = 1
         DO WHILE ePROD <= 20
            SELE 3
            GOTO TOP
            eTAM_PROD = LENNUM(INT(ePROD))
            eTAM_PRON = LENNUM(INT(ePRON))
            eTAM_FORN = LENNUM(INT(eNUM_FORN))
            eCAMPO_1 = "ePRODU_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eCAMPO_2 = "eQUANT_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eCAMPO_3 = "ePR_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))+"_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
            eCAMPO_4 = "eSELEC_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eBANCO_1 = "PRODUTO_"+IIF(eTAM_PRON - 2 = 0,STR(ePRON,2),"0"+STR(ePRON,1))
            eBANCO_2 = "QUANT_"+IIF(eTAM_PRON - 2 = 0,STR(ePRON,2),"0"+STR(ePRON,1))
            eBANCO_3 = "PR_"+IIF(eTAM_PRON - 2 = 0,STR(ePRON,2),"0"+STR(ePRON,1))+"_"+"01"
            SELE 1
            IF F_BLOQR()
               IF &eCAMPO_1 <> 0
                  REPLACE &eBANCO_1  WITH &eCAMPO_1,;
                          &eBANCO_2  WITH &eCAMPO_2,;
                          &eBANCO_3  WITH &eCAMPO_3
                  ePRON = ePRON + 1
               ENDIF
               UNLOCK
            ENDIF
            ePROD = ePROD + 1
         ENDDO
         UNLOCK
         eCONT_FORN = eCONT_FORN + 1
      ENDDO
      F_LACESSO(sisUSUARIO,"DESMEMBRAMENTO COTACAO","DESMEMBRAMENTO COTACAO No. "+ALLTRIM(STR(eCOTACAO,10))+"/"+ALLTRIM(STR(eNOVACOTA,10)))
      SELE 1
   ENDIF
ENDDO
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************


