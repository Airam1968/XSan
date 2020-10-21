*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : LAN_COTA.PRG
* Data        : 01/01/99
* Objetivo    : LANCAMENTO DE COTACOES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION LAN_COTA()
SAVE SCREEN TO TELA03
PTIT( "LANCAMENTO DE COTACÂES")
SELE 6
USE CLAS_PRO INDEX CLAS_PRO,DESC_PRO
GOTO TOP
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
   eCONDIC_1   = CONDICA_01 
   eCONDIC_2   = CONDICA_02
   eCONDIC_3   = CONDICA_03
   eCONDIC_4   = CONDICA_04
   eCONDIC_5   = CONDICA_05
   eENTREG_1   = ENTREGA_01
   eENTREG_2   = ENTREGA_02
   eENTREG_3   = ENTREGA_03
   eENTREG_4   = ENTREGA_04
   eENTREG_5   = ENTREGA_05
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
   eGERAL_1    = 0
   eGERAL_2    = 0
   eGERAL_3    = 0
   eGERAL_4    = 0
   eGERAL_5    = 0
   eNUM_FORN   = 1
   eCONDICAO   = SPACE(50)
   eENTREGA    = SPACE(20)
   eCLASSE_PRO = EMPREENDIM
   eNUM_REQ    = NUM_PEDIDO
   DO WHILE eNUM_FORN <= 5
      ePROD       = 1
      DO WHILE ePROD <= 20
         eTAM_PROD = LENNUM(INT(ePROD))
         eTAM_FORN = LENNUM(INT(eNUM_FORN))
         eCAMPO_1 = "eGERAL_"+ALLTRIM(STR(eNUM_FORN,2))
         eCAMPO_2 = "eQUANT_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
         eCAMPO_3 = "ePR_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))+"_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
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
            eNOME = NOME
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
            eNOME = NOME
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
            eNOME = NOME
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
            eNOME = NOME
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
            eNOME = NOME
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
   SELE 6
   GOTO TOP
   DO WHILE .T.
      @ 15,02 SAY "Cod. Produto: " GET eCLASSE_PRO PICT "@R 99999999"
      CLEAR GETS
      SEEK eCLASSE_PRO
      IF FOUND()
         @ 15,28 SAY " - "+ALLTRIM(DESCRICAO)
         EXIT
      ELSE
         PLQUA("01")
         PMSGR( "Produto Inexistente !",3)
         PMENSAG("Produto Inexistente !")
         PPARA()
         RESTORE SCREEN FROM TELA03
         CLOSE ALL
         RETURN nil
      ENDIF
   ENDDO
   @ 17,02 SAY "Requisiáao  : "GET eNUM_REQ PICT "@R 999,999,999"
   CLEAR GETS
   DO WHILE .T.
      eNUM_FORN = SPACE(1)
      PLQUA("01")
      @ 23,00 SAY "Informe o fornecedor, para as cotaááes (1/2/3/4/5) ? " GET eNUM_FORN PICT "@! X" VALID eNUM_FORN $("12345")
      READ
      eNUM_FORN = VAL(eNUM_FORN)
      DO CASE
         CASE eNUM_FORN = 1
            IF eFORN_1 = 0
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
			   PMENSAG("Fornecedor Inativo nesta Cotacao ...")
               PPARA()
               LOOP
            ELSE
               EXIT
            ENDIF
         CASE eNUM_FORN = 2
            IF eFORN_2 = 0
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
			   PMENSAG("Fornecedor Inativo nesta Cotacao ...")
               PPARA()
               LOOP
            ELSE
               EXIT
            ENDIF
         CASE eNUM_FORN = 3
            IF eFORN_3 = 0
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
			   PMENSAG("Fornecedor Inativo nesta Cotacao ...")
               PPARA()
               LOOP
            ELSE
               EXIT
            ENDIF
         CASE eNUM_FORN = 4
            IF eFORN_4 = 0
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
			   PMENSAG("Fornecedor Inativo nesta Cotacao ...")
               PPARA()
               LOOP
            ELSE
               EXIT
            ENDIF
         CASE eNUM_FORN = 5
            IF eFORN_5 = 0
               PLQUA("01")
               PMSGR( "Atená∆o !",3)
			   PMENSAG("Fornecedor Inativo nesta Cotacao ...")
               PPARA()
               LOOP
            ELSE
               EXIT
            ENDIF
      ENDCASE
   ENDDO
   PLQUA("01")
   SELE 4
   GOTO TOP
   SEEK eNUM_FORN
   @ 23,01 SAY NOME
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
   @ 06,60 TO 21,60
   @ 06,62 SAY "Total"
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
      IF ePROD <= 20
         @ eAUX+06,02 GET &eCAMPO_1 PICT "@R 999,999"
         READ
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
         @ eAUX+06,32 GET &eCAMPO_2 PICT "@R 9999.9999"
         @ eAUX+06,45 GET &eCAMPO_3 PICT "@R 99,999.9999"
         READ
         eTOTAL = 0
         eTOTAL = &eCAMPO_2 * &eCAMPO_3
         @ eAUX+06,62 GET eTOTAL    PICT "@R 9999,999.9999"
         CLEAR GETS
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
            @ 06,60 TO 21,60
            @ 06,62 SAY "Total"
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
   READ
   DO CASE
      CASE ePROD = 1
         eCONDICAO = eCONDIC_1
         eENTREGA  = eENTREG_1
      CASE ePROD = 2
         eCONDICAO = eCONDIC_2
         eENTREGA  = eENTREG_2
      CASE ePROD = 3
         eCONDICAO = eCONDIC_3
         eENTREGA  = eENTREG_3
      CASE ePROD = 4
         eCONDICAO = eCONDIC_4
         eENTREGA  = eENTREG_4
      CASE ePROD = 5
         eCONDICAO = eCONDIC_5
         eENTREGA  = eENTREG_5
   ENDCASE
   PLQUA("01")
   @ 23,00 SAY "Condiá∆o : " GET eCONDICAO
   READ
   PLQUA("01")
   @ 23,00 SAY "Entrega  : " GET eENTREGA
   READ
   PMENSAG("Pressione Qualquer Tecla Para Finalizar ...")
   PPARA()
   SELE 1
   PLQUA("01")
   PPERG ( "Confirma Alteraá∆o (S/N) ? ")
   IF sisRESP = "S"
	  PMENSAG("GRAVANDO, AGUARDE ...")
      SELE 1
      IF F_BLOQR()
         REPLACE NUMERO     WITH eCOTACAO,;
                 FORNECED_1 WITH eFORN_1,;
                 FORNECED_2 WITH eFORN_2
         REPLACE FORNECED_3 WITH eFORN_3,;
                 FORNECED_4 WITH eFORN_4,;
                 FORNECED_5 WITH eFORN_5
         ePROD   = 1
         DO WHILE ePROD <= 20
            SELE 3
            GOTO TOP
            eTAM_PROD = LENNUM(INT(ePROD))
            eTAM_FORN = LENNUM(INT(eNUM_FORN))
            eCAMPO_1 = "ePRODU_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eCAMPO_2 = "eQUANT_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eCAMPO_3 = "ePR_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))+"_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
            eBANCO_1 = "PRODUTO_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eBANCO_2 = "QUANT_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
            eBANCO_3 = "PR_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))+"_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
            SELE 1
            REPLACE &eBANCO_1  WITH &eCAMPO_1,;
                    &eBANCO_2  WITH &eCAMPO_2,;
                    &eBANCO_3  WITH &eCAMPO_3
            ePROD = ePROD + 1
         ENDDO
         eCAMPO_4 = "CONDICA_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
         eCAMPO_5 = "ENTREGA_"+IIF(eTAM_FORN - 2 = 0,STR(eNUM_FORN,2),"0"+STR(eNUM_FORN,1))
         REPLACE &eCAMPO_4 WITH eCONDICAO,;
                 &eCAMPO_5 WITH eENTREGA
	 UNLOCK
      ENDIF
      F_LACESSO(sisUSUARIO,"LANCAMENTO COTACAO","LANCAMENTO COTACAO No. "+ALLTRIM(STR(eCOTACAO,10))) 
      SELE 1
   ENDIF
ENDDO
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************


