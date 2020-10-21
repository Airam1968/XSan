*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_COTA.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE COTACOES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_COTA()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA DE COTACÂES")
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
   ePR_01_01   = 0
   ePR_01_02   = 0
   ePR_01_03   = 0
   ePR_01_04   = 0
   ePR_01_05   = 0
   ePR_02_01   = 0
   ePR_02_02   = 0
   ePR_02_03   = 0
   ePR_02_04   = 0
   ePR_02_05   = 0
   ePR_03_01   = 0
   ePR_03_02   = 0
   ePR_03_03   = 0
   ePR_03_04   = 0
   ePR_03_05   = 0
   ePR_04_01   = 0
   ePR_04_02   = 0
   ePR_04_03   = 0
   ePR_04_04   = 0
   ePR_04_05   = 0
   ePR_05_01   = 0
   ePR_05_02   = 0
   ePR_05_03   = 0
   ePR_05_04   = 0
   ePR_05_05   = 0
   ePR_06_01   = 0
   ePR_06_02   = 0
   ePR_06_03   = 0
   ePR_06_04   = 0
   ePR_06_05   = 0
   ePR_07_01   = 0
   ePR_07_02   = 0
   ePR_07_03   = 0
   ePR_07_04   = 0
   ePR_07_05   = 0
   ePR_08_01   = 0
   ePR_08_02   = 0
   ePR_08_03   = 0
   ePR_08_04   = 0
   ePR_08_05   = 0
   ePR_09_01   = 0
   ePR_09_02   = 0
   ePR_09_03   = 0
   ePR_09_04   = 0
   ePR_09_05   = 0
   ePR_10_01   = 0
   ePR_10_02   = 0
   ePR_10_03   = 0
   ePR_10_04   = 0
   ePR_10_05   = 0
   ePR_11_01   = 0
   ePR_11_02   = 0
   ePR_11_03   = 0
   ePR_11_04   = 0
   ePR_11_05   = 0
   ePR_12_01   = 0
   ePR_12_02   = 0
   ePR_12_03   = 0
   ePR_12_04   = 0
   ePR_12_05   = 0
   ePR_13_01   = 0
   ePR_13_02   = 0
   ePR_13_03   = 0
   ePR_13_04   = 0
   ePR_13_05   = 0
   ePR_14_01   = 0
   ePR_14_02   = 0
   ePR_14_03   = 0
   ePR_14_04   = 0
   ePR_14_05   = 0
   ePR_15_01   = 0
   ePR_15_02   = 0
   ePR_15_03   = 0
   ePR_15_04   = 0
   ePR_15_05   = 0
   ePR_16_01   = 0
   ePR_16_02   = 0
   ePR_16_03   = 0
   ePR_16_04   = 0
   ePR_16_05   = 0
   ePR_17_01   = 0
   ePR_17_02   = 0
   ePR_17_03   = 0
   ePR_17_04   = 0
   ePR_17_05   = 0
   ePR_18_01   = 0
   ePR_18_02   = 0
   ePR_18_03   = 0
   ePR_18_04   = 0
   ePR_18_05   = 0
   ePR_19_01   = 0
   ePR_19_02   = 0
   ePR_19_03   = 0
   ePR_19_04   = 0
   ePR_19_05   = 0
   ePR_20_01   = 0
   ePR_20_02   = 0
   ePR_20_03   = 0
   ePR_20_04   = 0
   ePR_20_05   = 0
   eCONDICAO   = SPACE(50)
   eENTREGA    = SPACE(20)
   eCLASSE_PRO = EMPREENDIM
   eNUM_REQ    = NUM_PEDIDO
   eFORN = .T.
   SET COLOR TO W/N+
   @ 07,02 SAY "FORNECEDORES ENVOLVIDOS"
   SET COLOR TO W/N,+W/R
   DO WHILE .T.
      @ 09,02 SAY "Fornecedor 1  : " GET eFORN_1 PICT "@R 999,999"
      @ 10,02 SAY "Fornecedor 2  : " GET eFORN_2 PICT "@R 999,999"
      @ 11,02 SAY "Fornecedor 3  : " GET eFORN_3 PICT "@R 999,999"
      @ 12,02 SAY "Fornecedor 4  : " GET eFORN_4 PICT "@R 999,999"
      @ 13,02 SAY "Fornecedor 5  : " GET eFORN_5 PICT "@R 999,999"
      CLEAR GETS
      SELE 4
      GOTO TOP
      IF eFORN_1 <> 0
         SEEK eFORN_1
         IF FOUND()
            @ 09,27 SAY "- "+SUBSTR(NOME,1,40)
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
            @ 10,27 SAY "- "+SUBSTR(NOME,1,40)
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
            @ 11,27 SAY "- "+SUBSTR(NOME,1,40)
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
            @ 12,27 SAY "- "+SUBSTR(NOME,1,40)
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
            @ 13,27 SAY "- "+SUBSTR(NOME,1,40)
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
   eAUX    = 1
   ePROD   = 1
   DO WHILE .T.
      SELE 3
      GOTO TOP
      eTAM_PROD = LENNUM(INT(ePROD))
      eCAMPO_1 = "ePRODU_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
      eCAMPO_2 = "eQUANT_"+IIF(eTAM_PROD - 2 = 0,STR(ePROD,2),"0"+STR(ePROD,1))
      IF eAUX <= 20
         @ eAUX+06,02 GET &eCAMPO_1 PICT "@R 999,999"
      ELSE
		 PMENSAG("Pressione Qualquer Tecla Para Finalizar ...")
         PPARA()
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
            eAUX    = 1
         ENDIF
      ELSE
         CLEAR GETS
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
   PMENSAG("Pressione Qualquer Tecla Para Finalizar ...")
   PPARA()
   SELE 1
   PLQUA("01")
   PPERG ( "Continua (S/N) ? ")
   IF sisRESP = "S"
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************


