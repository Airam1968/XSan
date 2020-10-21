*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_PDC2.PRG
* Data        : 10/05/2001
* Objetivo    : RELATORIOS DE PEDIDOS DE COMPRA - MODELO 2
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_PDC2()
PTIT( "RELATORIO PEDIDO DE COMPRA - MODELO 2")
PLQUA("01")
SELE 1
USE COTACAO INDEX COTACAO
GOTO TOP
SELE 2
USE ARQ_TIPO INDEX ARQ_TIPO
GOTO TOP
SELE 3
USE ARQ_ITEM INDEX ARQ_ITEM,ITEM_DES
GOTO TOP
SELE 5
USE STATUS
GOTO TOP
SELE 6
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 4
USE FORNECED INDEX FORNECED
GOTO TOP
DO WHILE .T.
   SET DEVICE TO SCREEN
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
   IF F_BLOQR()
      REPLACE SITUACAO WITH "PEDIDO DE COMPRA T2"
      UNLOCK
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
   eMENOR_01   = MENOR_01
   eMENOR_02   = MENOR_02
   eMENOR_03   = MENOR_03
   eMENOR_04   = MENOR_04
   eMENOR_05   = MENOR_05
   eENTRE_01   = ENTREGA_01
   eENTRE_02   = ENTREGA_02
   eENTRE_03   = ENTREGA_03
   eENTRE_04   = ENTREGA_04
   eENTRE_05   = ENTREGA_05
   eEMPREENDIM = EMPREENDIM
   eENTREGA    = SPACE(20)
   eNUM_REQ    = NUM_PEDIDO
   eFORN = .T.
   SELE 1
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
   eSAIDA   = SPACE(10)
   DO WHILE .T.
      PLQUA("01")
      PPERG ( "Deseja extrato em arquivo texto no Drive (X:) (S/N) ? ")
      IF sisRESP = "S"
         PLQUA("01")
         @ 23,00 SAY "CONFIRME O DESTINO  : " GET eARQUIVO
         READ
         eARQUIVO = ALLTRIM(eARQUIVO)
         PLQUA("01")
		 PMENSAG("Insira um disco no drive (X:), e tecle ENTER.")
         PPARA()
         eSAIDA = "ARQUIVO"
         EXIT
      ELSE
         eSAIDA = "IMPRESSORA"
         EXIT
      ENDIF
   ENDDO
   eFORNE = 1
   eAUX_FORN = 1
   eCONDICAO = SPACE(50)
   eMENOR = 100000000000
   IF eMENOR_01 < eMENOR
      eFORNE    = eFORN_1
      eMENOR    = eMENOR_01
      eCONDICAO = eCONDIC_1
      eENTREGA  = eENTRE_01
      eAUX_FORN = 1
   ENDIF
   IF eMENOR_02 < eMENOR .AND. eMENOR_02 <> 0
      eFORNE    = eFORN_2
      eMENOR    = eMENOR_02
      eCONDICAO = eCONDIC_2
      eENTREGA  = eENTRE_02
      eAUX_FORN = 2
   ENDIF
   IF eMENOR_03 < eMENOR .AND. eMENOR_03 <> 0
      eFORNE    = eFORN_3
      eMENOR    = eMENOR_03
      eCONDICAO = eCONDIC_3
      eENTREGA  = eENTRE_03
      eAUX_FORN = 3
   ENDIF
   IF eMENOR_04 < eMENOR .AND. eMENOR_04 <> 0
      eFORNE    = eFORN_4
      eMENOR    = eMENOR_04
      eCONDICAO = eCONDIC_4
      eENTREGA  = eENTRE_04
      eAUX_FORN = 4
   ENDIF
   IF eMENOR_05 < eMENOR .AND. eMENOR_05 <> 0
      eFORNE    = eFORN_5
      eMENOR    = eMENOR_05
      eCONDICAO = eCONDIC_5
      eENTREGA  = ENTRE_05
      eAUX_FORN = 5
   ENDIF
   IF F_BLOQR()
      REPLACE MENOR WITH eFORNE
      UNLOCK
   ENDIF
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
   eAUX = 1
   eTOTAL = 0
   DO WHILE .T.
      @ 01,00  SAY REPLICATE("-",135)
      @ 02,00  SAY "|"
      @ 02,135 SAY "|"
      @ 03,00  SAY "|"
      @ 03,05  SAY "ORDEM DE COMPRA No."+STR(eCOTACAO,10)
      @ 03,05  SAY "ORDEM DE COMPRA No."+STR(eCOTACAO,10)
      @ 03,90  SAY "|"
      @ 03,95  SAY sisTIT1
      @ 03,95  SAY sisTIT1
      @ 03,135 SAY "|"
      @ 04,00  SAY REPLICATE("-",135)
      @ 05,00  SAY "|"
      SELE 4
      SEEK eFORNE
      @ 05,05  SAY "Fornecedor : "+ALLTRIM(STR(eFORNE,10))+" - "+NOME
      @ 05,70  SAY "|"
      @ 05,75  SAY "Emissao"
      @ 05,90  SAY "|"
      SELE 5
      @ 05,95  SAY ALLTRIM(ENDERECO)
      @ 05,135 SAY "|"
      @ 06,00  SAY "|"
      SELE 4
      @ 06,05  SAY "Telefone   : "
      @ 06,20  SAY TEL_1 PICT "@R (XXXX) XXXX-XXXX"
      @ 06,40  SAY "Contato : "+ALLTRIM(CONTATO)
      @ 06,70  SAY "|"
      @ 06,75  SAY dtoc(DATE())
      @ 06,90  SAY "|"
      SELE 5
      @ 06,95  SAY ALLTRIM(BAIRRO)+" - "+ALLTRIM(CIDADE)
      @ 06,135 SAY "|"
      @ 07,00  SAY "|"
      @ 07,01  SAY REPLICATE("-",88)
      @ 07,90  SAY "|"
      @ 07,95  SAY "CEP : "
      @ 07,102 SAY CEP PICT "@R XXXXX-XXX"
      @ 07,135 SAY "|"
      @ 08,00  SAY "|"
      SELE 6
      SEEK eEMPREENDIM
      @ 08,25  SAY DESCRICAO
      @ 08,90  SAY "|"
      SELE 5
      @ 08,95  SAY "Fone : "
      @ 08,105 SAY TEL1 PICT "@R (XXXX) XXXX-XXXX"
      @ 08,135 SAY "|"
      @ 09,00  SAY "|"
      SELE 6
      @ 09,05  SAY "EMPREENDIMENTO"
      @ 09,25  SAY ENDERECO 
      @ 09,90  SAY "|"
      SELE 5
      @ 09,95  SAY "CNPJ No. : "+ALLTRIM(CGC_CPF)
      @ 09,135 SAY "|"
      @ 10,00  SAY "|"
      SELE 6
      @ 10,25  SAY ALLTRIM(BAIRRO)+" - "+ALLTRIM(CIDADE)+" - "+ALLTRIM(ESTADO)
      @ 10,90  SAY "|"
      SELE 5
      @ 10,95  SAY "I.Estadual No. : "+ALLTRIM(INSCRICAO)
      @ 10,135 SAY "|"
      @ 11,00  SAY REPLICATE("-",135)
      @ 12,00  SAY "|"
      @ 12,05  SAY "ITEM"
      @ 12,15  SAY "|"
      @ 12,20  SAY "DESCRICAO"
      @ 12,60  SAY "|"
      @ 12,65  SAY "UND."
      @ 12,70  SAY "|"
      @ 12,75  SAY "QUANT."
      @ 12,85  SAY "|"
      @ 12,90  SAY "PRECO UND."
      @ 12,105 SAY "|"
      @ 12,110 SAY "PRECO TOTAL"
      @ 12,135 SAY "|"
      @ 13,00  SAY REPLICATE("-",135)
      SELE 1
      LIN = 14
      DO WHILE eAUX < 31 .AND. LIN <= 50
         @ LIN,00  SAY "|"
         eTAM_PROD = 0
         eTAM_PROD = LENNUM(INT(eAUX))
         eTAM_FORN = 0
         eTAM_FORN = LENNUM(INT(eAUX_FORN))
         eCAMPO_1  = "ePRODU_"+IIF(eTAM_PROD - 2 = 0,STR(eAUX,2),"0"+STR(eAUX,1))
         eCAMPO_2  = "eQUANT_"+IIF(eTAM_PROD - 2 = 0,STR(eAUX,2),"0"+STR(eAUX,1))
         eCAMPO_3  = "ePR_"+IIF(eTAM_PROD - 2 = 0,STR(eAUX,2),"0"+STR(eAUX,1))+"_"+IIF(eTAM_FORN - 2 = 0,STR(eAUX_FORN,2),"0"+STR(eAUX_FORN,1))
         IF &eCAMPO_1 <> 0
            @ LIN,02  SAY &eCAMPO_1 PICT "@R 999,999"
            @ LIN,15  SAY "|"
            SELE 3
            SEEK &eCAMPO_1
            @ LIN,17  SAY DESCRICAO
            @ LIN,60  SAY "|"
            @ LIN,62  SAY UNIDADE
            @ LIN,70  SAY "|"
            SELE 1
            @ LIN,72  SAY &eCAMPO_2 PICT "@R 999,999"
            @ LIN,85  SAY "|"
            @ LIN,90  SAY &eCAMPO_3 PICT "@R 999,999,999.99"
            @ LIN,105 SAY "|"
            @ LIN,107 SAY &eCAMPO_2 * &eCAMPO_3 PICT "@R 999,999,999.99"
            @ LIN,135 SAY "|"
            eTOTAL = eTOTAL + &eCAMPO_2 * &eCAMPO_3
            LIN  = LIN + 1
            eAUX = eAUX + 1
         ELSE
            EXIT
         ENDIF
      ENDDO
      @ LIN,00 SAY REPLICATE("-",135)
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,105  SAY "|"
      @ LIN,107 SAY eTOTAL PICT "@R 999,999,999.99"
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,05  SAY "CONDICAO DE PAGAMENTO : "+eCONDICAO
      @ LIN,105 SAY "|"
      @ LIN,106 SAY REPLICATE("-",28)
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,05  SAY "PRAZO DE ENTREGA : "+eENTREGA
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",135)
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,05  SAY "OBS. GERAL : A Nota Fiscal devera ser emitida com os dados da COMPRADORA constando no corpo da nota o nome do EMPREENDIMENTO,"
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,05  SAY "           : seu Endereco, conforme a presente ORDEM DE COMPRA."
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      *** CHECAGEM E IMPRESSAO DOS AVISOS.
      eAUX = 1
      eTAM_PROD = 0
      eTAM_PROD = LENNUM(INT(eAUX))
      eTAM_FORN = 0
      eTAM_FORN = LENNUM(INT(eAUX_FORN))
      eCAMPO_1  = "ePRODU_"+IIF(eTAM_PROD - 2 = 0,STR(eAUX,2),"0"+STR(eAUX,1))
      DO WHILE &eCAMPO_1 <> 0
         SELE 3
         GOTO TOP
         SEEK &eCAMPO_1
         IF OBSERVA_01 <> SPACE(50)
            @ LIN,00  SAY "|"
            @ LIN,05  SAY "OBS.PRODUTO: "+OBSERVA_01
            @ LIN,135 SAY "|"
            LIN = LIN + 1
            IF OBSERVA_02 <> SPACE(50)
               @ LIN,00  SAY "|"
               @ LIN,05  SAY "           : "+OBSERVA_02
               @ LIN,135 SAY "|"
               LIN = LIN + 1
            ENDIF
            IF OBSERVA_03 <> SPACE(50)
               @ LIN,00  SAY "|"
               @ LIN,05  SAY "           : "+OBSERVA_03
               @ LIN,135 SAY "|"
               LIN = LIN + 1
            ENDIF
            IF OBSERVA_04 <> SPACE(50)
               @ LIN,00  SAY "|"
               @ LIN,05  SAY "           : "+OBSERVA_04
               @ LIN,135 SAY "|"
               LIN = LIN + 1
            ENDIF
            IF OBSERVA_05 <> SPACE(50)
               @ LIN,00  SAY "|"
               @ LIN,05  SAY "           : "+OBSERVA_05
               @ LIN,135 SAY "|"
               LIN = LIN + 1
            ENDIF
            IF OBSERVA_06 <> SPACE(50)
               @ LIN,00  SAY "|"
               @ LIN,05  SAY "           : "+OBSERVA_06
               @ LIN,135 SAY "|"
               LIN = LIN + 1
            ENDIF
            IF OBSERVA_07 <> SPACE(50)
               @ LIN,00  SAY "|"
               @ LIN,05  SAY "           : "+OBSERVA_07
               @ LIN,135 SAY "|"
               LIN = LIN + 1
            ENDIF
         ENDIF
         eAUX = eAUX + 1
         eTAM_PROD = 0
         eTAM_PROD = LENNUM(INT(eAUX))
         eTAM_FORN = 0
         eTAM_FORN = LENNUM(INT(eAUX_FORN))
         eCAMPO_1  = "ePRODU_"+IIF(eTAM_PROD - 2 = 0,STR(eAUX,2),"0"+STR(eAUX,1))
      ENDDO
      @ LIN,00  SAY "|"
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,05  SAY "Requisicao No : "
      @ LIN,20  SAY eNUM_REQ PICT "999,999,999"
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",135)
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,05  SAY "Recebido em : ____/____/________"
      @ LIN,60  SAY "|"
      @ LIN,65  SAY "Nota Fiscal No. : ________________"
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00  SAY "|"
      @ LIN,05  SAY "Conferido por : ________________"
      @ LIN,60  SAY "|"
      @ LIN,65  SAY "Vencimento em : ____/____/________"
      @ LIN,135 SAY "|"
      LIN = LIN + 1
      @ LIN,00 SAY REPLICATE("-",135)
      LIN = LIN + 1
      SET DEVICE TO SCREEN
      SELE 1
      IF F_BLOQR()
         REPLACE TOTAL WITH eTOTAL
         UNLOCK
      ENDIF
      SET DEVICE TO PRINT
      @ LIN,00 SAY REPLICATE("-",135)
      LIN = LIN + 1
      @ LIN,90 SAY sisTIT3
      @ 01,00  SAY " "+sisLETRAN
      EXIT
   ENDDO
   SET DEVICE TO SCREEN
ENDDO
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RETURN nil
***************************** fim ***********************************
