*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_RECO.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO PEDIDO DE MATERIAL        
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_RECO()
PTIT( "RELATORIO PEDIDO DE MATERIAL")
PLQUA("01")
SELE 6
USE CLAS_PRO INDEX CLAS_PRO,DESC_PRO
GOTO TOP
SELE 1
USE PED_COMP INDEX PED_COMP
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
   SET DEVICE TO SCREEN
   ePED_COMP = 0
   SELE 1
   PLIMPA()
   SET COLOR TO W/N,+W/R
   @ 05,02 SAY "Pedido Num. : " GET ePED_COMP PICT "@R 999,999"
   READ
   IF ePED_COMP = 0
      CLOSE DATABASES
      RESTORE SCREEN FROM TELA03
      RETURN nil
   ENDIF
   SELE 1
   GO TOP
   SEEK ePED_COMP
   IF .NOT. FOUND()
      PLQUA("01")
	  PMSGR( "C¢digo N∆o existe",3)
      LOOP
   ENDIF
   IF F_BLOQR()
      REPLACE SITUACAO WITH "PEDIDO FORNECEDOR"
      UNLOCK
   ENDIF
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
   eFORN = .T.
   eCLAS_PRO   = EMPREENDIM
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
   SELE 6
   GOTO TOP
   SEEK eCLAS_PRO
   eDESC_PRO = DESCRICAO
   SELE 1
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
   eAUX      = 1
   eAUX_FOR  = 1
   DO WHILE .T.
      eAUX      = 1
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SCC001"
      @ 03,40  SAY sisLETRAE+"REQUISICAO DE MATERIAIS No."+STR(ePED_COMP,10)+sisLETRAC
      @ 04,00  SAY "Empreendimento : "+STR(eCLAS_PRO,10)+" - "+eDESC_PRO
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,00  SAY "|"
      @ 06,02  SAY "CODIGO"
      @ 06,12  SAY "TIPO"
      @ 06,18  SAY "DESCRICAO"
      @ 06,68  SAY "MARCA"
      @ 06,88  SAY "UN"
      @ 06,94  SAY "QUANT."
      @ 06,102 SAY "PRECO UN."
      @ 06,118 SAY "PRECO TOTAL"
      @ 06,134 SAY "|"
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 08 
      DO WHILE eAUX < 21 .AND. LIN <= 57
         @ LIN,00  SAY "|"
         eTAM_PROD = 0
         eTAM_PROD = LENNUM(INT(eAUX))
         eCAMPO_1  = "ePRODU_"+IIF(eTAM_PROD - 2 = 0,STR(eAUX,2),"0"+STR(eAUX,1))
         eCAMPO_2  = "eQUANT_"+IIF(eTAM_PROD - 2 = 0,STR(eAUX,2),"0"+STR(eAUX,1))
         IF &eCAMPO_1 <> 0
            @ LIN,02  SAY &eCAMPO_1 PICT "@R 999,999"
            SELE 3
            SEEK &eCAMPO_1
            @ LIN,12  SAY TIPO     PICT "@R 999"
            @ LIN,18  SAY DESCRICAO
            @ LIN,68  SAY MARCA
            @ LIN,88  SAY UNIDADE
            SELE 1
            @ LIN,94  SAY &eCAMPO_2 PICT "@R 9999.99"
            @ LIN,134 SAY "|"
            LIN  = LIN + 1
            eAUX = eAUX + 1
         ELSE
            EXIT
         ENDIF
      ENDDO
      @ 53,00 SAY "DATA DA SOLICITACAO : ____/____/______    PREVISAO DE ENTREGA : ____/____/______"
      @ 54,00 SAY "OBS: A COMPRA SERA EFETUADA 15 DIAS APOS A SOLICITACAO"
      @ 55,15 SAY "_____________________________"
      @ 55,80 SAY "_____________________________"
      @ 56,15 SAY "RESPONSAVEL PELO ALMOXARIFADO"
      @ 56,80 SAY "  ENGENHEIRO - ADMINISTRACAO "
      @ 58,00 SAY REPLICATE("=",135)
      @ 59,100 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
      EXIT
   ENDDO
   EXIT
ENDDO
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RETURN nil
***************************** fim ***********************************
