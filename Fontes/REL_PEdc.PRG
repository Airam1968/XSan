*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_PEDC.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIOS DE PEDIDOS DE COMPRA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_PEDC()
PTIT( "RELATORIO PEDIDO DE COMPRA")
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
SELE 4
USE FORNECED INDEX FORNECED
GOTO TOP
SELE 5
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 4
DO WHILE .T.
   SET DEVICE TO SCREEN
   eCOTACAO = 0
   SELE 1
   PLIMPA()
   SET COLOR TO W/N,+W/R
   @ 05,02 SAY "Cota‡Æo     : " GET eCOTACAO PICT "@R 999,999"
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
	  PMSGR( "C¢digo NÆo existe",3)
      LOOP
   ENDIF
   IF F_BLOQR()
      REPLACE SITUACAO WITH "PEDIDO DE COMPRA T1"
      UNLOCK
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
   eMENOR_01   = MENOR_01
   eMENOR_02   = MENOR_02
   eMENOR_03   = MENOR_03
   eMENOR_04   = MENOR_04
   eMENOR_05   = MENOR_05
   eEMPREENDIM = EMPREENDIM
   eFORN = .T.
   eARQUIVO = SPACE(60)
   eARQUIVO = sisDRIVE_REL+":\"+sisPASTA_REL+"CTAPAG.TXT"+SPACE(30)
   IF (ALLTRIM(sisDRIVE_REL) = "").and.(ALLTRIM(sisPASTA_REL) = "") 
      eARQUIVO = "C:\RELATORIOS\RELATORIO.TXT          "
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
   SELE 1
   eFORNE = 1
   eAUX_FORN = 1
   eCONDICAO = SPACE(50)
   eENTREGA  = SPACE(50)
   eMENOR = 100000000000
   IF eMENOR_01 < eMENOR .AND. eMENOR_01 <> 0
      eFORNE    = eFORN_1
      eMENOR    = eMENOR_01
      eCONDICAO = CONDICA_01
      eENTREGA  = ENTREGA_01
      eAUX_FORN = 1
   ENDIF
   IF eMENOR_02 < eMENOR .AND. eMENOR_02 <> 0
      eFORNE    = eFORN_2
      eMENOR    = eMENOR_02
      eCONDICAO = CONDICA_02
      eENTREGA  = ENTREGA_02
      eAUX_FORN = 2
   ENDIF
   IF eMENOR_03 < eMENOR .AND. eMENOR_03 <> 0
      eFORNE    = eFORN_3
      eMENOR    = eMENOR_03
      eCONDICAO = CONDICA_03
      eENTREGA  = ENTREGA_03
      eAUX_FORN = 3
   ENDIF
   IF eMENOR_04 < eMENOR .AND. eMENOR_04 <> 0
      eFORNE    = eFORN_4
      eMENOR    = eMENOR_04
      eCONDICAO = CONDICA_04
      eENTREGA  = ENTREGA_04
      eAUX_FORN = 4
   ENDIF
   IF eMENOR_05 < eMENOR .AND. eMENOR_05 <> 0
      eFORNE    = eFORN_5
      eMENOR    = eMENOR_05
      eCONDICAO = CONDICA_05
      eENTREGA  = ENTREGA_05
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
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SCC001"
      @ 03,40  SAY sisLETRAE+"PEDIDO DE COMPRA No."+STR(eCOTACAO,10)+sisLETRAC
      SELE 4
      SEEK eFORNE
      @ 04,00  SAY "Fornecedor : "+STR(eFORNE,10)+" - "+NOME+" - "+APELIDO
      @ 05,00  SAY "Endereco   : "+ALLTRIM(END)+" - "+ALLTRIM(BAIRRO)+" - "+ALLTRIM(CIDADE)+" - "+ALLTRIM(ESTADO)
      @ 06,00  SAY "Vendedor   : "+ALLTRIM(CONTATO)
      @ 07,00  SAY "Telefone   : "
      @ 07,14  SAY TEL_1 PICT "@R (XX) XXXXX-XXXXX"
      @ 07,40  SAY "Fax        : "
      @ 07,54  SAY TEL_2 PICT "@R (XX) XXXXX-XXXXX"
      SELE 1
      @ 08,00  SAY REPLICATE("=",135)
      @ 09,00  SAY "|"
      @ 09,02  SAY "CODIGO"
      @ 09,12  SAY "TIPO"
      @ 09,24  SAY "DESCRICAO"
      @ 09,74  SAY "UN"
      @ 09,80  SAY "QUANTIDADE"
      @ 09,93  SAY "PRECO UN."
      @ 09,110 SAY "TOTAL"
      @ 09,134 SAY "|"
      @ 10,00 SAY REPLICATE("=",135)
      LIN = 11
      DO WHILE eAUX < 31 .AND. LIN <= 57
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
            SELE 3
            SEEK &eCAMPO_1
            @ LIN,12  SAY TIPO     PICT "@R 999"
            @ LIN,24  SAY DESCRICAO
            @ LIN,74  SAY UNIDADE
            SELE 1
            @ LIN,80  SAY &eCAMPO_2 PICT "@R 999,999"
            @ LIN,90  SAY &eCAMPO_3 PICT "@R 99999,999.9999"
            @ LIN,110 SAY &eCAMPO_2 * &eCAMPO_3 PICT "@R 9,999,999.9999"
            @ LIN,134 SAY "|"
            eTOTAL = eTOTAL + &eCAMPO_2 * &eCAMPO_3
            LIN  = LIN + 1
            eAUX = eAUX + 1
         ELSE
            EXIT
         ENDIF
      ENDDO
      @ LIN,00 SAY REPLICATE("-",135)
      LIN = LIN + 1
      @ LIN,110 SAY eTOTAL PICT "@R 9,999,999.9999"
      LIN = LIN + 2
      SET DEVICE TO SCREEN 
      SELE 1
      IF F_BLOQR()
         REPLACE TOTAL WITH eTOTAL
         UNLOCK
      ENDIF
      SET DEVICE TO PRINT
      @ LIN,00  SAY "CONDICOES : "+eCONDICAO
      LIN = LIN + 1
      @ LIN,00  SAY "ENTREGA   : "+eENTREGA
      LIN = LIN + 2
      SELE 5
      SEEK eEMPREENDIM
      @ LIN,00  SAY "ENDERE‡O DE ENTREGA : "+DESCRICAO
      LIN = LIN + 1
      @ LIN,00  SAY "                    : "+ENDERECO+" - "+BAIRRO
      LIN = LIN + 1
      @ LIN,00  SAY "                    : "+CIDADE+" - "+ESTADO+" - "+CEP
      LIN = LIN + 1
      @ 58,00 SAY REPLICATE("=",135)
      @ 59,100 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
      EXIT
   ENDDO
   IF eSAIDA = "ARQUIVO"
      SET PRINT TO
      SET PRINT OFF
      SET DEVICE TO SCREEN
   ELSE
      SET PRINT TO
      SET DEVICE TO SCREEN
   ENDIF
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
RETURN nil
***************************** fim ***********************************
