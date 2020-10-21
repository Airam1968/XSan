*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : INC_PEDC.PRG
* Data        : 01/07/2001
* Objetivo    : INCLUSAO DE PEDIDO DE MATERIAL
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
***************************************************************************
FUNCTION INC_PEDC()
SAVE SCREEN TO TELA03
PTIT( "INCLUSAO DE PEDIDO DE MATERIAL")
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
DO WHILE .T.
   SELE 5
   USE STATUS
   GOTO TOP
   ePED_COMP  = 0
   ePED_COMP  = PED_COMP
   eORDE_CLAS = ORDE_CLAS
   SELE 1
   PLIMPA()
   SET COLOR TO W/N,+W/R
   @ 05,02 SAY "Cotaá∆o     : " GET ePED_COMP PICT "@R 999,999"
   READ
   IF ePED_COMP = 0
      CLOSE DATABASES
      RESTORE SCREEN FROM TELA03
      RETURN nil
   ENDIF
   SELE 1
   GO TOP
   SEEK ePED_COMP
   IF FOUND()
      PLQUA("01")
	  PMSGR( "C¢digo já existe",3)
      LOOP
   ENDIF
   eFORN_1     = 0
   eFORN_2     = 0
   eFORN_3     = 0
   eFORN_4     = 0
   eFORN_5     = 0
   eCONDIC_1   = SPACE(60)
   eCONDIC_2   = SPACE(60)
   eCONDIC_3   = SPACE(60)
   eCONDIC_4   = SPACE(60)
   eCONDIC_5   = SPACE(60)
   eENTREG_1   = SPACE(20)
   eENTREG_2   = SPACE(20)
   eENTREG_3   = SPACE(20)
   eENTREG_4   = SPACE(20)
   eENTREG_5   = SPACE(20)
   ePRODU_01   = 0
   ePRODU_02   = 0
   ePRODU_03   = 0
   ePRODU_04   = 0
   ePRODU_05   = 0
   ePRODU_06   = 0
   ePRODU_07   = 0
   ePRODU_08   = 0
   ePRODU_09   = 0
   ePRODU_10   = 0
   ePRODU_11   = 0
   ePRODU_12   = 0
   ePRODU_13   = 0
   ePRODU_14   = 0
   ePRODU_15   = 0
   ePRODU_16   = 0
   eQUANT_01   = 0
   eQUANT_02   = 0
   eQUANT_03   = 0
   eQUANT_04   = 0
   eQUANT_05   = 0
   eQUANT_06   = 0
   eQUANT_07   = 0
   eQUANT_08   = 0
   eQUANT_09   = 0
   eQUANT_10   = 0
   eQUANT_11   = 0
   eQUANT_12   = 0
   eQUANT_13   = 0
   eQUANT_14   = 0
   eQUANT_15   = 0
   eQUANT_16   = 0
   ePRODU_17   = 0
   ePRODU_18   = 0
   ePRODU_19   = 0
   ePRODU_20   = 0
   eQUANT_01   = 0
   eQUANT_02   = 0
   eQUANT_03   = 0
   eQUANT_04   = 0
   eQUANT_05   = 0
   eQUANT_06   = 0
   eQUANT_07   = 0
   eQUANT_08   = 0
   eQUANT_09   = 0
   eQUANT_10   = 0
   eQUANT_11   = 0
   eQUANT_12   = 0
   eQUANT_13   = 0
   eQUANT_14   = 0
   eQUANT_15   = 0
   eQUANT_16   = 0
   eQUANT_17   = 0
   eQUANT_18   = 0
   eQUANT_19   = 0
   eQUANT_20   = 0
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
   ePR_21_01   = 0
   ePR_21_02   = 0
   ePR_21_03   = 0
   ePR_21_04   = 0
   ePR_21_05   = 0
   eCONDICAO   = SPACE(50)
   eENTREGA    = SPACE(20)
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
      IF ePROD <= 20 
         @ eAUX+06,02 GET &eCAMPO_1 PICT "@R 999,999"
         READ
         IF &eCAMPO_1 = 999999
            &eCAMPO_1 = 0
            EXIT
         ENDIF
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
         READ
         eAUX = eAUX + 1
         ePROD = ePROD + 1
      ELSE
         SELE 3
         USE ARQ_ITEM INDEX ITEM_DES,ARQ_ITEM
         GOTO TOP
         SAVE SCREEN
         SET COLOR TO W/N,+W/R
         DECLARE CAMPO[10],PIC[10],CAB[10],LENS[10]
         AFIELDS(CAMPO,.T.,LENS)
         CAMPO[10]="ATIVA"
         LENS[10] =1
         PIC[1] ="@R 99999"
         PIC[2] ="@R 99999"
         PIC[3] ="@S2"
         PIC[4] ="@S40"
         PIC[5] ="@S15"
         PIC[6] ="@S15"
         PIC[7] ="@R 999,999.99"
         PIC[8] ="@R 99,999,999.99"
         PIC[09]="@R 99,999,999.99"
         PIC[10]="L"
         CAB[1] ="C¢digo"
         CAB[2] ="TIPO"
         CAB[3] ="UNID."
         CAB[4] ="DESCRIá∆o"
         CAB[5] ="MARCA"
         CAB[6] ="MODELO"
         CAB[7] ="ESTOQ. MIN."
         CAB[8] ="CUSTO"
         CAB[9] ="PRECO 1"
         CAB[10]="ATIVA"
         @ 09,00 TO 17,79 DOUBLE
         PLQUA("01")
         SET COLOR TO W/N,+W/R
         DBEDIT(10,01,17,78,CAMPO,"F_TAB_ITEM",PIC,CAB,CHR(196),CHR(179),CHR(205))
         RESTORE SCREEN
         &eCAMPO_1 = CODIGO
         SELE 3
         USE ARQ_ITEM INDEX ARQ_ITEM,ITEM_DES
         GOTO TOP
         IF &eCAMPO_1 = 0 
            PLQUA("01")
            PPERG ( "Deseja Encerrar o Formulario (S/N) ? ")
            IF sisRESP = "N"
               LOOP
            ELSE
               EXIT
            ENDIF
         ENDIF
      ENDIF
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
   ENDDO
   SELE 6
   GOTO TOP
   IF .NOT. eORDE_CLAS
      eCLAS_PRO = 0
      eDESC_PRO = SPACE(30)
      DO WHILE .T.
         @ 23,02 SAY "Cod. Produto: " GET eCLAS_PRO PICT "@R 99999999"
         READ
         SET COLOR TO W/N,+W/R
         @ 23,00
         @ 23,02 SAY "Cod. Produto: "
         SAVE SCREEN TO PESQUISA
         SELE 6
         USE CLAS_PRO INDEX CLAS_PRO,DESC_PRO
         GOTO TOP
         IF eCLAS_PRO <> 0
            SEEK eCLAS_PRO
         ENDIF
         DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@S35"
         CAB[1]="C¢digo"
         CAB[2]="Tipo"
         CAB[3]="Produto"         && Era Empreendimento
         @ 8,19 TO 18,79 DOUBLE
         DBEDIT(9,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
         eCLAS_PRO = CODIGO
         eDESC_PRO = DESCRICAO
         RESTORE SCREEN FROM PESQUISA
         @ 23,00
         @ 23,02 SAY "Cod. Produto: "
         @ 23,18 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
         SELE 1
         EXIT
      ENDDO
   ELSE
      eCLAS_PRO = 0
      eDESC_PRO = SPACE(30)
      DO WHILE .T.
         @ 23,02 SAY "Cod. Produto: " GET eDESC_PRO PICT "@S30"
         READ
         eDESC_PRO = ALLTRIM(eDESC_PRO)
         SELE 6
         USE CLAS_PRO INDEX DESC_PRO,CLAS_PRO
         GOTO TOP
         IF eDESC_PRO <> SPACE(30)
            SEEK eDESC_PRO
         ENDIF
         SET COLOR TO W/N,+W/R
         @ 23,00
         @ 23,02 SAY "Cod. Produto: "
         SAVE SCREEN TO PESQUISA
         DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@S35"
         CAB[1]="C¢digo"
         CAB[2]="Tipo"
         CAB[3]="Produto"
         @ 8,19 TO 18,79 DOUBLE
         DBEDIT(9,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
         eCLAS_PRO = CODIGO
         eDESC_PRO = DESCRICAO
         RESTORE SCREEN FROM PESQUISA
         @ 23,00
         @ 23,02 SAY "Cod. Produto: "
         @ 23,18 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
         SELE 1
         EXIT
      ENDDO
   ENDIF
   PMENSAG("Pressione Qualquer Tecla Para Finalizar ...")
   PPARA()
   SELE 1
   PLQUA("01")
   PPERG ( "Confirma (S/N) ? ")
   IF sisRESP = "S"
      PMENSAG("GRAVANDO, AGUARDE ...")
      SELE 1
      APPEND BLANK
      IF F_BLOQR()
         REPLACE NUMERO     WITH ePED_COMP
         REPLACE PRODUTO_01 WITH ePRODU_01,;
                 PRODUTO_02 WITH ePRODU_02,;
                 PRODUTO_03 WITH ePRODU_03 
         REPLACE PRODUTO_04 WITH ePRODU_04,;
                 PRODUTO_05 WITH ePRODU_05,;
                 PRODUTO_06 WITH ePRODU_06
         REPLACE PRODUTO_07 WITH ePRODU_07,;
                 PRODUTO_08 WITH ePRODU_08,;
                 PRODUTO_09 WITH ePRODU_09  
         REPLACE PRODUTO_10 WITH ePRODU_10,;
                 PRODUTO_11 WITH ePRODU_11,; 
                 PRODUTO_12 WITH ePRODU_12
         REPLACE PRODUTO_13 WITH ePRODU_13,;
                 PRODUTO_14 WITH ePRODU_14,;
                 PRODUTO_15 WITH ePRODU_15
         REPLACE PRODUTO_16 WITH ePRODU_16,;
                 PRODUTO_17 WITH ePRODU_17,;
                 PRODUTO_18 WITH ePRODU_18 
         REPLACE PRODUTO_19 WITH ePRODU_19,;
                 PRODUTO_20 WITH ePRODU_20
         REPLACE QUANT_01   WITH eQUANT_01,;
                 QUANT_02   WITH eQUANT_02,;
                 QUANT_03   WITH eQUANT_03 
         REPLACE QUANT_04   WITH eQUANT_04,;
                 QUANT_05   WITH eQUANT_05,;
                 QUANT_06   WITH eQUANT_06
         REPLACE QUANT_07   WITH eQUANT_07,;
                 QUANT_08   WITH eQUANT_08,;
                 QUANT_09   WITH eQUANT_09  
         REPLACE QUANT_10   WITH eQUANT_10,;
                 QUANT_11   WITH eQUANT_11,; 
                 QUANT_12   WITH eQUANT_12
         REPLACE QUANT_13   WITH eQUANT_13,;
                 QUANT_14   WITH eQUANT_14,;
                 QUANT_15   WITH eQUANT_15
         REPLACE QUANT_16   WITH eQUANT_16,;
                 QUANT_17   WITH eQUANT_17,;
                 QUANT_18   WITH eQUANT_18 
         REPLACE QUANT_19   WITH eQUANT_19,;
                 QUANT_20   WITH eQUANT_20
         REPLACE SITUACAO   WITH "EM ABERTO",;
                 EMPREENDIM WITH eCLAS_PRO
	 UNLOCK
      ENDIF
      SELE 9
      USE STATUS
      GOTO TOP
      IF F_BLOQR()
         REPLACE PED_COMP WITH ePED_COMP + 1
	 UNLOCK
      ENDIF
      F_LACESSO(sisUSUARIO,"CADASTRO PEDIDO MATERIAL","CADASTRO No. "+ALLTRIM(STR(ePED_COMP,10))) 
      SELE 1
   ENDIF
ENDDO
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************


