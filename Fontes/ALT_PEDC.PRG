*********************************************************************
SAVE SCREEN TO TELA03
PTIT( "MANUTEN€ÇO DE PEDIDO DE MATERIAL")
SELECT 6
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
   ePED_COMP = 0
   SELE 1
   PLIMPA()
   SET COLOR TO W/N,+W/R
   @ 05,02 SAY "Cota‡Æo     : " GET ePED_COMP PICT "@R 999,999"
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
	  PMSGR( "C¢digo NÆo existe",3)
      LOOP
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
   eCONDICAO   = SPACE(50)
   eENTREGA    = SPACE(20)
   eCLASSE_PRO = EMPREENDIM
   eFORN = .T.
   @ 06,01 CLEAR TO 21,79
   SET COLOR TO W/N+
   @ 05,35 SAY "CADASTRO DE PRODUTOS"
   SET COLOR TO W/N,+W/R
   @ 06,02 SAY "Codigo"
   @ 06,10 TO 21,10
   @ 06,12 SAY "Descri‡Æo"
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
         READ
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
            PMSGR( "Aten‡Æo !",3)
            PMENSAG("Produto NÆo Localizado, repita a Opera‡Æo ...")
            PPARA()
            LOOP
         ENDIF
         @ eAUX+06,32 GET &eCAMPO_2 PICT "@R 99,999.99"
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
            @ 06,12 SAY "Descri‡Æo"
            @ 06,30 TO 21,30
            @ 06,32 SAY "Quant."
            @ 06,43 TO 21,43
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
   SELE 6
   GOTO TOP
   DO WHILE .T.
      @ 23,02 SAY "Cod. Produto: " GET eCLASSE_PRO PICT "@R 99999999"
      READ
      SEEK eCLASSE_PRO
      IF FOUND()
         @ 23,28 SAY " - "+ALLTRIM(DESCRICAO)
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
   PMENSAG("Pressione Qualquer Tecla Para Finalizar ...")
   PPARA()
   SELE 1
   PLQUA("01")
   PPERG ( "Confirma Altera‡Æo (S/N) ? ")
   IF sisRESP = "S"
      PMENSAG("GRAVANDO, AGUARDE ...")
      SELE 1
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
                 QUANT_20   WITH eQUANT_20,;
                 EMPREENDIM WITH eCLASSE_PRO
	 UNLOCK
      ENDIF
      F_LACESSO(sisUSUARIO,"ALTERACAO PEDIDO MATERIAL","ALTERACAO No. "+ALLTRIM(STR(ePED_COMP,10))) 
      SELE 1
   ENDIF
ENDDO
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************


