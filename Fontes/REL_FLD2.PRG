*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_FLD2.PRG
* Data        : 01/01/99
* Objetivo    : Relatorio dos Fluxo de Caixa Previsto - Diario Horiz
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_FLD2()
SAVE SCREEN TO TELA04
PTIT( "RELATORIO DE FLUXO DE CAIXA - DIARIO 2")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "N"
   RESTORE SCREEN FROM TELA04
   RETURN nil
ENDIF
SELE 7
USE BANCOS INDEX BANCOS
GOTO TOP
SELE 4
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 5
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 8
USE FORNECED INDEX FORNECED
GOTO TOP
SELE 9
USE CLIENTES INDEX CLIENTES
GOTO TOP
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,02 SAY "Periodo : " GET eDATA_INI
   @ 23,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("") .AND. MONTH(eDATA_INI) <> MONTH(eDATA_FIM)
      PLQUA("01")
      PMSGR( "Datas Inv†lidas !",3)
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
eOPCAO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Deseja Informaá‰es de (C)lasse ou (F)ornecedor ? " GET eOPCAO PICT "@!X" VALID eOPCAO $("CF")
READ
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
DO WHILE .T.
   eCLAS_PRO = 0           
   PLQUA("01")
   @ 23,00 SAY " Selecione  Produto   ou  (0)  para  Todos : " GET eCLAS_PRO PICT "@R 9999999"
   READ
   SELE 2
   SEEK eCLAS_PRO
   IF .NOT. FOUND()
      IF eCLAS_PRO <> 0
         PLQUA("01")
		 PMSGR( "   Produto   Inexistente  ",3)
         SELE 1
         LOOP
      ELSE
         @ 23,48 SAY " - Escritorio"
         SELE 1
         EXIT
      ENDIF
   ELSE
      @ 23,48 SAY " - "+ALLTRIM(DESCRICAO)
      eNOME_EMPRE = DESCRICAO
      SELE 1
      EXIT
   ENDIF
ENDDO
* Consolidacao de Saldos Normais
SET EXCLUSIVE ON
SELE 6
USE FLUXO_D2
GOTO TOP
DELE ALL 
PACK
GOTO TOP
SET EXCLUSIVE OFF
SELE 6
USE
USE FLUXO_D2
INDEX ON DATA TO FLUXO_D2
GOTO TOP
IF eCLAS_PRO = 0
   SELE 1
   USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
   SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. REALIZADO = 0
   GOTO TOP
   SELE 3
   USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
   SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. REALIZADO = 0
   GOTO TOP
ELSE
   SELE 1
   USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
   SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. REALIZADO = 0
   GOTO TOP
   SELE 3
   USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE
   SET FILTER TO EMPREENDIM = eCLAS_PRO .AND. DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. REALIZADO = 0
   GOTO TOP
ENDIF
eDATA_AUX = eDATA_INI
eSALDO_I  = 0
eAPLIC_D  = 0
eAPLIC_B  = 0
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

PLQUA("01")
@ 23,00 SAY "Informe o Saldo Inicial : " GET eSALDO_I PICT "@R 999,999,999.99"
READ
DO WHILE eDATA_AUX <= eDATA_FIM
   PLQUA("01")
   PMENSAG("Aguarde o Processamento, Dia "+DTOC(eDATA_AUX))
   SELE 3
   GOTO TOP
   SET FILTER TO DATA_PREV = eDATA_AUX .AND. CLASSE_R > 0 
   GOTO TOP
   DO WHILE .NOT. EOF()
      eVLR_REAL  = 0
      eCLASSE_R  = 0
      eDESCRICAO = SPACE(40)
      eVALOR     = PREVISTO
      eDATA      = DATA_PREV
      eCLASSE_R  = CLASSE_R
      eCLASSE_D  = 0
      eDESCRICA2 = DESCRICAO
      eEMPREEND  = EMPREENDIM
      eFORNECED  = CODIGO_CLI
      eCONTA     = COD_CONTA
      SELE 5
      SEEK eCLASSE_R
      eDESCRICAO = DESCRICAO
      SELE 6
      GOTO TOP
      IF eOPCAO = "C"
         LOCATE FOR CLASSE_R = eCLASSE_R
         IF EOF()
            APPEND BLANK
            IF F_BLOQR()
               REPLACE DATA       WITH eDATA
               REPLACE CONTA      WITH eCONTA,;
                       FORNECED   WITH eFORNECED,;
                       CLASSE_D   WITH eCLASSE_D 
               REPLACE CLASSE_R   WITH eCLASSE_R,;
                       DESCRICAO  WITH eDESCRICAO,;
                       EMPREENDIM WITH eEMPREEND
               eDIA = SUBSTR(DTOC(eDATA),1,2)
               eCAMPO = "DIA_"+eDIA
               REPLACE &eCAMPO WITH eVALOR  
               UNLOCK
            ENDIF
         ELSE
            IF F_BLOQR()
               eDIA = SUBSTR(DTOC(eDATA),1,2)
               eCAMPO = "DIA_"+eDIA
               REPLACE &eCAMPO WITH &eCAMPO+eVALOR  
               UNLOCK
            ENDIF
         ENDIF
      ELSE
         LOCATE FOR FORNECED = eFORNECED
         IF EOF()
            APPEND BLANK
            IF F_BLOQR()
               REPLACE DATA       WITH eDATA
               REPLACE CONTA      WITH eCONTA,;
                       FORNECED   WITH eFORNECED,;
                       CLASSE_D   WITH eCLASSE_D 
               REPLACE CLASSE_R   WITH eCLASSE_R,;
                       DESCRICAO  WITH eDESCRICAO,;
                       EMPREENDIM WITH eEMPREEND
               eDIA = SUBSTR(DTOC(eDATA),1,2)
               eCAMPO = "DIA_"+eDIA
               REPLACE &eCAMPO WITH eVALOR  
               UNLOCK
            ENDIF
         ELSE
            IF F_BLOQR()
               eDIA = SUBSTR(DTOC(eDATA),1,2)
               eCAMPO = "DIA_"+eDIA
               REPLACE &eCAMPO WITH &eCAMPO+eVALOR  
               UNLOCK
            ENDIF
         ENDIF
      ENDIF
      SELE 3
      SKIP
   ENDDO
   SELE 1
   GOTO TOP
   SET FILTER TO DATA_PREV = eDATA_AUX .AND. CLASSE_D > 0 
   GOTO TOP
   DO WHILE .NOT. EOF()
      eVLR_REAL  = 0
      eDESCRICAO = SPACE(40)
      eVALOR     = PREVISTO
      eDATA      = DATA_PREV
      eCLASSE_R  = 0
      eCLASSE_D  = CLASSE_D
      eDESCRICA2 = DESCRICAO
      eEMPREEND  = EMPREENDIM
      eFORNECED  = CODIGO_FOR
      eCONTA     = COD_CONTA
      SELE 5
      SEEK eCLASSE_D
      eDESCRICAO = DESCRICAO
      SELE 6
      GOTO TOP
      IF eOPCAO = "C"
         LOCATE FOR CLASSE_D = eCLASSE_D
         IF EOF()
            APPEND BLANK
            IF F_BLOQR()
               REPLACE DATA       WITH eDATA
               REPLACE CONTA      WITH eCONTA,;
                       FORNECED   WITH eFORNECED,;
                       CLASSE_D   WITH eCLASSE_D 
               REPLACE CLASSE_R   WITH eCLASSE_R,;
                       DESCRICAO  WITH eDESCRICAO,;
                       EMPREENDIM WITH eEMPREEND
               eDIA = SUBSTR(DTOC(eDATA),1,2)
               eCAMPO = "DIA_"+eDIA
               REPLACE &eCAMPO WITH eVALOR  
               UNLOCK
            ENDIF
         ELSE
            IF F_BLOQR()
               eDIA = SUBSTR(DTOC(eDATA),1,2)
               eCAMPO = "DIA_"+eDIA
               REPLACE &eCAMPO WITH &eCAMPO+eVALOR  
               UNLOCK
            ENDIF
         ENDIF
      ELSE
         LOCATE FOR FORNECED = eFORNECED
         IF EOF()
            APPEND BLANK
            IF F_BLOQR()
               REPLACE DATA       WITH eDATA
               REPLACE CONTA      WITH eCONTA,;
                       FORNECED   WITH eFORNECED,;
                       CLASSE_D   WITH eCLASSE_D 
               REPLACE CLASSE_R   WITH eCLASSE_R,;
                       DESCRICAO  WITH eDESCRICAO,;
                       EMPREENDIM WITH eEMPREEND
               eDIA = SUBSTR(DTOC(eDATA),1,2)
               eCAMPO = "DIA_"+eDIA
               REPLACE &eCAMPO WITH eVALOR  
               UNLOCK
            ENDIF
         ELSE
            IF F_BLOQR()
               eDIA = SUBSTR(DTOC(eDATA),1,2)
               eCAMPO = "DIA_"+eDIA
               REPLACE &eCAMPO WITH &eCAMPO+eVALOR  
               UNLOCK
            ENDIF
         ENDIF
      ENDIF
      SELE 1
      SKIP
   ENDDO
   eDATA_AUX = eDATA_AUX + 1
ENDDO
SELE 6
GOTO TOP
SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM
GOTO TOP
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
eDATA_AUX = DATA
eTOT_DIA01 = 0
eTOT_DIA02 = 0
eTOT_DIA03 = 0
eTOT_DIA04 = 0
eTOT_DIA05 = 0
eTOT_DIA06 = 0
eTOT_DIA07 = 0
eTOT_DIA08 = 0
eTOT_DIA09 = 0
eTOT_DIA10 = 0
eTOT_DIA11 = 0
eTOT_DIA12 = 0
eTOT_DIA13 = 0
eTOT_DIA14 = 0
eTOT_DIA15 = 0
eTOT_DIA16 = 0
eTOT_DIA17 = 0
eTOT_DIA18 = 0
eTOT_DIA19 = 0
eTOT_DIA20 = 0
eTOT_DIA21 = 0
eTOT_DIA22 = 0
eTOT_DIA23 = 0
eTOT_DIA24 = 0
eTOT_DIA25 = 0
eTOT_DIA26 = 0
eTOT_DIA27 = 0
eTOT_DIA28 = 0
eTOT_DIA29 = 0
eTOT_DIA30 = 0
eTOT_DIA31 = 0
eTOT_REC01 = 0
eTOT_REC02 = 0
eTOT_REC03 = 0
eTOT_REC04 = 0
eTOT_REC05 = 0
eTOT_REC06 = 0
eTOT_REC07 = 0
eTOT_REC08 = 0
eTOT_REC09 = 0
eTOT_REC10 = 0
eTOT_REC11 = 0
eTOT_REC12 = 0
eTOT_REC13 = 0
eTOT_REC14 = 0
eTOT_REC15 = 0
eTOT_REC16 = 0
eTOT_REC17 = 0
eTOT_REC18 = 0
eTOT_REC19 = 0
eTOT_REC20 = 0
eTOT_REC21 = 0
eTOT_REC22 = 0
eTOT_REC23 = 0
eTOT_REC24 = 0
eTOT_REC25 = 0
eTOT_REC26 = 0
eTOT_REC27 = 0
eTOT_REC28 = 0
eTOT_REC29 = 0
eTOT_REC30 = 0
eTOT_REC31 = 0
eTOT_DES01 = 0
eTOT_DES02 = 0
eTOT_DES03 = 0
eTOT_DES04 = 0
eTOT_DES05 = 0
eTOT_DES06 = 0
eTOT_DES07 = 0
eTOT_DES08 = 0
eTOT_DES09 = 0
eTOT_DES10 = 0
eTOT_DES11 = 0
eTOT_DES12 = 0
eTOT_DES13 = 0
eTOT_DES14 = 0
eTOT_DES15 = 0
eTOT_DES16 = 0
eTOT_DES17 = 0
eTOT_DES18 = 0
eTOT_DES19 = 0
eTOT_DES20 = 0
eTOT_DES21 = 0
eTOT_DES22 = 0
eTOT_DES23 = 0
eTOT_DES24 = 0
eTOT_DES25 = 0
eTOT_DES26 = 0
eTOT_DES27 = 0
eTOT_DES28 = 0
eTOT_DES29 = 0
eTOT_DES30 = 0
eTOT_DES31 = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,05  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO DIARIO DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 04,00  SAY "Produto :"+IIF(eCLAS_PRO = 0,"Todos",eNOME_EMPRE)
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "CLASSE"
   @ 06,22  SAY "EMPR."
   @ 06,35  SAY "01"
   @ 06,45  SAY "02"
   @ 06,55  SAY "03"
   @ 06,65  SAY "04"
   @ 06,75  SAY "05"
   @ 06,85  SAY "06"
   @ 06,95  SAY "07"
   @ 06,105 SAY "08"
   @ 06,115 SAY "09"
   @ 06,125 SAY "10"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF eOPCAO = "C"
         IF CLASSE_R <> 0
            SELE 6
            @ LIN,00  SAY CLASSE_R PICT "@R 9999999"
            eCLASSE = CLASSE_R
            SELE 5
            SEEK eCLASSE
            @ LIN,08 SAY SUBSTR(DESCRICAO,1,16)
            @ LIN,08 SAY SUBSTR(DESCRICAO,1,16)
            SELE 6
         ELSE
            SELE 6
            @ LIN,00  SAY CLASSE_D PICT "@R 9999999"
            eCLASSE = CLASSE_D
            SELE 4
            SEEK eCLASSE
            @ LIN,08 SAY SUBSTR(DESCRICAO,1,16)
            SELE 6
         ENDIF
      ELSE
         IF CLASSE_R <> 0
            SELE 6
            @ LIN,00  SAY FORNECED PICT "@R 9999999"
            eFORNECED = FORNECED
            SELE 9
            SEEK eFORNECED
            @ LIN,08 SAY SUBSTR(NOME,1,16)
            SELE 6
         ELSE
            SELE 6
            @ LIN,00  SAY FORNECED PICT "@R 9999999"
            eFORNECED = FORNECED
            SELE 8
            SEEK eFORNECED
            @ LIN,08 SAY SUBSTR(NOME,1,16)
            SELE 6
         ENDIF
      ENDIF
      @ LIN,25  SAY EMPREENDIM PICT "9999"
      @ LIN,30  SAY DIA_01     PICT "@R 9,999,999"
      @ LIN,40  SAY DIA_02     PICT "@R 9,999,999"
      @ LIN,50  SAY DIA_03     PICT "@R 9,999,999"
      @ LIN,60  SAY DIA_04     PICT "@R 9,999,999"
      @ LIN,70  SAY DIA_05     PICT "@R 9,999,999"
      @ LIN,80  SAY DIA_06     PICT "@R 9,999,999"
      @ LIN,90  SAY DIA_07     PICT "@R 9,999,999"
      @ LIN,100 SAY DIA_08     PICT "@R 9,999,999"
      @ LIN,110 SAY DIA_09     PICT "@R 9,999,999"
      @ LIN,120 SAY DIA_10     PICT "@R 9,999,999"
      LIN = LIN + 1
      * Totalizando Despesas
      eTOT_DES01 = eTOT_DES01 + IIF(DIA_01 < 0,DIA_01,0)
      eTOT_DES02 = eTOT_DES02 + IIF(DIA_02 < 0,DIA_02,0)
      eTOT_DES03 = eTOT_DES03 + IIF(DIA_03 < 0,DIA_03,0)
      eTOT_DES04 = eTOT_DES04 + IIF(DIA_04 < 0,DIA_04,0)
      eTOT_DES05 = eTOT_DES05 + IIF(DIA_05 < 0,DIA_05,0)
      eTOT_DES06 = eTOT_DES06 + IIF(DIA_06 < 0,DIA_06,0)
      eTOT_DES07 = eTOT_DES07 + IIF(DIA_07 < 0,DIA_07,0)
      eTOT_DES08 = eTOT_DES08 + IIF(DIA_08 < 0,DIA_08,0)
      eTOT_DES09 = eTOT_DES09 + IIF(DIA_09 < 0,DIA_09,0)
      eTOT_DES10 = eTOT_DES10 + IIF(DIA_10 < 0,DIA_10,0)
      * Totalizando Receitas
      eTOT_REC01 = eTOT_REC01 + IIF(DIA_01 > 0,DIA_01,0)
      eTOT_REC02 = eTOT_REC02 + IIF(DIA_02 > 0,DIA_02,0)
      eTOT_REC03 = eTOT_REC03 + IIF(DIA_03 > 0,DIA_03,0)
      eTOT_REC04 = eTOT_REC04 + IIF(DIA_04 > 0,DIA_04,0)
      eTOT_REC05 = eTOT_REC05 + IIF(DIA_05 > 0,DIA_05,0)
      eTOT_REC06 = eTOT_REC06 + IIF(DIA_06 > 0,DIA_06,0)
      eTOT_REC07 = eTOT_REC07 + IIF(DIA_07 > 0,DIA_07,0)
      eTOT_REC08 = eTOT_REC08 + IIF(DIA_08 > 0,DIA_08,0)
      eTOT_REC09 = eTOT_REC09 + IIF(DIA_09 > 0,DIA_09,0)
      eTOT_REC10 = eTOT_REC10 + IIF(DIA_10 > 0,DIA_10,0)
      * Total Geral
      eTOT_DIA01 = eTOT_DIA01 + DIA_01
      eTOT_DIA02 = eTOT_DIA02 + DIA_02
      eTOT_DIA03 = eTOT_DIA03 + DIA_03
      eTOT_DIA04 = eTOT_DIA04 + DIA_04
      eTOT_DIA05 = eTOT_DIA05 + DIA_05
      eTOT_DIA06 = eTOT_DIA06 + DIA_06
      eTOT_DIA07 = eTOT_DIA07 + DIA_07
      eTOT_DIA08 = eTOT_DIA08 + DIA_08
      eTOT_DIA09 = eTOT_DIA09 + DIA_09
      eTOT_DIA10 = eTOT_DIA10 + DIA_10
            SKIP
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
      @ LIN,0  SAY REPLICATE("_",132)
      LIN = LIN + 1
      @ LIN,02  SAY "SubTotal: "
      @ LIN,30  SAY eTOT_DIA01    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_DIA02    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_DIA03    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_DIA04    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_DIA05    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_DIA06    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_DIA07    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_DIA08    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_DIA09    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_DIA10    PICT "@R 9,999,999"
      LIN = LIN + 2
      @ LIN,02  SAY "Despesas: "
      @ LIN,30  SAY eTOT_DES01    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_DES02    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_DES03    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_DES04    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_DES05    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_DES06    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_DES07    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_DES08    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_DES09    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_DES10    PICT "@R 9,999,999"
      LIN = LIN + 1
      @ LIN,02  SAY "Receitas: "
      @ LIN,30  SAY eTOT_REC01    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_REC02    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_REC03    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_REC04    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_REC05    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_REC06    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_REC07    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_REC08    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_REC09    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_REC10    PICT "@R 9,999,999"
      LIN = LIN + 1
      @ LIN,02  SAY "Total:    "
      @ LIN,30  SAY eTOT_DIA01    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_DIA02    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_DIA03    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_DIA04    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_DIA05    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_DIA06    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_DIA07    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_DIA08    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_DIA09    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_DIA10    PICT "@R 9,999,999"
      LIN = LIN + 2
   ENDIF
   @ 56,00 SAY REPLICATE("=",135)
   @ 57,95 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
GOTO TOP
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,05  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO DIARIO DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "DATA"
   @ 06,10  SAY "CLASSIFICACAO"
   @ 06,35  SAY "11"
   @ 06,45  SAY "12"
   @ 06,55  SAY "13"
   @ 06,65  SAY "14"
   @ 06,75  SAY "15"
   @ 06,85  SAY "16"
   @ 06,95  SAY "17"
   @ 06,105 SAY "18"
   @ 06,115 SAY "19"
   @ 06,125 SAY "20"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF eOPCAO = "C"
         IF CLASSE_R <> 0
            SELE 6
            @ LIN,00  SAY CLASSE_R PICT "@R 99999"
            eCLASSE = CLASSE_R
            SELE 5
            SEEK eCLASSE
            @ LIN,06 SAY SUBSTR(DESCRICAO,1,18)
            @ LIN,06 SAY SUBSTR(DESCRICAO,1,18)
            SELE 6
         ELSE
            SELE 6
            @ LIN,00  SAY CLASSE_D PICT "@R 99999"
            eCLASSE = CLASSE_D
            SELE 4
            SEEK eCLASSE
            @ LIN,06 SAY SUBSTR(DESCRICAO,1,18)
            SELE 6
         ENDIF
      ELSE
         IF CLASSE_R <> 0
            SELE 6
            @ LIN,00  SAY FORNECED PICT "@R 99999"
            eFORNECED = FORNECED
            SELE 9
            SEEK eFORNECED
            @ LIN,06 SAY SUBSTR(NOME,1,18)
            SELE 6
         ELSE
            SELE 6
            @ LIN,00  SAY FORNECED PICT "@R 99999"
            eFORNECED = FORNECED
            SELE 8
            SEEK eFORNECED
            @ LIN,06 SAY SUBSTR(NOME,1,18)
            SELE 6
         ENDIF
      ENDIF
      @ LIN,25  SAY EMPREENDIM PICT "9999"
      @ LIN,30  SAY DIA_11     PICT "@R 9,999,999"
      @ LIN,40  SAY DIA_12     PICT "@R 9,999,999"
      @ LIN,50  SAY DIA_13     PICT "@R 9,999,999"
      @ LIN,60  SAY DIA_14     PICT "@R 9,999,999"
      @ LIN,70  SAY DIA_15     PICT "@R 9,999,999"
      @ LIN,80  SAY DIA_16     PICT "@R 9,999,999"
      @ LIN,90  SAY DIA_17     PICT "@R 9,999,999"
      @ LIN,100 SAY DIA_18     PICT "@R 9,999,999"
      @ LIN,110 SAY DIA_19     PICT "@R 9,999,999"
      @ LIN,120 SAY DIA_20     PICT "@R 9,999,999"
      LIN = LIN + 1
      * Totalizando Despesas
      eTOT_DES11 = eTOT_DES11 + IIF(DIA_11 < 0,DIA_11,0)
      eTOT_DES12 = eTOT_DES12 + IIF(DIA_12 < 0,DIA_12,0)
      eTOT_DES13 = eTOT_DES13 + IIF(DIA_13 < 0,DIA_13,0)
      eTOT_DES14 = eTOT_DES14 + IIF(DIA_14 < 0,DIA_14,0)
      eTOT_DES15 = eTOT_DES15 + IIF(DIA_15 < 0,DIA_15,0)
      eTOT_DES16 = eTOT_DES16 + IIF(DIA_16 < 0,DIA_16,0)
      eTOT_DES17 = eTOT_DES17 + IIF(DIA_17 < 0,DIA_17,0)
      eTOT_DES18 = eTOT_DES18 + IIF(DIA_18 < 0,DIA_18,0)
      eTOT_DES19 = eTOT_DES19 + IIF(DIA_19 < 0,DIA_19,0)
      eTOT_DES20 = eTOT_DES20 + IIF(DIA_20 < 0,DIA_20,0)
      * Totalizando Receitas
      eTOT_REC11 = eTOT_REC11 + IIF(DIA_11 > 0,DIA_11,0)
      eTOT_REC12 = eTOT_REC12 + IIF(DIA_12 > 0,DIA_12,0)
      eTOT_REC13 = eTOT_REC13 + IIF(DIA_13 > 0,DIA_13,0)
      eTOT_REC14 = eTOT_REC14 + IIF(DIA_14 > 0,DIA_14,0)
      eTOT_REC15 = eTOT_REC15 + IIF(DIA_15 > 0,DIA_15,0)
      eTOT_REC16 = eTOT_REC16 + IIF(DIA_16 > 0,DIA_16,0)
      eTOT_REC17 = eTOT_REC17 + IIF(DIA_17 > 0,DIA_17,0)
      eTOT_REC18 = eTOT_REC18 + IIF(DIA_18 > 0,DIA_18,0)
      eTOT_REC19 = eTOT_REC19 + IIF(DIA_19 > 0,DIA_19,0)
      eTOT_REC20 = eTOT_REC20 + IIF(DIA_20 > 0,DIA_20,0)
      * Total Geral
      eTOT_DIA11 = eTOT_DIA11 + DIA_11
      eTOT_DIA12 = eTOT_DIA12 + DIA_12
      eTOT_DIA13 = eTOT_DIA13 + DIA_13
      eTOT_DIA14 = eTOT_DIA14 + DIA_14
      eTOT_DIA15 = eTOT_DIA15 + DIA_15
      eTOT_DIA16 = eTOT_DIA16 + DIA_16
      eTOT_DIA17 = eTOT_DIA17 + DIA_17
      eTOT_DIA18 = eTOT_DIA18 + DIA_18
      eTOT_DIA19 = eTOT_DIA19 + DIA_19
      eTOT_DIA20 = eTOT_DIA20 + DIA_20
            SKIP
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
      @ LIN,00  SAY REPLICATE("_",135)
      LIN = LIN + 1
      @ LIN,02  SAY "SubTotal: "
      @ LIN,30  SAY eTOT_DIA11    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_DIA12    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_DIA13    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_DIA14    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_DIA15    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_DIA16    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_DIA17    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_DIA18    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_DIA19    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_DIA20    PICT "@R 9,999,999"
      LIN = LIN + 2
      @ LIN,02  SAY "Despesas: "
      @ LIN,30  SAY eTOT_DES11    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_DES12    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_DES13    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_DES14    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_DES15    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_DES16    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_DES17    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_DES18    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_DES19    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_DES20    PICT "@R 9,999,999"
      LIN = LIN + 1
      @ LIN,02  SAY "Receitas: "
      @ LIN,30  SAY eTOT_REC11    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_REC12    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_REC13    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_REC14    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_REC15    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_REC16    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_REC17    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_REC18    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_REC19    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_REC20    PICT "@R 9,999,999"
      LIN = LIN + 1
      @ LIN,02  SAY "Total:    "
      @ LIN,30  SAY eTOT_DIA11    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_DIA12    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_DIA13    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_DIA14    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_DIA15    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_DIA16    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_DIA17    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_DIA18    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_DIA19    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_DIA20    PICT "@R 9,999,999"
      LIN = LIN + 2
   ENDIF
   @ 56,00 SAY REPLICATE("=",135)
   @ 57,95 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
GOTO TOP
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,05  SAY sisLETRAE+"FLUXO DE CAIXA PREVISTO DIARIO DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "DATA"
   @ 06,10  SAY "CLASSIFICACAO"
   @ 06,35  SAY "21"
   @ 06,45  SAY "22"
   @ 06,55  SAY "23"
   @ 06,65  SAY "24"
   @ 06,75  SAY "25"
   @ 06,85  SAY "26"
   @ 06,95  SAY "27"
   @ 06,105 SAY "28"
   @ 06,115 SAY "29"
   @ 06,125 SAY "30"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF eOPCAO = "C"
         IF CLASSE_R <> 0
            SELE 6
            @ LIN,00  SAY CLASSE_R PICT "@R 99999"
            eCLASSE = CLASSE_R
            SELE 5
            SEEK eCLASSE
            @ LIN,06 SAY SUBSTR(DESCRICAO,1,18)
            @ LIN,06 SAY SUBSTR(DESCRICAO,1,18)
            SELE 6
         ELSE
            SELE 6
            @ LIN,00  SAY CLASSE_D PICT "@R 99999"
            eCLASSE = CLASSE_D
            SELE 4
            SEEK eCLASSE
            @ LIN,06 SAY SUBSTR(DESCRICAO,1,18)
            SELE 6
         ENDIF
      ELSE
         IF CLASSE_R <> 0
            SELE 6
            @ LIN,00  SAY FORNECED PICT "@R 99999"
            eFORNECED = FORNECED
            SELE 9
            SEEK eFORNECED
            @ LIN,06 SAY SUBSTR(NOME,1,18)
            SELE 6
         ELSE
            SELE 6
            @ LIN,00  SAY FORNECED PICT "@R 99999"
            eFORNECED = FORNECED
            SELE 8
            SEEK eFORNECED
            @ LIN,06 SAY SUBSTR(NOME,1,18)
            SELE 6
         ENDIF
      ENDIF
      @ LIN,25  SAY EMPREENDIM PICT "9999"
      @ LIN,30  SAY DIA_21     PICT "@R 9,999,999"
      @ LIN,40  SAY DIA_22     PICT "@R 9,999,999"
      @ LIN,50  SAY DIA_23     PICT "@R 9,999,999"
      @ LIN,60  SAY DIA_24     PICT "@R 9,999,999"
      @ LIN,70  SAY DIA_25     PICT "@R 9,999,999"
      @ LIN,80  SAY DIA_26     PICT "@R 9,999,999"
      @ LIN,90  SAY DIA_27     PICT "@R 9,999,999"
      @ LIN,100 SAY DIA_28     PICT "@R 9,999,999"
      @ LIN,110 SAY DIA_29     PICT "@R 9,999,999"
      @ LIN,120 SAY DIA_30     PICT "@R 9,999,999"
      * Totalizando Despesas
      eTOT_DES21 = eTOT_DES21 + IIF(DIA_21 < 0,DIA_21,0)
      eTOT_DES22 = eTOT_DES22 + IIF(DIA_22 < 0,DIA_22,0)
      eTOT_DES23 = eTOT_DES23 + IIF(DIA_23 < 0,DIA_23,0)
      eTOT_DES24 = eTOT_DES24 + IIF(DIA_24 < 0,DIA_24,0)
      eTOT_DES25 = eTOT_DES25 + IIF(DIA_25 < 0,DIA_25,0)
      eTOT_DES26 = eTOT_DES26 + IIF(DIA_26 < 0,DIA_26,0)
      eTOT_DES27 = eTOT_DES27 + IIF(DIA_27 < 0,DIA_27,0)
      eTOT_DES28 = eTOT_DES28 + IIF(DIA_28 < 0,DIA_28,0)
      eTOT_DES29 = eTOT_DES29 + IIF(DIA_29 < 0,DIA_29,0)
      eTOT_DES30 = eTOT_DES30 + IIF(DIA_30 < 0,DIA_30,0)
      * Totalizando Receitas
      eTOT_REC21 = eTOT_REC21 + IIF(DIA_21 > 0,DIA_21,0)
      eTOT_REC22 = eTOT_REC22 + IIF(DIA_22 > 0,DIA_22,0)
      eTOT_REC23 = eTOT_REC23 + IIF(DIA_23 > 0,DIA_23,0)
      eTOT_REC24 = eTOT_REC24 + IIF(DIA_24 > 0,DIA_24,0)
      eTOT_REC25 = eTOT_REC25 + IIF(DIA_25 > 0,DIA_25,0)
      eTOT_REC26 = eTOT_REC26 + IIF(DIA_26 > 0,DIA_26,0)
      eTOT_REC27 = eTOT_REC27 + IIF(DIA_27 > 0,DIA_27,0)
      eTOT_REC28 = eTOT_REC28 + IIF(DIA_28 > 0,DIA_28,0)
      eTOT_REC29 = eTOT_REC29 + IIF(DIA_29 > 0,DIA_29,0)
      eTOT_REC30 = eTOT_REC30 + IIF(DIA_30 > 0,DIA_30,0)
      * Total Geral
      eTOT_DIA21 = eTOT_DIA21 + DIA_21
      eTOT_DIA22 = eTOT_DIA22 + DIA_22
      eTOT_DIA23 = eTOT_DIA23 + DIA_23
      eTOT_DIA24 = eTOT_DIA24 + DIA_24
      eTOT_DIA25 = eTOT_DIA25 + DIA_25
      eTOT_DIA26 = eTOT_DIA26 + DIA_26
      eTOT_DIA27 = eTOT_DIA27 + DIA_27
      eTOT_DIA28 = eTOT_DIA28 + DIA_28
      eTOT_DIA29 = eTOT_DIA29 + DIA_29
      eTOT_DIA30 = eTOT_DIA30 + DIA_30
      LIN = LIN + 1
            SKIP
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
      @ LIN,00  SAY REPLICATE("_",135)
      LIN = LIN + 1
      @ LIN,02  SAY "SubTotal: "
      @ LIN,30  SAY eTOT_DIA21    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_DIA22    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_DIA23    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_DIA24    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_DIA25    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_DIA26    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_DIA27    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_DIA28    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_DIA29    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_DIA30    PICT "@R 9,999,999"
      LIN = LIN + 2
      @ LIN,02  SAY "Despesas: "
      @ LIN,30  SAY eTOT_DES21    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_DES22    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_DES23    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_DES24    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_DES25    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_DES26    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_DES27    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_DES28    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_DES29    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_DES30    PICT "@R 9,999,999"
      LIN = LIN + 1
      @ LIN,02  SAY "Receitas: "
      @ LIN,30  SAY eTOT_REC21    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_REC22    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_REC23    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_REC24    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_REC25    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_REC26    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_REC27    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_REC28    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_REC29    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_REC30    PICT "@R 9,999,999"
      LIN = LIN + 1
      @ LIN,02  SAY "Total:    "
      @ LIN,30  SAY eTOT_DIA21    PICT "@R 9,999,999"
      @ LIN,40  SAY eTOT_DIA22    PICT "@R 9,999,999"
      @ LIN,50  SAY eTOT_DIA23    PICT "@R 9,999,999"
      @ LIN,60  SAY eTOT_DIA24    PICT "@R 9,999,999"
      @ LIN,70  SAY eTOT_DIA25    PICT "@R 9,999,999"
      @ LIN,80  SAY eTOT_DIA26    PICT "@R 9,999,999"
      @ LIN,90  SAY eTOT_DIA27    PICT "@R 9,999,999"
      @ LIN,100 SAY eTOT_DIA28    PICT "@R 9,999,999"
      @ LIN,110 SAY eTOT_DIA29    PICT "@R 9,999,999"
      @ LIN,120 SAY eTOT_DIA30    PICT "@R 9,999,999"
      LIN = LIN + 2
   ENDIF
   @ 56,00 SAY REPLICATE("=",135)
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
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
