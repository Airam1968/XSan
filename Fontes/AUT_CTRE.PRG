*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa	: 
* Data        : 01/01/1999
* Objetivo    : LANCAMENTOS AUTOM‡TICOS DE PAGAMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION AUT_CTRE()
SAVE SCREEN TO TELA03
PLIMPA()
PTIT( "LAN€AMENTOS AUTOMµTICOS DE RECEBIMENTOS")
SELE 1
USE UNI_IMOB INDEX UNI_IMOB
GOTO TOP
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 3
USE CAD_QUAD INDEX CAD_QUAD
GOTO TOP
SELE 4
USE CAD_APAR INDEX CAD_APAR
GOTO TOP
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SELE 6
USE CAD_VEND INDEX CAD_VEND,DATA_VEN
GOTO TOP
SELE 7
USE CAD_QUIT
INDEX ON DATA_VENC TO TEMP_QUI
GOTO TOP
SELE 8
* Cadastro de Parcelas
SELE 9
USE CRECEBER INDEX CRECEBER,DATA_CRE,DATA_CRV,DATA_NF
GOTO TOP
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 06,02 SAY "Periodo : " GET eDATA_INI
   @ 06,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv lidas !",3)
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
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
DO WHILE .T.
   eCLAS_PRO = 0
   PLQUA("01")
   @ 23,00 SAY " Produto   ou  (0)  para  Todos  : " GET eCLAS_PRO PICT "@R 9999999"
   READ
   SELE 2
   SEEK eCLAS_PRO
   IF .NOT. FOUND()
      PLQUA("01")
	  PMSGR( "  Produto   Inexistente   ",3)
      SELE 2
      LOOP
   ELSE
      @ 23,48 SAY " - "+ALLTRIM(DESCRICAO)
      SELE 2
      IF eCLAS_PRO <> 0
         SET FILTER TO CODIGO = eCLAS_PRO
      ENDIF
      GOTO TOP
      EXIT
   ENDIF
ENDDO
eTIPO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Consolida‡Æo com Previsto (I)ntegral ou (R)ealizado ? " GET eTIPO PICT "@!X" VALID eTIPO $("IR")
READ
PLQUA("01")
PMENSAG("Aguarde ...")
SELE 9
SET EXCLUSIVE ON
USE CRECEBER INDEX CRECEBER,DATA_CRE,DATA_CRV,DATA_NF
GOTO TOP
PLQUA("01")
PMENSAG("Apagando registros pre-existentes no periodo...")
IF F_BLOQA()
   IF eCLAS_PRO = 0
      DELE ALL FOR (DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. CODIGO_CLI = 1) .OR. (DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. CODIGO_CLI = 1)
      PACK
   ELSE
      DELE ALL FOR EMPREENDIM = eCLAS_PRO .AND. ((DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. CODIGO_CLI = 1) .OR. (DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. CODIGO_CLI = 1))
      PACK
   ENDIF
   UNLOCK
ENDIF
PLQUA("01")
SET EXCLUSIVE OFF
SELE 9
USE CRECEBER INDEX CRECEBER,DATA_CRE,DATA_CRV,DATA_NF
GOTO TOP
SELE 2
GOTO TOP
DO WHILE .NOT. EOF()
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   eDESC_PRO = DESCRICAO
   eCONTA    = COD_CONTA
   IF eTIPO_PRO = 1
      SELE 3
      GOTO TOP
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
   ELSE
      SELE 4
      GOTO TOP
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
   ENDIF
   eDATA_AUX = eDATA_INI
   DECLARE eTOT_REAL[1000],eTOT_PREV[1000],eSUB_PREV[1000],eSUB_REAL[1000]
   AFILL(eTOT_REAL,0)
   AFILL(eTOT_PREV,0)
   AFILL(eSUB_PREV,0)
   AFILL(eSUB_REAL,0)
   DO WHILE eDATA_AUX <= eDATA_FIM
      eNUM_QUADRA = SPACE(5)
      IF eTIPO_PRO = 1
	 SELE 3
	 GOTO TOP
	 eNUM_QUADRA = NUM_QUADRA
      ELSE
	 SELE 4
	 GOTO TOP
	 eNUM_QUADRA = NUM_BLOCO
      ENDIF
      DO WHILE .NOT. EOF()
	 SELE 8
	 eIND_CLAS = STR(eCLAS_PRO,5)
	 eIND_QUAD = eNUM_QUADRA
	 eZERO = "000"
	 eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
	 eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
	 eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
	 eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
	 USE &eARQ INDEX &eIND2,&eIND1,&eIND3
	 GOTO TOP
         SET FILTER TO (DATA_VENC = eDATA_AUX .AND. VLR_PGTO = 0) .OR. (DATA_PGTO = eDATA_AUX .AND. VLR_PGTO <> 0)
	 GOTO TOP
         @ 10,00 
         @ 10,00 SAY "   Produto     ("+ALLTRIM(eIND_CLAS)+"/"+ALLTRIM(eIND_QUAD)+") em ("+DTOC(eDATA_AUX)+")"
         SELE 8
         * Arquivo de parcelas
         DO WHILE .NOT. EOF()
            IF CONTA > 0
               @ 10,00 
               @ 10,00 SAY "   Produto     ("+ALLTRIM(CLASSE)+"/"+ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/"+STR(PARTE,1)+") em ("+DTOC(eDATA_AUX)+")"
               IF eDATA_AUX = DATA_PGTO
                  eSUB_REAL[CONTA] = eSUB_REAL[CONTA] + VLR_PGTO
               ENDIF
               IF eTIPO = "I"
                  IF eDATA_AUX = DATA_VENC
                     eSUB_PREV[CONTA] = eSUB_PREV[CONTA] + VLR_PARC
                  ENDIF
               ELSE
                  IF eDATA_AUX = DATA_VENC .AND. DATA_PGTO <> DATA_VENC
                     eSUB_PREV[CONTA] = eSUB_PREV[CONTA] + VLR_PARC
                  ENDIF
               ENDIF
            ELSE
               @ 10,00 
               @ 10,00 SAY "   Produto     ("+ALLTRIM(CLASSE)+"/"+ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/"+STR(PARTE,1)+") em ("+DTOC(eDATA_AUX)+")"
               IF eDATA_AUX = DATA_PGTO
                  eSUB_REAL[100] = eSUB_REAL[100] + VLR_PGTO
               ENDIF
               IF eTIPO = "I"
                  IF eDATA_AUX = DATA_VENC
                     eSUB_PREV[100] = eSUB_PREV[100] + VLR_PARC
                  ENDIF
               ELSE
                  IF eDATA_AUX = DATA_VENC .AND. DATA_PGTO <> DATA_VENC
                     eSUB_PREV[100] = eSUB_PREV[100] + VLR_PARC
                  ENDIF
               ENDIF
            ENDIF
	    SKIP
	 ENDDO
         * Arquivo de desistentes
         SELE 7
         SET FILTER TO (CLASSE = eIND_CLAS .AND. QUADRA = eIND_QUAD .AND. DATA_VENC = eDATA_AUX .AND. VLR_PGTO = 0) .OR. (CLASSE = eIND_CLAS .AND. QUADRA = eIND_QUAD .AND. DATA_PGTO = eDATA_AUX .AND. VLR_PGTO <> 0)
	 GOTO TOP
         DO WHILE .NOT. EOF()
            IF CONTA > 0
               @ 10,00 
               @ 10,00 SAY " Produto   Desistentes ("+ALLTRIM(CLASSE)+"/"+ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/"+STR(PARTE,1)+") em ("+DTOC(eDATA_AUX)+")"
               IF eDATA_AUX = DATA_PGTO
                  eSUB_REAL[CONTA] = eSUB_REAL[CONTA] + VLR_PGTO
               ENDIF
               IF eTIPO = "I"
                  IF eDATA_AUX = DATA_VENC
                     eSUB_PREV[CONTA] = eSUB_PREV[CONTA] + VLR_PARC
                  ENDIF
               ELSE
                  IF eDATA_AUX = DATA_VENC .AND. DATA_PGTO <> DATA_VENC
                     eSUB_PREV[CONTA] = eSUB_PREV[CONTA] + VLR_PARC
                  ENDIF
               ENDIF
            ELSE
               @ 10,00 
               @ 10,00 SAY "  Produto  Desistentes ("+ALLTRIM(CLASSE)+"/"+ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/"+STR(PARTE,1)+") em ("+DTOC(eDATA_AUX)+")"
               IF eDATA_AUX = DATA_PGTO
                  eSUB_REAL[100] = eSUB_REAL[100] + VLR_PGTO
               ENDIF
               IF eTIPO = "I"
                  IF eDATA_AUX = DATA_VENC
                     eSUB_PREV[100] = eSUB_PREV[100] + VLR_PARC
                  ENDIF
               ELSE
                  IF eDATA_AUX = DATA_VENC .AND. DATA_PGTO <> DATA_VENC
                     eSUB_PREV[100] = eSUB_PREV[100] + VLR_PARC
                  ENDIF
               ENDIF
            ENDIF
	    SKIP
	 ENDDO
         SELE 8
         eAUX = 1
         DO WHILE eAUX <= 100
            eTOT_PREV[eAUX] = eTOT_PREV[eAUX] + eSUB_PREV[eAUX]
            eTOT_REAL[eAUX] = eTOT_REAL[eAUX] + eSUB_REAL[eAUX]
            @ 12,00 CLEAR TO 18,79
            @ 12,00 SAY "Sub Total - Previs‡o  : " GET eSUB_PREV[eAUX] PICT "@R 999,999,999,999.99"
            @ 13,00 SAY "Sub Total - Realizado : " GET eSUB_REAL[eAUX] PICT "@R 999,999,999,999.99"
            @ 16,00 SAY "Total - Previs‡o      : " GET eTOT_PREV[eAUX] PICT "@R 999,999,999,999.99"
            @ 17,00 SAY "Total - Realizado     : " GET eTOT_REAL[eAUX] PICT "@R 999,999,999,999.99"
            CLEAR GETS
            eAUX = eAUX + 1
         ENDDO
         AFILL(eSUB_PREV,0)
         AFILL(eSUB_REAL,0)
	 IF eTIPO_PRO = 1
	    SELE 3
	    SKIP
	    eNUM_QUADRA = NUM_QUADRA
	 ELSE
	    SELE 4
	    SKIP
	    eNUM_QUADRA = NUM_BLOCO
	 ENDIF
      ENDDO
      eAUX = 1
      DO WHILE eAUX <= 100
         IF eTOT_PREV[eAUX] <> 0 .OR. eTOT_REAL[eAUX] <> 0 
            SELE 9
            USE STATUS
            GOTO TOP
            UNLOCK
            eNUM_LANC = LANC_CRE 
            USE CRECEBER INDEX CRECEBER,DATA_CRE,DATA_CRV,DATA_NF
            GOTO TOP
            APPEND BLANK
            IF F_BLOQR()
               REPLACE LANCAMENTO WITH eNUM_LANC,;
                       CLASSE_R   WITH 1,;
                       EMPREENDIM WITH eCLAS_PRO
               REPLACE CODIGO_CLI WITH 1,;
                       DESCRICAO  WITH "Parcelas "+eDESC_PRO,;
                       DATA_PREV  WITH eDATA_AUX
               REPLACE PREVISTO   WITH eTOT_PREV[eAUX],;
                       DATA_REAL  WITH IIF(eTOT_REAL[eAUX] = 0 ,CTOD(""),eDATA_AUX),;
                       REALIZADO  WITH eTOT_REAL[eAUX]
               REPLACE COD_CONTA  WITH IIF(eAUX < 100,eAUX,0),;
                       DATA_COMPE WITH IIF(eTOT_REAL[eAUX] = 0 ,CTOD(""),eDATA_AUX),;
                       CONTABIL   WITH .T.
               UNLOCK
            ENDIF
            USE STATUS
            GOTO TOP
            IF F_BLOQR()
               REPLACE LANC_CRE WITH eNUM_LANC + 1
               COMMIT
               UNLOCK
            ENDIF
            USE CRECEBER INDEX CRECEBER,DATA_CRE,DATA_CRV,DATA_NF
            GOTO TOP
         ENDIF
         eAUX = eAUX + 1
      ENDDO
      AFILL(eTOT_REAL,0)
      AFILL(eTOT_PREV,0)
      AFILL(eSUB_PREV,0)
      AFILL(eSUB_REAL,0)
      eDATA_AUX = eDATA_AUX + 1
   ENDDO
   SELE 2
   SKIP
ENDDO
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
