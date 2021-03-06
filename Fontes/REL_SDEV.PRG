*********************************************************************
* Sistema     : Gest�o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_SDEV.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE SALDO DEVEDOR
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_SDEV()
SAVE SCREEN TO TELA03
PTIT( "RELAT�RIO DE SALDO DEVEDOR")
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
SELE 8
* Cadastro de Parcelas
SELE 9
USE MSG_BOLE INDEX MSG_PROD,MSG_BOLE
GOTO TOP
SELE 2
* Selecao do produto
DO WHILE .T.
   PLIMPA()
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   @ 05,02 SAY "Produto : "
   GO TOP
   DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@R99999"
   PIC[2]="@R99999"
   PIC[3]="@S40"
   CAB[1]="    "
   CAB[2]="    "
   CAB[3]="    "
   DBEDIT(5,15,5,70,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   eDESC_PRO = DESCRICAO
   EXIT
ENDDO
eCLAS_PRO = STR(eCLAS_PRO,5)
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
eDATA_INI   = DATE()
eDATA_FIM   = CTOD("31/12/2999")
DO WHILE .T.
   @ 08,02 SAY "Periodo : " GET eDATA_INI
   @ 08,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv�lidas !",3)
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
PLQUA("01")
PPERG ( "Deseja Efetuar a Consolida��o do Arquivo de Saldo (S/N) ? ")
IF sisRESP = "S"
   eTIPO = SPACE(1)
   PLQUA("01")
   @ 23,00 SAY "Relat�rio (G)eral ou apenas (P)eriodo Informado ? " GET eTIPO PICT "@!X" VALID eTIPO $("GP")
   READ
   PLQUA("01")
   PMSGR( "Aten��o !",3)
   PMENSAG("Aguarde a montagem do Banco de Dados para Impress�o ...")
   IF eTIPO_PRO = 1
      eTIPOREL = SPACE(1)
      PLQUA("01")
      @ 23,00 SAY "Relat�rio (T)odos, (I)nteiros ou (M)eios Lotes ? " GET eTIPOREL PICT "@!X" VALID eTIPOREL $("TIM")
      READ
      SELE 8
      SET EXCLUSIVE ON
      USE TEMP_PAR
      IF F_BLOQA()
         ZAP
         UNLOCK
      ENDIF
      SET EXCLUSIVE OFF
      SELE 3
      SET FILTER TO NUM_LOTEAM = eCLAS_PRO .AND. CADASTRO 
      GOTO TOP
      DO WHILE .NOT. EOF()
         eIND_CLAS = eCLAS_PRO
         eIND_QUAD = NUM_QUADRA
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         IF eTIPOREL = "T"
            IF eTIPO = "G"
               APPEND FROM &eARQ FOR ((DATA_VENC >= eDATA_INI .AND. DATA_PGTO = CTOD("")) .OR. DATA_PGTO > eDATA_FIM) .AND. ATIVA .AND. CONTABIL
            ELSE
               APPEND FROM &eARQ FOR ((DATA_VENC >= eDATA_INI .AND. DATA_PGTO = CTOD("")) .OR. DATA_PGTO > eDATA_FIM) .AND. ATIVA .AND. CONTABIL .AND. DATA_VENC <= eDATA_FIM
            ENDIF
         ELSE
            IF eTIPOREL = "I"
               IF eTIPO = "G"
                  APPEND FROM &eARQ FOR ((DATA_VENC >= eDATA_INI .AND. DATA_PGTO = CTOD("")) .OR. DATA_PGTO > eDATA_FIM) .AND. ATIVA .AND. CONTABIL .AND. PARTE = 0
               ELSE
                  APPEND FROM &eARQ FOR ((DATA_VENC >= eDATA_INI .AND. DATA_PGTO = CTOD("")) .OR. DATA_PGTO > eDATA_FIM) .AND. ATIVA .AND. CONTABIL .AND. DATA_VENC <= eDATA_FIM .AND. PARTE = 0
               ENDIF
            ELSE
               IF eTIPO = "G"
                  APPEND FROM &eARQ FOR ((DATA_VENC >= eDATA_INI .AND. DATA_PGTO = CTOD("")) .OR. DATA_PGTO > eDATA_FIM) .AND. ATIVA .AND. CONTABIL .AND. PARTE <> 0
               ELSE
                  APPEND FROM &eARQ FOR ((DATA_VENC >= eDATA_INI .AND. DATA_PGTO = CTOD("")) .OR. DATA_PGTO > eDATA_FIM) .AND. ATIVA .AND. CONTABIL .AND. DATA_VENC <= eDATA_FIM .AND. PARTE <> 0
               ENDIF
            ENDIF
         ENDIF
         GOTO TOP
         SELE 3
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
      GOTO TOP
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
      SET EXCLUSIVE OFF
   ELSE
      SELE 8
      SET EXCLUSIVE ON
      USE TEMP_PAR
      IF F_BLOQA()
         ZAP
         UNLOCK
      ENDIF
      SET EXCLUSIVE OFF
      SELE 4
      SET FILTER TO NUM_LOTEAM = eCLAS_PRO .AND. CADASTRO 
      GOTO TOP
      DO WHILE .NOT. EOF()
         eIND_CLAS = eCLAS_PRO
         eIND_QUAD = NUM_BLOCO
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         IF eTIPO = "G"
            APPEND FROM &eARQ FOR ((DATA_VENC >= eDATA_INI .AND. DATA_PGTO = CTOD("")) .OR. DATA_PGTO > eDATA_FIM) .AND. ATIVA .AND. CONTABIL  && .AND. DATA_VENC <= eDATA_FIM
         ELSE
            APPEND FROM &eARQ FOR ((DATA_VENC >= eDATA_INI .AND. DATA_PGTO = CTOD("")) .OR. DATA_PGTO > eDATA_FIM) .AND. ATIVA .AND. CONTABIL .AND. DATA_VENC <= eDATA_FIM
         ENDIF
         GOTO TOP
         SELE 4
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
      GOTO TOP
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
      SET EXCLUSIVE OFF
   ENDIF
ENDIF
eCONVERT = .F.
PLQUA("01")
PPERG ( "Converter Saldos Anteriores a 01/07/1994 em Real (R$) (S/N) ? ")
IF sisRESP = "S"
   eCONVERT = .T.
ENDIF
eTOTAIS = .F.
PLQUA("01")
PPERG ( "Deseja Relatorio apenas com Totais (S/N) ? ")
IF sisRESP = "S"
   eTOTAIS = .T.
ENDIF
eCLAS_PRO = SUBSTR(eCLAS_PRO,5)
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"SALDODEVEDOR.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "SALDODEVEDOR.TXT"+SPACE(10) 
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
      PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
      PPARA()
      DO PIMPR
      eSAIDA = "IMPRESSORA"
      EXIT
   ENDIF
ENDDO
SELE 8
IF F_BLOQA()
   PACK
   UNLOCK
ENDIF
GOTO TOP
IF EOF()
   PLQUA("01")
   PMSGR( "Aten��o !",3)
   PMENSAG("N�o H� Devedores Neste Per�odo !")
   PPARA()
ELSE
   eCONTRATO = 0
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
   eSUB_SD = 0
   eSUB_PA = 0
   eTOT_SD = 0
   eTOT_PA = 0
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SAN004"
      @ 03,20  SAY sisLETRAE+"SALDOS DEVEDORES DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
      @ 04,00  SAY "Produto : "+eCLAS_PRO+" - "+ALLTRIM(eDESC_PRO)
      @ 04,100 SAY "Tipo : "+IIF(eTIPO="G","Geral","Periodo")
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,02  SAY "No. IMOVEL"
      @ 06,15  SAY "NOME CLIENTE"
      @ 06,70  SAY "SD. DEVEDOR"
      @ 06,95  SAY "PARCELAS"
      @ 06,115 SAY "M2"
      @ 06,122 SAY "CONTRATOS"
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 8
      DO WHILE .NOT. EOF() .AND. LIN <= 56
         SELE 8
         IF eTIPO_PRO = 1
            eCLAS_PRO   = CLASSE
            eQUADRA     = QUADRA
            eNUM_IMOVEL = NUM_IMOVEL
            ePARTE      = PARTE
         ELSE
            eCLAS_PRO   = CLASSE
            eQUADRA     = QUADRA
            eNUM_IMOVEL = NUM_IMOVEL
            ePARTE      = PARTE
         ENDIF
         SELE 6
         SEEK eCLAS_PRO+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
         * Pula Venda Caso seja posterior a data de fim
         IF DATA_VENDA >= eDATA_FIM
            SELE 8
            DO WHILE (CLASSE = eCLAS_PRO .AND. eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE) .AND. (.NOT. EOF())
               SKIP
            ENDDO
            LOOP
         ENDIF
         eNUM_CLIE = NUM_CLIE
         SELE 5
         SEEK eNUM_CLIE
         SELE 8
         eSUB_SD = 0
         eSUB_PA = 0
         DO WHILE (CLASSE = eCLAS_PRO .AND. eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE) .AND. (.NOT. EOF())
            IF ATIVA
               IF DATA_VENC < CTOD("01/07/1994") .AND. eCONVERT
                  IF DATA_PGTO = CTOD("") .OR. (DATA_PGTO > eDATA_FIM) .AND. ATIVA
                     eSUB_SD = eSUB_SD + (VLR_PARC / 2750)
                  ENDIF
               ELSE
                  IF DATA_PGTO = CTOD("") .OR. (DATA_PGTO > eDATA_FIM) .AND. ATIVA
                     eSUB_SD = eSUB_SD + VLR_PARC
                  ENDIF
               ENDIF
               eSUB_PA = eSUB_PA + 1
            ENDIF
            SKIP
         ENDDO
         IF .NOT. eTOTAIS
            @ LIN,02  SAY STR(ePARTE,1)+"/"+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)
            SELE 5
            @ LIN,15  SAY CLIENTE
            @ LIN,70 SAY eSUB_SD PICT "@R 999,999,999,999.99"
            @ LIN,95 SAY eSUB_PA PICT "@R 999,999"
            eCONTRATO = eCONTRATO + 1
            SELE 1
            GOTO TOP
            SEEK eCLAS_PRO+eQUADRA+eNUM_IMOVEL
            IF FOUND()
               IF ePARTE = 0 .OR. ePARTE = 1
                  @ LIN,110 SAY METRAGEM_1 PICT "@R 9,999.99"
               ELSE
                  @ LIN,110 SAY METRAGEM_2 PICT "@R 9,999.99"
               ENDIF
            ELSE
               @ LIN,110 SAY "<<ERRO>>"
            ENDIF
            LIN = LIN + 1
         ENDIF
         SELE 8
         eTOT_SD = eTOT_SD + eSUB_SD
         eTOT_PA = eTOT_PA + eSUB_PA
         DO PTECLA
         IF sisINTERR
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
         ENDIF
      ENDDO
      IF EOF()
         LIN = LIN + 1
         @ LIN,02  SAY "Total Geral    : "
         @ LIN,70  SAY eTOT_SD   PICT "@R 999,999,999,999.99"
         @ LIN,95  SAY eTOT_PA   PICT "@R 999,999"
         @ LIN,122 SAY eCONTRATO PICT "@R 999,999"
      ENDIF
      SELE 8
      @ 58,00 SAY REPLICATE("=",135)
      @ 59,104 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
   ENDDO
ENDIF
IF eSAIDA = "ARQUIVO"
   SET PRINT TO
   SET PRINT OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINT TO
   SET DEVICE TO SCREEN
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
