*********************************************************************
* Sistema     : Gest�o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CTRE.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CONTAS A RECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CTRE()
SAVE SCREEN TO TELA06
PTIT( "RELAT�RIO DE CONTAS A RECEBER")
SELE 1
USE CRECEBER INDEX DATA_CRE,CRECEBER,DATA_NF
GOTO TOP
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
SELE 3
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 4
USE CLIENTES INDEX CLIENTES
GOTO TOP
eOPCAO = "P"
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ou (0) para Todas ? " GET eCONTA PICT "@R 999"
   READ
   SELE 2
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA = 0
         EXIT
      ELSE
         PLQUA("01")
         PMSGR( "Aten��o !",3)
         PMENSAG("Conta N�o Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
PLIMPA()
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 08,02 SAY "Periodo : " GET eDATA_INI
   @ 08,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv�lidas !",3)
      SET DEVICE TO SCREEN
      CLOSE DATABASES
      RESTORE SCREEN FROM TELA06
      RETURN nil
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
   eCLASSE_PRO = 0
   PLQUA("01")
   @ 23,00 SAY " Produto ou (0) para Escritorio: " GET eCLASSE_PRO PICT "@R 9999999"
   READ
   SELE 2
   SEEK eCLASSE_PRO
   IF .NOT. FOUND()
      IF eCLASSE_PRO <> 0
         PLQUA("01")
         PMSGR( " Produto Inexistente ",3
         SELE 1
         LOOP
      ELSE
         @ 23,48 SAY " - Escritorio"
         SELE 1
         EXIT
      ENDIF
   ELSE
      @ 23,48 SAY " - "+ALLTRIM(DESCRICAO)
      eEMPREEND = DESCRICAO
      SELE 1
      EXIT
   ENDIF
ENDDO
eSALDO_INI = 0
PLQUA("01")
@ 23,00 SAY "Informe o Saldo Inicial : " GET eSALDO_INI PICT "@R 999,999,999,999.99"
READ
eARQUIVO = SPACE(50)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"CTAREC.TXT"+SPACE(30)+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "CTAREC.TXT"+SPACE(30)+SPACE(10) 
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

IF eCONTA = 0
   SELE 1
   USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE,DATA_NF
   SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. DATA_REAL = CTOD("") .AND. REALIZADO = 0 .AND. EMPREENDIM = eCLASSE_PRO
   GOTO TOP
ELSE
   SELE 1
   USE CRECEBER INDEX DATA_CRV,CRECEBER,DATA_CRE,DATA_NF
   SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. DATA_REAL = CTOD("") .AND. REALIZADO = 0 .AND. COD_CONTA = eCONTA .AND. EMPREENDIM = eCLASSE_PRO
   GOTO TOP
ENDIF
eDATA_AUX = DATA_PREV
eSUB_PRE  = 0
eSUB_REAL = 0
eSUB_NPRE = 0
eTOT_PRE  = 0
eTOT_REAL = 0
eTOT_NPRE = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,30  SAY sisLETRAE+"CONTAS A RECEBER DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 04,00  SAY "    Produto    : "+IIF(eCLASSE_PRO = 0,STR(eCLASSE_PRO,5)+" - Escritorio",STR(eCLASSE_PRO,5)+" - "+eEMPREEND)
   @ 04,50  SAY "Tipo Relat�rio : "+IIF(eOPCAO ="P","PREVISTO","REALIZADO")
   @ 04,80  SAY "Conta No. : "+STR(eCONTA,5)
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "LANC."
   @ 06,10  SAY "CONTA"
   @ 06,20  SAY "EMPR."
   @ 06,30  SAY "CLIE."
   @ 06,40  SAY "CLASSE RECEITA" 
   @ 06,100 SAY "DATA PREV."
   @ 06,112 SAY "VLR.PREVISTO"
   @ 07,10  SAY "DOCUMENTO"
   @ 07,30  SAY "APELIDO"
   @ 07,40  SAY "DESCRICAO"
   @ 07,100 SAY "DATA REAL."
   @ 07,112 SAY "VLR.REALIZADO"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF eDATA_AUX = DATA
         @ LIN,00  SAY LANCAMENTO PICT "999999"
         @ LIN,10  SAY COD_CONTA  PICT "999999"
         @ LIN,20  SAY EMPREENDIM PICT "999999"
         @ LIN,30  SAY CODIGO_CLI PICT "999999"
         @ LIN,40  SAY CLASSE_R   PICT "999999"
         eCLASSE_R = CLASSE_R
         eCODIGO_CLI = CODIGO_CLI
         SELE 3
         SEEK eCLASSE_R
         @ LIN,50  SAY ALLTRIM(DESCRICAO)
         SELE 1
         @ LIN,100 SAY DATA_PREV
         @ LIN,112 SAY PREVISTO   PICT "@R 99,999,999.99"
         LIN = LIN + 1
         @ LIN,10  SAY NUM_DOCUME
         SELE 4
         SEEK eCODIGO_CLI
         @ LIN,30  SAY APELIDO
         SELE 1
         @ LIN,50  SAY ALLTRIM(DESCRICAO)
         @ LIN,100 SAY DATA_REAL
         @ LIN,112 SAY REALIZADO  PICT "@R 99,999,999.99"
         @ LIN,127 SAY NUM_CHEQUE PICT "@R 99999999"
         LIN = LIN + 1
         eSUB_PRE  = eSUB_PRE  + PREVISTO
         eSUB_REAL = eSUB_REAL + REALIZADO
         SKIP
      ELSE
         IF eOPCAO = "P"
            @ LIN,84  SAY REPLICATE("_",45)
            LIN = LIN + 1
            @ LIN,02  SAY "Subtotais : "
            @ LIN,112 SAY eSUB_PRE   PICT "@R 99,999,999.99"
            eTOT_PRE  = eTOT_PRE  + eSUB_PRE
            LIN = LIN + 1
            @ LIN,112 SAY eTOT_PRE   PICT "@R 99,999,999.99"
            eSUB_PRE  = 0
            eSUB_REAL = 0
            LIN = LIN + 2
            eDATA_AUX = IIF(eOPCAO = "P",DATA_PREV,DATA_REAL)
         ELSE
            @ LIN,84  SAY REPLICATE("_",45)
            LIN = LIN + 1
            @ LIN,02  SAY "Subtotais : "
            @ LIN,112 SAY eSUB_REAL  PICT "@R 99,999,999.99"
            eTOT_REAL = eTOT_REAL + eSUB_REAL
            LIN = LIN + 1
            @ LIN,112 SAY eTOT_REAL  PICT "@R 99,999,999.99"
            eSUB_PRE  = 0
            eSUB_REAL = 0
            LIN = LIN + 2
            eDATA_AUX = IIF(eOPCAO = "P",DATA_PREV,DATA_REAL)
         ENDIF
      ENDIF
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
      @ LIN,84  SAY REPLICATE("_",48)
      LIN = LIN + 1
      IF eOPCAO = "P"
         @ LIN,02  SAY "Subtotais : "
         @ LIN,112 SAY eSUB_PRE   PICT "@R 99,999,999.99"
         eTOT_PRE  = eTOT_PRE  + eSUB_PRE
         LIN = LIN + 1
         @ LIN,112 SAY eTOT_PRE   PICT "@R 99,999,999.99"
         eSUB_PRE  = 0
         eSUB_REAL = 0
         LIN = LIN + 2
         eDATA_AUX = IIF(eOPCAO = "P",DATA_PREV,DATA_REAL)
         LIN = LIN + 2
         @ LIN,84  SAY REPLICATE("_",45)
         LIN = LIN + 1
         @ LIN,02  SAY "Totais : "
         @ LIN,112 SAY eTOT_PRE   PICT "@R 99,999,999.99"
         LIN = LIN + 2
         @ LIN,02  SAY "Saldo inicial : "
         @ LIN,112 SAY eSALDO_INI PICT "@R 99,999,999.99"
         LIN = LIN + 1
         @ LIN,02  SAY "Movimentacoes : "
         @ LIN,112 SAY eTOT_PRE   PICT "@R 99,999,999.99"
         LIN = LIN + 1
         @ LIN,02  SAY "Saldo Final   : "
         @ LIN,112 SAY eSALDO_INI+eTOT_PRE  PICT "@R 99,999,999.99"
         LIN = LIN + 1
      ELSE
         @ LIN,02  SAY "Subtotais : "
         @ LIN,112 SAY eSUB_REAL  PICT "@R 99,999,999.99"
         eTOT_REAL = eTOT_REAL + eSUB_REAL
         LIN = LIN + 1
         @ LIN,112 SAY eTOT_REAL  PICT "@R 99,999,999.99"
         eSUB_PRE  = 0
         eSUB_REAL = 0
         LIN = LIN + 2
         eDATA_AUX = IIF(eOPCAO = "P",DATA_PREV,DATA_REAL)
         LIN = LIN + 2
         @ LIN,84  SAY REPLICATE("_",45)
         LIN = LIN + 1
         @ LIN,02  SAY "Totais : "
         @ LIN,112 SAY eTOT_REAL  PICT "@R 99,999,999.99"
         LIN = LIN + 2
         @ LIN,02  SAY "Saldo inicial : "
         @ LIN,112 SAY eSALDO_INI PICT "@R 99,999,999.99"
         LIN = LIN + 1
         @ LIN,02  SAY "Movimentacoes : "
         @ LIN,112 SAY eTOT_REAL  PICT "@R 99,999,999.99"
         LIN = LIN + 1
         @ LIN,02  SAY "Saldo Final   : "
         @ LIN,112 SAY eSALDO_INI+eTOT_REAL PICT "@R 99,999,999.99"
         LIN = LIN + 1
      ENDIF
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
CLOSE DATABASES
RESTORE SCREEN FROM TELA06
RETURN nil
*************************  fim  *********************************
