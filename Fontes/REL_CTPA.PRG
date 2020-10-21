*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CTPA.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CONTAS A PAGAR - TITULOS EM ABERTO"
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CTPA()
SAVE SCREEN TO TELA06
PTIT( "RELATàRIO DE CONTAS A PAGAR - TÖTULOS EM ABERTO")
PLQUA("01")
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
SELE 3
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 4
USE FORNECED INDEX FORNECED
GOTO TOP
eOPCAO = "P"
eSALDO_INI = 0
PLQUA("01")
@ 23,00 SAY "Informe o Saldo Inicial do Relatorio : " GET eSALDO_INI PICT "@R 999,999,999.99"
READ
PLQUA("01")
eRELATORIO = SPACE(1)
@ 23,00 SAY "Deseja Descri‡‡es por (D)espesa ou (F)ornecedor ? " GET eRELATORIO PICT "@!X" VALID eRELATORIO $("DF")
READ
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
         PMSGR( "Aten‡Æo !",3)
         PMENSAG("Conta NÆo Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,02 SAY "Periodo : " GET eDATA_INI
   @ 23,25 SAY "Ate : "     GET eDATA_FIM
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
DO CASE
   CASE eCONTA = 0
      SELE 1
      USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
      SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. DATA_REAL = CTOD("") .AND. REALIZADO = 0
      GOTO TOP
   CASE eCONTA <> 0
      SELE 1
      USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
      SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. DATA_REAL = CTOD("") .AND. REALIZADO = 0 .AND. COD_CONTA = eCONTA
      GOTO TOP
ENDCASE
eSALDO_INI = 0
PLQUA("01")
@ 23,00 SAY "Informe o Saldo Inicial : " GET eSALDO_INI PICT "@R 999,999,999,999.99"
READ
SELE 1
GOTO TOP
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
      @ 23,00 SAY "CAMINHO\ARQUIVO: " GET eARQUIVO
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
eDATA_AUX = DATA_PREV
eSUB_PRE  = 0
eSUB_REAL = 0
eTOT_PRE  = 0
eTOT_REAL = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,10  SAY sisLETRAE+"CONTAS A PAGAR DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+" - EM ABERTO"+sisLETRAC
   @ 04,00  SAY "Tipo Relat‡rio : PREVISTO"
   @ 04,50  SAY "Conta No. : "+STR(eCONTA,5)
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "LANC."
   @ 06,10  SAY "CONTA"
   @ 06,20  SAY "PROD."
   @ 06,30  SAY "FORN."
   @ 06,40  SAY "CLASSE DESPESA" 
   @ 06,100 SAY "DATA PREV."
   @ 06,112 SAY "VLR.PREVISTO"
   @ 07,10  SAY "DOCUMENTO" 
   @ 07,30  SAY "APELIDO"
   @ 07,40  SAY "DESCRICAO"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF eDATA_AUX = DATA_PREV
         @ LIN,00  SAY LANCAMENTO PICT "999999"
         @ LIN,10  SAY COD_CONTA  PICT "999999"
         @ LIN,20  SAY EMPREENDIM PICT "999999"
         @ LIN,30  SAY CODIGO_FOR PICT "999999"
         @ LIN,40  SAY CLASSE_D   PICT "999999"
         eCLASSE_D   = CLASSE_D
         eCODIGO_FOR = CODIGO_FOR
         IF eRELATORIO = "D"
            SELE 3
            SEEK eCLASSE_D
            @ LIN,50  SAY ALLTRIM(DESCRICAO)
         ELSE
            SELE 4
            SEEK eCODIGO_FOR
            @ LIN,50  SAY ALLTRIM(NOME)
         ENDIF
         SELE 1
         @ LIN,100 SAY DATA_PREV
         @ LIN,112 SAY PREVISTO   PICT "@R 99,999,999.99"
         LIN = LIN + 1
         @ LIN,10  SAY NUM_DOCUME
         SELE 4
         SEEK eCODIGO_FOR
         @ LIN,30  SAY APELIDO
         SELE 1
         @ LIN,50  SAY ALLTRIM(DESCRICAO)
         eSUB_PRE  = eSUB_PRE  + PREVISTO
         eSUB_REAL = eSUB_REAL + PREVISTO
         LIN = LIN + 1
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
      IF LIN > 48
         @ 58,00 SAY REPLICATE("=",135)
         @ 59,104 SAY sisTIT3
         @ 01,00 SAY " "+sisLETRAN
         @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
         @ 01,120 SAY "Data : "+dtoc(DATE())
         @ 02,00  SAY "Sistema : "+sisTIT2
         @ 02,121 SAY "Relat: SAN004"
         @ 03,10  SAY sisLETRAE+"CONTAS A PAGAR DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+" - EM ABERTO"+sisLETRAC
         @ 05,00  SAY REPLICATE("=",135)
         @ 06,02  SAY "LANC."
         @ 06,10  SAY "CONTA"
         @ 06,20  SAY "PROD."
         @ 06,30  SAY "FORN."
         @ 06,40  SAY "CLASSE DESPESA" 
         @ 06,100 SAY "DATA PREV."
         @ 06,112 SAY "VLR.PREVISTO"
         @ 07,10  SAY "DOCUMENTO" 
         @ 07,30  SAY "APELIDO"
         @ 07,40  SAY "DESCRICAO"
         @ 08,00 SAY REPLICATE("=",135)
       LIN = 9
      ENDIF
      @ LIN,84  SAY REPLICATE("_",45)
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
   @ 57,104 SAY sisTIT3
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
