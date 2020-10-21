*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CTP2.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CONTAS A PAGAR - POR PRODUTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CTP2()
SAVE SCREEN TO TELA06
PTIT( "RELATàRIO DE CONTAS A PAGAR   -   POR  PRODUTO  ")
PLQUA("01")
SELE 3
USE STATUS
GOTO TOP
IF F_BLOQR()
   eORDE_FORN = ORDE_FORN
   eORDE_CLAD = ORDE_CLAD
   eORDE_CLAS = ORDE_CLAS
   UNLOCK
ENDIF
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
SELE 3
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 4
USE FORNECED INDEX FORNECED
GOTO TOP
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
PLQUA("01")
eOPCAO = SPACE(1)
@ 23,00 SAY "Deseja T‡tulos em Aberto ou Liquidados (A/L) ? " GET eOPCAO PICT "@!X" VALID eOPCAO $("AL")
READ
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
* Selecao de Produto    
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
IF .NOT. eORDE_CLAS
   eCLAS_PRO = 0
   eDESC_PRO = SPACE(30)
   DO WHILE .T.
      PLQUA("01")
      @ 23,00 SAY " Produto   ou  (0)  para  Escritorio : " GET eCLAS_PRO PICT "@R 9999999"
      READ
      SET COLOR TO W/N,+W/R
      PLQUA("01")
      @ 23,00 SAY "   Produto     : "
      SAVE SCREEN TO PESQUISA
      SELE 2
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
      CAB[3]="   Produto    "
      @ 8,19 TO 18,79 DOUBLE
      DBEDIT(9,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
      eCLAS_PRO = CODIGO
      eDESC_PRO = DESCRICAO
      RESTORE SCREEN FROM PESQUISA
      PLQUA("01")
      @ 23,00 SAY "   Produto     : "
      @ 23,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
      SELE 1
      EXIT
   ENDDO
ELSE
   eCLAS_PRO = 0
   eDESC_PRO = SPACE(30)
   DO WHILE .T.
      PLQUA("01")
      @ 23,00 SAY " Produto   ou  (0)  para  Escritorio : " GET eCLAS_PRO PICT "@R 9999999"
      READ
      eDESC_PRO = ALLTRIM(eDESC_PRO)
      SELE 2
      USE CLAS_PRO INDEX DESC_PRO,CLAS_PRO
      GOTO TOP
      IF eDESC_PRO <> SPACE(30)
         SEEK eDESC_PRO
      ENDIF
      SET COLOR TO W/N,+W/R
      PLQUA("01")
      @ 23,00 SAY "   Produto     : "
      SAVE SCREEN TO PESQUISA
      DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@S35"
      CAB[1]="C¢digo"
      CAB[2]="Tipo"
      CAB[3]="   Produto    "
      @ 8,19 TO 18,79 DOUBLE
      DBEDIT(9,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
      eCLAS_PRO = CODIGO
      eDESC_PRO = DESCRICAO
      RESTORE SCREEN FROM PESQUISA
      PLQUA("01")
      @ 23,00 SAY "   Produto     : "
      @ 23,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
      SELE 1
      EXIT
   ENDDO
ENDIF
* Selecao do Fornecedor
IF eORDE_FORN
   SELE 4
   USE FORNECED
   INDEX ON NOME TO TEMP_FOR
   GOTO TOP
ELSE
   SELE 4
   USE FORNECED INDEX FORNECED
   GOTO TOP
ENDIF
eCODIGO_FOR  = 0
eCODIGO_FORN = 0
IF .NOT. eORDE_FORN
   DO WHILE .T.
      PLQUA("01")
      @ 23,05 SAY "Fornecedor    : " GET eCODIGO_FOR PICT "@R 9999999"
      READ
      IF eCODIGO_FOR = 0
         EXIT
      ENDIF
      SET COLOR TO W/N,+W/R
      @ 23,00
      @ 23,05 SAY "Fornecedor    : " 
      SAVE SCREEN TO PESQUISA
      SELE 4
      GOTO TOP
      IF eCODIGO_FOR <> 0
         SEEK eCODIGO_FOR
      ENDIF
      DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@S40"
      CAB[1]="C¢digo"
      CAB[2]="Fornecedor"
      @ 7,19 TO 18,79 DOUBLE
      DBEDIT(08,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
      eCODIGO_FORN = NOME
      eCODIGO_FOR  = CODIGO
      RESTORE SCREEN FROM PESQUISA
      PLQUA("01")
      @ 23,00
      @ 23,05 SAY "Fornecedor    : "
      @ 23,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(NOME)
      EXIT
   ENDDO
ELSE
   eNOME_FORN  = SPACE(30)
   eCODIGO_FOR = 0
   DO WHILE .T.
      PLQUA("01")
      @ 23,05 SAY "Fornecedor    : " GET eNOME_FORN
      READ
      IF eNOME_FORN = SPACE(30)
         EXIT
      ENDIF
      SELE 4
      GOTO TOP
      eNOME_FORN = ALLTRIM(eNOME_FORN)
      IF eNOME_FORN <> SPACE(30)
         SEEK eNOME_FORN
      ENDIF
      SET COLOR TO W/N,+W/R
      @ 23,00
      @ 23,05 SAY "Fornecedor    : "
      SAVE SCREEN TO PESQUISA
      DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@S40"
      CAB[1]="C¢digo"
      CAB[2]="Fornecedor"
      @ 7,19 TO 18,79 DOUBLE
      DBEDIT(8,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
      eNOME_FORN   = NOME
      eCODIGO_FOR  = CODIGO
      RESTORE SCREEN FROM PESQUISA
      @ 23,00
      @ 23,05 SAY "Fornecedor    : "
      @ 23,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(NOME)
      EXIT
   ENDDO
ENDIF
eFORNECED = CODIGO
DO CASE
   CASE eOPCAO = "A"
      DO CASE
      CASE eCONTA = 0
         DO CASE
         CASE eFORNECED = 0
            SELE 1
            USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. REALIZADO = 0
            GOTO TOP
         CASE eFORNECED <> 0
            SELE 1
            USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND.  CODIGO_FOR = eFORNECED .AND. REALIZADO = 0
            GOTO TOP
         ENDCASE
      CASE eCONTA <> 0
         DO CASE
         CASE eFORNECED = 0
            SELE 1
            USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. COD_CONTA = eCONTA .AND. REALIZADO = 0
            GOTO TOP
         CASE eFORNECED <> 0
            SELE 1
            USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. CODIGO_FOR = eFORNECED .AND. COD_CONTA = eCONTA .AND. REALIZADO = 0
            GOTO TOP
         ENDCASE
      ENDCASE
   CASE eOPCAO = "L"
      DO CASE
      CASE eCONTA = 0
         DO CASE
         CASE eFORNECED = 0
            SELE 1
            USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. REALIZADO <> 0
            GOTO TOP
         CASE eFORNECED <> 0
            SELE 1
            USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. CODIGO_FOR = eFORNECED .AND. REALIZADO <> 0
            GOTO TOP
         ENDCASE
      CASE eCONTA <> 0
         DO CASE
         CASE eFORNECED = 0
            SELE 1
            USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. COD_CONTA = eCONTA .AND. REALIZADO <> 0
            GOTO TOP
         CASE eFORNECED <> 0
            SELE 1
            USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. CODIGO_FOR = eFORNECED .AND. COD_CONTA = eCONTA .AND. REALIZADO <> 0
            GOTO TOP
         ENDCASE
      ENDCASE
ENDCASE
SELE 3
USE CLASSE_D INDEX CLASSE_D
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
SELE 1
eDATA_AUX = IIF(eOPCAO ="A",DATA_PREV,DATA_REAL)
eSUB_PRE  = 0
eSUB_REAL = 0
eTOT_PRE  = 0
eTOT_REAL = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,10  SAY sisLETRAE+"CONTAS A PAGAR DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+"   -   PRODUTO   "+sisLETRAC
   @ 04,00  SAY "   Produto     : "+IIF(eCLAS_PRO = 0,STR(eCLAS_PRO,5)+" - Escritorio",STR(eCLAS_PRO,5)+" - "+eDESC_PRO)
   @ 04,60  SAY "Tipo Relat‡rio : "+IIF(eOPCAO ="A","EM ABERTO","LIQUIDADOS")
   @ 04,90  SAY "Conta No. : "+STR(eCONTA,5)
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
   @ 07,100 SAY "DATA REAL."
   @ 07,112 SAY "VLR.REALIZADO"
   @ 07,127 SAY "CHEQUE"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF eDATA_AUX = IIF(eOPCAO ="A",DATA_PREV,DATA_REAL)
         IF eOPCAO = "A"
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
            eSUB_PRE  = eSUB_PRE  + PREVISTO
            LIN = LIN + 1
            @ LIN,10  SAY NUM_DOCUME
            SELE 4
            SEEK eCODIGO_FOR
            @ LIN,30  SAY APELIDO
            SELE 1
            @ LIN,50  SAY ALLTRIM(DESCRICAO)
            LIN = LIN + 1
            SELE 1
            SKIP
         ELSE
            @ LIN,00  SAY LANCAMENTO PICT "999999"
            @ LIN,10  SAY COD_CONTA  PICT "999999"
            @ LIN,20  SAY EMPREENDIM PICT "999999"
            @ LIN,30  SAY CODIGO_FOR PICT "999999"
            @ LIN,40  SAY CLASSE_D   PICT "999999"
            eCLASSE   = CLASSE_D
            eCODIGO_FOR = CODIGO_FOR
            SELE 3
            SEEK eCLASSE
            @ LIN,50  SAY ALLTRIM(DESCRICAO)
            SELE 1
            LIN = LIN + 1
            @ LIN,10  SAY NUM_DOCUME
            SELE 4
            SEEK eCODIGO_FOR
            @ LIN,30  SAY APELIDO
            SELE 1
            @ LIN,50  SAY ALLTRIM(DESCRICAO)
            @ LIN,100 SAY DATA_REAL
            @ LIN,112 SAY REALIZADO  PICT "@R 99,999,999.99"
            @ LIN,125 SAY NUM_CHEQUE PICT "@R 99999999"
            eSUB_REAL = eSUB_REAL + REALIZADO
            LIN = LIN + 1
            SKIP
         ENDIF
      ELSE
         @ LIN,84  SAY REPLICATE("_",45)
         LIN = LIN + 1
         @ LIN,02  SAY "Subtotais : "
         @ LIN,112 SAY eSUB_PRE   PICT "@R 99,999,999.99"
         LIN = LIN + 1
         @ LIN,112 SAY eSUB_REAL  PICT "@R 99,999,999.99"
         eTOT_PRE  = eTOT_PRE  + eSUB_PRE
         eTOT_REAL = eTOT_REAL + eSUB_REAL
         eSUB_PRE  = 0
         eSUB_REAL = 0
         LIN = LIN + 2
         eDATA_AUX = IIF(eOPCAO = "A",DATA_PREV,DATA_REAL)
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
         @ 03,10  SAY sisLETRAE+"CONTAS A PAGAR DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+" - LIQUIDADOS"+sisLETRAC
         @ 04,60  SAY "Tipo Relat‡rio : "+IIF(eOPCAO ="A","EM ABERTO","LIQUIDADOS")
         @ 04,90  SAY "Conta No. : "+STR(eCONTA,5)
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
         @ 07,100 SAY "DATA REAL."
         @ 07,112 SAY "VLR.REALIZADO"
         @ 07,127 SAY "CHEQUE"
         @ 08,00  SAY REPLICATE("=",135)
         LIN = 9
      ENDIF
      @ LIN,84  SAY REPLICATE("_",45)
      LIN = LIN + 1
      @ LIN,02  SAY "Subtotais : "
      @ LIN,112 SAY eSUB_PRE   PICT "@R 99,999,999.99"
      LIN = LIN + 1
      @ LIN,112 SAY eSUB_REAL  PICT "@R 99,999,999.99"
      LIN = LIN + 2
      eTOT_PRE  = eTOT_PRE  + eSUB_PRE
      eTOT_REAL = eTOT_REAL + eSUB_REAL
      @ LIN,84  SAY REPLICATE("_",45)
      LIN = LIN + 1
      @ LIN,02  SAY "Totais : "
      @ LIN,112 SAY eTOT_PRE   PICT "@R 99,999,999.99"
      LIN = LIN + 1
      @ LIN,112 SAY eTOT_REAL  PICT "@R 99,999,999.99"
      LIN = LIN + 2
      @ LIN,02  SAY "Saldo inicial : "
      @ LIN,112 SAY eSALDO_INI PICT "@R 99,999,999.99"
      LIN = LIN + 1
      @ LIN,112 SAY eSALDO_INI PICT "@R 99,999,999.99"
      LIN = LIN + 1
      @ LIN,02  SAY "Movimentacoes : "
      @ LIN,112 SAY eTOT_PRE   PICT "@R 99,999,999.99"
      LIN = LIN + 1
      @ LIN,112 SAY eTOT_REAL  PICT "@R 99,999,999.99"
      LIN = LIN + 1
      @ LIN,02  SAY "Saldo Final   : "
      @ LIN,112 SAY eSALDO_INI+eTOT_PRE  PICT "@R 99,999,999.99"
      LIN = LIN + 1
      @ LIN,112 SAY eSALDO_INI+eTOT_REAL PICT "@R 99,999,999.99"
      LIN = LIN + 1
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
