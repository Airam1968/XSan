*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_DENC.PRG
* Data        : 01/01/99
* Objetivo    : Relatorio dos Depositos Nao Compensados
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_DENC()
SAVE SCREEN TO TELA04
PTIT( "RELATORIO DE DEPOSITOS N«O COMPENSADOS")
SELE 3
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ? " GET eCONTA PICT "@R 999"
   READ
   SELE 2
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA <> 0
         PLQUA("01")
         PMSGR( "Atená∆o !",3)
         PMENSAG("Conta N∆o Existe !")
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
SELE 1
USE CRECEBER INDEX DATA_CRE,CRECEBER
SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE = CTOD("") 
GOTO TOP
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"DEPOSITOS.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "DEPOSITOS.TXT"+SPACE(10) 
ENDIF    
PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
PPARA()
DO PIMPR
SET DEVICE TO PRINT
eDATA_AUX = DATA_REAL
eSUB_PRE  = 0
eSUB_REAL = 0
eTOT_PRE  = 0
eTOT_REAL = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,10  SAY sisLETRAE+"DEPOSITOS NAO COMPENSADOS DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "LANCAM."
   @ 06,10  SAY "CONTA"
   @ 06,20  SAY "PROD."
   @ 06,30  SAY "CLIE."
   @ 06,40  SAY "CLASSE RECEITA"
   @ 06,100 SAY "DATA REAL."
   @ 06,112 SAY "VLR.REALIZADO"
   @ 07,40  SAY "DESCRICAO" 
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      @ LIN,00  SAY LANCAMENTO PICT "@R 99999999"
      @ LIN,10  SAY COD_CONTA  PICT "999999"
      @ LIN,20  SAY EMPREENDIM PICT "999999"
      @ LIN,30  SAY CODIGO_CLI PICT "999999"
      @ LIN,40  SAY CLASSE_R   PICT "999999"
      eCLASSE_R = CLASSE_R
      SELE 3
      SEEK eCLASSE_R
      @ LIN,50  SAY ALLTRIM(DESCRICAO)
      SELE 1
      @ LIN,100 SAY DATA_REAL
      @ LIN,112 SAY REALIZADO  PICT "@R 99,999,999.99"
      LIN = LIN + 1
      @ LIN,50  SAY ALLTRIM(DESCRICAO)
      LIN = LIN + 1
      eSUB_PRE  = eSUB_PRE  + REALIZADO
      eSUB_REAL = eSUB_REAL + REALIZADO
      SKIP
      DO PTECLA
      IF sisINTERR
         CLOSE ALL
         RETURN nil
      ENDIF
   ENDDO
   IF EOF()
      @ LIN,84  SAY REPLICATE("_",45)
      LIN = LIN + 1
      @ LIN,02  SAY "Subtotais : "
*     @ LIN,112 SAY eSUB_PRE   PICT "@R 99,999,999.99"
*     LIN = LIN + 1
      @ LIN,112 SAY eSUB_REAL  PICT "@R 99,999,999.99"
      LIN = LIN + 2
      eTOT_PRE  = eTOT_PRE  + eSUB_PRE
      eTOT_REAL = eTOT_REAL + eSUB_REAL
      @ LIN,84  SAY REPLICATE("_",45)
      LIN = LIN + 1
      @ LIN,02  SAY "Totais : "
*     @ LIN,112 SAY eTOT_PRE   PICT "@R 99,999,999.99"
*     LIN = LIN + 1
      @ LIN,112 SAY eTOT_REAL  PICT "@R 99,999,999.99"
   ENDIF
   @ 56,00 SAY REPLICATE("=",135)
   @ 57,95 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
                     
