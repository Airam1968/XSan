*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CTRD.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CONTAS A RECEBER POR DEZENA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CTRD()
SAVE SCREEN TO TELA06
PTIT( "RELATàRIO DE CONTAS A RECEBER POR DEZENA")
SELE 1
USE CRECEBER INDEX DATA_CRE,CRECEBER,DATA_NF
GOTO TOP
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 4
USE CLIENTES INDEX CLIENTES
GOTO TOP
SELE 1
PLIMPA()
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 08,02 SAY "Periodo : " GET eDATA_INI
   @ 08,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv lidas !",3)
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
eARQUIVO = SPACE(50)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"CONTASAPAGAR.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "CONTASAPAGAR.TXT"+SPACE(10) 
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
SELE 1
SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM
GOTO TOP
eDATA_AUX = eDATA_INI
eSUB_PRE  = 0
eSUB_REAL = 0
eSUB_NPRE = 0
eTOT_PRE  = 0
eTOT_REAL = 0
eTOT_NPRE = 0
SELE 2
GOTO TOP
eCLASSE = CODIGO
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,30  SAY sisLETRAE+"CONTAS A RECEBER DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 04,00  SAY "Tipo Relat‡rio : "+IIF(eOPCAO ="P","PREVISTO","REALIZADO")
   @ 04,50  SAY "Conta No. : "+STR(eCONTA,5)
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "No. LANC."
   @ 06,12  SAY "DATA"
   @ 06,22  SAY "CLIENTE"
   @ 06,32  SAY "DESCRICAO DA RECEITA"
   @ 06,86  SAY "PREVISTO"
   @ 06,102 SAY "REALIZADO"
   @ 06,118 SAY "N.PREVISTO"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      SELE 1
      GOTO TOP
      DO WHILE .NOT. EOF()
	 IF EMPREENDIM = eCLASSE
	    eCODIGO_CLI = CODIGO_CLI
	    eDESCRICAO  = DESCRICAO
	    eSUB_PRE    = eSUB_PRE  + PREVISTO
	    eSUB_REAL   = eSUB_REAL + REALIZADO
	    eSUB_NPRE   = eSUB_NPRE + NPREVISTO
	 ENDIF
	 SKIP
      ENDDO
      SELE 2
      IF eSUB_PRE <> 0 .OR. eSUB_REAL <> 0 .OR. eSUB_NPRE <> 0
	 @ LIN,22  SAY eCODIGO_CLI PICT "99999"
	 @ LIN,32  SAY ALLTRIM(eDESCRICAO)
	 @ LIN,84  SAY eSUB_PRE   PICT "@R 999,999,999.99"
	 @ LIN,100 SAY eSUB_REAL  PICT "@R 999,999,999.99"
	 @ LIN,116 SAY eSUB_NPRE  PICT "@R 999,999,999.99"
	 eTOT_PRE  = eTOT_PRE  + eSUB_PRE
	 eTOT_REAL = eTOT_REAL + eSUB_REAL
	 eTOT_NPRE = eTOT_NPRE + eSUB_NPRE
	 LIN = LIN + 1
      ENDIF
      SKIP
      eCLASSE = CODIGO
      eSUB_PRE  = 0
      eSUB_REAL = 0
      eSUB_NPRE = 0
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
      @ LIN,02  SAY "Totais : "
      @ LIN,82  SAY eTOT_PRE   PICT "@R 9,999,999,999.99"
      @ LIN,98  SAY eTOT_REAL  PICT "@R 9,999,999,999.99"
      @ LIN,114 SAY eTOT_NPRE  PICT "@R 9,999,999,999.99"
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
