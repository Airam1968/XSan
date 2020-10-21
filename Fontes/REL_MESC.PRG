*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_MESC.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE MES CORRECAO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_MESC()
SAVE SCREEN TO TELA03
PTIT( "RELATORIO DE MES CORRECAO")
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
USE TIPO_PRO INDEX TIPO_PRO
GOTO TOP
SELE 7
USE CAD_VEND INDEX DATA_VEN,CAD_VEND
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
   eQUADRA    = SPACE(1)
   eQUADRA   = SPACE(1)
   EXIT
ENDDO
eMES = 0
DO WHILE .T.
   PLQUA("01")
   @ 23,00 SAY "Informe o Mes de 1 a 12 : " GET eMES PICT "@R 99"
   READ
   IF eMES <= 0 .OR. eMES >= 13
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("Mes Inv†lido !")
      LOOP
   ELSE
      eMES = ALLTRIM(STR(eMES,4))
      EXIT
   ENDIF
ENDDO
eANO = 0
DO WHILE .T.
   PLQUA("01")
   @ 23,00 SAY "Informe o Ano com o formato de 4 digitos : " GET eANO PICT "@R 9999"
   READ
   IF eANO = 0
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("Ano Inv†lido !")
      LOOP
   ELSE
      eANO = ALLTRIM(STR(eANO,4))
      EXIT
   ENDIF
ENDDO
SELE 1
SET FILTER TO CLASSE = STR(eCLAS_PRO,5) 
GOTO TOP
eORDEM = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Deseja Relatorio por Ordem de Data de Venda ou "+sisUNID1+" (D/I) : " GET eORDEM PICT "@!X" VALID eORDEM $("DI")
READ
IF eORDEM = "I" 
   SELE 7
   USE CAD_VEND INDEX CAD_VEND,DATA_VEN
   GOTO TOP
ENDIF
SELE 7
SET FILTER TO (CLASSE = STR(eCLAS_PRO,5) .AND. ((alltrim(STR(MONTH(ULT_CORREC),2)) = eMES) .AND. (alltrim(STR(YEAR(ULT_CORREC),4)) = eANO)))
GOTO TOP
IF EOF()
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("N∆o H† Vendas no Per°odo !")
   PPARA()
   SET DEVICE TO SCREEN
   CLOSE DATABASES
   RESTORE SCREEN FROM TELA03
   RETURN nil
ENDIF
SELE 7
GOTO TOP
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"MESCORRECAO.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "MESCORRECAO.TXT"+SPACE(10) 
ENDIF 
eSAIDA   = SPACE(10)
DO WHILE .T.
   PLQUA("01")
   PPERG ( "Deseja relat¢rio em arquivo texto no Drive (X:) (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
	  @ 23,00 SAY "CONFIRME O DESTINO  : " GET eARQUIVO
      READ
      eARQUIVO = ALLTRIM(eARQUIVO)
      eSAIDA = "ARQUIVO"
      EXIT
   ELSE
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

eTOT_VEND = 0
eTOT_SINA = 0
eTOT_CASO = 0
eTOT_M2TO = 0 
eTOT_M2PR = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN003"
   @ 03,40  SAY sisLETRAE+"MES DE CORRECAO MES "+eMES+" E ANO "+eANO+sisLETRAC
   @ 04,00  SAY "   Produto     : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,00  SAY "IMOVEL"
   @ 06,10  SAY "NOME COMPRADOR"
   @ 06,45  SAY "VLR.VENDA"
   @ 06,56  SAY "DT.VENDA"
   @ 06,66  SAY "MESES"
   @ 06,75  SAY "VLR. SINAL"
   @ 06,91  SAY "1o.VENC."
   @ 06,101 SAY "CORRECAO"
   @ 06,113 SAY "INDICE"
   @ 06,122 SAY "M2 TOT."
   @ 06,130 SAY "M2 PRO."
   @ 07,113 SAY "PERIODICIDADE"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      @ LIN,00  SAY ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/"+STR(PARTE,1)
      eQUADRA     = QUADRA
      eNUM_IMOVEL = NUM_IMOVEL
      ePARTE      = PARTE
      eNUM_CLIE   = NUM_CLIE
      SELE 5
      SEEK eNUM_CLIE
      @ LIN,10  SAY SUBSTR(CLIENTE,1,34)
      SELE 7
      eVLR_VENDA = 0
      eVLR_VENDA = SINAL + VLR_PARCEL + INTERMEDIA
      @ LIN,40  SAY eVLR_VENDA PICT "@R 999,999,999.99"
      * Ganbiara da desur(pedro pediu 10/08/96)
      IF sisCGC_CPF = "00465597000140"
         IF DATA_VENDA < CTOD("01/01/1996")
            @ LIN,55  SAY CTOD(SUBSTR(DTOC(DATA_VENDA),1,2)+"/01/"+STR(((VAL(SUBSTR(DTOC(DATA_VENDA),7,4))+1)),4))
         ELSE
            @ LIN,55  SAY DATA_VENDA
         ENDIF
      ELSE
         @ LIN,55  SAY DATA_VENDA
      ENDIF
      @ LIN,66  SAY PARCELAS PICT "@R 999"
      @ LIN,75  SAY SINAL    PICT "@R 999,999,999.99"
      @ LIN,90  SAY PRI_VENC
      @ LIN,101 SAY ULT_CORREC
      @ LIN,112 SAY INDICE
      SELE 1
      SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL
      IF ePARTE = 0 .OR. ePARTE = 1
         eM2_TOTAL = METRAGEM_1
         eM2_PROPO = M2_PROP_1
      ELSE
         eM2_TOTAL = METRAGEM_2
         eM2_PROPO = M2_PROP_2
      ENDIF
      SELE 7
      @ LIN,122 SAY eM2_TOTAL PICT "@R 999.99"
      @ LIN,130 SAY eM2_PROPO PICT "@R 999.99"
      LIN = LIN + 1
      @ LIN,112 SAY PERIODO
      eTOT_M2TO = eTOT_M2TO + eM2_TOTAL
      eTOT_M2PR = eTOT_M2PR + eM2_PROPO
      eTOT_VEND = eTOT_VEND + eVLR_VENDA
      eTOT_SINA = eTOT_SINA + SINAL
      eTOT_CASO = eTOT_CASO + 1
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
      @ LIN,00 SAY REPLICATE("-",135)
      LIN = LIN + 1
      @ LIN,02 SAY "Total Geral : "
      @ LIN,80  SAY eTOT_CASO  PICT "@R 999"
      @ LIN,50  SAY eTOT_VEND  PICT "@R 999,999,999,999.99"
      @ LIN,89  SAY eTOT_SINA  PICT "@R 999,999,999.99"
      @ LIN,119 SAY eTOT_M2TO PICT "@R 99,999.99"
      LIN = LIN + 1
      @ LIN,127 SAY eTOT_M2PR PICT "@R 99,999.99"
      LIN = LIN + 1
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
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
