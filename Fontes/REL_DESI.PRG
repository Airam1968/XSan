*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_DESI.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE DESISTENCIAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_DESI()
SAVE SCREEN TO TELA03
PTIT( "RELATORIO DE DESISTENCIAS")
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
USE RENEGOCI 
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
   eQUADRA   = SPACE(1)
   eQUADRA   = SPACE(1)
   EXIT
ENDDO
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 07,02 SAY "Periodo : " GET eDATA_INI
   @ 07,25 SAY "Ate : "     GET eDATA_FIM
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
eORDEM = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Deseja Relatorio por Ordem de Data de Venda ou "+sisUNID1+" (D/I) : " GET eORDEM PICT "@!X" VALID eORDEM $("DI")
READ
PLQUA("01")
PMENSAG("Aguarde ...")
IF eORDEM = "I" 
   SELE 7
   USE RENEGOCI
   GOTO TOP
   INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO RENEGOCI
   GOTO TOP
   USE RENEGOCI INDEX RENEGOCI
   GOTO TOP
ELSE
   SELE 7
   USE RENEGOCI
   GOTO TOP
   INDEX ON DATA_RENEG TO RENEGOCI
   GOTO TOP
   USE RENEGOCI INDEX RENEGOCI
   GOTO TOP
ENDIF
SELE 7
USE RENEGOCI INDEX RENEGOCI
GOTO TOP
SET FILTER TO CLASSE = STR(eCLAS_PRO) .AND. (DATA_RENEG >= eDATA_INI .AND. DATA_RENEG <= eDATA_FIM) .AND. TIPO_RENEG = "D"
GOTO TOP
IF EOF()
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("NÆo H  Desistˆncias no Per¡odo !")
   PPARA()
   SET DEVICE TO SCREEN
   CLOSE DATABASES
   RESTORE SCREEN FROM TELA03
   RETURN nil
ENDIF
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"DESISTENCIAS.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "DESISTENCIAS.TXT"+SPACE(10) 
ENDIF     
SELE 7
GOTO TOP
PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
PPARA()
DO PIMPR
SET DEVICE TO PRINT
eTOT_VEND = 0
eTOT_SINA = 0
eTOT_CASO = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN003"
   @ 03,30  SAY sisLETRAE+"DESISTENCIAS DE "+DTOC(eDATA_INI)+" A "+DTOC(eDATA_FIM)+sisLETRAC
   SELE 6
   SEEK eTIPO_PRO
   eTIPO_PRD = DESCRICAO
   SELE 7
   @ 04,00  SAY "Produto :"+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+IIF(eQUADRA = SPACE(5)," "," - Quadra : "+ALLTRIM(eQUADRA))
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,00  SAY "IMOVEL"
   @ 06,10  SAY "NOME COMPRADOR"
   @ 06,55  SAY "VALOR"
   @ 06,71  SAY "DESISTENCIA"
   @ 06,90  SAY "MESES"
   @ 06,105 SAY "1o.VENC."
   @ 07,00 SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      @ LIN,00  SAY ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/"+STR(PARTE,1)
      eNUM_CLIE = NUM_CLIE
      SELE 5
      SEEK eNUM_CLIE
      IF FOUND()
         @ LIN,10  SAY SUBSTR(CLIENTE,1,39)
      ELSE
		 @ LIN,10  SAY "CLIENTE EXCLUIDO !"
      ENDIF
      SELE 7
      eVLR_VENDA = 0
      eVLR_VENDA = VLR_PARCEL 
      @ LIN,50  SAY eVLR_VENDA PICT "@R 999,999,999,999.99"
      @ LIN,69  SAY DATA_RENEG
      @ LIN,90  SAY PARCELAS PICT "@R 999"
      @ LIN,104 SAY PRI_VENC
      eTOT_VEND = eTOT_VEND + eVLR_VENDA
      eTOT_CASO = eTOT_CASO + 1
      LIN = LIN + 1
      SKIP
      DO PTECLA
      IF sisINTERR
         SET DEVICE TO SCREEN
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
      LIN = LIN + 1
   ENDIF
   @ 56,00 SAY REPLICATE("=",135)
   @ 57,95 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
