*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_VEND.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE PRODUTOS VENDIDOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_VEND()
SAVE SCREEN TO TELA03
PTIT( "RELATORIO DE UNIDADES VENDIDAS")
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
* Selecao da rotina de Impressao conforme o tipo de produto
DO CASE
   CASE eTIPO_PRO = 1
      * Relatorio de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA   = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar a "+sisGRUPO1+" (S/N) ? ")
      IF sisRESP = "S"
         @ 06,02 SAY sisGRUPO1+" : "
         GO TOP 
         DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         PIC[4]="@S99999"
         PIC[5]="@S1"  
         PIC[6]="@S20"         
         CAB[1]="    "
         CAB[2]="    "
         CAB[3]="    "
         CAB[4]="    "
         CAB[5]=" "   
         CAB[6]="    "        
         DBEDIT(6,14,6,75,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
         eQUADRA     = NUM_QUADRA
         eNUM_INICIO = NUM_INICIO
         eNUM_FINAL  = NUM_FINAL
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
         GOTO TOP
      ENDIF
   CASE eTIPO_PRO = 2
      * Relatorio de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar "+sisGRUPO2+" (S/N) ? ")
      IF sisRESP = "S"
         @ 06,02 SAY sisGRUPO2+" : "
         GO TOP
         DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         CAB[1]="    "
         CAB[2]="    "
         CAB[3]="    "
         DBEDIT(6,10,6,41,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
         eQUADRA      = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
         GOTO TOP
      ENDIF
   OTHERWISE
      * Relatorio de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
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
eCORRETOR = 0
PLQUA("01")
@ 23,00 SAY "Informe o Codigo do Corretor ou (0) para Todos : " GET eCORRETOR PICT "@R 999" 
READ
eORDEM = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Deseja Relatorio por Ordem de Data de Venda ou Item (D/I) : " GET eORDEM PICT "@!X" VALID eORDEM $("DI")
READ
IF eORDEM = "I"
   SELE 7
   USE CAD_VEND INDEX CAD_VEND,DATA_VEN
   GOTO TOP
ENDIF
IF eCORRETOR = 0
   IF eQUADRA <> SPACE(5)
      SELE 7
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. DATA_VENDA >= eDATA_INI .AND. DATA_VENDA <= eDATA_FIM
      GOTO TOP
   ELSE
      SELE 7
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. DATA_VENDA >= eDATA_INI .AND. DATA_VENDA <= eDATA_FIM
      GOTO TOP
   ENDIF
ELSE
   IF eQUADRA <> SPACE(5)
      SELE 7
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. DATA_VENDA >= eDATA_INI .AND. DATA_VENDA <= eDATA_FIM .AND. CORRETOR = eCORRETOR
      GOTO TOP
   ELSE
      SELE 7
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. DATA_VENDA >= eDATA_INI .AND. DATA_VENDA <= eDATA_FIM .AND. CORRETOR = eCORRETOR
      GOTO TOP
   ENDIF
ENDIF

eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"VENDAS.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "VENDAS.TXT"+SPACE(10) 
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

IF EOF()
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("NÆo H  Vendas no Per¡odo !")
   PPARA()
   SET PRINT TO
   SET DEVICE TO SCREEN
   CLOSE DATABASES
   RESTORE SCREEN FROM TELA03
   RETURN nil
else
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
ENDIF
SELE 7
GOTO TOP
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
   IF eCORRETOR = 0
      @ 03,40  SAY sisLETRAE+"VENDAS DE "+DTOC(eDATA_INI)+" A "+DTOC(eDATA_FIM)+sisLETRAC
   ELSE
      @ 03,20  SAY sisLETRAE+"VENDAS DE "+DTOC(eDATA_INI)+" A "+DTOC(eDATA_FIM)+" CORRETOR No. "+ALLTRIM(STR(eCORRETOR,5))+sisLETRAC
   ENDIF
   SELE 6
   SEEK eTIPO_PRO
   eTIPO_PRD = DESCRICAO
   SELE 7
   @ 04,00  SAY "Produto : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+IIF(eQUADRA = SPACE(5)," "," - Quadra : "+ALLTRIM(eQUADRA))+" - Corretor : "+IIF(eCORRETOR = 0,"Todos",STR(eCORRETOR,5))
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,00  SAY "IMOVEL"
   @ 06,08  SAY "NOME COMPRADOR"
   @ 06,34  SAY "CPF"   
   @ 06,50  SAY "VLR.VENDA"
   @ 06,63  SAY "DT.VENDA"
   @ 06,73  SAY "MESES"
   @ 06,79  SAY "VLR. SINAL"
   @ 06,91  SAY "1o.VENC."
   @ 06,101 SAY "CORRECAO"
   @ 06,113 SAY "INDICE"
   @ 06,122 SAY "M2 TOT."
   @ 06,130 SAY "M2 PRO."
   @ 07,113 SAY "PERIOD."
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
      @ LIN,08  SAY SUBSTR(CLIENTE,1,25)
      @ LIN,34  SAY SUBSTR(CPF_CGC,1,14)      
      SELE 7
      eVLR_VENDA = 0
      eVLR_VENDA = SINAL + VLR_PARCEL + INTERMEDIA
      @ LIN,49  SAY eVLR_VENDA PICT "@R 9,999,999.99"
      @ LIN,62  SAY DATA_VENDA
      @ LIN,73  SAY PARCELAS PICT "@R 999"
      @ LIN,79  SAY SINAL    PICT "@R 999,999.99"
      @ LIN,90  SAY PRI_VENC
      @ LIN,101 SAY ULT_CORREC
      @ LIN,113 SAY INDICE
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
      @ LIN,123 SAY eM2_TOTAL PICT "@R 999.99"
      @ LIN,130 SAY eM2_PROPO PICT "@R 999.99"
      LIN = LIN + 1
      @ LIN,113 SAY PERIODO
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
	    SET PRINT TO
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
      @ LIN,00 SAY REPLICATE("-",135)
      LIN = LIN + 1
      @ LIN,02 SAY "Total Geral : "
      @ LIN,43  SAY eTOT_VEND  PICT "@R 999,999,999,999.99"
      @ LIN,73  SAY eTOT_CASO  PICT "@R 999"      
      @ LIN,77  SAY eTOT_SINA  PICT "@R 9,999,999.99"
    
      @ LIN,121 SAY eTOT_M2TO  PICT "@R 9,999.99"
      LIN = LIN + 1
      @ LIN,128 SAY eTOT_M2PR  PICT "@R 9,999.99"
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
