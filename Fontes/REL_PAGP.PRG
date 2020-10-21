*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_PAGP.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE PAGAMENTOS DE PARCELAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_PAGP()
SAVE SCREEN TO TELA03
PTIT( "RELATORIO DE PARCELAS PAGAS")
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
USE CAD_VEND INDEX CAD_VEND
GOTO TOP
SELE 8
USE CAD_PARC INDEX CAD_PARC,DAT_PARC
GOTO TOP
SELE 9
USE TIPO_PRO INDEX TIPO_PRO
GOTO TOP
SELE 2
* Selecao do produto
DO WHILE .T.
   PLIMPA()
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   eDESC_PRO = DESCRICAO
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
eQUADRA  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA   = 0
      @ 06,02 SAY sisGRUPO+" : "
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
         DBEDIT(6,14,6,75,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eQUADRA     = NUM_QUADRA
      eDESC_QUA   = SPACE(1)
      eNUM_INICIO = NUM_INICIO
      eNUM_FINAL  = NUM_FINAL
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
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = 0
      @ 06,02 SAY sisGRUPO2+" : "
      GO TOP
      DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@S30"
      PIC[4]="@R99999"
      CAB[1]="    "
      CAB[2]="    "
      CAB[3]="    "
      CAB[4]="    "
      DBEDIT(6,08,6,76,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eQUADRA      = NUM_BLOCO
      eDESC_QUA   = DESCRICAO
      eQUANTIDADE = QUANTIDADE
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
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eT_PARC = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Deseja Parcelas Normais/Intermedi‡rias/Ambas (N/I/A) ? " GET eT_PARC PICT "@! X" VALID eT_PARC $("NIA")
READ
SELE 8
USE CAD_PARC INDEX DAT_PARC,CAD_PARC
GOTO TOP
IF eT_PARC = "A"
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)  
   GOTO TOP
ELSE
   IF eT_PARC = "I"
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. TIPO_PARC = eT_PARC
      GOTO TOP
   ELSE
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. TIPO_PARC = eT_PARC
      GOTO TOP
   ENDIF
ENDIF
IF EOF()
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("NÆo H  Parcelas Pagas Neste Periodo !")
   PPARA()
ELSE
	eARQUIVO = SPACE(60)
	IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
		eARQUIVO = sisDRIVE_REL+":"
	ELSE
		eARQUIVO = "C:"
	ENDIF	
	
	IF sisPASTA_REL <> SPACE(30)
		eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"PAGAMENTOS.TXT"+SPACE(10)  
	ELSE
		eARQUIVO = ALLTRIM(eARQUIVO) + "PAGAMENTOS.TXT"+SPACE(10) 
	ENDIF     
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,115 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,115 SAY "Relat: SAN003"
      SELE 9
      SEEK eTIPO_PRO
      eTIPO_PRD = DESCRICAO
      SELE 8
      @ 03,40  SAY sisLETRAE+"RELATORIO DE PARCELAS PAGAS"+sisLETRAC
      @ 04,00  SAY "Produto : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+" - Quadra\Bloco : "+ALLTRIM(eQUADRA)+" - "+eDESC_QUA
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,02  SAY "No. IMOVEL"
      @ 06,15  SAY "STATUS"
      @ 06,25  SAY "PARCELA"
      @ 06,35  SAY "TIPO"
      @ 06,45  SAY "DATA VENC."
      @ 06,57  SAY "VLR.PARCELA"
      @ 06,77  SAY "DATA PAGTO."
      @ 06,89  SAY "VLR.PAGO"
      @ 06,119 SAY "ORIGEM/COD."
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 8
      DO WHILE .NOT. EOF() .AND. LIN <= 54
	 @ LIN,02  SAY NUM_IMOVEL PICT "@R 99999"
	 IF PARTE = 0
	    @ LIN,15  SAY "Unico"
	 ELSE
	    IF PARTE = 1
	       @ LIN,15  SAY "1o.Prop."
	    ELSE
	       @ LIN,15  SAY "2o.Prop."
	    ENDIF
	 ENDIF
	 @ LIN,25  SAY NUM_PARC  PICT "99999"
	 @ LIN,35  SAY TIPO_PARC PICT "@! X"
	 @ LIN,45  SAY DATA_VENC
	 @ LIN,57  SAY VLR_PARC  PICT "@R 999,999,999,999.99"
	 @ LIN,77  SAY DATA_PGTO
	 @ LIN,89  SAY VLR_PGTO  PICT "@R 999,999,999,999.99"
	 @ LIN,120 SAY ALLTRIM(ORIGEM)+"/"+ALLTRIM(COD_BANCO)
	 LIN = LIN + 1
	 SKIP
	 DO PTECLA
	 IF sisINTERR
	    SET DEVICE TO SCREEN
	    CLOSE ALL
	    RETURN nil
	 ENDIF
      ENDDO
      @ 56,00 SAY REPLICATE("=",135)
      @ 57,95 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
   ENDDO
   SET DEVICE TO SCREEN
ENDIF
SET PRINT TO
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
