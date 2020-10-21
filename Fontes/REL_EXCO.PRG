*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_EXCO.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE EXTRATO DE CORREá∆o
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_EXCO()
SAVE SCREEN TO TELA03
PTIT( "RELAT‡RIO EXTRATO DE CORREÄ«O ANUAL")
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   eDESC_PRO = DESCRICAO
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
      eQUADRA     = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
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
         DBEDIT(6,14,6,75,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eQUADRA     = NUM_QUADRA
      eNUM_INICIO = NUM_INICIO
      eNUM_FINAL  = NUM_FINAL
      @ 07,02 SAY sisUNID1+" : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Atená∆o !",3)
			PMENSAG("N∆o H† "+sisUNID1+" Dispon°veis !")
	 PPARA()
	 CLOSE DATABASES
	 RESTORE SCREEN FROM TELA03
	 RETURN nil
      ENDIF
      DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5]="PROPRIET_1"
      CAMPO[6]="PROPRIET_2"
      LENS[5] =5
      LENS[6] =5
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      PIC[4]="@R99999"
      PIC[5]="@R99999"
      PIC[6]="@R99999"
      CAB[1]="     "
      CAB[2]="     "
      CAB[3]="     "
      CAB[4]="     "
      CAB[5]="     "
      CAB[6]="     "
      DBEDIT(7,15,7,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eNUM_IMOVEL = NUM_IMOVEL
      eSTATUS     = STATUS
      ePROPR_1    = PROPRIET_1
      ePROPR_2    = PROPRIET_2
      IF eSTATUS = 0
	 IF ePROPR_1 = 1
	    PLQUA("01")
	    PMSGR( "Atená∆o !",3)
            PMENSAG ( sisUNID1+" N∆o Vendido !")
	    PPARA()
	 ELSE
	    ePARTE = 0
	 ENDIF
      ELSE
	 eOPCAO = 0
	 PLQUA("01")
	 @ 23,00 SAY "Informe se (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
	 READ
	 IF eOPCAO = 1
	    IF ePROPR_1 = 1
	       PLQUA("01")
		  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 1
	       ENDIF
	    ELSE
	       IF ePROPR_2 = 1
		  PLQUA("01")
		  PMSGR( "Atená∆o !",3)
                  PMENSAG ( sisUNID1+" N∆o Vendido !")
		  PPARA()
	       ELSE
		  ePARTE = 2
	       ENDIF
	    ENDIF
	 ENDIF
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      eNUM_IMOVEL= SPACE(5)
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
      eQUANTIDADE = QUANTIDADE
      @ 07,02 SAY sisUNID2+" : "
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
      GOTO TOP
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Atená∆o !",3)
			PMENSAG("N∆o H† "+sisUNID2+" Dispon°veis !")
	 PPARA()
	 CLOSE DATABASES
	 RESTORE SCREEN FROM TELA03
	 RETURN nil
      ENDIF
      DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5]="PROPRIET_1"
      CAMPO[6]="PROPRIET_2"
      LENS[5] =5
      LENS[6] =5
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      PIC[4]="@R99999"
      PIC[5]="@R99999"
      PIC[6]="@R99999"
      CAB[1]="     "
      CAB[2]="     "
      CAB[3]="     "
      CAB[4]="     "
      CAB[5]="     "
      CAB[6]="     "
      DBEDIT(7,15,7,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eNUM_IMOVEL = NUM_IMOVEL
      eSTATUS     = STATUS
      ePROPR_1    = PROPRIET_1
      ePARTE      = 0
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE

eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"EXTRATOCORRECAO.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "EXTRATOCORRECAO.TXT"+SPACE(10) 
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
      eSAIDA = "ARQUIVO"
      EXIT
   ELSE
      DO PIMPR
      eSAIDA = "IMPRESSORA"
      EXIT
   ENDIF
ENDDO

SELE 1
IF ePARTE = 0 .OR. ePARTE = 1
   eNUM_CLIE = PROPRIET_1
ELSE
   eNUM_CLIE = PROPRIET_2
ENDIF
SELE 8
eIND_CLAS = STR(eCLAS_PRO,5)
eIND_QUAD = eQUADRA
eZERO = "000"
eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
USE &eARQ INDEX &eIND2,&eIND3,&eIND1
SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE
GOTO TOP
eTOT_ORIGIN = 0
IF EOF()
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("N∆o H† Extrato Deste Per°odo !")
   PPARA()
ELSE
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

   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SAN004"
      @ 03,40  SAY sisLETRAE+"EXTRATO DE CORRECAO ANUAL"+sisLETRAC
      @ 04,00  SAY "   Produto     : "+ALLTRIM(eDESC_PRO)+" - Quadra\Bloco : "+ALLTRIM(eQUADRA)+" - Imovel No. : "+ALLTRIM(eNUM_IMOVEL)+" - Parte : "+STR(ePARTE,1)
      SELE 5
      SEEK eNUM_CLIE
      @ 05,00  SAY "Proprietario   : "+ALLTRIM(CLIENTE)
      SELE 6
      SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
      @ 06,00  SAY "Indexador Atual : "+INDICE
      @ 06,40  SAY "Ultima Data Base : "+DTOC(ULT_CORREC)
      SELE 8
      @ 07,00  SAY REPLICATE("=",135)
      @ 08,02  SAY "IMOVEL"
      @ 08,10  SAY "DATA VENC."
      @ 08,22  SAY "TIPO"
      @ 08,28  SAY "PARC."
      @ 08,34  SAY "CONTA"
      @ 08,42  SAY "VLR. BASE"
      @ 08,60  SAY "TX.MES"
      @ 08,68  SAY "TX.ACUM"
      @ 08,78  SAY "INDICE"
      @ 08,88  SAY "VLR.CORRIGIDO"
      @ 08,108 SAY "RESI.MES"
      @ 08,120 SAY "RESI.ACUM."
      @ 09,00 SAY REPLICATE("=",135)
      LIN = 10
      DO WHILE .NOT. EOF() .AND. LIN <= 56
	 DO WHILE .NOT. EOF() .AND. LIN <= 56
	    @ LIN,02  SAY ALLTRIM(NUM_IMOVEL)
            @ LIN,10  SAY DATA_VENC
            @ LIN,22  SAY TIPO_PARC
            @ LIN,28  SAY ALLTRIM(NUM_PARC)
            @ LIN,34  SAY CONTA      PICT "@R 999"
            @ LIN,42  SAY VLR_ORIGIN PICT "@R 999,999,999.99"
            eTOT_ORIGIN = eTOT_ORIGIN + VLR_ORIGIN
            @ LIN,58  SAY TAXA_MES   PICT "@R 999.9999"
            @ LIN,66  SAY TAXA_ACUM  PICT "@R 999.9999"
            @ LIN,78  SAY INDICE     PICT "@S XXXXXXXXXX"          
            @ LIN,88  SAY VLR_PARC   PICT "@R 999,999,999.99"
            @ LIN,108 SAY RESI_MES   PICT "@R 99,999.99"
            @ LIN,120 SAY RESI_ACUM  PICT "@R 99,999.99"
	    LIN = LIN + 1
	    SKIP
	 ENDDO
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
         @ LIN,42  SAY eTOT_ORIGIN PICT "@R 999,999,999.99"
      ENDIF
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
