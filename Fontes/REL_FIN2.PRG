*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_FIN2.PRG
* Data        : 02/04/2001
* Objetivo	: RELATORIO DE POSIá∆o DE FINANCIAMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_FIN2()
SAVE SCREEN TO TELA03
PTIT( "RELATORIO DE POSIÄ«O DE FINANCIAMENTOS")
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   eDESC_PRO = DESCRICAO
   EXIT
ENDDO
eCLAS_PRO = STR(eCLAS_PRO,5)
PLQUA("01")
PPERG ( "Deseja Efetuar a Consolidaá∆o do Arquivo de Saldo (S/N) ? ")
IF sisRESP = "S"
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("Aguarde a montagem do Banco de Dados para Impress∆o ...")
   IF eTIPO_PRO = 1
      eTIPOREL = SPACE(1)
      PLQUA("01")
      @ 23,00 SAY "Relatário (T)odos, (I)nteiros ou (M)eios Lotes ? " GET eTIPOREL PICT "@!X" VALID eTIPOREL $("TIM")
      READ
      SELE 8
      SET EXCLUSIVE ON
      USE TEMP_PAR
      IF F_BLOQA()
         ZAP
         UNLOCK
      ENDIF
      SET EXCLUSIVE OFF
      SELE 3
      SET FILTER TO NUM_LOTEAM = eCLAS_PRO .AND. CADASTRO 
      GOTO TOP
      DO WHILE .NOT. EOF()
         eIND_CLAS = eCLAS_PRO
         eIND_QUAD = NUM_QUADRA
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         APPEND FROM &eARQ 
         GOTO TOP
         SELE 3
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
      GOTO TOP
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
      SET EXCLUSIVE OFF
   ELSE
      SELE 8
      SET EXCLUSIVE ON
      USE TEMP_PAR
      IF F_BLOQA()
         ZAP
         UNLOCK
      ENDIF
      SET EXCLUSIVE OFF
      SELE 4
      SET FILTER TO NUM_LOTEAM = eCLAS_PRO .AND. CADASTRO 
      GOTO TOP
      DO WHILE .NOT. EOF()
         eIND_CLAS = eCLAS_PRO
         eIND_QUAD = NUM_BLOCO
         SELE 8
         USE TEMP_PAR
         GOTO TOP
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         APPEND FROM &eARQ
         GOTO TOP
         SELE 4
         SKIP
      ENDDO
      SET EXCLUSIVE ON
      SELE 8
      USE TEMP_PAR
      INDEX ON CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1) TO TEMP_PAR
      GOTO TOP
      USE TEMP_PAR INDEX TEMP_PAR
      GOTO TOP
      SET EXCLUSIVE OFF
   ENDIF
ENDIF
SELE 8
IF F_BLOQA()
   PACK
   UNLOCK
ENDIF
GOTO TOP
IF EOF()
   PLQUA("01")
   PMSGR( "Atená∆o !",3)
   PMENSAG("N∆o H† Devedores Neste Per°odo !")
   PPARA()
ELSE
	eARQUIVO = SPACE(60)
	IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
		eARQUIVO = sisDRIVE_REL+":"
	ELSE
		eARQUIVO = "C:"
	ENDIF	
	
	IF sisPASTA_REL <> SPACE(30)
		eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"DEVEDORES.TXT"+SPACE(10)  
	ELSE
		eARQUIVO = ALLTRIM(eARQUIVO) + "DEVEDORES.TXT"+SPACE(10) 
	ENDIF    
   eCONTRATO = 0
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
   eSUB_SD = 0
   eSUB_SC = 0
   eSUB_DI = 0
   eTOT_SD = 0
   eTOT_SC = 0
   eTOT_DI = 0
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SAN004"
      @ 03,20  SAY sisLETRAE+"RELATORIO DE POSIá∆o DE FINANCIAMENTOS"+sisLETRAC
      @ 04,00  SAY "Produto : "+eCLAS_PRO+" - "+ALLTRIM(eDESC_PRO)
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,00  SAY "No. IMOVEL"
      @ 06,15  SAY "NOME CLIENTE"
      @ 06,60  SAY "VLR. VENDA"
      @ 06,75  SAY "DATA VENDA"
      @ 06,90  SAY "SD. CREDOR"
      @ 06,105 SAY "SD. DEVEDOR"
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 8
      DO WHILE .NOT. EOF() .AND. LIN <= 55
         SELE 8
         IF eTIPO_PRO = 1
            eCLAS_PRO   = CLASSE
            eQUADRA     = QUADRA
            eNUM_IMOVEL = NUM_IMOVEL
            ePARTE      = PARTE
         ELSE
            eCLAS_PRO   = CLASSE
            eQUADRA     = QUADRA
            eNUM_IMOVEL = NUM_IMOVEL
            ePARTE      = PARTE
         ENDIF
         SELE 6
         SEEK eCLAS_PRO+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
         eNUM_CLIE   = NUM_CLIE
         eVLR_VENDA  = SINAL+VLR_PARCEL+INTERMEDIA
         eDATA_VENDA = DATA_VENDA
         SELE 5
         SEEK eNUM_CLIE
         SELE 8
         eSUB_SD = 0
         eSUB_SC = 0
         DO WHILE (CLASSE = eCLAS_PRO .AND. eQUADRA = QUADRA .AND. eNUM_IMOVEL = NUM_IMOVEL .AND. ePARTE = PARTE) .AND. (.NOT. EOF())
            IF DATA_PGTO = CTOD("")
               eSUB_SD = eSUB_SD + VLR_PARC
            ELSE
               eSUB_SC = eSUB_SC + VLR_PARC
            ENDIF
            SKIP
         ENDDO
         @ LIN,02  SAY STR(ePARTE,1)+"/"+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)
         SELE 5
         @ LIN,15  SAY CLIENTE
         @ LIN,60  SAY eVLR_VENDA PICT "@R 999,999,999.99"
         @ LIN,75  SAY eDATA_VENDA
         @ LIN,90  SAY eSUB_SC PICT "@R 999,999,999.99"
         @ LIN,105 SAY eSUB_SD PICT "@R 999,999,999.99"
         LIN = LIN + 1
         SELE 8
         eTOT_SD = eTOT_SD + eSUB_SD
         eTOT_SC = eTOT_SC + eSUB_SC
         DO PTECLA
         IF sisINTERR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF
      ENDDO
      IF EOF()
         LIN = LIN + 1
         @ LIN,02  SAY "Total Geral    : "
         @ LIN,90  SAY eTOT_SC   PICT "@R 999,999,999.99"
         @ LIN,105 SAY eTOT_SD   PICT "@R 999,999,999.99"
      ENDIF
      SELE 8
      @ 57,00 SAY REPLICATE("=",135)
      @ 58,104 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
   ENDDO
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
