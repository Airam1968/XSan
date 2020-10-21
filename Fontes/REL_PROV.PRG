*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_PROV.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE UNIDADES VENDIDAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_PROV()
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
USE CAD_VEND INDEX CAD_VEND,DATA_VEN
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
   eBLOCO    = SPACE(1)
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
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
      GOTO TOP
   CASE eTIPO_PRO = 2
      * Relatorio de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = SPACE(5)
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
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
      GOTO TOP
   OTHERWISE
      * Relatorio de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eUNIDADES = 0
eTOT_M2   = 0
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"EXTRATO.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "EXTRATO.TXT"+SPACE(10) 
ENDIF     

SELE 1
GOTO TOP
PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
PPARA()
DO PIMPR
SET DEVICE TO PRINT
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN003"
   SELE 6
   SEEK eTIPO_PRO
   eTIPO_PRD = DESCRICAO
   SELE 1
   @ 03,40  SAY sisLETRAE+"RELATORIO DE UNIDADES VENDIDAS"+sisLETRAC
   @ 04,00  SAY "Produto : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+" - Quadra\Bloco : "+ALLTRIM(eQUADRA)+"\"+ALLTRIM(eBLOCO)
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "No. IMOVEL"
   @ 06,15  SAY "STATUS"
   @ 06,25  SAY "NOME COMPRADOR"
   @ 06,75  SAY "DATA VENDA"
   @ 06,87  SAY "QUITADO"
   @ 06,97  SAY "TELEFONE"
   @ 07,00 SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF PROPRIET_1 <> 1
         @ LIN,02  SAY NUM_IMOVEL PICT "@R 99999"
         eNUM_IMOVEL = NUM_IMOVEL
         IF STATUS = 0
            @ LIN,15  SAY "Unico"
            ePARTE = 0
         ELSE
            @ LIN,15  SAY "1o.Prop."
            ePARTE = 1
         ENDIF
         ePROPRIET_1 = PROPRIET_1
         SELE 5
         SEEK ePROPRIET_1
         @ LIN,25  SAY CLIENTE
         IF eTIPO_PRO = 1
            SELE 7
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            @ LIN,75 SAY DATA_VENDA
         ELSE
            SELE 7
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eBLOCO+eNUM_IMOVEL+STR(ePARTE,1)
            @ LIN,75 SAY DATA_VENDA
         ENDIF
         SELE 1
         @ LIN,87  SAY IIF(QUITADO_1,"Sim","Nao")
         SELE 5
         IF OPCAO = "R"
            @ LIN,97  SAY TEL_R_1 PICT "@R (XX) XXXXX-XXXXX"
            @ LIN,118 SAY "R"
         ELSE
            @ LIN,97  SAY TEL_C_1 PICT "@R (XX) XXXXX-XXXXX"
            @ LIN,118 SAY "C"
         ENDIF
         SELE 1
         LIN = LIN + 1
         eUNIDADES = eUNIDADES + 1
      ENDIF
      IF STATUS <> 0 .AND. PROPRIET_2 <> 1
         @ LIN,02  SAY NUM_IMOVEL PICT "@R 99999"
         eNUM_IMOVEL = NUM_IMOVEL
         ePARTE      = 2
         @ LIN,15  SAY "2o.Prop."
         ePROPRIET_2 = PROPRIET_2
         SELE 5
         SEEK ePROPRIET_2
         @ LIN,25  SAY CLIENTE
         IF eTIPO_PRO = 1
            SELE 7
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
            @ LIN,75 SAY DATA_VENDA
         ELSE
            SELE 7
            GOTO TOP
            SEEK STR(eCLAS_PRO,5)+eBLOCO+eNUM_IMOVEL+STR(ePARTE,1)
            @ LIN,75 SAY DATA_VENDA
         ENDIF
         SELE 1
         @ LIN,87  SAY IIF(QUITADO_2,"Sim","Nao")
         SELE 5
         IF OPCAO = "R"
            @ LIN,97  SAY TEL_R_1 PICT "@R (XX) XXXXX-XXXXX"
            @ LIN,118 SAY "R"
         ELSE
            @ LIN,97  SAY TEL_C_1 PICT "@R (XX) XXXXX-XXXXX"
            @ LIN,118 SAY "C"
         ENDIF
         SELE 1
         LIN = LIN + 1
         eUNIDADES = eUNIDADES + 1
      ENDIF
      SKIP
      DO PTECLA
      IF sisINTERR
         SET DEVICE TO SCREEN
         CLOSE ALL
         RETURN nil
      ENDIF
      IF EOF()
         LIN = LIN + 3
         @ LIN,25 SAY "Total de Unidades : "+STR(eUNIDADES,7)
      ENDIF
   ENDDO
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
