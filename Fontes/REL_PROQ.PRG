*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_PROQ.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE UNIDADES QUITADAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_PROQ()
SAVE SCREEN TO TELA03
PTIT( "RELATORIO DE UNIDADES QUITADAS")
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
USE CAD_VEND INDEX CAD_VEND
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
eQUADRA  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA     = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar "+sisGRUPO1+" (S/N) ? ")
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
      ENDIF
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      eNUM_IMOVEL= SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar "+sisGRUPO2+" (S/N) ? ")
      IF sisRESP = "S"
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
         DBEDIT(6,08,6,76,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
         eQUADRA      = NUM_BLOCO
         eQUANTIDADE = QUANTIDADE
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
PLQUA("01")
PPERG ( "Deseja so quitados (S/N) ? ")
IF sisRESP = "S"
   IF eQUADRA <> SPACE(5)
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. (QUITADO_1 = .T. .OR. QUITADO_2 = .T.)
      GOTO TOP
   ELSE
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. (QUITADO_1 = .T. .OR. QUITADO_2 = .T.)
      GOTO TOP
   ENDIF
   eQUITADO = .T.
ELSE
   IF eQUADRA <> SPACE(5)
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA 
      GOTO TOP
   ELSE
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5)
      GOTO TOP
   ENDIF
   eQUITADO = .F.
ENDIF
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
   @ 02,115 SAY "Relat: SAN005"
   SELE 6
   SEEK eTIPO_PRO
   eTIPO_PRD = DESCRICAO
   SELE 1
   @ 03,40  SAY sisLETRAE+"RELATORIO DE UNIDADES QUITADAS"+sisLETRAC
   @ 04,00  SAY "Produto : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)+" - Quadra\Bloco : "+ALLTRIM(eQUADRA)+"\"+ALLTRIM(eBLOCO)
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "IMOVEL"
   @ 06,10  SAY "NOME COMPRADOR"
   @ 06,52  SAY "QUITADO"
   @ 06,63  SAY "VENDA"
   @ 06,76  SAY "ESCRIT."
   @ 06,92  SAY "CARTORIO"
   @ 06,112 SAY "LIVRO"
   @ 06,119 SAY "FOLHA"
   @ 06,126 SAY "CONTRATO"
   @ 07,00 SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF PROPRIET_1 <> 1 .AND. IIF(eQUITADO,QUITADO_1 = .T.,(QUITADO_1 = .T. .OR. QUITADO_1 = .F.))
         @ LIN,01  SAY ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/"+ALLTRIM(STR(STATUS,1))
         ePROPRIET_1 = PROPRIET_1
         eQUADRA     = QUADRA
         eNUM_IMOVEL = NUM_IMOVEL
         ePARTE      = IIF(STATUS = 0,0,1)
         SELE 5
         SEEK ePROPRIET_1
         @ LIN,10  SAY SUBSTR(CLIENTE,1,39)
         SELE 7
         SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
         @ LIN,52  SAY "[Sim][Nao]"
         @ LIN,63  SAY DATA_VENDA 
         SELE 1
         @ LIN,76  SAY ESCRITU_01
         @ LIN,87  SAY CARTORIO_1  PICT "@S20"
         @ LIN,112 SAY LIVRO_1     PICT "99999"
         @ LIN,118 SAY FOLHA_1     PICT "99999"
         @ LIN,125 SAY "[Sim][Nao]"
         LIN = LIN + 1
      ENDIF
      IF STATUS <> 0 .AND. PROPRIET_2 <> 1 .AND. IIF(eQUITADO,QUITADO_2 = .T.,(QUITADO_2 = .T. .OR. QUITADO_2 = .F.))
         @ LIN,01  SAY ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)+"/2"
         ePROPRIET_2 = PROPRIET_2
         eQUADRA     = QUADRA
         eNUM_IMOVEL = NUM_IMOVEL
         ePARTE      = 2
         SELE 5
         SEEK ePROPRIET_2
         @ LIN,10  SAY SUBSTR(CLIENTE,1,39)
         SELE 7
         SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
         @ LIN,55  SAY "[Sim][Nao]"
         @ LIN,65  SAY DATA_VENDA
         SELE 1
         @ LIN,78  SAY ESCRITU_02
         @ LIN,87  SAY CARTORIO_2  PICT "@S20"
         @ LIN,112 SAY LIVRO_2     PICT "@R 99999"
         @ LIN,118 SAY FOLHA_2     PICT "@R 99999"
         @ LIN,125 SAY "[Sim][Nao]"
         LIN = LIN + 1
      ENDIF
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
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
