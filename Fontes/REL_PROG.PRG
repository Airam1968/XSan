*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_PROG.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE PRODUTOS GERAL
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_PROG()
SAVE SCREEN TO TELA03
PTIT( "RELATORIO DE UNIDADES CADASTRO GERAL")
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
      eQUADRA   = 0
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
      eBLOCO    = 0
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
      DBEDIT(6,10,6,41,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
      GOTO TOP
   OTHERWISE
      * Relatorio de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
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
GOTO TOP
   IF EOF()
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("N∆o H† Dados Neste Per°odo !")
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
   ENDIF
eTOTAL = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN007"
   SELE 6
   SEEK eTIPO_PRO
   eTIPO_PRD = DESCRICAO
   SELE 1
   @ 03,40  SAY sisLETRAE+"RELATORIO DE UNIDADES GERAL"+sisLETRAC
   @ 04,00  SAY "Produto : "+STR(eCLAS_PRO)+" - "+ALLTRIM(eDESC_PRO)+" - Quadra\Bloco : "+ALLTRIM(eQUADRA)+"\"+ALLTRIM(eBLOCO)
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "No. UNIDADE"
   @ 06,15  SAY "STATUS"
   @ 06,30  SAY "METRAGEM"
   @ 06,40  SAY "INSCRICAO IMOBILIARIA"
   @ 06,65  SAY "NOME PROPRIETARIO"
   @ 06,105 SAY "VALOR"
   @ 06,120 SAY "ESCRITURA"
   @ 07,00 SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      @ LIN,02  SAY NUM_IMOVEL PICT "@R 99999"
      IF STATUS = 0
         @ LIN,15  SAY "Unico"
      ELSE
         @ LIN,15  SAY "1o.Prop."
      ENDIF
      @ LIN,30  SAY METRAGEM_1 PICT "@R9.999,99"
      @ LIN,39  SAY "M2"
      SELE 1
      @ LIN,40  SAY INSCRICA_1 PICT "@R 99.999.999.99.9"
      ePROPRIET_1 = PROPRIET_1
      SELE 5
      SEEK ePROPRIET_1
      @ LIN,65  SAY CLIENTE
      SELE 1
      @ LIN,105 SAY VALOR_1 PICT "@R 999,999.99"
      eTOTAL = eTOTAL + VALOR_1
      @ LIN,125 SAY ESCRITU_01 PICT "@! X"
      LIN = LIN + 1
      IF STATUS <> 0 
         @ LIN,02  SAY NUM_IMOVEL PICT "@R 99999"
         @ LIN,15  SAY "2o.Prop."
         @ LIN,30  SAY METRAGEM_2 PICT "@R9.999,99"
         @ LIN,39  SAY "M2"
         @ LIN,40  SAY INSCRICA_2 PICT "@R 99.999.999.99.9"
         ePROPRIET_2 = PROPRIET_2
         SELE 5
         SEEK ePROPRIET_2
         @ LIN,65  SAY CLIENTE
         SELE 1
         @ LIN,105 SAY VALOR_2 PICT "@R 999,999.99"
         eTOTAL = eTOTAL + VALOR_2
         @ LIN,125 SAY ESCRITU_02 PICT "@! X"
         LIN = LIN + 1
      ENDIF
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
      @ LIN,105 SAY eTOTAL PICT "@R 999.999,99"
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
