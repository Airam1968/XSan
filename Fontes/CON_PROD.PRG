*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_PROD.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE PRODUTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_PROD()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA DE PRODUTO")
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_PROD",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA   = 0
      @ 06,02 SAY sisGRUPO1+"  : "
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
      DBEDIT(6,14,6,75,CAMPO,"FUNC_PROD",PIC,CAB," "," - "," ")
      eQUADRA     = NUM_QUADRA
      eNUM_INICIO = NUM_INICIO
      eNUM_FINAL  = NUM_FINAL
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
      GOTO TOP
      DECLARE CAMPO[10],PIC[10],CAB[10],LENS[10]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5] ="METRAGEM_1"
      CAMPO[6] ="INSCRICA_1"
      CAMPO[7] ="METRAGEM_2"
      CAMPO[8] ="INSCRICA_2"
      CAMPO[9] ="VALOR_1"
      CAMPO[10]="VALOR_2"
      LENS[5] =5
      LENS[6] =10
      LENS[7] =5
      LENS[8] =10
      LENS[9] =19
      LENS[10]=19
      PIC[1] ="@R99999"
      PIC[2] ="@R99999"
      PIC[3] ="@R99999"
      PIC[4] ="@R99999"
      PIC[5] ="@R999,999.999"
      PIC[6] ="@R 99.999.999.99.9"
      PIC[7] ="@R999,999.999"
      PIC[8] ="@R 99.999.999.99.9"
      PIC[9] ="@R 999,999,999.99"
      PIC[10]="@R 999,999,999,99"
      CAB[1] = sisPROD1
      CAB[2] = sisGRUPO1
      CAB[3] = sisUNID1
      CAB[4] ="Status"
      CAB[5] ="1o. M2"
      CAB[6] ="1o. Inscri‡Æo"
      CAB[7] ="2o. M2"
      CAB[8] ="2o. Inscri‡Æo"
      CAB[9] ="1o. Valor"
      CAB[10]="2o. Valor"
      @ 08,00 TO 21,79 DOUBLE
      DBEDIT(9,01,21,78,CAMPO,"F_C_PROD",PIC,CAB,CHR(196),CHR(179),CHR(205))
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = 0
      @ 06,02 SAY sisGRUPO2+"  : "
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
      DBEDIT(6,08,6,76,CAMPO,"FUNC_PROD",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
      GOTO TOP
      DECLARE CAMPO[7],PIC[7],CAB[7],LENS[7]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5]= "METRAGEM_1"
      CAMPO[6]= "INSCRICA_1"
      CAMPO[7]= "VALOR_1"
      LENS[5] = 5
      LENS[6] = 10
      LENS[7] = 19
      PIC[1]  = "@R99999"
      PIC[2]  = "@R99999"
      PIC[3]  = "@R99999"
      PIC[4]  = "@R99999"
      PIC[5]  = "@R999,999.999"
      PIC[6]  = "@R 99.999.999.99.9"
      PIC[7]  = "@R 999,999,999.99"
      CAB[1]  = sisPROD2
      CAB[2]  = sisGRUPO2
      CAB[3]  = sisUNID2
      CAB[4]  = "Status"
      CAB[5]  = "M2"
      CAB[6]  = "Inscri‡Æo"
      CAB[7]  = "Valor Unid."
      @ 08,00 TO 21,79 DOUBLE
      DBEDIT(9,01,21,78,CAMPO,"F_C_PROD",PIC,CAB,CHR(196),CHR(179),CHR(205))
   CASE eTIPO_PRO = 3
      * Consulta de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = 0
      @ 06,02 SAY "Bloco   : "
      GO TOP
      DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      CAB[1]="    "
      CAB[2]="    "
      CAB[3]="    "
      DBEDIT(6,10,6,41,CAMPO,"FUNC_PROD",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
      GOTO TOP
      DECLARE CAMPO[7],PIC[7],CAB[7],LENS[7]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5]="METRAGEM_1"
      CAMPO[6]="INSCRICA_1"
      CAMPO[7]="VALOR_1"
      LENS[5] =5
      LENS[6] =10
      LENS[7] =19
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      PIC[4]="@R99999"
      PIC[5]="@R999,999.999"
      PIC[6]="@R 99.999.999.99.9"
      PIC[7]="@R 999,999,999.99"
      CAB[1]="Empreend."
      CAB[2]="Bloco"
      CAB[3]="Apto"
      CAB[4]="Status"
      CAB[5]="M2"
      CAB[6]="Inscri‡Æo"
      CAB[7]="Valor Unid."
      @ 08,00 TO 21,79 DOUBLE
      DBEDIT(9,01,21,78,CAMPO,"F_C_PROD",PIC,CAB,CHR(196),CHR(179),CHR(205))
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION FUNC_PROD
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_C_PROD
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
	 IF DB_COLUMN > 3
            SAVE SCREEN TO PRODUTO
	    IF F_BLOQR()
	       SET CURSOR ON
	       FLD_NAME = CAMPO[DB_COLUMN]
	       FLD_PIC  = PIC[DB_COLUMN]
	       @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
	       READ
	       SET CURSOR OFF
	       UNLOCK
	    ENDIF
            RESTORE SCREEN FROM PRODUTO
	 ENDIF
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
*************************  fim  *********************************
