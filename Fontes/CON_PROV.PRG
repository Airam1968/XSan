*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_PROV.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE PRODUTOS VENDIDOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_PROV()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA DE UNIDADES VENDIDAS")
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
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
      @ 06,02 SAY sisUNID1+" : "
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
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Atená∆o !",3)
         PMENSAG("N∆o H† "+sisUNID1+" Vendidos !")
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
      CAB[1]=" Produto "
      CAB[2]="Grupo"
      CAB[3]="Unid"
      CAB[4]="Status"
      CAB[5]="1o. Proprietario"
      CAB[6]="2o. Proprietario"
      @ 08,00 TO 21,79 DOUBLE
      DBEDIT(9,01,21,78,CAMPO,"FUNC_PROV",PIC,CAB,CHR(196),CHR(179),CHR(205))
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = 0
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
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Atená∆o !",3)
         PMENSAG("N∆o H† "+sisUNID2+" Vendidos !")
	 PPARA()
         CLOSE DATABASES
         RESTORE SCREEN FROM TELA03
         RETURN nil
      ENDIF
      DECLARE CAMPO[5],PIC[5],CAB[5],LENS[5]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5]="PROPRIET_1"
      LENS[5] =5
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      PIC[4]="@R99999"
      PIC[5]="@R99999"
      CAB[1]=" Produto "
      CAB[2]="Grupo"
      CAB[3]="Unid."
      CAB[4]="Status"
      CAB[5]="Proprietario"
      @ 08,00 TO 21,79 DOUBLE
      DBEDIT(9,01,21,78,CAMPO,"FUNC_PROV",PIC,CAB,CHR(196),CHR(179),CHR(205))
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
      DBEDIT(6,10,6,41,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
      GOTO TOP
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Atená∆o !",3)
	 PMENSAG("N∆o H† Imoveis Vendidos !")
	 PPARA()
         CLOSE DATABASES
         RESTORE SCREEN FROM TELA03
         RETURN nil
      ENDIF
      DECLARE CAMPO[5],PIC[5],CAB[5],LENS[5]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5]="PROPRIET_1"
      LENS[5] =5
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      PIC[4]="@R99999"
      PIC[5]="@R99999"
      CAB[1]="Empreend."
      CAB[2]="Bloco"
      CAB[3]="Sala"
      CAB[4]="Status"
      CAB[5]="Proprietario"
      @ 08,00 TO 21,79 DOUBLE
      DBEDIT(9,01,21,78,CAMPO,"FUNC_PROV",PIC,CAB,CHR(196),CHR(179),CHR(205))
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION FUNC_PROV
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         IF DB_COLUMN = 5
            SAVE SCREEN TO T_PROP1
            ePROPRIET_1 = PROPRIET_1
            SELE 5
            SEEK ePROPRIET_1
            @ 12,13 CLEAR TO 14,67
            @ 12,13 TO       14,67 DOUBLE
			PCENTRA( "Nome do Proprietario No. 1",12)
            @ 13,15 GET CLIENTE
            CLEAR GETS
            SELE 1
            PPARA()
            RESTORE SCREEN FROM T_PROP1
         ELSE
            IF DB_COLUMN = 6
               SAVE SCREEN TO T_PROP2
               ePROPRIET_2 = PROPRIET_2
               SELE 5
               SEEK ePROPRIET_2
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Nome do Proprietario No. 2",12)
               @ 13,15 GET CLIENTE
               CLEAR GETS
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP2
            ELSE
               RET = 0
            ENDIF
         ENDIF
   ENDCASE
ENDIF
RETURN RET
*************************  fim  *********************************
