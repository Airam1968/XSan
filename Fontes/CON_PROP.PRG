*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_PROP.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE PRODUTOS EM PROCESSO JUDICIAL
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_PROP()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA DE UNIDADES EM PROCESSO")
SELE 1
USE UNI_IMOB INDEX UNI_IMOB ALIAS S1
GO TOP
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_PROP",PIC,CAB," "," - "," ")
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
      @ 06,02 SAY sisGRUPO1+" : "
      GO TOP
      DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      PIC[4]="@S99999"
      CAB[1]="    "
      CAB[2]="    "
      CAB[3]="    "
      CAB[4]="    "
      DBEDIT(6,14,6,45,CAMPO,"FUNC_PROP",PIC,CAB," "," - "," ")
      eQUADRA     = NUM_QUADRA
      eNUM_INICIO = NUM_INICIO
      eNUM_FINAL  = NUM_FINAL
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA && .AND. (PROCESSO_1 .OR. PROCESSO_2)
      GOTO TOP
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Aten‡Æo !",3)
		 PMENSAG("NÆo H  Unidades Dispon¡veis !")
	 PPARA()
	 CLOSE DATABASES
	 RESTORE SCREEN FROM TELA03
	 RETURN nil
      ENDIF
      DECLARE CAMPO[8],PIC[8],CAB[8],LENS[8]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5]="PROPRIET_1"
      CAMPO[6]="PROCESSO_1"
      CAMPO[7]="PROPRIET_2"
      CAMPO[8]="PROCESSO_2"
      LENS[5]=5
      LENS[6]=1
      LENS[7]=5
      LENS[8]=1
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
      CAB[6]="Processo 1o."
      CAB[7]="2o. Proprietario"
      CAB[8]="Processo 2o."
      @ 08,00 TO 21,79 DOUBLE
      DBEDIT(9,01,21,78,CAMPO,"FUNC_PROP",PIC,CAB,CHR(196),CHR(179),CHR(205))
   CASE eTIPO_PRO = 2
      * Alteracao de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = 0
      @ 06,02 SAY sisGRUPO2+"   : "
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
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO && .AND. PROCESSO_1
      GOTO TOP
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Aten‡Æo !",3)
		 PMENSAG("NÆo H  Unidades Dispon¡veis !")
	 PPARA()
	 CLOSE DATABASES
	 RESTORE SCREEN FROM TELA03
	 RETURN nil
      ENDIF
      DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5]="PROPRIET_1"
      CAMPO[6]="PROCESSO_1"
      LENS[5]=5
      LENS[6]=1
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      PIC[4]="@R99999"
      PIC[5]="@R99999"
      PIC[5]="@R99999"
      CAB[1]=" Produto "
      CAB[2]="Grupo"
      CAB[3]="Unid."
      CAB[4]="Status"
      CAB[5]="Proprietario"
      CAB[6]="Processo"
      @ 08,00 TO 21,79 DOUBLE
      DBEDIT(9,01,21,78,CAMPO,"FUNC_PROP",PIC,CAB,CHR(196),CHR(179),CHR(205))
   CASE eTIPO_PRO = 3
      * Alteracao de Salas Comerciais
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
      DBEDIT(6,10,6,41,CAMPO,"FUNC_PROP",PIC,CAB," "," - "," ")
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. PROCESSO_1
      GOTO TOP
      IF EOF()
	 PLQUA("01")
	 PMSGR( "Aten‡Æo !",3)
	 PMENSAG("NÆo H  Imoveis Dispon¡veis !")
	 PPARA()
	 CLOSE DATABASES
	 RESTORE SCREEN FROM TELA03
	 RETURN nil
      ENDIF
      DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5]="PROPRIET_1"
      CAMPO[6]="PROCESSO_1"
      LENS[5]=5
      LENS[6]=1
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      PIC[4]="@R99999"
      PIC[5]="@R99999"
      PIC[5]="@R99999"
      CAB[1]="Empreend."
      CAB[2]="Bloco"
      CAB[3]="Sala"
      CAB[4]="Status"
      CAB[5]="Proprietario"
      CAB[6]="Processo"
      @ 08,00 TO 21,79 DOUBLE
      SET COLOR TO
      SET COLOR TO W/N+*
      @ 23,00 SAY "[ENTER]"
      @ 23,21 SAY "[ESC]"
      @ 23,37 SAY "[CTRL-W]"
      @ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
      SET COLOR TO
      SET COLOR TO W/N,+W/R
      @ 23,07 SAY "Para Alterar"
      @ 23,26 SAY "Para Sair"
      @ 23,45 SAY "Para Salvar"
      @ 23,65 SAY "Movimenta‡Æo"
      DBEDIT(9,01,21,78,CAMPO,"FUNC_PROP",PIC,CAB,CHR(196),CHR(179),CHR(205))
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION FUNC_PROP
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
	 DO CASE
	    CASE DB_COLUMN = 5
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
	    CASE DB_COLUMN = 6
	       SAVE SCREEN TO T_PROC1
               @ 09,01 CLEAR TO 21,79
               @ 09,01 TO       21,79
			   PCENTRA( " Observacoes Sobre o Processo de Cobran‡a ",09)
               SET COLOR TO
               SET COLOR TO W/N+*
               @ 23,00 SAY "[ENTER]"
               @ 23,21 SAY "[ESC]"
               @ 23,37 SAY "[CTRL-W]"
               @ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
               SET COLOR TO
               SET COLOR TO W/N,+W/R
               @ 23,07 SAY "Para Alterar"
               @ 23,26 SAY "2X Para Sair"
               @ 23,45 SAY "Para Salvar"
               @ 23,65 SAY "Movimenta‡Æo"
	       IF F_BLOQR()
                  SET CURSOR ON
                  eOLD = S1->OBS_PROC_1
                  eNEW = MEMOEDIT(eOLD,10,02,20,78,.T.,"F_MEDIT")
                  REPLACE OBS_PROC_1 WITH eNEW
                  SET CURSOR OFF
                  UNLOCK
	       ENDIF
	       PPARA()
	       RESTORE SCREEN FROM T_PROC1
	    CASE DB_COLUMN = 7
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
	    CASE DB_COLUMN = 8
	       SAVE SCREEN TO T_PROC2
               @ 09,01 CLEAR TO 21,79
               @ 09,01 TO       21,79
			   PCENTRA( " Observacoes Sobre o Processo de Cobran‡a ",09)
	       IF F_BLOQR()
                  SET CURSOR ON
                  eOLD = S1->OBS_PROC_2
                  eNEW = MEMOEDIT(eOLD,10,02,20,78,.T.,"F_MEDIT")
                  REPLACE OBS_PROC_2 WITH eNEW
                  SET CURSOR OFF
		  UNLOCK
	       ENDIF
	       PPARA()
	       RESTORE SCREEN FROM T_PROC2
	    OTHERWISE
	       RET = 0
	 ENDCASE
      ENDCASE
ENDIF
RETURN RET
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_MEDIT
PARAMETERS mode, line, column
PRIVATE keypress, ret2
RET2 = 0
KEYPRESS = LASTKEY()
IF MODE == 1 .OR. MODE == 2
   DO CASE
      CASE KEYPRESS == 273
         RET2 = 34
      CASE KEYPRESS == -4
         RET2 = 1
      CASE KEYPRESS == -5
         RET2 = 6
      CASE KEYPRESS == -8
         RET2 = 25
      CASE KEYPRESS == 301
         RET2 = 23
   ENDCASE
ENDIF
RETURN RET2
*************************  fim  *********************************
