*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_SIGM.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE DADOS DE ARQUIVO SIGMA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_SIGM()
SAVE SCREEN TO TELA04
PTIT( "CONSULTA DE DADOS ÖTAU-SIGMA")
SELE 1
USE TRANSTXT
GO TOP
PLQUA("11")
SELE 1
GOTO TOP
@ 5,00 TO 21,79 DOUBLE
DECLARE CAMPO[40],PIC[40],CAB[40],LENS[40]
AFIELDS(CAMPO,.T.,LENS)
*PIC[1]="@S50"
*PIC[2]="@S1"
*PIC[3]="  /  /  "
*CAB[1]="NOME"
*CAB[2]="SELECAO"
*CAB[3]="NASCIMENTO"
DBEDIT(6,01,21,78,CAMPO,"F_SIGMA",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_SIGMA
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
	 IF DB_COLUMN > 1
	    IF F_BLOQR()
	       SET CURSOR ON
	       FLD_NAME = CAMPO[DB_COLUMN]
	       FLD_PIC  = PIC[DB_COLUMN]
	       @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
	       READ
	       SET CURSOR OFF
	       UNLOCK
	    ENDIF
	 ENDIF
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************

