*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_ESMI.PRG
* Data        : 01/01/99
* Objetivo    : Consulta a banco de dados de estoque Minimo
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_ESMI()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA A ESTOQUE MÖNIMO")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   PLQUA("01")
   PMENSAG("Aguarde, Elaborando consulta de Ötens com estoque m¡nimo ...")
   SELE 3
   USE TIPO INDEX TIPO
   GOTO TOP
   SELE 2
   USE STATUS
   GOTO TOP
   eULT_ITEM = ULT_ITEM
   SELE 4
   USE ITENS INDEX ITENS ALIAS S4
   GOTO TOP
   SELE 1
   USE ESTOQUE INDEX ESTOQUE ALIAS S1
   SET RELATION TO CODIGO INTO S4
   GOTO TOP
   SELE 5
   USE ESTO_MIN INDEX ESTO_MIN
   ZAP
   SELE 1
   GOTO TOP
   DO WHILE .NOT. EOF()
      IF S4->MINIMO >= SALDO_FINA
	 SELE 5
	 APPEND BLANK
	 REPLACE CODIGO     WITH S1->CODIGO,;
		 DESCRICAO  WITH S4->DESCRICAO,;
                 MINIMO     WITH S4->MINIMO
	 REPLACE SALDO_FINA WITH S1->SALDO_FINA
	 SELE 1
      ENDIF
      SKIP
   ENDDO
   SELE 5
   GOTO TOP
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1] ="@R 999999"
   PIC[2] ="@S40"
   PIC[3] ="@R 999,999"
   PIC[4] ="@R 999,999"
   CAB[1] ="C¢digo"
   CAB[2] ="DESCRICAO"
   CAB[3] ="MINIMO"
   CAB[4] ="SALDO ATUAL"
   @ 5,00 TO 21,79 DOUBLE
   PLQUA("01")
   SET COLOR TO
   SET COLOR TO W/N+*
   @ 23,00 SAY "[ESC]"
   SET COLOR TO
   SET COLOR TO W/N,+W/R
   @ 23,06 SAY "Para Sair"
   DBEDIT(06,01,21,78,CAMPO,"F_ESMI",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
**************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_ESMI
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
