*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_ESTO.PRG
* Data        : 01/01/99
* Objetivo    : Consulta a banco de dados de estoque 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_ESTO()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA A ESTOQUE")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
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
   USE ESTOQUE INDEX ESTOQUE
   SET RELATION TO CODIGO INTO S4
   GOTO TOP
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1] ="@R 999999"
   PIC[2] ="@R 999,999"
   PIC[3] ="@R 999,999"
   PIC[4] ="@R 999,999"
   PIC[5] ="@R 999,999"
   PIC[6] ="  /  /  "
   CAB[1] ="C¢digo"
   CAB[2] ="INICIAL (+)"
   CAB[3] ="ENTRADAS (+)"
   CAB[4] ="SAIDAS (-)"
   CAB[5] ="FINAL (=)"
   CAB[6] ="ULT.MOVIMENTAá∆o"
   @ 5,00 TO 21,79 DOUBLE
   PLQUA("01")
   SET COLOR TO
   SET COLOR TO W/N+*
   @ 23,00 SAY "[ESC]"
   SET COLOR TO
   SET COLOR TO W/N,+W/R
   @ 23,06 SAY "Para Sair"
   DBEDIT(06,01,21,78,CAMPO,"F_ESTO",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
**************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_ESTO
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
