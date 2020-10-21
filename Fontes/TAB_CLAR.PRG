*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_CLAR.PRG
* Data        : 01/01/99
* Objetivo    : Tabela de Classe de Receita\Despesa
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_CLAR()
SAVE SCREEN TO TELA03
PTIT( "TABELA TIPO DE RECEITA")
PLQUA("01")
PPERG ( "Deseja Consulta por Ordem Alfab‹¨«tica (S/N) ? ")
IF sisRESP = "N"
   SELE 1
   USE CLASSE_R INDEX CLASSE_R,TEMP_CLR
   GOTO TOP
   eINCLUI = .T.
ELSE
   SELE 1
   USE CLASSE_R INDEX TEMP_CLR,CLASSE_R
   GOTO TOP
   eINCLUI = .F.
ENDIF
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="@R 9999999"
PIC[2]="@S40"
PIC[3]="@S15"
CAB[1]="C‹¨«digo"
CAB[2]="Descri‹¨«‹¨«o"
CAB[3]="Conta Contabil"
@ 6,01 TO 19,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ENTER]"
@ 23,21 SAY "[ESC]"
@ 23,37 SAY "[ESPA€O]"
@ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,07 SAY "Para Alterar"
@ 23,26 SAY "Para Sair"
@ 23,45 SAY "Para Incluir"
@ 23,65 SAY "Movimenta‡Æo"
DBEDIT(7,02,19,78,CAMPO,"F_CLAS",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
