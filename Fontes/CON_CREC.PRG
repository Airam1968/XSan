*********************************************************************
* Sistema     : Gest�o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CREC.PRG
* Data        : 01/01/99
* Objetivo    : Consulta dos Lan�amentos do Contas a Receber
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_CREC()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA A LAN�AMENTOS DO CONTAS A RECEBER")
SELE 1
USE CRECEBER INDEX DATA_CRE,CRECEBER
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[7],PIC[7],CAB[7],LENS[7]
AFIELDS(CAMPO,.T.,LENS)
PIC[1] ="@R 99999999"
PIC[2] ="  /  /  "
PIC[3] ="@R 99999999"
PIC[4] ="@S50"
PIC[5] ="@R 999,999,999,999.99"
PIC[6] ="X"
PIC[7] ="99"
CAB[1] ="Lan�amento"
CAB[2] ="Data"
CAB[3] ="Fornecedor"
CAB[4] ="Descri��o"
CAB[5] ="Valor"
CAB[6] ="Situa��o"
CAB[7] ="Tipo"
@ 6,00 TO 20,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ESC]"
@ 23,20 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,05 SAY "Para Alterar"
@ 23,26 SAY "Movimenta��o"
DBEDIT(7,01,20,78,CAMPO,"F_CONS",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
