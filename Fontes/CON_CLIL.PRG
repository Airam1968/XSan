*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CLIL.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE CLIENTES 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_CLIL()
SAVE SCREEN TO TELA03
PTIT("CONSULTA DE CLIENTES CADASTRADOS")
PLQUA("01")
PPERG("Deseja Consultar em Ordem Alfab‚tica (S/N) ? ")
IF sisRESP = "S"
   SELE 1
   USE CONTATO INDEX CONTATO,NUM_CLIE
   GO TOP
ELSE
   SELE 1
   USE CONTATO INDEX NUM_CLIE,CONTATO
   GOTO TOP
ENDIF
PLQUA("11")
SELE 1
GOTO TOP
@ 6,00 TO 20,79 DOUBLE
DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
AFIELDS(CAMPO,.T.,LENS)
CAMPO[1] = "NUM_CLIE"
LENS[1]  = 10
CAMPO[2] = "CLIENTE"
LENS[2]  = 50
CAMPO[3] = "CPF_CGC"
LENS[3]  = 18
CAMPO[4] = "MAIL_PROP"
LENS[4]  = 50
CAMPO[5] = "MAIL_C"
LENS[5]  = 50
CAMPO[6] = "MAIL_CONJ"
LENS[6]  = 50
PIC[1]="@R 99999"
PIC[2]="@S50"
PIC[3]="@S20"
PIC[4]="@S50"
PIC[5]="@S50"
PIC[6]="@S50"
CAB[1]="CODIGO"
CAB[2]="NOME"
CAB[3]="CPF/CGC"
CAB[4]="EMAIL PROPRIETµRIO PESSOAL"
CAB[5]="EMAIL PROPRIETµRIO COMERCIAL"
CAB[6]="EMAIL PROPRIETµRIO CONJUGE"
**DBEDIT(7,01,20,78,CAMPO,"KEYS_4",PIC,CAB,CHR(196),CHR(179),CHR(205))
DBEDIT(7,01,20,78,CAMPO,"FUNC_NOM",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN NIL
*************************  fim  *********************************
