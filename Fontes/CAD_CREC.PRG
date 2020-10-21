*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CAD_CREC.PRG
* Data        : 01/01/1999
* Objetivo    : Cadastro de Contas a Receber
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CAD_CREC()
SAVE SCREEN TO TELA03
PTIT( "CADASTRO DE CONTAS A RECEBER")
SELE 4
USE CLASSE INDEX CLASSE
SET FILTER TO R_D = "R"
GOTO TOP
SELE 3
USE STATUS
GOTO TOP
SELE 2
USE CLIENTES INDEX CLIENTES
GOTO TOP
SELE 1
USE CRECEBER INDEX CRECEBER,DATA_CRE
GOTO TOP
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   PLQUA("11")
   eNUM_LANC   = 0
   eDATA       = CTOD("")
   eCODIGO_CLI = 0
   eDESCRICAO  = SPACE(50)
   eVALOR      = 0
   eSITUACAO   = SPACE(1)
   SELE 3
   eNUM_LANC = LANC_CPG
   @ 06,05 SAY "Lanáamento    : " GET eNUM_LANC PICT "@R 9999999999"
   CLEAR GETS
   SELE 1
   DO WHILE .T.
      @ 08,05 SAY "Data Pagamento: " GET eDATA
      READ
      IF eDATA = CTOD("")
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   DO WHILE .T.
      @ 10,05 SAY "Cliente       : " GET eCODIGO_CLI PICT "@R 9999999"
      READ
      SELE 2
      SEEK eCODIGO_CLI
      IF .NOT. FOUND()
         PLQUA("01")
         PMSGR( "C¢digo Inexistente",3
         SELE 1
         LOOP
      ELSE
         @ 10,29 SAY " - "+ALLTRIM(NOME)
         SELE 1
         EXIT
      ENDIF
   ENDDO
   @ 12,05 SAY "Descriá∆o     : " GET eDESCRICAO
   DO WHILE .T.
      @ 14,05 SAY "Valor a Pagar : " GET eVALOR PICT "@R 999,999,999,999.99"
      READ
      IF eVALOR = 0
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   @ 16,05 SAY "Previsáo\Real : " GET eSITUACAO PICT "X" VALID eSITUACAO $("PRpr")
   READ
   SELE 4
   @ 18,05 SAY "Tipo Receita  : "
   DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1] ="@R 99999999"
   PIC[2] ="@S30"
   CAB[1] ="        "
   CAB[2] ="                               "
   DBEDIT(18,20,18,63,CAMPO,"F_CONS",PIC,CAB,CHR(196),CHR(16),CHR(205))
   eTIPO = CODIGO
   SELE 1
   PLQUA("01")
   PPERG ( "Confirma (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
      PMENSAG("Gravando, Aguarde ...")
      SELE 3
      GOTO TOP
      IF F_BLOQR()
         REPLACE LANC_CRE WITH eNUM_LANC + 1
	 UNLOCK
      ENDIF
      SELE 1
      APPEND BLANK
      IF BLOQ_R()
         REPLACE LANCAMENTO WITH eNUM_LANC,;
                 DATA       WITH eDATA,;
                 CODIGO_CLI WITH eCODIGO_CLI
         REPLACE DESCRICAO  WITH eDESCRICAO,;
                 VALOR      WITH eVALOR,;
                 SITUACAO   WITH eSITUACAO
         REPLACE TIPO       WITH eTIPO
	 UNLOCK
      ENDIF
   ENDIF
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
