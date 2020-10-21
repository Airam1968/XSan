*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : EXC_CREC.PRG
* Data        : 01/01/99
* Objetivo    : EXCLUSAO DE CONTAS A RECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION EXC_CREC()
SAVE SCREEN TO TELA03
PTIT( "EXCLUS«O DE CONTAS A RECEBER")
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
   DO WHILE .T.
      PLQUA("11")
      DO WHILE .T.
         eNUM_LANC   = 0
         @ 06,05 SAY "Lanáamento    : " GET eNUM_LANC PICT "@R 9999999999"
         READ
         IF eNUM_LANC = 0
            CLOSE ALL
            RESTORE SCREEN FROM TELA03
            RETURN nil
         ELSE
            SEEK eNUM_LANC
            IF .NOT. FOUND()
               PLQUA("01")
               PMSGR( "Lanáamento Inválido !",3)
               LOOP
            ELSE
               EXIT
            ENDIF
         ENDIF
      ENDDO
      @ 08,05 SAY "Data Pagamento: " GET DATA
      @ 10,05 SAY "Cliente       : " GET CODIGO_CLI PICT "@R 9999999"
      eCODIGO_CLI = CODIGO_CLI
      SELE 2
      SEEK eCODIGO_CLI
      @ 10,29 SAY " - "+ALLTRIM(NOME)
      SELE 1
      @ 12,05 SAY "Descriá∆o     : " GET DESCRICAO
      @ 14,05 SAY "Valor a Pagar : " GET VALOR PICT "@R 999,999,999,999.99"
      @ 16,05 SAY "Previsáo\Real : " GET SITUACAO PICT "X"
      @ 18,05 SAY "Tipo Receita  : " GET TIPO PICT "99"
      CLEAR GETS
      PLQUA("01")
      PPERG ( "Confirma (S/N) ? ")
      IF sisRESP = "S"
	 IF F_BLOQR()
            DELETE
	    UNLOCK
	 ENDIF
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
