*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CONF_SIG.PRG
* Data        : 01/01/99
* Objetivo    : Configuraá∆o da Captura de Dados do SIGMA-ITAU
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CONF_SIG()
SAVE SCREEN TO TELA04
PTIT( "CONFIGURACAO SIGMA- ÷TAU")
SELE 1
USE CONF_SIG
GOTO TOP
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   DO WHILE .T.
      PLQUA("11")
      IF F_BLOQR()
         REPLACE DATA WITH DATE()
	 COMMIT
         @ 06,05 SAY "Data Configuraá∆o : " GET DATA
         CLEAR GETS
         @ 08,05 SAY "Drive de Dados    : " GET DRIVE PICT "@! X"
         @ 10,05 SAY "SubDiretário      : " GET SUBDIR
	 READ
         UNLOCK
      ENDIF
      PLQUA("01")
      PPERG ( "Continua (S/N) ? ")
      IF sisRESP = "S"
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
