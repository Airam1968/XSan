*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : FECH_EST.PRG
* Data        : 01/01/99
* Objetivo    : Fechamento de Estoque
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION FECH_EST()
SAVE SCREEN TO TELA03
PTIT( "FECHAMENTO DE ESTOQUE")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   PLQUA("01")
   PMENSAG("Aguarde o Fechamento de Estoque ....")
   SELE 1
   USE ESTOQUE INDEX ESTOQUE
   GOTO TOP
   DO WHILE .NOT. EOF()
      REPLACE SALDO_INIC WITH ((SALDO_INIC+ENTRADA)-SAIDA),;
              ENTRADA    WITH 0,;
              SAIDA      WITH 0
      REPLACE SALDO_FINA WITH SALDO_INIC
      SKIP
   ENDDO
   PMSGR( "Processamento Concluido !",3)
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
**************************  fim  *********************************
