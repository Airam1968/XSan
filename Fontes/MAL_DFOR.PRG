*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MAL_DFOR.PRG
* Data        : 01/01/99
* Objetivo    : PROGRAMA DE MALA DIRETA - FORNECEDORES
* PRG"s       :
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MAL_DFOR()
DO PLQUA WITH "0100"
PPERG ( "Confirma ? (S/N)")
IF sisRESP="N"
   RETURN nil
ENDIF
PLQUA("01")
SET DEVICE TO SCREEN
PLQUA("01")
PMENSAG("Aguarde, Preparando Nomes Selecionados ...")
SELE 1
USE FORNECED INDEX FORNECED
GOTO TOP
IF F_BLOQA()
   SET FILTER TO SELECAO = "X"
   GOTO TOP
   UNLOCK
ENDIF
IF EOF()
   PLQUA("01")
   PMENSAG("N∆o H† clientes selecionados, Tecle [ENTER] para finalizar")
   PPARA()
   RETURN nil
ENDIF
PLQUA("01")
PMENSAG("Posicione as etiquetas na impressora, Tecle [ENTER] para comecar")
PPARA()
DO PIMPR
PLQUA("01")
PPERG ( "Deseja teste de posicionamento de impressora ? (S/N)")
IF sisRESP="S"
   DO WHILE .T.
      SET DEVICE TO PRINT
      @ PROW(),PCOL() SAY sisLETRAC+" "
      LIN=PROW()
      @ LIN,00 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      @ LIN,64 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      LIN = LIN + 1
      @ LIN,00 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      @ LIN,64 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      LIN = LIN + 1
      @ LIN,00 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      @ LIN,64 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      LIN = LIN + 1
      @ LIN,00 SAY "XXXXX - XXXXXXXXXXXXXXXXXXXXX - XXXXXXXXXXXXXXXXXX"
      @ LIN,64 SAY "XXXXX - XXXXXXXXXXXXXXXXXXXXX - XXXXXXXXXXXXXXXXXX"
      LIN = LIN + 1
      @ LIN,00 SAY "XXXXX"
      @ LIN,64 SAY "XXXXX"
      LIN = LIN + 2
      @ LIN,00 SAY sisLETRAN+" "
      SET DEVICE TO SCREEN
      PLQUA("01")
	  PPERG ( "Repetir o teste ? (S/N)")
      IF sisRESP="N"
         EXIT
      ENDIF
   ENDDO
ENDIF
CONSTANTE = 53
PLQUA("01")
PMENSAG("Aguarde a Preparaá∆o ...")
SET DEVICE TO PRINT
LIN = PROW()
GOTO TOP
DO WHILE .NOT. EOF()
   @ PROW(),PCOL() SAY sisLETRAC+" "
   eCLIENT1   = NOME
   eEND1      = END
   eBAI1      = BAIRRO
   eCID1      = CIDADE
   eEST1      = ESTADO
   eCEP1      = CEP
   SKIP
   IF .NOT. EOF()
      eCLIENT2   = NOME
      eEND2 = END
      eBAI2 = BAIRRO
      eCID2 = CIDADE
      eEST2 = ESTADO
      eCEP2 = CEP
   ELSE
      eCLIENT2   = SPACE(50)
      eEND2      = SPACE(50)
      eBAI2      = SPACE(20)
      eCID2      = SPACE(20)
      eEST2      = SPACE(2)
      eCEP2      = SPACE(8)
   ENDIF
   @ LIN,00 SAY eCLIENT1
   @ LIN,64 SAY eCLIENT2
   LIN = LIN + 1
   LIN = LIN + 1
   @ LIN,00 SAY eEND1
   @ LIN,64 SAY eEND2
   LIN = LIN + 1
   @ LIN,00 SAY ALLTRIM(eBAI1)+" - "+ALLTRIM(eCID1)+" - "+ALLTRIM(eEST1)
   @ LIN,64 SAY ALLTRIM(eBAI2)+" - "+ALLTRIM(eCID2)+" - "+ALLTRIM(eEST2)
   LIN = LIN + 1
   @ LIN,00 SAY eCEP1 PICT "@R XXXXX-XXX"
   @ LIN,64 SAY eCEP2 PICT "@R XXXXX-XXX"
   LIN = LIN + 2
   @ LIN,00 SAY sisLETRAN+" "
   SKIP
ENDDO
SET DEVICE TO SCREEN
CLOSE DATABASES
RETURN nil
***********************  fim ***********************************
