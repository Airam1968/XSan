*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MAL_DIRE.PRG
* Data        : 01/01/99
* Objetivo    : PROGRAMA DE MALA DIRETA 
* PRG"s       :
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MAL_DIRE()
PLQUA("01")
PPERG("Confirma ? (S/N)")
IF sisRESP="N"
   RETURN nil
ENDIF
PLQUA("01")
SET DEVICE TO SCREEN
PLQUA("01")
PMENSAG("Aguarde, Preparando Nomes Selecionados ...")
SELE 1
USE CONTATO INDE CONTATO,NUM_CLIE
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
PPERG ("Deseja teste de posicionamento de impressora ? (S/N)")
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
      PPERG("Repetir o teste ? (S/N)")
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
   eCLIENT1   = CLIENTE
   eIDENTIF_1 = IDENTIF
   IF OPCAO = "E"
      eEND1 = END_C
      eBAI1 = BAIRRO_C
      eCID1 = CIDADE_C
      eEST1 = ESTADO_C
      eCEP1 = CEP_C
   ELSE
      eEND1 = END_R
      eBAI1 = BAIRRO_R
      eCID1 = CIDADE_R
      eEST1 = ESTADO_R
      eCEP1 = CEP_R
   ENDIF
   SKIP
   IF .NOT. EOF()
      eCLIENT2   = CLIENTE
      eIDENTIF_2 = IDENTIF
      IF OPCAO = "E"
         eEND2 = END_C
         eBAI2 = BAIRRO_C
         eCID2 = CIDADE_C
         eEST2 = ESTADO_C
         eCEP2 = CEP_C
      ELSE
         eEND2 = END_R
         eBAI2 = BAIRRO_R
         eCID2 = CIDADE_R
         eEST2 = ESTADO_R
         eCEP2 = CEP_R
      ENDIF
   ELSE
      eIDENTIF_2 = SPACE(12)
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
   @ LIN,00 SAY eIDENTIF_1
   @ LIN,64 SAY eIDENTIF_2
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
